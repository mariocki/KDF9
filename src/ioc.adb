-- Emulation of the common functionality of a KDF9 IOC "buffer" (DMA channel),
--    with fail-stop stubs for operations having device-specific behaviour.
--
-- This file is part of ee9 (6.0a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2021, W. Findlay; all rights reserved.
--
-- The ee9 program is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. This program is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details. You should have
-- received a copy of the GNU General Public License distributed with
-- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Exceptions;
--
with exceptions;
with IOC.slow;
with KDF9.CPU;
with KDF9.PHU_store;
with tracing;

with IOC.the_locker_of;

use  exceptions;
use  KDF9.CPU;
use  KDF9.PHU_store;
use  tracing;

package body IOC is

   procedure set_state_of (the_buffer : in device_class_access;
                           allocated  : in Boolean) is
   begin
      if the_buffer = null then
         raise emulation_failure with "trying to set state of null buffer to " & allocated'Image;
      end if;
      if the_buffer.is_allocated = allocated then
         -- Allocating an already allocated device, or deallocating an unallocated device.
         -- Both are benign, so ignore.
         return;
      else
         the_buffer.is_allocated := allocated;
         the_CPDAR(the_buffer.number) := allocated;
      end if;
    end set_state_of;

   function is_allocated (the_buffer : device_class_access)
   return Boolean
   is (the_buffer.is_allocated);

   function is_unallocated (the_buffer : device_class_access)
   return Boolean
   is (not the_buffer.is_allocated);

   function device_name_of (the_buffer : IOC.device)
   return IOC.device_name
   is (IOC.device_kind'Image(the_buffer.kind)(1 .. 2)
     & trimmed(the_buffer.unit'Image));

   function device_name_of (the_number : IOC.device_number)
   return IOC.device_name
   is (device_name_of(buffer(the_number).all));

   function device_kind_of (the_number : IOC.device_number)
   return IOC.device_kind
   is (buffer(the_number).kind);

   overriding
   procedure Initialize (the_buffer : in out IOC.device) is
   begin
      if not IOC.device'Class(the_buffer).is_open then
         the_buffer.is_abnormal := True;
         the_buffer.is_offline  := True;
      end if;
      install(the_buffer);
   end Initialize;

   procedure open (the_buffer : in out IOC.device'Class;
                   the_mode   : in POSIX.access_mode) is
   begin
      the_buffer.device_name := device_name_of(the_buffer);
      host_IO.open(the_buffer.stream, the_buffer.device_name, the_mode);
      if the_buffer.is_open then
         if the_mode = write_mode then
            truncate(the_buffer.stream, to_length => 0);
         end if;
      else
         trap_operator_error(the_buffer.device_name & " cannot be found");
      end if;
      IOC.device(the_buffer).Initialize;
   end open;

   overriding
   procedure Finalize (the_buffer : in out IOC.device) is
      buffer : constant String  := oct_of(KDF9.Q_part(the_buffer.number), 2);
   begin
      if IOC.device'Class(the_buffer).is_open   and then
            IOC.device'Class(the_buffer).usage /= 0 then
         IOC.device'Class(the_buffer).close;
      end if;
   exception
      when error : others =>
         raise emulation_failure
            with "Finalizing buffer #" & buffer & ": " & Ada.Exceptions.Exception_Information(error);
   end Finalize;

   function is_open (the_buffer : IOC.device)
   return Boolean
   is (the_buffer.stream.is_open);

   function usage (the_buffer : IOC.device)
   return KDF9.word is
   begin
      return the_buffer.stream.bytes_moved;
   end usage;

   procedure flush (the_buffer : in out IOC.device) is
   begin
      flush(the_buffer.stream);
   end flush;

   procedure close (the_buffer : in out IOC.device) is
   begin
      close(the_buffer.stream);
      IOC.buffer(the_buffer.number) := null;
   end close;

   procedure finalize_all_KDF9_buffers is
   begin
      for b in IOC.buffer'Range loop
         if IOC.buffer(b) /= null then
            Finalize(IOC.buffer(b).all);
         end if;
      end loop;
   end finalize_all_KDF9_buffers;

   procedure add_in_the_IO_lockout_CPU_time (Q_operand : in KDF9.Q_register) is
      IO_size : constant KDF9.Q_part := Q_operand.M - Q_operand.I;
   begin
      the_CPU_delta := the_CPU_delta + KDF9.us(IO_size + group_size - 1) / group_size;
   end add_in_the_IO_lockout_CPU_time;

   function IO_elapsed_time (the_buffer   : IOC.device;
                             atomic_items : KDF9.word)
   return KDF9.us
   is (
       if IOC.device'Class(the_buffer).is_open then
          KDF9.us(atomic_items) * the_buffer.quantum
       else
          0
      );

   procedure add_in_the_IO_CPU_time (IO_CPU_time : in KDF9.us) is
   begin
      the_CPU_delta := the_CPU_delta + IO_CPU_time;
   end add_in_the_IO_CPU_time;

   procedure add_in_the_IO_CPU_time (the_buffer  : in IOC.device'Class;
                                     bytes_moved : in KDF9.word) is
      the_IO_CPU_time : KDF9.us;
   begin
      if the_buffer.is_open then
         if the_buffer in IOC.slow.device'Class then
            the_IO_CPU_time := KDF9.us(bytes_moved) * 6;          -- 6µs/char
         else
            the_IO_CPU_time := KDF9.us(bytes_moved + 7) / 8 * 6;  -- 6µs/word
         end if;
      else
         the_IO_CPU_time := 0;
      end if;
      add_in_the_IO_CPU_time(the_IO_CPU_time);
   end add_in_the_IO_CPU_time;

   function IO_elapsed_time_total (the_buffer : IOC.device)
   return KDF9.us
   is (IO_elapsed_time(IOC.device'Class(the_buffer), IOC.device'Class(the_buffer).usage));

   procedure install (the_device : in out IOC.device'Class) is
   begin
      if buffer(the_device.number) /= null then
         if buffer(the_device.number).kind /= AD_kind                      and then
               buffer(the_device.number).device_name /= the_device.device_name then
            raise emulation_failure
               with "attempt to install a second device, namely "
                  & the_device.device_name
                  & ", on buffer #"
                  & oct_of(the_device.number)
                  & " which already has "
                  & buffer(the_device.number).device_name;
         end if;
      end if;
      buffer(the_device.number) := the_device'Unchecked_Access;
   end install;

   -- Mask off the buffer number in the Q_operand.C; to remove any disc parameter.
   function canonical (Q_operand : KDF9.Q_register)
   return KDF9.Q_register
   is (C => Q_operand.C and buffer_number_mask, I => Q_operand.I, M => Q_operand.M);

   procedure validate_device (the_buffer : in IOC.device'Class;
                              Q_operand  : in KDF9.Q_register) is
      Q : constant KDF9.Q_register := canonical(Q_operand);
   begin
      if not the_buffer.is_open then
         trap_operator_error("buffer #" & oct_of(the_buffer.number, 2) & " is not configured");
      end if;
      if KDF9.Q_part(the_buffer.number) /= Q.C then
         raise emulation_failure
            with "wrong C-part: "
               & oct_of(Q_operand.C)
               & " for "
               & the_buffer.device_name
               & " on buffer #"
               & oct_of(KDF9.Q_part(the_buffer.number), 2);
      end if;
      if not the_CPDAR(the_buffer.number) and the_CPU_state /= Director_state then
         trap_illegal_instruction("unallocated I/O device " & the_buffer.device_name);
      end if;
   end validate_device;

   procedure validate_bounds (Q_operand  : in KDF9.Q_register) is
   begin
      if Q_operand.I > Q_operand.M then
         trap_illegal_instruction("invalid I/O Q operand: I > M");
      end if;
      validate_address_range(Q_operand.I, Q_operand.M);
   end validate_bounds;

   procedure validate_transfer (the_buffer : in IOC.device'Class;
                                Q_operand  : in KDF9.Q_register) is
   begin
      validate_device(the_buffer, Q_operand);
      validate_bounds(Q_operand);
   end validate_transfer;

   procedure validate_parity (the_buffer : in IOC.device'Class) is
   begin
      if the_buffer.is_abnormal then
         trap_illegal_instruction("the buffer for "
                                & the_buffer.device_name
                                & " is abnormal (parity error or end-of-data)");
      end if;
   end validate_parity;

   procedure require_positive_count (count : in KDF9.Q_part) is
   begin
      if resign(count) <= 0 then
         trap_illegal_instruction("nonpositive I/O repetition count");
      end if;
   end require_positive_count;

   procedure require_nonnegative_count (count : in KDF9.Q_part) is
   begin
      if resign(count) < 0 then
         trap_illegal_instruction("negative I/O repetition count");
      end if;
   end require_nonnegative_count;

   function image (the_buffer : in IOC.device'Class)
   return String
   is (
       the_buffer.device_name
     & " Q"  & the_buffer.control_word.C'Image
     & "/#"  & oct_of(the_buffer.control_word.I)
     & "/#"  & oct_of(the_buffer.control_word.M)
      );

   -- In boot mode, effect the LOV interrupt to Director.
   -- In other modes, advance the elapsed time to the next-interrupt time,
   --    and suppress the LOV by simulating an earlier end of transfer.
   procedure handle_a_buffer_lockout (the_buffer : in IOC.device'Class) is
   begin
      PHU(CPL) := (
                   is_held_up => True,
                   blockage   => (buffer_busy, the_buffer.number, by_INTQq => False)
                  );
      take_note_of_buffer_lockout(the_buffer.device_name, the_buffer.operation);
      if the_execution_mode = boot_mode then
         LOV_if_user_mode(the_buffer.device_name & " is busy");
      else
         advance_the_clock(the_buffer.completion_time);
         act_on_pending_interrupts;
      end if;
   end handle_a_buffer_lockout;

   procedure deal_with_a_busy_device (the_buffer  : in out IOC.device'Class;
                                      order_time  : in KDF9.us;
                                      set_offline : in Boolean) is
   begin
      advance_the_clock(the_clock_time+order_time);
      if the_buffer.is_busy then
         handle_a_buffer_lockout(the_buffer);
      end if;
      the_buffer.is_offline := set_offline;
   end deal_with_a_busy_device;

   procedure finalize_transfer (the_buffer : in out IOC.device'Class;
                                need_EDT,
                                need_PR    : out Boolean);

   procedure effect_device_interrupt (code : KDF9.interrupt_number; the_buffer : in IOC.device'Class) is
   begin
      effect(code, the_buffer.image);
    end effect_device_interrupt;

   function is_DMAing (the_buffer : in IOC.device'Class)
   return Boolean
   is (the_buffer.operation in input_operation | output_operation);

   procedure start_data_transfer (the_buffer   : in out IOC.device'Class;
                                  Q_operand    : in KDF9.Q_register;
                                  set_offline  : in Boolean;
                                  busy_time    : in KDF9.us;
                                  operation    : in IOC.transfer_kind := some_other_operation) is
      pragma Unreferenced(set_offline);
      transferring_data : constant Boolean := operation in input_operation | output_operation;
      time_now          : constant KDF9.us := the_clock_time;
      real_Q            : KDF9.Q_register := Q_operand;
      EDT_needed,
      PR_needed         : Boolean;
   begin
      -- Check the IO parameters and the buffer state, and handle any lockout set by another device.
      validate_device(the_buffer, Q_operand);

      if transferring_data then
         validate_bounds(real_Q);
         real_Q := (real_Q.C, real_Q.I+BA, real_Q.M+BA);
      else
         require_nonnegative_count(real_Q.M);
      end if;

      validate_parity(the_buffer);

      if the_buffer.is_busy then
         handle_a_buffer_lockout(the_buffer);
         if the_execution_mode = boot_mode then
            finalize_transfer (the_buffer, EDT_needed, PR_needed);
            if the_next_interrupt_time > time_now + 1_024_000 then
               the_next_interrupt_time := time_now + 1_024_000;
            end if;
            if EDT_needed then
               effect_device_interrupt(EDT_interrupt, the_buffer);
            elsif PR_needed then
               effect_device_interrupt(PR_interrupt, the_buffer);
            end if;
         end if;
      end if;

      if transferring_data                            and then
            there_are_locks_in_physical_addresses(real_Q) then
         LOV_if_user_mode(
                          "in "
                       &  "#"   & oct_of(the_buffer.control_word.I)
                       &  "/#"  & oct_of(the_buffer.control_word.M)
                       &  " for "
                       &  the_buffer.device_name
                         );
      end if;

      -- Set up the transfer parameters.
      the_buffer.is_for_Director := (the_CPU_state = Director_state);
      the_buffer.priority_level  := CPL;
      the_buffer.control_word    := real_Q;
      the_buffer.operation       := operation;
      the_buffer.order_count     := ICR+1;
      the_buffer.order_address   := CIA;
      the_buffer.decoded_order   := INS;
      the_buffer.initiation_time := time_now;
      the_buffer.transfer_time   := busy_time;
      the_buffer.completion_time := the_buffer.initiation_time + busy_time;

      if busy_time > 0 or transferring_data then
         if the_buffer.completion_time < the_next_interrupt_time then
            the_next_interrupt_time := the_buffer.completion_time;
         end if;
         the_buffer.is_busy := True;
         take_note_of_IO_start(
                               the_buffer.device_name,
                               the_buffer.completion_time,
                               the_buffer.control_word,
                               the_buffer.operation
                              );
      else
         the_buffer.is_busy := False;
         take_note_of_IO_finis (
                                the_buffer.order_count,
                                the_buffer.order_address,
                                the_buffer.decoded_order,
                                the_buffer.initiation_time,
                                the_buffer.device_name,
                                the_buffer.is_for_Director,
                                the_buffer.priority_level,
                                the_buffer.completion_time,
                                the_buffer.control_word,
                                the_buffer.operation
                               );
      end if;
      PHU(CPL) := idle_PHU;
   end start_data_transfer;

   -- start_slow_transfer takes a pessimistic view of transfers-to-End_Message.
   -- When the actual transfer length is known, the end-of-transfer time can be
   --    made more realistic by specifying its real length to correct_transfer_time.
   -- correct_transfer_time must be called before finalize_transfer is called.

   procedure correct_transfer_time (the_buffer  : in out IOC.device'Class;
                                    actual_time : in KDF9.us) is
   begin
      the_buffer.transfer_time :=  actual_time;
      the_buffer.completion_time := the_buffer.initiation_time + the_buffer.transfer_time;
      if the_buffer.completion_time < the_next_interrupt_time then
         the_next_interrupt_time := the_buffer.completion_time;
      end if;
   end correct_transfer_time;

   procedure correct_transfer_time (the_buffer    : in out IOC.device'Class;
                                    actual_length : in KDF9.word) is
   begin
      the_buffer.transfer_time := IO_elapsed_time(the_buffer, actual_length);
      the_buffer.completion_time := the_buffer.initiation_time + the_buffer.transfer_time;
      if the_buffer.completion_time < the_next_interrupt_time then
         the_next_interrupt_time := the_buffer.completion_time;
      end if;
   end correct_transfer_time;

   -- If the buffer has a terminated transfer, clear its lockouts, reset its state,
   --    update the PHUs, and demand an EDT or PR interrupt as needed.
   procedure finalize_transfer (the_buffer : in out IOC.device'Class;
                                need_EDT,
                                need_PR    : out Boolean) is
      the_PHU : KDF9.PHU_store.PHU_register renames KDF9.PHU_store.PHU(the_buffer.priority_level);
      blocked : PHU_reason;
   begin
      if the_buffer.transfer_time /= 0 then
         take_note_of_IO_finis (
                                the_buffer.order_count,
                                the_buffer.order_address,
                                the_buffer.decoded_order,
                                the_buffer.initiation_time,
                                the_buffer.device_name,
                                the_buffer.is_for_Director,
                                the_buffer.priority_level,
                                the_buffer.completion_time,
                                the_buffer.control_word,
                                the_buffer.operation
                               );
      end if;

      need_EDT := the_buffer.is_for_Director;

      -- Clear down the transfer and idle the buffer.
      if the_buffer.is_DMAing then
         unlock_absolute_addresses(the_buffer.control_word);
      end if;
      the_buffer.is_busy := False;
      the_buffer.is_for_Director := False;

      -- The following code is somewhat redundant, but written like this to exactly mirror the
      -- logic stated in the KDF9 TIME-SHARING DIRECTOR SUPPORT DOCUMENTATION of 1-May-1965.
      need_PR := False;
      if the_PHU.is_held_up then
         blocked := the_PHU.blockage;
         if (blocked.reason = buffer_busy and then blocked.by_INTQq)                      or else
            (blocked.reason = buffer_busy and then not buffer(blocked.buffer_nr).is_busy) or else
            (blocked.reason = locked_core and then is_unlocked(blocked.group_nr))            then
            the_PHU := idle_PHU;
            need_PR := the_buffer.priority_level < CPL;
         end if;
      end if;

      -- A PR interrupt may be wanted, BUT not if an EDT interrupt is wanted.
      -- EDT is wanted if the transfer was for Director OR another program awaits the same buffer.

      -- Check the rest of the PHU stores for an EDT interrupt.
      -- This is needed only when running a Director.
      if the_execution_mode = boot_mode then
         -- Test for possible priority inversion, i.e. other program(s) blocked on this buffer.
         for p of PHU loop
            if p.is_held_up                               and then
                  p.blockage.reason = buffer_busy         and then
                     p.blockage.buffer_nr = the_buffer.number then
               -- The KDF9 TIME-SHARING DIRECTOR SUPPORT DOCUMENTATION of 1-May-1965
               --   says such a PHU is NOT cleared, but EDT is requested INSTEAD of PR,
               --   so Director can take action according to what it finds there.
               need_EDT := True;
            end if;
         end loop;
      end if;
      if need_EDT then
         need_PR := False;
      end if;
   end finalize_transfer;

   procedure act_on_pending_interrupts is
      time_now   : constant KDF9.us := the_clock_time;
      EDT_needed,
      PR_needed  : Boolean := False;
      number     : IOC.device_number := 16;
   begin
      advance_the_clock(the_next_interrupt_time);
      -- Predict another interrupt (at most 2**20 seconds in the future in boot mode).
      the_next_interrupt_time := KDF9.us'Last;
      for b of buffer loop
         if b /= null and then
               b.is_busy  then
            if time_now >= b.completion_time then
               finalize_transfer(b.all, EDT_needed, PR_needed);
               if EDT_needed or PR_needed then
                  number := b.number;
               end if;
            elsif the_next_interrupt_time > b.completion_time then
               the_next_interrupt_time := b.completion_time;
            end if;
         end if;
      end loop;
      -- Prevent an inadvertant double clock interrupt.
      if the_execution_mode = boot_mode                and then
            the_next_interrupt_time > time_now + 1_048_575 then
         the_next_interrupt_time := time_now + 1_048_575;
      end if;
      if EDT_needed then
         effect_device_interrupt(EDT_interrupt, buffer(number).all);
      elsif PR_needed then
         effect_device_interrupt(PR_interrupt, buffer(number).all);
      end if;
   end act_on_pending_interrupts;

   -- Advance the time to a point after all extant transfer have terminated,
   --    finalizing all extant transfer in temporal order in the process.
   procedure complete_all_extant_transfers is
      EDT_needed,
      PR_needed      : Boolean := False;
      last_time      : KDF9.us := 0;
      next_time      : KDF9.us;
   begin
      -- At least one transfer should be terminated each time around outer_loop,
      --    if not, outer_loop is exited.
   outer_loop:
      for c in buffer'Range loop
         -- Find the earliest transfer termination time.
         next_time := KDF9.us'Last;
         for b of buffer loop
            if b /= null                       and then
                  b.is_busy                    and then
                     b.completion_time < next_time then
               next_time := b.completion_time;
            end if;
         end loop;

         if next_time = KDF9.us'Last then
            -- All the buffers are quiescent.
            exit outer_loop;
         else
            -- At least one transfer remains to be finalized.
            advance_the_clock(next_time);
            last_time := KDF9.us'Max(last_time, next_time);
         end if;

         -- Finalize all transfers with completion time <= next_time.
         for b of buffer loop
            if b /= null                        and then
                  b.is_busy                     and then
                     b.completion_time <= next_time then
               finalize_transfer(b.all, EDT_needed, PR_needed);
            end if;
         end loop;

      end loop outer_loop;
   end complete_all_extant_transfers;

   procedure handle_a_main_store_lockout is
      the_locker : KDF9.buffer_number;
   begin
      PHU(CPL) := (
                   is_held_up => True,
                   blockage   => (locked_core, group_address(group(the_locked_out_address)))
                  );
      -- Store access LOV interrupts invoke instruction restart outside Director.
      the_locker := the_locker_of(the_locked_out_address);
      take_note_of_store_lockout(device_name_of(buffer(the_locker).all));
      if the_execution_mode = boot_mode then
         if_user_mode_then_LOV(the_locked_out_address);
      else
         set_NIA_to(CIA);
         advance_the_clock(buffer(the_locker).completion_time);
         act_on_pending_interrupts;
      end if;
      ICR := ICR + 1;
   end handle_a_main_store_lockout;

   procedure MANUAL_CT (the_buffer  : in out IOC.device'Class;
                        Q_operand   : in KDF9.Q_register;
                        set_offline : in Boolean) is
      EDT_needed, PR_needed : Boolean := False;
   begin
      validate_device(the_buffer, Q_operand);
      take_note_of_test(the_buffer.device_name, Q_operand, the_buffer.is_busy);
      -- ee9 allows the transfer to terminate normally, as if MANUALQ with set_offline = True;
      --    even when set_offline = False, i.e. CTQ, which aborted any residual I/O on the KDF9.
      if the_buffer.is_busy then
         the_buffer.completion_time := KDF9.us'Min(the_buffer.completion_time, the_clock_time);
         finalize_transfer(the_buffer, EDT_needed, PR_needed);
      end if;
      the_buffer.is_busy     := False;
      the_buffer.operation   := some_other_operation;
      the_buffer.is_abnormal := False;
      the_buffer.is_offline  := set_offline;
   end MANUAL_CT;

   procedure INT (the_buffer  : in out IOC.device'Class;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      now  : constant KDF9.us := the_clock_time;
      step : KDF9.us;
   begin
      validate_device(the_buffer, Q_operand);
      if the_buffer.is_busy then
         PHU(CPL) := (
                      is_held_up => True,
                      blockage   => (buffer_busy, the_buffer.number, by_INTQq => True)
                     );
         take_note_of_buffer_lockout(the_buffer.device_name, the_buffer.operation);
         if the_execution_mode = boot_mode then
            step := KDF9.us'Max((the_buffer.completion_time - now) / 16, 1);
            advance_the_clock(KDF9.us'Min(the_buffer.completion_time, now + step));
            effect(PR_interrupt, image(the_buffer));
         else
            advance_the_clock(the_buffer.completion_time);
            act_on_pending_interrupts;
         end if;
      else
         take_note_of_test(the_buffer.device_name, Q_operand, False);
      end if;
      the_buffer.is_offline := set_offline;
   end INT;

   procedure BUSY (the_buffer  : in out IOC.device'Class;
                   Q_operand   : in KDF9.Q_register;
                   set_offline : in Boolean;
                   result      : out Boolean) is
   begin
      validate_device(the_buffer, Q_operand);
      result := the_buffer.is_busy;
      take_note_of_test(the_buffer.device_name, Q_operand, result);
      if the_buffer.is_busy and then
            (the_execution_mode /= boot_mode or the_CPU_state = Director_state) then
         act_on_pending_interrupts;
      end if;
      the_buffer.is_offline := set_offline;
   end BUSY;

   procedure PAR (the_buffer  : in out IOC.device'Class;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean;
                  result      : out Boolean) is
   begin
      validate_device(the_buffer, Q_operand);
      deal_with_a_busy_device(the_buffer, 13, set_offline);
      result := the_buffer.is_abnormal;
      take_note_of_test(the_buffer.device_name, Q_operand, result);
      the_buffer.is_abnormal := False;
      the_buffer.is_offline := set_offline;
   end PAR;

   subtype IO_mnemonic is String(1 .. 5);
   type synonyms       is array (1 .. 2) of IO_mnemonic;
   type synonym_list   is array (Positive range <>) of synonyms;

   FW_synonyms  : constant synonym_list
                := (
                    ("POA  ", "TW   "), ("POB  ", "TWE  "),
                    ("PIA  ", "TR   "), ("PIB  ", "TRE  ")
                   );

   TR_synonyms  : constant synonym_list
                := (
                    ("PIA  ", "PR   "), ("PIB  ", "PRE  "),
                    ("PIC  ", "PRC  "), ("PID  ", "PRCE ")
                   );

   TP_synonyms  : constant synonym_list
                := (
                    ("POA  ", "PW   "), ("POB  ", "PWE  "),
                    ("POC  ", "PWC  "), ("POD  ", "PWCE "),
                    ("POE  ", "PGAP ")
                   );

   LP_synonyms  : constant synonym_list
                := (
                    ("POA  ", "LP   "), ("POB  ", "LPE  ")
                   );

   MT_synonyms  : constant synonym_list
                := (
                    ("PIA  ", "MFR  "), ("PIB  ", "MFRE "),
                    ("PIE  ", "MBR  "), ("PIF  ", "MBRE "),
                    ("POA  ", "MW   "), ("POB  ", "MWE  "),
                    ("POC  ", "MLW  "), ("POD  ", "MLWE "),
                    ("POE  ", "MGAP "), ("POF  ", "MWIPE"),
                    ("PMA  ", "MFSK "), ("PMB  ", "MBT  "),
                    ("PMC  ", "MLB  "), ("PMD  ", "MRWD "),
                    ("PME  ", "MBSK "), ("PMF  ", "MET  ")
                   );

   function mnemonic (order : in String; class : in IOC.device_name)
   return String is

      key : constant IO_mnemonic := just_left(order(order'First..order'First+2), 5);
      Qij : constant String      := order(order'First+3..order'Last);

      function choose (synonyms : synonym_list)
      return String is
      begin
         for s of synonyms loop
            if s(1) = key then return trimmed(s(2)) & Qij; end if;
         end loop;
         return order;
      end choose;

      XY : constant String(1..2) := class(class'First..class'First+1);

   begin
      if key(1..3) in "TLO" | "CLO" | "PMH" | "SLO" then
         -- These orders do not necessarily involve a device.
         return order;
      end if;
      if XY in "AD" | "CP" | "CR" | "DR" | "FD" | "GP" | "ST" | "SI" then
         return order;
      elsif XY = "FW" then -- FlexoWriter
         return choose(FW_synonyms);
      elsif XY = "LP" then -- Line Printer
         return choose(LP_synonyms);
      elsif XY = "MT" then -- Magnetic Tape
         return choose(MT_synonyms);
      elsif XY = "TP" then -- Tape Punch
         return choose(TP_synonyms);
      elsif XY = "TR" then -- Tape Reader
         return choose(TR_synonyms);
      else
         return "??";
      end if;
   end mnemonic;

   procedure trap_failing_IO_operation (the_buffer : in out IOC.device; the_message : in String) is
      the_diagnostic : constant String := "%" & the_message & " on " & the_buffer.device_name;
   begin
      if the_execution_mode in program_mode | test_program_mode then
         raise IO_error with the_diagnostic;
      elsif the_CPU_state = program_state then
         the_buffer.is_abnormal := True;
         raise abandon_this_order with the_diagnostic;
      else
         -- The Director itself has gone seriously wrong.
         raise Director_IO_error with the_diagnostic;
      end if;
   end trap_failing_IO_operation;

   procedure trap_illegal_IO_operation (order : in String; buffer : in IOC.device) is
   begin
      trap_illegal_instruction(order & " cannot be used on " & buffer.device_name);
   end trap_illegal_IO_operation;

   --
   -- The following bodies provide inheritable default actions for
   -- operations that are not implemented by specific device types.
   --

   procedure PIA (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PIA", the_buffer);
   end PIA;

   procedure PIB (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PIB", the_buffer);
   end PIB;

   procedure PIC (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PIC", the_buffer);
   end PIC;

   procedure PID (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PID", the_buffer);
   end PID;

   procedure PIE (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PIE", the_buffer);
   end PIE;

   procedure PIF (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PIF", the_buffer);
   end PIF;

   procedure PIG (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PIG", the_buffer);
   end PIG;

   procedure PIH (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PIH", the_buffer);
   end PIH;

   procedure PMA (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PMA", the_buffer);
   end PMA;

   procedure PMB (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(the_buffer);
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      null;
   end PMB;

   procedure PMC (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(the_buffer);
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      null;
   end PMC;

   procedure PMD (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PMD", the_buffer);
   end PMD;

   procedure PME (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PME", the_buffer);
   end PME;

   procedure PMF (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(the_buffer);
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      null;
   end PMF;

   procedure PMG (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PMG", the_buffer);
   end PMG;

-- procedure PMH is subsumed by SLOC.

   procedure PMK (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PMK", the_buffer);
   end PMK;

   procedure PML (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("PML", the_buffer);
   end PML;

   procedure POA (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("POA", the_buffer);
   end POA;

   procedure POB (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("POB", the_buffer);
   end POB;

   procedure POC (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("POC", the_buffer);
   end POC;

   procedure POD (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("POD", the_buffer);
   end POD;

   procedure POE (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("POE", the_buffer);
   end POE;

   procedure POF (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("POF", the_buffer);
   end POF;

   procedure POG (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("POG", the_buffer);
   end POG;

   procedure POH (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("POH", the_buffer);
   end POH;

   procedure POK (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("POK", the_buffer);
   end POK;

   procedure POL (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(Q_operand);
      pragma Unreferenced(set_offline);
   begin
      trap_illegal_IO_operation("POL", the_buffer);
   end POL;

end IOC;
