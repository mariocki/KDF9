-- ioc.adb
--
-- Emulation of the common functionality of a KDF9 IOC "buffer" (DMA channel),
--    with fail-stop stubs for operations having device-specific behaviour.
--
-- This file is part of ee9 (V2.0r), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2015, W. Findlay; all rights reserved.
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
with formatting;
with KDF9.CPU;
with KDF9.PHU_store;
with KDF9.store;
with settings;

use  formatting;
use  KDF9.CPU;
use  KDF9.PHU_store;
use  KDF9.store;
use  settings;

package body IOC is

   pragma Unsuppress(All_Checks);

   procedure set_state_of (the_buffer : in device_class_access;
                           allocated  : in Boolean) is
   begin
      if the_buffer.is_allocated = allocated then
         -- Allocating an already allocated device, or deallocating an unallocated device.
         if the_buffer.is_allocated then
            raise emulation_failure with "attempt to allocate an allocated device";
         else
            raise emulation_failure with "attempt to deallocate an unallocated device";
         end if;
      else
         the_buffer.is_allocated := allocated;
         the_CPDAR(the_buffer.number) := Boolean'Pos(allocated);
      end if;
    end set_state_of;

   function is_unallocated (the_buffer : device_class_access)
   return Boolean is
   begin
      return not the_buffer.is_allocated;
   end is_unallocated;

   function logical_device_name_of (the_buffer : IOC.device)
   return KDF9.logical_device_name is
   begin
      return IOC.device_kind'Image(the_buffer.kind)(1 .. 2)
           & trimmed(KDF9.buffer_number'Image(the_buffer.unit));
   end logical_device_name_of;

   function logical_device_name_of (the_number : IOC.device_number)
   return KDF9.logical_device_name is
   begin
      return logical_device_name_of(buffer(the_number).all);
   end logical_device_name_of;

   overriding
   procedure Initialize (the_buffer : in out IOC.device) is
   begin
      if not IOC.device'Class(the_buffer).is_open then
         if the_buffer.kind = MT_kind or
               the_buffer.kind = ST_kind or
                  the_buffer.kind = DR_kind then
            null;
         else
            output_line(the_buffer.device_name
                      & " is on buffer #"
                      & oct_of(KDF9.Q_part(the_buffer.number), 2)
                      & ", but the device is offline!");
         end if;
         the_buffer.is_abnormal := True;
         the_buffer.is_offline  := True;
      end if;
      install(the_buffer);
   end Initialize;

   procedure open (the_buffer : in out IOC.device'Class;
                   the_mode   : in POSIX.access_mode;
                   attaching  : in Boolean := False;
                   to_fd      : in Integer := no_fd) is
   begin
      the_buffer.device_name := logical_device_name_of(the_buffer);
      if attaching then
         open(the_buffer.stream, the_buffer.device_name, the_mode, to_fd);
         null;  -- Do not truncate an already open output stream.
      else
         open(the_buffer.stream, the_buffer.device_name, the_mode);
         if the_mode = write_mode and the_buffer.is_open then
            truncate(the_buffer.stream, to_length => 0);
         end if;
      end if;
      IOC.device(the_buffer).Initialize;
   end open;

   overriding
   procedure Finalize (the_buffer : in out IOC.device) is
   begin
      if IOC.device'Class(the_buffer).is_open   and then
            IOC.device'Class(the_buffer).usage /= 0 then
         IOC.device'Class(the_buffer).close;
      end if;
   exception
      when error : others =>
         output_line("Finalize for buffer #"
                   & oct_of(KDF9.Q_part(the_buffer.number))
                   & "; "
                   & Ada.Exceptions.Exception_Information(error));
   end Finalize;

   function is_open (the_buffer : IOC.device)
   return Boolean is
   begin
      return the_buffer.stream.is_open;
   end is_open;

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
         else
            output_line("There is no device on buffer #" & oct_of(b) & "!");
         end if;
      end loop;
   end finalize_all_KDF9_buffers;

   procedure add_in_the_IO_lockout_CPU_time (Q_operand : in KDF9.Q_register) is
      IO_size : constant KDF9.Q_part := Q_operand.M - Q_operand.I;
   begin
      the_CPU_delta := the_CPU_delta + KDF9.microseconds(IO_size + group_size) / group_size;
   end add_in_the_IO_lockout_CPU_time;

   function IO_elapsed_time (the_buffer   : IOC.device;
                             atomic_items : KDF9.word)
   return KDF9.microseconds is
   begin
      if IOC.device'Class(the_buffer).is_open then
         return KDF9.microseconds(atomic_items) * the_buffer.quantum;
      else
         return 0;
      end if;
   end IO_elapsed_time;

   procedure add_in_the_IO_CPU_time (IO_CPU_time : in KDF9.microseconds) is
   begin
      the_CPU_delta := the_CPU_delta + IO_CPU_time;
   end add_in_the_IO_CPU_time;

   procedure add_in_the_IO_CPU_time (the_buffer  : in IOC.device'Class;
                                     bytes_moved : in KDF9.word) is
      the_IO_CPU_time : KDF9.microseconds;
   begin
      if the_buffer.is_open then
         if the_buffer.is_slow then
            the_IO_CPU_time := KDF9.microseconds(bytes_moved)*6;     -- 6µs/char
         else
            the_IO_CPU_time := KDF9.microseconds(bytes_moved)*6 / 8; -- 6µs/word
         end if;
      else
         the_IO_CPU_time := 0;
      end if;
      add_in_the_IO_CPU_time(the_IO_CPU_time);
   end add_in_the_IO_CPU_time;

   function IO_elapsed_time_total (the_buffer : IOC.device)
   return KDF9.microseconds is
   begin
      return IO_elapsed_time(IOC.device'Class(the_buffer), IOC.device'Class(the_buffer).usage);
   end IO_elapsed_time_total;

   procedure install (the_device : in out IOC.device'Class) is
   begin
      if the_device.number = ND0_number then
         -- This device is not to be included in the configuration.
         return;
      end if;
      if buffer(the_device.number) /= null then
         raise emulation_failure with
            "Attempt to install a second device, namely "
          & logical_device_name_of(the_device)
          & ", on buffer #"
          & oct_of(the_device.number);
      end if;
      buffer(the_device.number) := the_device'Unchecked_Access;
   end install;

   function canonical (Q_operand : KDF9.Q_register)
   return KDF9.Q_register is
   begin
      return (
              C => Q_operand.C and buffer_number_mask,
              I => Q_operand.I,
              M => Q_operand.M
             );
   end canonical;

   procedure validate_device (the_buffer : in IOC.device'Class;
                              Q_operand  : in KDF9.Q_register) is
      Q : constant KDF9.Q_register := canonical(Q_operand);
   begin
      if not the_buffer.is_open then
         trap_invalid_instruction(the_buffer.device_name
                                & " on buffer #"
                                & oct_of(KDF9.Q_part(the_buffer.number), 2)
                                & " is offline");
      end if;
      if KDF9.Q_part(the_buffer.number) /= Q.C then
         raise emulation_failure
            with "wrong operand: "
               & oct_of(Q.C)
               & " for "
               & the_buffer.device_name
               & " on buffer #"
               & oct_of(KDF9.Q_part(the_buffer.number), 2);
      end if;
      if the_CPDAR(the_buffer.number) = 0 and the_CPU_state /= Director_state then
         trap_invalid_instruction("attempt to use unallocated I/O device #"
                                & oct_of(the_buffer.number));
      end if;
   end validate_device;

   procedure validate_bounds (Q_operand  : in KDF9.Q_register) is
   begin
      if Q_operand.I > Q_operand.M then
         trap_invalid_instruction("invalid I/O Q operand: I > M");
      end if;
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
         trap_invalid_instruction("reading past end of data, or a parity error, on "
                                & the_buffer.device_name);
      end if;
   end validate_parity;

   procedure require_nonnegative_count (count : in KDF9.Q_part) is
   begin
      if resign(count) < 0 then
         trap_invalid_instruction("negative I/O repetition count");
      end if;
   end require_nonnegative_count;

   procedure require_positive_count (count : in KDF9.Q_part) is
   begin
      if resign(count) <= 0 then
         trap_invalid_instruction("nonpositive I/O repetition count");
      end if;
   end require_positive_count;

   procedure start_timed_transfer (the_buffer   : in out IOC.device'Class;
                                   Q_operand    : in KDF9.Q_register;
                                   set_offline  : in Boolean;
                                   busy_time    : in KDF9.microseconds;
                                   is_DMAing    : in Boolean := True) is
      pragma Unreferenced(set_offline);
   begin
      -- Check the IO parameters and the buffer state, and handle any old lockout.
      validate_device(the_buffer, Q_operand);
      if start_timed_transfer.is_DMAing then
         validate_bounds(Q_operand);
      else
         require_nonnegative_count(Q_operand.M);
      end if;
      validate_parity(the_buffer);
      if the_buffer.is_busy then
         handle_a_buffer_lockout(the_buffer);
      end if;
      if start_timed_transfer.is_DMAing          and then
            KDF9.store.test_lockouts(Q_operand) /= 0 then
         LOV_if_user_mode;
      end if;
      -- Set up the transfer parameters.
      the_buffer.is_for_Director := (the_CPU_state = Director_state);
      the_buffer.priority_level  := CPL;
      the_buffer.control_word    := Q_operand;
      the_buffer.is_DMAing       := is_DMAing;
      the_buffer.order_count     := ICR+1;
      the_buffer.order_address   := CIA;
      the_buffer.decoded_order   := INS;
      the_buffer.initiation_time := the_clock_time;
      the_buffer.transfer_time   := busy_time;
      the_buffer.completion_time := the_buffer.initiation_time + busy_time;
      if busy_time > 0 or is_DMAing then
         if the_buffer.completion_time < the_next_interrupt_time then
            the_next_interrupt_time := the_buffer.completion_time;
         end if;
         the_buffer.is_busy := True;
         take_note_of (start_transfer,
                       the_buffer.order_count,
                       the_buffer.order_address,
                       the_buffer.decoded_order,
                       the_buffer.initiation_time,
                       the_buffer.device_name,
                       the_buffer.completion_time,
                       the_buffer.is_for_Director,
                       the_buffer.priority_level,
                       the_buffer.control_word
                      );
      else -- Take note of, e.g., a disc seek, which does not busy the buffer.
         the_buffer.is_busy := False;
         take_note_of (finis_transfer,
                       the_buffer.order_count,
                       the_buffer.order_address,
                       the_buffer.decoded_order,
                       the_buffer.initiation_time,
                       the_buffer.device_name,
                       the_buffer.completion_time,
                       the_buffer.is_for_Director,
                       the_buffer.priority_level,
                       the_buffer.control_word
                      );
      end if;
      PHU(the_buffer.priority_level) := idle_PHU;
   end start_timed_transfer;

   -- initialize_byte_mode_transfer takes a pessimistic view of transfers-to-End_Message.

   -- When the actual transfer length is known, the end-of-transfer time can be
   --    made more realistic by specifying its real length to correct_transfer_time.

   -- correct_transfer_time must be called before finalize_transfer is called.

   procedure correct_transfer_time (the_buffer  : in out IOC.device'Class;
                                    actual_time : in KDF9.microseconds) is
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

   procedure finalize_transfer (the_buffer : in out IOC.device'Class;
                                need_EDT,
                                need_PR    : out Boolean) is
   begin
      if the_buffer.transfer_time /= 0 then
         take_note_of (finis_transfer,
                       the_buffer.order_count,
                       the_buffer.order_address,
                       the_buffer.decoded_order,
                       the_buffer.initiation_time,
                       the_buffer.device_name,
                       the_buffer.completion_time,
                       the_buffer.is_for_Director,
                       the_buffer.priority_level,
                       the_buffer.control_word
                      );
      end if;
      need_EDT := the_buffer.is_for_Director;
      need_PR  := the_buffer.priority_level < CPL;
      PHU(the_buffer.priority_level) := idle_PHU;
      if the_execution_mode = boot_mode then
         -- Test for possible priority inversion, i.e. other program(s) blocked on this buffer.
         for p in PHU'Range loop
            if PHU(p).is_held_up                               and then
                  PHU(p).blockage.reason = buffer_busy         and then
                     PHU(p).blockage.buffer_nr = the_buffer.number then
               -- The EE paper "KDF9 TIME-SHARING DIRECTOR SUPPORT DOCUMENTATION" of 1-May-1965
               --   says such a PHU is NOT cleared, but EDT is requested INSTEAD of PR,
               --   so Director can take action according to what it finds there.
               need_EDT := True;
               need_PR  := False;
            end if;
         end loop;
      end if;
      if the_buffer.is_DMAing then
         clear_lockouts(the_buffer.control_word);
      end if;
      the_buffer.is_busy         := False;
      the_buffer.is_for_Director := False;
      the_buffer.completion_time := KDF9.microseconds'Last;
   end finalize_transfer;

   procedure act_on_pending_interrupts is
      EDT_needed, PR_needed : Boolean := False;
   begin
      advance_the_clock_past(the_next_interrupt_time);
      -- Predict another interrupt (at most 1 second in the future in Director).
      the_next_interrupt_time := KDF9.microseconds'Last;
      for b in buffer'Range loop
         if buffer(b) /= null and then
               buffer(b).is_busy  then
            if the_clock_time >= buffer(b).completion_time then
               finalize_transfer(buffer(b).all, EDT_needed, PR_needed);
            elsif the_next_interrupt_time > buffer(b).completion_time then
               the_next_interrupt_time := buffer(b).completion_time;
            end if;
         end if;
      end loop;
      if the_execution_mode = boot_mode and then
            the_next_interrupt_time > the_clock_time + 1_000_000 then
         the_next_interrupt_time := the_clock_time + 1_000_000;
      end if;
      if EDT_needed then
         signal_interrupt(EDT_flag);
      elsif PR_needed then
         signal_interrupt(PR_flag);
      end if;
   end act_on_pending_interrupts;

   -- Advance the time to a point after all extant transfer have terminated,
   --    finalizing all extant transfer in temporal order in the process.
   procedure complete_all_extant_transfers is
      EDT_needed,
      PR_needed      : Boolean := False;
      last_time      : KDF9.microseconds := 0;
      next_time      : KDF9.microseconds;
   begin
      -- At least one transfer is terminated each time around outer_loop,
      --    if not, outer_loop is exited.
   outer_loop:
      for c in buffer'Range loop
         -- Find the earliest transfer termination time.
         next_time := KDF9.microseconds'Last;
         for b in buffer'Range loop
            if buffer(b) /= null and then
                  buffer(b).is_busy and then
                     buffer(b).completion_time < next_time then
               next_time := buffer(b).completion_time;
            end if;
         end loop;

         if next_time = KDF9.microseconds'Last then
            -- All the buffers are quiescent.
            exit outer_loop;
         else
            -- At least one transfer remains to be finalized.
            advance_the_clock_past(next_time);
            last_time := KDF9.microseconds'Max(last_time, next_time);
         end if;

         -- Finalize all transfers with completion time <= next_time.
         for b in buffer'Range loop
            if buffer(b) /= null and then
                  buffer(b).is_busy and then
                     buffer(b).completion_time <= next_time then
               finalize_transfer(buffer(b).all, EDT_needed, PR_needed);
            end if;
         end loop;

      end loop outer_loop;
   end complete_all_extant_transfers;

   procedure handle_a_buffer_lockout (the_buffer : in IOC.device'Class) is
      the_start_time : constant KDF9.microseconds := the_clock_time;
   begin
      PHU(CPL) := (is_held_up => True,
                   blockage   => (buffer_busy, the_buffer.number, INTQq_wait => 0));
      take_note_of (buffer_lockout, ICR, CIA, INS,
                    initiation_time => the_start_time,
                    device_name     => the_buffer.device_name
                   );
      if the_execution_mode = boot_mode then
         LOV_if_user_mode;
      else
         advance_the_clock_past(the_buffer.completion_time);
         act_on_pending_interrupts;
      end if;
   end handle_a_buffer_lockout;

   function the_buffer_responsible_for (address : KDF9.Q_part)
   return KDF9.buffer_number is
      candidate_found  : Boolean := False;
      candidate_time   : KDF9.microseconds := KDF9.microseconds'First;
      candidate_number : KDF9.buffer_number;
   begin
      -- Select the buffer active in the_group;
      --    if there is more than one, choose the buffer with the latest completion time.
      -- The latter case should not arise in practice, but is allowed by the hardware.
      for b in buffer'Range loop
         if buffer(b) /= null                                      and then
               buffer(b).is_busy                                   and then
                  buffer(b).completion_time > candidate_time       and then
                     group(address) in group(buffer(b).control_word.I)
                                    .. group(buffer(b).control_word.M) then
            candidate_number := b;
            candidate_time   := buffer(b).completion_time;
            candidate_found  := True;
         end if;
      end loop;
      if candidate_found then
         return candidate_number;
      else
         raise emulation_failure with "cannot identify any buffer responsible for this lockout";
      end if;
   end the_buffer_responsible_for;

   procedure handle_a_main_store_lockout (address : in KDF9.Q_part) is
      the_start_time : constant KDF9.microseconds := the_clock_time;
      the_buffer     : constant KDF9.buffer_number := the_buffer_responsible_for(address);
   begin
      PHU(CPL) := (is_held_up => True,
                   blockage   => (locked_out, group_address(group(address))));
      -- Store access LOV interrupts invoke instruction restart outside Director.
      if the_execution_mode = boot_mode then
         LOV_if_user_mode;
      else
         set_NIA_to(CIA);
         take_note_of (store_lockout, ICR, CIA, INS,
                       initiation_time => the_start_time,
                       device_name     => logical_device_name_of(buffer(the_buffer).all)
                      );
         advance_the_clock_past(buffer(the_buffer).completion_time);
         act_on_pending_interrupts;
      end if;
      ICR := ICR + 1;
   end handle_a_main_store_lockout;

   procedure CTQ (the_buffer  : in out IOC.device'Class;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      EDT_needed, PR_needed : Boolean := False;
   begin
      validate_device(the_buffer, Q_operand);
      take_note_of(Q_operand,
                   the_buffer.device_name,
                   KDF9.word(Boolean'Pos(the_buffer.is_busy))
                  );
      if the_buffer.is_busy then
         the_buffer.completion_time := the_clock_time;
         finalize_transfer(the_buffer, EDT_needed, PR_needed);
      end if;
      the_buffer.is_abnormal := False;
      the_buffer.is_offline  := set_offline;
   end CTQ;

   procedure INT (the_buffer  : in IOC.device'Class;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
      the_start_time : constant KDF9.microseconds := the_clock_time;
   begin
      validate_device(the_buffer, Q_operand);
      take_note_of(Q_operand,
                   the_buffer.device_name,
                   KDF9.word(Boolean'Pos(the_buffer.is_busy))
                  );
      if the_buffer.is_busy then
         PHU(CPL) := (is_held_up => True,
                      blockage   => (buffer_busy, the_buffer.number, INTQq_wait => 1));
         take_note_of (buffer_lockout, ICR, CIA, INS,
                       initiation_time => the_start_time,
                       device_name     => the_buffer.device_name
                      );
         if the_execution_mode = boot_mode then
            signal_interrupt(PR_flag);
         else
            advance_the_clock_past(the_buffer.completion_time);
            act_on_pending_interrupts;
         end if;
      end if;
   end INT;

   procedure BUSY (the_buffer  : in IOC.device'Class;
                   Q_operand   : in KDF9.Q_register;
                   set_offline : in Boolean;
                   result      : out KDF9.word) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_buffer, Q_operand);
      result := KDF9.word(Boolean'Pos(the_buffer.is_busy));
      take_note_of(Q_operand, the_buffer.device_name, result);
      if the_buffer.is_busy and then
            (the_execution_mode /= boot_mode or the_CPU_state = Director_state) then
         act_on_pending_interrupts;
      end if;
   end BUSY;

   procedure PAR (the_buffer  : in out IOC.device'Class;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean;
                  result      : out KDF9.word) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_buffer, Q_operand);
      if the_buffer.is_busy then
         handle_a_buffer_lockout(the_buffer);
      end if;
      result := KDF9.word(Boolean'Pos(the_buffer.is_abnormal));
      take_note_of(Q_operand, the_buffer.device_name, result);
      the_buffer.is_abnormal := False;
   end PAR;

   procedure PIA (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PIA cannot be used on " & the_buffer.device_name);
   end PIA;

   procedure PIB (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PIB cannot be used on " & the_buffer.device_name);
   end PIB;

   procedure PIC (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PIC cannot be used on " & the_buffer.device_name);
   end PIC;

   procedure PID (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PID cannot be used on " & the_buffer.device_name);
   end PID;

   procedure PIE (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PIE cannot be used on " & the_buffer.device_name);
   end PIE;

   procedure PIF (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PIF cannot be used on " & the_buffer.device_name);
   end PIF;

   procedure PIG (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PIG cannot be used on " & the_buffer.device_name);
   end PIG;

   procedure PIH (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PIH cannot be used on " & the_buffer.device_name);
   end PIH;

   procedure PMA (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PMA cannot be used on " & the_buffer.device_name);
   end PMA;

   procedure PMB (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_buffer, Q_operand);
      validate_parity(the_buffer);
   end PMB;

   procedure PMC (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_buffer, Q_operand);
      validate_parity(the_buffer);
   end PMC;

   procedure PMD (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PMD cannot be used on " & the_buffer.device_name);
   end PMD;

   procedure PME (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PME cannot be used on " & the_buffer.device_name);
   end PME;

   procedure PMF (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_buffer, Q_operand);
      validate_parity(the_buffer);
   end PMF;

   procedure PMG (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PMG cannot be used on " & the_buffer.device_name);
   end PMG;

   procedure PMH (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PMH cannot be used on " & the_buffer.device_name);
   end PMH;

   procedure PMK (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PMK cannot be used on " & the_buffer.device_name);
   end PMK;

   procedure PML (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("PML cannot be used on " & the_buffer.device_name);
   end PML;

   procedure POA (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("POA cannot be used on " & the_buffer.device_name);
   end POA;

   procedure POB (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("POB cannot be used on " & the_buffer.device_name);
   end POB;

   procedure POC (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("POC cannot be used on " & the_buffer.device_name);
   end POC;

   procedure POD (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("POD cannot be used on " & the_buffer.device_name);
   end POD;

   procedure POE (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("POE cannot be used on " & the_buffer.device_name);
   end POE;

   procedure POF (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("POF cannot be used on " & the_buffer.device_name);
   end POF;

   procedure POG (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("POG cannot be used on " & the_buffer.device_name);
   end POG;

   procedure POH (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("POH cannot be used on " & the_buffer.device_name);
   end POH;

   procedure POK (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("POK cannot be used on " & the_buffer.device_name);
   end POK;

   procedure POL (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_transfer(the_buffer, Q_operand);
      validate_parity(the_buffer);
      trap_invalid_instruction("POL cannot be used on " & the_buffer.device_name);
   end POL;

   procedure close (the_buffer  : in out IOC.byte_device;
                    the_action  : in String;
                    the_amount  : in KDF9.word;
                    the_quantum : in String) is
   begin
      if the_final_state_is_wanted and then
            the_buffer.is_open and then
               the_amount /= 0 then
         if (the_buffer.number = 0) and not (API_logging_is_requested or the_log_is_wanted) then
            output_line("");  -- Take a new line at the head of the list, for low-visibility modes.
         end if;
         output_line(the_buffer.device_name
                   & " on buffer #"
                   & oct_of(KDF9.Q_part(the_buffer.number), 2)
                   & " "
                   & the_action
                   & KDF9.word'Image(the_amount)
                   & " "
                   & the_quantum
                   & ".");
      end if;
      IOC.device(the_buffer).close;
   end close;

   function atomic_item_count (the_buffer : IOC.byte_device;
                               Q_operand  : KDF9.Q_register)
   return KDF9.word is
      words : constant KDF9.Q_part := Q_operand.M - Q_operand.I + 1;
   begin
      if the_buffer.is_open then
         return KDF9.word(words) * 8;
      else
         return 0;
      end if;
   end atomic_item_count;

   procedure reattach (the_buffer   : in out IOC.byte_device;
                       the_file     : in String) is
   begin
      reattach(the_buffer.stream, the_file, read_mode);
   end reattach;

   procedure deal_with_end_of_data (the_buffer : in out IOC.byte_device) is
      BELL     : constant String := (1 => Character'Val(7));   -- Audible prompt
      response : Character;
   begin
      output_line(BELL);
      output_line("ee9: End of data for " & the_buffer.device_name & ". ");
      prompt("To append a file give its identifying letter, or RETURN: ",
             response,
             default => '|');
      case response is
         when 'a' .. 'z' | 'A' .. 'Z' =>
            reattach(the_buffer, the_buffer.device_name & response);
         when others =>
            the_buffer.is_abnormal := True;
            raise end_of_stream;
      end case;
   end deal_with_end_of_data;

   procedure initialize_byte_mode_gapping (the_buffer   : in out IOC.byte_device;
                                           Q_operand    : in KDF9.Q_register;
                                           set_offline  : in Boolean) is
      time_needed : constant KDF9.microseconds
                  := IO_elapsed_time(the_buffer, KDF9.word(Q_operand.M));
   begin
      require_positive_count(Q_operand.M);
      start_timed_transfer(the_buffer, Q_operand, set_offline,
                           busy_time => time_needed,
                           is_DMAing => False);
   end initialize_byte_mode_gapping;

   procedure initialize_byte_mode_transfer (the_buffer   : in out IOC.byte_device;
                                            Q_operand    : in KDF9.Q_register;
                                            set_offline  : in Boolean) is
      atomic_items : constant KDF9.word := atomic_item_count(the_buffer, Q_operand);
      time_needed  : constant KDF9.microseconds := IO_elapsed_time(the_buffer, atomic_items);
   begin
      start_timed_transfer(the_buffer, Q_operand, set_offline,
                           busy_time => time_needed,
                           is_DMAing => True);
   end initialize_byte_mode_transfer;

   procedure get_char_from_stream (char       : out Character;
                                   the_buffer : in out IOC.byte_device;
                                   size       : in out KDF9.word) is
   begin
      loop
         begin
            get_char(char, the_buffer.stream);
            return;
         exception
            when end_of_stream =>
               add_in_the_IO_CPU_time(the_buffer, size);
               correct_transfer_time(the_buffer, size);
               the_buffer.byte_count := the_buffer.byte_count + size;
               size := 0;
               deal_with_end_of_data(the_buffer);
         end;
      end loop;
   end get_char_from_stream;

   overriding
   function IO_elapsed_time_total (the_buffer : IOC.unit_record_device)
   return KDF9.microseconds is
   begin
      return IO_elapsed_time(the_buffer, the_buffer.unit_count);
   end IO_elapsed_time_total;

   overriding
   function atomic_item_count (the_buffer : IOC.unit_record_device;
                               Q_operand  : KDF9.Q_register)
   return KDF9.word is
      pragma Unreferenced(the_buffer);
      pragma Unreferenced(Q_operand);
   begin
      return 1;
   end atomic_item_count;

end IOC;
