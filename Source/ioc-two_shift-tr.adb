-- ioc-two_shift-tr.adb
--
-- Emulation of a paper tape reader buffer.
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

with IO;
with IOC; pragma Elaborate_All(IOC);
with IOC.two_shift;
with KDF9.TOD_clock;
with KDF9.Directors;
with KDF9.store;
with settings;

pragma Unreferenced(IO);

use  KDF9.TOD_clock;
use  KDF9.store;
use  settings;

package body IOC.two_shift.TR is

   overriding
   procedure Initialize (the_TR     : in out TR.device) is
   begin
      if the_TR.unit = 0 then
         -- Use the emulator's standard input.
         open(the_TR, read_mode, attaching => True, to_fd => 0);
      else
         -- Open the associated file.
         open(the_TR, read_mode, attaching => False);
      end if;
      the_TR.current_case := KDF9.Case_Normal;
   end Initialize;

   overriding
   procedure Finalize (the_TR : in out TR.device) is
   begin
      close(the_TR, "read", the_TR.byte_count, "character(s)");
   end Finalize;

   -- Read 8-bit bytes, and pack into words without transformation.
   procedure read_bytes_verbatim (the_TR    : in out TR.device;
                                         Q_operand : in KDF9.Q_register) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size  : KDF9.word := 0;
      w     : KDF9.code_location := KDF9.code_location(start_address);
      s     : KDF9.syllable_code range 0 .. 5 := 0;
      char  : Character;
      octet : KDF9.syllable;
   begin
      loop
         get_byte(char, the_TR.stream);
         octet := KDF9.syllable(Character'Pos(char));
         store_syllable(octet, (s, w));
         if s < 5 then
            s := s + 1;
         else
            s := 0;
            w := w + 1;
         end if;
         size := size + 1;
         exit when w > KDF9.code_location(end_address);
      end loop;
      add_in_the_IO_CPU_time(the_TR, size);
      correct_transfer_time(the_TR, KDF9.word'(0));
   exception
      when end_of_stream =>
         add_in_the_IO_CPU_time(the_TR, size);
         correct_transfer_time(the_TR, KDF9.word'(0));
   end read_bytes_verbatim;

   -- Read 8-bit bytes, compress to 6-bit characters, and pack into words.
   procedure read_orders (the_TR    : in out TR.device;
                          Q_operand : in KDF9.Q_register) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size   : KDF9.word := 0;
      w      : KDF9.address := start_address;
      c      : KDF9.symbol_number := 0;
      char   : Character;
      octet  : KDF9.syllable;
      symbol : KDF9.symbol;
   begin
      validate_range_access(start_address, end_address);
   word_loop:
      loop
         loop
            get_byte(char, the_TR.stream);
         exit when Character'Pos(char) /= 0 and Character'Pos(char) /= 255;
         end loop;
         octet  := KDF9.syllable(Character'Pos(char));
         symbol := KDF9.symbol((octet and 16#6_0#)/2 or (octet and 16#F#));
         store_symbol(symbol, w, c);
         if c < KDF9.symbol_number'Last then
            c := c + 1;
         else
            c := 0;
            w := w + 1;
         end if;
         size := size + 1;
         exit word_loop when w > end_address;
      end loop word_loop;
      add_in_the_IO_CPU_time(the_TR, size);
   exception
      when end_of_stream =>
         add_in_the_IO_CPU_time(the_TR, size);
         correct_transfer_time(the_TR, size);
         the_TR.is_abnormal := True;
         diagnose(the_TR.stream, "premature EOF in read_orders");
   end read_orders;

   -- Like read_orders, but transfer to End_Message.
   procedure read_orders_to_EM (the_TR    : in out TR.device;
                                Q_operand : in KDF9.Q_register) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size   : KDF9.word := 0;
      w      : KDF9.address := start_address;
      c      : KDF9.symbol_number := 0;
      char   : Character;
      octet  : KDF9.syllable;
      symbol : KDF9.symbol;
   begin
      validate_range_access(start_address, end_address);
   word_loop:
      loop
         loop
            get_byte(char, the_TR.stream);
         exit when Character'Pos(char) /= 0 and Character'Pos(char) /= 255;
         end loop;
         octet  := KDF9.syllable(Character'Pos(char));
         symbol := KDF9.symbol((octet and 16#6_0#)/2 or (octet and 16#F#));
         store_symbol(symbol, w, c);
         if c < KDF9.symbol_number'Last then
            c := c + 1;
         else
            c := 0;
            w := w + 1;
         end if;
         size := size + 1;
      exit word_loop when w > end_address;
         if symbol = KDF9.End_Message then
            for d in 1 .. 7-c loop
               store_symbol(KDF9.Blank_Space, w, c+d);
            end loop;
            exit word_loop;
         end if;
      end loop word_loop;
      add_in_the_IO_CPU_time(the_TR, size);
      correct_transfer_time(the_TR, size);
   exception
      when end_of_stream =>
         add_in_the_IO_CPU_time(the_TR, size);
         correct_transfer_time(the_TR, size);
         the_TR.is_abnormal := True;
         diagnose(the_TR.stream, "premature EOF in read_orders_to_EM");
   end read_orders_to_EM;

   -- PRQq
   overriding
   procedure PIA (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_TR, Q_operand, set_offline);
      if the_execution_mode = boot_mode then
         read_orders(the_TR, Q_operand);
      else
         read(the_TR, Q_operand);
      end if;
      set_lockouts(Q_operand);
   end PIA;

   -- PREQq
   overriding
   procedure PIB (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_TR, Q_operand, set_offline);
      if the_execution_mode = boot_mode then
         read_orders_to_EM(the_TR, Q_operand);
      else
         read_to_EM(the_TR, Q_operand);
      end if;
      set_lockouts(Q_operand);
   end PIB;

   -- PRCQq
   overriding
   procedure PIC (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_TR, Q_operand, set_offline);
      words_read(the_TR, Q_operand);
      set_lockouts(Q_operand);
   end PIC;

   -- PRCEQq
   overriding
   procedure PID (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_TR, Q_operand, set_offline);
      words_read_to_EM(the_TR, Q_operand);
      set_lockouts(Q_operand);
   end PID;

   -- as PIA
   overriding
   procedure PIE (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIA(the_TR, Q_operand, set_offline);
   end PIE;

   -- as PIB
   overriding
   procedure PIF (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIB(the_TR, Q_operand, set_offline);
   end PIF;

   -- as PIC
   overriding
   procedure PIG (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIC(the_TR, Q_operand, set_offline);
   end PIG;

   -- as PID
   overriding
   procedure PIH (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PID(the_TR, Q_operand, set_offline);
   end PIH;

   -- the_T_bit := (the reader is set to 8-track mode);
   --    it is always in 8-track mode, as 5-track input is not supported.
   overriding
   procedure PMB (the_TR      : in out TR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_TR, Q_operand);
      validate_parity(the_TR);
      the_T_bit := 1;
      take_note_of(Q_operand, the_TR.device_name, the_T_bit);
   end PMB;

   TR_quantum : constant := 1E6 / 1_000;  -- 1000 characters per second.

   --
   -- TR0 is the hardware bootstrap device for reading initial orders.
   --

   TR0 : aliased TR.device (TR0_number,
                            kind    => TR_kind,
                            unit    => 0,
                            quantum => TR_quantum,
                            is_slow => True);

   TR1 : aliased TR.device (TR1_number,
                            kind    => TR_kind,
                            unit    => 1,
                            quantum => TR_quantum,
                            is_slow => True);
   pragma Unreferenced(TR1);

   procedure load_a_program is
      store_limit : constant := 32767 * 2**24;
      descriptor  : constant KDF9.Q_register := (C => TR0.number, I => 0, M => 8191);
   begin
      if is_unallocated(buffer(TR0_number)) then
         set_state_of(buffer(TR0_number), allocated => True);
      end if;
      initialize_byte_mode_transfer(TR0, descriptor, set_offline => False);
      read_bytes_verbatim(TR0, descriptor);
      complete_all_extant_transfers;  -- To get an accurate elapsed time.
      save_the_initial_jump;
       -- Set the store limit in E1L.
      store_halfword(store_limit, 1, 1);
      store_word(todays_date_28n_years_ago, 7);
      -- Set the time iff we are not computing a signature, to get a repeatable hash.
      if not the_signature_is_enabled then
         KDF9.Directors.set_the_time_of_loading(the_time_of_day);
      end if;
      set_state_of(buffer(TR0_number), allocated => False);
      reattach_TR0("TR0");
      clear_IOC_FIFO;
   end load_a_program;

   procedure bootstrap_the_KDF9 is
      descriptor  : constant KDF9.Q_register := (C => TR0.number, I => 0, M => 8);
   begin
      read_orders(TR0, descriptor);
      clear_IOC_FIFO;
   end bootstrap_the_KDF9;

   overriding
   procedure reattach (the_buffer  : in out TR.device;
                       to_the_file : in String) is
   begin
      IOC.byte_device(the_buffer).reattach(to_the_file);
      the_buffer.current_case := KDF9.Case_Normal;
   end reattach;

   procedure reattach_TR0 (to_the_file : in String) is
   begin
      reattach(TR0, to_the_file);
   end reattach_TR0;

end IOC.two_shift.TR;
