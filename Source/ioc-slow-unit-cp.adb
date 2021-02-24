-- Emulation of a card punch buffer.
--
-- This file is part of ee9 (6.1a), the GNU Ada emulator of the English Electric KDF9.
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

package body IOC.slow.unit.CP is

   use KDF9_char_sets;

   overriding
   procedure Initialize (the_CP : in out CP.device) is
   begin
      open(the_CP, write_mode);
   end Initialize;

   procedure do_output_housekeeping (the_CP     : in out CP.device;
                                     fetched    : in KDF9.word) is
   begin
      correct_transfer_time(the_CP, actual_length => 1);
      add_in_the_IO_CPU_time(the_CP, fetched);
   end do_output_housekeeping;

   procedure write_card (the_CP        : in out CP.device;
                         Q_operand     : in KDF9.Q_register;
                         max_words     : in KDF9.address;
                         writing_to_EM : in Boolean := False) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size : KDF9.word := 0;
      char : Character;
      byte : KDF9_char_sets.symbol;
   begin
      check_addresses_and_lockouts(start_address, end_address);
   word_loop:
      for w in start_address .. KDF9.address'Min(end_address, start_address+max_words-1) loop
         for c in KDF9_char_sets.symbol_index'Range loop
            byte := fetch_symbol(w, c);
            size := size + 1;
            char := to_CP(byte);
            put_byte(char, the_CP.stream);
            exit word_loop when writing_to_EM and char = KDF9_char_sets.E_M;
         end loop;
      end loop word_loop;
      put_EOL(the_CP.stream);
      the_CP.unit_count := the_CP.unit_count + 1;
      do_output_housekeeping(the_CP, fetched => size);
   end write_card;

   procedure words_write_card (the_CP        : in out CP.device;
                               Q_operand     : in KDF9.Q_register;
                               max_words     : in KDF9.address;
                               writing_to_EM : in Boolean := False) is

      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size : KDF9.word := 0;
      char : Character;
      byte : KDF9_char_sets.symbol;
   begin
      check_addresses_and_lockouts(start_address, end_address);
      for w in start_address .. KDF9.address'Min(end_address, start_address+max_words-1) loop
         byte := KDF9_char_sets.symbol(fetch_word(w) and 8#77#);
         size := size + 1;
         char := to_CP(byte);
         put_byte(char, the_CP.stream);
      exit when writing_to_EM and char = KDF9_char_sets.E_M;
      end loop;
      put_EOL(the_CP.stream);
      the_CP.unit_count := the_CP.unit_count + 1;
      do_output_housekeeping(the_CP, fetched => size);
   end words_write_card;

   overriding
   procedure POA (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CP, Q_operand, set_offline, output_operation);
      write_card(the_CP, Q_operand, max_words => 20);
      lock_out_relative_addresses(Q_operand);
   end POA;

   overriding
   procedure POB (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CP, Q_operand, set_offline, output_operation);
      write_card(the_CP, Q_operand, max_words => 20, writing_to_EM => True);
      lock_out_relative_addresses(Q_operand);
   end POB;

   overriding
   procedure POC (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CP, Q_operand, set_offline, output_operation);
      words_write_card(the_CP, Q_operand, max_words => 160);
      lock_out_relative_addresses(Q_operand);
   end POC;

   overriding
   procedure POD (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CP, Q_operand, set_offline, output_operation);
      words_write_card(the_CP, Q_operand, max_words => 160, writing_to_EM => True);
      lock_out_relative_addresses(Q_operand);
   end POD;

   overriding
   procedure POE (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POC(the_CP, Q_operand, set_offline);
   end POE;

   overriding
   procedure POF (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POA(the_CP, Q_operand, set_offline);
   end POF;

   overriding
   procedure POG (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CP, Q_operand, set_offline, output_operation);
      write_card(the_CP, Q_operand, max_words => 10, writing_to_EM => False);
      lock_out_relative_addresses(Q_operand);
   end POG;

   overriding
   procedure POH (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CP, Q_operand, set_offline, output_operation);
      write_card(the_CP, Q_operand, max_words => 10, writing_to_EM => True);
      lock_out_relative_addresses(Q_operand);
   end POH;

   overriding
   procedure POK (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CP, Q_operand, set_offline, output_operation);
      -- See the Manual, p289.
      words_write_card(the_CP, Q_operand, max_words => 80, writing_to_EM => True);
      lock_out_relative_addresses(Q_operand);
   end POK;

   overriding
   procedure POL (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CP, Q_operand, set_offline, output_operation);
      -- See the Manual, p289.
      words_write_card(the_CP, Q_operand, max_words => 80, writing_to_EM => False);
      lock_out_relative_addresses(Q_operand);
   end POL;

   overriding
   procedure Finalize (the_CP : in out CP.device) is
   begin
      close(
            the_CP,
            "punched",
            the_CP.unit_count,
            "card" & plurality(the_CP.unit_count)
           );
   end Finalize;

   type CP_access is access CP.device;

   CP0 : CP_access with Warnings => Off;
   CP1 : CP_access with Warnings => Off;

   unit : IOC.unit_number := 0;

   procedure enable (b : in KDF9.buffer_number) is
   begin
      case unit is
         when 0 =>
            CP0 := new CP.device (number => b, unit => 0);
            CP0_number := b;
         when 1 =>
            CP1 := new CP.device (number => b, unit => 1);
            CP1_number := b;
         when others =>
            trap_operator_error("more than two CP units have been configured");
      end case;
      unit := unit + 1;
   end enable;

end IOC.slow.unit.CP;
