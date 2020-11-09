-- ioc-cp.adb
--
-- Emulation of a card punch buffer.
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

with IOC; pragma Elaborate_All(IOC);
with KDF9.store;


use  KDF9.store;

package body IOC.CP is

   pragma Unsuppress(All_Checks);

   overriding
   procedure Initialize (the_CP : in out CP.device) is
   begin
      open(the_CP, write_mode, attaching => False);
   end Initialize;

   overriding
   procedure Finalize (the_CP : in out CP.device) is
   begin
      close(the_CP, "punched", the_CP.unit_count, "card(s)");
   end Finalize;

   procedure do_output_housekeeping (the_CP     : in out CP.device;
                                    fetched    : in KDF9.word) is
   begin
      flush(the_CP.stream);
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
      byte : KDF9.symbol;
   begin
      validate_range_access(start_address, end_address);
   word_loop:
      for w in start_address .. KDF9.address'Min(end_address, start_address+max_words-1) loop
         for c in KDF9.symbol_number'Range loop
            byte := fetch_symbol(w, c);
            size := size + 1;
            char := to_CP(byte);
            put_byte(char, the_CP.stream);
            exit word_loop when writing_to_EM and char = KDF9.E_M;
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
      byte : KDF9.symbol;
   begin
      validate_range_access(start_address, end_address);
      for w in start_address .. KDF9.address'Min(end_address, start_address+max_words-1) loop
         byte := KDF9.symbol(fetch_word(w) and 8#77#);
         size := size + 1;
            char := to_CP(byte);
         put_byte(char, the_CP.stream);
      exit when writing_to_EM and char = KDF9.E_M;
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
      initialize_byte_mode_transfer(the_CP, Q_operand, set_offline);
      write_card(the_CP, Q_operand, max_words => 160);
      set_lockouts(Q_operand);
   end POA;

   overriding
   procedure POB (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CP, Q_operand, set_offline);
      write_card(the_CP, Q_operand, max_words => 160, writing_to_EM => True);
      set_lockouts(Q_operand);
   end POB;

   overriding
   procedure POC (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CP, Q_operand, set_offline);
      words_write_card(the_CP, Q_operand, max_words => 160);
      set_lockouts(Q_operand);
   end POC;

   overriding
   procedure POD (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CP, Q_operand, set_offline);
      words_write_card(the_CP, Q_operand, max_words => 160, writing_to_EM => True);
      set_lockouts(Q_operand);
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
      initialize_byte_mode_transfer(the_CP, Q_operand, set_offline);
      write_card(the_CP, Q_operand, max_words => 80, writing_to_EM => False);
      set_lockouts(Q_operand);
   end POG;

   overriding
   procedure POH (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CP, Q_operand, set_offline);
      write_card(the_CP, Q_operand, max_words => 80, writing_to_EM => True);
      set_lockouts(Q_operand);
   end POH;

   overriding
   procedure POK (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CP, Q_operand, set_offline);
      -- See the Manual, p289.
      words_write_card(the_CP, Q_operand, max_words => 80, writing_to_EM => True);
      set_lockouts(Q_operand);
   end POK;

   overriding
   procedure POL (the_CP      : in out CP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CP, Q_operand, set_offline);
      -- See the Manual, p289.
      words_write_card(the_CP, Q_operand, max_words => 80, writing_to_EM => False);
      set_lockouts(Q_operand);
   end POL;

   CP_quantum : constant := 1E6 / (300 / 60); -- 300 cards per minute.

   CP0 : aliased CP.device (number  => CP0_number,
                            kind    => CP_kind,
                            unit    => 0,
                            quantum => CP_quantum,
                            is_slow => True);
   pragma Unreferenced(CP0);

end IOC.CP;
