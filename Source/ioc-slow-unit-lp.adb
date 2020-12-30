-- ioc-slow-unit-lp.adb
--
-- Emulation of a lineprinter buffer.
--
-- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2020, W. Findlay; all rights reserved.
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

with IOC.equipment;

use  IOC.equipment;

package body IOC.slow.unit.LP is

   use KDF9_char_sets;

   overriding
   procedure Initialize (the_LP : in out LP.device) is
   begin
      open(the_LP, write_mode);
   end Initialize;

   max_LP_line_length : constant := 160;  -- This is a hardware limit.
   max_LP_page_length : constant :=  66;  -- This is the length of a page of standard stationery.

   -- The number of lines traversed by paper motion with a standard control loop.
   function skip_length (the_LP : LP.device; symbol : KDF9_char_sets.symbol)
   return KDF9.word
   is (
       if symbol = KDF9_char_sets.Page_Change
       then max_LP_page_length - the_LP.unit_count mod max_LP_page_length
       else 1
      );

   procedure do_output_housekeeping (the_LP   : in out LP.device;
                                     old_count,
                                     fetched  : in KDF9.word) is
   begin
      flush(the_LP.stream);
      correct_transfer_time(the_LP, IO_elapsed_time(the_LP, the_LP.unit_count-old_count));
      add_in_the_IO_CPU_time(the_LP, fetched);
   end do_output_housekeeping;

   next_column : Natural := 0;

   procedure print (symbol : in KDF9_char_sets.symbol; the_LP : in out LP.device) is
      char : constant Character := to_LP(symbol);
   begin
      if char /= KDF9_char_sets.W_F then
         if symbol in KDF9_char_sets.Line_Shift | KDF9_char_sets.Page_Change then
            the_LP.unit_count := the_LP.unit_count + skip_length(the_LP, symbol);
            put_char(char, the_LP.stream);
            next_column := 0;
         elsif next_column < max_LP_line_length then
            next_column := next_column + 1;
            put_char(char, the_LP.stream);
         end if;
      end if;
   end print;

   -- It is unclear what should happen if more than max_LP_line_length printable characters
   --    are sent to the printer before a LS or PC character, which empties the print matrix.
   -- ee9 simply ignores the excess.

   procedure put_symbols (the_LP        : in out LP.device;
                          Q_operand     : in KDF9.Q_register;
                          writing_to_EM : in Boolean) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      count         : constant KDF9.word := the_LP.unit_count;
      size   : KDF9.word := 0;
      symbol : KDF9_char_sets.symbol;
   begin
      check_addresses_and_lockouts(start_address, end_address);
   word_loop:
      for w in start_address .. end_address loop
         for c in KDF9_char_sets.symbol_index'Range loop
            symbol := fetch_symbol(w, c);
            size := size + 1;
      -- Is this what should happen transfers on EM, leaving the print matrix ready for more data ??
      exit word_loop when writing_to_EM and symbol = KDF9_char_sets.End_Message;
            print(symbol, the_LP);
         end loop;
      end loop word_loop;
      do_output_housekeeping(the_LP, old_count => count, fetched => size);
   end put_symbols;

   -- LPQq
   overriding
   procedure POA (the_LP      : in out LP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_LP, Q_operand, set_offline);
      put_symbols(the_LP, Q_operand, writing_to_EM => False);
      lock_out_relative_addresses(Q_operand);
   end POA;

   -- LPEQq
   overriding
   procedure POB (the_LP      : in out LP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_LP, Q_operand, set_offline);
      put_symbols(the_LP, Q_operand, writing_to_EM => True);
      lock_out_relative_addresses(Q_operand);
   end POB;

   procedure put_words (the_LP        : in out LP.device;
                        Q_operand     : in KDF9.Q_register;
                        writing_to_EM : in Boolean) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      count         : constant KDF9.word := the_LP.unit_count;
      size   : KDF9.word := 0;
      symbol : KDF9_char_sets.symbol;
   begin
      check_addresses_and_lockouts(start_address, end_address);
      for w in start_address .. end_address loop
         symbol := KDF9_char_sets.symbol(fetch_word(w) and 8#77#);
         size := size + 1;
      -- Is this what should happen transfers on EM, leaving the print matrix ready for more data ??
      exit when writing_to_EM and symbol = KDF9_char_sets.End_Message;
         print(symbol, the_LP);
      end loop;
      do_output_housekeeping(the_LP, old_count => count, fetched => size);
   end put_words;

   -- Character write ?? Usercode Digest and Manual conflict!
   overriding
   procedure POC (the_LP      : in out LP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_LP, Q_operand, set_offline);
      put_words(the_LP, Q_operand, writing_to_EM => False);
      lock_out_relative_addresses(Q_operand);
   end POC;

   -- Character write to End_Message ?? Usercode Digest and Manual conflict!
   overriding
   procedure POD (the_LP      : in out LP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_LP, Q_operand, set_offline);
      put_words(the_LP, Q_operand, writing_to_EM => True);
      lock_out_relative_addresses(Q_operand);
   end POD;

   overriding
   procedure Finalize (the_LP : in out LP.device) is
   begin
      close(
            the_LP,
            "printed",
            the_LP.unit_count,
            "line" & plurality(the_LP.unit_count)
           );
   end Finalize;

   LP_quantum : constant := 1E6 / (900 / 60);  -- 900 lines per minute.

   type LP_access is access LP.device;

   LP0 : LP_access with Warnings => Off;
   LP1 : LP_access with Warnings => Off;

   unit : IOC.unit_number := 0;

   procedure enable (b : in KDF9.buffer_number) is
   begin
      case unit is
         when 0 =>
            LP0 := new LP.device (number  => b,
                                  kind    => LP_kind,
                                  unit    => 0,
                                  quantum => LP_quantum);
            LP0_number := b;
         when 1 =>
            LP1 := new LP.device (number  => b,
                                  kind    => LP_kind,
                                  unit    => 1,
                                  quantum => LP_quantum);
            LP1_number := b;
         when others =>
            trap_operator_error("more than 2 LP units specified");
      end case;
      unit := unit + 1;
   end enable;

end IOC.slow.unit.LP;
