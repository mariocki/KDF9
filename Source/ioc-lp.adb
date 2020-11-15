-- ioc-lp.adb
--
-- Emulation of a lineprinter buffer.
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

package body IOC.LP is

   pragma Unsuppress(All_Checks);

   overriding
   procedure Initialize (the_LP : in out LP.device) is
   begin
      open(the_LP, write_mode, attaching => False);
   end Initialize;

   overriding
   procedure Finalize (the_LP : in out LP.device) is
   begin
      close(the_LP, "printed", the_LP.unit_count, "line(s)");
   end Finalize;

   max_LP_line_length : constant := 160;

   procedure do_output_housekeeping (the_LP   : in out LP.device;
                                    old_count,
                                    fetched  : in KDF9.word) is
   begin
      flush(the_LP.stream);
      correct_transfer_time(the_LP, IO_elapsed_time(the_LP, the_LP.unit_count-old_count));
      add_in_the_IO_CPU_time(the_LP, fetched);
   end do_output_housekeeping;

   procedure put_symbols (the_LP        : in out LP.device;
                          Q_operand     : in KDF9.Q_register;
                          writing_to_EM : in Boolean) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      count         : constant KDF9.word := the_LP.unit_count;
      size   : KDF9.word := 0;
      next   : Natural := 0;
      symbol : KDF9.symbol;
      char   : Character;
   begin
      validate_range_access(start_address, end_address);
   word_loop:
      -- What should happen if transfer length > max_LP_line_length characters ??
      for w in start_address .. end_address loop
         for c in KDF9.symbol_number'Range loop
            exit word_loop when next > max_LP_line_length;
            symbol := fetch_symbol(w, c);
            size := size + 1;
            exit word_loop when writing_to_EM and symbol = KDF9.End_Message;
            char := to_LP(symbol);
            if char /= KDF9.W_F then
               next := next + 1;
               put_char(char, the_LP.stream);
               if symbol = KDF9.Line_Shift or symbol = KDF9.Page_Change then
                  the_LP.unit_count := the_LP.unit_count + 1;
                  flush(the_LP.stream);
                  next := 0;
               end if;
            end if;
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
      initialize_byte_mode_transfer(the_LP, Q_operand, set_offline);
      put_symbols(the_LP, Q_operand, writing_to_EM => False);
      set_lockouts(Q_operand);
   end POA;

   -- LPEQq
   overriding
   procedure POB (the_LP      : in out LP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_LP, Q_operand, set_offline);
      put_symbols(the_LP, Q_operand, writing_to_EM => True);
      set_lockouts(Q_operand);
   end POB;

   procedure put_words (the_LP        : in out LP.device;
                        Q_operand     : in KDF9.Q_register;
                        writing_to_EM : in Boolean) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      count         : constant KDF9.word := the_LP.unit_count;
      size   : KDF9.word := 0;
      next   : Natural := 0;
      symbol : KDF9.symbol;
      char   : Character;
   begin
      validate_range_access(start_address, end_address);
      -- What should happen if transfer length > max_LP_line_length characters ??
      for w in start_address .. end_address loop
         exit when next > max_LP_line_length;
         symbol := KDF9.symbol(fetch_word(w) and 8#77#);
         size := size + 1;
         exit when writing_to_EM and symbol = KDF9.End_Message;
         char := to_LP(symbol);
         if char /= KDF9.W_F then
            next := next + 1;
            put_char(char, the_LP.stream);
            if symbol = KDF9.Line_Shift or symbol = KDF9.Page_Change then
               the_LP.unit_count := the_LP.unit_count + 1;
               flush(the_LP.stream);
               next := 0;
            end if;
         end if;
      end loop;
      do_output_housekeeping(the_LP, old_count => count, fetched => size);
   end put_words;

   -- Character write ?? Usercode Digest and Manual conflict!
   overriding
   procedure POC (the_LP      : in out LP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_LP, Q_operand, set_offline);
      put_words(the_LP, Q_operand, writing_to_EM => False);
      set_lockouts(Q_operand);
   end POC;

   -- Character write to End_Message ?? Usercode Digest and Manual conflict!
   overriding
   procedure POD (the_LP      : in out LP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_LP, Q_operand, set_offline);
      put_words(the_LP, Q_operand, writing_to_EM => True);
      set_lockouts(Q_operand);
   end POD;

   LP_quantum : constant := 1E6 / (900 / 60);  -- 900 lines per minute.

   LP0 : aliased LP.device (number  => LP0_number,
                            kind    => LP_kind,
                            unit    => 0,
                            quantum => LP_quantum,
                            is_slow => True);
   pragma Unreferenced(LP0);

end IOC.LP;
