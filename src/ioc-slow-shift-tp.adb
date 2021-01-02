-- ioc-slow-shift-tp.ads
--
-- Emulation of a tape punch buffer.
--
-- This file is part of ee9 (V5.2b), the GNU Ada emulator of the English Electric KDF9.
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

with IOC.equipment;
with tracing;

use  IOC.equipment;
use  tracing;

package body IOC.slow.shift.TP is

   use KDF9_char_sets;

   overriding
   procedure Initialize (the_TP : in out TP.device) is
   begin
      open(the_TP, write_mode);
   end Initialize;

   -- the_T_bit_is_set := (the buffer has been switched from a tape punch to a graph plotter)
   overriding
   procedure PMB (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      validate_device(the_TP, Q_operand);
      validate_parity(the_TP);
      deal_with_a_busy_device(the_TP, 13, set_offline);
      the_T_bit_is_set := False;  -- We never get here if GP0 is enabled.
      take_note_of_test(the_TP.device_name, Q_operand, the_T_bit_is_set);
   end PMB;

--
--
   --
   -- See Manual, §17.4 for paper tape 8-bit frame format.
   --
--
--

   procedure write_KDF9_tape_code (the_TP        : in out TP.device;
                                   Q_operand     : in KDF9.Q_register;
                                   writing_to_EM : in Boolean := False) is

      function framed (symbol : KDF9_char_sets.symbol)
      return Natural is

         SP : constant := 8#000#;

         function channel_8
         return KDF9.syllable
         is (if symbol = SP then 2#10_000_000# else 0);

         function parity
         return KDF9.syllable is
            frame  : KDF9.syllable := KDF9.syllable(KDF9_char_sets.symbol'Pos(symbol)) or channel_8;
            parity : KDF9.syllable := 0;
         begin -- parity
            while frame /= 0 loop
               parity := parity xor (frame and 1);
               frame  := frame / 2;
            end loop;
            return (if parity = 0 then 0 else 2#00_010_000#);
         end parity;

         low_4_bits : constant KDF9.syllable := KDF9.syllable(symbol)   and 2#00_001_111#;
         bits_5and6 : constant KDF9.syllable := KDF9.syllable(symbol)*2 and 2#01_100_000#;

      begin -- framed
         return KDF9.syllable'Pos(channel_8 or bits_5and6 or parity or low_4_bits);
      end framed;

      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size   : KDF9.word := 0;
      symbol : KDF9_char_sets.symbol;
      char   : Character;

   begin -- write_KDF9_tape_code
      check_addresses_and_lockouts(start_address, end_address);
   word_loop:
      for w in start_address .. end_address loop
         for c in KDF9_char_sets.symbol_index'Range loop
            symbol := fetch_symbol(w, c);
            size := size + 1;
            char := Character'Val(framed(symbol));
            put_byte(char, the_TP.stream);
         exit word_loop when writing_to_EM and symbol = KDF9_char_sets.End_Message;
         end loop;
      end loop word_loop;
      do_output_housekeeping(the_TP, written => size, fetched => size);
   exception
      when end_of_stream =>
         do_output_housekeeping(the_TP, written => size, fetched => size);
   end write_KDF9_tape_code;

   -- PWQq
   overriding
   procedure POA (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_TP, Q_operand, set_offline, output_operation);
      if the_TP.is_transcribing then
         write(the_TP, Q_operand);
      else
         write_KDF9_tape_code(the_TP, Q_operand);
      end if;
      lock_out_relative_addresses(Q_operand);
   end POA;

   -- PWEQq
   overriding
   procedure POB (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_TP, Q_operand, set_offline, output_operation);
      if the_TP.is_transcribing then
         write_to_EM(the_TP, Q_operand);
      else
         write_KDF9_tape_code(the_TP, Q_operand, writing_to_EM => True);
      end if;
      lock_out_relative_addresses(Q_operand);
   end POB;

   -- PWCQq
   overriding
   procedure POC (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_TP, Q_operand, set_offline, output_operation);
      words_write(the_TP, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end POC;

   -- PWCEQq
   overriding
   procedure POD (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_TP, Q_operand, set_offline, output_operation);
      words_write_to_EM(the_TP, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end POD;

   -- PGAPQq
   overriding
   procedure POE (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      require_nonnegative_count(Q_operand.M);
      output_a_gap(
                   the_TP,
                   Q_operand,
                   set_offline,
                   word_mode => False,
                   text_mode => the_TP.is_transcribing
                  );
   end POE;

   -- "word gap"
   overriding
   procedure POF (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      require_nonnegative_count(Q_operand.M);
      output_a_gap(
                   the_TP,
                   Q_operand,
                   set_offline,
                   word_mode => True,
                   text_mode => the_TP.is_transcribing
                  );
   end POF;

   overriding
   procedure Finalize (the_TP : in out TP.device) is
   begin
      close(
            the_TP,
            "punched",
            the_TP.byte_count,
            "character" & plurality(the_TP.byte_count)
          & " in "
          & (if the_TP.is_transcribing then "Latin-1" else "KDF9")
          & " code"
           );
   end Finalize;

   TP_quantum : constant := 1E6 / 110;  -- 110 characters per second.

   type TP_access is access TP.device;

   TP0  : TP_access with Warnings => Off;
   TP1  : TP_access with Warnings => Off;

   unit : IOC.unit_number := 0;

   procedure enable (b : in KDF9.buffer_number) is
   begin
      case unit is
         when 0 =>
            TP0 := new TP.device (number  => b,
                                  kind    => TP_kind,
                                  unit    => 0,
                                  quantum => TP_quantum);
            TP0_number := b;
         when 1 =>
            TP1 := new TP.device (number  => b,
                                  kind    => TP_kind,
                                  unit    => 1,
                                  quantum => TP_quantum);
            TP1_number := b;
            GP0_number := b;
         when others =>
            trap_operator_error("TP:", "more than two units specified");
      end case;
      unit := unit + 1;
   end enable;

   procedure disable_TP1 (b : out KDF9.buffer_number) is
   begin
      if TP1 /= null then
         Finalize(TP1.all);
      end if;
      b := GP0_number;
   end disable_TP1;

   -- Set the character code to be used by the designated TP.
   procedure set_unit_code (unit : in Natural; is_transcribing : in Boolean) is
   begin
      if set_unit_code.unit < Natural(IOC.slow.shift.TP.unit) then
         if unit = 0 then
            TP0.is_transcribing := set_unit_code.is_transcribing;
         else
            TP1.is_transcribing := set_unit_code.is_transcribing;
         end if;
      end if;
   end set_unit_code;

end IOC.slow.shift.TP;
