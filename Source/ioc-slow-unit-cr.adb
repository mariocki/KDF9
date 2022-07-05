-- Emulation of a card reader buffer.
--
-- This file is part of ee9 (8.2z), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2022, W. Findlay; all rights reserved.
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

package body IOC.slow.unit.CR is

   use KDF9_char_sets;

   overriding
   procedure Initialize (the_CR : in out CR.device) is
   begin
      open(the_CR, read_mode);
   end Initialize;

   blank_card : constant String(max_card_columns) := (others => SP);

   procedure get_card_image (the_CR         : in out CR.device;
                              size          : in out KDF9.word;
                              max_columns   : in KDF9.address;
                              reading_to_EM : in Boolean := False) is
      max  : constant Positive := Positive(max_columns);
      char : Character;
   begin
      -- Clear out the card image field.
      the_CR.card_image(1..max) := blank_card(1..max);
      -- Fill as much of the card image as possible with the next data line, padded out with
      --    blanks, so that it is unnecessary to type all 80 or 160 characters.
      -- For transfers to End Message, a line terminator must follow the E_M.
      for i in 1 .. max loop
         get_char_from_stream (char, the_CR);
         size := size + 1;
      exit when char = LF;
         the_CR.card_image(i) := char;
      exit when reading_to_EM and char = KDF9_char_sets.E_M;
      end loop;
      if char /= KDF9_char_sets.E_M then  -- The whole card was read.
         size := KDF9.word(max);
      end if;
      the_CR.unit_count := the_CR.unit_count + 1;
      -- Discard excess characters in the current data line.
      while char /= LF loop
         get_char_from_stream (char, the_CR);  -- N.B. do not update size for discards.
      end loop;
   exception
      when end_of_stream =>
         flush(the_CR.stream);
         the_CR.is_abnormal := True;
         raise;
   end get_card_image;

   procedure do_input_housekeeping (the_CR  : in out CR.device;
                                    fetched : in KDF9.word) is
   begin
      add_in_the_IO_CPU_time(the_CR, fetched);
      correct_transfer_time(the_CR, actual_length => 1);
   end do_input_housekeeping;

   procedure read_card (the_CR        : in out CR.device;
                        Q_operand     : in KDF9.Q_register;
                        max_words     : in KDF9.address;
                        reading_to_EM : in Boolean := False) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size : KDF9.word := 0;
      next : Natural := 0;
      char : Character;
      byte : KDF9_char_sets.symbol;
   begin
      check_addresses_and_lockouts(start_address, end_address);
      get_card_image(the_CR, size, max_columns => max_words*8);
   word_loop:
      for w in start_address .. KDF9.address'Min(end_address, start_address+max_words-1) loop
         for c in KDF9_char_sets.symbol_index'Range loop
            next := next + 1;
            char := the_CR.card_image(next);
            byte := CR_in(char);
            store_symbol(byte, w, c);
            if reading_to_EM and byte = KDF9_char_sets.End_Message then
               for d in 1 .. 7-c loop
                  store_symbol(KDF9_char_sets.Blank_Space, w, c+d);
               end loop;
               exit word_loop;
            end if;
         end loop;
      end loop word_loop;
      do_input_housekeeping(the_CR, size);
   exception
      when end_of_stream =>
         do_input_housekeeping(the_CR, size);
   end read_card;

   procedure words_read_card (the_CR        : in out CR.device;
                              Q_operand     : in KDF9.Q_register;
                              max_words     : in KDF9.address;
                              reading_to_EM : in Boolean := False) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size : KDF9.word := 0;
      next : Natural := 0;
      char : Character;
      word : KDF9.word;
   begin
      check_addresses_and_lockouts(start_address, end_address);
      get_card_image(the_CR, size, max_columns => max_words);
      if the_CR.is_abnormal then return; end if;
      for w in start_address .. KDF9.address'Min(end_address, start_address+max_words-1) loop
         next := next + 1;
         char := the_CR.card_image(next);
         word := KDF9.word(CR_in(char));
         store_word(word, w);
      exit when reading_to_EM and char = KDF9_char_sets.E_M;
      end loop;
      add_in_the_IO_CPU_time(the_CR, size);
      correct_transfer_time(the_CR, actual_length => 1);
   exception
      when end_of_stream =>
         flush(the_CR.stream);
         add_in_the_IO_CPU_time(the_CR, size);
         correct_transfer_time(the_CR, actual_length => 1);
   end words_read_card;

   overriding
   procedure PIA (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CR, Q_operand, set_offline, input_operation);
      read_card(the_CR, Q_operand, max_words => 20);
      lock_out_relative_addresses(Q_operand);
   end PIA;

   overriding
   procedure PIB (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CR, Q_operand, set_offline, input_operation);
      read_card(the_CR, Q_operand, max_words => 20, reading_to_EM => True);
      lock_out_relative_addresses(Q_operand);
   end PIB;

   overriding
   procedure PIC (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CR, Q_operand, set_offline, input_operation);
      words_read_card(the_CR, Q_operand, max_words => 160);
      lock_out_relative_addresses(Q_operand);
   end PIC;

   overriding
   procedure PID (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CR, Q_operand, set_offline, input_operation);
      words_read_card(the_CR, Q_operand, max_words => 160, reading_to_EM => True);
      lock_out_relative_addresses(Q_operand);
   end PID;

   overriding
   procedure PIE (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CR, Q_operand, set_offline, input_operation);
      read_card(the_CR, Q_operand, max_words => 10);
      lock_out_relative_addresses(Q_operand);
   end PIE;

   overriding
   procedure PIF (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CR, Q_operand, set_offline, input_operation);
      read_card(the_CR, Q_operand, max_words => 10, reading_to_EM => True);
      lock_out_relative_addresses(Q_operand);
   end PIF;

   overriding
   procedure PIG (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CR, Q_operand, set_offline, input_operation);
      words_read_card(the_CR, Q_operand, max_words => 80);
      lock_out_relative_addresses(Q_operand);
   end PIG;

   overriding
   procedure PIH (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_CR, Q_operand, set_offline, input_operation);
      words_read_card(the_CR, Q_operand, max_words => 80, reading_to_EM => True);
      lock_out_relative_addresses(Q_operand);
   end PIH;

   -- the_T_bit_is_set := (RECHECK switch is OFF). {It always is nowadays!}
   overriding
   procedure PMB (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      validate_device(the_CR);
      validate_parity(the_CR);
      deal_with_a_busy_device(the_CR, 13, set_offline);
      the_T_bit_is_set := True;
      take_note_of_test(the_CR.device_name, Q_operand, the_T_bit_is_set);
   end PMB;

   overriding
   procedure Finalize (the_CR : in out CR.device) is
   begin
      close(
            the_CR,
            "read",
            the_CR.unit_count,
            "card" & plurality(the_CR.unit_count)
           );
   end Finalize;

   type CR_access is access CR.device;

   CR0 : CR_access with Warnings => Off;
   CR1 : CR_access with Warnings => Off;

   unit : IOC.unit_number := 0;

   procedure enable (b : in KDF9.buffer_number) is
   begin
      case unit is
         when 0 =>
            CR0 := new CR.device (number => b, unit => 0);
            CR0_number := b;
         when 1 =>
            CR1 := new CR.device (number => b, unit => 1);
            CR1_number := b;
         when others =>
            trap_operator_error("more than two CR units have been configured");
      end case;
      unit := unit + 1;
   end enable;

end IOC.slow.unit.CR;
