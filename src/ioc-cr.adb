-- ioc-cr.adb
--
-- Emulation of a card reader buffer.
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

package body IOC.CR is

   pragma Unsuppress(All_Checks);

   use Latin_1;

   overriding
   procedure Initialize (the_CR : in out CR.device) is
   begin
      open(the_CR, read_mode, attaching => False);
   end Initialize;

   overriding
   procedure Finalize (the_CR : in out CR.device) is
   begin
      close(the_CR, "read", the_CR.unit_count, "card(s)");
   end Finalize;

   blank_card : constant String(max_card_columns) := (others => Space);

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
         get_char_from_stream (char, the_CR, size);
      exit when char = LF;
         the_CR.card_image(i) := char;
      exit when reading_to_EM and char = KDF9.E_M;
      end loop;
      -- Discard excess characters in the current data line.
      while char /= LF loop
         get_char(char, the_CR.stream);  -- N.B. do not update size for discards.
      end loop;
      the_CR.unit_count := the_CR.unit_count + 1;
   exception
      when end_of_stream =>
         the_CR.is_abnormal := True;
   end get_card_image;

   procedure do_input_housekeeping (the_CR : in out CR.device;
                                    fetched    : in KDF9.word) is
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
      byte : KDF9.symbol;
   begin
      validate_range_access(start_address, end_address);
      get_card_image(the_CR, size, max_columns => max_words*8);
      if the_CR.is_abnormal then return; end if;
   word_loop:
      for w in start_address .. KDF9.address'Min(end_address, start_address+max_words-1) loop
         for c in KDF9.symbol_number'Range loop
            next := next + 1;
            char := the_CR.card_image(next);
            byte := CR_in(char);
            -- CR goes abnormal if the data character is not in the supported character set.
            the_CR.is_abnormal := the_CR.is_abnormal or
                                      (byte = KDF9.Word_Filler and char /= KDF9.W_F);
            store_symbol(byte, w, c);
            if reading_to_EM and byte = KDF9.End_Message then
               for d in 1 .. 7-c loop
                  store_symbol(KDF9.Blank_Space, w, c+d);
               end loop;
               exit word_loop;
            end if;
         end loop;
      end loop word_loop;
      do_input_housekeeping(the_CR, size);
   exception
      when end_of_stream =>
         flush(the_CR.stream);
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
      validate_range_access(start_address, end_address);
      get_card_image(the_CR, size, max_columns => max_words);
      if the_CR.is_abnormal then return; end if;
      for w in start_address .. KDF9.address'Min(end_address, start_address+max_words-1) loop
         next := next + 1;
         char := the_CR.card_image(next);
         word := KDF9.word(CR_in(char));
         -- CR goes abnormal if the data character is not in the supported character set.
         the_CR.is_abnormal := the_CR.is_abnormal or
                                   (word = KDF9.word(KDF9.Word_Filler) and char /= KDF9.W_F);
         store_word(word, w);
      exit when reading_to_EM and char = KDF9.E_M;
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
      initialize_byte_mode_transfer(the_CR, Q_operand, set_offline);
      read_card(the_CR, Q_operand, max_words => 20);
      set_lockouts(Q_operand);
   end PIA;

   overriding
   procedure PIB (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CR, Q_operand, set_offline);
      read_card(the_CR, Q_operand, max_words => 20, reading_to_EM => True);
      set_lockouts(Q_operand);
   end PIB;

   overriding
   procedure PIC (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CR, Q_operand, set_offline);
      words_read_card(the_CR, Q_operand, max_words => 160);
      set_lockouts(Q_operand);
   end PIC;

   overriding
   procedure PID (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CR, Q_operand, set_offline);
      words_read_card(the_CR, Q_operand, max_words => 160, reading_to_EM => True);
      set_lockouts(Q_operand);
   end PID;

   overriding
   procedure PIE (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CR, Q_operand, set_offline);
      read_card(the_CR, Q_operand, max_words => 10);
      set_lockouts(Q_operand);
   end PIE;

   overriding
   procedure PIF (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CR, Q_operand, set_offline);
      read_card(the_CR, Q_operand, max_words => 10, reading_to_EM => True);
      set_lockouts(Q_operand);
   end PIF;

   overriding
   procedure PIG (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CR, Q_operand, set_offline);
      words_read_card(the_CR, Q_operand, max_words => 80);
      set_lockouts(Q_operand);
   end PIG;

   overriding
   procedure PIH (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_CR, Q_operand, set_offline);
      words_read_card(the_CR, Q_operand, max_words => 80, reading_to_EM => True);
      set_lockouts(Q_operand);
   end PIH;

   -- the_T_bit := (RECHECK switch is OFF). {It always is nowadays!}
   overriding
   procedure PMB (the_CR      : in out CR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_CR, Q_operand);
      validate_parity(the_CR);
      the_T_bit := 1;
      take_note_of(Q_operand, the_CR.device_name, the_T_bit);
   end PMB;

   CR_quantum : constant := 1E6 / (600 / 60); -- 600 cards per minute.

   CR0 : aliased CR.device (number  => CR0_number,
                            kind    => CR_kind,
                            unit    => 0,
                            quantum => CR_quantum,
                            is_slow => True);
   pragma Unreferenced(CR0);

end IOC.CR;
