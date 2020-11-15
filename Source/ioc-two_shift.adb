-- ioc-byte_mode-two_shift.adb
--
-- Emulation of the common functionality of a 2-case (Normal/Shift) buffer.
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

with KDF9.store;

use  KDF9.store;

package body IOC.two_shift is

   overriding
   procedure Finalize (the_device : in out two_shift.device) is
   begin
      close(the_device, "transferred", the_device.byte_count, "character(s)");
   end Finalize;

   procedure do_input_housekeeping (the_device : in out two_shift.device;
                                    read_in,
                                    stored     : in KDF9.word) is
   begin
      if read_in > 0 then
         add_in_the_IO_CPU_time(the_device, stored);
      end if;
      correct_transfer_time(the_device, read_in);
      the_device.byte_count := the_device.byte_count + read_in;
   end do_input_housekeeping;

   procedure get_symbols (the_device    : in out two_shift.device;
                          Q_operand     : in KDF9.Q_register;
                          reading_to_EM : in Boolean) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      fill   : KDF9.word := 0;
      size   : KDF9.word := 0;
      symbol : KDF9.symbol;
      char   : Character;
   begin
      validate_range_access(start_address, end_address);
   word_loop:
      for w in start_address .. end_address loop
         store_word(0, w);
         for c in KDF9.symbol_number'Range loop
            get_char_from_stream(char, the_device, size);
            size := size + 1;
            if char = KDF9.W_F then
               -- Filler was suppressed on normal input from the slow devices.
               fill := fill + 1;
            elsif case_of(char) /= both and case_of(char) /= the_device.current_case then
               store_symbol(CN_TR(next_case(the_device.current_case)), w, c);
               the_device.current_case := the_device.current_case xor 1;
               back_off(the_device.stream);
            else
               symbol := CN_TR(char) or CS_TR(char);
               store_symbol(symbol, w, c);
               if reading_to_EM and symbol = KDF9.End_Message then
                  for d in 1 .. 7-c loop
                     store_symbol(KDF9.Blank_Space, w, c+d);
                  end loop;
                  exit word_loop;
               end if;
            end if;
         end loop;
      end loop word_loop;
      do_input_housekeeping(the_device, read_in => size, stored => size-fill);
   exception
      when end_of_stream =>
         flush(the_device.stream);
         do_input_housekeeping(the_device, read_in => size, stored => size-fill);
   end get_symbols;

   procedure read (the_device : in out two_shift.device;
                   Q_operand  : in KDF9.Q_register) is
   begin
      get_symbols(the_device, Q_operand, reading_to_EM => False);
   end read;

   procedure read_to_EM (the_device : in out two_shift.device;
                         Q_operand  : in KDF9.Q_register) is
   begin
      get_symbols(the_device, Q_operand, reading_to_EM => True);
   end read_to_EM;

   procedure get_words (the_device    : in out two_shift.device;
                        Q_operand     : in KDF9.Q_register;
                        reading_to_EM : in Boolean) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size : KDF9.word := 0;
      word : KDF9.word;
      char : Character;
   begin
      validate_range_access(start_address, end_address);
      for w in start_address .. end_address loop
         get_char_from_stream(char, the_device, size);
         word := KDF9.word(Character'Pos(char));
         size := size + 1;
         store_word(word, w);
      exit when reading_to_EM and (word and 8#77#) = KDF9.word(KDF9.End_Message);
      end loop;
      do_input_housekeeping(the_device, read_in => size, stored => size);
   exception
      when end_of_stream =>
         flush(the_device.stream);
         do_input_housekeeping(the_device, read_in => size, stored => size);
   end get_words;

   procedure words_read (the_device : in out two_shift.device;
                         Q_operand  : in KDF9.Q_register) is
   begin
      get_words(the_device, Q_operand, reading_to_EM => False);
   end words_read;

   procedure words_read_to_EM (the_device : in out two_shift.device;
                               Q_operand  : in KDF9.Q_register) is
   begin
      get_words(the_device, Q_operand, reading_to_EM => True);
   end words_read_to_EM;

   procedure do_output_housekeeping (the_device : in out two_shift.device;
                                     written,
                                     fetched    : in KDF9.word) is
   begin
      flush(the_device.stream);
      add_in_the_IO_CPU_time(the_device, fetched);
      correct_transfer_time(the_device, written);
      the_device.byte_count := the_device.byte_count + fetched;
   end do_output_housekeeping;

   procedure put_symbols (the_device    : in out two_shift.device;
                          Q_operand     : in KDF9.Q_register;
                          writing_to_EM : in Boolean) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      fill   : KDF9.word := 0;
      size   : KDF9.word := 0;
      symbol : KDF9.symbol;
      char   : Character;
   begin
      validate_range_access(start_address, end_address);
   word_loop:
      for w in start_address .. end_address loop
         for c in KDF9.symbol_number'Range loop
            symbol := fetch_symbol(w, c);
            size := size + 1;
            if symbol = KDF9.Word_Filler then
               -- Filler was suppressed on normal output to the slow devices.
               fill := fill + 1;
            elsif symbol = KDF9.Case_Shift then
               the_device.current_case := KDF9.Case_Shift;
            elsif  symbol = KDF9.Case_Normal then
               the_device.current_case := KDF9.Case_Normal;
            else
               if the_device.current_case = KDF9.Case_Normal then
                  char := TP_CN(symbol);
               else
                  char := TP_CS(symbol);
               end if;
               put_char(char, the_device.stream);
               exit word_loop when writing_to_EM and symbol = KDF9.End_Message;
            end if;
         end loop;
      end loop word_loop;
      do_output_housekeeping(the_device, written => size-fill, fetched => size);
   exception
      when end_of_stream =>
         do_output_housekeeping(the_device, written => size-fill, fetched => size);
   end put_symbols;

   procedure write (the_device : in out two_shift.device;
                    Q_operand  : in KDF9.Q_register) is
   begin
      put_symbols(the_device, Q_operand, writing_to_EM => False);
   end write;

   procedure write_to_EM (the_device : in out two_shift.device;
                          Q_operand  : in KDF9.Q_register) is
   begin
      put_symbols(the_device, Q_operand, writing_to_EM => True);
   end write_to_EM;

   procedure put_words (the_device    : in out two_shift.device;
                        Q_operand     : in KDF9.Q_register;
                        writing_to_EM : in Boolean) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size : KDF9.word := 0;
      word : KDF9.word;
   begin
      validate_range_access(start_address, end_address);
      for w in start_address .. end_address loop
         word := fetch_word(w) and 8#377#;
         put_byte(Character'Val(word), the_device.stream);
         size := size + 1;
      exit when writing_to_EM and (word and 8#77#) = KDF9.word(KDF9.End_Message);
      end loop;
      do_output_housekeeping(the_device, written => size, fetched => size);
   exception
      when end_of_stream =>
         do_output_housekeeping(the_device, written => size, fetched => size);
   end put_words;

   procedure words_write (the_device : in out two_shift.device;
                          Q_operand  : in KDF9.Q_register) is
   begin
      put_words(the_device, Q_operand, writing_to_EM => False);
   end words_write;

   procedure words_write_to_EM (the_device : in out two_shift.device;
                                Q_operand  : in KDF9.Q_register) is
   begin
      put_words(the_device, Q_operand, writing_to_EM => True);
   end words_write_to_EM;

   procedure set_case (the_device  : in out two_shift.device;
                       the_setting : in KDF9.letter_case := Case_Normal) is
   begin
      the_device.current_case := the_setting;
   end set_case;

end IOC.two_shift;
