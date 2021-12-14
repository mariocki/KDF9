-- Emulation of the common functionality of a 2-case (Normal/Shift) buffer.
--
-- This file is part of ee9 (8.1x), the GNU Ada emulator of the English Electric KDF9.
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

with KDF9_char_sets.framed;

package body IOC.slow.shift is

   use KDF9_char_sets;

   overriding
   procedure Initialize (the_device : in out shift.device) is
   begin
      -- Open the associated file.
      open(the_device, read_mode);
   end Initialize;

   procedure do_input_housekeeping (the_device : in out shift.device;
                                    read_in,
                                    stored     : in KDF9.word) is
   begin
      add_in_the_IO_CPU_time(the_device, stored);
      correct_transfer_time(the_device, read_in);
      the_device.byte_count := the_device.byte_count + read_in;
   end do_input_housekeeping;

   procedure get_symbols (the_device    : in out shift.device;
                          Q_operand     : in KDF9.Q_register;
                          reading_to_EM : in Boolean) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      fill   : KDF9.word := 0;
      size   : KDF9.word := 0;
      symbol : KDF9_char_sets.symbol;
      char   : Character;
   begin
      check_addresses_and_lockouts(start_address, end_address);
   word_loop:
      for w in start_address .. end_address loop
         store_word(0, w);
         for c in KDF9_char_sets.symbol_index'Range loop
            get_char_from_stream(char, the_device);
            size := size + 1;
            if char = KDF9_char_sets.W_F then
               -- Filler was suppressed on normal input from the slow devices.
               fill := fill + 1;
            elsif case_of(char) /= both                   and then
                     case_of(char) /= the_device.current_case then
               store_symbol(CN_TR(next_case(the_device.current_case)), w, c);
               the_device.current_case := the_device.current_case xor 1;
               back_off(the_device.stream);
            else
               symbol := CN_TR(char) or CS_TR(char);
               store_symbol(symbol, w, c);
               if reading_to_EM and symbol = KDF9_char_sets.End_Message then
                  for d in 1 .. 7-c loop
                     store_symbol(KDF9_char_sets.Blank_Space, w, c+d);
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

   procedure read (the_device : in out shift.device;
                   Q_operand  : in KDF9.Q_register) is
   begin
      get_symbols(the_device, Q_operand, reading_to_EM => False);
   end read;

   procedure read_to_EM (the_device : in out shift.device;
                         Q_operand  : in KDF9.Q_register) is
   begin
      get_symbols(the_device, Q_operand, reading_to_EM => True);
   end read_to_EM;

   procedure get_frame_from_stream (frame      : out Character;
                                    the_device : in out shift.device) is
   begin
      loop
         begin
            get_byte(frame, the_device.stream);
            if the_device.is_reading_a_file then
               -- Assume file, in KDF9 paper tape code, contains all needed shift characters.
               return;
            end if;
            -- Reading from the terminal, the frame is in Latin-1 and must be converted.
            -- This may entail interpolating a shift character.
            if case_of(frame) not in both | the_device.current_case then
               frame := framed(CN_TR(next_case(the_device.current_case)));
               the_device.current_case := the_device.current_case xor 1;
               back_off(the_device.stream);
            elsif the_device.current_case = KDF9_char_sets.Case_Normal then
               frame := framed(CN_TR(frame));
            else
               frame := framed(CS_TR(frame));
            end if;
            return;
         exception
            when end_of_stream =>
               deal_with_end_of_data(the_device);
         end;
      end loop;
   end get_frame_from_stream;

   procedure get_words (the_device    : in out shift.device;
                        Q_operand     : in KDF9.Q_register;
                        reading_to_EM : in Boolean) is

      function reframed (byte : Character)
      return KDF9.word is
         data : KDF9.word;
      begin -- reframed
         -- Permute the paper tape frame bits to make a data byte, see the Manual, § 17.7, pp. 137-138.
         -- This is the converse of put_words.reframed.
         data := KDF9.word(Character'Pos(byte));
         return  (data and 2#0000_1111#)    -- D44-D47 -> D44-D47
              or (data and 2#0110_0000#)/2  -- D41-D42 -> D42-D43
              or (data and 2#0001_0000#)*4  -- D43     -> D41
              or (data and 2#1000_0000#);   -- D40     -> D40
      end reframed;

      start_address : constant KDF9.address := Q_operand.I;
      end_address : constant KDF9.address := Q_operand.M;
      size        : KDF9.word := 0;
      word        : KDF9.word;
      done        : Boolean;
      char        : Character;

   begin -- get_words
      check_addresses_and_lockouts(start_address, end_address);
      for w in start_address .. end_address loop
         if the_device.is_transcribing then
            -- "transcribing" actually means "transparent" for character mode input.
            get_char_from_stream(char, the_device);
            word := KDF9.word(Character'Pos(char));
            done := char = E_M;
         else
            -- KDF9 mode.
            get_frame_from_stream(char, the_device);
            word := reframed(char);
            done := KDF9_char_sets.symbol(word and 8#77#) = End_Message;
         end if;
         store_word(word, w);
         size := size + 1;
      exit when reading_to_EM and done;
      end loop;
      do_input_housekeeping(the_device, read_in => size, stored => size);
   exception
      when end_of_stream =>
         flush(the_device.stream);
         do_input_housekeeping(the_device, read_in => size, stored => size);
   end get_words;

   procedure words_read (the_device : in out shift.device;
                         Q_operand  : in KDF9.Q_register) is
   begin
      get_words(the_device, Q_operand, reading_to_EM => False);
   end words_read;

   procedure words_read_to_EM (the_device : in out shift.device;
                               Q_operand  : in KDF9.Q_register) is
   begin
      get_words(the_device, Q_operand, reading_to_EM => True);
   end words_read_to_EM;

   procedure put_symbols (the_device    : in out shift.device;
                          Q_operand     : in KDF9.Q_register;
                          writing_to_EM : in Boolean) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      fill   : KDF9.word := 0;
      size   : KDF9.word := 0;
      symbol : KDF9_char_sets.symbol;
      char   : Character;
   begin
      check_addresses_and_lockouts(start_address, end_address);
   word_loop:
      for w in start_address .. end_address loop
         for c in KDF9_char_sets.symbol_index'Range loop
            symbol := fetch_symbol(w, c);
            size := size + 1;
            if symbol = KDF9_char_sets.Word_Filler then
               -- Filler was suppressed on normal output to the slow devices.
               fill := fill + 1;
            elsif symbol = KDF9_char_sets.Case_Shift then
               the_device.current_case := KDF9_char_sets.Case_Shift;
            elsif  symbol = KDF9_char_sets.Case_Normal then
               the_device.current_case := KDF9_char_sets.Case_Normal;
            else
               if the_device.current_case = KDF9_char_sets.Case_Normal then
                  char := TP_CN(symbol);
               else
                  char := TP_CS(symbol);
               end if;
               put_char(char, the_device.stream);
               exit word_loop when writing_to_EM and symbol = KDF9_char_sets.End_Message;
            end if;
         end loop;
      end loop word_loop;
      do_output_housekeeping(the_device, written => size-fill, fetched => size);
   exception
      when end_of_stream =>
         do_output_housekeeping(the_device, written => size-fill, fetched => size);
   end put_symbols;

   procedure write (the_device : in out shift.device;
                    Q_operand  : in KDF9.Q_register) is
   begin
      put_symbols(the_device, Q_operand, writing_to_EM => False);
   end write;

   procedure write_to_EM (the_device : in out shift.device;
                          Q_operand  : in KDF9.Q_register) is
   begin
      put_symbols(the_device, Q_operand, writing_to_EM => True);
   end write_to_EM;

   procedure put_words (the_device    : in out shift.device;
                        Q_operand     : in KDF9.Q_register;
                        writing_to_EM : in Boolean) is

      function reframed (byte : Character)
      return Character is
         data : KDF9.word:= KDF9.word(Character'Pos(byte));
      begin -- reframed
         -- Permute the data bits to make a paper tape frame, see the Manual, § 17.7, pp. 137-138.
         -- This is the converse of get_words.reframed.
         data := (data and 2#0000_1111#)    -- D44-D47 -> D44-D47
              or (data and 2#0011_0000#)*2  -- D42-D43 -> D41-D42
              or (data and 2#0100_0000#)/4  -- D41     -> D43
              or (data and 2#1000_0000#);   -- D40     -> D40
         return Character'Val(Natural(data));
      end reframed;

      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size : KDF9.word := 0;
      done : Boolean;
      char : Character;

   begin -- put_words
      check_addresses_and_lockouts(start_address, end_address);
      for w in start_address .. end_address loop
         char := Character'Val(fetch_word(w) and 8#377#);
         if the_device.is_transcribing then
            -- "transcribing" actually means "transparent Latin-1" for character mode transfers.
            done := char = E_M;
         else
            -- KDF9 mode.
            char := reframed(char);
            done := KDF9_char_sets.symbol(Character'Pos(char) mod 64) = End_Message;
         end if;
         put_byte(char, the_device.stream);
         size := size + 1;
      exit when writing_to_EM and done;
      end loop;
      do_output_housekeeping(the_device, written => size, fetched => size);
   end put_words;

   procedure words_write (the_device : in out shift.device;
                          Q_operand  : in KDF9.Q_register) is
   begin
      put_words(the_device, Q_operand, writing_to_EM => False);
   end words_write;

   procedure words_write_to_EM (the_device : in out shift.device;
                                Q_operand  : in KDF9.Q_register) is
   begin
      put_words(the_device, Q_operand, writing_to_EM => True);
   end words_write_to_EM;

   procedure output_a_gap (the_device   : in out shift.device;
                           Q_operand    : in KDF9.Q_register;
                           set_offline  : in Boolean;
                           word_mode    : in Boolean := False;
                           text_mode    : in Boolean := False) is
      length : constant KDF9.word :=  KDF9.word(Q_operand.M) * (if word_mode then 8 else 1);
      char   : constant Character := Character'Val(0);
      size   : KDF9.word := 0;
   begin
      require_positive_count(Q_operand.M);
      for i in 1 .. length loop
         size := size + 1;
         if text_mode then
            do_not_put_byte(char, the_device.stream);
         else
            put_byte(char, the_device.stream);
         end if;
      end loop;
      start_data_transfer(
                          the_device,
                          (Q_operand.C, 0, Q_operand.M),
                          set_offline,
                          busy_time => IO_elapsed_time(the_device, length)
                         );
      do_output_housekeeping(the_device, written => length, fetched => 0);
   exception
      when end_of_stream =>
         do_output_housekeeping(the_device, written => size, fetched => 0);
   end output_a_gap;

   procedure do_output_housekeeping (the_device : in out shift.device;
                                     written,
                                     fetched    : in KDF9.word) is
   begin
      flush(the_device.stream);
      add_in_the_IO_CPU_time(the_device, fetched);
      correct_transfer_time(the_device, written);
      the_device.byte_count := the_device.byte_count + fetched;
   end do_output_housekeeping;

   procedure set_case (the_device  : in out shift.device;
                       the_setting : in KDF9_char_sets.letter_case := KDF9_char_sets.Case_Normal) is
   begin
      the_device.current_case := the_setting;
   end set_case;

   function uses_Latin_1 (the_device : in shift.device)
   return Boolean
   is (the_device.is_transcribing);

   overriding
   procedure Finalize (the_device : in out shift.device) is
   begin
      close(
            the_device,
            "transferred",
            the_device.byte_count,
            "character" & plurality(the_device.byte_count)
           );
   end Finalize;

end IOC.slow.shift;
