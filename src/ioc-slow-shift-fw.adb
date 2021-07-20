-- Emulation of the FlexoWriter buffer: monitor typewriter functionality.
--
-- This file is part of ee9 (7.0a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2021, W. Findlay; all rights reserved.
--
-- The ee9 program is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. This program is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details. You should have
-- received the copy of the GNU General Public License distributed with
-- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Text_IO;
--
with HCI;
with host_IO;

use  Ada.Text_IO;
--
use  HCI;
use  host_IO;

package body IOC.slow.shift.FW is

   function a_LF_was_just_read (the_FW : FW.device)
   return Boolean
   is (the_FW.mode = the_flexowriter_is_reading and then a_LF_was_just_read(the_FW.stream));

   max_text_length : constant Positive := 64;  -- This is the limit imposed by NTSD and TSD.
   min_text_length : constant Positive :=  2;  -- This is arbitrary, but seems reasonable.

   type interaction is
      record
         text           : String(1 .. max_text_length);
         prompt_length,
         total_length   : Positive range 1 .. max_text_length;
      end record;

   max_interactions : constant Positive := 16; -- This is arbitrary, but seems reasonable.

   interactions     : array (1 .. max_interactions) of FW.interaction;
   next_interaction : Positive := 1;
   last_interaction : Natural  := 0;

    -- A '®' denotes LF, and the '©' denotes FF in an interaction text input.
   LF_surrogate     : constant Character := '®';
   FF_surrogate     : constant Character := '©';

   -- These are the ANSI SGR terminal escape codes for styling FW output.
   red_font_code   : constant String := ESC & "[0m" & ESC & "[31m";
   black_font_code : constant String := ESC & "[0m" & ESC & "[39m";
   underline_code  : constant String := ESC & "[4m";
   plain_font_code : constant String := ESC & "[0m";

   procedure set_text_colour_to_red (the_flexowriter_output : in out host_IO.stream) is
   begin
      if the_terminal_is_ANSI_compatible and realistic_FW_output_is_wanted then
         put_escape_code(red_font_code, the_flexowriter_output);
      end if;
   end set_text_colour_to_red;

   procedure set_text_colour_to_black (the_flexowriter_output : in out host_IO.stream) is
   begin
      if the_terminal_is_ANSI_compatible then
         put_escape_code(black_font_code, the_flexowriter_output);
      end if;
   end set_text_colour_to_black;

   procedure set_text_style_to_underline (the_flexowriter_output : in out host_IO.stream) is
   begin
      if the_terminal_is_ANSI_compatible then
         put_escape_code(underline_code, the_flexowriter_output);
      end if;
   end set_text_style_to_underline;

   procedure set_text_style_to_plain (the_flexowriter_output : in out host_IO.stream) is
   begin
      if the_terminal_is_ANSI_compatible then
         put_escape_code(plain_font_code, the_flexowriter_output);
      end if;
   end set_text_style_to_plain;

   overriding
   procedure Initialize (the_FW : in out FW.device) is

      procedure complain (part_1         : in String;
                          part_2, part_3 : in String := "")
      with
         No_Return
      is
         left_quote  : constant String := (if part_2 /= "" then  " «" else "");
         right_quote : constant String := (if part_2 /= "" then  "» " else "");
      begin
         raise operator_error
            with part_1 & left_quote & glyphs_for(part_2) & right_quote & part_3;
      end complain;

      the_data         : String(1 .. max_text_length+1);
      the_data_length  : Natural;

      interaction_file : Ada.Text_IO.File_Type;

   begin -- Initialize
      ensure_UI_is_open;
      the_FW.mode := the_flexowriter_is_writing;
      the_FW.device_name := device_name_of(the_FW);

      if the_FW.device_name = "FW0" then
         -- Attempt to open the command file for the console the_FW.
         begin
            Open(interaction_file, In_File, "FW0");
         response_list_loop:
            while not End_of_File(interaction_file) loop
               if last_interaction = max_interactions then
                  complain("The file FW0 contains too many prompts");
               end if;
               last_interaction := last_interaction + 1;
               Get_Line(interaction_file, the_data, the_data_length);

            exit response_list_loop when the_data_length = 0;

               declare
                  next   : FW.interaction renames interactions(last_interaction);
                  this   : String  := the_data(1..the_data_length);
                  length : Natural := 0;
               begin
                  if the_data_length > max_text_length then
                     complain("The FW0 prompt", this, "is too long");
                  end if;
                  if the_data_length < min_text_length then
                     complain("The FW0 prompt", this, "is too short");
                  end if;

                  for p in this'Range loop
                     if this(p) = ';' then
                        if length /= 0 then
                           complain("The FW0 prompt", this, "contains 2 semicolons");
                        end if;
                        length := p;
                     elsif this(p) = LF_surrogate then
                        -- Convert '®' to LF to allow for multi-line prompts.
                        this(p) := LF;
                     elsif this(p) = FF_surrogate then
                        -- Convert '©' to FF to allow for multi-line prompts.
                        this(p) := FF;
                     end if;
                  end loop;

                  if length = 0 then
                     complain("The FW0 prompt", this, "contains no semicolon");
                  end if;

                  next.text(1..this'Length) := this;
                  next.prompt_length        := length;
                  next.total_length         := this'Length;
               end;

            end loop response_list_loop;

         exception

            when Name_Error =>
               complain("The file FW0 is absent");
            when Use_Error =>
               complain("The file FW0 exists, but cannot be read");
         end;
      end if;

      open(the_FW.stream, the_FW.device_name, read_mode, UI_in_FD);
      open(the_FW.output, the_FW.device_name, write_mode, UI_out_FD);
      IOC.device(the_FW).Initialize;
      the_FW.current_case := KDF9_char_sets.Case_Normal;
   end Initialize;

   -- If authentic timing, the delay of length the_pause is inserted between characters output
   --    to the Flexowriter, with the aim of approximating the actual speed of its typing.
   the_pause  : KDF9.us := 0;

   procedure set_the_duration_of_the_pause (the_FW : in FW.device) is
   begin
      if authentic_timing_is_enabled then
         the_pause := the_FW.quantum;
      else
         the_pause := 0;
      end if;
   end set_the_duration_of_the_pause;

   call_for_manual_input    : constant String (1..2) := (others => BEL);

   procedure inject_a_response (the_FW     : in out FW.device;
                                the_prompt : in String;
                                the_size   : in out KDF9.word) is
   begin
      set_the_duration_of_the_pause(the_FW);
      for t in next_interaction .. last_interaction loop
         declare
            the : interaction renames interactions(t);
         begin
            if the.prompt_length = the.total_length then
               -- A null response, so terminate the program.
               raise exceptions.quit_request with "at the prompt: «"& the_prompt & "»";
            end if;
            next_interaction := next_interaction + 1;
            if the.text(1..the.prompt_length-1) = the_prompt and then
                  the.text(the.prompt_length-0) = ';'            then
               inject(the.text(the.prompt_length+1..the.total_length) & LF, the_FW.stream);
               the_size := the_size + KDF9.word(the.total_length-the.prompt_length);
               put_chars(the.text(the.prompt_length+1..the.total_length) & LF, the_FW.output);
               -- Human operators type much more slowly than KDF9 buffers!
               flush(the_FW.output, the_pause*10);
               the_FW.mode := the_flexowriter_is_reading;
               return;
            end if;
         end;
      end loop;
      -- No canned response is available, so control reverts to the terminal.
      -- Output an audible signal to notify the operator.
      if noninteractive_usage_is_enabled then
         raise input_is_impossible;
      end if;
      put_bytes(call_for_manual_input, the_FW.output);
      flush(the_FW.output, the_pause);
      the_FW.mode := the_flexowriter_is_reading;
   end inject_a_response;

   -- TRQq
   overriding
   procedure PIA (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      if noninteractive_usage_is_enabled then
         raise input_is_impossible;
      end if;
      put_bytes(call_for_manual_input, the_FW.output);
      flush(the_FW.output);
      the_FW.mode := the_flexowriter_is_reading;
      start_slow_transfer(the_FW, Q_operand, set_offline);
      read(the_FW, Q_operand);
      lock_out_relative_addresses(Q_operand);
      reset(the_FW.stream);
   end PIA;

   -- TREQq
   overriding
   procedure PIB (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      if noninteractive_usage_is_enabled then
         raise input_is_impossible;
      end if;
      put_bytes(call_for_manual_input, the_FW.output);
      flush(the_FW.output);
      the_FW.mode := the_flexowriter_is_reading;
      start_slow_transfer(the_FW, Q_operand, set_offline);
      read_to_EM(the_FW, Q_operand);
      lock_out_relative_addresses(Q_operand);
      reset(the_FW.stream);
   end PIB;

   overriding
   procedure PIC (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      if noninteractive_usage_is_enabled then
         raise input_is_impossible;
      end if;
      put_bytes(call_for_manual_input, the_FW.output);
      flush(the_FW.output);
      the_FW.mode := the_flexowriter_is_reading;
      start_slow_transfer(the_FW, Q_operand, set_offline);
      words_read(the_FW, Q_operand);
      lock_out_relative_addresses(Q_operand);
      reset(the_FW.stream);
   end PIC;

   overriding
   procedure PID (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      if noninteractive_usage_is_enabled then
         raise input_is_impossible;
      end if;
      put_bytes(call_for_manual_input, the_FW.output);
      flush(the_FW.output);
      the_FW.mode := the_flexowriter_is_reading;
      start_slow_transfer(the_FW, Q_operand, set_offline);
      words_read_to_EM(the_FW, Q_operand);
      lock_out_relative_addresses(Q_operand);
      reset(the_FW.stream);
   end PID;

   overriding
   procedure PIE (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIA(the_FW, Q_operand, set_offline);
   end PIE;

   overriding
   procedure PIF (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIB(the_FW, Q_operand, set_offline);
   end PIF;

   overriding
   procedure PIG (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIC(the_FW, Q_operand, set_offline);
   end PIG;

   overriding
   procedure PIH (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PID(the_FW, Q_operand, set_offline);
   end PIH;

   -- neat strips off any enclosing non-graphic characters from s.
   function neat (s : String)
   return String is
      l : Positive := 1;
      r : Natural  := 0;
   begin
      for i in s'Range loop
         l := i;
      exit when s(i) > SP and s(i) /= DEL;
      end loop;
      for i in reverse s'Range loop
         r := i;
      exit when s(i) > SP and s(i) /= DEL;
      end loop;
      return s(l..r);  -- s(1..0) yields the null string when s is the null string.
   end neat;

   overriding
   procedure do_output_housekeeping (the_FW   : in out FW.device;
                                     written,
                                     fetched  : in KDF9.word) is
   begin
      flush(the_FW.stream);
      add_in_the_IO_CPU_time(the_FW, fetched);
      correct_transfer_time(the_FW, written);
      the_FW.byte_count := the_FW.byte_count + fetched;
   end do_output_housekeeping;

   underlined : Boolean := False;

   procedure put_symbols (the_FW         : in out FW.device;
                          Q_operand      : in KDF9.Q_register;
                          transfer_to_EM : in Boolean) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      fill   : KDF9.word := 0;
      size   : KDF9.word := 0;
      symbol : KDF9_char_sets.symbol;
      char   : Character;
   begin
      check_addresses_and_lockouts(start_address, end_address);
      set_the_duration_of_the_pause(the_FW);
      the_FW.mode := the_flexowriter_is_writing;
      set_text_style_to_plain(the_FW.output);
      set_text_colour_to_red(the_FW.output);

      -- Ensure that any prompt occupies the buffer alone.
      flush(the_FW.output);

   word_loop:
      for w in start_address .. end_address loop
         for c in KDF9_char_sets.symbol_index'Range loop
            case the_FW.mode is

               when the_flexowriter_is_writing =>
                  symbol := fetch_symbol(w, c);
                  size := size + 1;

                  if symbol = KDF9_char_sets.Word_Filler then
                     fill := fill + 1;

                  elsif symbol = KDF9_char_sets.Case_Shift then
                     the_FW.current_case := KDF9_char_sets.Case_Shift;
                     the_FW.shifts := the_FW.shifts + 1;

                  elsif  symbol = KDF9_char_sets.Case_Normal then
                     the_FW.current_case := KDF9_char_sets.Case_Normal;
                     the_FW.shifts := the_FW.shifts + 1;

                  else

                     if the_FW.current_case = KDF9_char_sets.Case_Normal then
                        char := TP_CN(symbol);
                     else
                        char := TP_CS(symbol);
                     end if;

                     if char = ';' then

                        declare
                           the_prompt : constant String := contents(the_FW.output);
                        begin
                           -- Must flush AFTER saving the prompt and BEFORE going black.
                           flush(the_FW.output, the_pause);
                           set_text_colour_to_black(the_FW.output);
                           set_text_style_to_plain(the_FW.output);
                           put_byte(';', the_FW.output);
                           flush(the_FW.output, the_pause);

                           inject_a_response(the_FW, neat(the_prompt), size);

                           the_FW.mode := the_flexowriter_is_reading;
                           set_text_style_to_plain(the_FW.output);
                        end;

                     elsif flexowriter_output_is_wanted then

                        if char = '_' then
                           underlined := True;
                           do_not_put_byte(char, the_FW.output);
                           flush(the_FW.output, the_pause);
                        else
                           if underlined then
                              set_text_style_to_underline(the_FW.output);
                           end if;
                           put_char(char, the_FW.output);
                           if underlined then
                              flush(the_FW.output, the_pause);
                              set_text_style_to_plain(the_FW.output);
                              set_text_colour_to_red(the_FW.output);
                              underlined := False;
                           end if;
                        end if;

                     else
                        do_not_put_byte(char, the_FW.output);
                     end if;

                     exit word_loop when transfer_to_EM and symbol = KDF9_char_sets.End_Message;
                  end if;

               when the_flexowriter_is_reading =>
                  get_char(char, the_FW.stream);
                  if case_of(char) not in both | the_FW.current_case then
                     store_symbol(CN_TR(next_case(the_FW.current_case)), w, c);
                     size := size + 1;
                     the_FW.current_case := the_FW.current_case xor 1;
                     back_off(the_FW.stream);
                  else
                     if the_FW.current_case = KDF9_char_sets.Case_Normal then
                        symbol := CN_TR(char);
                     else
                        symbol := CS_TR(char);
                     end if;
                     store_symbol(symbol, w, c);
                     size := size + 1;
                     if transfer_to_EM and symbol = KDF9_char_sets.End_Message then
                        for d in 1 .. 7-c loop
                           store_symbol(KDF9_char_sets.Blank_Space, w, c+d);
                        end loop;
                        exit word_loop;
                     end if;
                  end if;

            end case;
         end loop;
      end loop word_loop;

      flush(the_FW.output, the_pause);
      set_text_style_to_plain(the_FW.output);
      set_text_colour_to_black(the_FW.output);
      do_output_housekeeping(the_FW, written => size-fill, fetched => size);
      flush(the_FW.output);
   exception
      when end_of_stream =>
         flush(the_FW.output);
         set_text_colour_to_black(the_FW.output);
         set_text_style_to_plain(the_FW.output);
         do_output_housekeeping(the_FW, written => size-fill, fetched => size);
   end put_symbols;

   overriding
   procedure write (the_FW    : in out FW.device;
                    Q_operand : in KDF9.Q_register) is
   begin
      put_symbols(the_FW, Q_operand, transfer_to_EM => False);
   end write;

   overriding
   procedure write_to_EM (the_FW    : in out FW.device;
                          Q_operand : in KDF9.Q_register) is
   begin
      put_symbols(the_FW, Q_operand, transfer_to_EM => True);
   end write_to_EM;

   -- TWQq
   overriding
   procedure POA (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_FW, Q_operand, set_offline, output_operation);
      write(the_FW, Q_operand);
      lock_out_relative_addresses(Q_operand);
      reset(the_FW.stream);
   end POA;

   -- TWEQq
   overriding
   procedure POB (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_FW, Q_operand, set_offline, output_operation);
      write_to_EM(the_FW, Q_operand);
      lock_out_relative_addresses(Q_operand);
      -- reset(the_FW.stream);
   end POB;

   procedure put_words (the_FW         : in out FW.device;
                        Q_operand      : in KDF9.Q_register;
                        transfer_to_EM : in Boolean := False) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size : KDF9.word := 0;
      word : KDF9.word;
      char : Character;
   begin
      check_addresses_and_lockouts(start_address, end_address);
      set_the_duration_of_the_pause(the_FW);
      the_FW.mode := the_flexowriter_is_writing;
      set_text_style_to_plain(the_FW.output);
      set_text_colour_to_red(the_FW.output);
   word_loop:
      for w in start_address .. end_address loop
         case the_FW.mode is

            when the_flexowriter_is_writing =>
               word := fetch_word(w) and 8#377#;
               size := size + 1;
               char := Character'Val(word);

               if word = KDF9_char_sets.Semi_Colon_tape_bits then
                  -- Hypothesis: POC and POD act like POA and POB with respect to prompting;
                  --    and change from writing to reading after the output of any word that has
                  --       the KDF9 FW tape code for a semicolon in its least significant 8 bits.
                  declare
                     the_prompt : constant String := contents(the_FW.output);
                  begin
                     -- Must flush AFTER saving the prompt and BEFORE going black.
                     flush(the_FW.output, the_pause);
                     set_text_colour_to_black(the_FW.output);
                     set_text_style_to_plain(the_FW.output);
                     put_byte(';', the_FW.output);
                     flush(the_FW.output, the_pause);

                     inject_a_response(the_FW, neat(the_prompt), size);

                     the_FW.mode := the_flexowriter_is_reading;
                     set_text_style_to_plain(the_FW.output);
                  end;
               elsif flexowriter_output_is_wanted then

                        if char = '_' then
                           underlined := True;
                           do_not_put_byte(char, the_FW.output);
                           flush(the_FW.output, the_pause);
                        else
                           if underlined then
                              set_text_style_to_underline(the_FW.output);
                           end if;
                           put_char(char, the_FW.output);
                           if underlined then
                              flush(the_FW.output, the_pause);
                              set_text_style_to_plain(the_FW.output);
                              set_text_colour_to_red(the_FW.output);
                              underlined := False;
                           end if;
                        end if;
      exit word_loop when transfer_to_EM and then word = KDF9_char_sets.End_Message_tape_bits;
               end if;

            when the_flexowriter_is_reading =>
               get_char(char, the_FW.stream);
               size := size + 1;
               word := KDF9.word(Character'Pos(char));
               store_word(word, w);
      exit word_loop when transfer_to_EM and then word = KDF9_char_sets.End_Message_tape_bits;

         end case;
      end loop word_loop;

      flush(the_FW.output);
      set_text_colour_to_black(the_FW.output);
      set_text_style_to_plain(the_FW.output);
      do_output_housekeeping(the_FW, written => size, fetched => size);
   exception
      when end_of_stream =>
         flush(the_FW.output);
         set_text_colour_to_black(the_FW.output);
         set_text_style_to_plain(the_FW.output);
         do_output_housekeeping(the_FW, written => size, fetched => size);
   end put_words;

   overriding
   procedure words_write (the_FW    : in out FW.device;
                          Q_operand : in KDF9.Q_register) is
   begin
      put_words(the_FW, Q_operand, transfer_to_EM => False);
   end words_write;

   overriding
   procedure words_write_to_EM (the_FW    : in out FW.device;
                                Q_operand : in KDF9.Q_register) is
   begin
      put_words(the_FW, Q_operand, transfer_to_EM => True);
   end words_write_to_EM;

   -- TWCQq
   overriding
   procedure POC (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_FW, Q_operand, set_offline, output_operation);
      words_write(the_FW, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end POC;

   -- TWECQq
   overriding
   procedure POD (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_FW, Q_operand, set_offline, output_operation);
      words_write_to_EM(the_FW, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end POD;

   overriding
   procedure Finalize (the_FW : in out FW.device) is
      total : constant KDF9.word := the_FW.output.bytes_moved+the_FW.stream.bytes_moved + the_FW.shifts;
   begin
      close(
           the_FW,
           "transferred",
           total,
           "character" & plurality(total)
          );
   end Finalize;

   -- This is the monitor console Flexowriter.

   type FW_access is access FW.device;

   FW0 : FW_access with Warnings => Off;

   already_enabled : Boolean := False;

   procedure enable (b : in KDF9.buffer_number) is
   begin
      if already_enabled then
         trap_operator_error("more than one FW unit has been configured");
      end if;
      if b /= 0 then
         trap_operator_error("FW0 must be on buffer 0");
      end if;
      FW0 := new FW.device (number => b, unit => 0);
      already_enabled := True;
   end enable;

end IOC.slow.shift.FW;
