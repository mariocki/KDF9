-- a2b.adb
--
-- Convert between KDF9 character codes.
--
-- This file is an auxiliary of ee9 (V5.2b), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2021, W. Findlay; all rights reserved.
--
-- This program is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. This program is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details. You should have
-- received a copy of the GNU General Public License distributed with
-- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
--
with KDF9_char_sets;
with OS_specifics;
with POSIX;
with from_5_hole;

use  Ada.Characters.Handling;
use  Ada.Characters.Latin_1;
--
use  KDF9_char_sets;
use  OS_specifics;
use  POSIX;

procedure a2b is

   package CLI renames Ada.Command_Line;

   command_error : exception;

   procedure complain (about : in String) is
   begin
      if about /= "" then
         output_line(about & ".");
      end if;
      output_line("usage: a2b { -r2p | -L2p | -p2c | -p2L | -p2o | -p2r | -p2t}");
      CLI.Set_Exit_Status(CLI.Failure);
      raise command_error;
   end complain;

   subtype load_medium_flag_set is Character
      with Static_Predicate => load_medium_flag_set in 'D' | 'F' | 'M' | 'P' ;

   load_medium : load_medium_flag_set := 'F';  -- The default is whatever the object code file says.

   procedure check_flag_setting is

      argument_1 : constant String := To_Lower(CLI.Argument(1));

   begin -- check_flag_setting
      -- Reject an empty parameter.
      if argument_1'Length = 0 then
         complain("No parameter was given");
      end if;

      -- Fail any non-flag parameter.
      if argument_1(1) /= '-'  then
         complain("The parameter '" & argument_1 & "' is invalid");
      end if;

      -- Fail a too-short flag parameter.
      if argument_1'Length < 4 then
         complain("The parameter '" & argument_1 & "' is too short");
      end if;

      -- Fail a too-long flag parameter.
      if argument_1'Length > 4 then
         complain("The parameter '" & argument_1 & "' is too long");
      end if;

      -- Check for a conversion parameter.
      if argument_1 in "-r2p" | "-l2p" | "-p2c" | "-p2l" | "-p2o" | "-p2r" | "-p2t" | "-f2l" then
         if argument_1 = "-p2c"                             and then
               CLI.Argument_Count = 2                       and then
                  CLI.Argument(2)'Length = 1                and then
                     CLI.Argument(2)(1) in load_medium_flag_set then
            load_medium := CLI.Argument(2)(1);
         elsif CLI.Argument_Count = 2 then
            complain("The parameter '" & CLI.Argument(2) & "' is invalid");
         end if;
         return;
      end if;

      -- The flag is invalid.
      complain("The parameter '" & argument_1 & "' is unrecognized");
   end check_flag_setting;

   type word is mod 2**48;

   function oct_of (w : word)
   return String is
      value : word := w;
      oct   : String(1 .. 16);
   begin
      for i in reverse oct'Range loop
         oct(i) := Character'Val(Natural(value mod 8)+Character'Pos('0'));
         value := value / 8;
      end loop;
      return oct;
   end oct_of;

   type Q_part is mod 2**16;

   function oct_of (w : Q_part)
   return String
   is (oct_of(word(w))(11..16));

   type syllable is mod 2**8;

   function oct_of (w : syllable)
   return String
   is (oct_of(word(w))(14..16));

   function oct_of (w : symbol)
   return String
   is (oct_of(word(w))(15..16));

   function "abs" (c : Character)
   return symbol
   is (CN_TR(c) or CS_TR(c));

   function framed (s : symbol)
   return Natural is
      parity_bits  : constant syllable := 2#00_010_000#;
      channel_bits : constant syllable := 2#10_000_000#;

      function channel_8
      return syllable
      is (if s = 8#000# then channel_bits else 0);

      function parity
      return syllable is
         frame  : syllable := syllable(symbol'Pos(s));
         parity : syllable := 0;
      begin -- parity
         if s = 0 then return parity_bits; end if;
         while frame /= 0 loop
            parity := parity xor (frame mod 2);
            frame  := frame / 2;
         end loop;
         return (if parity = 0 then 0 else parity_bits);
      end parity;

      low_4_bits : constant syllable := syllable(s and 2#001_111#);
      top_2_bits : constant syllable := syllable(s and 2#110_000#);

   begin -- framed
      return syllable'Pos(channel_8 or (top_2_bits*2) or parity or low_4_bits);
   end framed;

   procedure convert_Latin_1_to_paper_tape_code is
      input, output : String(1..1);
      bytes_in, bytes_out : Integer;
   begin -- convert_Latin_1_to_paper_tape_code
      loop
         bytes_in := POSIX.read(0, input, 1);
      exit when bytes_in /= 1;
         output(1) := Character'Val(framed(abs input(1)));
         bytes_out := POSIX.write(1, output, 1);
      exit when bytes_out /= 1;
      end loop;
   end convert_Latin_1_to_paper_tape_code;

   procedure convert_raw_bytes_to_paper_tape_code is
      size   : word := 0;
      o      : syllable;
      s      : symbol;
      w      : word;
      char   : String(1..8);
      last, bytes_in, bytes_out : Integer;
   begin -- convert_raw_bytes_to_paper_tape_code
   main_loop:
      loop
         w := 0;
         last := 0;
         for s in 0 .. 5 loop
            bytes_in := POSIX.read(0, char, 1);
         exit when bytes_in /= 1;
            o := syllable(Character'Pos(char(1)));
            w := w or (word(o) * 256**(5-s));
            size := size + 1;
            last := last + 1;
         end loop;
      exit main_loop when last = 0;
         for c in 1 .. 8 loop
            s := symbol((w / 64**(8-c)) and 8#77#);
            char(c) := Character'Val(framed(s));
         end loop;
         bytes_out := POSIX.write(1, char, 8);
      exit main_loop when bytes_in < 1 or bytes_out < 8 or last < 6;
      end loop main_loop;

      if last /= 0 then
         output_line(NL & "Warning: the last word had only" & last'Image & " data bytes!");
         output_line("It was """ & oct_of(w) & """.");
      end if;
   end convert_raw_bytes_to_paper_tape_code;

   parity_error : exception;
   parity_count : Natural;

   function sixbit (c : Character; p : word)
   return symbol is

      b : constant syllable := syllable(Character'Pos(c));

      function parity
      return syllable is
         frame : syllable := b;
         sum   : syllable := 0;
      begin -- sum
         while frame /= 0 loop
            sum := sum xor (frame mod 2);
            frame  := frame / 2;
         end loop;
         return sum;
      end parity;

      low_4_bits : constant syllable := b and 2#00_001_111#;
      top_2_bits : constant syllable := b and 2#01_100_000#;

   begin -- sixbit
      if parity /= 0 then
         parity_count := parity_count + 1;
         output_line;
         output_line("Parity error: value was #" & oct_of(word(b))(15..16)
                   & ", found at byte #" & oct_of(p)(11..16) & "!");
      end if;
      return symbol(top_2_bits/2 or low_4_bits);
   end sixbit;

   type output_code_table is array (symbol) of Character;

   -- The Case Normal shift paper tape code:
   CN : constant output_code_table
      := (' ',  '"',   LF,   FF,   HT,  '#',  'ß',  'ñ',
          '&',  '?',  '!',  '%',  ''',  '$',  '~',  '/',
          '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
          '8',  '9',  '_',  'º',  ';',  '+',  '-',  '.',
          '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
          'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
          'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
          'X',  'Y',  'Z',  '{',  '}',  '|',  '\',  'Ø'
         );

   -- The Case Shift shift paper tape code:
   CS : constant output_code_table
      := (' ',  '"',   LF,   FF,   HT,  '#',  'ß',  'ñ',
          '&',  '?',  '!',  '%',  ''',  '$',  '~',  ':',
          '^',  '[',  ']',  '<',  '>',  '=',  '×',  '÷',
          '(',  ')',  '_',  '£',  ';',  '±',  '*',  ',',
          '@',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
          'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
          'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
          'x',  'y',  'z',  '{',  '}',  '|',  '\',  'Ø'
         );

   function as_symbol (w : word)
   return Character
   is (glyph_for(CN(symbol(w))));

   procedure convert_paper_tape_code_to_raw_bytes is
      count  : word := 0;
      input,
      output : String(1..1);
      bytes_in,
      bytes_out : Integer;
      byte,
      group  : word;
   begin -- convert_paper_tape_code_to_raw_bytes
      parity_count := 0;
      loop
         group := 0;
         for i in 0 .. 7 loop
            bytes_in := POSIX.read(0, input, 1);
         exit when bytes_in /= 1;
            count := count + 1;
            group := group * 64 + word(sixbit(input(1), count));
         end loop;
         for i in 0 .. 5 loop
            byte := group / 256**(5-i);
            group := group mod 256**(5-i);
            output(1) := Character'Val(byte);
            bytes_out := POSIX.write(1, output, 1);
         exit when bytes_out /= 1;
         end loop;
      exit when bytes_in /= 1 or bytes_out /= 1;
      end loop;
      if parity_count /= 0 then
         raise parity_error;
      end if;
   end convert_paper_tape_code_to_raw_bytes;

   procedure convert_program_A_block_to_call_tape is

      subtype PTC is Character;  -- A paper tape code read in a a byte to a string.

      function sixbit (c : PTC)
         return symbol is

            b : constant syllable := syllable(PTC'Pos(c));

            function parity
            return syllable is
               frame : syllable := b;
               sum   : syllable := 0;
            begin -- sum
               while frame /= 0 loop
                  sum := sum xor (frame mod 2);
                  frame  := frame / 2;
               end loop;
               return sum;
            end parity;

            low_4_bits : constant syllable := b and 2#00_001_111#;
            top_2_bits : constant syllable := b and 2#01_100_000#;

         begin -- sixbit
            if parity /= 0 then
               parity_count := parity_count + 1;
               output_line;
               output_line("Parity error: value was #" & oct_of(word(b))(15..16));
               complain(about => "parity");
            end if;
            return symbol(top_2_bits/2 or low_4_bits);
         end sixbit;

     type call_tape is array (1 .. 64) of KDF9_char_sets.symbol;

      comment   : constant String(18..63) := "CALL TAPE DERIVED FROM THE OBJECT CODE FILE   ";
      skeleton  : constant call_tape
                := (1 =>      Case_Normal,
                    2 =>      Line_Shift,
                    3 =>      Upper_Case_P,
                    4 =>      Line_Shift,
                    5..16 =>  Blank_Space,
                    17 =>     Line_Shift,
                    18..63 => Blank_Space,
                    64 =>     End_Message);

      the_tape  : call_tape := skeleton;

      function "abs" (c : PTC)
      return KDF9_char_sets.symbol
      is (sixbit(c));

      output    : String(1..64);
      input     : String(1..32);
      bytes_in,
      bytes_out : Integer;

   begin -- convert_program_A_block_to_call_tape
      parity_count := 0;
      bytes_in := POSIX.read(0, input, 32);
      if bytes_in /= 32 then
         complain("The input file is too short for a valid KDF9 object program");
      end if;

      input(1..16) := input(17..32);
      if abs input(1) /= Case_Normal                                           or else
            abs input(2) /= Line_Shift                                         or else
               abs input(3) not in Upper_Case_D | Upper_Case_M | Upper_Case_P  or else
                  abs input(4) /= Line_Shift                                      then
            complain("The input file does not have a valid A block");
      end if;
      the_tape(3) := (
                      case load_medium is
                         when 'D' => Upper_Case_D,
                         when 'F' => abs input(3),
                         when 'M' => Upper_Case_M,
                         when 'P' => Upper_Case_P
                     );
      for i in 18..63 loop
         the_tape(i) := CN_TR(comment(i));
      end loop;
      for i in 5..16 loop
         the_tape(i) := abs(input(i));
      end loop;
      for i in 1..64 loop
         output(i) := Character'Val(framed(the_tape(i)));
      end loop;
      bytes_out := POSIX.write(1, output, 64);
      if parity_count /= 0 then
         raise parity_error with bytes_out'Image;
      end if;
   end convert_program_A_block_to_call_tape;

   procedure convert_paper_tape_code_to_Latin_1 is
      count  : word := 0;
      input,
      output : String(1..1);
      bytes_in,
      bytes_out : Integer;
   begin -- convert_paper_tape_code_to_Latin_1
      parity_count := 0;
      loop
         bytes_in := POSIX.read(0, input, 1);
      exit when bytes_in /= 1;
         if input(1) = NUL then
            -- Suppress runout characters in textual output.
            null;
         else
            count := count + 1;
            output(1) := CN(sixbit(input(1), count));
            bytes_out := POSIX.write(1, output, 1);
         end if;
      exit when bytes_in < 1 or bytes_out < 1;
      end loop;
      if parity_count /= 0 then
         raise parity_error;
      end if;
   end convert_paper_tape_code_to_Latin_1;

   Ferranti_Letters_Case : Boolean := True;

   procedure convert_Ferranti_code_to_Latin_1 is
      count     : word := 0;
      input,
      output    : String(1..1);
      bytes_in,
      bytes_out : Integer;
      legible   : Boolean;
   begin -- convert_Ferranti_code_to_Latin_1
      loop
         bytes_in := POSIX.read(0, input, 1);
      exit when bytes_in /= 1;
         count := count + 1;
         output(1) :=  from_5_hole(input(1), Ferranti_Letters_Case, legible);
         if legible then
            bytes_out := POSIX.write(1, output, 1);
         end if;
      exit when bytes_out /= 1;
      end loop;
   end convert_Ferranti_code_to_Latin_1;

   procedure convert_paper_tape_code_to_text is
      count       : word := 0;
      Normal      : Boolean := True;
      input,
      output      : String(1..1);
      bytes_in,
      bytes_out   : Integer;
   begin -- convert_paper_tape_code_to_text
      parity_count := 0;
      loop
         bytes_in := POSIX.read(0, input, 1);
      exit when bytes_in /= 1;
         count := count + 1;
         if input(1) = NUL then
            -- Suppress runout characters in textual output.
            null;
         elsif sixbit(input(1), count) = Case_Normal then
            Normal := True;
         elsif sixbit(input(1), count) = Case_Shift then
            Normal := False;
         elsif sixbit(input(1), count) /= Word_Filler then
            if Normal then
               output(1) := CN(sixbit(input(1), count)); bytes_out := POSIX.write(1, output, 1);
            else
               output(1) := CS(sixbit(input(1), count)); bytes_out := POSIX.write(1, output, 1);
            end if;
         end if;
      exit when bytes_in < 1 or bytes_out < 1;
      end loop;
      if parity_count /= 0 then
         raise parity_error;
      end if;
   end convert_paper_tape_code_to_text;

   procedure convert_paper_tape_code_to_octal_listing is
      count    : word := 0;
      value    : word := 0;  -- the accumulated value of a a full 8 symbols
      last_in  : Natural := 0;
      input    : String(1..1);
      result   : String(1..2);
      bytes_out: Integer with Warnings => Off;  -- only written, never read!
      bytes_in : Integer;
      s        : symbol;

      procedure put_Q_format (w : in word) is
         C_part : constant String := oct_of(Q_part(w / 2**32));
         I_part : constant String := oct_of(Q_part(w / 2**16 mod 2**16));
         M_part : constant String := oct_of(Q_part(w mod 2**16));
      begin
         bytes_out := POSIX.write(1, C_part, C_part'Length);
         result(1) := '/';
         bytes_out := POSIX.write(1, result, 1);
         bytes_out := POSIX.write(1, I_part, I_part'Length);
         result(1) := '/';
         bytes_out := POSIX.write(1, result, 1);
         bytes_out := POSIX.write(1, M_part, M_part'Length);
      end put_Q_format;

      procedure put_syllables_format (w : in word) is
         procedure put_syllables (w : in word; count : Natural) is
            image   : String(1..3);
         begin
            if count = 0 then return; end if;
            put_syllables(w / 2**8, count-1);
            image := oct_of(syllable(w mod 2**8));
            bytes_out := POSIX.write(1, image, image'Length);
            result(1) := ' ';
            bytes_out := POSIX.write(1, result, 1);
         end put_syllables;
      begin -- put_syllables_format
         put_syllables(w, 6);
      end put_syllables_format;

      procedure put_symbols_format (w : in word) is
         procedure put_symbols (w : in word; count : Natural) is
            image   : String(1..1);
         begin
            if count = 0 then return; end if;
            put_symbols(w / 2**6, count-1);
            image(1) := as_symbol(w mod 2**6);
            bytes_out := POSIX.write(1, image, 1);
         end put_symbols;
      begin -- put_symbols_format
         put_symbols(w, 8);
      end put_symbols_format;

      procedure print_word_number (w : in word) is
         count : constant String := w'Image;
         image : String (1 .. 8) := (7 => ':', others => ' ');
      begin
         image(image'Length-count'Last .. image'Length-2) := count(2 .. count'Last);
         image(image'Length-count'Last-1) := 'E';
         bytes_out := POSIX.write(1, image, image'Length);
      end print_word_number;

   begin -- convert_paper_tape_code_to_octal_listing
      parity_count := 0;

      loop
         bytes_in := POSIX.read(0, input, 1);
      exit when bytes_in /= 1;

         if count mod 8 = 0 then
            print_word_number(count/8);
         end if;

         last_in := last_in + 1;
         count := count + 1;
         s := sixbit(input(1), count);
         result := oct_of(s);

         bytes_out := POSIX.write(1, result, 2);

         value := value * 64 + word(s);
         if count mod 8 = 0 then

            result := "  ";
            bytes_out := POSIX.write(1, result, 2);
            result := "Q ";
            bytes_out := POSIX.write(1, result, 2);
            put_Q_format(value);

            result := "  ";
            bytes_out := POSIX.write(1, result, 2);
            result := "S ";
            bytes_out := POSIX.write(1, result, 2);
            put_syllables_format(value);

            result := "  ";
            bytes_out := POSIX.write(1, result, 2);
            result := " «";
            bytes_out := POSIX.write(1, result, 2);
            put_symbols_format(value);
            result := "» ";
            bytes_out := POSIX.write(1, result, 2);

            result(1..NL'Length) := NL;
            bytes_out := POSIX.write(1, result, NL'Length);

         elsif count mod 4 = 0 then

            result(1) := ' ';
            bytes_out := POSIX.write(1, result, 1);

         end if;
      end loop;

      if result(1..NL'Length) /= NL then
         result(1..NL'Length) := NL;
         bytes_out := POSIX.write(1, result, NL'Length);
      end if;

      if last_in mod 8 /= 0 then
         output_line("PTC data warning: input was not a multiple of 8 characters!");
      end if;

      if parity_count /= 0 then
         raise parity_error;
      end if;
   end convert_paper_tape_code_to_octal_listing;

   procedure do_the_requested_conversion is
      argument : constant String := To_Lower(CLI.Argument(1)(1..4));
   begin -- do_the_requested_conversion
      if    argument = "-r2p" then
         convert_raw_bytes_to_paper_tape_code;
      elsif argument = "-l2p" then
         convert_Latin_1_to_paper_tape_code;
      elsif argument = "-p2c" then
         convert_program_A_block_to_call_tape;
      elsif argument = "-p2l" then
         convert_paper_tape_code_to_Latin_1;
      elsif argument = "-p2t" then
         convert_paper_tape_code_to_text;
      elsif argument = "-p2r" then
         convert_paper_tape_code_to_raw_bytes;
      elsif argument = "-p2o" then
         convert_paper_tape_code_to_octal_listing;
      elsif argument = "-f2l" then
         convert_Ferranti_code_to_Latin_1;
      else
         complain("Something went wrong");
      end if;
   end do_the_requested_conversion;

begin -- a2b
   if CLI.Argument_Count not in 1..2 then
      complain("The wrong number of parameters was given");
   end if;
   check_flag_setting;

   make_transparent(fd => 0);
   make_transparent(fd => 1);
   make_transparent(fd => 2);
   ensure_UI_is_open;

   do_the_requested_conversion;

exception

   when command_error =>
      null;
   when parity_error =>
      output_line(parity_count'Image & " parity error(s) found.");

end a2b;
