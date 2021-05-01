-- Settings-reader I/O support.
--
-- This file is part of ee9 (6.3b), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Characters.Latin_1;
--
with file_interfacing;
with KDF9;

use  Ada.Characters.Latin_1;

package body settings.IO is

   procedure open_options_file (file : in out File_Type; name : in String) is
   begin
      file_interfacing.initialize(file, in_file, name);
      line_number := 1;
   exception
      when others =>
         raise Status_Error with name;
   end open_options_file;

   procedure close_options_file (file : in out File_Type; name : in String) is
   begin
      file_interfacing.finalize(file, name);
   end close_options_file;

   comment_flag_character : constant Character := '|';

   procedure skip_to_next_non_blank (file : File_Type) is
      next_char : Character := ' ';
      end_line  : Boolean;
   begin
      loop
        look_ahead(file, next_char, end_line);
      exit when end_line or else (next_char not in ' ' | HT);
         get(file, next_char);
      end loop;
      if next_char = comment_flag_character then
         while not end_of_line(file) loop
            get(file, next_char);
         end loop;
      end if;
   end skip_to_next_non_blank;

   procedure ensure_not_at_end_of_line (file : File_Type) is
   begin
      skip_to_next_non_blank (file);
      if end_of_line(file) then
         raise Data_Error;
      end if;
   end ensure_not_at_end_of_line;

   procedure skip_to_next_nonempty_line (file : in File_Type) is
      flag     : Character;
      end_line : Boolean;
   begin
      loop
         look_ahead(file, flag, end_line);
         if end_line                      or else
               flag = comment_flag_character then
            Skip_Line(file);
            line_number := line_number + 1;
         else
            exit;
         end if;
      end loop;
      if flag = comment_flag_character then
         raise Data_Error;
      end if;
   end skip_to_next_nonempty_line;

   digit_offset : constant := Character'Pos('0');

   procedure get_octal (file : in File_Type; value : out KDF9.word) is
      next_char : Character;
      last_char : Character := '_';
      place     : Natural   := 0;
      end_line  : Boolean   := False;
   begin
      value := 0;
      ensure_not_at_end_of_line(file);
      get(file, next_char);
      if next_char = '#' then
         get(file, next_char);
      else
         raise Data_Error;
      end if;
      loop
         if next_char in '0' .. '7' then
            value := value*8 + KDF9.word(Character'Pos(next_char)-digit_offset);
            place := place + 1;
            if place > 16 then
               raise Data_Error;
            end if;
         elsif next_char = '_' then
            if place = 0 then
               raise Data_Error;
            end if;
         else
            if last_char = '_' or place = 0 then
               raise Data_Error;
            end if;
            exit;
         end if;
         last_char := next_char;
         look_ahead(file, next_char, end_line);
      exit when end_line;
         if next_char in '0' .. '7' or next_char = '_' then
            get(file, next_char);
         else
            if last_char = '_' or place = 0 then
               raise Data_Error;
            end if;
            exit;
         end if;
      end loop;
   end get_octal;

   procedure get_decimal (file : in File_Type; value : out KDF9.word) is
      next_char : Character;
      last_char : Character := '_';
      place     : Natural   := 0;
      end_line  : Boolean   := False;
   begin
      value := 0;
      ensure_not_at_end_of_line(file);
      get(file, next_char);
      if next_char not in '0' .. '9' then
         raise Program_Error with "get_decimal " & next_char;
      end if;
      loop
         if next_char in '0' .. '9' then
            value := value*10 + KDF9.word(Character'Pos(next_char)-digit_offset);
            place := place + 1;
            if place > 15 then
               raise Data_Error;
            end if;
         elsif next_char = '_' then
            if place = 0 then
               raise Data_Error;
            end if;
         else
            if last_char = '_' or place = 0 then
               raise Data_Error;
            end if;
      exit;
         end if;
         last_char := next_char;
         look_ahead(file, next_char, end_line);
      exit when end_line;
         if next_char in '0' .. '9' or next_char = '_' then
            get(file, next_char);
         else
            if last_char = '_' or place = 0 then
               raise Data_Error;
            end if;
      exit;
         end if;
      end loop;
   end get_decimal;

   procedure get_word (file : in File_Type; value : out KDF9.word) is
      next_char : Character;
      end_line  : Boolean;
   begin
      ensure_not_at_end_of_line(file);
      look_ahead(file, next_char, end_line);
      pragma Unreferenced(end_line);
      if next_char = '#' then
         get_octal(file, value);
      else
         get_decimal(file, value);
      end if;
   end get_word;

   procedure get_char (file : in File_Type; value : out Character) is
      end_line : Boolean;
      char     : Character;
   begin
      ensure_not_at_end_of_line(file);
      look_ahead(file, char, end_line);
      if end_line then
         raise Data_Error;
      end if;
      if char /= ' ' then
         get(file, value);
      end if;
   end get_char;

end settings.IO;

