-- Settings-reader I/O support.
--
-- This file is part of ee9 (9.0p), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Text_IO;
--
with postscript;

use  Ada.Text_IO;

package settings.IO is

   line_number : Natural := 0;

   procedure open_options_file (file : in out File_Type; name : in String);

   procedure close_options_file (file : in out File_Type; name : in String);

   -- Check that the end of the line has not yet been reached, else raise Data_Error.
   procedure ensure_not_at_end_of_line (file : in File_Type);

   -- Move the reading position to the next non-blank or EOL, skipping comment.
   procedure skip_to_next_non_blank (file : in File_Type);

   -- Discard input until a non-empty line is reached,
   --    leaving the reading position at the start of that line,
   --    and incrementing line_number for each line terminator passed.
   procedure skip_to_next_nonempty_line (file : in File_Type);

   -- Read octal digits string as KDF9.word,
   --    raising Data_Error on overflow or bad syntax.
   procedure get_octal (file : in File_Type; value : out KDF9.word);

   -- Read decimal digits string as KDF9.word,
   --    raising Data_Error on overflow or bad syntax.
   procedure get_decimal (file  : in File_Type; value : out KDF9.word);

   -- Read an address as a KDF9.word in either octal or decimal,
   --    using get_octal or get_decimal as indicated by the syntax.
   procedure get_word (file : in File_Type; value : out KDF9.word);

   -- Read the character value immediately following an octal or decimal number,
   --    if it is not a space character; if it is a space, leave value unchanged.
   procedure get_char (file : in File_Type; value : out Character);

   package colour_IO is new Ada.Text_IO.Enumeration_IO(postscript.pen_colour);
   package  width_IO is new Ada.Text_IO.Enumeration_IO(postscript.pen_tip_size);

end settings.IO;
