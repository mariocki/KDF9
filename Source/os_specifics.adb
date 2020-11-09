-- os_specifics.adb
--
-- Special operations for the console streams.
-- This is the Linux, OS X and UNIX version.
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

with Latin_1;
with POSIX;

use  Latin_1;
use  POSIX;

package body OS_specifics is

   procedure make_transparent (fd : in Integer) is
      pragma Unreferenced(fd);
   begin
      null;  -- Real POSIX systems are already transparent!
   end make_transparent;

   function the_terminal_is_ANSI_compatible
   return Boolean
   renames True;

   red_font_code   : constant String := ESC & "[1;31m";
   black_font_code : constant String := ESC & "[22m" & ESC & "[39m";

   procedure set_text_colour_to_red (the_flexowriter_output : in out IO.stream) is
   begin
      put_bytes(red_font_code, the_flexowriter_output);
      flush(the_flexowriter_output);
   end set_text_colour_to_red;

   procedure set_text_colour_to_black (the_flexowriter_output : in out IO.stream) is
   begin
      put_bytes(black_font_code, the_flexowriter_output);
      flush(the_flexowriter_output);
   end set_text_colour_to_black;

   function EOL
   return String is
   begin
      return (1 => LF);
   end EOL;

   procedure open_ui is
      ui_in_name  : constant String := "/dev/tty";
      ui_out_name : constant String := "/dev/tty";
   begin
      ui_in_fd := open(ui_in_name, read_mode);
      verify(ui_in_fd, ui_in_name);
      ui_out_fd := open(ui_out_name, write_mode);
      verify(ui_out_fd, ui_out_name);
      ui_is_open := True;
   end open_ui;

end OS_specifics;
