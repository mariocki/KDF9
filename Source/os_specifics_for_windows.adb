-- OS_specifics.adb
--
-- Special operations for the console streams.
-- This is the Windows (i.e., crippled) version.
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

with Interfaces.C;
--
with Latin_1;
with POSIX;

use  Latin_1;
use  POSIX;

package body OS_specifics is

   package C renames Interfaces.C;
   use C;

   function get_O_BINARY return C.int;
   pragma Import(C, get_O_BINARY, External_Name => "get_O_BINARY");

   function setmode (fd : C.int; mode : C.int) return C.int;
   pragma Import(C, setmode, External_Name => "setmode");

   procedure make_transparent (fd : in Integer) is
      response : C.int;
   begin
      -- Convince Windows not to corrupt binary data.
      response := setmode(C.int(fd), get_O_BINARY);
      if response < 0 then  -- Either setmode or get_O_BINARY failed.
         raise Program_Error
            with "make_transparent failed, response = " & C.int'Image(response);
      end if;
   end make_transparent;

   function the_terminal_is_ANSI_compatible
   return Boolean
   renames False;

   procedure set_text_colour_to_red (the_flexowriter_output : in out IO.stream) is
   begin
      flush(the_flexowriter_output);
   end set_text_colour_to_red;

   procedure set_text_colour_to_black (the_flexowriter_output : in out IO.stream) is
   begin
      flush(the_flexowriter_output);
   end set_text_colour_to_black;

   function EOL
   return String is
   begin
      return (1 => CR, 2 => LF);
   end EOL;

   procedure open_ui is
      ui_in_name  : constant String := "CONIN$";
      ui_out_name : constant String := "CONOUT$";
   begin
      ui_in_fd := open(ui_in_name, read_mode);
      verify(ui_in_fd, ui_in_name);
      ui_out_fd := open(ui_out_name, write_mode);
      verify(ui_out_fd, ui_out_name);
      ui_is_open := True;
   end open_ui;

end OS_specifics;
