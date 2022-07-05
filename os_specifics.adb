-- Specific feature values and operation for the console terminal streams.
--
-- This file is part of ee9 (8.2a), the GNU Ada emulator of the English Electric KDF9.
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

with imported_value_of;

package body OS_specifics is

   host_kind : constant String  := imported_value_of("OS_TYPE", "POSIX");

   POSIX_API : constant Boolean := host_kind /= "WINDOWS";

   NL   : constant String := (1 => Character'Val(16#0A#));
   CRLF : constant String := (1 => Character'Val(16#0D#), 2 => Character'Val(16#0A#));

   function EOL
   return String
   is (if POSIX_API then NL else CRLF);

   function UI_in_name
   return String
   is (if POSIX_API then "/dev/tty" else "CONIN$");

   function UI_out_name
   return String
   is (if POSIX_API then "/dev/tty" else "CONOUT$");

end OS_specifics;
