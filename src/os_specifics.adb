-- OS_specifics.adb
--
-- Specific feature values and operation for the console terminal streams.
-- This is the Linux, macOS and UNIX version.
--
-- This file is part of ee9 (V5.2b), the GNU Ada emulator of the English Electric KDF9.
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

package body OS_specifics is

   procedure make_transparent (fd : in Integer) is null;

   function EOL
   return String
   is (1 => Character'Val(16#0A#));

   function UI_in_name
   return String
   is ("/dev/tty");

   function UI_out_name
   return String
   is ("/dev/tty");

end OS_specifics;
