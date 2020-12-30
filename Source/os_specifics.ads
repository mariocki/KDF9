-- OS_specifics.ads
--
-- Specific feature values and operation for the console terminal streams.
-- This specification is the same for Windows, Linux, macOS and UNIX versions of ee9.
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

package OS_specifics is

   -- make_transparent sets the "binary" mode of I/O on Windows/Cygwin.
   -- It does nothing on UNIX-family systems, where no such precaution is necessary.
   procedure make_transparent (fd : in Integer);

   -- UI_in_name returns the interactive input device name appropriate to the host OS, e.g.:
   -- "/dev/tty"  for macOS/UNIX/Linux,
   -- "CONIN$" for Windows.
   function UI_in_name
   return String;

   -- UI_out_name returns the interactive output device name appropriate to the host OS, e.g.:
   -- "/dev/tty"  for macOS/UNIX/Linux,
   -- "CONOUT$" for Windows.
   function UI_out_name
   return String;

   -- EOL returns the appropriate line terminator for the selected host OS, e.g.:
   -- LF for macOS/UNIX/Linux,
   -- CRLF for Windows.
   function EOL
   return String;

end OS_specifics;
