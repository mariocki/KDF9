-- OS_specifics.ads
--
-- Special operations for the console terminal streams.
-- This specification is the same for the Windows, Linux, OS X and UNIX versions;
--    although not all features are used in all system types.
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

with IO;

use  IO;

package OS_specifics is

   pragma Unsuppress(All_Checks);

   -- open_ui opens /dev/tty on UNIX systems and CON{IN|OUT}$ on Windows.
   procedure open_ui;

   -- make_transparent sets the "binary" mode of I/O on Windows/Cygwin;
   --    it does nothing on UNIX-family systems, where no such precaution is necessary.
   procedure make_transparent (fd : in Integer);

   function the_terminal_is_ANSI_compatible
   return Boolean;

   -- set_text_colour_to_* is effective iff the_terminal_is_ANSI_compatible yields True.
   procedure set_text_colour_to_red   (the_flexowriter_output : in out IO.stream);

   procedure set_text_colour_to_black (the_flexowriter_output : in out IO.stream);

   -- EOL returns the appropriate line terminator for the selected host OS:
   -- LF for OS X/UNIX/Linux,
   -- CRLF for Windows.
   function EOL
   return String;

end OS_specifics;
