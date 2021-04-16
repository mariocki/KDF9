-- Specific feature values and operation for the console terminal streams.
-- This is the Windows (i.e., somewhat crippled) version.
--
-- This file is part of ee9 (6.2r), the GNU Ada emulator of the English Electric KDF9.
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

with Interfaces.C;

with settings;

package body OS_specifics is

   package C renames Interfaces.C;
   use C;

   function get_O_BINARY
   return C.int
      with Import, Convention => C, External_Name => "get_O_BINARY";

   function setmode (fd : C.int; mode : C.int)
   return C.int
      with Import, Convention => C, External_Name => "_setmode";

   procedure make_transparent (fd : in Integer) is
      response : C.int;
   begin
      -- Convince Windows not to corrupt binary data.
      response := setmode(C.int(fd), get_O_BINARY);
      if response < 0 then  -- Either setmode or get_O_BINARY failed.
         raise Program_Error with "make_transparent fails; _setmode response was " & response'Image;
      end if;
   end make_transparent;

   function EOL
   return String
   is (1 => Character'Val(16#0D#), 2 => Character'Val(16#0A#));

   function UI_in_name
   return String
   is ("CONIN$");

   function UI_out_name
   return String
   is ("CONOUT$");

end OS_specifics;
