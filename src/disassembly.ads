-- Produce dis-assembled instructions in an approximation to KDF9 Usercode.
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

with KDF9;

use  KDF9;

package disassembly is

   function the_code_and_name_of_INS
   return String;

   function the_full_name_of (order        : KDF9.decoded_order;
                              octal_option : Boolean := True;
                              both_bases   : Boolean := True)
   return String;

   function the_short_name_of (syllable_0 : KDF9.syllable)
   return String;

end disassembly;
