-- disassembly.ads
--
-- Produce dis-assembled instructions in an approximation to KDF9 Usercode.
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

with KDF9;

use  KDF9;

package disassembly is

   pragma Unsuppress(All_Checks);

   function machine_code (decoded : KDF9.decoded_order)
   return String;

   function one_syllable_order_name (decoded : KDF9.decoded_order)
   return String;

   function two_syllable_order_name (decoded : KDF9.decoded_order)
   return String;

   function normal_jump_order_name (decoded      : KDF9.decoded_order;
                                    octal_option : Boolean)
   return String;

   function data_access_order_name (decoded      : KDF9.decoded_order;
                                    octal_option : Boolean)
   return String;

   function the_name_of (order : KDF9.decoded_order; octal_option : Boolean := True)
   return String;

   function the_order (order : KDF9.syllable_group; octal_option : Boolean)
   return String;

   function the_order_at (address : KDF9.code_point; octal_option : Boolean)
   return String;

   function the_skeleton_order (syllable_0 : KDF9.syllable)
   return String;

end disassembly;
