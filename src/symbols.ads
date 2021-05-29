 -- Map object code addresses to Usercode symbolic addresses.
--
-- This file is part of ee9 (7.0a), the GNU Ada emulator of the English Electric KDF9.
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

package symbols is

   procedure clear_all_symbol_definitions;

   procedure declare_P0 (P0v : in KDF9.address);
   procedure declare_Pp (P_number : in Natural; V_count, P_address : in KDF9.address);

   procedure define_Y_size (size : in KDF9.address);

   procedure define_W0 (address : in KDF9.address);
   procedure define_Y0 (address : in KDF9.address);
   procedure define_Z0 (address : in KDF9.address);

   subtype Y_store_id is Character range 'A' ..'Z';

   procedure define_Yy0 (x : in Y_store_id; address : in KDF9.address);

   function P_symbol (address : KDF9.syllable_address; in_octal : Boolean)
   return String;

   function symbolic (address : KDF9.address; in_octal : Boolean; not_for_SET : Boolean := True)
   return String;

   type V_definition is
   record
      P_number  : Natural;
      V_count   : KDF9.address;
      P_address : KDF9.address;
      V_address : KDF9.address;
   end record;

   type V_definition_list is array (Natural range <>) of V_definition;

   function the_V_definition_list
   return V_definition_list;

   function the_V_definition_list_length
   return Natural;

   type address_list is array (Y_store_id) of KDF9.address;

   type non_V_store_table is
      record
         Yy_base : address_list;
         W_base  : KDF9.address := KDF9.address'Last;
         Y_base  : KDF9.address := KDF9.address'Last;
         Y_max   : KDF9.address := 0;
         Z_base  : KDF9.address := KDF9.address'Last;
         Z_min   : KDF9.address := KDF9.address'Last;
      end record;

   function the_non_V_store_table
   return non_V_store_table;

end symbols;
