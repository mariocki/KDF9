-- Map object code addresses to Usercode symbolic addresses.
--
-- This file is part of ee9 (8.0k), the GNU Ada emulator of the English Electric KDF9.
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

with formatting;

use  formatting;

package body disassembly.symbols is

   procedure set_Z_min is
   begin
      if the_WYZ_table.Y_max = 0 or the_WYZ_table.Y_base = KDF9.address'Last then
         the_WYZ_table.Z_min := the_WYZ_table.Z_base - (the_WYZ_table.Y_base+the_WYZ_table.Z_base)/8;
      else
         the_WYZ_table.Z_min := the_WYZ_table.Y_base + the_WYZ_table.Y_max;
      end if;
   end set_Z_min;

   procedure define_W0 (address : in KDF9.address) is
   begin
      the_WYZ_table.W_base := address;
   end define_W0;

   procedure define_Y0 (address : in KDF9.address) is
   begin
      the_WYZ_table.Y_base := address;
   end define_Y0;

   procedure define_Z0 (address : in KDF9.address) is
   begin
      the_WYZ_table.Z_base := address;
      set_Z_min;
   end define_Z0;

   procedure define_Yy0 (x : in Y_store_id; address : in KDF9.address) is
   begin
      the_WYZ_table.Yy_base(x) := address;
   end define_Yy0;

   function Y_symbol (address : KDF9.address)
   return String is
   begin
      if address >= the_WYZ_table.Z_min then
         return "Z" & trimmed(KDF9.address'Image(the_WYZ_table.Z_base - address));
      end if;
      if address >= the_WYZ_table.Y_base then
         return "Y" & trimmed(KDF9.address'Image(address - the_WYZ_table.Y_base));
      end if;
      for y in reverse Y_store_id loop
         if address >= the_WYZ_table.Yy_base(y) then
            return "Y" & y & trimmed(KDF9.address'Image(address - the_WYZ_table.Yy_base(y))) ;
         end if;
      end loop;
      if address >= the_WYZ_table.W_base then
         return "W" & trimmed(KDF9.address'Image(address - the_WYZ_table.W_base));
      end if;
      return "E" & trimmed(KDF9.address'Image(address));
   end Y_symbol;

   procedure declare_P0 (P0v : in KDF9.address) is
   begin
      V_store_base(0) := (P_number => 0, V_count => P0v, P_address => P0v+8,  V_address => 8);
   end declare_P0;

   procedure define_Y_size (size : in KDF9.address) is
   begin
      the_WYZ_table.Y_max := size;
   end define_Y_size;

   procedure declare_Pp (P_number : in Natural; V_count, P_address : in KDF9.address) is
      V_address : constant KDF9.address := P_address - V_count;
   begin
      last_P_number := last_P_number + 1;
      if last_P_number not in 0 .. 999 then
         raise Program_Error with "number of V store bases >" & Integer'Image(V_store_base'Last);
      end if;
      V_store_base(last_P_number)   := (P_number, V_count, P_address, V_address);
      V_store_base(last_P_number+1) := (8191, 8191, 8191, 8191);
   end declare_Pp;

   function V_symbol (address : KDF9.address; in_octal : Boolean; not_for_SET : Boolean := True)
   return String is
   begin
      if address > 255 or not_for_SET then
         for p in 0 .. last_P_number loop
         exit when address < V_store_base(p).V_address;
            if V_store_base(p).V_count /= 0                                       and then
                  address in V_store_base(p).V_address .. V_store_base(p).P_address   then
                  -- N.B. NOT "V_store_base(p).P_address-1" to avoid wrap-around.
               return "V"
                    & trimmed(KDF9.address'Image(address - V_store_base(p).V_address))
                    & "P"
                    & trimmed(Natural'Image(V_store_base(p).P_number));
            end if;
         end loop;
      end if;
      return "E"
           & (if in_octal then "#" & oct_of(address, 1) else trimmed(KDF9.address'Image(address)));
   end V_symbol;

   function P_symbol (address : KDF9.syllable_address; in_octal : Boolean)
   return String is
   begin
      for p in 0 .. last_P_number loop
         if KDF9.address(address.code_address) = V_store_base(p).P_address then
            return "P" & trimmed(Natural'Image(V_store_base(p).P_number));
         end if;
      end loop;
      return "E" & oct_or_dec_of(address, in_octal);
   end P_symbol;

   function symbolic (address : KDF9.address; in_octal : Boolean; not_for_SET : Boolean := True)
   return String is
      Y_store : constant String := Y_symbol(address);
   begin
     return (if Y_store(1) = 'E' then V_symbol(address, in_octal, not_for_SET) else Y_store);
   end symbolic;

   procedure clear_all_symbol_definitions is
   begin
      the_WYZ_table.Yy_base := (others => KDF9.address'Last);
      V_store_base  := (others => (0, 0, 0, 0));
      last_P_number := 0;
      V_store_base(0) := (P_number => 0, V_count => 0, P_address => 8,  V_address => 8);
   end clear_all_symbol_definitions;

end disassembly.symbols;
