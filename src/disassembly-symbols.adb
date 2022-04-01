-- Map object code addresses to Usercode data_label addresses.
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

with KDF9.imaging;
with string_editing;

use  KDF9.imaging;
use  string_editing;

package body disassembly.symbols is

   T : non_V_store_table renames the_WYZ_table;

   procedure set_whole_program_data (size, E0_jump : in KDF9.word) is
      safe_size   : constant KDF9.Q_part := KDF9.Q_part(size and 32767);
      -- Construct an address from the jump order.
      module_bit  : constant KDF9.word := E0_jump / 2**7 and 4096;
      word_number : constant KDF9.word :=  E0_jump and 4095;
      code_base   : constant KDF9.code_address := KDF9.code_address(module_bit + word_number);
   begin
      T.data_max := safe_size;
      T.Z_base := safe_size-1;
      if T.data_max <= 8191 then
         T.code_max := KDF9.code_address(T.data_max);
      else
         T.code_max := 8191;
      end if;
      T.code_max := KDF9.code_address'Max(T.code_max, code_base);
      if code_base > 8 then
         P_store_base(0).V_address := 8;
      else
         P_store_base(0).V_address := 0;
      end if;
      P_store_base(0).P_address := KDF9.Q_part(code_base);
      P_store_base(0).V_max := (if code_base <= 8 then 0 else Natural(code_base) - 8);
   end set_whole_program_data;

   procedure set_Y_size (size : in KDF9.Q_part) is
   begin
      T.Y_size := size;
   end set_Y_size;

   procedure set_W0 (address : in KDF9.Q_part) is
   begin
      T.W_base := address;
   end set_W0;

   procedure set_Y0 (address : in KDF9.Q_part) is
   begin
      T.Y_base := address;
   end set_Y0;

   procedure set_Yy0 (y : in Y_store_id; address : in KDF9.Q_part) is
   begin
      T.Yy_base(y) := address;
   end set_Yy0;

   P0_was_missing : Boolean := True;

   procedure set_Z0 (address : in KDF9.Q_part) is
      a : KDF9.Q_part;
   begin
      T.Z_base := address;
      if T.Y_size = 0 or T.Y_base = KDF9.Q_part'Last then
         T.Z_min := T.Z_base - 64;  -- This is an arbitrary allowance.
      else
         T.Z_min := T.Y_base + T.Y_size;
      end if;

      -- Z0 is the last regional symbol to be defined, so we can now bound the code and data areas.
      a := KDF9.Q_part'Min(8191, T.Y_base);
      a := KDF9.Q_part'Min(a, T.Yy_base('A'));
      a := KDF9.Q_part'Min(a, T.W_base-1);
      -- a is guaranteed to be < 8192.
      T.code_max := KDF9.code_address(a);

      T.Y_last := T.Y_base + T.Y_size;
      T.data_max := KDF9.Q_part'Min(T.Z_base, 32767);
      T.data_max := KDF9.Q_part'Min(T.Y_last, T.data_max);

      if P0_was_missing then
         P_store_base(0).P_address := 8 + KDF9.address(P_store_base(0).V_max);
      end if;
   end set_Z0;

   procedure set_main_program_V_size (V_max : in Natural) is
   begin
      P_store_base(0).P_address := 0;
      P_store_base(0).P_number := 0;
      P_store_base(0).V_max := set_main_program_V_size.V_max;
      P_store_base(0).V_address := 8;
      P_store_base(1) := (8191, 8191, 8191, 8191);
   end set_main_program_V_size;

   procedure site_P0 (P_address : in KDF9.Q_part) is
   begin
      P_store_base(0).P_address := site_P0.P_address;
      P0_was_missing := False;
   end site_P0;

   procedure site_Pp (P_number : in Natural; P_address : in KDF9.Q_part) is
   begin
      -- Handle P0 declarations specially.
      if P_number = 0 then site_P0(P_address); return; end if;
      last_P_number := last_P_number + 1;
      if last_P_number = P_store_base'Last then
         raise Program_Error with "number of P store bases >" & Integer'Image(P_store_base'Last-1);
      end if;
      P_store_base(last_P_number)   := (P_number, 0, P_address, 0);
      P_store_base(last_P_number+1) := (8191, 8191, 8191, 8191);
   end site_Pp;

   procedure site_Pp (P_number : in Natural; P_address : in KDF9.Q_part; V_max : in Natural) is
      V_address : constant KDF9.Q_part := P_address - KDF9.Q_part(V_max + 1);
   begin
      -- Handle P0 declarations specially.
      if P_number = 0 then site_P0(P_address); return; end if;
      last_P_number := last_P_number + 1;
      if last_P_number = P_store_base'Last then
         raise Program_Error with "number of P store bases >" & Integer'Image(P_store_base'Last-1);
      end if;
      P_store_base(last_P_number)   := (P_number, V_max, P_address, V_address);
      P_store_base(last_P_number+1) := (8191, 8191, 8191, 8191);
   end site_Pp;

   function Y_symbol (address : KDF9.Q_part)
   return String is
      last_Y : KDF9.Q_part := KDF9.Q_part'Last;
   begin
      if T.Y_base = KDF9.Q_part'Last then
         for y in reverse Y_store_id loop
            if T.Yy_base(y) < KDF9.Q_part'Last then
               last_Y := T.Yy_base(y);
         exit;
            end if;
         end loop;
      else
         last_Y := T.Y_base;
      end if;

      if last_Y = KDF9.Q_part'Last then
         return "Z" & trimmed(KDF9.Q_part'Image(T.Z_base - address));
      end if;

      if address >= T.Y_base then
         return "Y" & trimmed(KDF9.Q_part'Image(address - T.Y_base));
      end if;

      for y in reverse Y_store_id loop
         if address >= T.Yy_base(y) then
            return "Y" & y & trimmed(KDF9.Q_part'Image(address - T.Yy_base(y))) ;
         end if;
      end loop;

      if address >= T.W_base then
         return "W" & trimmed(KDF9.Q_part'Image(address - T.W_base));
      end if;
      return "";
   end Y_symbol;

   function V_symbol (address : KDF9.Q_part)
   return String is
   begin
      if address > 7 then
         for p in reverse 0 .. last_P_number loop
            if P_store_base(p).V_address /= 0                                  and then
               address in P_store_base(p).V_address .. P_store_base(p).P_address-1 then
               return "V"
                    & trimmed(KDF9.Q_part'Image(address - P_store_base(p).V_address))
                    & "P"
                    & trimmed(Natural'Image(P_store_base(p).P_number));
            end if;
         end loop;
      end if;
      return "";
   end V_symbol;

   function V_store_count (address : KDF9.syllable_address)
   return Natural is
   begin
      for p in 0 .. last_P_number loop
         if P_store_base(p).P_address = KDF9.Q_part(address.code_address) then
            return (if P_store_base(p).V_address = 0 then 0 else Natural(P_store_base(p).V_max+1));
         end if;
      end loop;
      return 0; -- raise error??
   end V_store_count;

   function SET_operand (value : KDF9.Q_part; in_octal : Boolean)
   return String is
      smallish : constant Boolean := value < 256 or value > KDF9.Q_part'Last - 256;
      negative : constant Boolean := value > 2**15 - 1;
      place    : constant String  := oct_or_dec_of(value, in_octal);
      E_store  : constant String  := "E" & place;
      Y_store  : constant String  := Y_symbol(value);
      name     : constant String  := (if Y_store = "" then V_symbol(value) else Y_store);
      basis    : constant String  := (if smallish then place elsif name = "" then E_store else name);
   begin
      return
         (if smallish or basis(basis'First) = 'E' then
            (
              if value < 8 then
                 oct_of(value, 1)
              elsif in_octal then
                 "B"
               & place(2..place'Last)
               & (if value > 7 then ";("  & signed_dec_of(value) & ")" else "")
              elsif negative then
                 signed_dec_of(value)
               & ";(#"
               & oct_of(value, 1)
               & ")"
              else
                 place
               & (if value > 9 then ";(#" & oct_of(value, 1) & ")" else "")
            )
         else
            "A"
          & basis
          & ";("
          & oct_or_dec_of(value, not in_octal)
          & ")"
         );
   end SET_operand;

   function data_operand (address : KDF9.Q_part; in_octal : Boolean)
   return String is
      E_store : constant String := "E" & oct_and_dec_of(address, in_octal, ";(", ")");
      Y_store : constant String := Y_symbol(address);
      name    : constant String := (if Y_store = "" then V_symbol(address) else Y_store);
   begin
      return (if name = "" then E_store else name);
   end data_operand;

   function data_label (address : KDF9.Q_part; in_octal : Boolean)
   return String is
      E_name : constant String := "E" & oct_and_dec_of(address, in_octal, ", E", "");
      name   : constant String := data_operand(address, in_octal);
   begin
      return (if name(1) = 'E' then E_name else name & ", " & E_name);
   end data_label;

   function code_operand (address : KDF9.syllable_address; in_octal : Boolean)
   return String is
   begin
      for p in 0 .. last_P_number loop
         if KDF9.Q_part(address.code_address) = P_store_base(p).P_address then
            return "P" & trimmed(Natural'Image(P_store_base(p).P_number));
         end if;
      end loop;
      return "E" & oct_or_dec_of(address, in_octal);
   end code_operand;

   function routine_name (address : KDF9.syllable_address; in_octal : Boolean)
   return String is
      word_address : constant KDF9.Q_part := KDF9.Q_part(address.code_address);
      addendum     : constant String := "E" & oct_and_dec_of(word_address, in_octal, ", E", "");
   begin
      for p in 0 .. last_P_number loop
         if word_address = P_store_base(p).P_address then
            return "P"
                 & trimmed(Natural'Image(P_store_base(p).P_number))
                 & (
                    if P_store_base(p).V_address /= 0
                    then "V" & trimmed(P_store_base(p).V_max'Image)
                    else ""
                   )
                 & ", "
                 & addendum;
         end if;
      end loop;
      return addendum;
   end routine_name;

   function a_routine_starts_at (address : KDF9.syllable_address)
   return Boolean is
      word_address : constant KDF9.Q_part := KDF9.Q_part(address.code_address);
   begin
      if last_P_number /= 0 then
         for p in 0 .. last_P_number loop
            if word_address = P_store_base(p).P_address then
               return True;
            end if;
         end loop;
      end if;
      return False;
   end a_routine_starts_at;

   procedure clear_all_symbol_definitions is
   begin
      T.W_base   := 32767;
      T.Yy_base := (others => 32767);
      T.Y_base   := 32767;
      T.Y_size   := 0;
      T.Y_last   := 32767;
      T.Z_min    := 32767;
      T.Z_base   := 32767;
      T.data_max := 32767;
      T.code_max := 8191;
      P_store_base  := (others => (0, 0, 0, 0));
      last_P_number := 0;
   end clear_all_symbol_definitions;

   function bounded_code_address (address : KDF9.syllable_address)
   return KDF9.syllable_address is
      word_number : constant KDF9.code_address := address.code_address;
   begin
      return (KDF9.code_address'Min(T.code_max, word_number), 0);
   end bounded_code_address;

   function bounded_code_address (address : KDF9.Q_part)
   return KDF9.syllable_address is
      word_number : constant KDF9.code_address := KDF9.code_address(address);
   begin
      return bounded_code_address((word_number, 0));
   end bounded_code_address;

   function bounded_data_address (address : KDF9.Q_part)
   return KDF9.Q_part is
   begin
      return KDF9.Q_part'Min(T.data_max, address);
   end bounded_data_address;

end disassembly.symbols;
