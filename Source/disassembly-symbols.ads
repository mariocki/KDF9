 -- Map object code addresses to Usercode data_label addresses.
--
-- This file is part of ee9 (8.1x), the GNU Ada emulator of the English Electric KDF9.
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

package disassembly.symbols is

   procedure clear_all_symbol_definitions;

   procedure set_whole_program_data (size, E0_jump : in KDF9.word);

   subtype Y_store_id is Character range 'A' ..'Z';

   procedure set_main_program_V_size (V_max : in Natural);
   procedure set_W0 (address : in KDF9.Q_part);
   procedure set_Y0 (address : in KDF9.Q_part);
   procedure set_Y_size (size : in KDF9.Q_part);
   procedure set_Yy0 (y : in Y_store_id; address : in KDF9.Q_part);
   procedure set_Z0 (address : in KDF9.Q_part);

   procedure site_P0 (P_address : in KDF9.Q_part);
   procedure site_Pp (P_number : in Natural; P_address : in KDF9.Q_part);
   procedure site_Pp (P_number : in Natural; P_address : in KDF9.Q_part; V_max : in Natural);

   function V_store_count (address : KDF9.syllable_address)
   return Natural;

   function code_operand (address : KDF9.syllable_address; in_octal : Boolean)
   return String;

   function data_operand (address : KDF9.Q_part; in_octal : Boolean)
   return String;

   function SET_operand (value : KDF9.Q_part; in_octal : Boolean)
   return String;

   function routine_name (address : KDF9.syllable_address; in_octal : Boolean)
   return String;

   function data_label (address : KDF9.Q_part; in_octal : Boolean)
   return String;

   function a_routine_starts_at (address : KDF9.syllable_address)
   return Boolean;

   function bounded_code_address (address : KDF9.syllable_address)
   return KDF9.syllable_address;

   function bounded_code_address (address : KDF9.Q_part)
   return KDF9.syllable_address;

   function bounded_data_address (address : KDF9.Q_part)
   return KDF9.Q_part;

   -- There physically cannot by more than 8191 routines.
   subtype P_number_range is Natural range 0 .. 8191;

   last_P_number : P_number_range := 0;

   -- If V_address = 0 the routine has no V stores and V_max is unused.
   type P_definition is
   record
      P_number  : Natural := 0;
      V_max     : Natural := 0;
      P_address : KDF9.Q_part := 0;
      V_address : KDF9.Q_part := 0;
   end record;

   type P_definition_list is array (P_number_range range <>) of P_definition;

   P_store_base : P_definition_list (P_number_range);

   type address_list is array (Y_store_id) of KDF9.Q_part;

   type non_V_store_table is
      record
         W_base   : KDF9.Q_part := 32767;
         Yy_base  : address_list := (others => 32767);
         Y_base   : KDF9.Q_part := 32767;
         Y_size   : KDF9.Q_part := 0;
         Y_last   : KDF9.Q_part := 32767;
         Z_min    : KDF9.Q_part := 32767;
         Z_base   : KDF9.Q_part := 32767;
         data_max : KDF9.Q_part := 32767;
         code_max : KDF9.code_address := 8191;
      end record;

   the_WYZ_table : non_V_store_table;

end disassembly.symbols;
