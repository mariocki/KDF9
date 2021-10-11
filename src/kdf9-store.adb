-- KDF9 core store operations.
--
-- This file is part of ee9 (8.1a), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Unchecked_Conversion;
--
with formatting;
with KDF9.CPU;

use formatting;
use  KDF9.CPU;

package body KDF9.store is

   -- diagnose_invalid_address avoids secondary stack usage in the address validation procedures.
   procedure diagnose_invalid_address (message : in String; address : in KDF9.word)
      with Inline => False;

   procedure diagnose_invalid_address (message : in String; address : in KDF9.word) is
   begin
      trap_illegal_instruction(message & " =" & address'Image);
   end diagnose_invalid_address;

   -- Check that EA, EA+BA are valid; LIV if invalid.
   procedure validate_virtual_and_real_addresses (EA : in KDF9.Q_part)
      with Inline => True;

   procedure validate_virtual_and_real_addresses (EA : in KDF9.Q_part) is
      PA : constant KDF9.word := (KDF9.word(EA) + KDF9.word(BA)) and Q_part_mask;
   begin
      if EA > NOL and then the_CPU_state = program_state then
         diagnose_invalid_address("NOL < virtual address", KDF9.word(EA));
      end if;
      if PA > max_address and then the_CPU_state = program_state then
         diagnose_invalid_address("32K-1 < physical address", PA);
      end if;
   end validate_virtual_and_real_addresses;

   procedure if_user_mode_then_LOV (address_1 : KDF9.Q_part;
                                    address_2 : KDF9.Q_part := 0;
                                    solo      : Boolean     := True) is
   begin
      LOV_if_user_mode(
                       if solo
                       then "at #" & oct_of(address_1) & " (E" & dec_of(address_1) & ")"
                       else "in #" & oct_of(address_1) & "..#" & oct_of(address_2)
                      );
   end if_user_mode_then_LOV;

   function group (PA : KDF9.Q_part)
   return KDF9.Q_part
   is (PA / group_size);

   procedure check_address_and_lockout (EA : in KDF9.Q_part) is
      PA : constant KDF9.Q_part := EA + BA;
   begin
      validate_virtual_and_real_addresses(EA);
      if locked_out(group(PA)) then
         the_locked_out_address := PA;
         if the_CPU_state /= Director_state then
            if_user_mode_then_LOV(PA);
         end if;
      end if;
   end check_address_and_lockout;

   procedure validate_address_range (EA1, EA2 : in KDF9.Q_part) is
   begin
      if EA1 > EA2 then
         diagnose_invalid_address("initial address > final address", KDF9.word(EA2));
      end if;
      validate_virtual_and_real_addresses(EA1);
      validate_virtual_and_real_addresses(EA2);
   end validate_address_range;

   procedure check_addresses_and_lockouts (EA1, EA2 : in KDF9.Q_part) is
       PA1 : constant KDF9.Q_part := EA1 + BA;
       PA2 : constant KDF9.Q_part := EA2 + BA;
   begin
      validate_address_range (EA1, EA2);
      if there_are_locks_in_physical_addresses(KDF9.Q_register'(C => 0, I => PA1, M => PA2)) then
         if the_CPU_state /= Director_state then
            if_user_mode_then_LOV(PA1, PA2, solo => False);
         end if;
      end if;
   end check_addresses_and_lockouts;

   -- Check that A1+A2 is valid; trap if it is invalid.
   function valid_word_address (A1, A2 : in KDF9.Q_part)
   return KDF9.address is
      V : constant KDF9.word := (KDF9.word(A1) + KDF9.word(A2)) and Q_part_mask;
   begin
      if V > max_address then
         diagnose_invalid_address("32K-1 < virtual address", V);
      end if;
      return KDF9.address(V);
   end valid_word_address;

   function signed is new Ada.Unchecked_Conversion (KDF9.Q_part, CPU.signed_Q_part);
   function design is new Ada.Unchecked_Conversion (CPU.signed_Q_part, KDF9.Q_part);

   -- Check that A1+A2/2 is valid; trap if it is invalid.  A2 must be treated as a signed number.
   function valid_halfword_address (A1, A2 : in KDF9.Q_part)
   return KDF9.address is
      V : constant KDF9.word := (KDF9.word(A1) + KDF9.word(design(signed(A2)/2))) and Q_part_mask;
   begin
      if V > max_address then
         diagnose_invalid_address("32K-1 < virtual address", V);
      end if;
      return KDF9.address(V);
   end valid_halfword_address;

   function fetch_symbol (EA : KDF9.address; index : KDF9_char_sets.symbol_index)
   return KDF9_char_sets.symbol
   is (KDF9_char_sets.symbol(shift_word_right(core(EA+BA), 42 - 6*Natural(index)) and 8#77#));

   procedure store_symbol (value : in KDF9_char_sets.symbol;
                           EA    : in KDF9.address;
                           index : in KDF9_char_sets.symbol_index) is
      place  : constant Natural   := 42 - 6*Natural(index);
      mask   : constant KDF9.word := not shift_word_left(8#77#, place);
      symbol : constant KDF9.word := shift_word_left(KDF9.word(value), place);
   begin
      core(EA+BA) := (core(EA+BA) and mask) or symbol;
   end store_symbol;

   function fetch_octet (EA : KDF9.address; index : KDF9_char_sets.octet_index)
   return KDF9_char_sets.octet is
      place : constant Natural := 40 - 8*Natural(index);
   begin
      return KDF9_char_sets.octet(shift_word_right(core(EA+BA), place) and 8#377#);
   end fetch_octet;

   procedure store_octet  (value : in KDF9_char_sets.octet;
                           EA    : in KDF9.address;
                           index : in KDF9_char_sets.octet_index) is
      place : constant Natural   := 40 - 8*Natural(index);
      octet : constant KDF9.word := shift_word_left(KDF9.word(value), place);
      mask  : constant KDF9.word := not shift_word_left(8#377#, place);
   begin
      core(EA+BA) := (core(EA+BA) and mask) or octet;
   end store_octet;

   function fetch_syllable (EA : KDF9.syllable_address)
   return KDF9.syllable is
      address : constant KDF9.address := Q_part(EA.code_address) + BA;
      place   : constant Natural      := 40 - 8*Natural(EA.syllable_index);
   begin
      return KDF9.syllable(shift_word_right(core(address), place) and 8#377#);
   end fetch_syllable;

   procedure store_syllable (value : in KDF9.syllable;
                             EA    : in KDF9.address;
                             index : in KDF9.syllable_index) is
      place    : constant Natural   := 40 - 8*Natural(index);
      syllable : constant KDF9.word := shift_word_left(KDF9.word(value), place);
      mask     : constant KDF9.word := not shift_word_left(8#377#, place);
   begin
      core(EA+BA) := (core(EA+BA) and mask) or syllable;
   end store_syllable;

   function fetch_halfword (EA : KDF9.address; index : KDF9.halfword_number)
   return KDF9.word
   is (shift_word_left(shift_word_right(core(EA+BA), 24 - 24*Natural(index)), 24));

   procedure store_halfword (value : in KDF9.word;
                             EA    : in KDF9.address;
                             index : in KDF9.halfword_number) is
      place   : constant Natural   := 24 - 24*Natural(index);
      half    : constant KDF9.word := shift_word_left(shift_word_right(value, 24), place);
      mask    : constant KDF9.word := not shift_word_left(halfword_mask, place);
   begin
      core(EA+BA) := (core(EA+BA) and mask) or half;
   end store_halfword;

   function fetch_word (EA : KDF9.address)
   return KDF9.word
   is (core(EA+BA));

   procedure store_word (value : in KDF9.word; EA : in KDF9.address) is
   begin
      core(EA+BA) := value;
   end store_word;

   function there_are_locks_in_relative_addresses (Q : KDF9.Q_register)
   return Boolean is
   begin
      validate_address_range (Q.I, Q.M);
      return there_are_locks_in_physical_addresses((0, Q.I+BA, Q.M+BA));
   end there_are_locks_in_relative_addresses;

   function there_are_locks_in_physical_addresses (Q : KDF9.Q_register)
   return Boolean is
   begin
      for g in group(Q.I) .. group(Q.M) loop
         if locked_out(g) then
            the_locked_out_address := g * group_size;
            return True;
         end if;
      end loop;
      return False;
   end there_are_locks_in_physical_addresses;

   function is_unlocked (G : KDF9.store.group_address)
   return Boolean is
   begin
      return not locked_out(KDF9.Q_part(G));
   end is_unlocked;

   procedure lock_out_relative_addresses (Q : in KDF9.Q_register) is
   begin
      validate_address_range (Q.I, Q.M);
      lock_out_absolute_addresses((0, Q.I+BA, Q.M+BA));
   end lock_out_relative_addresses;

   procedure lock_out_absolute_addresses (Q : in KDF9.Q_register) is
   begin
      for g in group(Q.I) .. group(Q.M) loop
         locked_out(g) := True;
      end loop;
   end lock_out_absolute_addresses;

   procedure unlock_absolute_addresses (Q : in KDF9.Q_register) is
   begin
      for g in group(Q.I) .. group(Q.M) loop
         locked_out(g) := False;
      end loop;
   end unlock_absolute_addresses;

end KDF9.store;
