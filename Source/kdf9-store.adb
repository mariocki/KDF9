-- kdf9-store.adb
--
-- KDF9 core store operations.
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

with KDF9.CPU;

use  KDF9.CPU;

package body KDF9.store is

   function group (EA : KDF9.Q_part)
   return KDF9.Q_part is
   begin
      return EA / group_size;
   end group;

   procedure validate_address (EA : in KDF9.address) is
      PA : constant KDF9.Q_part := EA + BA;
   begin
      if EA > NOL then
         trap_invalid_instruction("virtual address > NOL");
      end if;
      if PA > max_address then
         trap_invalid_instruction("physical address > 32K-1");
      end if;
   end validate_address;

   procedure validate_access (EA : in KDF9.address) is
      PA : constant KDF9.Q_part := EA + BA;
   begin
      validate_address(EA);
      if locked_out(group(PA)) then
         the_locked_out_address := PA;
         if the_CPU_state /= Director_state then
            raise LOV_trap;
         end if;
      end if;
   end validate_access;

   procedure validate_address_range (EA1, EA2 : in KDF9.address) is
   begin
      validate_address(EA1);
      validate_address(EA2);
      if EA1 > EA2 then
         trap_invalid_instruction("initial address > final address");
      end if;
   end validate_address_range;

   procedure validate_range_access (EA1, EA2 : in KDF9.address) is
      PA1 : constant KDF9.Q_part := EA1 + BA;
      PA2 : constant KDF9.Q_part := EA2 + BA;
   begin
      validate_address_range (EA1, EA2);
      if test_lockouts(KDF9.Q_register'(0, PA1, PA2)) /= 0 then
         if the_CPU_state /= Director_state then
            raise LOV_trap;
         end if;
      end if;
   end validate_range_access;

   -- Check that A1+A2 is valid; LIV if it is invalid.
   function validated_sum (A1, A2 : in KDF9.Q_part)
   return KDF9.address is
      W : constant KDF9.word := (KDF9.word(A1) + KDF9.word(A2)) and Q_part_mask;
   begin
      if W > max_address then
         trap_invalid_instruction("virtual address > 32K-1");
      end if;
      return KDF9.address(W);
   end validated_sum;

   function fetch_symbol (EA : KDF9.Q_part; sn : KDF9.symbol_number)
   return KDF9.symbol is
      place   : constant Natural     := 42 - 6*Natural(sn);
      address : constant KDF9.Q_part := EA + BA;
   begin
      return KDF9.symbol(shift_word_right(core(address), place) and 8#77#);
   end fetch_symbol;

   procedure store_symbol (value : in KDF9.symbol;
                           EA    : in KDF9.Q_part;
                           sn    : in KDF9.symbol_number) is
      place  : constant Natural   := 42 - 6*Natural(sn);
      mask   : constant KDF9.word := not shift_word_left(8#77#, place);
      symbol : constant KDF9.word := shift_word_left(KDF9.word(value), place);
   begin
      core(EA+BA) := (core(EA+BA) and mask) or symbol;
   end store_symbol;

   function fetch_syllable (EA : KDF9.code_point)
   return KDF9.syllable is
      place   : constant Natural     := 40 - 8*Natural(EA.syllable_number);
      address : constant KDF9.Q_part := KDF9.Q_part(EA.word_number) + BA;
   begin
      return KDF9.syllable(shift_word_right(core(address), place) and 8#377#);
   end fetch_syllable;

   procedure store_syllable (value : in KDF9.syllable; EA : in KDF9.code_point) is
      place    : constant Natural      := 40 - 8*Natural(EA.syllable_number);
      address  : constant KDF9.Q_part  := KDF9.Q_part(EA.word_number) + BA;
      syllable : constant KDF9.word := shift_word_left(KDF9.word(value), place);
      mask     : constant KDF9.word := not shift_word_left(8#377#, place);
   begin
      core(address) := (core(address) and mask) or syllable;
   end store_syllable;

   function fetch_halfword (EA : KDF9.Q_part; hn : KDF9.halfword_number)
   return KDF9.word is
      place   : constant Natural      := 24 - 24*Natural(hn);
      address : constant KDF9.Q_part  := EA + BA;
   begin
      return shift_word_left(shift_word_right(core(address), place), 24);
   end fetch_halfword;

   procedure store_halfword (value : in KDF9.word;
                             EA    : in KDF9.Q_part;
                             hn    : in KDF9.halfword_number) is
      place   : constant Natural      := 24 - 24*Natural(hn);
      address : constant KDF9.Q_part  := EA + BA;
      half    : constant KDF9.word := shift_word_left(shift_word_right(value, 24), place);
      mask    : constant KDF9.word := not shift_word_left(halfword_mask, place);
   begin
      core(address) := (core(address) and mask) or half;
   end store_halfword;

   function fetch_word (EA : KDF9.Q_part)
   return KDF9.word is
   begin
      return core(EA+BA);
   end fetch_word;

   procedure store_word (value : in KDF9.word; EA : in KDF9.Q_part) is
   begin
      core(EA+BA) := value;
   end store_word;

   function test_lockouts (Q : in KDF9.Q_register)
   return KDF9.word is
      a : KDF9.address;
   begin
      validate_address_range (Q.I, Q.M);
      a := Q.I;
      loop
         if locked_out(group(a)) then
            the_locked_out_address := a;
            return 1;
         end if;
      exit when group_size > Q.M - a;
         a := a + group_size;
      end loop;
      return 0;
   end test_lockouts;

   procedure set_lockouts (Q : in KDF9.Q_register) is
   begin
      validate_address_range (Q.I, Q.M);
      for g in group(Q.I) .. group(Q.M) loop
         locked_out(g) := True;
      end loop;
   end set_lockouts;

   procedure clear_lockouts (Q : in KDF9.Q_register) is
   begin
      validate_address_range (Q.I, Q.M);
      for g in group(Q.I) .. group(Q.M) loop
         locked_out(g) := False;
      end loop;
   end clear_lockouts;

   procedure mirror (start_address, end_address : in KDF9.address) is
      lower_address : KDF9.address := start_address;
      upper_address : KDF9.address := end_address;
      lo_word, hi_word : KDF9.word;
   begin
      while lower_address < upper_address loop
         lo_word := fetch_word(lower_address);
         hi_word := fetch_word(upper_address);
         store_word(hi_word, lower_address);
         store_word(lo_word, upper_address);
         lower_address := lower_address + 1;
         upper_address := upper_address - 1;
      end loop;
   end mirror;

end KDF9.store;
