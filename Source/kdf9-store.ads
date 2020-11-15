-- kdf9-store.ads
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

package KDF9.store is

   function group (EA : KDF9.Q_part)
   return KDF9.Q_part;
   pragma Inline(group);

   function fetch_symbol (EA : KDF9.Q_part; sn : KDF9.symbol_number)
   return KDF9.symbol;

   function fetch_syllable (EA : KDF9.code_point)
   return KDF9.syllable;
   pragma Inline(fetch_syllable);

   function fetch_halfword (EA : KDF9.Q_part; hn : KDF9.halfword_number)
   return KDF9.word;

   function fetch_word (EA : KDF9.Q_part)
   return KDF9.word;
   pragma Inline(fetch_word);

   procedure store_symbol (value : in KDF9.symbol;
                           EA    : in KDF9.Q_part;
                           sn    : in KDF9.symbol_number);

   procedure store_syllable (value : in KDF9.syllable; EA : in KDF9.code_point);

   procedure store_halfword (value : in KDF9.word;
                             EA    : in KDF9.Q_part;
                             hn    : in KDF9.halfword_number);

   procedure store_word (value : in KDF9.word; EA : in KDF9.Q_part);
   pragma Inline(store_word);

   -- Check that A1+A2 is valid; LIV if it is invalid.
   function validated_sum (A1, A2 : in KDF9.Q_part)
   return KDF9.address;
   pragma Inline(validated_sum);

   -- Check that EA, EA+BA are valid; LIV if invalid.
   procedure validate_address (EA : in KDF9.address);
   pragma Inline(validate_address);

   -- If a store access is locked out, the offending address is left here,
   --    by test_lockouts or by validate_access.
   the_locked_out_address : KDF9.Q_part;

   -- Check EA and lockout for EA; deal with LOV if lockout set.
   procedure validate_access (EA : in KDF9.address);
   pragma Inline(validate_access);

   -- Check that EA1,, EA2, EA1+BA, EA2+BA are valid, and EA1 <= EA2.
   --    LIV in any invalid case.
   procedure validate_address_range (EA1, EA2 : in KDF9.address);

   -- Check EA1, EA2, and lockouts for EA1 .. EA2.
   procedure validate_range_access (EA1, EA2 : in KDF9.address);

   function test_lockouts (Q : in KDF9.Q_register)
   return KDF9.word; -- Yields True if any of the designated words are locked out.

   procedure clear_lockouts (Q : in KDF9.Q_register);

   procedure set_lockouts (Q : in KDF9.Q_register);

   -- Reverse the contents of the store area delimited by the given addresses.
   procedure mirror (start_address, end_address : in KDF9.address);

   -- The group size of 32 words is 1 core allocation/lockout unit.
   group_size : constant := 32;

   -- The maximum size KDF9 core store has 32Kibiwords.
   max_address   : constant := 2**15 - 1;

private

   type word_array is array (KDF9.Q_part range <>) of KDF9.word;
   for  word_array'Component_Size use 64;
   pragma Convention (C, word_array);

   -- The core store of KDF9.
   core : word_array (KDF9.Q_part range 0 .. max_address) := (others => 0);

   -- The lockout store has one bit for every group_size words.
   last_lockout : constant := (max_address + group_size) / group_size - 1;
   locked_out   : array (KDF9.Q_part range 0 .. last_lockout) of Boolean := (others => False);

end KDF9.store;
