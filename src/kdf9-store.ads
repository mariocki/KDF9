-- KDF9 core store operations.
--
-- This file is part of ee9 (9.0p), the GNU Ada emulator of the English Electric KDF9.
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

with KDF9_char_sets;

use  KDF9_char_sets;

package KDF9.store is

   --
   -- Relative addresses may be either virtual or physical.
   -- Virtual addresses are generated by problem programs and must be relativized by adding BA.
   -- These addresses must also be validated to ensure they do not breach store limits.
   -- Physical "relative" addresses are generated by Director, when BA is guaranteed to be 0,
   --   so it has no effect, thus allowing the relative address routines to be used.
   --
   -- Absolute addresses are generated by I/O Control, which may be doing a transfer for either
   --   a Director or a problem program, and must therefore ensure that BA is not added.
   --

   --
   -- Parameters named EA are Effective "relative" Addresses.
   --

   function fetch_symbol (EA : KDF9.address; index : KDF9_char_sets.symbol_index)
   return KDF9_char_sets.symbol;

   function fetch_octet (EA : KDF9.address; index : KDF9_char_sets.octet_index)
   return KDF9_char_sets.octet
      with Inline;

   function fetch_syllable (EA : KDF9.syllable_address)
   return KDF9.syllable
      with Inline;

   function fetch_halfword (EA : KDF9.address; index : KDF9.halfword_number)
   return KDF9.word
      with Inline;

   function fetch_word (EA : KDF9.address)
   return KDF9.word
      with Inline;

   procedure store_symbol (value : in KDF9_char_sets.symbol;
                           EA    : in KDF9.address;
                           index : in KDF9_char_sets.symbol_index)
      with Inline;

   procedure store_octet  (value : in KDF9_char_sets.octet;
                           EA    : in KDF9.address;
                           index : in KDF9_char_sets.octet_index)
      with Inline;

   procedure store_syllable (value : in KDF9.syllable;
                             EA    : in KDF9.address;
                             index : in KDF9.syllable_index)
      with Inline;

   procedure store_halfword (value : in KDF9.word;
                             EA    : in KDF9.address;
                             index : in KDF9.halfword_number)
      with Inline;

   procedure store_word (value : in KDF9.word; EA : in KDF9.address)
      with Inline;

   -- Compute A1+A2 as a valid word address, mod 32K.
   function virtual_word_address (A1, A2 : in KDF9.Q_part)
   return KDF9.address
      with Inline;

   -- Check that A1+A2/2 as a valid word address, mod 32K.  A2 is treated as a signed number.
   function virtual_halfword_address (A1, A2 : in KDF9.Q_part)
   return KDF9.address
      with Inline;

   -- If a store access is locked out, its physical address is left here.
   the_locked_out_address : KDF9.Q_part;

   procedure if_user_mode_then_LOV (address_1 : KDF9.Q_part;
                                    address_2 : KDF9.Q_part := 0;
                                    solo      : Boolean     := True)
      with Inline => False;

   -- Check EA and lockout for EA.
   procedure check_address_and_lockout (EA : in KDF9.Q_part)
      with Inline;

   -- Check that EA1, EA2, EA1+BA, EA2+BA are valid, and EA1 <= EA2.
   --    LIV in any invalid case.
   procedure validate_address_range (EA1, EA2 : in KDF9.Q_part);

   -- Check EA1, EA2, and lockouts for EA1+BA .. EA2+BA.
   procedure check_addresses_and_lockouts (EA1, EA2 : in KDF9.Q_part);

   function there_are_locks_in_relative_addresses (Q : KDF9.Q_register)
   return Boolean;

   function there_are_locks_in_physical_addresses (Q : KDF9.Q_register)
   return Boolean;

   procedure lock_out_relative_addresses (Q : in KDF9.Q_register);

   procedure lock_out_absolute_addresses (Q : in KDF9.Q_register);

   procedure unlock_absolute_addresses (Q : in KDF9.Q_register);

   -- The group size of 32 words is 1 physical core allocation unit and physical lockout unit.
   group_size : constant := 32;

   type group_address is mod 1024;

    -- is_unlocked yields True if the designated group is NOT locked out.
   function is_unlocked (G : KDF9.store.group_address)
   return Boolean;

   function group (PA : KDF9.Q_part)
   return KDF9.Q_part
      with Inline;

private

   type word_array is array (KDF9.Q_part range <>) of KDF9.word
      with Component_Size => 64, Convention => C;

   -- The core store of KDF9.  Must be zeroized before loading any software.
   core : word_array (KDF9.Q_part range 0 .. KDF9.address'Last) := (others => 0);

   -- The lockout store has one bit for every group_size words.
   last_lockout : constant := KDF9.address'Last / group_size;
   locked_out   : array (KDF9.Q_part range 0 .. last_lockout) of Boolean := (others => False);

end KDF9.store;
