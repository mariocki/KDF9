-- The K5 operation data formats.
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

with Ada.Unchecked_Conversion;
--
with KDF9.CPU;

package body KDF9.PHU_store is

   function short_PHU (p : KDF9.priority)
   return KDF9.word is

      use type KDF9.store.group_address;

      type PHU_as_6_bits is mod 2**6
         with Size => 6;

      function as_6_bits is new Ada.Unchecked_Conversion(Source => PHU_store.PHU_subset,
                                                         Target => short_PHU.PHU_as_6_bits);

      the_reason    : PHU_store.blockage_kind;
      the_parameter : KDF9.buffer_number;

   begin
      if not PHU(p).is_held_up then
         return 0;  -- All fields are non-significant.
      end if;

      -- PHU(p).is_held_up, so other fields are valid.
      the_reason := PHU(p).blockage.reason;
      if the_reason = buffer_busy then
         the_parameter := PHU(p).blockage.buffer_nr;
      else
         -- This is next to useless, but is what the K5 order actually did.
         the_parameter := KDF9.buffer_number(PHU(p).blockage.group_nr mod 2**4);
      end if;

      return KDF9.word(as_6_bits((the_parameter, the_reason, True)));
   end short_PHU;

   function K5_operand
   return KDF9.word
   is (
       KDF9.CPU.shift_word_left(short_PHU(0), 48-06) or
       KDF9.CPU.shift_word_left(short_PHU(1), 48-12) or
       KDF9.CPU.shift_word_left(short_PHU(2), 48-18) or
       KDF9.CPU.shift_word_left(short_PHU(3), 48-24)
      );

end KDF9.PHU_store;
