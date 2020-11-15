-- kdf9-PHU_store.adb
--
-- The K5 operation data formats.
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

with Unchecked_Conversion;

with KDF9.CPU;

package body KDF9.PHU_store is

   pragma Unsuppress(All_Checks);

   function short_PHU (p : KDF9.priority)
   return KDF9.word is

      type PHU_as_6_bits is mod 2**6;
      for  PHU_as_6_bits'Size use 6;

      function as_6_bits is new Unchecked_Conversion(Source => PHU_store.PHU_subset,
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

      return KDF9.word(as_6_bits((True, the_reason, the_parameter)));
   end short_PHU;

   function K5_operand
   return KDF9.word is
   begin
      return
          KDF9.CPU.shift_word_left(short_PHU(0), 47-05) or
          KDF9.CPU.shift_word_left(short_PHU(1), 47-11) or
          KDF9.CPU.shift_word_left(short_PHU(2), 47-17) or
          KDF9.CPU.shift_word_left(short_PHU(3), 47-23);
   end K5_operand;

end KDF9.PHU_store;
