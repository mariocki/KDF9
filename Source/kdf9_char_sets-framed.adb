-- This file is an auxiliary of ee9 (6.2e), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2021, W. Findlay; all rights reserved.
--
-- This program is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. This program is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details. You should have
-- received a copy of the GNU General Public License distributed with
-- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
--

-- Convert a 6-bit KDF9 character to an 8-channel paper tape code frame.

function KDF9_char_sets.framed (symbol : KDF9_char_sets.symbol)
return Character is

   parity_bits  : constant Natural := 2#00_010_000#;
   channel_bits : constant Natural := 2#10_000_000#;
   channel_8    : constant Natural := (if symbol = 0 then channel_bits else 0);
   low_4_bits   : constant Natural := Natural(symbol and 2#001_111#);
   top_2_bits   : constant Natural := Natural(symbol and 2#110_000#) * 2;

   datum  : Natural;
   parity : Natural;

begin -- framed
   if symbol = 0 then
      parity := parity_bits;
   else
      datum := Natural(symbol); parity := 0;
      while datum /= 0 loop
         parity := parity + Natural(datum mod 2);
         datum  := datum / 2;
      end loop;
      parity := (if parity mod 2 = 0 then 0 else parity_bits);
   end if;
   return Character'Val(channel_8 + top_2_bits + parity + low_4_bits);
end KDF9_char_sets.framed;
