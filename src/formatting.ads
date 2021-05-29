-- Provide basic data-formatting operations for KDF9 data types.
--
-- This file is part of ee9 (7.0a), the GNU Ada emulator of the English Electric KDF9.
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

with KDF9;

use  KDF9;

package formatting is

   subtype unit_string         is String(1 .. 1);
   subtype word_as_byte_string is String(1 .. 8);
   subtype pair_as_byte_string is String(1 .. 16);

   -- Return N as 3 octal digits.
   function oct_of (N : KDF9.syllable)
   return String;

   -- Return N as 6 octal digits.
   function oct_of (N : KDF9.field_of_16_bits)
   return String;

   subtype octal_width is Positive range 1 .. 6;

   -- Return N as octal digits, with (partial) zero suppression.
   -- The first (6-min_digits) are elided if '0'; all remaining digits are returned.
   -- Up to 6 digits can be returned if the result is longer than min_digits.
   -- If N is 0, the String (1..min_digits => '0') is returned.
   function oct_of (N : KDF9.Q_part; min_digits : octal_width := 6)
   return String;

   -- Return N as 1 .. 5 decimal digits, with zero suppression and sign when neagtive.
   function signed_dec_of (N : KDF9.Q_part)
   return String;

   -- Return N as 1 .. 6 decimal digits, with zero suppression.
   function dec_of (N : KDF9.Q_part)
   return String;

   -- Return N as up to 5 octal digits.
   function oct_of (N : KDF9.code_address)
   return String;

   -- Return N as decimal digits, with zero suppression.
   function dec_of (N : KDF9.code_address)
   return String ;

   -- Return N as 8 octal digits.
   function oct_of (N : KDF9.halfword)
   return String;

   -- Return N as #wwwww/s, where w and s are octal digits.
   function oct_of (N : KDF9.SJNS_link)
   return String;

   -- Return N as #wwwww/s, where w and s are octal digits.
   function oct_of (N : KDF9.syllable_address)
   return String;

   -- Return N as dddd/d, where d is a decimal digit.
   function dec_of (N : KDF9.syllable_address)
   return String;

   -- Return N as #wwwww/s, where w and s are octal digits;
   --    or as dddd/s, where d is a decimal digit, according to octal_option.
   function oct_or_dec_of (N : KDF9.syllable_address; octal_option : Boolean)
   return String;

   -- Return N as 16 octal digits.
   function oct_of (N : KDF9.word)
   return String;

   -- Return "L', R'", or "L'"if R' is empty; ' indicates removal of trailing blanks.
   function "-" (L, R : String)
   return String;

   -- Return S with all leading and trailing blanks removed.
   function trimmed (S : String)
   return String;

   -- Return trimmed(S), right-justified in a field of width at least W.
   function just_right (S : String; W : Positive := 3)
   return String;

   -- Return trimmed(S), left-justified in a field of width at least W.
   function just_left (S : String; W : Positive := 3)
   return String;

   -- Return the (pluralizing) suffix if count /= 1.
   function plurality (count : KDF9.word; for_1 : String := ""; for_more : String := "s")
   return String;

   -- Return C converted to a 1-character string.
   function "+" (C : Character)
   return unit_string;

   -- Return C with all Latin-1 lower-case letters converted to upper-case.
   function to_upper (C : Character)
   return Character;

   -- Return S with all Latin-1 lower-case letters converted to upper-case.
   function to_upper (S : String)
   return String;

   -- Return C with all Latin-1 upper-case letters converted to lower-case.
   function to_lower (C : Character)
   return Character;

   -- Return S with all Latin-1 upper-case letters converted to lower-case.
   function to_lower (S : String)
   return String;

   -- Return the 8-character Latin-1 string representing the 8 Case Normal characters in N.
   function to_string (N : KDF9.word)
   return word_as_byte_string;

   -- Return the result of applying to_string to each word of a double-word.
   function to_string (P : KDF9.pair)
   return pair_as_byte_string;

   -- Like to_string, but with glyphs for format effectors.
   function glyphs_for (N : KDF9.word)
   return word_as_byte_string;

   function glyphs_for (S : String)
   return String;

end formatting;
