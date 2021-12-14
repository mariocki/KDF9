-- Provide basic data-data_imaging operations for KDF9 data types.
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

with KDF9_char_sets;
with KDF9.CPU;
with string_editing;

use  KDF9_char_sets;
use  KDF9.CPU;
use  string_editing;

package body data_imaging is

   digit_map : constant array (KDF9.halfword range 0 .. 15) of Character := "0123456789ABCDEF";

   -- Return N as 3 octal digits.
   function oct_of (N : KDF9.syllable)
   return String
   is (oct_of(KDF9.halfword(N))(6 .. 8));

   -- Return N as 6 octal digits.
   function oct_of (N : KDF9.field_of_16_bits)
   return String is
      value : KDF9.field_of_16_bits := N;
      oct   : String(1 .. 6);
   begin
      for i in reverse oct'Range loop
         oct(i) := digit_map(KDF9.halfword(value mod 8));
         value := value / 8;
      end loop;
      return oct;
   end oct_of;

   -- Return N as 1 .. min_digits octal digits, with (partial) zero suppression.
   function oct_of (N : KDF9.Q_part; min_digits : octal_width := 6)
   return String is
      oct : constant String(octal_width) := oct_of(KDF9.field_of_16_bits(N));
   begin
     if N = 0 then return (1..min_digits => '0'); end if;
     for i in 1 .. 6-min_digits loop
        if oct(i) /= '0' then
           return oct(i .. 6);
        end if;
      end loop;
      return oct(7-min_digits .. 6);
   end oct_of;

   -- Return N as 1 .. 5 decimal digits, with zero suppression and sign when neagtive.
   function signed_dec_of (N : KDF9.Q_part)
   return String is
      dec : constant String := resign(N)'Image;
   begin
      return trimmed(dec);
   end signed_dec_of;

   -- Return N as #wwwwww, where w is an octal digits;
   --    or as ddddd, where d is a decimal digit, according to octal_option.
   function oct_or_dec_of (N : KDF9.Q_part; octal_option : Boolean)
   return String is
   begin
      if octal_option then
         return "#" & oct_of(N, min_digits => 1);
      else
         return trimmed(N'Image);
      end if;
   end oct_or_dec_of;

   -- Return N as #wwwwww{ddddd}  OR as ddddd{#wwwwww}
   --   where w is an octal digit and d is a decimal digit,
   --   depending on the value of octal_first,
   --   and { } represents the values of the insert and closer parameters.
   function oct_and_dec_of (
                            N           : KDF9.Q_part;
                            octal_first : Boolean;
                            insert      : String := "; (";
                            closer      : String := ")"
                           )
   return String is
      octal   : constant String := "#" & oct_of(N, min_digits => 1);
      decimal : constant String := trimmed(N'Image);
   begin
      if octal_first then
         return octal   & insert & decimal & closer;
      else
         return decimal & insert & octal   & closer;
      end if;
   end oct_and_dec_of;

   -- Return N as decimal digits, with zero suppression.
   function dec_of (N : KDF9.Q_part)
   return String is
      dec : constant String := N'Image;
   begin
      return trimmed(dec);
   end dec_of;

   -- Return N as up to 5 octal digits.
   function oct_of (N : KDF9.code_address)
   return String is
      value : KDF9.code_address := N;
      j     : Positive := 5;
      oct   : String(1 .. 5);
   begin
      for i in reverse oct'Range loop
         oct(i) := digit_map(KDF9.halfword(value mod 8));
         value := value / 8;
      end loop;
      for i in oct'Range loop
         if oct(i) /= '0' then j := i; exit; end if;
      end loop;
      return oct(j..5);
   end oct_of;

   -- Return N as decimal digits, with zero suppression.
   function dec_of (N : KDF9.code_address)
   return String
   is (trimmed(N'Image));

   -- Return N as 8 octal digits.
   function oct_of (N : KDF9.halfword)
   return String is
      value : KDF9.halfword := N;
      oct   : String(1 .. 8);
   begin
      for i in reverse oct'Range loop
         oct(i) := digit_map(value mod 8);
         value := value / 8;
      end loop;
      return oct;
   end oct_of;

   -- Return N as #wwwww/s, where w and s are octal digits.
   function oct_of (N : KDF9.SJNS_link)
   return String
   is (
       "#"
      &  oct_of(N.code_address)
      & '/'
      & digit_map(KDF9.halfword(N.syllable_index))
      );

   -- Return N as #wwwww/s, where w and s are octal digits.
   function oct_of (N : KDF9.syllable_address)
   return String
   is (oct_of(KDF9.SJNS_link(N)));

   -- Return N as dddd/d, where d is a decimal digit.
   function dec_of (N : KDF9.syllable_address)
   return String
   is (
       trimmed(N.code_address'Image)
            & '/'
            & digit_map(KDF9.halfword(N.syllable_index))
      );

   -- Return N as #wwwww/s, where w and s are octal digits;
   --    or as dddd/s, where d is a decimal digit, according to octal_option.
   function oct_or_dec_of (N : KDF9.syllable_address; octal_option : Boolean)
   return String
   is (if octal_option then oct_of(N) else dec_of(N));

   -- Return N as ##wwwww/s{ddddd/s}  OR as ddddd/s{#wwwwww/s}
   --   where w, s is an octal digit and d is a decimal digit,
   --   depending on the value of octal_first,
   --   and { } represents the values of the insert and closer parameters.
   function oct_and_dec_of (
                            N           : KDF9.syllable_address;
                            octal_first : Boolean;
                            insert      : String := "; (";
                            closer      : String := ")"
                           )
   return String is
      octal   : constant String := oct_of(N);
      decimal : constant String := dec_of(N);
   begin
      if octal_first then
         return octal   & insert & decimal & closer;
      else
         return decimal & insert & octal   & closer;
      end if;
   end oct_and_dec_of;

   -- Return N as 16 octal digits
   function oct_of (N : KDF9.word)
   return String is
      value : KDF9.word := N;
      oct   : String(1 .. 16);
   begin
      for i in reverse oct'Range loop
         oct(i) := digit_map(KDF9.halfword(value mod 8));
         value := value / 8;
      end loop;
      return oct;
   end oct_of;

   function plurality (count : KDF9.word; for_1 : String := ""; for_more : String := "s")
   return String
   is (if count /= 1 then for_more else for_1);

   -- Return the 8-character Latin-1 string representing the 8 Case Normal characters in N.
   function to_string (N : KDF9.word)
   return word_as_byte_string is
      word   : KDF9.word := N;
      result : word_as_byte_string;
   begin
      for i in reverse 1 .. 8 loop
         result(i) := KDF9_char_sets.TP_CN(KDF9_char_sets.symbol(word and 8#77#));
         word := word / 64;
      end loop;
      return result;
   end to_string;

   -- Return the result of applying to_string to each word of a double-word.
   function to_string (P : KDF9.pair)
   return pair_as_byte_string is
      result : pair_as_byte_string;
   begin
      result(1 ..  8) := to_string(P.msw);
      result(9 .. 16) := to_string(P.lsw);
      return result;
   end to_string;

   -- Like to_string, but with glyphs for format effectors.
   function glyphs_for (N : KDF9.word)
   return word_as_byte_string is
      word   : KDF9.word := N;
      glyphs : word_as_byte_string;
   begin
      for i in reverse 1..8 loop
         glyphs(i) := glyph_for(to_CP(KDF9_char_sets.symbol(word and 8#77#)));
         word := word / 64;
      end loop;
      return glyphs;
   end glyphs_for;

   function glyphs_for (S : String)
   return String is
      T : String (S'First..S'Last);
   begin
      for i in T'Range loop
         T(i) := glyph_for(S(i));
      end loop;
      return T;
   end glyphs_for;

end data_imaging;
