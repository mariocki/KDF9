-- formatting.adb
--
-- Provide basic data-formatting operations.
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

with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;

use  Ada.Characters.Handling;
use  Ada.Strings;
use  Ada.Strings.Fixed;

package body formatting is

   pragma Unsuppress(All_Checks);

   -- Return N as 3 octal digits.
   function oct_of (N : KDF9.syllable)
   return String is
   begin
      return oct_of(KDF9.halfword(N))(6 .. 8);
   end oct_of;

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

   -- Return N as decimal digits, with zero suppression.
   function dec_of (N : KDF9.Q_part)
   return String is
      dec : constant String := KDF9.Q_part'Image(N);
   begin
      return trimmed(dec);
   end dec_of;

   -- Return N as 5 octal digits.
   function oct_of (N : KDF9.code_location)
   return String is
      value : KDF9.code_location := N;
      oct   : String(1 .. 5);
   begin
      for i in reverse oct'Range loop
         oct(i) := digit_map(KDF9.halfword(value mod 8));
         value := value / 8;
      end loop;
      return oct;
   end oct_of;

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
   function oct_of (N : KDF9.code_link)
   return String is
      image : constant String  := '#'
                                &  oct_of(N.word_number)
                                & '/'
                                & digit_map(KDF9.halfword(N.syllable_number));
   begin
      return image;
   end oct_of;

   -- Return N as #wwwww/s, where w and s are octal digits.
   function oct_of (N : KDF9.code_point)
   return String is
   begin
      return oct_of(KDF9.code_link(N));
   end oct_of;

   -- Return N as dddd/d, where d is a decimal digit.
   function dec_of (N : KDF9.code_point)
   return String is
      image : constant String  := trimmed(KDF9.code_location'Image(N.word_number))
                                & '/'
                                & digit_map(KDF9.halfword(N.syllable_number));
   begin
      return image;
   end dec_of;

   -- Return N as #wwwww/s, where w and s are octal digits;
   --    or as dddd/s, where d is a decimal digit, according to octal_option.
   function oct_or_dec_of (N : KDF9.code_point; octal_option : Boolean)
   return String is
   begin
      return optional(octal_option, oct_of(N), dec_of(N));
   end oct_or_dec_of;

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

   -- Return "L', R'", or "L'" if R' is empty; "'" indicates removal of trailing blanks.
   function "-" (L, R : String) return String is
      trim_R : constant String := trim(R, right);
   begin
      if trim_R /= "" then
         return trim(L, right) & ", " & trim_R;
      else
         return trim(L, right);
      end if;
   end "-";

   -- Return S with all leading an trailing blanks removed.
   function trimmed (S : String) return String is
   begin
      return trim(S, Ada.Strings.both);
   end trimmed;

   -- Return trimmed(S), right-justified in a field of width at least W.
   function justified (S : String; W : Positive := 3)
   return String is
     image   : constant String   := trim(S, Ada.Strings.both);
     columns : constant Positive := Positive'Max(W, image'Length);
   begin
     return tail(image, columns, ' ');
   end justified;

   -- Return (if B then S else ""): for pre-Ada 2012 compliers.
   function optional (B : Boolean; T : String)
   return String is
   begin
     if B then return T; else return ""; end if;
   end optional;

   -- Return (if B then S else T): for pre-Ada 2012 compliers.
   function optional (B : Boolean; S, T : String)
   return String is
   begin
     if B then return S; else return T; end if;
   end optional;

   -- Return C converted to a 1-character string.
   function "+" (C : Character) return unit_string is
      result : unit_string;
   begin
      result(1) := C;
      return result;
   end "+";

   -- Return C with all Latin-1 lower-case letters converted to upper-case.
   function to_upper (C : Character)
   return Character is
   begin
      return Ada.Characters.Handling.to_upper(C);
   end to_upper;

   -- Return S with all Latin-1 lower-case letters converted to upper-case.
   function to_upper (S : String)
   return String is
   begin
      return Ada.Characters.Handling.to_upper(S);
   end to_upper;

   -- Return C with all Latin-1 upper-case letters converted to lower-case.
   function to_lower (C : Character)
   return Character is
   begin
      return Ada.Characters.Handling.to_lower(C);
   end to_lower;

   -- Return S with all Latin-1 upper-case letters converted to lower-case.
   function to_lower (S : String)
   return String is
   begin
      return Ada.Characters.Handling.to_lower(S);
   end to_lower;

   -- Return the 8-character Latin-1 string representing the 8 Case Normal characters in N.
   function to_string (N : KDF9.word)
   return word_as_byte_string is
      word   : KDF9.word := N;
      result : word_as_byte_string;
   begin
      for i in reverse 1 .. 8 loop
         result(i) := KDF9.TP_CN(KDF9.symbol(word and 8#77#));
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

end formatting;
