-- formatting.adb
--
-- Provide basic data-formatting operations for KDF9 data types.
--
-- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2020, W. Findlay; all rights reserved.
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
--
with KDF9_char_sets;
with KDF9.CPU;

use  Ada.Characters.Handling;
use  Ada.Strings;
use  Ada.Strings.Fixed;
--
use  KDF9_char_sets;
use  KDF9.CPU;

package body formatting is

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

   -- Return N as decimal digits, with zero suppression.
   function dec_of (N : KDF9.Q_part)
   return String is
      dec : constant String := N'Image;
   begin
      return trimmed(dec);
   end dec_of;

   -- Return N as up to 5 octal digits.
   function oct_of (N : KDF9.order_word_number)
   return String is
      value : KDF9.order_word_number := N;
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
   function dec_of (N : KDF9.order_word_number)
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
   function oct_of (N : KDF9.sjns_link)
   return String
   is (
       "#"
      &  oct_of(N.order_word_number)
      & '/'
      & digit_map(KDF9.halfword(N.syllable_index))
      );

   -- Return N as #wwwww/s, where w and s are octal digits.
   function oct_of (N : KDF9.syllable_address)
   return String
   is (oct_of(KDF9.sjns_link(N)));

   -- Return N as dddd/d, where d is a decimal digit.
   function dec_of (N : KDF9.syllable_address)
   return String
   is (
       trimmed(N.order_word_number'Image)
            & '/'
            & digit_map(KDF9.halfword(N.syllable_index))
      );

   -- Return N as #wwwww/s, where w and s are octal digits;
   --    or as dddd/s, where d is a decimal digit, according to octal_option.
   function oct_or_dec_of (N : KDF9.syllable_address; octal_option : Boolean)
   return String
   is (if octal_option then oct_of(N) else dec_of(N));

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

   function as_DR_command (Q_operand : KDF9.Q_register)
   return String is
      sector : constant KDF9.Q_part := Q_operand.C/64;
      drive  : constant KDF9.Q_part := Q_operand.C/16 mod 4;
   begin
      -- The drum geometry and I/O command bits are as defined in the DR package.
      return "SECT"
           & (if sector < 10 then "00" elsif sector < 100 then "0" else "")
           & dec_of(sector)
           & "D"
           & dec_of(drive);
   end as_DR_command;

   function as_FD_command (Q_operand : KDF9.Q_register; for_seek, for_FH : Boolean := False)
   return String is
      parameter : constant KDF9.Q_part := Q_operand.C/16;
      seek_bits : constant := 6;
      cylinder  : constant KDF9.Q_part := parameter mod 2**seek_bits;
      disk_bits : constant := 4;
      platter   : constant KDF9.Q_part := parameter  /  2**seek_bits mod 2**disk_bits;
      drive     : constant KDF9.Q_part := parameter  /  2**seek_bits  /  2**disk_bits;
   begin
      -- The disc geometry and I/O command bits are as defined in the FD package.
      if for_seek then
         return "D" & dec_of(drive)
              & "P" & dec_of(if for_FH then KDF9.Q_part'(16) else platter)
              & "C" & dec_of(cylinder);
      else -- for data transfer, parameter is sector #, with maximum 96 sectors per track.
         return "SECT" & (if parameter < 10 then "0" else "") & dec_of(parameter);
      end if;
   end as_FD_command;

   -- Return "L', R'", or "L'" if R' is empty; "'" indicates removal of trailing blanks.
   function "-" (L, R : String)
   return String is
      trim_R : constant String := trim(R, right);
   begin
      if trim_R /= "" then
         return trim(L, right) & ", " & trim_R;
      else
         return trim(L, right);
      end if;
   end "-";

   -- Return S with all leading an trailing blanks removed.
   function trimmed (S : String)
   return String
   is (Trim(S, Ada.Strings.Both));

   -- Return trimmed(S), right-just_right in a field of width at least W.
   function just_right (S : String; W : Positive := 3)
   return String is
     image   : constant String   := Trim(S, Ada.Strings.Both);
     columns : constant Positive := Positive'Max(W, image'Length);
   begin
     return Ada.Strings.Fixed.Tail(image, columns, ' ');
   end just_right;

   -- Return trimmed(S), left-justified in a field of width at least W.
   function just_left (S : String; W : Positive := 3)
   return String is
     image   : constant String   := Trim(S, Ada.Strings.Both);
     columns : constant Positive := Positive'Max(W, image'Length);
   begin
     return Ada.Strings.Fixed.Head(image, columns, ' ');
   end just_left;

   function plurality (count : KDF9.word; for_1 : String := ""; for_more : String := "s")
   return String
   is (if count /= 1 then for_more else for_1);

   -- Return C converted to a 1-character string.
   function "+" (C : Character)
   return unit_string
   is ((1 => C));

   -- Return C with all Latin-1 lower-case letters converted to upper-case.
   function to_upper (C : Character)
   return Character
   renames Ada.Characters.Handling.to_upper;

   -- Return S with all Latin-1 lower-case letters converted to upper-case.
   function to_upper (S : String)
   return String
   renames Ada.Characters.Handling.to_upper;

   -- Return C with all Latin-1 upper-case letters converted to lower-case.
   function to_lower (C : Character)
   return Character
   renames Ada.Characters.Handling.to_lower;

   -- Return S with all Latin-1 upper-case letters converted to lower-case.
   function to_lower (S : String)
   return String
   renames Ada.Characters.Handling.to_lower;

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

   -- Take a string and ignore it.
   procedure discard (S : String) is null;

end formatting;
