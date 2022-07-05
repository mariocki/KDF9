-- Provide basic string-data reformatting operations.
--
-- This file is part of ee9 (8.2z), the GNU Ada emulator of the English Electric KDF9.
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

package body string_editing is

    -- Return C enclosed in quotes.
   function "abs" (C : Character)
   return String
   is ("""" & C & """");

   -- Return S trimmed and enclosed in quotes.
   function "abs" (S : String)
   return String
   is ("""" & trimmed(S) & """");

   -- Return S with all trailing blanks removed.
   function trim_right (S : String)
   return String is
      interim : constant String  := S;
      left    : constant Natural := interim'First;
      right   : Natural := interim'Last;
   begin
      for r in reverse interim'Range loop
         if interim(r) /= ' ' then
            right := r;
      exit;
         end if;
      end loop;
      return result : String (1..right-left+1) do
         result := interim(left..right);
      end return;
   end trim_right;

   -- Return "L', R'", or "L'"if R' is empty; ' indicates removal of trailing blanks.
   function "-" (L, R : String)
   return String is
      trim_R : constant String := trim_right(R);
   begin
      if trim_R /= "" then
         return trim_right(L) & ", " & trim_R;
      else
         return trim_right(L);
      end if;
   end "-";

   -- Return S with all leading and trailing blanks removed.
   function trimmed (S : String)
   return String is
      interim : constant String  := S;
      left    : Natural := interim'First;
      right   : Natural := interim'Last;
   begin
      for r in reverse interim'Range loop
         if interim(r) /= ' ' then
            right := r;
      exit;
         end if;
      end loop;
      for l in interim'Range loop
         if interim(l) /= ' ' then
            left := l;
      exit;
         end if;
      end loop;
      return result : String (1..right-left+1) do
         result := interim(left..right);
      end return;
   end trimmed;

   -- Return trimmed(S), right-just_right in a field of width at least W.
   function just_right (S : String; W : Positive := 3)
   return String is
      image : constant String   := trimmed(S);
      count : constant Positive := Positive'Max(W, image'Length);
      blank : constant String   := (1..count-image'Length => ' ');
   begin
      return (if count <= image'Length then image(1..count) else blank & image);
   end just_right;

   -- Return trimmed(S), left-justified in a field of width at least W.
   function just_left (S : String; W : Positive := 3)
   return String is
     image : constant String  := trimmed(S);
     count : constant Natural := Natural'Max(W, image'Length);
     blank : constant String  := (1..count-image'Length => ' ');
   begin
      return (if count <= image'Length then image(1..count) else image & blank);
   end just_left;

   function "+" (C : Character)
   return String
   is ((1 => C));

   function upper (C : Character)
   return Character
   is(upper(+C)(1));

   function upper (S : String)
   return String is
      offset : constant Positive := Character'Pos('a') - Character'Pos('A');
      result : String := S;
   begin
      for c of result loop
         if c in 'a' .. 'z' then
            c := Character'Val(Character'Pos(c) - offset);
         end if;
      end loop;
      return result;
   end upper;

   function lower (C : Character)
   return Character
   is(lower(+C)(1));

   function lower (S : String)
   return String is
      offset : constant Positive := Character'Pos('a') - Character'Pos('A');
      result : String := S;
   begin
      for c of result loop
         if c in 'A' .. 'Z' then
            c := Character'Val(Character'Pos(c) + offset);
         end if;
      end loop;
      return result;
   end lower;

   function plurality (multiple : Boolean; for_1 : String := ""; for_more : String := "s")
   return String
   is (if multiple then for_more else for_1);

   function index_forward (text, key : String; start : Positive)
   return Natural is
   begin
      if start not in text'First .. text'Last - key'Length + 1 then
         return 0;
      end if;
      for i in start .. text'Last - key'Length + 1 loop
         if text(i..i+key'Length-1) = key then
            return i;
         end if;
      end loop;
      return 0;
   end index_forward;

   function index_backward (text, key : String; start : Positive)
   return Natural is
   begin
      if start not in text'First .. text'Last - key'Length + 1 then
         return 0;
      end if;
      for i in reverse text'First .. start loop
         if text(i..i+key'Length-1) = key then
            return i;
         end if;
      end loop;
      return 0;
   end index_backward;

   -- An alternative to "&" which inserts a space between the operands.
   function "+" (S : String; C : Character)
   return String
   is (trimmed(S) & " " & C);

   function "+" (C : Character; S : String)
   return String
   is (C & " " & trimmed(S));

   -- An alternative to "&" which inserts a space between the trimmed operands.
   function "+" (S1, S2 : String)
   return String
   is (trimmed(S1) & " " & trimmed(S2));

end string_editing;
