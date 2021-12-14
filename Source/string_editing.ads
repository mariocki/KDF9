-- Provide basic string-data reformatting operations.
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

package string_editing is

   -- Return C enclosed in the specified quotes.
   function quote (C : Character; left_quote, right_quote : String := """")
   return String;

   -- Return trimmed(S) enclosed in the specified quotes.
   function quote (S : String; left_quote, right_quote : String := """")
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

   -- Return "L', R'", or "L'"if R' is empty; ' indicates removal of trailing blanks.
   function "-" (L, R : String)
   return String;

   -- Return C with any letter in the range a .. z (only) converted to upper-case.
   function upper (C : Character)
   return Character;

   -- Return S with all letters in the range a .. z (only)  converted to upper-case.
   function upper (S : String)
   return String;

   -- Return C with any letter in the range A .. Z (only) converted to lower-case.
   function lower (C : Character)
   return Character;

   -- Return S with all letters in the range A .. Z (only) converted to lower-case.
   function lower (S : String)
   return String;

   -- Return the (pluralizing) suffix if multiple.
   function plurality (multiple : Boolean; for_1 : String := ""; for_more : String := "s")
   return String;

   -- Return the position, at or after start, of key within text.  Return 0 if absent.
   function index_forward (text, key : String; start : Positive)
   return Natural;

   -- Return the position, at or before start, of key within text.  Return 0 if absent.
   -- Searches backwards.
   function index_backward (text, key : String; start : Positive)
   return Natural;

   -- An alternative to "&" which inserts a space between the operands.
   function "+" (S1, S2 : String)
   return String;

end string_editing;
