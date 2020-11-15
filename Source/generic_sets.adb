-- generic_sets.adb
--
-- Arbitrary-sized sets of a discrete member type.
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

-- generic
--    type member is (<>);
package body generic_sets is

   pragma Unsuppress(All_Checks);

   function "abs" (set : generic_sets.set)
   return Natural is
      result : Natural := 0;
   begin
      for member in generic_sets.member loop
         if set/member then
            result := result + 1;
         end if;
      end loop;
      return result;
   end "abs";

   function is_empty (set : generic_sets.set)
   return Boolean is
   begin
      return set = empty_set;
   end is_empty;

   function singleton (member : generic_sets.member)
   return generic_sets.set is
   begin
      return result : generic_sets.set := empty_set do
         result(member) := True;
      end return;
   end singleton;

   function interval (low, high : generic_sets.member)
   return generic_sets.set is
   begin
      return result : generic_sets.set := empty_set do
         result(low .. high) := (others => True);
      end return;
   end interval;

   function "/" (member : generic_sets.member; set : generic_sets.set)
   return Boolean is
   begin
      return set(member);
   end "/";

   function "/" (set : generic_sets.set; member : generic_sets.member)
   return Boolean is
   begin
      return set(member);
   end "/";

   function "not" (member : generic_sets.member)
   return generic_sets.set is
   begin
      return result : generic_sets.set := universe do
         result(member) := False;
      end return;
   end "not";

   function "and" (member : generic_sets.member; set : generic_sets.set)
   return generic_sets.set is
   begin
      return result : generic_sets.set := empty_set do
         result(member) := set(member);
      end return;
   end "and";

   function "and" (set : generic_sets.set; member : generic_sets.member)
   return generic_sets.set is
   begin
      return result : generic_sets.set := empty_set do
         result(member) := set(member);
      end return;
   end "and";

   function "or" (member : generic_sets.member; set : generic_sets.set)
   return generic_sets.set is
   begin
      return result : generic_sets.set := set do
         result(member) := True;
      end return;
   end "or";

   function "or" (set : generic_sets.set; member : generic_sets.member)
   return generic_sets.set is
   begin
      return result : generic_sets.set := set do
         result(member) := True;
      end return;
   end "or";

   function "xor" (member : generic_sets.member; set : generic_sets.set)
   return generic_sets.set is
   begin
      return result : generic_sets.set := set do
         result(member) := not set(member);
      end return;
   end "xor";

   function "xor" (set : generic_sets.set; member : generic_sets.member)
   return generic_sets.set is
   begin
      return result : generic_sets.set := set do
         result(member) := not set(member);
      end return;
   end "xor";

   function "-" (set : generic_sets.set; member : generic_sets.member)
   return generic_sets.set is
   begin
      return result : generic_sets.set := set do
         result(member) := False;
      end return;
   end "-";

   function "-" (set1, set2 : generic_sets.set)
   return generic_sets.set is
   begin
      return set1 and not set2;
   end "-";

   overriding
   function "<=" (set1, set2 : generic_sets.set)
   return Boolean is
   begin
      return set1 = (set1 and set2);
   end "<=";

   overriding
   function "<"  (set1, set2 : generic_sets.set)
   return Boolean is
   begin
      return (set1 <= set2) and (set1 /= set2);
   end "<";

end generic_sets;
