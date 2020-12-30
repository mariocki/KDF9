-- generic_sets.adb
--
-- Powersets of a discrete member type.
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

-- generic
--    type member is (<>);
package body generic_sets is

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

   function "/" (member : generic_sets.member; set : generic_sets.set)
   return Boolean
   is (set(member));

   function "/" (set : generic_sets.set; member : generic_sets.member)
   return Boolean
   is (set(member));

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

   function "-" (set : generic_sets.set; member : generic_sets.member)
   return generic_sets.set is
   begin
      return result : generic_sets.set := set do
         result(member) := False;
      end return;
   end "-";

   function "-" (set1, set2 : generic_sets.set)
   return generic_sets.set is
   begin -- (set1 and not set2), avoiding need for large statically allocated workspace
      return result : generic_sets.set := set1 do
         for m in generic_sets.member loop
            if set2(m) then
               result(m) := False;
            end if;
         end loop;
      end return;
   end "-";

end generic_sets;
