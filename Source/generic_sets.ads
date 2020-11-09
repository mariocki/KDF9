-- generic_sets.ads
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

generic
   type member is (<>);
package generic_sets is

   pragma Unsuppress(All_Checks);

   type set is array (generic_sets.member) of Boolean;
   for  set'Component_Size use 1;
   pragma Convention(C, Entity => set);

   universe  : constant generic_sets.set;

   empty_set : constant generic_sets.set;

   function is_empty (set : generic_sets.set)
   return Boolean;

   -- The cardinality of the set.
   function "abs" (set : generic_sets.set)
   return Natural;

   -- Test for membership of the set.
   function "/" (set : generic_sets.set; member : generic_sets.member)
   return Boolean;

   function "/" (member : generic_sets.member; set : generic_sets.set)
   return Boolean;

   -- Constructors.

   function singleton (member : generic_sets.member)
   return generic_sets.set;

   function interval  (low, high : generic_sets.member)
   return generic_sets.set;

   function "not" (member : generic_sets.member)
   return generic_sets.set;

-- "not" (set : generic_sets.set) is predefined

   function "and" (member : generic_sets.member; set : generic_sets.set)
   return generic_sets.set;

   function "and" (set : generic_sets.set; member : generic_sets.member)
   return generic_sets.set;

-- "and" (set1, set2 : generic_sets.set) is predefined

   function "or"  (member : generic_sets.member; set : generic_sets.set)
   return generic_sets.set;

   function "or"  (set : generic_sets.set; member : generic_sets.member)
   return generic_sets.set;

-- "or"  (set1, set2 : generic_sets.set) is predefined

   function "xor" (member : generic_sets.member; set : generic_sets.set)
   return generic_sets.set;

   function "xor" (set : generic_sets.set; member : generic_sets.member)
   return generic_sets.set;

-- "xor" (set1, set2 : generic_sets.set) is predefined

   function "-" (set : generic_sets.set; member : generic_sets.member)
   return generic_sets.set;

   function "-" (set1, set2 : generic_sets.set)
   return generic_sets.set;

   -- subset
   overriding
   function "<=" (set1, set2 : generic_sets.set)
   return Boolean;

   -- proper subset
   overriding
   function "<"  (set1, set2 : generic_sets.set)
   return Boolean;

private

   universe  : constant generic_sets.set := (others => True);

   empty_set : constant generic_sets.set := (others => False);

end generic_sets;
