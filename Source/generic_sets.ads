-- generic_sets.ads
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

generic
   type member is (<>);
package generic_sets is

   pragma Preelaborate;

--
-- This package implements only those set operations that are needed by ee9.
--

   type set is array (generic_sets.member) of Boolean
      with Component_Size => 1, Convention => C;

   universe  : constant generic_sets.set := (others => True);

   empty_set : constant generic_sets.set := (others => False);

   function "abs" (set : generic_sets.set)
   return Natural;

   -- Test for membership of the set.
   function "/" (set : generic_sets.set; member : generic_sets.member)
   return Boolean;

   function "/" (member : generic_sets.member; set : generic_sets.set)
   return Boolean;

    function "or"  (member : generic_sets.member; set : generic_sets.set)
    return generic_sets.set;

    function "or"  (set : generic_sets.set; member : generic_sets.member)
    return generic_sets.set;

-- "or"  (set1, set2 : generic_sets.set) is predefined

   function "-" (set : generic_sets.set; member : generic_sets.member)
   return generic_sets.set;

   function "-" (set1, set2 : generic_sets.set)
   return generic_sets.set;

end generic_sets;
