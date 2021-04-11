-- This contains information used by magnetic tape emulation.
-- It is separated out to allow use in programs other than ee9
--    without dragging in all the dependencies of IOC.
--
-- This file is part of ee9 (6.2e), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Characters.Latin_1;

use  Ada.Characters.Latin_1;

package IOC_tape_data is

   -- 81 is efficient for both card images and full 160-column print lines with final LS or PC.
   slice_size_limit : constant := 81;

   MT_record_length : constant := slice_size_limit + 3;

   subtype valid_slice_kinds is Character
      with Static_Predicate => valid_slice_kinds in 'D' | 'G' | 'W' | 'e' | 'o';

   subtype erasure_kinds is Character
      with Static_Predicate => erasure_kinds in 'G' | 'W';

   subtype tape_mark_kinds is Character
      with Static_Predicate => tape_mark_kinds in 'e' | 'o';

   subtype valid_slice_flags is Character
      with Static_Predicate => valid_slice_flags in NUL | SOH | BS | HT | '@' | 'A' | 'H' | 'I';

   subtype final_slice_flags is Character
      with Static_Predicate => final_slice_flags in BS | HT | 'H' | 'I';

   subtype last_block_flags is Character
      with Static_Predicate => last_block_flags in '@' | 'A' | 'H' | 'I';

   -- This is the (only) character written to a 7-track tape mark block.
   -- It is the § character on file, but is read back as #17 by the PI?Qq orders.
   -- Section_Sign is appropriate because tape marks are used to delimit sections of a tape file.
   -- See Manual, Appendix 7 ¶2, p317.
   tape_mark_sign  : constant Character := Section_Sign;

   block_padding   : constant Character := Middle_Dot;

   -- This subtype is used in the post-processing of OUT 8 spool tapes.
   subtype OUT8_selection_characters is Character
      with Static_Predicate => OUT8_selection_characters in '#' | '_' | '@' | '"';

end IOC_tape_data;
