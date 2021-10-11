-- Provide support for diagnostic core-dumping area descriptions.
--
-- This file is part of ee9 (8.1a), the GNU Ada emulator of the English Electric KDF9.
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

with generic_sets;
with KDF9;

use  KDF9;

package dumping is

   type flag is new Character range '@' .. 'Z';

   no_flag        : constant dumping.flag := '@';
   ASCII_flag     : constant dumping.flag := 'A';
   card_code_flag : constant dumping.flag := 'C';
   decimal_flag   : constant dumping.flag := 'D';
   single_flag    : constant dumping.flag := 'E';
   final_flag     : constant dumping.flag := 'F';
   half_flag      : constant dumping.flag := 'H';
   initial_flag   : constant dumping.flag := 'I';
   normal_flag    : constant dumping.flag := 'N';
   orders_flag    : constant dumping.flag := 'O';
   printer_flag   : constant dumping.flag := 'L';
   shift_flag     : constant dumping.flag := 'S';
   tape_code_flag : constant dumping.flag := 'T';
   Usercode_flag  : constant dumping.flag := 'U';
   word_flag      : constant dumping.flag := 'W';

   function dumping_flag (c : Character)
   return dumping.flag;

   package flag_support is new generic_sets(member => dumping.flag);

   subtype format_set is flag_support.set;
   use type format_set;

   is_parameter_flag : constant dumping.format_set
                     := (  decimal_flag
                         | single_flag
                         | half_flag
                         | ASCII_flag
                         | orders_flag
                         | printer_flag
                         | tape_code_flag
                         | Usercode_flag
                         | card_code_flag
                         | normal_flag
                         | shift_flag
                         | word_flag     => True,
                           others        => False
                        );


   is_epoch_flag : constant dumping.format_set
                 := (  initial_flag
                     | final_flag => True,
                       others     => False
                    );

   is_dumping_flag  : constant dumping.format_set
                    := is_parameter_flag or is_epoch_flag;

   no_dumping_flags : constant dumping.format_set
                    := flag_support.empty_set;

   nr_of_dumping_areas : constant := 100;
   subtype area_count  is Natural  range 0 .. nr_of_dumping_areas;
   subtype area_number is Positive range 1 .. nr_of_dumping_areas;

   procedure request_a_dumping_area (
                                     format_set  : in dumping.format_set;
                                     first, last : in KDF9.address;
                                     was_stored  : out Boolean
                                    );

   procedure print_prerun_dump_areas;

   procedure remove_prerun_dump_areas;

   procedure print_postrun_dump_areas;

   procedure remove_postrun_dump_areas;

   function nr_of_pre_dumping_areas
   return dumping.area_count;

   function nr_of_post_dumping_areas
   return dumping.area_count;

   no_specification : constant String := "";

   subtype sub_word_flag is Character
      with Static_Predicate =>
         sub_word_flag in 'S' | 's' | 'C' | 'c' | 'L' | 'l' | 'U' | 'u' | 'W' | 'w';

   -- poke support is in dumping because it is needed at the same time during initialization.
   procedure add_to_poke_list (
                               address    : in KDF9.address;
                               sub_word   : in sub_word_flag;
                               position   : in KDF9.address;
                               value      : in KDF9.word;
                               was_stored : out Boolean
                              );

   -- poke_all_amendments effects all stored pokes and then clears the poke list for reuse.
   procedure poke_all_amendments;

end dumping;
