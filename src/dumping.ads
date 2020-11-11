-- dumping.ads
--
-- Provide support for diagnostic core-dumping area descriptions.
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

with KDF9;
with generic_sets; pragma Elaborate_All(generic_sets);

use  KDF9;

package dumping is

   pragma Unsuppress(All_Checks);

   type flag is new Character range '@' .. 'Z';

   no_dump_flag        : constant dumping.flag := '@';
   ASCII_dump_flag     : constant dumping.flag := 'A';
   card_code_dump_flag : constant dumping.flag := 'C';
   decimal_dump_flag   : constant dumping.flag := 'D';
   single_dump_flag    : constant dumping.flag := 'E';
   half_dump_flag      : constant dumping.flag := 'H';
   initial_dump_flag   : constant dumping.flag := 'I';
   normal_dump_flag    : constant dumping.flag := 'N';
   orders_dump_flag    : constant dumping.flag := 'O';
   printer_dump_flag   : constant dumping.flag := 'L';
   shift_dump_flag     : constant dumping.flag := 'S';
   tape_code_dump_flag : constant dumping.flag := 'T';
   Usercode_dump_flag  : constant dumping.flag := 'U';
   word_dump_flag      : constant dumping.flag := 'W';
   expunge_dump_flag   : constant dumping.flag := 'X';
   final_dump_flag     : constant dumping.flag := 'P';

   subtype form is Character range Character(dumping.flag'First) .. Character(dumping.flag'Last);

   function dumping_flag (c : Character) return dumping.flag;

   package flag_support is new generic_sets(member => dumping.flag);

   subtype format_set is flag_support.set;
   use type format_set;
   pragma Warnings(Off, format_set);

   is_parameter_flag : constant dumping.format_set
                     := (  decimal_dump_flag
                         | single_dump_flag
                         | half_dump_flag
                         | ASCII_dump_flag
                         | orders_dump_flag
                         | printer_dump_flag
                         | tape_code_dump_flag
                         | Usercode_dump_flag
                         | card_code_dump_flag
                         | normal_dump_flag
                         | shift_dump_flag
                         | word_dump_flag     => True,
                           others             => False
                        );


   is_epoch_flag : constant dumping.format_set
                 := (  initial_dump_flag
                     | final_dump_flag => True,
                       others         => False
                    );

   is_dumping_flag  : constant dumping.format_set
                    := is_parameter_flag or is_epoch_flag;

   no_dumping_flag  : constant dumping.format_set
                    := flag_support.empty_set;

   nr_of_dumping_areas : constant := 100;

   subtype area_count  is Natural  range 0 .. nr_of_dumping_areas;
   subtype area_number is Positive range 1 .. nr_of_dumping_areas;

   no_specification : constant String;

   -- area_image returns no_specification if area(d) is undefined or empty.
   function area_image (d : dumping.area_number)
   return String;

   -- format_image returns blanks if format_set is empty.
   function format_image (format_set : dumping.format_set)
   return String;

   procedure request_a_dumping_area (format_set  : in dumping.format_set;
                                     first, last : in KDF9.address;
                                     was_stored  : out Boolean);

   procedure remove_specified_areas (format_set  : in dumping.format_set;
                                     first, last : in KDF9.address);

   procedure print_prerun_dump_areas;

   procedure remove_prerun_dump_areas;

   procedure print_postrun_dump_areas;

   procedure remove_postrun_dump_areas;

   function nr_of_pre_dumping_areas
   return dumping.area_count;

   function nr_of_post_dumping_areas
   return dumping.area_count;

private

   no_specification : constant String := "";

end dumping;
