-- Provide support for diagnostic core-dumping area descriptions.
--
-- This file is part of ee9 (7.0a), the GNU Ada emulator of the English Electric KDF9.
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

with formatting;
with state_display;

use  formatting;
use  state_display;

package body dumping is

   type poke_list_entry is
      record
         address  : KDF9.address;
         sub_word : sub_word_flag;
         position : KDF9.address;
         value    : KDF9.word;
      end record;

   length_of_poke_list : constant := 100;
   poke_list_count     : Natural range 0 .. length_of_poke_list := 0;
   poke_list           : array (Positive range 1 .. length_of_poke_list) of poke_list_entry;

   use dumping.flag_support;

   function dumping_flag (c : Character)
   return dumping.flag
   is (dumping.flag(to_upper(c)));

   type area is
      record
         format_set  : dumping.format_set := no_dumping_flags;
         first, last : KDF9.address;
      end record;

   no_dumping_area : constant dumping.area := (no_dumping_flags, 0, 0);

   dumping_areas : array (dumping.area_number) of dumping.area := (others => no_dumping_area);

   pre_dumping_area_count  : area_count := 0;
   post_dumping_area_count : area_count := 0;

   function nr_of_pre_dumping_areas
   return dumping.area_count
   is (pre_dumping_area_count);

   function nr_of_post_dumping_areas
   return dumping.area_count
   is (post_dumping_area_count);

   procedure request_a_dumping_area (
                                     format_set  : in dumping.format_set;
                                     first, last : in KDF9.address;
                                     was_stored  : out Boolean
                                    ) is
   begin
      was_stored := False;
      if pre_dumping_area_count+post_dumping_area_count = nr_of_dumping_areas then
         return;
      end if;
      for d of dumping_areas loop
         if d = (format_set, first, last) then
            was_stored := True;
            return;
         end if;
      end loop;
      for d of dumping_areas loop
         if d.format_set = no_dumping_flags then
            d := (format_set, first, last);
            was_stored := True;
            if format_set/initial_flag then
               pre_dumping_area_count := pre_dumping_area_count + 1;
            end if;
            if format_set/final_flag then
               post_dumping_area_count := post_dumping_area_count + 1;
            end if;
            return;
         end if;
      end loop;
   end request_a_dumping_area;

   procedure print_formatted_area (d : in dumping.area) is
      format_set  : constant dumping.format_set := d.format_set;
      first       : constant KDF9.address := d.first;
      last        : constant KDF9.address := d.last;
   begin
      if format_set/tape_code_flag then
         show_core_in_tape_code(first, last);
      end if;
      if format_set/normal_flag then
         show_core_in_case_normal(first, last);
      end if;
      if format_set/shift_flag then
         show_core_in_case_shift(first, last);
      end if;
      if format_set/ card_code_flag then
         show_core_in_card_code(first, last);
      end if;
      if format_set/printer_flag then
         show_core_in_print_code(first, last);
      end if;
      if format_set/ASCII_flag then
         show_core_in_Latin_1(first, last);
      end if;
      if format_set/word_flag then
         show_core_as_word_forms(first, last);
      end if;
      if format_set/Usercode_flag then
         show_core_as_Usercode(
                               (KDF9.code_address(first), 0),
                               (KDF9.code_address(last),  0),
                                octal_option => not format_set/decimal_flag
                              );
      end if;
      if format_set/orders_flag then
         show_core_as_syllables((KDF9.code_address(first), 0),
                                (KDF9.code_address(last),  0));
      end if;
   end print_formatted_area;

   procedure print_dump_areas (flag : in dumping.flag; count : in dumping.area_count) is
      Usercode_wanted : Boolean := False;
   begin
      if count = 0 then
         return;
      end if;
      for d of dumping_areas loop
         Usercode_wanted := Usercode_wanted or d.format_set/Usercode_flag;
      end loop;
      if Usercode_wanted then
         mark_all_code_blocks_and_data_blocks(pre_run => flag = initial_flag);
      end if;
      for d of dumping_areas loop
         if d.format_set/flag then
            print_formatted_area(d);
         end if;
      end loop;
   end print_dump_areas;

   procedure print_prerun_dump_areas is
   begin
      print_dump_areas(initial_flag, pre_dumping_area_count);
   end print_prerun_dump_areas;

   procedure print_postrun_dump_areas is
   begin
      print_dump_areas(final_flag, post_dumping_area_count);
   end print_postrun_dump_areas;

   procedure remove_dump_areas (flag : in dumping.flag; count : in out dumping.area_count) is
   begin
      if count = 0 then
         return;
      end if;
      for d of dumping_areas loop
         if d.format_set/flag then
            d := no_dumping_area;
         end if;
      end loop;
      count := 0;
   end remove_dump_areas;

   procedure remove_prerun_dump_areas is
   begin
      remove_dump_areas(initial_flag, pre_dumping_area_count);
   end remove_prerun_dump_areas;

   procedure remove_postrun_dump_areas is
   begin
      remove_dump_areas(final_flag, post_dumping_area_count);
   end remove_postrun_dump_areas;

   procedure add_to_poke_list (address    : in KDF9.address;
                               sub_word   : in sub_word_flag;
                               position   : in KDF9.address;
                               value      : in KDF9.word;
                               was_stored : out Boolean) is
   begin
      if poke_list_count < length_of_poke_list then
         poke_list_count := poke_list_count + 1;
         poke_list(poke_list_count) := (address, sub_word, position, value);
         was_stored := True;
      else
         was_stored := False;
      end if;
   end add_to_poke_list;

   procedure poke_all_amendments is
   begin
      for p in 1..poke_list_count loop
         poke(poke_list(p).address, poke_list(p).sub_word, poke_list(p).position, poke_list(p).value);
      end loop;
      poke_list_count := 0;
   end poke_all_amendments;

end dumping;
