-- dumping.adb
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

with formatting;
with state_display;

use  formatting;
use  state_display;

package body dumping is

   pragma Unsuppress(All_Checks);

   use dumping.flag_support;

   function dumping_flag (c : Character) return dumping.flag is
   begin
      return dumping.flag(dumping.form'(to_upper(c)));
   end dumping_flag;

   type area is
      record
         format_set  : dumping.format_set := no_dumping_flag;
         first, last : KDF9.address;
      end record;

   no_dumping_area : constant dumping.area := (no_dumping_flag, 0, 0);

   dumping_area : array (dumping.area_number) of dumping.area := (others => no_dumping_area);

   pre_dumping_area_count  : Natural range 0 .. nr_of_dumping_areas := 0;
   post_dumping_area_count : Natural range 0 .. nr_of_dumping_areas := 0;

   function nr_of_pre_dumping_areas
   return dumping.area_count is
   begin
      return pre_dumping_area_count;
   end nr_of_pre_dumping_areas;

   function nr_of_post_dumping_areas
   return dumping.area_count is
   begin
      return post_dumping_area_count;
   end nr_of_post_dumping_areas;

   procedure request_a_dumping_area (format_set  : in dumping.format_set;
                                     first, last : in KDF9.address;
                                     was_stored  : out Boolean) is
   begin
      was_stored := False;
      if pre_dumping_area_count+post_dumping_area_count = nr_of_dumping_areas then
         return;
      end if;
      for d in dumping_area'Range loop
         if dumping_area(d) = (format_set, first, last) then
            was_stored := True;
            return;
         end if;
      end loop;
      if format_set/expunge_dump_flag then
         remove_specified_areas(format_set - expunge_dump_flag, first, last);
      end if;
      for d in dumping_area'Range loop
         if dumping_area(d).format_set = no_dumping_flag then
            dumping_area(d) := (format_set, first, last);
            was_stored := True;
            if initial_dump_flag/format_set then
               pre_dumping_area_count := pre_dumping_area_count + 1;
            end if;
            if final_dump_flag/format_set then
               post_dumping_area_count := post_dumping_area_count + 1;
            end if;
            return;
         end if;
      end loop;
   end request_a_dumping_area;

   max_types : constant Positive := abs is_dumping_flag - 1; -- P XOR Q

   -- format_image returns no_specification if format_set is empty.
   function format_image (format_set : dumping.format_set)
   return String is
      image_set  : dumping.format_set := format_set;
      result     : String(1 .. max_types) := (others => ' ');
      p          : Positive range 2 .. max_types := 2;
   begin
      if image_set = no_dumping_flag then
         return result;
      elsif image_set/initial_dump_flag then
          image_set := image_set - initial_dump_flag;
          result(1) := Character(initial_dump_flag);
      else
          image_set := image_set - final_dump_flag;
          result(1) := Character(final_dump_flag);
      end if;
      for f in dumping.flag loop
         if image_set/f then
            result(p) := Character(f);
            p := p + 1;
         end if;
      end loop;
      return trimmed(result);
   end format_image;

   function area_image (d : dumping.area_number)
   return String is
      first       : constant KDF9.address := dumping_area(d).first;
      last        : constant KDF9.address := dumping_area(d).last;
      format_set  : constant dumping.format_set := dumping_area(d).format_set;
      result      : String(1 .. max_types+2*(7)) := (others => ' ');
   begin
      if pre_dumping_area_count+post_dumping_area_count = 0 then
         return no_specification;
      end if;
      result(1 .. max_types)             := format_image(format_set);
      result(max_types+2 .. max_types+7) := oct_of(first);
      result(max_types+9 .. result'Last) := oct_of(last);
      return result;
   end area_image;

   procedure remove_specified_areas (format_set  : in dumping.format_set;
                                     first, last : in KDF9.address) is
   begin
      if pre_dumping_area_count+post_dumping_area_count = 0 then
         return;
      end if;
      for d in dumping_area'Range loop
         if dumping_area(d).first >= first and dumping_area(d).last <= last then
            dumping_area(d).format_set := dumping_area(d).format_set - format_set;
            if dumping_area(d).format_set-initial_dump_flag-final_dump_flag = no_dumping_flag then
               dumping_area(d) := no_dumping_area;
            end if;
            if initial_dump_flag/dumping_area(d).format_set then
               pre_dumping_area_count := Integer'Max(pre_dumping_area_count - 1, 0);
            end if;
            if final_dump_flag/dumping_area(d).format_set then
               post_dumping_area_count := Integer'Max(post_dumping_area_count - 1, 0);
            end if;
         end if;
      end loop;
   end remove_specified_areas;

   procedure print_formatted_area (d : in dumping.area_number) is
      format_set  : constant dumping.format_set := dumping_area(d).format_set;
      first       : constant KDF9.address := dumping_area(d).first;
      last        : constant KDF9.address := dumping_area(d).last;
   begin
      if format_set/tape_code_dump_flag then
         show_core_in_tape_code(first, last);
      end if;
      if format_set/normal_dump_flag then
         show_core_in_case_normal(first, last);
      end if;
      if format_set/shift_dump_flag then
         show_core_in_case_shift(first, last);
      end if;
      if format_set/ card_code_dump_flag then
         show_core_in_card_code(first, last);
      end if;
      if format_set/printer_dump_flag then
         show_core_in_print_code(first, last);
      end if;
      if format_set/ASCII_dump_flag then
         show_core_in_Latin_1(first, last);
      end if;
      if format_set/word_dump_flag then
         show_core_as_word_forms(first, last);
      end if;
      if format_set/Usercode_dump_flag then
         show_core_as_Usercode((0, KDF9.code_location(first)),
                               (0, KDF9.code_location( last)),
                                octal_option => not format_set/decimal_dump_flag);
      end if;
      if format_set/orders_dump_flag then
         show_core_as_syllables((0, KDF9.code_location(first)),
                                (0, KDF9.code_location( last)));
      end if;
   end print_formatted_area;

   procedure print_prerun_dump_areas is
   begin
      if pre_dumping_area_count = 0 then
         return;
      end if;
      mark_all_code_blocks_and_data_blocks;
      for d in dumping_area'Range loop
         if dumping_area(d).format_set/initial_dump_flag then
            print_formatted_area(d);
         end if;
      end loop;
   end print_prerun_dump_areas;

   procedure remove_prerun_dump_areas is
   begin
      if pre_dumping_area_count = 0 then
         return;
      end if;
      for d in dumping_area'Range loop
         if dumping_area(d).format_set/initial_dump_flag then
            dumping_area(d) := (no_dumping_flag, 0, 0);
         end if;
      end loop;
      pre_dumping_area_count := 0;
   end remove_prerun_dump_areas;

   procedure print_postrun_dump_areas is
   begin
      if post_dumping_area_count = 0 then
         return;
      end if;
      mark_all_code_blocks_and_data_blocks;
      for d in dumping_area'Range loop
         if dumping_area(d).format_set/final_dump_flag then
            print_formatted_area(d);
         end if;
      end loop;
   end print_postrun_dump_areas;

   procedure remove_postrun_dump_areas is
   begin
      if post_dumping_area_count = 0 then
         return;
      end if;
      for d in dumping_area'Range loop
         if dumping_area(d).format_set/final_dump_flag then
            dumping_area(d) := (no_dumping_flag, 0, 0);
         end if;
      end loop;
      post_dumping_area_count := 0;
   end remove_postrun_dump_areas;

end dumping;
