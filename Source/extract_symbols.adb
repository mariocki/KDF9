-- Derive a symbol table from a kal3 listing file.
--
-- This file is an auxiliary of ee9 (8.1x), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Unbounded;
--
with simple_IO;
with string_editing;

use  Ada.Exceptions;
use  Ada.Strings;
use  Ada.Strings.Unbounded;
--
use  simple_IO;
use  string_editing;

procedure extract_symbols is

   pragma Unsuppress(All_Checks);

   package CLI renames Ada.Command_Line;

   type fragment is access String;

   function "-" (text : String)
   return fragment
   is (new String'(text));

   type one_value_extraction  is
      record
         old_left, new_left : fragment;
      end record;

   one_value_extractions : constant array (Positive range <>) of one_value_extraction
      := (
          (-"V# = ",    -"Y#V "),
          (-"Y# = ",    -"Y#Y "),
          (-"W0 = E",   -"YW "),
          (-"Y0 = E",   -"YY "),
          (-"Z0 = E-1", null),
          (-"Z0 = E",   -"YZ ")
         );

   max_line_length : constant := 4096;
   subtype line_length_range is Natural range 0 .. max_line_length;
   subtype source_code_line  is String(line_length_range range 1..max_line_length);

   data : Unbounded_String;

   procedure do_three_field_extractions is
      one : constant String := "=== Start P";
      two : constant String := "V";
      three : constant String := "; at address ";
      pos_1, pos_2, pos_3, pos_4 : line_length_range;
   begin
      pos_1 := Index(data, one, 1);
      if pos_1 = 0 then return; end if;
      pos_2 := Index(data, two, pos_1);
      if pos_2 = 0 then return; end if;
      pos_3 := Index(data, three, pos_2);
      if pos_3 = 0 then return; end if;
      pos_4 := Index(data, "/", pos_3);
      if pos_4 = 0 then return; end if;
      if To_String(Unbounded_Slice(data, pos_1+one'Length, pos_2-1)) = "-1" then
         -- We ignore the spurious appearance of "P-1".
         return;
      end if;
      declare
         V_max_US  : constant Unbounded_String := Unbounded_Slice(data, pos_2+1, pos_3-1);
         V_max     : constant String           := trimmed(To_String(V_max_US));
         P_number  : constant Unbounded_String := Unbounded_Slice(data, pos_1+one'Length, pos_2-1);
         P_address : constant Unbounded_String := Unbounded_Slice(data, pos_3+three'Length, pos_4-1);
      begin
         print_line("YP"
                & trimmed(To_String(P_number))
                & (if V_max = "-1" then "" else "V" & V_max)
                & " @"
                & trimmed(To_String(P_address))
                 )
                ;
      end;
   end do_three_field_extractions;

   procedure do_two_field_extractions is
      l_pos : line_length_range;
   begin
     for x in Character range 'A'..'Z' loop
         l_pos := Index(data, "Y" & x & "0 = E", 1);
         if l_pos /= 0 then
            l_pos := Index(data, "E", 1);
            print_line("Y " & x & " " & To_String(Unbounded_Slice(data, l_pos+1, Length(data))));
            return;
         end if;
      end loop;
   end do_two_field_extractions;

   procedure do_one_field_extractions is
      l_pos : line_length_range;
   begin
      for i in one_value_extractions'Range loop
         declare
            this   : one_value_extraction renames one_value_extractions(i);
            before : String               renames this.old_left.all;
         begin
            l_pos := Index(data, before, 1);
            if l_pos /= 0 then
               if this.new_left = null then
                  -- Handle kal3's treatment of Z stores when the program includes no ST directive.
                  data := To_Unbounded_String("YZ 32767");
               else
                  data := this.new_left.all
                        & Unbounded_Slice(data, l_pos+before'Length, Length(data));
                  print_line(To_String(data));
                  return;
               end if;
            end if;
         end;
      end loop;
   end do_one_field_extractions;

   procedure do_all_value_extractions is
   begin
      if Length(data) = 0 then
         return;
      end if;
      do_one_field_extractions;
      do_two_field_extractions;
      do_three_field_extractions;
      flush_outputs;
   end do_all_value_extractions;

   procedure complain (about : in String := "") is
   begin
      if about /= "" then
         report_line(about & ".");
      end if;
      report_line("usage: extract_symbols");
      CLI.Set_Exit_Status(CLI.Failure);
      raise end_error;
   end complain;

   line : source_code_line;
   last : line_length_range;

begin
   if CLI.Argument_Count /= 0 then
      complain;
   end if;
   -- Get past the first occurrence of "Finished".
   for i in 1 .. 10 loop
      read_line(line, last);
   end loop;
   -- Look for and convert symbol definitions.
   loop
      read_line(line, last);
      if line(1..last) = "Finished" then
         flush_outputs;
         return;
      else
         data := To_Unbounded_String(line(1..last));
         do_all_value_extractions;
      end if;
   end loop;
exception
   when end_error =>
      flush_outputs;
   when error : others =>
      report_line(Exception_Message(error));
      flush_outputs;
      CLI.Set_Exit_Status(CLI.Failure);
end extract_symbols;
