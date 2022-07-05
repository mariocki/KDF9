-- Set new ST and TL parameters in a kal3 source file produced by kalgol.
--
-- This file is an auxiliary of ee9 (8.2z), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2022, W. Findlay; all rights reserved.
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
--
with simple_IO;
with string_editing;

use  simple_IO;
use  string_editing;

procedure st_tl is

   pragma Unsuppress(All_Checks);

   package CLI renames Ada.Command_Line;

   procedure complain (about : in String := "") is
   begin
      if about /= "" then
         report_line(about & ".");
      end if;
      report_line("usage: st_tl [ store_size [ time_limit ] ]");
      CLI.Set_Exit_Status(CLI.Failure);
      raise end_error;
   end complain;


   max_line_length : constant := 1024; -- The longest line observed had 282 characters.

   subtype line_length_range is Natural range 0 .. max_line_length;

   subtype source_code_line  is String(line_length_range range 1..max_line_length);

   line : source_code_line;
   last : line_length_range;

   ST : Natural := 7680;
   TL : Natural := 3000;

begin
   case CLI.Argument_Count is
      when 0 =>
         null; -- Defaults apply;
      when 1 =>
         ST := Natural'Value(CLI.Argument(1));
      when 2 =>
         ST := Natural'Value(CLI.Argument(1));
         TL := Natural'Value(CLI.Argument(2));
      when others =>
         complain;
   end case;

   loop
      read_line(line, last);
   exit when index_forward(line(1..last), ";", 1) /= 0;
      print_line(line(1..last));
   end loop;

   print_line("ST " & trimmed(ST'Image) & "; TL " & trimmed(TL'Image) & ";");

   if index_forward(line(1..last), "ST", 1) = 0 and index_forward(line(1..last), "TL", 1) = 0 then
      print_line(line(1..last));
   end if;

   loop
      read_line(line, last);
      print_line(line(1..last));
   end loop;
exception
   when end_error =>
      flush_outputs;
      return;
   when others =>
      report_line("st_tl failed!");
      flush_outputs;
      CLI.Set_Exit_Status(CLI.Failure);
end st_tl;
