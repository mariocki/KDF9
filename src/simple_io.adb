-- Simple line-oriented I/O avoiding the need to include Ada.Text_IO.
--
-- This file is part of ee9 (8.2z), the GNU Ada emulator of the English Electric KDF9.
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

with host_IO;

use  host_IO;

package body simple_IO is

   standard_input, standard_output, error_output : host_IO.stream;

   procedure report (s : in String) is
   begin
      put_chars(s, error_output);
      flush(error_output);
   end report;

   procedure report_line is
   begin
      put_EOL(error_output);
      flush(error_output);
   end report_line;

   procedure report_line (s : String) is
   begin
      report(s);
      report_line;
   end report_line;

   procedure print (s : in String) is
   begin
      put_chars(s, standard_output);
   end print;

   procedure print_line is
   begin
      put_EOL(standard_output);
   end print_line;

   procedure print_line (s : String) is
   begin
      print(s);
      print_line;
   end print_line;

   LF : constant Character := Character'Val(16#0A#);

   procedure skip_line is
      c : Character;
   begin
      loop
         get_char(c, standard_input);
      exit when c = LF;
      end loop;
   exception
      when end_of_stream =>
         raise end_error;
   end skip_line;

   procedure read_line (s : out String; last : out Natural; fail_EOL : in Boolean := False) is
      ch : Character;
   begin
      last := 0;
      for i in s'Range loop
         get_char(s(i), standard_input);
      exit when s(i) = LF;
         last := i;
      end loop;
      if fail_EOL and last = s'Last then
         raise EOL_error;
      elsif last = s'Last then
         loop
            get_char(ch, standard_input);
         exit when ch = LF;
         end loop;
      end if;
   exception
      when end_of_stream =>
         raise end_error;
   end read_line;

   procedure new_line (count : Positive := 1) is
   begin
      for i in 1 .. count loop
         print_line;
      end loop;
   end new_line;

   procedure flush_outputs is
   begin
      flush(standard_output);
      flush(error_output);
   end flush_outputs;

begin
   open(standard_input,  0);
   open(standard_output, 1);
   open(error_output,    2);
end simple_IO;
