-- Simple line-oriented I/O avoiding the need to include Ada.Text_IO.
--
-- This file is part of ee9 (9.0p), the GNU Ada emulator of the English Electric KDF9.
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

package simple_IO is

   end_error, EOL_error : exception;

   procedure report (s : in String);

   procedure report_line;

   procedure report_line (s : String);

   procedure new_line (count : Positive := 1);

   procedure print (s : in String);

   procedure print_line;

   procedure print_line (s : String);

   procedure flush_outputs;

   procedure skip_line;

   procedure read_line (s : out String; last : out Natural; fail_EOL : in Boolean := False);

end simple_IO;
