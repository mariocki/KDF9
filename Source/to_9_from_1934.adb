-- to_9_from_1934.adb
--
-- Convert ICT 1934 plotter code to KDF9 plotter code.
--
-- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2020, W. Findlay; all rights reserved.
--
-- This program is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. This program is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details. You should have
-- received a copy of the GNU General Public License distributed with
-- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Exceptions;
--
with HCI;
with host_IO;
with POSIX;
with plotter;

use  HCI;
use  host_IO;
use  POSIX;

procedure to_9_from_1934 is

   package plot_1900 is

      -- These are the 1900 Series plotting commands defined in ICT TP 4003/C,
      --    System Manual, Volume X: Visual Input/Output Equipment.

      type command is mod 2**6;

      dummy      : constant plot_1900.command := 2#000_000#;
      pen_down   : constant plot_1900.command := 2#000_001#;
      go_pY      : constant plot_1900.command := 2#000_010#;
      go_nY      : constant plot_1900.command := 2#000_100#;
      go_far_Y   : constant plot_1900.command := go_pY or go_nY;
      go_pX      : constant plot_1900.command := 2#001_000#;
      go_nX      : constant plot_1900.command := 2#010_000#;
      go_2in_X   : constant plot_1900.command := go_nX + go_pX;
      go_NW      : constant plot_1900.command := go_pX + go_pY;
      go_SW      : constant plot_1900.command := go_nX + go_pY;
      go_NE      : constant plot_1900.command := go_pX + go_nY;
      go_SE      : constant plot_1900.command := go_nX + go_nY;
      go_2in_far : constant plot_1900.command := go_2in_X + go_far_Y;
      pen_up     : constant plot_1900.command := 2#100_000#;

   end plot_1900;

   function plotter_type (c : Character)
   return Character is
      code : constant Natural := Character'Pos(c);
      zero : constant Natural := Character'Pos('0');
   begin
      if code / 8 /= 30 then
         raise Constraint_Error
               with "invalid plotter type code " & Natural'Image(code);
      end if;
      return Character'Val(code mod 8 + zero);
   end plotter_type;

   function octal (c : plot_1900.command)
   return String is
      code : constant Natural := plot_1900.command'Pos(c);
      zero : constant Natural := Character'Pos('0');
   begin
      return (Character'Val(code / 8 + zero), Character'Val(code mod 8 + zero));
   end octal;

   the_1900_stream : host_IO.stream;

   is_valid : constant array (plot_1900.command) of Boolean
            := (plot_1900.dummy      |
                plot_1900.go_NE      |
                plot_1900.go_NW      |
                plot_1900.go_nX      |
                plot_1900.go_nY      |
                plot_1900.go_pX      |
                plot_1900.go_pY      |
                plot_1900.go_SW      |
                plot_1900.go_SE      |
                plot_1900.pen_down   |
                plot_1900.pen_up     |
                plot_1900.go_2in_far |
                plot_1900.go_2in_X   |
                plot_1900.go_far_Y   => True,
                others               => False
               );

   p : Integer := 0;

   procedure get_1900_command (command : out plot_1900.command) is
      c : Character;
   begin
      get_byte(c, the_1900_stream);
      command := plot_1900.command(Character'Pos(c));
      if is_valid(command) then
         return;
      else
         raise Constraint_Error
            with "get_1900_command: invalid 1900 plotter command #"
               & octal(command)
               & " at character"
               & p'Image;
      end if;
   end get_1900_command;

   the_KDF9_stream : host_IO.stream;

   package KDF9 renames plotter;

   procedure put_KDF9_command (command : in plot_1900.command) is

      procedure put_command (KDF9_command : in plotter.command) is
      begin
         put_byte(Character'Val(Natural(KDF9_command)), the_KDF9_stream);
      end put_command;

   begin -- put_KDF9_command
      case command is
         when plot_1900.dummy      => null;  -- no need to do anything
         when plot_1900.pen_down   => put_command(KDF9.pen_down);
         when plot_1900.go_pY      => put_command(KDF9.go_pY);
         when plot_1900.go_nY      => put_command(KDF9.go_nY);
         when plot_1900.go_far_Y   => null;  -- not implemented
         when plot_1900.go_pX      => put_command(KDF9.go_pX);
         when plot_1900.go_nX      => put_command(KDF9.go_nX);
         when plot_1900.go_2in_X   => null;  -- not implemented
         when plot_1900.go_2in_far => null;  -- not implemented
         when plot_1900.go_NW      => put_command(KDF9.go_pXpY);
         when plot_1900.go_SE      => put_command(KDF9.go_nXnY);
         when plot_1900.go_SW      => put_command(KDF9.go_nXpY);
         when plot_1900.go_NE      => put_command(KDF9.go_pXnY);
         when plot_1900.pen_up     => put_command(KDF9.pen_up);
         when others               => raise Constraint_Error
                                         with "put_KDF9_command: invalid 1900 plotter command #"
                                            & octal(command)
                                            & " at character"
                                            & p'Image
                                            & " of the input file";
      end case;
   end put_KDF9_command;

   c : plot_1900.command;
   t : Character;

begin -- to_9_from_1934

   open(the_1900_stream, "Assembly/wabbit_data_1900", read_mode);
   open(the_KDF9_stream, "Assembly/wabbit_data_kdf9", write_mode);
   truncate(the_KDF9_stream, to_length => 0);

   get_byte(t, the_1900_stream);
   log_line("The input is for an ICT 1900 Series plotter type 1934/" & plotter_type(t));
   log_line("The output is for an EE KDF9 plotter, Calcomp type 564");

   loop
      get_1900_command(c);
      p := p + 1;
      put_KDF9_command(c);
   end loop;

exception

   when end_of_stream =>
      flush(the_KDF9_stream);
      log_line("to_9_from_1934:" & p'Image & " ICT 1934 commands were converted");

   when error : others =>
      flush(the_KDF9_stream);
      log_new_line;
      log_line(
               "**** Failure in to_9_from_1934; unexpected exception: "
             & Ada.Exceptions.Exception_Information(error)
             & " was raised!");

end to_9_from_1934;
