-- Generate a P option setting for the Kidsgrove compiler from symbolic option names.
--
-- kidopt is an auxiliary of ee9 (7.0a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2021, W. Findlay; all rights reserved.
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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Text_IO;

use  Ada.Characters.Handling;
use  Ada.Text_IO;

procedure kidopt is

   package CLI renames Ada.Command_Line;

   type options is (SEGMENT, TRACE, TABLES, ORIG_SW, TEXT, NO_OPT, NO_TEST, NO_WARN, LOAD_AND_GO);

   type values  is mod 2**8;

   options_set   : values := 0;

   option_bit    : constant array(options) of values
                 := (
                     SEGMENT     => 8#200#, -- D0
                     TRACE       => 8#100#, -- D1
                     TABLES      => 8#040#, -- D2
                     ORIG_SW     => 8#020#, -- D3
                     TEXT        => 8#010#, -- D4
                     NO_OPT      => 8#004#, -- D5
                     NO_TEST     => 8#002#, -- D6
                     NO_WARN     => 8#001#, -- D7
                     LOAD_AND_GO => 8#000#  -- so as not to mess with syllable 0
                    );

   load_and_go_is_requested : Boolean := False;

   procedure include_option (given : in String) is
   begin
      if given in "SEGMENT"
                | "TRACE" | "TABLES" | "ORIG_SW" | "TEXT" | "NO_OPT" | "NO_TEST" | "NO_WARN"
                | "LOAD_AND_GO"
      then
         for o in options loop
            if given = o'Image then
               options_set := options_set or option_bit(o);
               if given = "LOAD_AND_GO" then
                  load_and_go_is_requested := True;
               end if;
            end if;
         end loop;
      else
         Put_Line(Standard_Error, given & " is not a valid option for kidopt.");
         CLI.Set_Exit_Status(CLI.Failure);
      end if;
   end include_option;

   function oct_of (v : values)
   return String is
      value : values := v;
      oct   : String(1 .. 8);
   begin
      for i in reverse oct'Range loop
         oct(i) := Character'Val(Natural(value mod 8)+Character'Pos('0'));
         value := value / 8;
      end loop;
      return "#" & oct;
   end oct_of;

   start : Positive := 1;

begin -- kidopt

   if CLI.Argument_Count = 0 then
      return;
   elsif CLI.Argument_Count = 1                                 and then
            To_Upper(CLI.Argument(1)) in "-H" | "-HELP" | "--HELP"  then
      Put_Line(Standard_Error, "kidopt [ WITH ] { option }");
      Put     (Standard_Error, "where option is one of: ");
      Put_Line(Standard_Error, "TRACE, TABLES, ORIG_SW, TEXT, NO_OPT, NO_TEST, NO_WARN, LOAD_AND_GO");
      return;
   elsif CLI.Argument_Count > 0             and then
            To_Upper(CLI.Argument(1)) = "WITH"  then
      start := 2;
   end if;

   for i in start .. CLI.Argument_Count loop
      if To_Upper(CLI.Argument(i)) = "NONE" then
         options_set := 0;
      else
         include_option(To_Upper(CLI.Argument(i)));
      end if;
   end loop;

   Put_Line("P 11065S0 " & oct_of(options_set));
   if load_and_go_is_requested then
      Put_Line("P 11065S1 #200");
   end if;

end kidopt;
