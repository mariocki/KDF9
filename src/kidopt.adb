-- kidopt.adb
--
-- Generate a P option setting for the Kidsgrove compiler from symbolic option names.
--
-- kidopt is an auxiliary of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Text_IO;

use  Ada.Characters.Handling;
use  Ada.Text_IO;

procedure kidopt is

   package CLI renames Ada.Command_Line;

   type options is (SEGMENT, TRACE, TABLES, LABELS, NO_OPT, DEBUG, NO_WARN);

   type values  is mod 2**8;

   options_set   : values := 0;

   option_bit    : constant array(options) of values
                 := (
                     SEGMENT   => 8#200#, -- D0
                     TRACE     => 8#100#, -- D1
                     TABLES    => 8#040#, -- D2
                     -- D3 is ignored.    -- D3
                     LABELS    => 8#010#, -- D4 was WITH TEXT !!
                     NO_OPT    => 8#004#, -- D5
                     DEBUG     => 8#002#, -- D6 was WITHOUT TESTING !!
                     NO_WARN   => 8#001#  -- D7
                    );

   procedure include_option (given : in String) is
   begin
      if given in "SEGMENT" | "TRACE" | "TABLES" | "LABELS" | "NO_OPT" | "DEBUG" | "NO_WARN" then
         for o in options loop
            if given = o'Image then
               options_set := options_set or option_bit(o);
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
      Put_Line(Standard_Error, "where option is one of: LABELS, NO_OPT, DEBUG, and NONE");
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

end kidopt;