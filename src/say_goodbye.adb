-- Finalize emulation with a helpful message derived from exception information.
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

with Ada.Command_Line;
--
with finalize_ee9;
with HCI;
with settings;
with string_editing;

use  Ada.Command_Line;
--
use  HCI;
use  settings;
use  string_editing;

procedure say_goodbye (
                       reason : in String;
                       cause  : in String := "";
                       status : in Exit_Status := Failure
                      ) is

   function explanation
   return String is
      CR   : constant Character := Character'Val(16#D#);
      LF   : constant Character := Character'Val(16#A#);
      L, R : Natural;
   begin  -- explanation
      if cause'Length < 3 then
         return cause;
      end if;
      L := index_forward(cause, "%", start => cause'First) + 1;
      if cause(cause'Last-1) = CR then
         R := cause'Last - 2;
      elsif cause(cause'Last) in LF | CR then
         R := cause'Last - 1;
      else
         R := cause'Last;
      end if;
      return cause(L .. R);
   end explanation;

begin  -- say_goodbye
   if the_log_is_wanted then
      log_new_line;
   end if;
   if reason = "" then
      finalize_ee9;
   elsif reason'Length > 2 and then
         reason(reason'Last-2..reason'Last) = "OUT" then
      finalize_ee9(reason + explanation);
   elsif reason'Length = 0 then
      finalize_ee9(explanation);
   elsif explanation'Length = 0 then
      finalize_ee9(reason);
   else
      finalize_ee9(reason & ": " & explanation);
   end if;
   if the_log_is_wanted then
      log_new_line;
   end if;
   Set_Exit_Status(status);
end say_goodbye;
