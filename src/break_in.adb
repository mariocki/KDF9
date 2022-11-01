-- This communicates a break-in to the microcode.
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

with exceptions;
with HCI;
with KDF9;
with POSIX;
with finalize_ee9;
with settings;
with state_display;

use  exceptions;
use  HCI;
use  KDF9;
use  settings;
use  state_display;

package body break_in is

   requested : Natural := 0
      with Atomic, Volatile;

   procedure note_user_interrupt is
   begin
      requested := requested + 1;
      if requested > 1 then
         -- A previous interrupt has not been serviced and the user is getting antsy.
         -- Perhaps ee9 itself has gone into an infinite loop, so abandon the run.
         finalize_ee9("Run abandoned by the user");
         POSIX.exit_program(0);
      end if;
   end note_user_interrupt;

   function has_been_requested
   return Boolean is
   begin
      return requested /= 0;
   end has_been_requested;

   procedure handler is
   begin
      requested := 0;
      interact("Break-in");
      quit_if_requested;
      if the_execution_mode = boot_mode then
         effect_interrupt(caused_by_FLEX, "the operator interrupts");
      else
         show_current_state;
      end if;
      flush;
   exception
      when quit_request =>
         finalize_ee9("Quit requested by the user");
         POSIX.exit_program(0);
   end handler;

end break_in;
