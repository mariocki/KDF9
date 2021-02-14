-- break_in.adb
--
-- This communicates a break-in to the microcode.
--
-- This file is part of ee9 (V5.2b), the GNU Ada emulator of the English Electric KDF9.
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

   requested : Boolean := False
      with Atomic, Volatile;

   procedure note_user_interrupt is
   begin
      if requested then return; end if;  -- The handler is already running.
      requested := True;
   end note_user_interrupt;

   function has_been_requested
   return Boolean is
   begin
      return requested;
   end has_been_requested;

   procedure handler is
   begin
      requested := False;
      interact("Break-in");
      quit_if_requested;
      if the_execution_mode = boot_mode then
         effect(FLEX_interrupt);
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
