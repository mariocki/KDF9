-- map_ctrl_c_to_flex.adb
--
-- Handle user's CTRL-C interrupt; convert it to a KDF9 FLEX (TINT) interrupt.
--
-- This file is part of ee9 (V2.0r), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2015, W. Findlay; all rights reserved.
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

with Ada.Exceptions;
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

procedure map_CTRL_C_to_FLEX is

   pragma Unsuppress(All_Checks);

begin
   log_new_line;
   continue_when_GO_is_pressed;
   quit_if_requested;
   if the_execution_mode = boot_mode then
      signal_interrupt(FLEX_flag);
   else
      show_current_state;
   end if;
   flush;
exception
   when quit_request =>
      log_line("Run stopped by user!");
      finalize_ee9;
      POSIX.exit_program(0);
   when error : others =>
      log_ee9_status("Failure in ee9: "
                    & Ada.Exceptions.Exception_Information(error)
                    & " was raised in 'map_CTRL_C_to_FLEX'!");
      finalize_ee9;
      POSIX.exit_program(1);
end map_CTRL_C_to_FLEX;

