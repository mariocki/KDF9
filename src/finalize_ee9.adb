-- finalize_ee9.adb
--
-- Shut down processing in preparation for a dignified exit.
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
with HCI;
with IOC;
with state_display;

use  HCI;
use  IOC;
use  state_display;

procedure finalize_ee9 is

   pragma Unsuppress(All_Checks);

begin
   show_final_state;
   finalize_all_KDF9_buffers;
exception
   when error : others =>
      log_line("Failure in ee9: "
             & Ada.Exceptions.Exception_Information(error)
             & " was raised in 'finalize_ee9'!");
end finalize_ee9;
