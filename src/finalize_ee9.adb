-- Shut down processing in preparation for a dignified exit.
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

with Ada.Exceptions;
--
with HCI;
with IOC;
with logging.file;
with logging.panel;
with settings;
with state_display;

use  HCI;
use  IOC;
use  logging.file;
use  logging.panel;
use  settings;
use  state_display;

procedure finalize_ee9 (because : in String := "") is
   core_file_name : constant String := "pascal_core";
   reason         : constant String := (if because = "" then "Normal end of run" else because);
begin
   show_final_state(reason);
   finalize_all_KDF9_buffers;
   if core_file_is_enabled and because /= "" then
      -- Make a final state dump for POST.
      close(panel_logger);
      close(file_logger);
      open(file_logger, core_file_name);
      show_final_state(reason);
      save_core_image;
      close(file_logger);
   end if;
exception
   when error : others =>
      log_line("Failure: " & Ada.Exceptions.Exception_Information(error));
end finalize_ee9;
