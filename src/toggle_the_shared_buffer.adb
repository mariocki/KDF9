-- toggle_the_shared_buffer.adb
--
-- Switch the shared buffer from TP1 to GP0 if necessary.
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

with HCI;
with IOC.two_shift.GP;
with IOC.two_shift.TP;
with settings;

use  HCI;
use  IOC.two_shift.GP;
use  IOC.two_shift.TP;
use  settings;

procedure toggle_the_shared_buffer is
begin
   if the_graph_plotter_is_configured then
      -- Switch the shared buffer from TP1 to GP0;
      switch_the_shared_buffer_from_TP1;
      switch_the_shared_buffer_onto_GP0;
      if the_log_is_wanted then
         log_new_line;
         log_line("The shared buffer has been switched from TP1 to GP0.");
         log_new_line;
      end if;
   end if;
end toggle_the_shared_buffer;
