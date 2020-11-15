-- exceptions.ads
--
-- Declare the exceptions used in emulation-mode control.
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

package exceptions is

   -- program_exit is raised when a KDF9 program terminates.
   program_exit        : exception;

   -- time_expired is raised when a KDF9 program executes too many instructions.
   time_expired        : exception;

   -- NYI_trap is raised by a KDF9 feature that is Not Yet Implemented.
   NYI_trap            : exception;

   -- emulation_failure is raised when the emulator discovers it has gone wrong.
   emulation_failure   : exception;

   -- Director_failure is raised when a failure is detected in Director mode.
   Director_failure    : exception;

   -- quit_request is raised when the user requests a quit at a break-in.
   quit_request        : exception;

   -- input_is_impossible is raised when an attempt is made to read from the terminal in
   --    noninteractive mode.  This prevents absent-user scripted usage from hanging.
   input_is_impossible : exception;

end exceptions;
