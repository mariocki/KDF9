-- Declare the exceptions used in emulation-mode control.
--
-- This file is part of ee9 (6.2r), the GNU Ada emulator of the English Electric KDF9.
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

package exceptions is

   -- program_exit is raised when a KDF9 program terminates normally.
   program_exit : exception;

  -- OUT_2_restart is raised when a KDF9 program terminates by means of OUT 2.
   OUT_2_restart : exception;

   -- time_expired is raised when a KDF9 program executes too many instructions.
   time_expired : exception;

   -- quit_request is raised when the user requests a quit at a break-in.
   quit_request : exception;

   -- input_is_impossible is raised when an attempt is made to read from the terminal in
   --    noninteractive mode.  This prevents absent-user scripted usage from hanging.
   input_is_impossible : exception;

   -- operator_error is raised when the operating context is invalid; e.g. labelled tape not mounted.
   operator_error : exception;

   -- OUT_error is raised when an impossible OUT action is requested in problem program state.
   OUT_error : exception;

   -- IO_error is raised when an impossible I/O operation is attempted in problem program state.
   IO_error : exception;

   -- Director_IO_error when an impossible I/O operation is attempted in Director state.
   Director_IO_error : exception;

   -- Director_failure is raised for an instruction that would LIV in problem program state.
   Director_failure : exception;

   -- invalid_paper_tape_file is raised when given invalid data for a KDF9-code paper tape file.
   invalid_paper_tape_file : exception;

   -- not_yet_implemented is raised by an incomplete emulation.
   not_yet_implemented : exception;

   -- emulation_failure is raised when an emulator self-check fails.
   emulation_failure : exception;

   -- debugging_stop is raised when a debugging run needs to halt at once.
   debugging_stop : exception;

end exceptions;
