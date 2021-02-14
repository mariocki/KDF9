-- Provide logging output to a named text file.
--
-- This file is part of ee9 (6.0a), the GNU Ada emulator of the English Electric KDF9.
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

private with Ada.Text_IO;

package logging.file is

   type output is new logging.output with private;

   overriding
   procedure tab_log (logger   : in out file.output;
                      at_least : in Natural;
                      spacing  : in Positive;
                      iff      : in Boolean := True);

   overriding
   procedure tab_log_to (logger : in out file.output;
                         column : in Positive;
                         iff    : in Boolean := True);

   overriding
   procedure log (logger : in out file.output;
                  char   : in Character;
                  iff    : in Boolean := True);

   overriding
   procedure log (logger : in out file.output;
                  text   : in String;
                  iff    : in Boolean := True);

   overriding
   procedure log_new_line (logger : in out file.output;
                           iff    : in Boolean := True);

   overriding
   procedure open  (logger : in out file.output; logfile_name : in String);

   overriding
   procedure close (logger : in out file.output; logfile_name : in String);

   overriding
   procedure flush (logger : in out file.output; iff    : in Boolean := True);

private

   type File_Type_access is access Ada.Text_IO.File_Type;

   type output is new logging.output with
      record
         log_file_is_shut : Boolean := True;
         the_log          : file.File_Type_access;
      end record;

end logging.file;
