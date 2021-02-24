-- Provide logging output to a named text file.
--
-- This file is part of ee9 (6.1a), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
--
with file_interfacing;

use  Ada.Text_IO;
--
use  file_interfacing;

package body logging.file is

   overriding
   procedure tab_log (logger   : in out file.output;
                      at_least : in Natural;
                      spacing  : in Positive;
                      iff      : in Boolean := True) is
      column_nr : constant Positive_Count := Col(logger.the_log.all) + Count(at_least);
      excess    : constant Count          := column_nr mod Count(spacing);
   begin
      if not iff or logger.log_file_is_shut then return; end if;
      Set_Col(logger.the_log.all, column_nr);
      if excess /= 0 then
         Set_Col(logger.the_log.all, column_nr + Count(spacing) - excess);
      end if;
   end tab_log;

   overriding
   procedure tab_log_to (logger : in out file.output;
                         column : in Positive;
                         iff    : in Boolean := True) is
   begin
      if not iff or logger.log_file_is_shut then return; end if;
      Set_Col(logger.the_log.all, Positive_Count(column));
   end tab_log_to;

   overriding
   procedure log_new_line (logger : in out file.output;
                           iff    : in Boolean := True) is
   begin
      if not iff or logger.log_file_is_shut then return; end if;
      New_Line(logger.the_log.all);
   end log_new_line;

   overriding
   procedure log (logger : in out file.output;
                  char   : in Character;
                  iff    : in Boolean := True) is
   begin
      if not iff or logger.log_file_is_shut then return; end if;
      Put(logger.the_log.all, char);
   end log;

   overriding
   procedure log (logger : in out file.output;
                  text   : in String;
                  iff    : in Boolean := True) is
   begin
      if not iff or logger.log_file_is_shut then return; end if;
      Put(logger.the_log.all, text);
   end log;

   overriding
   procedure open (logger : in out file.output; logfile_name : in String) is
   begin
      if logger.log_file_is_shut then
         logger.the_log := new Ada.Text_IO.File_Type;
         file_interfacing.initialize(logger.the_log.all, out_file, logfile_name);
         logger.log_file_is_shut := False;
      end if;
   end open;

   overriding
   procedure close (logger : in out file.output; logfile_name : in String) is

      procedure free_log_file is
         new Ada.Unchecked_Deallocation(Ada.Text_IO.File_Type, File_Type_access);

   begin
      if logger.log_file_is_shut then return; end if;
      file_interfacing.finalize(logger.the_log.all, logfile_name);
      free_log_file(logger.the_log);
      logger.log_file_is_shut := True;
   end close;

   overriding
   procedure flush (logger : in out file.output; iff : in Boolean := True) is
   begin
      if not iff or logger.log_file_is_shut then return; end if;
      Flush(logger.the_log.all);
   end flush;

end logging.file;
