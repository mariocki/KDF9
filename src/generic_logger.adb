-- Provide operations supporting replicated output to a list of logging interfaces.
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

-- generic
--    max_logger_list_size : in Positive;
package body generic_logger is

   not overriding
   procedure set_logger_list (logger : in out replicator; list : in distribution_list) is
   begin
      logger.data := (list'Length, list);
   end set_logger_list;

   overriding
   procedure tab_log (logger   : in out replicator;
                      at_least : in Natural;
                      spacing  : in Positive;
                      iff      : in Boolean := True) is
   begin
      for l in logger.data.list'Range loop
         logger.data.list(l).tab_log(at_least, spacing, iff);
      end loop;
   end tab_log;

   overriding
   procedure tab_log_to (logger : in out replicator;
                         column : in Positive;
                         iff    : in Boolean := True) is
   begin
      for l in logger.data.list'Range loop
         logger.data.list(l).tab_log_to(column, iff);
      end loop;
   end tab_log_to;

   overriding
   procedure log (logger : in out replicator;
                  char   : in Character;
                  iff    : in Boolean := True) is
   begin
      for l in logger.data.list'Range loop
         logger.data.list(l).log(char, iff);
      end loop;
   end log;

   overriding
   procedure log (logger : in out replicator;
                  text   : in String;
                  iff    : in Boolean := True) is
   begin
      for l in logger.data.list'Range loop
         logger.data.list(l).log(text, iff);
      end loop;
   end log;

   overriding
   procedure log_new_line (logger : in out replicator;
                           iff    : in Boolean := True) is
   begin
      for l in logger.data.list'Range loop
         logger.data.list(l).log_new_line(iff);
      end loop;
   end log_new_line;

   overriding
   procedure open (logger : in out replicator; log_name : in String) is
   begin
      for l in logger.data.list'Range loop
         logger.data.list(l).open(log_name);
      end loop;
   end open;

   overriding
   procedure close (logger : in out replicator; log_name : in String) is
   begin
      for l in logger.data.list'Range loop
         logger.data.list(l).close(log_name);
      end loop;
   end close;

   overriding
   procedure flush (logger : in out replicator; iff : in Boolean := True) is
   begin
      for l in logger.data.list'Range loop
         logger.data.list(l).flush(iff);
      end loop;
   end flush;

end generic_logger;
