-- Provide operations supporting replicated output to a list of logging interfaces.
--
-- This file is part of ee9 (6.2e), the GNU Ada emulator of the English Electric KDF9.
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

with logging;

generic
   max_logger_list_size : in Positive;
package generic_logger is

   type distribution_list is array (Positive range <>) of access logging.output'Class;

   type replicator is new logging.output with private;

   not overriding
   procedure set_logger_list (logger : in out replicator; list : in distribution_list);

   overriding
   procedure tab_log (logger   : in out replicator;
                      at_least : in Natural;
                      spacing  : in Positive;
                      iff      : in Boolean := True);

   overriding
   procedure tab_log_to (logger : in out replicator;
                         column : in Positive;
                         iff    : in Boolean := True);

   overriding
   procedure log (logger : in out replicator;
                  char   : in Character;
                  iff    : in Boolean := True);

   overriding
   procedure log (logger : in out replicator;
                  text   : in String;
                  iff    : in Boolean := True);

   overriding
   procedure log_new_line (logger : in out replicator;
                           iff    : in Boolean := True);

   overriding
   procedure open  (logger : in out replicator; log_name : in String);

   overriding
   procedure close (logger : in out replicator; log_name : in String);

   overriding
   procedure flush (logger : in out replicator; iff : in Boolean := True);

private

   subtype logger_list_size is Natural range 0 .. max_logger_list_size;

   -- This type is needed because tagged types cannot have discriminants.
   type replica_list (length : logger_list_size := 0) is
      record
         list : distribution_list(1 .. length);
      end record;

   type replicator is new logging.output with
      record
         data : replica_list;
      end record;

end generic_logger;
