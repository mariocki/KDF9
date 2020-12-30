-- HCI.ads
--
-- Provide operations supporting replicated human-readable output:
--    1: to an interactive user interface for transient display, and
--    2: to a file for persistent storage.
-- If no file has been opened, or if it has been explicitly closed,
--    output is to the interactive interface only.
--
-- Also provide operations allowing synchronization with the user.
--
-- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2020, W. Findlay; all rights reserved.
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

with KDF9;
with logging.file;
with logging.panel;

use  KDF9;
use  logging.file;
use  logging.panel;

package HCI is

   file_logger  : aliased logging.file.output;
   panel_logger : aliased logging.panel.display;

   procedure tab_log (at_least : in Natural;
                      spacing  : in Positive := 6;
                      iff      : in Boolean := True);

   procedure tab_log_to (column : in Positive;
                         iff    : in Boolean := True);

   procedure log (char : in Character;
                  iff  : in Boolean := True);

   procedure log (text : in String;
                  iff  : in Boolean := True);

   procedure log_line (text : in String;
                       iff  : in Boolean := True);

   -- Log in octal with initial '#'.
   procedure log_octal (number : in KDF9.word;
                        width  : in Positive := 1);

   procedure log_octal (number : in KDF9.field_of_16_bits;
                        width  : in Positive := 1);

   procedure log_new_line (iff : in Boolean := True);

   procedure log_rule (start_a_new_line : in Boolean := False;
                       iff              : in Boolean := True);

   procedure log_rule_half (second_half : in Boolean := False);

   procedure log_message (message : in String);

   procedure log_title (message : in String);

   procedure log_ee9_status (message  : in String;
                             skip     : in Natural := 0;
                             complete : in Boolean := True;
                             iff      : in Boolean := True);

   procedure hoot (message : in String := "");

   procedure show (message : in String);

   procedure show_line (message : in String);

   procedure interact (reason : in String := "Mode");

   procedure open  (logfile_name : in String);

   procedure close (logfile_name : in String);

   procedure flush (iff : in Boolean := True);

   procedure log_to_file (message : in String);

end HCI;
