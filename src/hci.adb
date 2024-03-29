-- Provide operations supporting replicated human-readable output:
--    1: to an interactive user interface for transient display, and
--    2: to a file for persistent storage.
-- If no file has been opened, or if it has been explicitly closed,
--    output is to the interactive interface only.
--
-- Also provide operations allowing synchronization with the user.
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

with generic_logger;
with settings;

use  settings;

package body HCI is

   package log_manager is new generic_logger(max_logger_list_size => 2);

   cc_list : log_manager.replicator;

   procedure tab_log (at_least : in Natural;
                      spacing  : in Positive := 6;
                      iff      : in Boolean := True) is
   begin
      cc_list.tab_log(at_least, spacing, iff);
   end tab_log;

   procedure tab_log_to (column : in Positive;
                         iff    : in Boolean := True) is
   begin
      cc_list.tab_log_to(column, iff);
   end tab_log_to;

   procedure log (char : in Character;
                  iff  : in Boolean := True) is
   begin
      cc_list.log(char, iff);
   end log;

   procedure log (text : in String;
                  iff  : in Boolean := True) is
   begin
      cc_list.log(text, iff);
   end log;

   procedure log_line (text : in String;
                       iff  : in Boolean := True) is
   begin
      if text /= "" then
         cc_list.log(text, iff);
      end if;
      log_new_line(iff);
   end log_line;

   procedure log_new_line (iff : in Boolean := True) is
   begin
      cc_list.log_new_line(iff);
   end log_new_line;

   procedure log_rule (start_a_new_line : in Boolean := False;
                       iff              : in Boolean := True) is
   begin
       if start_a_new_line then
          cc_list.log_new_line(iff);
       end if;
       log_line(String'(1..80 => '_'), iff);
   end log_rule;

   procedure log_message (message : in String) is
   begin
      cc_list.log(message);
      cc_list.log_new_line;
   end log_message;

   procedure log_title (message : in String) is
   begin
      cc_list.log_new_line;
      cc_list.log(message);
      cc_list.log_new_line;
   end log_title;

   procedure log_ee9_status (message  : in String;
                             skip     : in Natural := 0;
                             complete : in Boolean := True;
                             iff      : in Boolean := True) is
   begin
      if not iff then return; end if;
      panel_logger.tab_log_to(1);
      for i in 1 .. skip loop
         log_new_line;
      end loop;
      if complete then
         log_line("ee9: " & message & ".");
      else
         log("ee9: " & message);
      end if;
   end log_ee9_status;

   procedure log_API_message (message  : in String;
                              skip     : in Natural := 1) is
   begin
      if API_logging_is_wanted then
         log_ee9_status(message, skip, True);
      end if;
   end log_API_message;

   procedure show_line (message : in String) is
   begin
      if debugging_is_enabled then
         panel_logger.show_line(message);
         flush;
      end if;
   end show_line;

   procedure interact (reason : in String := "Mode") is
   begin
      panel_logger.interact(reason);
   end interact;

   procedure open (logfile_name : in String) is
   begin
      cc_list.open(logfile_name);
   end open;

   procedure close is
   begin
      cc_list.close;
   end close;

   procedure flush (iff : in Boolean := True) is
   begin
      cc_list.flush(iff);
   end flush;

   procedure log_to_file (message : in String) is
   begin
      file_logger.log(message);
      file_logger.log_new_line;
   end log_to_file;

begin
   cc_list.set_logger_list((file_logger'Access, panel_logger'Access));
end HCI;
