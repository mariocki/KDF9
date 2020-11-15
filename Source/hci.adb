-- HCI.adb
--
-- Provide operations supporting replicated human-readable output:
--    1: to an interactive user interface for transient display, and
--    2: to a file for persistent storage.
-- If no file has been opened, or if it has been explicitly closed,
--    output is to the interactive interface only.
--
-- Also provide operations allowing synchronization with the user.
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

with formatting;
with generic_logger; pragma Elaborate_All(generic_logger);
with Latin_1;
with settings;

use  formatting;
use  Latin_1;
use  settings;

package body HCI is

   pragma Unsuppress(All_Checks);

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

   procedure log_padded_string (text  : in String;
                                width : in Positive := 1) is
      pad_width   : constant Natural := Integer'Max (0, width - text'Length);
      padding     : constant String (1 .. pad_width) := (others => ' ');
      padded_text : constant String := padding & text;
   begin
      cc_list.log(padded_text);
   end log_padded_string;

   procedure log_octal (number : in KDF9.field_of_16_bits;
                        width  : in Positive := 1) is
   begin
      log_padded_string("#" & oct_of(number), width);
   end log_octal;

   procedure log_octal (number : in KDF9.word;
                        width  : in Positive := 1) is
   begin
      log_padded_string("#" & oct_of(number), width);
   end log_octal;

   procedure log_new_line (iff : in Boolean := True) is
   begin
      cc_list.log_new_line(iff);
   end log_new_line;

   half_ruler : constant String (1 .. 36) := (others => '_');
   half_blank : constant String (1 .. 36) := (others => ' ');
   full_ruler : constant String (1 .. 72) := half_ruler & half_ruler;

   procedure log_rule (start_a_new_line : in Boolean := False;
                       iff              : in Boolean := True) is
   begin
      if start_a_new_line then
         cc_list.log_new_line(iff);
      end if;
      log_line(full_ruler, iff);
   end log_rule;

   procedure log_rule_half (second_half : in Boolean := False) is
   begin
      if second_half then
         log(half_blank);
      end if;
      log_line(half_ruler);
   end log_rule_half;

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

   procedure log_error_message (message : in String) is
   begin
      log_line("Error: " & message);
   end log_error_message;

   procedure log_ee9_status (message  : in String;
                             skip     : in Natural := 1;
                             complete : in Boolean := True;
                             iff      : in Boolean := True) is
   begin
      if not iff then return; end if;
      for i in 1 .. skip loop
         log_new_line;
      end loop;
      if complete then
         log_line("ee9: " & message & ".");
      else
         log("ee9: " & message);
      end if;
   end log_ee9_status;

   procedure hoot (message : in String := "") is
   begin
      panel_logger.log(message & BEL);
   end hoot;

   procedure show (message : in String) is
   begin
      if debugging_is_enabled then
         panel_logger.show(message);
         flush;
      end if;
   end show;

   procedure show_line (message : in String) is
   begin
      if debugging_is_enabled then
         panel_logger.show_line(message);
         flush;
      end if;
   end show_line;

   procedure respond_to_prompt (prompt   : in String;
                                response : out Character) is
   begin
      panel_logger.respond_to_prompt(prompt, response);
   end respond_to_prompt;

   procedure continue_when_GO_is_pressed (caption : in String := "") is
   begin
      panel_logger.continue_when_GO_is_pressed(caption);
   end continue_when_GO_is_pressed;

   procedure open (logfile_name : in String) is
   begin
      cc_list.open(logfile_name);
   end open;

   procedure close (logfile_name : in String) is
   begin
      cc_list.close(logfile_name);
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
