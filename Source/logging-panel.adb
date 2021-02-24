-- Provide logging output to an interactive terminal/control panel.
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

with POSIX;
with settings;

use  POSIX;
use  settings;

package body logging.panel is

   not overriding
   function column (logger : panel.display)
   return Positive
   is (logger.column_number);

   overriding
   procedure tab_log (logger   : in out panel.display;
                      at_least : in Natural;
                      spacing  : in Positive;
                      iff      : in Boolean := True) is
      new_col : constant Natural := logger.column_number + at_least;
      deficit : constant Natural := (spacing - new_col mod spacing) mod spacing;
   begin
      if not iff then return; end if;
      for i in logger.column_number .. (new_col + deficit) loop
         POSIX.output(' ');
      end loop;
      logger.column_number := new_col + deficit;
   end tab_log;

   overriding
   procedure tab_log_to (logger : in out panel.display;
                         column : in Positive;
                         iff    : in Boolean := True) is
   begin
      if not iff then return; end if;
      if column < logger.column_number then
         logger.log_new_line;
      end if;
      for i in logger.column_number .. column-1 loop
         POSIX.output(' ');
      end loop;
      logger.column_number := column;
   end tab_log_to;

   overriding
   procedure log (logger : in out panel.display;
                  char   : in Character;
                  iff    : in Boolean := True) is
   begin
      if not iff then return; end if;
      POSIX.output(char);
      logger.column_number := logger.column_number + 1;
   end log;

   overriding
   procedure log (logger : in out panel.display;
                  text   : in String;
                  iff    : in Boolean := True) is
   begin
      if not iff then return; end if;
      if text /= "" then
         POSIX.output(text);
      end if;
      logger.column_number := logger.column_number + text'Length;
   end log;

   overriding
   procedure log_new_line (logger : in out panel.display;
                           iff    : in Boolean := True) is
   begin
      if not iff then return; end if;
      POSIX.output_line;
      logger.column_number := 1;
   end log_new_line;

   not overriding
   procedure show (logger : in out panel.display; message : in String := "") is
   begin
      if message /= "" then
         logger.log(message);
      end if;
   end show;

   not overriding
   procedure show_line (logger : in out panel.display; message : in String := "") is
   begin
      if message /= "" then
         logger.log(message);
      end if;
      logger.log_new_line;
   end show_line;

   not overriding
   procedure interact (logger : in out panel.display; reason : in String := "Mode") is
      old_mode : constant settings.diagnostic_mode := the_diagnostic_mode;
      response : response_kind;
      choice   : Character;
   begin
   interaction_loop:
      loop
         logger.column_number := 1;
         POSIX.debug_prompt(noninteractive_usage_is_enabled, reason, response, choice);
         if response = name_response then
            case choice is
               when 'q' | 'Q' =>
                  quit_was_requested := True;
                  exit interaction_loop;
               when 'd' | 'D' =>
                  debugging_is_enabled := not debugging_is_enabled;
                  exit interaction_loop;
               when 'f' | 'F' =>
                  set_diagnostic_mode(fast_mode);
                  exit interaction_loop;
               when 'p' | 'P' =>
                  set_diagnostic_mode(pause_mode);
                  exit interaction_loop;
               when 't' | 'T' =>
                  set_diagnostic_mode(trace_mode);
                  exit interaction_loop;
               when others =>
                  null; -- An invalid choice, try again.
            end case;
         elsif response = EOF_response then
            exit;
         end if;
      end loop interaction_loop;
      the_diagnostic_mode_changed := (the_diagnostic_mode /= old_mode) or quit_was_requested;
   end interact;

end logging.panel;
