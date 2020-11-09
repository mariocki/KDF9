-- logging-panel.adb
--
-- Provide logging output to an interactive terminal/control panel.
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

with OS_specifics;
with POSIX;
with settings;

use  OS_specifics;
use  POSIX;
use  settings;

package body logging.panel is

   pragma Unsuppress(All_Checks);

   not overriding
   function column (logger : panel.display)
   return Positive is
   begin
      return logger.column_number;
   end column;

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
      POSIX.output(text);
      logger.column_number := logger.column_number + text'Length;
   end log;

   overriding
   procedure log_new_line (logger : in out panel.display;
                           iff    : in Boolean := True) is
   begin
      if not iff then return; end if;
      POSIX.output(EOL);
      logger.column_number := 1;
   end log_new_line;

   not overriding
   procedure show (logger : in out panel.display; message : in String := "") is
   begin
      if message /= "" then
         logger.log_new_line;
         logger.log(message);
      end if;
   end show;

   not overriding
   procedure show_line (logger : in out panel.display; message : in String := "") is
   begin
      if message /= "" then
         logger.log_new_line;
         logger.log(message);
      end if;
      logger.log_new_line;
   end show_line;

   not overriding
   procedure respond_to_prompt (logger   : in out panel.display;
                                prompt   : in String;
                                response : out Character) is
   begin
      POSIX.prompt(prompt, response, default => ' ');
      logger.column_number := 1;
   end respond_to_prompt;

   not overriding
   procedure continue_when_GO_is_pressed (logger  : in out panel.display;
                                          caption : in String := "") is
      prompt   : constant String
               := "Breakpoint:" & caption & " (f:ast | t:race | p:ause or q:uit)? ";
      old_mode : constant settings.diagnostic_mode := the_diagnostic_mode;
      response : Character;
   begin
      loop
         logger.respond_to_prompt(prompt, response);
         case response is
            when 'q' | 'Q' =>
               quit_was_requested := True;
               return;
            when ' ' =>
               exit;
            when 'f' | 'F' =>
               set_diagnostic_mode(fast_mode);
               exit;
            when 'p' | 'P' =>
               set_diagnostic_mode(pause_mode);
               exit;
            when 't' | 'T' =>
               set_diagnostic_mode(trace_mode);
               exit;
            when others =>
               null;
         end case;
      end loop;
      the_diagnostic_mode_changed := (the_diagnostic_mode /= old_mode) or quit_was_requested;
   end continue_when_GO_is_pressed;

end logging.panel;
