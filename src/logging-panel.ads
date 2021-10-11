-- Provide logging output to an interactive terminal/control panel.
--
-- This file is part of ee9 (8.1a), the GNU Ada emulator of the English Electric KDF9.
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

package logging.panel is

   type display is new logging.output with private;

   not overriding
   function column (logger : panel.display)
   return Positive;

   overriding
   procedure tab_log (logger   : in out panel.display;
                      at_least : in Natural;
                      spacing  : in Positive;
                      iff      : in Boolean := True);

   overriding
   procedure tab_log_to (logger : in out panel.display;
                         column : in Positive;
                         iff    : in Boolean := True);

   overriding
   procedure log (logger : in out panel.display;
                  char   : in Character;
                  iff    : in Boolean := True);

   overriding
   procedure log (logger : in out panel.display;
                  text   : in String;
                  iff    : in Boolean := True);

   overriding
   procedure log_new_line (logger : in out panel.display;
                           iff    : in Boolean := True);

   not overriding
   procedure show_line (logger : in out panel.display; message : in String := "");

   not overriding
   procedure interact (logger : in out panel.display; reason : in String := "Mode");

   overriding
   procedure open (logger : in out panel.display; logfile_name : in String) is null;

   overriding
   procedure close (logger : in out panel.display; logfile_name : in String) is null;

   overriding
   procedure flush (logger : in out panel.display; iff : in Boolean := True) is null;

private

   type display is new logging.output with
      record
         column_number : Positive := 1;
      end record;

end logging.panel;
