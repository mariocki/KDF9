-- logging.ads
--
-- Define an abstract log output device.
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

package logging is

   pragma Unsuppress(All_Checks);

   type output is interface;

   procedure tab_log (logger   : in out logging.output;
                      at_least : in Natural;
                      spacing  : in Positive;
                      iff      : in Boolean := True) is abstract;

   procedure tab_log_to (logger : in out logging.output;
                         column : in Positive;
                         iff    : in Boolean := True) is abstract;

   procedure log (logger : in out logging.output;
                  char   : in Character;
                  iff    : in Boolean := True) is abstract;

   procedure log (logger : in out logging.output;
                  text   : in String;
                  iff    : in Boolean := True) is abstract;

   procedure log_new_line (logger : in out logging.output;
                           iff    : in Boolean := True) is abstract;

   procedure open  (logger : in out logging.output; log_name : in String) is abstract;

   procedure close (logger : in out logging.output; log_name : in String) is abstract;

   procedure flush (logger : in out logging.output; iff : in Boolean := True) is abstract;

end logging;
