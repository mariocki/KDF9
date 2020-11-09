-- plotter.ads
--
-- Emulation of the plotting commands of the Calcomp 564 graph plotter.
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

with KDF9;

use KDF9;

package plotter is

   procedure open_the_plot_file;

   procedure close_the_plot_file;

   type command is new KDF9.symbol;

   -- These are the plotting commands defined in the Manual, Appendix 6, §5.3, pp. 303-304.

   pen_up   : constant plotter.command := 2#100_000#;
   pen_down : constant plotter.command := 2#010_000#;
   go_pXpY  : constant plotter.command := 2#001_010#;
   go_nXnY  : constant plotter.command := 2#001_001#;
   go_pY    : constant plotter.command := 2#001_000#;
   go_nY    : constant plotter.command := 2#000_100#;
   go_pX    : constant plotter.command := 2#000_010#;
   go_nX    : constant plotter.command := 2#000_001#;
   dummy    : constant plotter.command := 2#000_000#;

   is_valid : constant array (plotter.command) of Boolean
            := (dummy    |
                pen_up   |
                pen_down |
                go_pY    |
                go_nY    |
                go_pX    |
                go_nX    |
                go_pXpY  |
                go_nXnY  => True,
                others   => False
               );

   procedure perform (symbol : in plotter.command);

   -- This exception is raised by untimely calls on open_the_plot_file and close_the_plot_file;
   --    by an attempt to move the plotting position to a physically impossible location;
   --       and by a plotter.command value not listed in the Manual.
   -- Before raising the exception, close_the_plot_file is invoked if necessary.

   plotting_error : exception;

end plotter;
