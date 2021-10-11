-- Emulation of the plotting commands of the Calcomp 564 graph plotter.
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

with host_IO;
with KDF9_char_sets;

use  host_IO;

package plotter is

   procedure open_the_plot_file (stream : in out host_IO.stream);

   procedure close_the_plot_file (stream : in out host_IO.stream);

   type command is new KDF9_char_sets.symbol;

   -- The KDF9 plotting commands are defined in the Manual, Appendix 6, §5.3, pp. 303-304.

   -- BUT there is obviously an error in the Manual, as only 9 of the claimed 11 command
   --    codes are listed, and the last, go_nXnY, is coded inconsistently with the others.

   -- Hypothesis: the table should read as follows:

   dummy    : constant plotter.command := 2#000_000#;

   pen_up   : constant plotter.command := 2#100_000#;
   pen_down : constant plotter.command := 2#010_000#;

   go_pY    : constant plotter.command := 2#001_000#;
   go_nY    : constant plotter.command := 2#000_100#;
   go_pX    : constant plotter.command := 2#000_010#;
   go_nX    : constant plotter.command := 2#000_001#;

   go_nXnY  : constant plotter.command := go_nX + go_nY;
   go_pXnY  : constant plotter.command := go_pX + go_nY;
   go_nXpY  : constant plotter.command := go_nX + go_pY;
   go_pXpY  : constant plotter.command := go_pX + go_pY;

   -- These encodings are consistent with the Calcomp plotter command codes used here:
   --     ub.fnwi.uva.nl/computermuseum//calcomp565.html
   -- which defines a full set of 11 commands, two of which are missing from the KDF9 list.

   is_valid : constant array (plotter.command) of Boolean
            := (dummy    |
                pen_up   |
                pen_down |
                go_pY    |
                go_nY    |
                go_pX    |
                go_nX    |
                go_pXnY  |
                go_nXpY  |
                go_pXpY  |
                go_nXnY  => True,
                others   => False
               );

   procedure perform (action : in plotter.command; stream : in out host_IO.stream);

end plotter;
