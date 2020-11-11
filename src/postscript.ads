-- postscript.ads
--
-- Elementary Encapsulated PostScript (EPS) line drawing.
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

with IO;

package postscript is

   -- Open the PostScript file and write the prelude, with a placeholder for the bounds.
   procedure initialize_PostScript_output (the_GP_stream : in out IO.Stream);

   -- Close the PostScript file, having gone back to overwrite the bounding box placeholders.
   procedure finalize_PostScript_output;

   type pen_colour is (
                       Black,
                       Blue,
                       Brown,
                       Cyan,
                       Dark_Blue,
                       Dark_Cyan,
                       Dark_Green,
                       Dark_Grey,
                       Dark_Magenta,
                       Dark_Red,
                       Green,
                       Grey,
                       Magenta,
                       Red,
                       White,
                       Yellow
                      );

   the_default_colour : constant pen_colour := Black;

   type pen_tip_size is (
                         Extra_Extra_Fine,
                         Extra_Fine,
                         Fine,
                         Medium,
                         Medium_Broad,
                         Broad,
                         Extra_Broad
                        );

   the_default_pen_tip : constant pen_tip_size := Extra_Extra_Fine;

   -- Choose the pen's colour and tip size.
   procedure set_the_pen_properties (this_colour  : in pen_colour   := the_default_colour;
                                     this_pen_tip : in pen_tip_size := the_default_pen_tip);

   -- Drawing is done in terms of the plotter's co-ordinate system.
   -- (0, 0) is the top left point of the drawing,
   -- The x axis increases down the plot (long axis, direction of paper movement),
   --    and the y axis increases across the plot (short axis, direction of pen movement).

   type point is
     record
        x, y : Natural;  -- All physically possible co-ordinates are non-negative.
     end record;

   -- Draw a straight line from initial to final.
   procedure draw_a_PostScript_vector (initial, final : in postscript.point);

   -- This exception is raised by:
   --    untimely calls on initialize_PostScript_output and finalize_PostScript_output;
   --       and by drawing at points with infeasibly large co-ordinates.
   -- Before raising the exception the EPS file is finalized, if possible.

   postscript_error : exception;

end postscript;
