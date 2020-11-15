-- plotter.adb
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

with postscript; pragma Elaborate_All(postscript);

use  postscript;

package body plotter is

   pragma Unsuppress(All_Checks);

   -- The plotter made equal movements in the x and y directions, in quanta of 0.005 inches.
   -- Each command moves the plotting position by at most 1 quantum,
   --   in either the positive or negative direction of each axis.

   subtype step_change is Integer range -1 .. +1;

   type step is
      record
         dx, dy : step_change;
      end record;

   function "+" (p : postscript.point; s : plotter.step)
   return postscript.point is
   begin
      return (p.x + s.dx, p.y + s.dy);
   end "+";

   function "-" (p, q : postscript.point)
   return plotter.step is
   begin
      return (p.x - q.x, p.y - q.y);  -- Consecutive points are at most one unit apart.
   end "-";

   -- The plotter drew on a roll of paper 29.5 inches wide and 120 feet long.
   -- 29.5" is   5900 steps at 200 steps per inch = 59 * 100
   -- 120'  is 288000 steps  = 200 per inch * 12 inches per foot * 120 feet.
   -- This sets the boundaries of the plot.
   -- It was physically impossible to move to a point beyond these limits.

   plot_limit : constant postscript.point := (120*12*200, 59*100);

   -- A vector is represented by a series of consecutive colinear plotter movements.
   -- For better efficiency, the steps of a vector are accumulated until there is
   --    a change of direction, a pen lift, or the need to close the plotter file.
   -- On these events, any vector thus defined is drawn via a single PostScript command.

   -- last_step retains the direction of the previous plotter step.
   last_step : plotter.step := (0, 0);

   plot_position,
   start_position : postscript.point := (+0, +0);

   the_pen_is_down : Boolean := False;

   procedure close_any_open_vector is
   begin
      if the_pen_is_down                and then
            start_position /= plot_position then
         draw_a_PostScript_vector(start_position, plot_position);
         start_position := plot_position;
      end if;
   end close_any_open_vector;

   procedure draw_to (p : in postscript.point);
   pragma Inline(draw_to);

   procedure draw_to (p : in postscript.point) is
   begin
      if (plot_position - p) = last_step then
         -- p is colinear with the previous step, so merely extend the vector to p.
         plot_position := p;
      else
         -- Draw the whole vector and start a new one.
         draw_a_PostScript_vector(start_position, plot_position);
         last_step := plot_position - p;
         start_position := plot_position;
         plot_position := p;
      end if;
   end draw_to;

   procedure jump_to (p : in postscript.point);
   pragma Inline(jump_to);

   procedure jump_to (p : in postscript.point) is
   begin
      -- Posit a new vector starting at p.
      last_step := (0, 0);
      plot_position := p;
      start_position := p;
   end;

   procedure ensure_the_validity_of (this  : in postscript.point;
                                     after : in plotter.step) is
   begin
      if this.x + after.dx < 0                     or else
            this.y + after.dy < 0                  or else
               this.x + after.dx > plot_limit.x    or else
                  this.y + after.dy > plot_limit.y    then
         raise plotting_error
            with "movement to an impossible position from ("
                & this.x'Img
                & this.y'Img
                & ") by ("
                & after.dx'Img
                & after.dy'Img
                & ")"
                ;
      end if;
   end ensure_the_validity_of;

   procedure draw_by (this_step : in plotter.step);
   pragma Inline(draw_by);

   procedure draw_by (this_step : in plotter.step) is
   begin
      ensure_the_validity_of(this => plot_position, after => this_step);
      draw_to(plot_position + this_step);
   exception
      when plotting_error =>
         null;
   end draw_by;

   procedure jump_by (this_step : in plotter.step);
   pragma Inline(jump_by);

   procedure jump_by (this_step : in plotter.step) is
   begin
      ensure_the_validity_of(this => plot_position, after => this_step);
      jump_to(plot_position + this_step);
   exception
      when plotting_error =>
         null;
   end jump_by;

   procedure move_by (this_step : in plotter.step);
   pragma Inline(move_by);

   procedure move_by (this_step : in plotter.step) is
   begin
      -- Convert from natural orientation of X axis to PostScript direction.
      if the_pen_is_down then
         draw_by((-this_step.dx, +this_step.dy));
      else
         jump_by((-this_step.dx, +this_step.dy));
      end if;
   end move_by;

   procedure perform (symbol : in plotter.command) is
   begin
      case symbol is
         when dummy =>
            null;
         when pen_up =>
            close_any_open_vector;
            the_pen_is_down := False;
         when pen_down =>
            the_pen_is_down := True;
         when go_pY =>
            move_by((+0, +1));
         when go_nY =>
            move_by((+0, -1));
         when go_pX =>
            move_by((+1, +0));
         when go_nX =>
            move_by((-1, +0));
         when go_pXpY =>
            move_by((+1, +1));
         when go_nXnY =>
            move_by((-1, -1));
         when others =>
            close_the_plot_file;
            raise plotting_error with "invalid plotter command";
      end case;
   end perform;

   a_plot_is_open : Boolean := False;

   procedure open_the_plot_file is
   begin
      if a_plot_is_open then
         close_the_plot_file;
         raise plotting_error with "the plotter was already open";
      end if;
      plot_position := (0,0);
      a_plot_is_open := True;
   end open_the_plot_file;

   procedure close_the_plot_file is
   begin
      if not a_plot_is_open then
         raise plotting_error with "the plotter was already closed";
      end if;
      close_any_open_vector;
      a_plot_is_open := False;
   end close_the_plot_file;

end plotter;
