-- Emulation of the plotting commands of the Calcomp 564 graph plotter.
--
-- This file is part of ee9 (8.1x), the GNU Ada emulator of the English Electric KDF9.
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

with IOC.slow.shift.GP;
with postscript;

use  IOC.slow.shift.GP;
use  postscript;

package body plotter is

   -- The plotter made equal movements in the x and y directions, in steps of 0.005 inches.
   -- Each command moves the plotting position by at most 1 step,
   --   in either the positive or negative direction of each axis.

   type step is
      record
         dx, dy : Integer range -1 .. +1;
      end record;

   null_step : constant plotter.step := (0, 0);

   function "+" (p : postscript.point; s : plotter.step)
   return postscript.point
   is ((p.x + s.dx, p.y + s.dy));

   function "-" (p, q : postscript.point)
   return plotter.step
   is ((p.x - q.x, p.y - q.y));

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

   the_origin      : constant postscript.point := (0, 0);

   plot_position,
   start_position  : postscript.point := the_origin;

   -- last_step retains the direction of the previous plotter step.
   last_step       : plotter.step := null_step;

   the_pen_is_down : Boolean := False;

   procedure ensure_the_validity_of (from  : in postscript.point;
                                     step  : in plotter.step) is
   begin
     if from.x + step.dx < 0                     or else
           from.y + step.dy < 0                  or else
              from.x + step.dx > plot_limit.x    or else
                 from.y + step.dy > plot_limit.y    then
        notify_invalid_movement(from.x, from.y, step.dx, step.dy);
     end if;
   end ensure_the_validity_of;

   procedure jump_to (p : in postscript.point)
     with Inline;

   procedure jump_to (p : in postscript.point) is
   begin
     -- Posit a new vector starting at p.
     last_step := null_step;
     plot_position := p;
     start_position := p;
   end;

   procedure jump_by (this_step : in plotter.step)
     with Inline;

   procedure jump_by (this_step : in plotter.step) is
   begin
     ensure_the_validity_of(from => plot_position, step => this_step);
     jump_to(plot_position + this_step);
   end jump_by;

   procedure close_any_open_vector (stream : in out host_IO.stream) is
   begin
     if the_pen_is_down                and then
           start_position /= plot_position then
        draw_a_PS_vector(stream, start_position, plot_position);
        start_position := plot_position;
     end if;
   end close_any_open_vector;

   procedure perform (action : in plotter.command; stream : in out host_IO.stream) is

     procedure draw_to (p : in postscript.point)
        with Inline;

     procedure draw_to (p : in postscript.point) is
     begin
        if (plot_position - p) = last_step then
           -- p is colinear with the previous step, so merely extend the vector to p.
           plot_position := p;
        else
           -- Draw the whole vector and start a new one.
           draw_a_PS_vector(stream, start_position, plot_position);
           last_step := plot_position - p;
           start_position := plot_position;
           plot_position := p;
        end if;
     end draw_to;

     procedure draw_by (this_step : in plotter.step)
        with Inline;

     procedure draw_by (this_step : in plotter.step) is
     begin
        ensure_the_validity_of(from => plot_position, step => this_step);
        draw_to(plot_position + this_step);
     end draw_by;

     procedure move_by (this_step : in plotter.step)
        with Inline;

     procedure move_by (this_step : in plotter.step) is
     begin
        -- Convert from Natural orientation of X axis to PostScript direction.
        if the_pen_is_down then
           draw_by((-this_step.dx, +this_step.dy));
        else
           jump_by((-this_step.dx, +this_step.dy));
        end if;
     end move_by;

   begin -- perform
     case action is
        when dummy =>
           null;
        when pen_up =>
           close_any_open_vector(stream);
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
        when go_pXnY =>
           move_by((+1, -1));
        when go_nXpY =>
           move_by((-1, +1));
        when others =>
           -- EM causes a 'peculiar' motion, according to the Manual, Appendix 5.2, p.303,
           --    and other codes cause 'unpredictable' effects.
           -- ee9 therefore performs an arbitrary, but safe, operation: moving to the origin.
           close_any_open_vector(stream);
           plot_position := the_origin;
     end case;
   end perform;

   a_plot_is_open : Boolean := False;

   procedure open_the_plot_file (stream : in out host_IO.stream) is
   begin
     if a_plot_is_open then
        close_the_plot_file(stream);
     end if;
     plot_position := (0,0);
     a_plot_is_open := True;
   end open_the_plot_file;

   procedure close_the_plot_file (stream : in out host_IO.stream) is
   begin
     if not a_plot_is_open then
        return;
     end if;
     close_any_open_vector(stream);
     a_plot_is_open := False;
   end close_the_plot_file;

end plotter;
