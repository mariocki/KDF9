-- Elementary Encapsulated PostScript (EPS) line drawing.
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

package body postscript is

   use host_IO;

   -- A path is a series of vectors v1, v2, ..., vn such that the last point
   --    of vi is the same as the first point of v(i+1),
   --       and v1, ..., vn are all drawn in the same colour.
   -- A new path is started by a jump to a non-contiguous point or by a change of pen colour.

   there_is_an_open_path      : Boolean := False;
   the_last_point_in_the_path : postscript.point := (0, 0);

   -- The bounding box limits are set from the value of maximum_offset at the end of the plot.
   maximum_offset             : postscript.point := (0, 0);

   procedure ensure_separation (stream : in out host_IO.stream) is
   begin
      if column(stream) > 0 then
         put_byte(' ', stream);
      end if;
   end ensure_separation;

   procedure put (stream : in out host_IO.stream; PS_text : String) is
   begin
      put_bytes(PS_text, stream);
   end put;

   procedure put_unit (stream : in out host_IO.stream; PS_text : String) is
   begin
      ensure_separation(stream);
      put(stream, PS_text);
   end put_unit;

   procedure put_line (stream : in out host_IO.stream; PS_text : String) is
   begin
      put(stream, PS_text);
      put_EOL(stream);
   end put_line;

   procedure put_unit_line (stream : in out host_IO.stream; PS_text : String) is
   begin
      put_unit(stream, PS_text);
      put_EOL(stream);
   end put_unit_line;

   procedure put_integer (stream : in out host_IO.stream; i : Integer) is
      integer_image  : constant String := i'Image;
   begin
      ensure_separation(stream);
      if integer_image(integer_image'First) /= ' ' then
         put(stream, integer_image);
      else  -- Suppress the nuisance blank character.
         put(stream, integer_image(integer_image'First+1..integer_image'Last));
      end if;
   end put_integer;

   procedure terminate_any_open_path (stream : in out host_IO.stream) is
   begin
      if there_is_an_open_path then
         -- Draw the accumulated strokes.
         put_unit_line(stream, "s");
      end if;
      there_is_an_open_path := False;
   end terminate_any_open_path;

   procedure draw_a_PS_vector (stream : in out host_IO.stream;
                               initial,
                               final  : in postscript.point) is

      function largest_of (a, b, c : Natural)
      return Natural
      is (Natural'Max(a, Natural'Max(b, c)));

   begin -- draw_a_PS_vector
      if initial /= the_last_point_in_the_path then
         -- This vector is not contiguous with the previous one.
         terminate_any_open_path(stream);
      end if;
      if initial = final then
         -- This vector is of length 0.
         return;
      end if;
      maximum_offset.x := largest_of(maximum_offset.x, initial.x, final.x);
      maximum_offset.y := largest_of(maximum_offset.y, initial.y, final.y);
      if there_is_an_open_path then
         -- Draw a line to the final point, extending the current path.
         put_integer(stream, final.x);
         put_integer(stream, final.y);
         put_unit_line(stream, "l");
      else
         -- Move to the initial point, opening a fresh path, and draw a line to the final point.
         put_integer(stream, final.x);
         put_integer(stream, final.y);
         put_integer(stream, initial.x);
         put_integer(stream, initial.y);
         put_unit_line(stream, "n");
         there_is_an_open_path := True;
      end if;
      the_last_point_in_the_path := final;
   end draw_a_PS_vector;

   subtype RGB is String(1..11);
   gamut : constant array (pen_colour) of RGB
         := (
               Black          => ".00 .00 .00",
               Blue           => ".00 .00 1.0",
               Brown          => ".60 .20 .00",
               Cyan           => ".00 1.0 1.0",
               Dark_Blue      => ".10 .10 .80",
               Dark_Cyan      => ".20 .80 1.0",
               Dark_Green     => ".00 .60 .40",
               Dark_Grey      => ".50 .50 .50",
               Dark_Magenta   => ".75 .25 .75",
               Dark_Red       => ".75 .00 .00",
               Green          => ".00 1.0 .00",
               Grey           => ".80 .80 .80",
               Magenta        => "1.0 .00 1.0",
               Red            => "1.0 .00 .00",
               White          => "1.0 1.0 1.0",
               Yellow         => "1.0 1.0 .00"
            );

   subtype tip_breadth is String(1..4);
   breadth : constant array (pen_tip_size) of tip_breadth
           := (
               Extra_Extra_Fine => "1.00",
               Extra_Fine       => "2.00",
               Fine             => "4.00",
               Medium           => "6.00",
               Medium_Broad     => "8.00",
               Broad            => "10.0",
               Extra_Broad      => "12.0"
              );

   the_colour   : pen_colour   := the_default_colour;
   the_pen_size : pen_tip_size := the_default_tip_size;

   procedure put_the_pen_settings (stream : in out host_IO.Stream) is
   begin -- put_the_pen_settings
      terminate_any_open_path(stream);
      put_unit(stream, gamut(the_colour));
      put_unit_line(stream, "setrgbcolor");
      put_unit(stream, breadth(the_pen_size));
      put_unit_line(stream, "setlinewidth");
   end put_the_pen_settings;

   procedure set_the_pen_properties (this_colour   : in pen_colour   := the_default_colour;
                                     this_pen_size : in pen_tip_size := the_default_tip_size) is
   begin -- set_the_pen_properties
      the_colour := this_colour;
      the_pen_size := this_pen_size;
   end set_the_pen_properties;

   -- We eventually seek back to the bounding box parametsrs using this, their file offset.
   the_position_of_the_placeholders : Natural;

   procedure initialize_PS_output (stream : in out host_IO.Stream) is

   begin -- initialize_PS_output
      put_line(stream, "%!PS-Adobe-3.0 EPSF-1.0");
      put_unit(stream, "%%BoundingBox: ");

      -- Note the file offset of the bounding box placeholders.
      get_position(the_position_of_the_placeholders, stream);

      -- Write the 12-column placeholders.
      put_line(stream, "xxxxxxxxxxxx|yyyyyyyyyyyy");

      put_line(stream, "% This graph was plotted by ee9, the GNU Ada KDF9 emulator.");
      put_line(stream, "% For more information, see <http://www.findlayw.plus.com/KDF9>.");
      put_line(stream, "save");

      put_line(stream, "1 setlinecap");
      put_line(stream, "1 setlinejoin");

      put_the_pen_settings(stream);

      put_line(stream, "0 792 translate");  -- Assumes a page of length 11"!

      -- The plotter step was 0.005", which is the same as 0.36 PostScript points.
      -- The scaling factor is set here to make the wabbit example fit an A4 page.
      put_line(stream, "0.12 -0.12 scale");

      put_line(stream, "/l { lineto } bind def");
      put_line(stream, "/n { newpath moveto lineto } bind def");
      put_line(stream, "/s { stroke } bind def");

      put_line(stream, "save");
   end initialize_PS_output;

   procedure finalize_PS_output (stream : in out host_IO.Stream) is

      subtype bound_string is String(1..12);

      function bound_image (n : in Natural)
      return bound_string is
         n_image : constant String := n'Image;
      begin
         return b : bound_string := (others => ' ') do
            b(b'Last-n_image'Length+1 .. b'Last) := n_image;
         end return;
      end bound_image;

   begin -- finalize_PS_output
      terminate_any_open_path(stream);
      put_line(stream, "showpage");
      put_line(stream, "restore");
      put_line(stream, "restore");
      put_line(stream, "% End of plot");

      -- Go back to the bounding box placeholders in the output file.
      set_position(the_position_of_the_placeholders, stream);

      -- Overwrite them with the actual x and y co-ordinate bounds.
      put(stream, bound_image(maximum_offset.x));
      put(stream, " ");
      put(stream, bound_image(maximum_offset.y));

      close(stream);
   end finalize_PS_output;

end postscript;

