-- postscript.adb
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

package body postscript is

   pragma Unsuppress(All_Checks);

   use IO;

   the_PostScript_stream_access : access IO.stream;

   procedure ensure_separation is
   begin
      if column(the_PostScript_stream_access.all) > 0 then
         put_byte(' ', the_PostScript_stream_access.all);
      end if;
   end ensure_separation;

   procedure put_fragment (PS_fragment : String) is
   begin
      put_bytes(PS_fragment, the_PostScript_stream_access.all);
   end put_fragment;

   procedure put_separate_fragment (PS_fragment : String) is
   begin
      ensure_separation;
      put_fragment(PS_fragment);
   end put_separate_fragment;

   procedure put_fragment_and_a_new_line (PS_fragment : String) is
   begin
      put_fragment(PS_fragment);
      put_EOL(the_PostScript_stream_access.all);
   end put_fragment_and_a_new_line;

   procedure put_separate_fragment_and_a_new_line (PS_fragment : String) is
   begin
      ensure_separation;
      put_fragment(PS_fragment);
      put_EOL(the_PostScript_stream_access.all);
   end put_separate_fragment_and_a_new_line;

   procedure put_integer_fragment (i : Integer) is
      image  : constant String := Integer'Image(i);
   begin
      ensure_separation;
      if image(image'First) /= ' ' then
         put_fragment(image);
      else  -- Suppress the nuisance blank character.
         put_fragment(image(image'First+1..image'Last));
      end if;
   end put_integer_fragment;

   -- A path is a series of vectors v1, v2, ..., vn such that the last point
   --    of vi is the same as the first point of v(i+1),
   --       and v1, ..., vn are all drawn in the same colour.
   -- The vectors in a path are accumulated, and drawn only when the path is terminated
   --    by a jump to a non-contiguous point or by a change of pen colour.

   there_is_an_open_path      : Boolean := False;
   the_last_point_in_the_path : postscript.point := (0, 0);

   -- The bounding box limits are set from the value of maximum_offset at the end of the plot.
   maximum_offset             : postscript.point := (0, 0);

   procedure terminate_any_open_path is
   begin
      if there_is_an_open_path then
         -- Draw the accumulated strokes.
         put_separate_fragment_and_a_new_line("s");
      end if;
      there_is_an_open_path := False;
   end terminate_any_open_path;

   procedure draw_a_PostScript_vector (initial, final : in postscript.point) is

      function largest_of (a, b, c : Natural) return Natural is
      begin
         return Natural'Max(a, Natural'Max(b, c));
      end largest_of;
      pragma Inline(largest_of);

   begin
      if initial /= the_last_point_in_the_path then
         -- This vector is not contiguous with the previous one.
         terminate_any_open_path;
      end if;
      if initial = final then
         -- This vector is of length 0.
         return;
      end if;
      maximum_offset.x := largest_of(maximum_offset.x, initial.x, final.x);
      maximum_offset.y := largest_of(maximum_offset.y, initial.y, final.y);
      if there_is_an_open_path then
         -- Draw a line to the final point, extending the current path.
         put_integer_fragment(final.x);
         put_integer_fragment(final.y);
         put_separate_fragment_and_a_new_line("l");
      else
         -- Move to the initial point, opening a fresh path, and draw a line to the final point.
         put_integer_fragment(final.x);
         put_integer_fragment(final.y);
         put_integer_fragment(initial.x);
         put_integer_fragment(initial.y);
         put_separate_fragment_and_a_new_line("n");
         there_is_an_open_path := True;
      end if;
      the_last_point_in_the_path := final;
   end draw_a_PostScript_vector;

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

   the_colour  : pen_colour   := the_default_colour;
   the_pen_tip : pen_tip_size := the_default_pen_tip;

   procedure put_the_pen_settings is
   begin
      terminate_any_open_path;
      case the_pen_tip is
         when Extra_Extra_Fine =>
            put_separate_fragment("1.0");
         when Extra_Fine =>
            put_separate_fragment("2.0");
         when Fine =>
            put_separate_fragment("4.0");
         when Medium =>
            put_separate_fragment("6.0");
         when Medium_Broad =>
            put_separate_fragment("8.0");
         when Broad =>
            put_separate_fragment("10.0");
         when Extra_Broad =>
            put_separate_fragment("12.0");
      end case;
      put_separate_fragment_and_a_new_line("setlinewidth");
      put_separate_fragment(gamut(the_colour));
      put_separate_fragment_and_a_new_line("setrgbcolor");
   end put_the_pen_settings;

   procedure set_the_pen_properties (this_colour  : in pen_colour   := the_default_colour;
                                     this_pen_tip : in pen_tip_size := the_default_pen_tip) is
   begin
      the_colour  := this_colour;
      the_pen_tip := this_pen_tip;
      if the_PostScript_stream_access /= null     and then
            is_open(the_PostScript_stream_access.all) then
         put_the_pen_settings;
      end if;
   end set_the_pen_properties;

   -- We eventually seek back to the bounding box parametsrs using this, their file offset.
   the_position_of_the_placeholders : Natural;

   procedure initialize_PostScript_output (the_GP_stream : in out IO.Stream) is
   begin
      if the_PostScript_stream_access /= null then
         finalize_PostScript_output;
         raise postscript_error with "PostScript was already initialized";
      end if;
      the_PostScript_stream_access := the_GP_stream'Unchecked_Access;

      put_fragment_and_a_new_line("%!PS-Adobe-3.0 EPSF-1.0");
      put_separate_fragment("%%BoundingBox: ");

      -- Note the file offset of the bounding box placeholders.
      get_position(the_position_of_the_placeholders, the_PostScript_stream_access.all);

      -- Write the 10-column placeholders.
      put_fragment_and_a_new_line("xxxxxxxxxx|yyyyyyyyyy");

      put_separate_fragment_and_a_new_line("% This graph was plotted by ee9, the GNU Ada KDF9 emulator.");
      put_separate_fragment_and_a_new_line("% For more information, see <http://www.findlayw.plus.com/KDF9>.");
      put_separate_fragment_and_a_new_line("save");

      put_separate_fragment_and_a_new_line("1 setlinecap");
      put_separate_fragment_and_a_new_line("1 setlinejoin");

      put_the_pen_settings;

      put_separate_fragment_and_a_new_line("0 792 translate");  -- Assumes a page of length 11"!

      -- The plotter step was 0.005", which is the same as 0.36 PostScript points.
      -- The scaling factor is set here to make the wabbit example fit an A4 page.
      put_separate_fragment_and_a_new_line("0.12 -0.12 scale");

      put_separate_fragment_and_a_new_line("/l { lineto } bind def");
      put_separate_fragment_and_a_new_line("/n { newpath moveto lineto } bind def");
      put_separate_fragment_and_a_new_line("/s { stroke } bind def");

      put_separate_fragment_and_a_new_line("save");
   end initialize_PostScript_output;

   procedure finalize_PostScript_output is

      subtype bound_string is String(1..10);

      function bound_image (n : in Natural)
      return bound_string is
         n_image : constant String := Natural'Image(n);
         b : bound_string := (others => ' ');
      begin
         if n_image'Length > bound_string'Length then
            raise postscript_error with "infeasible bounding box size";
         else
            b(b'Last-n_image'Length+b'First .. b'Last) := n_image;
            return b;
         end if;
      end bound_image;

   begin
      if the_PostScript_stream_access = null then
         raise postscript_error with "PostScript was already finalized";
      end if;
      terminate_any_open_path;
      put_separate_fragment_and_a_new_line("showpage");
      put_separate_fragment_and_a_new_line("restore");
      put_separate_fragment_and_a_new_line("restore");
      put_separate_fragment_and_a_new_line("% End of plot");

      -- Go back to the bounding box placeholders in the output file.
      set_position(the_position_of_the_placeholders, the_PostScript_stream_access.all);

      -- Overwrite them with the actual x and y co-ordinate bounds.
      put_fragment(bound_image(maximum_offset.x));
      put_fragment(" ");
      put_fragment(bound_image(maximum_offset.y));

      close(the_PostScript_stream_access.all);
      the_PostScript_stream_access := null;
   exception
      when others =>
         close(the_PostScript_stream_access.all);
         the_PostScript_stream_access := null;
         raise;
   end finalize_PostScript_output;

end postscript;
