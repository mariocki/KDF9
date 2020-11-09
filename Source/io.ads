-- io.ads
--
-- Buffered I/O.
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
with POSIX;

use  KDF9;
use  POSIX;

package IO is

   stream_IO_error, end_of_stream : exception;

   type stream is tagged limited private;

   function fd_of (the_stream : IO.stream)
   return Natural;

   function image_of (the_stream : IO.stream;
                      caption    : String := "")
   return String;

   procedure diagnose (the_stream : IO.stream;
                       caption    : String := "");

   -- Open a stream with an established fd.
   procedure open (the_stream : in out IO.stream;
                   file_name  : in String;
                   mode       : in POSIX.access_mode;
                   fd         : in Integer);

   -- Open a base file then use its fd to open a stream.
   procedure open (the_stream : in out IO.stream;
                   file_name  : in String;
                   mode       : in POSIX.access_mode);

   procedure truncate (the_stream : in out IO.stream;
                       to_length  : in KDF9.word := 0);

   procedure close (the_stream : in out IO.stream);

   procedure flush (the_stream  : in out IO.stream;
                    a_byte_time : in KDF9.microseconds := 0);

   -- Reassign an open stream to another file.
   procedure reattach (the_stream : in out IO.stream;
                       file_name  : in String;
                       mode       : in POSIX.access_mode);

   function is_open(the_stream : IO.stream)
   return Boolean;

   function bytes_moved(the_stream : IO.stream)
   return KDF9.word;

   function file_size (the_stream : IO.stream)
   return Natural;

   function column (the_stream : IO.stream)
   return Natural;

   procedure get_position (position   : out Natural;
                           the_stream : in out IO.stream);

   procedure set_position (position   : in Natural;
                           the_stream : in out IO.stream;
                           whence     : in POSIX.seek_origin := from_start);

   procedure reset (the_stream : in out IO.stream);

   procedure back_off (the_stream : in out IO.stream);
   pragma Inline(back_off);

   procedure get_byte (char       : out Character;
                       the_stream : in out IO.stream);

   -- get_bytes iterates get_byte over the_string, for convenience.
   -- If uncounted then the output is not included in the_stream.bytes_moved.
   procedure get_bytes (the_string : out String;
                        the_stream : in out IO.stream;
                        uncounted  : in Boolean := True);

   -- True iff the last get_byte obtained a LF.
   function a_LF_was_just_read (the_stream : IO.stream)
   return Boolean;

   -- get_char differs from get_byte in the treatment of line terminators.
   -- CR, LF, and CRLF are all returned as a single LF character, so catering
   --    for old MacOS, MSDOS, and OS X/UNIX/Linux external text-file formats.
   procedure get_char (char       : out Character;
                       the_stream : in out IO.stream);

   -- peek_at_char uses get_char to inspect the next char to be delivered,
   --    then invokes back_off so that it is left in the input stream.
   procedure peek_at_char (char       : out Character;
                           the_stream : in out IO.stream);

   procedure put_byte (char       : in Character;
                       the_stream : in out IO.stream);

   -- put_bytes iterates put_byte over the_string, for convenience.
   -- If uncounted then the output is not included in the_stream.bytes_moved.
   procedure put_bytes (the_string : in String;
                        the_stream : in out IO.stream;
                        uncounted  : in Boolean := True);

   -- put_EOL writes the host-appropriate line terminator (CRLF, or just LF)
   procedure put_EOL (the_stream : in out IO.stream);

   -- put_char differs from put_byte only in the treatment of line terminators.
   -- If char is LF, then put_EOL is used to output a host-appropriate line terminator.
   procedure put_char (char       : in Character;
                       the_stream : in out IO.stream);

   -- put_chars iterates put_bytes over the_string, for convenience.
   procedure put_chars (the_string : in String;
                        the_stream : in out IO.stream);

   -- True iff the last put_byte wrote out a LF.
   function a_LF_was_just_written (the_stream : IO.stream)
   return Boolean;

   function buffer_is_empty (the_stream : IO.stream)
   return Boolean;
   pragma Inline(buffer_is_empty);

   function buffer_is_full (the_stream : IO.stream)
   return Boolean;
   pragma Inline(buffer_is_full);

   -- Return the last output as a single string.
   function contents (the_stream : IO.stream)
   return String;

   -- Make the_string appear to be input for the_stream (which must be empty).
   procedure inject (the_string : in String;
                     the_stream : in out IO.stream);

private

   -- N.B. in IO the term 'buffer' is used conventionally.
   -- It does NOT refer to a KDF9 DMA channel.

   -- IO_buffer_size is enough for a complete FD sector, lacking any better criterion.
   IO_buffer_size : constant Positive := 320;

   type stream is tagged limited
      record
         base_name   : String (1 .. 3) := "???";
         is_open     : Boolean := False;
         last_char   : Character := ' ';
         block_size,
         next_byte,
         position,
         column      : Natural := 0;
         bytes_moved : KDF9.word := 0;
         fd          : Natural;
         IO_mode     : POSIX.access_mode range read_mode .. rd_wr_mode;
         last_IO     : POSIX.access_mode range read_mode .. write_mode;
         buffer      : String(1 .. IO_buffer_size);
      end record;

end IO;
