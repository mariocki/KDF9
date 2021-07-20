-- Buffered I/O streams to support KDF9 device I/O.
--
-- This file is part of ee9 (7.0a), the GNU Ada emulator of the English Electric KDF9.
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

with KDF9;
with POSIX;

use  KDF9;
use  POSIX;

package host_IO is

   end_of_stream : exception;

   type stream is tagged limited private;

   function fd_of (the_stream : host_IO.stream)
   return Natural;

   -- Open a stream with an established fd.
   procedure open (the_stream : in out host_IO.stream;
                   file_name  : in String;
                   mode       : in POSIX.access_mode;
                   fd         : in Integer);

   -- Open a base file then use its fd to open a stream.
   procedure open (the_stream : in out host_IO.stream;
                   file_name  : in String;
                   mode       : in POSIX.access_mode)
      with Inline => False;

   procedure truncate (the_stream : in out host_IO.stream);

   procedure close (the_stream : in out host_IO.stream);

   procedure flush (the_stream  : in out host_IO.stream;
                    a_byte_time : in KDF9.us := 0);

   -- Reassign an open stream to another file.
   procedure reattach (the_stream : in out host_IO.stream;
                       file_name  : in String;
                       mode       : in POSIX.access_mode);

   function is_open (the_stream : host_IO.stream)
   return Boolean;

   function bytes_moved (the_stream : host_IO.stream)
   return KDF9.word;

   function column (the_stream : host_IO.stream)
   return Natural;

   procedure get_position (position   : out Natural;
                           the_stream : in out host_IO.stream);

   procedure set_position (position   : in Natural;
                           the_stream : in out host_IO.stream;
                           whence     : in POSIX.seek_origin := from_start);

   procedure clear (the_stream : in out host_IO.stream);

   procedure reset (the_stream : in out host_IO.stream);

   -- Arrange for the last-read byte to be read again.
   procedure back_off (the_stream : in out host_IO.stream)
      with Inline;

   procedure get_byte (char       : out Character;
                       the_stream : in out host_IO.stream);

   -- get_bytes iterates get_byte over the_string, for convenience.
   -- If uncounted then the output is not included in the_stream.bytes_moved.
   procedure get_bytes (the_string : out String;
                        the_stream : in out host_IO.stream;
                        uncounted  : in Boolean := True);

   -- True iff the last get_byte obtained a LF.
   function a_LF_was_just_read (the_stream : host_IO.stream)
   return Boolean;

   -- get_char differs from get_byte in the treatment of line terminators.
   -- CR, LF, and CRLF are all returned as a single LF character, so catering
   --    for old MacOS, MSDOS, and macOS/UNIX/Linux external text-file formats.
   procedure get_char (char       : out Character;
                       the_stream : in out host_IO.stream);

   -- peek_at_char uses get_char to inspect the next char to be delivered,
   --    then invokes back_off so that it is left in the input stream.
   procedure peek_at_char (char       : out Character;
                           the_stream : in out host_IO.stream);

   -- do_not_put_byte does the same as put_byte, except for actually writing it to the_stream.
   procedure do_not_put_byte (char       : in Character;
                              the_stream : in out host_IO.stream);

   procedure put_byte (char       : in Character;
                       the_stream : in out host_IO.stream);

   -- put_escape_code writes directly to the stream's device, avoiding the stream's buffers.
   procedure put_escape_code (the_string : in String;
                              the_stream : in out host_IO.stream);

   -- put_bytes iterates put_byte over the_string, for convenience.
   -- If uncounted then the output is not included in the_stream.bytes_moved.
   procedure put_bytes (the_string : in String;
                        the_stream : in out host_IO.stream;
                        uncounted  : in Boolean := True);

   -- put_EOL writes the host-appropriate line terminator (CRLF, or just LF)
   procedure put_EOL (the_stream : in out host_IO.stream;
                      uncounted  : in Boolean := True);

   -- put_char differs from put_byte only in the treatment of line terminators.
   -- If char is LF, then put_EOL is used to output a host-appropriate line terminator.
   procedure put_char (char       : in Character;
                       the_stream : in out host_IO.stream);

   -- put_chars iterates put_bytes over the_string, for convenience.
   procedure put_chars (the_string : in String;
                        the_stream : in out host_IO.stream);

   -- True iff the last put_byte wrote out a LF.
   function a_LF_was_just_written (the_stream : host_IO.stream)
   return Boolean;

   function buffer_is_empty (the_stream : host_IO.stream)
   return Boolean
      with Inline;

   function buffer_is_full (the_stream : host_IO.stream)
   return Boolean
      with Inline;

   -- Return the currently buffered output as a single string.
   function contents (the_stream : host_IO.stream)
   return String;

   -- Make the_string appear to be input for the_stream (which must be empty).
   procedure inject (the_string : in String;
                     the_stream : in out host_IO.stream);

private

   -- N.B. in host_IO the term 'buffer' is used conventionally.
   -- It does NOT refer to a KDF9 DMA channel.

   -- IO_buffer_size is enough for a full LP line, lacking any better criterion.
   IO_buffer_size : constant Positive := 161;

   type stream is tagged limited
      record
         base_name   : String (1 .. 3) := "???";
         is_open     : Boolean := False;
         last_char   : Character := ' ';
         block_size,
         next_byte,
         saved_size,
         position,
         column      : Natural := 0;
         bytes_moved : KDF9.word := 0;
         fd          : Natural := Natural'Last;
         IO_mode     : POSIX.access_mode range read_mode .. rd_wr_mode;
         last_IO     : POSIX.access_mode range read_mode .. write_mode;
         buffer,
         look_behind : String(1 .. IO_buffer_size);
      end record;

end host_IO;
