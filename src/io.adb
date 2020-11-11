-- io.adb
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

with Latin_1;
with OS_specifics;

use  Latin_1;
use  OS_specifics;

package body IO is

   pragma Unsuppress(All_Checks);

   function fd_of (the_stream : IO.stream)
   return Natural is
   begin
      return the_stream.fd;
   end fd_of;

   function image_of (the_stream : IO.stream;
                      caption    : String := "")
   return String is
   begin
      if the_stream.is_open then
         return caption
              & ": "
              & "base_name = "
              & the_stream.base_name
              & EOL
              & "block_size ="
              & Natural'Image(the_stream.block_size)
              & EOL
              & "bytes_moved ="
              & KDF9.word'Image(the_stream.bytes_moved)
              & EOL
              & "fd ="
              & Natural'Image(the_stream.fd)
              & EOL
              & "IO_mode ="
              & POSIX.access_mode'Image(the_stream.IO_mode)
              & EOL
              & "last_IO ="
              & POSIX.access_mode'Image(the_stream.last_IO)
              & EOL
              & "next_byte ="
              & Natural'Image(the_stream.next_byte)
              & EOL
              & "position ="
              & Natural'Image(the_stream.position)
              & EOL
              & "column = "
              & Natural'Image(the_stream.column);
      else
         return caption;
      end if;
   end image_of;

   procedure diagnose (the_stream : IO.stream;
                       caption    : String := "") is
   begin
      put_error_message(image_of(the_stream, caption));
   end diagnose;

   procedure open (the_stream : in out IO.stream;
                   file_name  : in String;
                   mode       : in POSIX.access_mode;
                   fd         : in Integer) is
   begin
      if fd >= 0 then
         make_transparent(fd);
         the_stream.base_name := file_name(file_name'First .. file_name'First+2);
         the_stream.IO_mode := mode;
         the_stream.last_IO := read_mode;
         the_stream.fd := fd;
         the_stream.is_open := True;
      else
         diagnose(the_stream, "'" & file_name & "'");
      end if;
   end open;

   procedure open (the_stream : in out IO.stream;
                   file_name  : in String;
                   mode       : in POSIX.access_mode) is
   begin
      open(the_stream, file_name, mode, open(file_name, mode));
   exception
      when others =>
         if file_name /= "FW0" then
            diagnose(the_stream, "'" & file_name & "'");
         end if;
   end open;

   procedure truncate (the_stream : in out IO.stream;
                       to_length  : in KDF9.word := 0) is
   begin
      truncate(the_stream.fd, POSIX.file_position(to_length));
   end truncate;

   procedure close (the_stream : in out IO.stream) is
      response : Integer;
      pragma Unreferenced(response);
   begin
      if the_stream.is_open then
         flush(the_stream);
         response := close(the_stream.fd);
         the_stream.is_open := False;
      end if;
   end close;

   procedure flush (the_stream  : in out IO.stream;
                    a_byte_time : in KDF9.microseconds := 0) is
      response : Integer;
      pragma Unreferenced(response);
   begin
      if the_stream.is_open      and then
            the_stream.next_byte > 0 then
         if the_stream.IO_mode > read_mode and the_stream.last_IO = write_mode then
            if a_byte_time = 0 then
               response := write(the_stream.fd, the_stream.buffer, the_stream.next_byte);
            else
               for b in 1 .. the_stream.next_byte loop
                  response := write(the_stream.fd, the_stream.buffer(b..b), 1);
                  KDF9.delay_by(a_byte_time);
               end loop;
            end if;
         end if;
         the_stream.next_byte := 0;
         the_stream.block_size := 0;
      end if;
   exception
      when POSIX_IO_error =>
         diagnose(the_stream, "FLUSH POSIX_IO_error");
         raise stream_IO_error;
   end flush;

   function a_LF_was_just_read (the_stream : IO.stream)
   return Boolean is
   begin
      if the_stream.is_open               and then
            the_stream.bytes_moved > 0    and then
               the_stream.last_IO = read_mode then
         return (the_stream.next_byte = 0 and the_stream.block_size = 0);
      else
         return False;
      end if;
   end a_LF_was_just_read;

   function a_LF_was_just_written (the_stream : IO.stream)
   return Boolean is
   begin
      if the_stream.is_open                and then
            the_stream.bytes_moved > 0     and then
               the_stream.last_IO /= read_mode then
         return the_stream.column = 0;
      else
         return False;
      end if;
   end a_LF_was_just_written;

   procedure reattach (the_stream : in out IO.stream;
                       file_name  : in String;
                       mode       : in POSIX.access_mode) is
      old_fd : constant Natural := the_stream.fd;
   begin
      close(the_stream);
      if mode /= the_stream.IO_mode then
         diagnose(the_stream, "REATTACH: the new mode is incompatible");
         raise stream_IO_error;
      end if;
      open(the_stream, file_name, mode);
      if old_fd = 0 and the_stream.fd /= 0 then
         diagnose(the_stream, "REATTACH standard input: the new fd /= 0");
         raise stream_IO_error;
      end if;
      the_stream.last_char := ' ';
      the_stream.block_size := 0;
      the_stream.next_byte := 0;
      the_stream.position := 0;
   exception
      when others =>
         diagnose(the_stream, "REATTACH error");
         raise stream_IO_error;
   end reattach;

   function is_open(the_stream : IO.stream)
   return Boolean is
   begin
      return the_stream.is_open;
   end is_open;

   function bytes_moved(the_stream : IO.stream)
   return KDF9.word is
   begin
      return the_stream.bytes_moved;
   end bytes_moved;

   function file_size (the_stream : IO.stream)
   return Natural is
      first : constant POSIX.file_position := seek(the_stream.fd, 0, from_here);
      size  : constant POSIX.file_position := seek(the_stream.fd, 0, from_end);
      last  : constant POSIX.file_position := seek(the_stream.fd, first, from_start);
   begin
      if first /= last then
         raise end_of_stream;
      end if;
      return Natural(size);
   end file_size;

   function column (the_stream : IO.stream)
   return Natural is
   begin
      return the_stream.column;
   end column;

   procedure get_position (position   : out Natural;
                           the_stream : in out IO.stream) is
   begin
      flush(the_stream);
      position := the_stream.position;
   end get_position;

   function buffer_is_empty (the_stream : IO.stream)
   return Boolean is
   begin
      return not the_stream.is_open or else
               (the_stream.next_byte = the_stream.block_size);
   end buffer_is_empty;

   function buffer_is_full (the_stream : IO.stream)
   return Boolean is
   begin
      return the_stream.is_open and then
                the_stream.next_byte = the_stream.buffer'Last;
   end buffer_is_full;

   procedure set_position (position   : in Natural;
                           the_stream : in out IO.stream;
                           whence     : in POSIX.seek_origin := from_start) is
      response : POSIX.file_position;
      pragma Unreferenced(response);
   begin
      flush(the_stream);
      response := seek(the_stream.fd, POSIX.file_position(position), whence);
      the_stream.position := position;
   exception
      when POSIX_IO_error =>
         diagnose(the_stream, "set_position POSIX_IO_error");
         raise;
   end set_position;

   procedure reset (the_stream : in out IO.stream) is
   begin
      flush(the_stream);
      if the_stream.is_open then
         the_stream.last_IO := read_mode;
         the_stream.position := 0;
         the_stream.next_byte := 0;
         the_stream.block_size := 0;
      end if;
   end reset;

   procedure back_off (the_stream : in out IO.stream) is
   begin
      if the_stream.is_open                and then
            the_stream.next_byte > 0       and then
               the_stream.last_IO = read_mode  then
         the_stream.next_byte := the_stream.next_byte - 1;
         the_stream.position := the_stream.position - 1;
      else
         raise stream_IO_error;
      end if;
   end back_off;

   procedure get_byte (char       : out Character;
                       the_stream : in out IO.stream) is
      response : Integer;
   begin
      if not the_stream.is_open then
         raise end_of_stream;
      end if;
      if buffer_is_empty(the_stream) then
         response := read(the_stream.fd,
                          the_stream.buffer,
                          the_stream.buffer'Size);
         if response <= 0 then
            raise end_of_stream;
         end if;
         the_stream.block_size := response;
         the_stream.next_byte := 0;
      end if;
      the_stream.next_byte := the_stream.next_byte + 1;
      the_stream.position := the_stream.position + 1;
      the_stream.bytes_moved := the_stream.bytes_moved + 1;
      the_stream.last_IO := read_mode;
      char := the_stream.buffer(the_stream.next_byte);
      if char = LF then
         the_stream.column := 0;
      else
         the_stream.column := the_stream.column + 1;
      end if;
   exception
      when end_of_stream =>
         raise;
      when POSIX_IO_error =>
         diagnose(the_stream, "GET_BYTE POSIX_IO_error");
         raise end_of_stream;
      when others =>
         diagnose(the_stream, "GET_BYTE error");
         raise stream_IO_error;
   end get_byte;

   procedure get_bytes (the_string : out String;
                        the_stream : in out IO.stream;
                        uncounted  : in Boolean := True) is
      old_bytes_moved : constant KDF9.word := the_stream.bytes_moved;
   begin
      for i in the_string'Range loop
         get_byte(the_string(i), the_stream);
      end loop;
      if uncounted then
         the_stream.bytes_moved := old_bytes_moved;
      end if;
   end get_bytes;

   procedure get_char (char       : out Character;
                       the_stream : in out IO.stream) is
   begin
      get_byte(char, the_stream);
      if char = CR then
         char := LF;
         the_stream.last_char := CR;
      elsif char = LF and the_stream.last_char = CR then
         the_stream.last_char := LF;
         get_byte(char, the_stream);
      else
         the_stream.last_char := char;
      end if;
   end get_char;

   procedure peek_at_char (char       : out Character;
                           the_stream : in out IO.stream) is
   begin
      get_char(char, the_stream);
      back_off(the_stream);
   end peek_at_char;

   procedure put_byte (char       : in Character;
                       the_stream : in out IO.stream) is
      response : Integer;
   begin
      if not the_stream.is_open then
         raise end_of_stream;
      end if;
      if buffer_is_full(the_stream) then
         response := write(the_stream.fd,
                           the_stream.buffer,
                           the_stream.buffer'Size);
         if response <= 0 then
            raise end_of_stream;
         end if;
         the_stream.next_byte := 0;
      end if;
      the_stream.next_byte := the_stream.next_byte + 1;
      the_stream.position := the_stream.position + 1;
      the_stream.bytes_moved := the_stream.bytes_moved + 1;
      the_stream.buffer(the_stream.next_byte) := char;
      the_stream.last_IO := write_mode;
      if char = LF then
         the_stream.column := 0;
      else
         the_stream.column := the_stream.column + 1;
      end if;
   exception
      when end_of_stream =>
         raise;
      when POSIX_IO_error =>
         diagnose(the_stream, "PUT_BYTE POSIX_IO_error");
         raise end_of_stream;
      when others =>
         diagnose(the_stream, "PUT_BYTE error (POSIX_IO_error) ");
         raise stream_IO_error;
   end put_byte;

   procedure put_bytes (the_string : in String;
                        the_stream : in out IO.stream;
                        uncounted  : in Boolean := True) is
      old_bytes_moved : constant KDF9.word := the_stream.bytes_moved;
   begin
      for i in the_string'Range loop
         put_byte(the_string(i), the_stream);
      end loop;
      if uncounted then
         the_stream.bytes_moved := old_bytes_moved;
      end if;
   end put_bytes;

   procedure put_EOL (the_stream : in out IO.stream) is
   begin
      put_bytes(EOL, the_stream);
   end put_EOL;

   procedure put_char (char       : in Character;
                       the_stream : in out IO.stream) is
   begin
      if char = LF then
         put_EOL(the_stream);
      else
         put_byte(char, the_stream);
      end if;
   end put_char;

   procedure put_chars (the_string : in String;
                        the_stream : in out IO.stream) is
   begin
      for i in the_string'Range loop
         put_char(the_string(i), the_stream);
      end loop;
   end put_chars;

   function contents (the_stream : IO.stream)
   return String is
   begin
      if the_stream.is_open then
         return the_stream.buffer(1 .. the_stream.next_byte);
      else
         return "";
      end if;
   end contents;

   procedure inject (the_string : in String;
                     the_stream : in out IO.stream) is
   begin
      if not the_stream.is_open then
         diagnose(the_stream,
                  "injecting '"
                & the_string
                & "' into  closed stream '"
                & the_stream.contents
                & "'"
                & EOL );
         raise stream_IO_error;
      end if;
      the_stream.block_size := the_string'Length;
      if the_stream.block_size > 0 then
         the_stream.buffer(1 .. the_stream.block_size) := the_string;
         the_stream.buffer(the_stream.block_size+1) := LF;
      end if;
   end inject;

end IO;
