-- Buffered I/O streams to support KDF9 device I/O.   Also used by ancillary programs.
--
-- This file is part of ee9 (8.2z), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2022, W. Findlay; all rights reserved.
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

with Ada.Characters.Latin_1;
--
with exceptions;
with OS_specifics;
with get_runtime_paths;

use  Ada.Characters.Latin_1;
--
use  exceptions;
use  OS_specifics;

package body host_IO is

   function fd_of (the_stream : host_IO.stream)
   return Natural
   is (the_stream.fd);

   procedure open (the_stream : in out host_IO.stream;
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
         the_stream.is_open := False;
      end if;
   end open;

   -- This duplicates code in KDF9 so we can avoid with-ing KDF9.
   procedure trap_operator_error (the_message : in String) is
   begin
      -- The program has failed for a reason, such as a misconfigured environment,
      --    that is beyond its control and prevents further execution.
      raise operator_error with "%" & the_message;
   end trap_operator_error;

   procedure open (the_stream : in out host_IO.stream;
                   file_name  : in String;
                   mode       : in POSIX.access_mode) is
      fd : Integer;
   begin -- open
      fd := POSIX.open(get_runtime_paths & file_name, mode);
      open(the_stream, file_name, mode, fd);
   exception
      when POSIX_IO_error =>
         trap_operator_error("""" & file_name & """ cannot be opened");
   end open;

  -- Open an anonymous stream, such as the standard input, with the given fd.
   procedure open (the_stream : in out host_IO.stream;
                   fd         : in Integer) is
      mode : constant POSIX.access_mode := (if fd = 0 then read_mode else write_mode);
   begin -- open
      open(the_stream, "???" & fd'Image, mode, fd);
   exception
      when POSIX_IO_error =>
         trap_operator_error("fd" & fd'Image & " cannot be opened");
   end open;

   procedure truncate (the_stream : in out host_IO.stream) is
   begin
      truncate(the_stream.fd);
   end truncate;

   procedure close (the_stream : in out host_IO.stream) is
      response : Integer;
   begin
      if the_stream.is_open then
         flush(the_stream);
         response := close(the_stream.fd);
         pragma Unreferenced(response);
         the_stream.is_open := False;
      end if;
   end close;

   procedure flush (the_stream  : in out host_IO.stream) is
      response : Integer with Unreferenced;
   begin
      if the_stream.is_open      and then
            the_stream.next_byte > 0 then
         if the_stream.IO_mode > read_mode and the_stream.last_IO = write_mode then
            response := write(the_stream.fd, the_stream.buffer, the_stream.next_byte);
         end if;
         the_stream.next_byte := 0;
         the_stream.block_size := 0;
      end if;
   end flush;

   function a_LF_was_just_read (the_stream : host_IO.stream)
   return Boolean
   is (
       the_stream.is_open                   and then
          the_stream.bytes_moved > 0        and then
             the_stream.last_IO = read_mode and then
                the_stream.next_byte = 0    and then
                   the_stream.block_size = 0
      );

   function a_LF_was_just_written (the_stream : host_IO.stream)
   return Boolean
   is (
       the_stream.is_open                    and then
          the_stream.bytes_moved > 0         and then
             the_stream.last_IO /= read_mode and then
                the_stream.column = 0
      );

   procedure reattach (the_stream : in out host_IO.stream;
                       file_name  : in String;
                       mode       : in POSIX.access_mode) is
   begin
      close(the_stream);
      open(the_stream, file_name, (if the_stream.IO_mode = rd_wr_mode then rd_wr_mode else mode));
      if the_stream.is_open then
         the_stream.last_char := ' ';
         the_stream.block_size := 0;
         the_stream.next_byte := 0;
         the_stream.position := 0;
      end if;
   end reattach;

   function is_open (the_stream : host_IO.stream)
   return Boolean
   is (the_stream.is_open);

   function bytes_moved (the_stream : host_IO.stream)
   return host_IO.bytes_moved_count
   is (the_stream.bytes_moved);

   function column (the_stream : host_IO.stream)
   return Natural
   is (the_stream.column);

   procedure get_position (position   : out Natural;
                           the_stream : in out host_IO.stream) is
   begin
      flush(the_stream);
      position := the_stream.position;
   end get_position;

   function buffer_is_empty (the_stream : host_IO.stream)
   return Boolean
   is (not the_stream.is_open or else the_stream.next_byte >= the_stream.block_size);

   function buffer_is_full (the_stream : host_IO.stream)
   return Boolean
   is (the_stream.is_open and then the_stream.next_byte = the_stream.buffer'Last);

   procedure set_position (position   : in Natural;
                           the_stream : in out host_IO.stream;
                           whence     : in POSIX.seek_origin := from_start) is
      response : POSIX.file_position with Warnings => Off;
   begin
      flush(the_stream);
      response := seek(the_stream.fd, POSIX.file_position(position), whence);
      the_stream.position := position;
   end set_position;

   procedure clear (the_stream : in out host_IO.stream) is
   begin
      the_stream.next_byte := 0;
      the_stream.block_size := 0;
   end clear;

   procedure reset (the_stream : in out host_IO.stream) is
   begin
      flush(the_stream);
      if the_stream.is_open then
         the_stream.last_IO := read_mode;
         the_stream.position := 0;
         the_stream.next_byte := 0;
         the_stream.block_size := 0;
      end if;
   end reset;

   procedure back_off (the_stream : in out host_IO.stream) is
   begin
      if the_stream.is_open                   and then
            the_stream.next_byte > 0          and then
               the_stream.last_IO = read_mode     then
         the_stream.next_byte := the_stream.next_byte - 1;
         the_stream.position := the_stream.position - 1;
      else
         trap_operator_error(the_stream.base_name & " cannot back_off");
      end if;
   end back_off;

   procedure get_byte (char       : out Character;
                       the_stream : in out host_IO.stream) is
      response : Integer;
   begin
      if buffer_is_empty(the_stream) then
         the_stream.block_size := 0;
         the_stream.next_byte := 0;
         response := read(the_stream.fd, the_stream.buffer, the_stream.buffer'Size);
         if response <= 0 then
            raise end_of_stream;
         end if;
         the_stream.block_size := response;
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
   end get_byte;

   procedure get_bytes (the_string : out String;
                        the_stream : in out host_IO.stream;
                        uncounted  : in Boolean := True) is
      old_bytes_moved : constant host_IO.bytes_moved_count := the_stream.bytes_moved;
   begin
      for b of the_string loop
         get_byte(b, the_stream);
      end loop;
      if uncounted then
         the_stream.bytes_moved := old_bytes_moved;
      end if;
   end get_bytes;

   procedure get_char (char       : out Character;
                       the_stream : in out host_IO.stream) is
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
                           the_stream : in out host_IO.stream) is
   begin
      get_char(char, the_stream);
      back_off(the_stream);
   end peek_at_char;

   -- put_escape_code writes directly to the stream's device, avoiding the stream's buffers.
   procedure put_escape_code (the_string : in String;
                              the_stream : in out host_IO.stream) is
      response : Integer with Warnings => Off;
   begin
      if not the_stream.is_open then
         raise end_of_stream;
      end if;
      response := write(the_stream.fd,
                        the_string,
                        the_string'Length);
   end put_escape_code;

   procedure put_byte (char       : in Character;
                       the_stream : in out host_IO.stream) is
      response : Integer with Warnings => Off;
   begin
      if the_stream.buffer_is_full then
         response := write(the_stream.fd,
                           the_stream.buffer,
                           the_stream.buffer'Size);
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
   end put_byte;

   procedure do_not_put_byte (char       : in Character;
                              the_stream : in out host_IO.stream) is
   begin
      the_stream.bytes_moved := the_stream.bytes_moved + 1;
      the_stream.last_IO := write_mode;
      if char = LF then
         the_stream.column := 0;
      else
         the_stream.column := the_stream.column + 1;
      end if;
   end do_not_put_byte;

   procedure put_bytes (the_string : in String;
                        the_stream : in out host_IO.stream;
                        uncounted  : in Boolean := True) is
      old_bytes_moved : constant host_IO.bytes_moved_count := the_stream.bytes_moved;
   begin
      for c of the_string loop
         put_byte(c, the_stream);
      end loop;
      if uncounted then
         the_stream.bytes_moved := old_bytes_moved;
      end if;
   end put_bytes;

   procedure put_EOL (the_stream : in out host_IO.stream;
                      uncounted  : in Boolean := True) is
   begin
      put_bytes(NL, the_stream, uncounted);
   end put_EOL;

   procedure put_char (char       : in Character;
                       the_stream : in out host_IO.stream) is
   begin
      if char = LF then
         put_EOL(the_stream, uncounted => False);
      else
         put_byte(char, the_stream);
      end if;
   end put_char;

   procedure put_chars (the_string : in String;
                        the_stream : in out host_IO.stream) is
   begin
      for c of the_string loop
         put_char(c, the_stream);
      end loop;
   end put_chars;

   function contents (the_stream : host_IO.stream)
   return String is
   begin
      return the_stream.buffer(1..the_stream.next_byte);
   end contents;

   procedure inject (the_string : in String;
                     the_stream : in out host_IO.stream) is
      the_length : constant Natural := the_string'Length;
   begin
      if the_length > 0 and the_length < the_stream.buffer'Length then
         the_stream.block_size := the_length + 1;
         the_stream.buffer(1 .. the_length) := the_string;
         the_stream.buffer(the_length + 1)  := LF;
      end if;
   end inject;

end host_IO;
