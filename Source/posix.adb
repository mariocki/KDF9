-- posix.adb
--
-- Provide a binding to a small subset of POSIX I/O operations.
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

with System;
--
with Latin_1;
with OS_specifics;
with settings;

use  Latin_1;
use  OS_specifics;
use  settings;

package body POSIX is

   pragma Unsuppress(All_Checks);

   -- N.B. in POSIX the term 'buffer' is used conventionally.
   --      It does NOT refer to a KDF9 DMA channel.

   use type C.int, C.long;

   procedure verify (IO_status : in Integer; what : in String := "") is
   begin
      if IO_status < 0 then
         raise POSIX_IO_error with Integer'Image(IO_status) & " in " & what;
      end if;
   end verify;

   procedure verify (IO_status : in C.long; what : in String := "") is
   begin
      if IO_status < 0 then
         raise POSIX_IO_error with C.long'Image(IO_status) & " in " & what;
      end if;
   end verify;

   procedure verify (IO_status : in C.int; what : in String := "") is
   begin
      if IO_status < 0 then
         raise POSIX_IO_error with C.int'Image(IO_status) & " in " & what;
      end if;
   end verify;

   function verify (IO_status : C.long; what : in String := "")
   return POSIX.file_position is
   begin
      if IO_status < 0 then
         raise POSIX_IO_error with C.long'Image(IO_status) & " in " & what;
      else
         return POSIX.file_position(IO_status);
      end if;
   end verify;

   function verify (IO_status : C.int; what : in String := "")
   return Integer is
   begin
      if IO_status < 0 then
         raise POSIX_IO_error with C.int'Image(IO_status) & " in " & what;
      else
         return Integer(IO_status);
      end if;
   end verify;

   function creat (name : C.char_array;  permissions : C.int)
   return C.int;
   pragma Import (C, creat);

   function create (name : String;  permissions : POSIX.permission_set)
   return Integer is
      file_name : constant C.char_array := C.To_C(name, Append_Nul => True);
   begin
      return verify(creat(file_name, C.int(permissions)), "create: " & name);
   end create;

   function open (name : C.char_array;  mode : C.int)
   return C.int;
   pragma Import (C, open);

   function open (name : String;  mode : POSIX.access_mode)
   return Integer is
      file_name : constant C.char_array := C.To_C(name, Append_Nul => True);
   begin
      return verify(open(file_name, C.int(mode)), "open file: " & name);
   end open;

   function ftruncate (fd : C.int;  to_length : C.long)
   return C.long;
   pragma Import (C, ftruncate);

   procedure truncate (fd : Natural;  to_length : POSIX.file_position := 0) is
   begin
      verify(ftruncate(C.int(fd), C.long(to_length)), "truncate fd: " & Natural'Image(fd));
   end truncate;

   function lseek (fd : C.int;  to_offset : C.long;  whence : C.int)
   return C.long;
   pragma Import (C, lseek);

   function seek (fd        : Natural;
                  to_offset : POSIX.file_position;
                  whence    : POSIX.seek_origin := from_start)
   return POSIX.file_position is
   begin
      return verify(lseek(C.int(fd), C.long(to_offset), C.int(whence)),
                    "seek fd: " & Natural'Image(fd));
   end seek;

   function read (fd : C.int;  buffer : System.Address;  count : C.int)
   return C.int;
   pragma Import (C, read);

   function read (fd : Natural;  buffer : String;  count : Natural)
   return Integer is
      safe_count : constant C.int := C.int(Integer'Min(count, buffer'Length));
   begin
      return verify(read(C.int(fd), buffer'Address, safe_count),
                    "read fd: " & Natural'Image(fd));
   end read;

   function write (fd : C.int;  buffer : System.Address;  count : C.int)
   return C.int;
   pragma Import (C, write);

   function write (fd : Natural;  buffer : String;  count : Natural)
   return Integer is
      safe_count : constant C.int := C.int(Integer'Min(count, buffer'Length));
   begin
      return verify(write(C.int(fd), buffer'Address, safe_count),
                    "write fd: " & Natural'Image(fd));
   end write;

   function close (fd : C.int)
   return C.int;
   pragma Import (C, close);

   function close (fd : Natural)
   return Integer is
   begin
      return verify(close(C.int(fd)), "close fd: " & Natural'Image(fd));
   end close;

   procedure ensure_ui_is_open is
   begin
      if not ui_is_open then
         open_ui;
      end if;
   end ensure_ui_is_open;

   function get_errno return C.int;
   pragma Import (C, get_errno, "__get_errno");

   function error_number return Integer is
   begin
      return Integer(get_errno);
   end error_number;

   procedure set_errno (error_number : in C.int);
   pragma Import (C, set_errno, "__set_errno");

   procedure set_error_number (error_number : in Integer) is
   begin
      set_errno(C.int(error_number));
   end set_error_number;

   procedure perror (error_message : in C.char_array);
   pragma Import (C, perror);

   procedure put_error_message (error_message : in String) is
      message : constant C.char_array := C.To_C(error_message, Append_Nul => True);
   begin
      perror(message);
      set_errno(0);
   end put_error_message;

   procedure prompt (message  : in String    := "?";
                     response : out Character;
                     default  : in Character := ' ') is
      unix_prompt   : constant C.char_array := C.To_C("ee9: " & message, Append_Nul => True);
      unix_response : C.char_array (0 .. 0);
   begin
      response := default;
      if noninteractive_usage_is_enabled then
         output_line("In non-interactive mode ee9 cannot get a reply to the prompt: ");
         output_line("'" & message & "'.");
         return;
      end if;
      ensure_ui_is_open;
      loop
         verify(write(C.int(ui_out_fd), unix_prompt'Address, unix_prompt'Length-1),
                "prompt: " & message);
         verify(read(C.int(ui_in_fd), unix_response'Address, 1), "response");
         if C.To_Ada(unix_response(0)) = LF then
            return;
         else
            response := C.To_Ada(unix_response(0));
            verify(read(C.int(ui_in_fd), unix_response'Address, 1),
                   "skipping line terminator");
            if C.To_Ada(unix_response(0)) = LF then
               return;
            end if;
         end if;
      end loop;
   end prompt;

   procedure output (message  : in String) is
      unix_message : constant C.char_array := C.To_C(message, Append_Nul => False);
   begin
      if message = "" then
         return;
      end if;
      ensure_ui_is_open;
      verify(write(C.int(ui_out_fd), unix_message'Address, unix_message'Length),
             "output: " & message);
   end output;

   procedure output_line (message : in String) is
      message_line : constant String := message & EOL;
   begin
      output(message_line);
   end output_line;

   procedure output (message  : in Character) is
   begin
      ensure_ui_is_open;
      verify(write(C.int(ui_out_fd), message'Address, 1), "output: " & message);
   end output;

   procedure input  (message  : out Character) is
   begin
      ensure_ui_is_open;
      verify(read(C.int(ui_in_fd), message'Address, 1), "input");
   end input;

   procedure POSIX_exit (status : in C.int);
   pragma Import (C, POSIX_exit, "exit");

   procedure exit_program (status : in Natural) is
   begin
      POSIX_exit(C.int(status));
   end exit_program;

end POSIX;

