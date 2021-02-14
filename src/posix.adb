-- Provide a binding to a small subset of POSIX I/O operations.
--
-- This file is part of ee9 (6.0a), the GNU Ada emulator of the English Electric KDF9.
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

with System;
--
with Ada.Characters.Latin_1;

use  Ada.Characters.Latin_1;

package body POSIX is

   -- N.B. in POSIX the term 'buffer' is used conventionally.
   --      It does NOT refer to a KDF9 DMA channel.

   use type C.int;
   use type C.long;

   procedure verify (IO_status : in Integer; what : in String := "") is
   begin
      if IO_status < 0 then
         put_error_message("POSIX operation failed in " & what);
         raise POSIX_IO_error with what;
      end if;
   end verify;

   procedure verify (IO_status : in C.long; what : in String := "") is
   begin
      if IO_status < 0 then
         put_error_message("POSIX operation failed in " & what);
         raise POSIX_IO_error with what;
      end if;
   end verify;

   procedure verify (IO_status : in C.int; what : in String := "") is
   begin
      if IO_status < 0 then
         put_error_message("POSIX operation failed in " & what);
         raise POSIX_IO_error with what;
      end if;
   end verify;

   function verify (IO_status : C.long; what : String := "")
   return POSIX.file_position
   is (
       if IO_status < 0 then
          raise POSIX_IO_error with what
       else
          POSIX.file_position(IO_status)
      );

   function verify (IO_status : C.int; what : String := "")
   return Integer
   is (
       if IO_status < 0 then
          raise POSIX_IO_error with what
       else
          Integer(IO_status)
      );

   function creat (name : C.char_array;  permissions : C.int)
   return C.int
      with Import, Convention => C;

   function create (name : String;  permissions : POSIX.permission_set)
   return Integer is
      fd : constant C.int := creat(C.To_C(name, Append_Nul => True), C.int(permissions));
   begin
       verify(fd, "create: " & name);
       return Integer(fd);
   end create;

   function open (name : C.char_array;  mode : C.int)
   return C.int
      with Import, Convention => C;

   function open (name : String;  mode : POSIX.access_mode)
   return Integer
   is (
       verify(open(C.To_C(name, Append_Nul => True), C.int(mode)), "open file: " & name)
      );

   function exists (name : String)
   return Boolean is
      response : C.int;
   begin
      response := open(C.To_C(name, Append_Nul => True), C.int(POSIX.read_mode));
      return response >= 0;
   exception
      when POSIX_IO_error =>
         return False;
   end exists;

   function ftruncate (fd : C.int;  to_length : C.long)
   return C.long
      with Import, Convention => C;

   procedure truncate (fd : Natural;  to_length : POSIX.file_position := 0) is
   begin
      verify(ftruncate(C.int(fd), C.long(to_length)), "truncate fd: " & fd'Image);
   end truncate;

   function lseek (fd : C.int;  to_offset : C.long;  whence : C.int)
   return C.long
      with Import, Convention => C;

   function seek (fd        : Natural;
                  to_offset : POSIX.file_position;
                  whence    : POSIX.seek_origin := from_start)
   return POSIX.file_position
   is (
       verify(lseek(C.int(fd), C.long(to_offset), C.int(whence)),
              "seek fd: " & fd'Image)
      );

   function read (fd : C.int;  buffer : System.Address;  count : C.int)
   return C.int
      with Import, Convention => C;

   function read (fd : Natural;  buffer : out String;  count : Natural)
   return Integer is
      size   : constant C.int := C.int(Integer'Min(count, buffer'Length));
      status : C.int;
   begin
       status := read(C.int(fd), buffer'Address, size);
       verify(status, "read fd: " & fd'Image);
       return Integer(status);
   end read;

   function write (fd : C.int;  buffer : System.Address;  count : C.int)
   return C.int
      with Import, Convention => C;

   function write (fd : Natural;  buffer : in String;  count : Natural)
   return Integer is
      size   : constant C.int := C.int(Integer'Min(count, buffer'Length));
      status : C.int;
   begin
       status := write(C.int(fd), buffer'Address, size);
       verify(status, "write fd: " & fd'Image);
       return Integer(status);
   end write;

   function close (fd : C.int)
   return C.int
      with Import, Convention => C;

   function close (fd : Natural)
   return Integer
   is (
       verify(close(C.int(fd)), "close fd: " & fd'Image)
      );

   function get_errno
   return C.int
      with Import, Convention => C, External_Name => "__get_errno";

   function error_number
   return Integer
   is (Integer(get_errno));

   procedure set_errno (error_number : in C.int)
      with Import, Convention => C, External_Name => "__set_errno";

   procedure set_error_number (error_number : in Integer) is
   begin
      set_errno(C.int(error_number));
   end set_error_number;

   procedure perror (error_message : in C.char_array)
      with Import, Convention => C;

   procedure put_error_message (error_message : in String) is
      message : constant C.char_array := C.To_C(error_message, Append_Nul => True);
   begin
      perror(message);
      set_errno(get_errno);
   end put_error_message;

   procedure open_UI is
   begin
      UI_in_FD := open(UI_in_name, read_mode);
      verify(UI_in_FD, UI_in_name);
      UI_out_FD := open(UI_out_name, write_mode);
      verify(UI_out_FD, UI_out_name);
      UI_is_open := True;
   end open_UI;

   procedure ensure_UI_is_open is
   begin
      if not UI_is_open then
         open_UI;
      end if;
   end ensure_UI_is_open;

   C_reply_string : C.char_array(1 .. 256);

   function next_file_name (prompt : String)
   return String is
      C_prompt        : constant C.char_array
                      := C.To_C(NL & "ee9: " & prompt & ": ", Append_Nul => False);
      C_reply_length : C.Int;
   begin
      ensure_UI_is_open;
      verify(write(C.int(UI_out_FD), C_prompt'Address, C_prompt'Length),
             "prompt: for file name");
      C_reply_string := (256 => Interfaces.C.char(NUL), others => '?');
      C_reply_length := read(C.int(UI_in_FD), C_reply_string'Address, C_reply_string'Length);
      if C_reply_length < 2 then
         -- ^C typed, probably.
         return "";
      else
         return C.To_Ada(C_reply_string)(1..Natural(C_reply_length-1));
      end if;
   end next_file_name;

   procedure data_prompt (offline   : in  Boolean;
                          prompt    : in String;
                          response  : out response_kind) is
      message  : constant String := "ee9: " & prompt & ": ";
      C_prompt : constant C.char_array := C.To_C(NL & message, Append_Nul => True);
      C_reply_length : C.Int;
   begin
      if offline then
         output_line("ee9: Running in the non-interactive mode: EOF signalled.");
         response := EOF_response;
         return;
      end if;
      ensure_UI_is_open;
      verify(write(C.int(UI_out_FD), C_prompt'Address, C_prompt'Length-1),
             "prompt: " & message);
      C_reply_string := (others => '?');
      C_reply_string(256) := C.char(NUL);
      C_reply_length := read(C.int(UI_in_FD), C_reply_string'Address, 2);
      verify(C_reply_length, "prompt: reply");

      response := wrong_response;

      if C_reply_length = 0 then
         response := quit_response;
      elsif C.To_Ada(C_reply_string(1)) = LF then
         response := EOF_response;
      elsif C_reply_length = 2                         and then
               C.To_Ada(C_reply_string(2)) = LF        and then
                  C.To_Ada(C_reply_string(1)) in 'q' | 'Q' then
         response := quit_response;
      elsif C_reply_length = 2                  and then
               C.To_Ada(C_reply_string(2)) = LF and then
                  C.To_Ada(C_reply_string(1)) = '/' then
         response := name_response;
      elsif C_reply_length = 2                  and then
               C.To_Ada(C_reply_string(2)) = LF and then
                  C.To_Ada(C_reply_string(1)) = '@' then
         response := at_response;
      elsif C_reply_length = 2                                                    and then
               C.To_Ada(C_reply_string(Interfaces.C.size_t(C_reply_length))) = LF and then
                   C.To_Ada(C_reply_string(1)) = '='                                  then
         response := here_response;
      elsif C_reply_length > 0 then
         while C.To_Ada(C_reply_string(Interfaces.C.size_t(C_reply_length))) /= LF loop
            C_reply_length := read(C.int(UI_in_FD), C_reply_string'Address, 1);
            verify(C_reply_length, "LF");
         end loop;
      end if;
   end data_prompt;

   procedure debug_prompt (offline   : in  Boolean;
                           reason    : in String;
                           response  : out response_kind;
                           letter    : out Character) is
      prompt       : constant String
                   := "ee9: " & reason & ": (d:ebug | f:ast | t:race | p:ause or q:uit)? ";
      UNIX_prompt  : constant C.char_array := C.To_C(NL & prompt, Append_Nul => True);
      C_reply_length : C.Int;
   begin
      if offline then
         output_line("ee9: Running in the non-interactive mode: EOF signalled.");
         response := EOF_response;
         letter   := '?';
         return;
      end if;
      ensure_UI_is_open;
      verify(write(C.int(UI_out_FD), UNIX_prompt'Address, UNIX_prompt'Length-1),
             "prompt: " & prompt);
      C_reply_string := (others => '?');
      C_reply_string(256) := C.char(NUL);
      C_reply_length := read(C.int(UI_in_FD), C_reply_string'Address, 2);
      verify(C_reply_length, "prompt: reply");

      response := wrong_response;

      if C_reply_length = 0               or else
            C.To_Ada(C_reply_string(1)) = LF then
         response := EOF_response;
      elsif C_reply_length = 2 then
         letter := C.To_Ada(C_reply_string(1));
         if C.To_Ada(C_reply_string(2)) = LF                                  and then
               letter in 'd' | 'f' | 'p' | 'q' | 't'| 'D' | 'F' | 'P' | 'Q' | 'T' then
         response := name_response;
         end if;
      elsif C_reply_length > 0 then
         while C.To_Ada(C_reply_string(Interfaces.C.size_t(C_reply_length))) /= LF loop
            C_reply_length := read(C.int(UI_in_FD), C_reply_string'Address, 1);
            verify(C_reply_length, "LF");
         end loop;
      end if;
   end debug_prompt;

   procedure output (message : in String) is
      UNIX_message : constant C.char_array := C.To_C(message, Append_Nul => False);
   begin
      if message = "" then
         return;
      end if;
      ensure_UI_is_open;
      verify(write(C.int(UI_out_FD), UNIX_message'Address, UNIX_message'Length),
             "output: " & message);
   end output;

   procedure output_line (message : in String) is
      message_line : constant String := message & NL;
   begin
      output(message_line);
   end output_line;

   procedure output (message  : in Character) is
   begin
      ensure_UI_is_open;
      verify(write(C.int(UI_out_FD), message'Address, 1), "output: " & message);
   end output;

   procedure output_line is
   begin
      output(NL);
   end output_line;

   procedure input  (message : out Character) is
   begin
      ensure_UI_is_open;
      verify(read(C.int(UI_in_FD), message'Address, 1), "input");
   end input;

   procedure POSIX_exit (status : in C.int)
      with Import, Convention => C, External_Name => "exit";

   procedure exit_program (status : in Natural) is
   begin
      POSIX_exit(C.int(status));
   end exit_program;

end POSIX;

