-- Provide a binding to a small subset of POSIX I/O operations.
--
-- This file is part of ee9 (8.2a), the GNU Ada emulator of the English Electric KDF9.
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

with Interfaces.C;
--
with OS_specifics;

use  OS_specifics;

package POSIX is

   NL : constant String := OS_specifics.EOL;

   package C renames Interfaces.C;

   -- N.B. within POSIX the term 'buffer' is used conventionally.
   -- It does NOT refer to a KDF9 DMA channel.

   POSIX_IO_error   : exception;

   type permission_set is mod 2**9;

   world_read_permission  : constant permission_set := 4;
   world_write_permission : constant permission_set := 2;
   world_exec_permission  : constant permission_set := 1;

   group_read_permission  : constant permission_set := 8 * world_read_permission;
   group_write_permission : constant permission_set := 8 * world_write_permission;
   group_exec_permission  : constant permission_set := 8 * world_exec_permission;

   owner_read_permission  : constant permission_set := 8 * group_read_permission;
   owner_write_permission : constant permission_set := 8 * group_write_permission;
   owner_exec_permission  : constant permission_set := 8 * group_exec_permission;

   type access_mode is mod 3;

   read_mode  : constant POSIX.access_mode := 0;
   write_mode : constant POSIX.access_mode := 1;
   rd_wr_mode : constant POSIX.access_mode := 2;

   function exists (name : String)
   return Boolean;

   function open (name : String;
                  mode : POSIX.access_mode)
   return Integer;

   type file_position is new C.long;

   procedure truncate (fd : Natural);

   type seek_origin is mod 3;

   from_start : constant POSIX.seek_origin := 0;
   from_here  : constant POSIX.seek_origin := 1;
   from_end   : constant POSIX.seek_origin := 2;

   function seek (fd        : Natural;
                  to_offset : POSIX.file_position;
                  whence    : POSIX.seek_origin := from_start)
   return POSIX.file_position;

   function read (fd : Natural;  buffer : out String;  count : Natural)
   return Integer;

   function write (fd : Natural;  buffer : in String;  count : Natural)
   return Integer;

   function close (fd : Natural)
   return Integer;

   --  get the task-safe error number
   function error_number
   return Integer;

   --  set the task-safe error number
   procedure set_error_number (error_number : in Integer);

   procedure put_error_message (error_message : in String); -- and set the errno error number

   procedure exit_program (status : in Natural);

   -- The following all act on the interactive UI.

   procedure ensure_UI_is_open;

   UI_in_FD, UI_out_FD : Natural;
   UI_is_open          : Boolean := False;

   procedure output (message : in String);

   procedure output (message : in Character);

   procedure output_line (message : in String);  -- output(message & EOL)

   procedure output_line;  -- output(EOL)

   procedure input  (message  : out Character);

   type response_kind is
                        (
                         quit_response,
                         EOF_response,
                         LF_response,
                         path_response,
                         at_response,
                         here_response,
                         wrong_response,
                         debug_response
                        );

   -- If we are in non-interactive mode, log an error and set response to wrong_response.
   -- Display a message and read a reply, letter.
   -- If it is null (EOF signalled) or EOL, set response to EOF_response.
   -- If it is in 'd' | 'f' | 'p' | 't' | 'D' | 'F' | 'P' | 'T', set response to debug_response.
   -- If it is anything else, set response to wrong_response.
   procedure debug_prompt (offline   : in  Boolean;
                           reason    : in String;
                           response  : out response_kind;
                           letter    : out Character);

   -- If we are in non-interactive mode, log an error and set response to wrong_response.
   -- Display a prompt message and read a reply.
   -- If it is null:          set response to EOF_response.
   -- If it is EOL:           set response to LF_response.
   -- If it is /:             set response to path_response.
   -- If it is @:             set response to at_response.
   -- If it is =:             set response to here_response.
   -- If it is q or Q:        set response to quit_response.
   -- If it is anything else: set response to wrong_response.
   -- Set inline True if data follows @ or / on the same line.
   procedure data_prompt (offline   : in  Boolean;
                          prompt    : in String;
                          response  : out response_kind;
                          inline    : out Boolean);

   -- Get the name of the file.
   -- Suppress the prompt if the data is expected on the current line of input.
   function next_file_name (prompt : String;
                            inline : Boolean)
   return String;

end POSIX;
