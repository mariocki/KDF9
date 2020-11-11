-- posix.ads
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

with Interfaces.C;

package POSIX is

   pragma Unsuppress(All_Checks);

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

   function create (name        : String;
                    permissions : POSIX.permission_set)
   return Integer;

   type access_mode is mod 3;

   read_mode  : constant POSIX.access_mode := 0;
   write_mode : constant POSIX.access_mode := 1;
   rd_wr_mode : constant POSIX.access_mode := 2;

   function open (name : String;
                  mode : POSIX.access_mode)
   return Integer;

   type file_position is new C.long;

   procedure truncate (fd        : Natural;
                       to_length : POSIX.file_position := 0);

   type seek_origin is mod 3;

   from_start : constant POSIX.seek_origin := 0;
   from_here  : constant POSIX.seek_origin := 1;
   from_end   : constant POSIX.seek_origin := 2;

   function seek (fd        : Natural;
                  to_offset : POSIX.file_position;
                  whence    : POSIX.seek_origin := from_start)
   return POSIX.file_position;

   function read (fd : Natural;  buffer : String;  count : Natural)
   return Integer;

   function write (fd : Natural;  buffer : String;  count : Natural)
   return Integer;

   function close (fd : Natural)
   return Integer;

   procedure put_error_message (error_message : in String);

   --  get the task-safe error number
   function error_number
   return Integer;

   --  set the task-safe error number
   procedure set_error_number (error_number : in Integer);

   procedure ensure_ui_is_open;

   ui_in_fd, ui_out_fd : Natural;
   ui_is_open          : Boolean := False;

   procedure output (message  : in String);

   procedure output (message  : in Character);

   procedure output_line (message : in String);  -- output(message & EOL)

   procedure input  (message  : out Character);

   procedure prompt (message  : in  String := "?";
                     response : out Character;
                     default  : in  Character := ' ');

   procedure exit_program (status : in Natural);

   procedure verify (IO_status : in Integer; what : in String := "");

end POSIX;
