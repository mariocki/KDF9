-- file_IO_interface.adb
--
-- Provide an Ada.Text_IO interface to the file system of the real OS.
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

with Ada.Exceptions;
--
with HCI;

use  HCI;

package body file_interfacing is

   pragma Unsuppress(All_Checks);

   procedure initialize (some_file : in out File_Type;
                         mode      : in File_Mode;
                         file_name : in String) is
   begin
      Open(some_file, mode, file_name);
   exception
      when others =>
         if mode = Out_File then
            Create(some_file, Out_File, file_name);
         else
            raise;
         end if;
   end initialize;

   procedure finalize (some_file : in out File_Type;
                       file_name : in String) is
   begin
      Close(some_file);
   exception
      when error : others =>
         log_line("Failure in ee9: "
                & Ada.Exceptions.Exception_Information(error)
                & " was raised for '" & file_name & "'"
                & " in 'file_interfacing.finalize'!");
         raise;
   end finalize;

end file_interfacing;