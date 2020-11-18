-- get_runtime_paths.adb
--
-- Get the location of the ee9 runtime.
--
-- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2020 all rights reserved.
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
with Ada.Environment_Variables;

function get_runtime_paths
return String is
   kdf9runtime : constant String := Ada.Environment_Variables.Value("KDF9RUNTIME", "");
   home : constant String := Ada.Environment_Variables.Value("HOME", "");
begin
    return (if kdf9runtime = "" then (if home = "" then "./" else home & "/.kdf9/") else kdf9runtime & "/");
end get_runtime_paths;