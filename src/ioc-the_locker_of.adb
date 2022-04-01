-- Identify the buffer that caused a store lockout.
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

function IOC.the_locker_of (address : KDF9.Q_part; found : out Boolean)
return KDF9.Q_part is
   candidate_time   : KDF9.us     := KDF9.us'Last;
   candidate_number : KDF9.Q_part := 0;
begin
   -- Select the buffer actively doing DMA in the_group;
   --    if there is more than one, choose the buffer with the earliest completion time.
   -- The latter case should not arise in practice, but is allowed by the hardware.
   found := False;
   for b in buffer'Range loop
      if buffer(b) /= null                                               and then
            buffer(b).is_busy                                            and then
               buffer(b).operation in input_operation | output_operation and then
                  buffer(b).completion_time < candidate_time             and then
                     group(address) in group(buffer(b).control_word.I)
                                    .. group(buffer(b).control_word.M)       then
         found := True;
         candidate_number := b;
         candidate_time   := buffer(b).completion_time;
      end if;
   end loop;
   return candidate_number;
end IOC.the_locker_of;
