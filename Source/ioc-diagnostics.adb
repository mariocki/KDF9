-- Provide diagnostic output of the state of all the buffers.
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

with disassembly;

with IOC.the_locker_of;

procedure IOC.diagnostics is
   Q : KDF9.Q_register;
   B : KDF9.Q_part;
   F : Boolean;
begin
   if not debugging_is_enabled then return; end if;
   for g in Q_part'(0) .. 100 loop
      if there_are_locks_in_physical_addresses(KDF9.Q_register'(0, 32*g, 32*g + 31)) then
         output("there are locks in group" & g'Image & " PHY" & Q_part'(32*g)'Image);
         B := the_locker_of(32*g, F);
         output(", locked by" & B'Image & ":");
         if F then output_line(buffer(B).device_name); else output_line("??"); end if;
      end if;
   end loop;
   for the_buffer of buffer loop
      if the_buffer /= null                        and then
         the_buffer.initiation_time /= KDF9.us'Last    then
         output_line;
         output_line("Current state of buffer #" & oct_of(the_buffer.number, 2));
         output_line("   device: " & the_buffer.device_name);
         output_line("  is_busy: " & the_buffer.is_busy'Image);
         output_line("operation: " & the_buffer.operation'Image);
         output_line(" off_line: " & the_buffer.is_offline'Image);
         output_line(" abnormal: " & the_buffer.is_abnormal'Image);
         output_line(" Director: " & the_buffer.is_for_Director'Image);
         output_line(" priority:"  & the_buffer.priority_level'Image);
         output_line("initiated:"  & the_buffer.initiation_time'Image);
         output_line("xfer_time:"  & the_buffer.transfer_time'Image);
         output_line("completes:"  & the_buffer.completion_time'Image);
         Q := the_buffer.control_word;
         output_line(
                     "  control: "
                   & "Q"
                   & Q.C'Image
                   &"/"
                   & Q.I'Image
                   & "/"
                   & Q.M'Image
                    );
         if Q.I <= KDF9.address'Last               and then
             Q.M <= KDF9.address'Last              and then
                Q.I <= the_buffer.control_word.M then
            output_line(
                        "locked in:"
                      & group(Q.I)'Image
                      & ".."
                      & group(Q.M)'Image
                      & " is "
                      & there_are_locks_in_physical_addresses(Q)'Image
                       );
         end if;
         output_line("order ICR:"  & the_buffer.order_count'Image);
         output_line("    order: " & disassembly.the_full_name_of(the_buffer.decoded_order, True));
         output_line("@ address: " & oct_of(the_buffer.order_address));
      end if;
   end loop;
end IOC.diagnostics;
