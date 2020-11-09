-- ioc-shift_devices-tp.ads
--
-- Emulation of a tape punch buffer.
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

with IO; pragma Unreferenced(IO);
with IOC; pragma Elaborate_All(IOC);
with IOC.two_shift;
with KDF9.store;
with POSIX; pragma Unreferenced(POSIX);
with settings;

use  KDF9.store;
use  settings;

package body IOC.two_shift.TP is

   pragma Unsuppress(All_Checks);

   overriding
   procedure Initialize (the_TP : in out TP.device) is
   begin
      if the_TP.unit = 0 then
         -- Use the emulator's standard output.
         open(the_TP, write_mode, attaching => True, to_fd => 1);
      else
         if the_graph_plotter_is_configured then return; end if;
         -- We are not plotting, so switch the buffer to the tape punch.
         open(the_TP, write_mode, attaching => False);
      end if;
      the_TP.current_case := KDF9.Case_Normal;
   end Initialize;

   overriding
   procedure Finalize (the_TP : in out TP.device) is
   begin
      close(the_TP, "punched", the_TP.byte_count, "character(s)");
   end Finalize;


   -- TR := (the buffer has been switched from a tape punch to a graph plotter)
   overriding
   procedure PMB (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_TP, Q_operand);
      validate_parity(the_TP);
      the_T_bit := Boolean'Pos(the_graph_plotter_is_configured);
      take_note_of(Q_operand, the_TP.device_name, the_T_bit);
   end PMB;

   -- PWQq
   overriding
   procedure POA (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_TP, Q_operand, set_offline);
      write(the_TP, Q_operand);
      set_lockouts(Q_operand);
   end POA;

   -- PWEQq
   overriding
   procedure POB (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_TP, Q_operand, set_offline);
      write_to_EM(the_TP, Q_operand);
      set_lockouts(Q_operand);
   end POB;

   -- PWCQq
   overriding
   procedure POC (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_TP, Q_operand, set_offline);
      words_write(the_TP, Q_operand);
      set_lockouts(Q_operand);
   end POC;

   -- PWCEQq
   overriding
   procedure POD (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_TP, Q_operand, set_offline);
      words_write_to_EM(the_TP, Q_operand);
      set_lockouts(Q_operand);
   end POD;

   -- PGAPQq
   overriding
   procedure POE (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      require_nonnegative_count(Q_operand.M);
      initialize_byte_mode_gapping(the_TP, Q_operand, set_offline);
   end POE;

   -- "word gap"
   overriding
   procedure POF (the_TP      : in out TP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      POE(the_TP, Q_operand, set_offline);
   end POF;

   TP_quantum : constant := 1E6 / 110;  -- 110 characters per second.

   TP0 : aliased TP.device (TP0_number,
                            kind    => TP_kind,
                            unit    => 0,
                            quantum => TP_quantum,
                            is_slow => True);
   pragma Unreferenced(TP0);

   TP1 : aliased TP.device (TP1_number,
                            kind    => TP_kind,
                            unit    => 1,
                            quantum => TP_quantum,
                            is_slow => True);

   procedure switch_the_shared_buffer_from_TP1 is
   begin
      Finalize(TP1);
   end switch_the_shared_buffer_from_TP1;

end IOC.two_shift.TP;
