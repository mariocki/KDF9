-- Emulation of a drum store buffer.
--
-- This file is part of ee9 (6.2e), the GNU Ada emulator of the English Electric KDF9.
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

package IOC.fast.DR is

   type device is new fast.device with private;

   overriding
   procedure PIA (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIB (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIC (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PID (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIE (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIF (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIG (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure PIH (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

-- PMA-PML, and POG-POL are inherited, as they have no new semantics for the drum.

   overriding
   procedure POA (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POB (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POC (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);
   overriding
   procedure POD (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POE (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   overriding
   procedure POF (the_DR      : in out DR.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure enable (b : in KDF9.buffer_number);

   procedure replace_on_buffer (b : in KDF9.buffer_number);

   procedure remove_from_buffer (b : in KDF9.buffer_number);

   DR0_is_enabled : Boolean := False;

   function as_DR_command (Q_operand : KDF9.Q_register; for_OUT : Boolean := False)
   return String;

private

   -- For what little we know from EE of the drum geometry, see the Manual, App. 6, §4.
   -- An additional and more helpful source is the SRLM, §103, Appendix 2, p.10-59-0,
   --   which describes the drum used with the non-Time Sharing Director.
   -- It says:
   --   Drum revolution time     = 20.4   ms
   --   Transfer time per sector =  2.15  ms
   --   Short gap time           =  0.034 ms between successive sectors
   --   Long gap time            =  2.97  ms after every 8th sector
   -- Hence: Mean time per sector = revolution time/8 - short gap time - long gap time/8
   --                             = 20.4 ms / 8       - 0.034          - 2.970 / 8
   --                             = 2.145 ms, rounding correctly to 2.15 ms
   -- This confirms 8 sectors per track, or 8192 characters per track, for 40 tracks per drum.

   bytes_per_sector   : constant := 1024;
   subtype byte_range is KDF9.word range 0 .. bytes_per_sector - 1;
   subtype sector     is String(1..bytes_per_sector);

   sectors_per_track  : constant := 8;
   subtype sector_range is KDF9.word range 0 .. sectors_per_track - 1;

   sectors_per_drum   : constant := 320;
   drums_per_system   : constant := 4;
   sectors_per_system : constant := sectors_per_drum * drums_per_system;
   subtype drum_index is KDF9.word range 0 .. sectors_per_system - 1;

   tracks_per_system   : constant := sectors_per_system / sectors_per_track;
   subtype track_range is KDF9.word range 0 .. tracks_per_system - 1;

   data_rate      : constant := 477_445;         -- chars/s
   us_per_char    : constant := 1E6 / data_rate; -- ~2.1 µs/char

   -- The following times are in microseconds.
   short_gap_time : constant := 34;
   sector_time    : constant := bytes_per_sector * us_per_char + short_gap_time;
   long_gap_time  : constant := 2_970;
   track_time     : constant := sector_time * sectors_per_track + long_gap_time;
   critical_time  : constant := sector_time * (sectors_per_track-1) - short_gap_time;

   type drum is array (drum_index) of DR.sector;

   type device is new fast.device with null record;

   overriding
   procedure Initialize (the_DR : in out DR.device);

   overriding
   procedure Finalize (the_DR : in out DR.device);

   overriding
   function kind (the_DR : DR.device)
   return IOC.device_kind
   is (DR_kind);

   overriding
   function quantum (the_DR : DR.device)
   return KDF9.us
   is (us_per_char);

   DR0_number : KDF9.buffer_number := 0;

end IOC.fast.DR;
