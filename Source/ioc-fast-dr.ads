-- ioc-fast-dr.ads
--
-- Emulation of a drum store buffer.
--
-- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2020, W. Findlay; all rights reserved.
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

   type device is new IOC.fast.device with private;

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

private

   -- For what little we know from EE of the drum geometry, see the Manual, App. 6, §4.
   -- This has been augmented here by some educated guesses (??) by WF.

   -- These have been informed by the specification of the contemporary Philco 2000's drum,
   --    which is consistent with the Manual, if we can assume 10 'bands' per drum instead 8,
   --       and, in effect, two interleaved 'tracks' per 'band'.
   -- See <bitsavers.trailing-edge.com/pdf/philco/2000/IO_System/TM-16C_212_IOsystem_Nov66.pdf>
   --    which says that the device has spare bands in addition to the used 8.

   bytes_per_sector   : constant := 1024;
   subtype byte_range is KDF9.word range 0 .. bytes_per_sector - 1;
   subtype sector     is String(1..bytes_per_sector);

   sectors_per_track  : constant := 16;             -- ??
   subtype sector_range is KDF9.word range 0 .. sectors_per_track - 1;

   sectors_per_drum   : constant := 320;
   drums_per_system   : constant := 4;
   sectors_per_system : constant := sectors_per_drum * drums_per_system;
   subtype drum_range is KDF9.word range 0 .. sectors_per_system - 1;

   tracks_per_system   : constant := sectors_per_system / sectors_per_track;
   subtype track_range is KDF9.word range 0 .. tracks_per_system - 1;

   data_rate    : constant := 500_000;  -- chars/s
   us_per_char  : constant := 1E6 / data_rate;

   -- Hypotheses: tracks are interleaved in pairs within one 'band';
   --    both tracks can be accessed without further penalty once the band is selected;
   --       but selecting another band incurs a switching time delay.

   -- The following times are in microseconds.
   sector_time  : constant := bytes_per_sector * us_per_char;
   gap_time     : constant := 0;                      -- ?? words interleave, so no inter-block gaps
   track_time   : constant := sector_time * sectors_per_track; -- ?? see gap_time
   switch_delay : constant := 8_000;                  -- ??

   type drum is array (drum_range) of DR.sector;

   type device is new IOC.fast.device with null record;

   overriding
   procedure Initialize (the_DR : in out DR.device);

   overriding
   procedure Finalize (the_DR : in out DR.device);

end IOC.fast.DR;
