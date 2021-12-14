-- Enable the devices included in the chosen KDF9 I/O configuration.
--
--
-- This file is part of ee9 (8.1x), the GNU Ada emulator of the English Electric KDF9.
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

with KDF9;

package IOC.equipment is

   -- These variables are used in the emulation of OUT 5.
   -- They are set to the device buffer number at the start of each run by configure_the_IOC.
   -- A value of 0 indicates that the device is not included in the configuration for the run.
   -- Fast devices are not allocated by this mechanism and so do not appear here.

   CP0_number  : KDF9.buffer_number := 0;
   CP1_number  : KDF9.buffer_number := 0;
   CR0_number  : KDF9.buffer_number := 0;
   CR1_number  : KDF9.buffer_number := 0;
   GP0_number  : KDF9.buffer_number := 0;
   LP0_number  : KDF9.buffer_number := 0;
   LP1_number  : KDF9.buffer_number := 0;
   SI0_number  : KDF9.buffer_number := 0;
   SI1_number  : KDF9.buffer_number := 0;
   TP0_number  : KDF9.buffer_number := 0;
   TP1_number  : KDF9.buffer_number := 0;
   TR0_number  : KDF9.buffer_number := 0;
   TR1_number  : KDF9.buffer_number := 0;

   type kind   is (AD, CP, CR, DR, FD, FW, GP, LP, MT, SI, ST, TP, TR);

   type setup  is array (KDF9.buffer_number) of equipment.kind;

   -- These are the buffer numbers for the devices in the default configuration.
   -- The Fixed Disc drive was on buffer 14 (#16) of Eldon 2 KDF9s, so I adopt that here.

   FW0_default : constant KDF9.buffer_number := 0;
   TR0_default : constant KDF9.buffer_number := 1;
   TR1_default : constant KDF9.buffer_number := 2;
   TP0_default : constant KDF9.buffer_number := 3;
   TP1_default : constant KDF9.buffer_number := 4;
   LP0_default : constant KDF9.buffer_number := 5;
   CR0_default : constant KDF9.buffer_number := 6;
   CP0_default : constant KDF9.buffer_number := 7;
   MT0_default : constant KDF9.buffer_number := 8;
   MT1_default : constant KDF9.buffer_number := 9;
   MT2_default : constant KDF9.buffer_number := 10;
   MT3_default : constant KDF9.buffer_number := 11;
   MT4_default : constant KDF9.buffer_number := 12;
   MT5_default : constant KDF9.buffer_number := 13;
   DR0_default : constant KDF9.buffer_number := 14;
   FD0_default : constant KDF9.buffer_number := 14;
   ST0_default : constant KDF9.buffer_number := 15;

   default : constant equipment.setup
           := (
               CP0_default => CP,
               CR0_default => CR,
               FD0_default => FD,
               FW0_default => FW,
               LP0_default => LP,
               MT0_default => MT,
               MT1_default => MT,
               MT2_default => MT,
               MT3_default => MT,
               MT4_default => MT,
               MT5_default => MT,
               ST0_default => ST,
               TP0_default => TP,
               TP1_default => TP,
               TR0_default => TR,
               TR1_default => TR
              );

   choice  : equipment.setup := default;

   -- Attach the chosen devices to their buffers.
   procedure configure_the_IOC;

   -- If a drum, disc or BSI has been enabled on the command line, make sure it is installed.
   procedure revise_the_IOC_configuration;

end IOC.equipment;
