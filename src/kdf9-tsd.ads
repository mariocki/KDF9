-- Implement the API (OUTs) of the EE Time Sharing Directors.
--
-- This file is part of ee9 (8.1a), the GNU Ada emulator of the English Electric KDF9.
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

private with exceptions;
private with formatting;
private with HCI;
private with IOC.equipment;
private with settings;
private with state_display;
private with tracing;

package KDF9.TSD is

   procedure do_a_TSD_OUT (OUT_number : in KDF9.word);

   -- Put the parameters of an I/O OUT back into the NEST in case the I/O order causes a lockout.
   procedure restore_the_IO_OUT_operands (OUT_number, parameter : KDF9.word);

   -- Remove the OUT parameters from the NEST after the I/O order completes without interrupting.
   procedure remove_the_IO_OUT_operands;

private

   use exceptions;     pragma Warnings(Off, exceptions);
   use formatting;     pragma Warnings(Off, formatting);
   use HCI;            pragma Warnings(Off, HCI);
   use IOC.equipment;  pragma Warnings(Off, IOC.equipment);
   use settings;       pragma Warnings(Off, settings);
   use state_display;  pragma Warnings(Off, state_display);
   use tracing;        pragma Warnings(Off, tracing);

end KDF9.TSD;
