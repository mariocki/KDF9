-- ioc-byte_mode-two_shift.ads
--
-- Emulation of the common functionality of a 2-case (Normal/Shift) buffer.
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

with IOC;
with Latin_1; pragma Unreferenced(Latin_1);

package IOC.two_shift is

   pragma Unsuppress(All_Checks);

   --
   -- Abstract common functionality of Case Normal / Case Shift devices, e.g.,
   --    the paper tape reader (TR), punch (TP) and console Flexowriter (FW).
   --
   type device is abstract new IOC.byte_device with private;

   procedure set_case (the_device  : in out two_shift.device;
                       the_setting : in KDF9.letter_case := Case_Normal);

private

   type device is abstract new IOC.byte_device with
      record
         current_case : KDF9.letter_case range Case_Shift .. Case_Normal := Case_Normal;
      end record;

   overriding
   procedure Finalize (the_device : in out two_shift.device);

   procedure do_input_housekeeping (the_device : in out two_shift.device;
                                   read_in,
                                   stored     : in KDF9.word);

   procedure do_output_housekeeping (the_device : in out two_shift.device;
                                    written,
                                    fetched    : in KDF9.word);

   procedure write (the_device : in out two_shift.device;
                    Q_operand  : in KDF9.Q_register);

   procedure read (the_device : in out two_shift.device;
                   Q_operand  : in KDF9.Q_register);

   procedure write_to_EM (the_device : in out two_shift.device;
                          Q_operand  : in KDF9.Q_register);

   procedure read_to_EM (the_device : in out two_shift.device;
                         Q_operand  : in KDF9.Q_register);

   procedure words_write (the_device : in out two_shift.device;
                          Q_operand  : in KDF9.Q_register);

   procedure words_read (the_device : in out two_shift.device;
                         Q_operand  : in KDF9.Q_register);

   procedure words_write_to_EM (the_device : in out two_shift.device;
                                Q_operand  : in KDF9.Q_register);

   procedure words_read_to_EM (the_device : in out two_shift.device;
                               Q_operand  : in KDF9.Q_register);

end IOC.two_shift;
