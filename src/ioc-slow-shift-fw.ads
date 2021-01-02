-- ioc-slow-shift-fw.ads
--
-- Emulation of a FlexoWriter buffer: monitor typewriter functionality.
--
-- This file is part of ee9 (V5.2b), the GNU Ada emulator of the English Electric KDF9.
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

package IOC.slow.shift.FW is

   type device is new IOC.slow.shift.device with private;

   function a_LF_was_just_read (the_FW : FW.device)
   return Boolean;

   function a_LF_was_just_written (the_FW : FW.device)
   return Boolean;

   -- TRQq
   overriding
   procedure PIA (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- TREQq
   overriding
   procedure PIB (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- TRCQq character read
   overriding
   procedure PIC (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- TRECQq character read to End_Message
   overriding
   procedure PID (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- as PIA
   overriding
   procedure PIE (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- as PIB
   overriding
   procedure PIF (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- as PIC
   overriding
   procedure PIG (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- as PID
   overriding
   procedure PIH (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- TWQq
   overriding
   procedure POA (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- TWEQq
   overriding
   procedure POB (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- NB the following assumes that page 285 of the Manual is erroneous,
   -- and that POC and POD for the Flexowriter are analogous to the tape punch,
   -- as other sources, such as the "Usecode Digest", do in fact indicate.

   -- TWCQq character write
   overriding
   procedure POC (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- TWECQq character write to End_Message
   overriding
   procedure POD (the_FW      : in out FW.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure enable (b : in KDF9.buffer_number);

private

   type flexowriter_mode is
      (the_flexowriter_is_reading, the_flexowriter_is_writing);

   -- The Flexowriter has separate input and output streams, to accommodate the console I/O API
   --    of MS Windows, which requires separate pseudo-devices for input and output.
   type device is new IOC.slow.shift.device with
      record
         output : host_IO.stream;
         mode   : FW.flexowriter_mode;
         shifts : KDF9.word := 0;
      end record;

   overriding
   procedure Initialize (the_FW : in out FW.device);

   overriding
   procedure Finalize (the_FW : in out FW.device);

   overriding
   procedure write (the_FW    : in out FW.device;
                    Q_operand : in KDF9.Q_register);

   overriding
   procedure write_to_EM (the_FW    : in out FW.device;
                          Q_operand : in KDF9.Q_register);
   overriding
   procedure words_write (the_FW    : in out FW.device;
                          Q_operand : in KDF9.Q_register);

   overriding
   procedure words_write_to_EM (the_FW    : in out FW.device;
                                Q_operand : in KDF9.Q_register);

   overriding
   procedure do_output_housekeeping (the_FW   : in out FW.device;
                                     written,
                                     fetched  : in KDF9.word);

end IOC.slow.shift.FW;
