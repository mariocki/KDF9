-- Emulation of the common functionality of a 2-case (Normal/Shift) buffer.
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

with KDF9_char_sets;

use  KDF9_char_sets;

package IOC.slow.shift is

   --
   -- Abstract common functionality of Case Normal / Case Shift devices, i.e.,
   --    the BSI interface (SI) paper tape reader (TR), punch (TP) and console Flexowriter (FW).
   --

   type device is abstract new IOC.slow.device with private;

   procedure set_case (the_device  : in out shift.device;
                       the_setting : in KDF9_char_sets.letter_case := KDF9_char_sets.Case_Normal);

   function uses_Latin_1 (the_device : in shift.device)
   return Boolean;

   -- Read a character from the stream and deal with any input file concatenation.
   -- If the buffer is reading from a file, deliver the character found; otherwise:
   --    convert the character from Latin-1 to the corresponding paper tape 8-bit code frame.
   procedure get_frame_from_stream (frame      : out Character;
                                    the_device : in out shift.device);

private

   type device is abstract new IOC.slow.device with
      record
         current_case : KDF9_char_sets.letter_case := KDF9_char_sets.Case_Normal;
      end record;

   overriding
   procedure Finalize (the_device : in out shift.device);

   overriding
   procedure Initialize (the_device : in out shift.device);

   procedure do_input_housekeeping (the_device : in out shift.device;
                                    read_in,
                                    stored     : in KDF9.word);

   procedure do_output_housekeeping (the_device : in out shift.device;
                                     written,
                                     fetched    : in KDF9.word);

   procedure write (the_device : in out shift.device;
                    Q_operand  : in KDF9.Q_register);

   procedure read (the_device : in out shift.device;
                   Q_operand  : in KDF9.Q_register);

   procedure write_to_EM (the_device : in out shift.device;
                          Q_operand  : in KDF9.Q_register);

   procedure read_to_EM (the_device : in out shift.device;
                         Q_operand  : in KDF9.Q_register);

   procedure words_write (the_device : in out shift.device;
                          Q_operand  : in KDF9.Q_register);

   procedure words_read (the_device : in out shift.device;
                         Q_operand  : in KDF9.Q_register);

   procedure words_write_to_EM (the_device : in out shift.device;
                                Q_operand  : in KDF9.Q_register);

   procedure words_read_to_EM (the_device : in out shift.device;
                               Q_operand  : in KDF9.Q_register);

   procedure output_a_gap (the_device   : in out shift.device;
                           Q_operand    : in KDF9.Q_register;
                           set_offline  : in Boolean;
                           word_mode    : in Boolean := False;
                           text_mode    : in Boolean := False);

end IOC.slow.shift;
