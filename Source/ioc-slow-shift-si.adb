-- Emulation of a standard interface buffer.
--
-- This file is part of ee9 (7.0a), the GNU Ada emulator of the English Electric KDF9.
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

package body IOC.slow.shift.SI is

   use KDF9_char_sets;

   overriding
   procedure Initialize (the_SI : in out SI.device) is
   begin
      -- Open the associated file.
      open(IOC.device(the_SI), rd_wr_mode);
   end Initialize;

   overriding
   procedure PIA (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_SI, Q_operand, set_offline, input_operation);
      read(the_SI, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end PIA;

   overriding
   procedure PIB (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_SI, Q_operand, set_offline, input_operation);
      read_to_EM(the_SI, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end PIB;

   overriding
   procedure PIC (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_SI, Q_operand, set_offline, input_operation);
      words_read(the_SI, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end PIC;

   overriding
   procedure PID (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_SI, Q_operand, set_offline, input_operation);
      words_read_to_EM(the_SI, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end PID;

   overriding
   procedure PIE (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      -- as PIA: "parity off" has no effect on the data read in
      PIA(the_SI, Q_operand, set_offline);
   end PIE;

   overriding
   procedure PIF (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      -- as PIB: "parity off" has no effect on the data read in
      PIB(the_SI, Q_operand, set_offline);
   end PIF;

   overriding
   procedure PIG (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PIC(the_SI, Q_operand, set_offline);
   end PIG;

   overriding
   procedure PIH (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PID(the_SI, Q_operand, set_offline);
   end PIH;

   overriding
   procedure PMB (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      -- ee9's SI0 always asserts 8 channel mode.
      validate_device(the_SI);
      validate_parity(the_SI);
      deal_with_a_busy_device(the_SI, 13, set_offline);
      the_T_bit_is_set := True;
      take_note_of_test(the_SI.device_name, Q_operand, the_T_bit_is_set);
   end PMB;

   overriding
   procedure PMC (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      PMB(the_SI, Q_operand, set_offline);
   end PMC;

   overriding
   procedure POA (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_SI, Q_operand, set_offline, output_operation);
      write(the_SI, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end POA;

   overriding
   procedure POB (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_SI, Q_operand, set_offline, output_operation);
      write_to_EM(the_SI, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end POB;

   overriding
   procedure POC (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_SI, Q_operand, set_offline, output_operation);
      words_write(the_SI, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end POC;

   overriding
   procedure POD (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_SI, Q_operand, set_offline, output_operation);
      words_write_to_EM(the_SI, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end POD;

   overriding
   procedure POE (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      require_nonnegative_count(Q_operand.M);
      output_a_gap(the_SI, Q_operand, set_offline, word_mode => False, text_mode => False);
   end POE;

   overriding
   procedure POF (the_SI      : in out SI.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      require_nonnegative_count(Q_operand.M);
      output_a_gap(the_SI, Q_operand, set_offline, word_mode => True, text_mode => False);
   end POF;

   overriding
   procedure Finalize (the_SI : in out SI.device) is
   begin
      close(
            the_SI,
            "transferred",
            the_SI.byte_count,
            "character" & plurality(the_SI.byte_count)
           );
   end Finalize;

   type SI_access is access SI.device;

   SI0 : SI_access with Warnings => Off;
   SI1 : SI_access with Warnings => Off;

   unit : IOC.unit_number := 0;

   procedure enable (b : in KDF9.buffer_number) is
   begin
      case unit is
         when 0 =>
            SI0 := new SI.device (number => b, unit => 0);
            SI0_number := b;
         when 1 =>
            SI1 := new SI.device (number => b, unit => 1);
            SI1_number := b;
         when others =>
            trap_operator_error("more than two SI units have been configured");
      end case;
      unit := unit + 1;
   end enable;

   procedure replace_on_buffer (b : in KDF9.buffer_number) is
   begin
      if SI0 /= null   and then
            SI0.number = b then
         return;
      end if;
      if SI1 /= null   and then
            SI1.number = b then
         return;
      end if;
      buffer(b) := null;
      enable(b);
   end replace_on_buffer;

   function SI0_is_enabled
   return Boolean
   is (SI0 /= null or SI1 /= null);

end IOC.slow.shift.SI;
