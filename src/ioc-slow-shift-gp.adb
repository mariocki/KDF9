-- ioc-slow-shift-gp.ads
--
-- Emulation of a Calcomp 564 graph plotter, switched to a tape punch buffer.
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

with Ada.Exceptions;
--
with HCI;
with IOC.equipment;
with plotter;
with postscript;
with tracing;

use  HCI;
use  IOC.equipment;
use  plotter;
use  postscript;
use  tracing;

package body IOC.slow.shift.GP is

   overriding
   procedure Initialize (the_GP : in out GP.device) is
   begin
      -- Ready the graph plotter driver and PostScript output file.
      the_GP.device_name := device_name_of(the_GP);
      open(the_GP.stream, the_GP.device_name, write_mode);
      IOC.device(the_GP).Initialize;
      if the_GP.is_open then
         truncate(the_GP.stream, to_length => 0);
         initialize_PS_output(the_GP.stream);
         open_the_plot_file(the_GP.stream);
      end if;
   end Initialize;

   overriding
   procedure Finalize (the_GP : in out GP.device) is
   begin
      if the_graph_plotter_is_enabled then
         if the_GP.is_open           and then
               the_GP.byte_count /= 0    then
            if the_final_state_is_wanted then
               log_line(
                        the_GP.device_name
                      & " on buffer #"
                      & oct_of(KDF9.Q_part(the_GP.number), 2)
                      & " made"
                      & the_GP.byte_count'Image
                      & " plotting steps."
                       );
            end if;
            the_GP.byte_count := 0;
            close_the_plot_file(the_GP.stream);
            finalize_PS_output(the_GP.stream);
         end if;
      end if;
   end Finalize;

   -- the_T_bit_is_set (the buffer has been switched from a tape punch to a graph plotter)
   overriding
   procedure PMB (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      validate_device(the_GP, Q_operand);
      validate_parity(the_GP);
      deal_with_a_busy_device(the_GP, 13, set_offline);
      the_T_bit_is_set := True;
      take_note_of_test(the_GP.device_name, Q_operand, the_T_bit_is_set);
   end PMB;

   GP_quantum   : constant := 1E6 / 200;  -- 200 plotting movements per second.
   GP_lift_time : constant := 1E6 /  10;  -- 10 pen up/down movements per second.
   lift_ratio   : constant := GP_lift_time / GP_quantum;

   overriding
   procedure do_output_housekeeping (the_GP      : in out GP.device;
                                     size, lifts : in     KDF9.word) is
   begin
      add_in_the_IO_CPU_time(the_GP, size);
      correct_transfer_time(the_GP, size - lifts + lifts * lift_ratio);
   end do_output_housekeeping;

   procedure put_symbols (the_GP    : in out GP.device;
                          Q_operand : in KDF9.Q_register) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size    : KDF9.word := 0;
      lifts   : KDF9.word := 0;
      command : plotter.command;
   begin
      check_addresses_and_lockouts(start_address, end_address);
   word_loop:
      for w in start_address .. end_address loop
         for c in KDF9_char_sets.symbol_index'Range loop
            command := plotter.command(fetch_symbol(w, c));
            perform(command, the_GP.stream);
            size := size + 1;
            the_GP.byte_count := the_GP.byte_count + 1;
            if command in pen_up | pen_down then
               -- These actions are much slower than plotting movements.
               lifts := lifts + 1;
            end if;
         end loop;
      end loop word_loop;
      do_output_housekeeping (the_GP, size, lifts);
   end put_symbols;

   overriding
   procedure POA (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_GP, Q_operand, set_offline);
      put_symbols(the_GP, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end POA;

   overriding
   procedure POB (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      -- See the Manual Appendix 6, �5.2, p.303.
      POA(the_GP, Q_operand, set_offline);
   end POB;

   procedure put_words (the_GP    : in out GP.device;
                        Q_operand : in KDF9.Q_register) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size    : KDF9.word := 0;
      lifts   : KDF9.word := 0;
      command : plotter.command;
   begin
      check_addresses_and_lockouts(start_address, end_address);
      for w in start_address .. end_address loop
         -- Ony the last 6 bits (character 7) of each word are used.
         command := plotter.command(fetch_symbol(w, 7));
         perform(command, the_GP.stream);
         size := size + 1;
         the_GP.byte_count := the_GP.byte_count + 1;
         if command in pen_up | pen_down then
            -- These actions are much slower than plotting movements.
            lifts := lifts + 1;
         end if;
      end loop;
      do_output_housekeeping (the_GP, size, lifts);
   end put_words;

   overriding
   procedure POC (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      start_slow_transfer(the_GP, Q_operand, set_offline);
      put_words(the_GP, Q_operand);
      lock_out_relative_addresses(Q_operand);
   end POC;

   overriding
   procedure POD (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      -- See the Manual Appendix 6, �5.2, p.303.
      POC(the_GP, Q_operand, set_offline);
   end POD;


   type GP_access is access GP.device;

   GP0 : GP_access with Warnings => Off;

   procedure enable (b : in KDF9.buffer_number) is
   begin
      GP0 := new GP.device (number  => b,
                            kind    => GP_kind,
                            unit    => 0,
                            quantum => GP_quantum);
      GP0_number := b;
   end enable;

end IOC.slow.shift.GP;
