-- ioc-shift_devices-gp.ads
--
-- Emulation of a Calcomp 564 graph plotter, switched to a tape punch buffer.
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

with Ada.Exceptions;
--
with formatting;
with IO; pragma Unreferenced(IO);
with IOC; pragma Elaborate_All(IOC);
with IOC.two_shift;
with KDF9.store;
with plotter; pragma Elaborate_All(plotter);
with POSIX; pragma Unreferenced(POSIX);
with postscript; pragma Elaborate_All(postscript);
with settings;

use  KDF9.store;
use  formatting;
use  plotter;
use  postscript;
use  settings;

package body IOC.two_shift.GP is

   pragma Unsuppress(All_Checks);

   overriding
   procedure Initialize (the_GP : in out GP.device) is
   begin
      if the_graph_plotter_is_configured then
         -- Switch the buffer to the graph plotter.
         open(the_GP, write_mode, attaching => False);
         if the_GP.is_open then
            initialize_PostScript_output(the_GP.stream);
            open_the_plot_file;
         end if;
      end if;
   end Initialize;

   overriding
   procedure Finalize (the_GP : in out GP.device) is
   begin
      if the_graph_plotter_is_configured then
         if the_GP.is_open         and then
               the_GP.byte_count /= 0  then
            if the_final_state_is_wanted then
               output_line(the_GP.device_name
                         & " on buffer #"
                         & oct_of(KDF9.Q_part(the_GP.number), 2)
                         & " plotted"
                         & KDF9.word'Image(the_GP.byte_count)
                         & " character(s).");
            end if;
            the_GP.byte_count := 0;
            close_the_plot_file;
            finalize_PostScript_output;
         end if;
      end if;
   end Finalize;

   -- the_T_bit := (the punch buffer is switched to a graph plotter);
   overriding
   procedure PMB (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
      pragma Unreferenced(set_offline);
   begin
      validate_device(the_GP, Q_operand);
      validate_parity(the_GP);
      the_T_bit := 1;
      take_note_of(Q_operand, the_GP.device_name, the_T_bit);
   end PMB;

   GP_quantum   : constant := 1E6 / 200;  -- 200 plotting movements per second.
   GP_lift_time : constant := 1E6 /  10;  --  10 pen up/down movements per second.
   lift_ratio   : constant := GP_lift_time / GP_quantum;

   overriding
   procedure do_output_housekeeping (the_GP      : in out GP.device;
                                     size, lifts : in     KDF9.word) is
   begin
      add_in_the_IO_CPU_time(the_GP, size);
      correct_transfer_time(the_GP, size - lifts + lifts * lift_ratio);
   end do_output_housekeeping;

   function command_in_octal (symbol : plotter.command) return String is
   begin
      return oct_of(KDF9.word(symbol))(15..16);
   end command_in_octal;

   procedure put_symbols (the_GP    : in out GP.device;
                          Q_operand : in KDF9.Q_register) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size    : KDF9.word := 0;
      lifts   : KDF9.word := 0;
      symbol  : plotter.command;
   begin
      validate_range_access(start_address, end_address);
   word_loop:
      for w in start_address .. end_address loop
         for c in KDF9.symbol_number'Range loop
            symbol := plotter.command(fetch_symbol(w, c));
            if is_valid(symbol) then
               perform(symbol);
               size := size + 1;
               the_GP.byte_count := the_GP.byte_count + 1;
               if symbol = pen_up or symbol = pen_down then
                  -- These actions are much slower than plotting movements.
                  lifts := lifts + 1;
               end if;
            else
               do_output_housekeeping (the_GP, size, lifts);
               trap_invalid_instruction("invalid plot command: #" & command_in_octal(symbol));
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
      initialize_byte_mode_transfer(the_GP, Q_operand, set_offline);
      put_symbols(the_GP, Q_operand);
      set_lockouts(Q_operand);
   end POA;

   overriding
   procedure POB (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      -- See the Manual Appendix 6, §5.2, p.303.
      POA(the_GP, Q_operand, set_offline);
   end POB;

   procedure put_words (the_GP    : in out GP.device;
                        Q_operand : in KDF9.Q_register) is
      start_address : constant KDF9.address := Q_operand.I;
      end_address   : constant KDF9.address := Q_operand.M;
      size    : KDF9.word := 0;
      lifts   : KDF9.word := 0;
      symbol  : plotter.command;
   begin
      validate_range_access(start_address, end_address);
      for w in start_address .. end_address loop
         -- Ony the last 6 bits (character 7) of each word are used.
         symbol := plotter.command(fetch_symbol(w, 7));
         if is_valid(symbol) then
            perform(symbol);
            size := size + 1;
            the_GP.byte_count := the_GP.byte_count + 1;
            if symbol = pen_up or symbol = pen_down then
               -- These actions are much slower than plotting movements.
               lifts := lifts + 1;
            end if;
         else
            do_output_housekeeping (the_GP, size, lifts);
            trap_invalid_instruction("invalid plot command: #" & command_in_octal(symbol));
         end if;
      end loop;
      do_output_housekeeping (the_GP, size, lifts);
   end put_words;

   overriding
   procedure POC (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      initialize_byte_mode_transfer(the_GP, Q_operand, set_offline);
      put_words(the_GP, Q_operand);
      set_lockouts(Q_operand);
   end POC;

   overriding
   procedure POD (the_GP      : in out GP.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean) is
   begin
      -- See the Manual Appendix 6, §5.2, p.303.
      POC(the_GP, Q_operand, set_offline);
   end POD;

   GP0 : aliased GP.device (GP0_number,
                            kind    => GP_kind,
                            unit    => 0,
                            quantum => GP_quantum,
                            is_slow => True);

   procedure switch_the_shared_buffer_onto_GP0 is
   begin
      Initialize(GP0);
   end switch_the_shared_buffer_onto_GP0;

end IOC.two_shift.GP;
