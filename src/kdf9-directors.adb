-- kdf9.directors.adb
--
-- Implement the system call API (OUTs) of the supported KDF9 Directors.
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

with dumping;
with exceptions;
with formatting;
with HCI;
with host_IO;
with IOC;
with IOC.equipment;
with IOC.fast.DR.OUTs;
with IOC.fast.FD.OUTs;
with IOC.fast.MT.OUTs;
with IOC.slow.shift.SI;
with IOC.slow.shift.TR;
with KDF9_char_sets;
with KDF9.CPU;
with KDF9.store;
with logging.file;
with settings;
with state_display;
with tracing;

with environmental_value_of;
with KDF9.Directors.do_TSD_IO;

use  dumping;
use  exceptions;
use  formatting;
use  HCI;
use  host_IO;
use  IOC;
use  IOC.equipment;
use  IOC.fast.DR.OUTs;
use  IOC.fast.FD.OUTs;
use  IOC.fast.MT.OUTs;
use  IOC.slow.shift.SI;
use  IOC.slow.shift.TR;
use  KDF9_char_sets;
use  KDF9.store;
use  logging.file;
use  settings;
use  state_display;
use  tracing;

package body KDF9.Directors is

   -- This is the actual wall clock time at which the program was loaded.
   -- If signature hashing is enabled, it stays at zero to get a repeatable hash.
   the_time_of_loading : KDF9.us := 0;

   -- Set the base for virtual elapsed time reckoning.
   procedure set_the_time_of_loading (the_time : in KDF9.us) is
   begin
      the_time_of_loading := the_time;
   end set_the_time_of_loading;

   -- is_free_for_explicit_allocation keepa a note of explicitly requested allocations by OUT 5.
   -- Allocations by OUT 8 emulation prevent LIVs on the the pseudo-spooled devices.
   -- These internal allocations by OUT 8 must not cause a reservation by OUT 5 to fail.

   is_free_for_explicit_allocation : array(KDF9.buffer_number) of Boolean := (others => True);

   procedure set_ancestor_to_rest (OUT_number : in KDF9.word) is
   begin
      if the_log_is_wanted and pre_overlay_state_is_enabled then
         show_final_state("before overlay by OUT" & OUT_number'Image);
      end if;

      if the_log_is_wanted and nr_of_post_dumping_areas /= 0 then
         log_new_line;
         log_rule;
         log_title("Post-run Dump:");
         print_postrun_dump_areas;
      end if;

      remove_prerun_dump_areas;
      remove_postrun_dump_areas;

      clear_retro_FIFO;
      clear_IOC_FIFO;
      clear_the_histogram;
      clear_the_profile;
      the_profile_is_wanted := False;
      the_INS_plot_is_wanted := False;
   end set_ancestor_to_rest;

   procedure prepare_successor is
   begin
      poke_all_amendments;
      save_the_initial_jump;

      the_program_has_been_analysed := False;
      show_all_prerun_dump_areas;

      -- Setting NIA must follow loading, as it fetches E0 into the IWBs.
      set_NIA_to((0, 0));
      the_V_bit_is_set := False;
      the_T_bit_is_set := False;
   end prepare_successor;

   procedure complete_TSD_OUT_2 (time_limit : in KDF9.word) is
   begin
      set_ancestor_to_rest(OUT_number => 2);
      log_API_message("OUT 2: ICR ="
                    & ICR'Image
                    & "; RAN/EL ="
                    & the_CPU_time'Image
                    & " /"
                    & KDF9.us'Image(the_clock_time)
                    & " KDF9 us"
                    & "; new time limit = "
                    & KDF9.word'Image(time_limit/2**24) &"s"
                     );

      get_settings_from_file("2");
      install_GP0;
      prepare_successor;
      store_halfword(time_limit, 1, 0);
      store_halfword((KDF9.word'(max_address)) * 2**24, 1, 1);

      log_new_line;
      display_execution_modes;
      reset_the_program_state;
   end complete_TSD_OUT_2;

   procedure overlay_a_new_program (program_name : in String) is
      overlay : constant String := environmental_value_of("KDF9_BINARY", default => "Binary")
                                 & "/"
                                 & program_name;
      W : KDF9.word;
   begin
      if program_name = "" then
         fail_OUT(1, "the given program name is an empty string");
      end if;

      if program_name = "KMW0201--UPU" then
         -- The Whetstone Controller is trying to overlay itself with the Translator.
         -- This is so inconvenient in practice that I simply prevent it.
         notify_termination;
         log_API_message("OUT 1: ee9 will not return to the Whetstone Translator",
                         skip => 2
                        );
         raise program_exit;
      end if;

      complete_all_extant_transfers;  -- To get an accurate elapsed time.
      is_free_for_explicit_allocation := (others => True);
      set_ancestor_to_rest(OUT_number => 1);
      log_API_message("OUT 1: ICR ="
                    & ICR'Image
                    & "; RAN/EL ="
                    & the_CPU_time'Image
                    & " /"
                    & KDF9.us'Image(the_clock_time)
                    & " KDF9 us"
                     );

      if the_external_trace_is_enabled then
         log_new_line(the_external_trace_file);
         log(the_external_trace_file, "ee9: Running overlay " & overlay);
         log_new_line(the_external_trace_file);
         log_an_external_trace_header;
      end if;

      get_settings_from_file("2");
      install_GP0;

      log_new_line;
      display_execution_modes(overlay);

      W := fetch_word(1);
      load_a_program(program_file_name => overlay);
      store_word(W, 1);

      prepare_successor;
      raise mode_change_request;
   end overlay_a_new_program;


   -- These are the device-type codes to be given when requesting
   --    the allocation of a peripheral with TSD OUT 5,
   --       according to the Manual and the document:
   --          "Service Routine Library Manual" §22.13, p22-28-0.

   FW_OUT5_code : constant := 0;
   TP_OUT5_code : constant := 1;
   TR_OUT5_code : constant := 2;
   LP_OUT5_code : constant := 3;
   CR_OUT5_code : constant := 4;
   FP_OUT5_code : constant := 5;      -- Ferranti 5-channel Tape punch
   CP_OUT5_code : constant := 7;
   GP_OUT5_code : constant := 8#20#;
   SI_OUT5_code : constant := 8#21#;  -- Standard Interface, "Data Link, N.P.L. Special Buffer"
   FE_OUT5_code : constant := 8#65#;  -- Tape buffer link for PDP-8 on Eldon2, and perhaps COTAN
   UT_OUT5_code : constant := 8#67#;  -- Unlabelled Tape

   procedure select_the_next_device_from_among
      (device_A, device_B : in  KDF9.buffer_number;
       wanted_device_type : in  KDF9.word;
       chosen_device      : out KDF9.buffer_number) is
   begin
      if device_A /= 0                            and then
            is_free_for_explicit_allocation(device_A) then
         chosen_device := device_A;
         is_free_for_explicit_allocation(device_A) := False;
      elsif device_B /= 0                         and then
            is_free_for_explicit_allocation(device_B) then
         chosen_device := device_B;
         is_free_for_explicit_allocation(device_B) := False;
      else
         fail_OUT(5, "there is no available device of type #" & oct_of(wanted_device_type));
      end if;
   end select_the_next_device_from_among;

   procedure allocate_a_device is
      B : KDF9.buffer_number;
      W : KDF9.word;
   begin
      ensure_that_the_nest_holds_an_operand;
      W := read_top;

      case W is
         -- 8 was added to the code to pre-allocate a device.
         -- I treat pre-allocating and allocating the same way here.
         when FW_OUT5_code
            | FW_OUT5_code+8 =>
            B := 0;  -- Always allowed, no checking performed.
         when TP_OUT5_code
            | TP_OUT5_code+8
            | FP_OUT5_code
            | FP_OUT5_code+8 =>
            select_the_next_device_from_among(TP0_number, TP1_number, W, B);
         when TR_OUT5_code
            | TR_OUT5_code+8 =>
            -- N.B. the TR devices must appear in this order.
            -- TR0 is used for reading the bootstrap/problem program in KDF9 code.
            -- When there is Latin-1 data it therefore needs to go in via TR1.
            select_the_next_device_from_among(TR1_number, TR0_number, W, B);
            set_case(IOC.slow.shift.TR.device(buffer(B).all));
         when LP_OUT5_code
            | LP_OUT5_code+8 =>
            select_the_next_device_from_among(LP0_number, LP1_number, W, B);
         when CR_OUT5_code
            | CR_OUT5_code+8 =>
            select_the_next_device_from_among(CR0_number, CR1_number, W, B);
         when CP_OUT5_code
            | CP_OUT5_code+8 =>
            select_the_next_device_from_among(CP0_number, CP1_number, W, B);
         when GP_OUT5_code
            | GP_OUT5_code+8 =>
            -- There is only 1 graph plotter.
            the_graph_plotter_is_enabled := True;
            install_GP0;
            select_the_next_device_from_among(GP0_number, GP0_number, W, B);
         when SI_OUT5_code =>
            if SI0_is_enabled then
               select_the_next_device_from_among(SI0_number, SI1_number, W, B);
            else
               fail_OUT(5, "the SI buffer has not been enabled");
            end if;
         when FE_OUT5_code =>
            trap_unimplemented_feature("PDP-8 Front End Tape buffers");
         when UT_OUT5_code =>
            trap_unimplemented_feature("Unlabelled Tape buffers");
         when others =>
            fail_OUT(5, "unknown device type" & W'Image);
      end case;

      pop;
      push(KDF9.word(B));
      the_trace_operand := KDF9.word(B);
      set_state_of(buffer(B), allocated => True);

      if buffer(B).all in IOC.slow.shift.device'Class and then
            buffer(B).kind /= GP_kind                     then
         log_API_message("OUT 5: requested a device of type #"
                       & oct_of(KDF9.Q_part(W), 2)
                       & " and got "
                       & device_name_of(buffer(B).all)
                       & " on buffer #"
                       & oct_of(B, 2)
                       & ", using "
                       & (
                          if IOC.slow.shift.device(buffer(B).all).uses_Latin_1 then
                             "Latin-1"
                          else
                             "KDF9"
                         )
                       & " code"
                        );
      else
         log_API_message("OUT 5: requested a device of type #"
                       & oct_of(KDF9.Q_part(W), 2)
                       & " and got "
                       & device_name_of(buffer(B).all)
                       & " on buffer #"
                       & oct_of(B, 2)
                        );
      end if;
   end allocate_a_device;

   procedure deallocate_a_verified_device (OUT_number : in KDF9.word; B : in KDF9.buffer_number) is
   begin
      if buffer(B).kind in MT_kind | ST_kind then
         if needs_rewinding(b) then
            PMD(buffer(B).all, KDF9.Q_register'(B, 0, 0), set_offline => (OUT_number = 6));
         end if;
      elsif OUT_number = 7 then
         fail_OUT(7, "device #" & oct_of(B, 2) & ", is not a tape deck");
      end if;
      set_state_of(buffer(B), allocated => False);
      is_free_for_explicit_allocation(B) := True;
      log_API_message("OUT" & OUT_number'Image & ": released " & device_name_of(buffer(B).all));
   end deallocate_a_verified_device;

   procedure deallocate_a_device (OUT_number : in KDF9.word) is
      B : KDF9.Q_part;
   begin
      ensure_that_the_nest_holds_an_operand;
      the_trace_operand := pop;
      if the_trace_operand > 15 then
         notify_termination;
         fail_OUT(OUT_number, "#" & oct_of(the_trace_operand) & " is not a valid buffer number");
      end if;
      B := KDF9.buffer_number(the_trace_operand);
      if is_unallocated(buffer(B)) then
         fail_OUT(OUT_number, "device #" & oct_of(B, 2) & " is not allocated to this program");
      end if;
      deallocate_a_verified_device(OUT_number, B);
   end deallocate_a_device;

   procedure dispose_all_allocated_tapes (OUT_number : in KDF9.word) is
   begin
      for b in KDF9.buffer_number loop
         if buffer(b) /= null                      and then
               buffer(b).kind in MT_kind | ST_kind and then
                  is_allocated(buffer(b))              then
            deallocate_a_verified_device(OUT_number, b);
         end if;
      end loop;
   end dispose_all_allocated_tapes;

   -- Return a time in µs as 48-bit seconds to 23 integral places.
   function OUT_time (microseconds : KDF9.us)
   return KDF9.word is
      -- The time was recorded by the hardware in units of 32 us, not 1 us.
      truncated_time : constant KDF9.us := microseconds and not 31;
   begin
      if truncated_time < 2**23 * 1E6 then
         -- 2**18 / 15625 = 2**24 / 1E6, with no risk of overflow in 64 bits.
         return KDF9.word(truncated_time * 2**18 / 15625);
      else
         -- The virtual elapsed time overflows the 23-bit seconds field.
         -- This would never have happened to a real KDF9, as 2**23 seconds is over three months.
         -- No KDF9 could stay up that long!
         -- However 2**23 KDF9 seconds pass in about 5 hours of ee9 real time,
         --    so precautions have to be taken.
         raise emulation_failure with "the KDF9 has been running too long, time > 2**23 seconds";
      end if;
   end OUT_time;

   -- Emulate a subset of the Time Sharing Director's OUT API.
   procedure do_a_TSD_OUT (OUT_number : in KDF9.word) is

      P : KDF9.pair;
      W : KDF9.word;

   begin
      -- Dismiss the OUT number in N1, allowing for an empty NEST, treated as OUT 0.
      if the_nest_depth > 0 then
         pop;
      end if;

      case OUT_number is

         when 0 =>
            -- Terminate program.
            dispose_all_allocated_tapes(OUT_number => 0);
            notify_termination;
            log_API_message("OUT 0: end of run");
            raise program_exit;

         when 1 =>
            -- Overlay a nominated program, retaining allocated I/O devices.
            ensure_that_the_nest_holds_2_operands;
            P := pop;
            P := CPU.shift_logical(P, 24);
            overlay_a_new_program(program_name => trimmed(to_string(P)));

         when 2 =>
            -- Resume a program with a new time limit.
            ensure_that_the_nest_holds_an_operand;
            W := pop;
            the_trace_operand := W;
            dispose_all_allocated_tapes(OUT_number => 2);
            notify_termination;
            raise program_restart;
            -- complete_TSD_OUT_2 is called by KDF9.microcode
            --    after it has finalized the execution of the OUT instruction.

         when 3 =>
            -- Get the virtual CPU time used, allowing for previous overlays.
            W := OUT_time(the_CPU_time);
            push(W);
            the_trace_operand := W;

         when 4 =>
            do_TSD_OUT_4;

         when 5 =>
            allocate_a_device;

         when 6
            | 7 =>
            deallocate_a_device(OUT_number);

         when 8 =>
            do_TSD_IO(this_OUT => 8);

         when 9 =>
            -- Get the time of day, in seconds since midnight to 23 integral places.
            -- A TOD clock is simulated using the real TOD at which the program was
            --    loaded, and the virtual time that has elapsed since.
            W := OUT_time(the_time_of_loading + the_clock_time);
            push(W);
            the_trace_operand := W;

         when 10 =>
            do_TSD_OUT_10;

         when 11 =>
            do_TSD_OUT_11;

         when 12 =>
            do_TSD_OUT_12;

         when 13 =>
            do_TSD_OUT_13;

         when 14 =>
            do_TSD_OUT_14;

         when 16 =>
            do_TSD_IO(this_OUT => 16);

         when 17 =>
            -- In program mode, the Notional Elapsed Time is the same thing as the_clock_time.
            ensure_that_the_nest_has_room_for_2_results;
            W := OUT_time(the_CPU_time);
            push(OUT_time(the_clock_time));
            push(W);
            the_trace_operand := W;

         when 41 =>
            do_TSD_OUT_41;

         when 42 =>
            do_TSD_OUT_42;

         when 43 =>
            do_TSD_OUT_43;

         when 44 =>
            do_TSD_OUT_44;

         when 45 =>
            do_TSD_OUT_45;

         when 70 =>
            -- This is not a genuine TSD OUT, it is an expedient for debugging KAlgol,
            --   so ee9 simply erases its parameters from N1 and N2.
            ensure_that_the_nest_holds_2_operands;
            W := pop;
            the_trace_operand := W;
            W := pop;

         when 98 =>
            -- This is not a genuine TSD OUT, it is an ee9 'OUT' for setting FW output format.
            ensure_that_the_nest_holds_an_operand;
            W := pop;
            the_trace_operand := W;
            realistic_FW_output_is_wanted := the_trace_operand /= 0;

         when 99 =>
            -- This is not a genuine TSD OUT, it is an ee9 'OUT' for program instrumentation.
            -- Get present value of the Instruction Count Register (ICR) from within ee9.
            W := KDF9.word(ICR);
            push(W);
            the_trace_operand := W;

         when others =>
            trap_unimplemented_feature("OUT" & OUT_number'Image);

      end case;

   end do_a_TSD_OUT;

   -- Emulate a subset of the EGDON Director's OUT API.
   procedure do_an_EGDON_OUT (OUT_number : in KDF9.word) is
   begin
      trap_unimplemented_feature("this EGDON OUT is not yet supported");
   end do_an_EGDON_OUT;

   -- Emulate a subset of some other Director's OUT API.
   procedure do_some_other_OUT (OUT_number : in KDF9.word) is
   begin
      trap_unimplemented_feature("this non-TSD OUT is not yet supported");
   end do_some_other_OUT;

end KDF9.Directors;
