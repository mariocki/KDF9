-- kdf9.directors.adb
--
-- Implement the system call API (OUTs) of the supported KDF9 Directors.
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

with dumping;
with exceptions;
with formatting;
with HCI;
with host_IO;
with IOC;
with IOC.dispatcher;
with IOC.equipment;
with IOC.fast.MT;
with IOC.slow.shift.FW;
with IOC.slow.shift.SI;
with IOC.slow.shift.TR;
with KDF9_char_sets;
with KDF9.CPU;
with KDF9.store;
with logging.file;
with settings;
with state_display;
with tracing;

with value_of;

use  dumping;
use  exceptions;
use  formatting;
use  HCI;
use  host_IO;
use  IOC;
use  IOC.dispatcher;
use  IOC.equipment;
use  IOC.fast.MT;
use  IOC.slow.shift.FW;
use  IOC.slow.shift.SI;
use  IOC.slow.shift.TR;
use  KDF9_char_sets;
use  KDF9.store;
use  logging.file;
use  settings;
use  state_display;
use  tracing;

package body KDF9.Directors is

   procedure log_API_message (message  : in String;
                              skip     : in Natural := 1) is
   begin
      if API_logging_is_wanted then
         log_ee9_status(message, skip, True);
      end if;
   end log_API_message;

   -- This is the actual wall clock time at which the program was loaded.
   -- If signature hashing is enabled, it stays at zero to get a repeatable hash.
   the_time_of_loading : KDF9.us := 0;

   -- Set the base for virtual elapsed time reckoning.
   procedure set_the_time_of_loading (the_time : in KDF9.us) is
   begin
      the_time_of_loading := the_time;
   end set_the_time_of_loading;

   -- Emulate a subset of the EGDON Director's OUT API.
   procedure do_an_EGDON_OUT (OUT_number : in KDF9.word) is
   begin
      trap_unimplemented_feature("EGDON OUTs are not yet supported");
   end do_an_EGDON_OUT;

   -- Implement a subset of the Time Sharing Director's OUT 8 spooling API.
   procedure do_an_OUT_8 is

      function destination_device_for (the_stream : KDF9.word)
      return IOC.device_number is
      begin
         case the_stream is
            when 8#00# =>
               return 0;
            when 8#10# |8#12# |8#14# | 8#16# =>
               return TP0_number;
            when 8#11# |8#13# |8#15# | 8#17# =>
               return TP1_number;
            when 8#30#..8#37# =>
               return LP0_number;
            when 8#50#..8#57# =>
               return TP1_number;
            when 8#70#..8#77# =>
               return LP0_number;
            when others =>
               trap_invalid_operand("OUT 8: invalid stream #" & oct_of(the_stream));
         end case;
      end destination_device_for;

      the_stream : KDF9.word;
      Q, G       : KDF9.Q_register;
      FW_query   : Boolean;

      procedure prepare_OUT8_to_FW0 is
      begin
         -- The logic of FW streams is rather complex, to preserve the layout of the typescript.
         -- There are three significant aspects.

         -- 1. The message is truncated if longer than 8 words.
         if Q.M - Q.I > 8 then
            Q.M := Q.I + 8;
         end if;

         -- 2. It must not contain LS or HT;
         --       nor ';' in the last word;
         --          nor ';' other than in character 7;
         --    but anything after an End Message can safely be ignored.
         word_loop: for w in Q.I+1 .. Q.M loop
             for c in KDF9_char_sets.symbol_index'Range loop
                declare
                   s : constant KDF9_char_sets.symbol := fetch_symbol(w, c);
                begin
                   if s = KDF9_char_sets.Line_Shift                       or else
                         s = KDF9_char_sets.Tabulation                    or else
                            ((s = KDF9_char_sets.Semi_Colon) and
                             (c /= 7 or  w = Q.M or not FW_query)) then
                      trap_invalid_operand("OUT 8: failure 73, invalid data for OUT8 to FW");
                   end if;
         exit word_loop when s = KDF9_char_sets.Semi_Colon or s = KDF9_char_sets.End_Message;
                end;
             end loop;
         end loop word_loop;

         -- 3. The Director takes a new line for each OUT 8 message to the FW.
         -- It sets up the format effector(s) in the first word of the OUT 8 area.
         declare
            FW_prefix : constant KDF9.word := 8#77_77_77_77_77_77_07_02#;  -- CN LS
            package FW renames IOC.slow.shift.FW;
            the_FW : FW.device renames FW.device(buffer(0).all);
         begin
            if a_LF_was_just_read(the_FW) then
               -- Replace the redundant Line Shift with a Word Filler character.
               store_word(FW_prefix or 8#77#, Q.I);
            else
               -- The initial Line Shift is needed.
               store_word(FW_prefix, Q.I);
            end if;
         end;
      end prepare_OUT8_to_FW0;

      page_change : constant := 8#77_77_77_77_77_77_77_03#;  --  LP Page Change character

   begin  -- do_an_OUT_8
      -- Spool output.
      ensure_that_the_nest_holds_an_operand;
      Q := as_Q(pop);  -- the N2 parameter
      the_trace_operand := as_word(Q);

      -- An OUT 8 to the FW for a query must have D0 of the control word set.
      FW_query := (Q.C and 8#1_00000#) /= 0;
      if FW_query then
         Q.C := 0;
      end if;

      --
         -- OUT 8 'output spooling'.
      --

      if Q.C = Q.I and Q.I = Q.M then
         -- The N2 parameter specifies stream closure.
         flush(buffer(destination_device_for(KDF9.word(Q.C))).all);
         return;
      end if;

      -- The Q = N2 parameter specifies a block starting with the stream number.
      check_address_and_lockout(Q.I);
      the_stream := fetch_word(Q.I);
      Q.C := destination_device_for(the_stream);
      set_state_of(buffer(Q.C), allocated => True);
      check_address_and_lockout(Q.I+1);
      G := as_Q(fetch_word(Q.I+1));

      -- See the Manual, §12.6.1.
      if G.C = 4095 and then G.I = 8#177777# then
         -- The G parameter specifies output of a 'gap' suitable for the device.
         the_trace_operand := as_word(G);
         if G.M = 0 then
            return;
         end if;
         G.M := (if G.M in 1 .. 511 then G.M else 120);
         if destination_device_for(the_stream) = 0 then
            null;  -- What else could be done; fail?
         elsif destination_device_for(the_stream) in TP0_number | TP1_number then
            POE((Q.C, 0, G.M), False);   -- Write gap.
         elsif destination_device_for(the_stream) = LP0_number then
            store_word(page_change, Q.I);
            POA((Q.C, Q.I, Q.I), False); -- Write PC.
         else
            trap_invalid_operand("OUT 8: invalid device for gapping #"
                               & oct_of(destination_device_for(the_stream)));
         end if;
         return;
      end if;

      if Q.M <= Q.I then
         trap_invalid_operand("OUT 8: invalid M-part #" & oct_of(Q.M));
      end if;

      -- Perform the transfer at once (no spooling is implemented).

      if Q.C /= 0 then
         -- For non-FW streams, the first word of the OUT 8 area is not transferred.
         Q.I := Q.I + 1;
      else
         -- The logic for FW streams is more complex, to preserve the layout of the typescript.
         prepare_OUT8_to_FW0;
      end if;

      -- OUT 8 transfers always go to End Message.
      POB(Q, False);

   exception
      when host_IO.end_of_stream =>
         trap_invalid_operand("OUT 8: no device file for stream #"
                            & oct_of(KDF9.Q_part(the_stream)));
   end do_an_OUT_8;

   -- This is used to keep a note of explicitly requested allocations by OUT 5.
   -- Allocations by OUT 8 emulation prevent LIVs on the the pseudo-spooled devices.
   -- These internal allocations by OUT 8 must not cause a reservation by OUT 5 to fail.
   is_free_for_explicit_allocation : array(KDF9.buffer_number) of Boolean := (others => True);

   -- Emulate a subset of the Time Sharing Director's OUT API.
   procedure do_a_TSD_OUT (OUT_number : in KDF9.word) is

      B : KDF9.Q_part;
      W : KDF9.word;
      P : KDF9.pair;

      procedure set_ancestor_to_rest is
      begin
         if the_log_is_wanted and pre_overlay_state_is_enabled then
            show_final_state("pre-overlay");
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
         clear_interrupt_FIFO;
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

      procedure overlay_a_new_program (program_name : in String) is
         overlay : constant String := value_of("KDF9ROOT", default => "")
                                    & "Binary/"
                                    & program_name;
         W : KDF9.word;
      begin
         --
         -- Handle exceptional cases first.
         --

         if program_name = "" then
            trap_invalid_operand("OUT 1: program name is an empty string");
         end if;

         if program_name = "KMW0201--UPU" then
            -- The Whetstone Controller is trying to overlay itself with the Translator.
            -- This is so inconvenient in practice that I simply prevent it.
            log_API_message("OUT 1: ee9 will not return to the Whetstone Translator",
                            skip => 2
                           );
            raise program_exit;
         end if;

         --
         -- Do all the common housekeeping for an effected overlay.
         --

         -- Clear up the calling program.

         complete_all_extant_transfers;  -- To get an accurate elapsed time.
         is_free_for_explicit_allocation := (others => True);
         set_ancestor_to_rest;

         log_API_message("OUT 1: ICR ="
                       & ICR'Image
                       & "; RAN/EL ="
                       & the_CPU_time'Image
                       & " /"
                       & KDF9.us'Image(the_clock_time)
                       & " KDF9 us"
                        );

         -- Set up the called program.

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

         -- Word 1 is preserved across overlays.
         W := fetch_word(1);
         load_a_program(program_file_name => overlay);
         store_word(W, 1);

         prepare_successor;

         raise mode_change_request;  -- signal a new program run.

      end overlay_a_new_program;

      procedure restart_this_program_with_new_time_limit (W : in KDF9.word) is
      begin
         set_ancestor_to_rest;

         log_API_message("OUT 2: ICR ="
                       & ICR'Image
                       & "; RAN/EL ="
                       & the_CPU_time'Image
                       & " /"
                       & KDF9.us'Image(the_clock_time)
                       & " KDF9 us"
                        );
         log_API_message(
                         "OUT 2: restarts with time limit = "
                       & KDF9.word'Image(W/2**24) &"s",
                         skip => 0
                        );

         get_settings_from_file("2");

         install_GP0;

         prepare_successor;

         ICR := 0;

          -- Set the new time limit in E1U.
         store_halfword(W, 1, 0);

          -- Set the new store limit in E1L.
         store_halfword((KDF9.word'(max_address)) * 2**24, 1, 1);

         log_new_line;
         display_execution_modes;

         reset_the_program_state;

      end restart_this_program_with_new_time_limit;

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
            trap_invalid_operand("OUT 5: no device of type #"
                               & oct_of(wanted_device_type)
                               & " is available"
                                );
         end if;
      end select_the_next_device_from_among;

      procedure allocate_a_device is
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
                  trap_invalid_operand("OUT 5: SI0 has not been enabled");
               end if;
            when FE_OUT5_code =>
               trap_unimplemented_feature("PDP-8 Front End Tape buffers");
            when UT_OUT5_code =>
               trap_unimplemented_feature("Unlabelled Tape buffers");
            when others =>
               trap_invalid_operand("OUT 5: invalid device type" & W'Image);
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
                          & " using "
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
                           );
         end if;
      end allocate_a_device;

      procedure validate_buffer_number (parameter : in KDF9.word) is
      begin
         if parameter > 15 then
            raise emulation_failure
               with "invalid buffer #"
                  & oct_of(parameter)
                  & " found in OUT "
                  & OUT_number'Image;
         end if;
      end validate_buffer_number;

      procedure allocate_a_tape_with_1_word_label is
      begin
         ensure_that_the_nest_holds_an_operand;
         W := pop;
         declare
            label : constant short_label := short_label(to_string(W));
         begin
            B := -1;
            find_tape_labelled(label, B, W);  -- W is not actually used in OUT 4
            validate_buffer_number(KDF9.word(B));
            push(KDF9.word(B));
            the_trace_operand := KDF9.word(B);
            if W = 0 then
               log_API_message("OUT 4: requested a scratch tape and got '"
                             & device_name_of(buffer(B).all)
                             & "' with TSN '"
                             & to_string(W)
                             & "'"
                              );
            else
               log_API_message("OUT 4: requested a tape labelled '"
                             & String(label)
                             & "' and got "
                             & device_name_of(buffer(B).all)
                             & " with TSN '"
                             & to_string(W)
                             & "'"
                              );
            end if;
         end;
         set_state_of(buffer(B), allocated => True);
         is_free_for_explicit_allocation(B) := False;
      end allocate_a_tape_with_1_word_label;

      procedure deallocate_a_device_and_unload_a_tape is
      begin
         ensure_that_the_nest_holds_an_operand;
         W := pop;
         the_trace_operand := W;
         validate_buffer_number(W);
         B := KDF9.Q_part(W);
         if is_free_for_explicit_allocation(B) then
            trap_invalid_operand("OUT 6: device #"
                               & oct_of(B, 2)
                               & ", i.e. "
                               & device_name_of(buffer(B).all)
                               & ", is not allocated to this program"
                                );
         elsif buffer(B).kind in MT_kind | ST_kind then
            -- Rewind the tape and unload it.
            PMD(buffer(B).all, KDF9.Q_register'(B, 0, 0), set_offline => True);
         end if;
         set_state_of(buffer(B), allocated => False);
         is_free_for_explicit_allocation(B) := True;
         log_API_message("OUT 6: released " & device_name_of(buffer(B).all));
      end deallocate_a_device_and_unload_a_tape;

      procedure deallocate_a_deck_leaving_the_tape_in_situ is
      begin
         ensure_that_the_nest_holds_an_operand;
         W := pop;
         the_trace_operand := W;
         validate_buffer_number(W);
         B := KDF9.Q_part(W);
         if is_unallocated(buffer(B)) then
            trap_invalid_operand("OUT 7: device #"
                               & oct_of(B, 2)
                               & ", i.e. "
                               & device_name_of(buffer(B).all)
                               & ", is not allocated to this program"
                                );
         elsif buffer(B).kind in MT_kind | ST_kind then
            -- Rewind the tape, but do not unload it.
            PMD(buffer(B).all, KDF9.Q_register'(B, 0, 0), set_offline => False);
            set_state_of(buffer(B), allocated => False);
            is_free_for_explicit_allocation(B) := True;
            log_API_message("OUT 7: released " & device_name_of(buffer(B).all));
         else
            trap_invalid_operand("OUT 7: device #"
                               & oct_of(B, 2)
                               & ", i.e. "
                               & device_name_of(buffer(B).all)
                               & ", is not a MT"
                                );
         end if;
      end deallocate_a_deck_leaving_the_tape_in_situ;

      procedure allocate_a_tape_with_2_word_label is
      begin
         ensure_that_the_nest_holds_2_operands;
         P := pop;
         declare
            label : constant long_label := long_label(to_string(P));
         begin
            find_tape_labelled(label, B, W);
            push(W);
            push(KDF9.word(B));
            the_trace_operand := KDF9.word(B);
            validate_buffer_number(KDF9.word(B));
            log_API_message("OUT 10: requested a tape labelled '"
                          & String(label)
                          & "' and got "
                          & device_name_of(buffer(B).all)
                          & " with TSN '"
                          & to_string(W)
                          & "'"
                           );
         end;
         set_state_of(buffer(B), allocated => True);
         is_free_for_explicit_allocation(B) := False;
      end allocate_a_tape_with_2_word_label;

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
            -- The virtual elapsed time overflows the 23-bit seconds field;
            --    return a nonce and indicate overflow.
            -- This would never have happened to a real KDF9,
            --    as 2**23 seconds is over three months.
            -- No KDF9 could stay up that long!
            -- However 2**23 KDF9 seconds pass in about 5 hours of ee9 real time,
            --    so precautions have to be taken.
            trap_invalid_operand("the time for OUT 3/9/17 exceeds the allowed range");
         end if;
      end OUT_time;

   begin -- do_a_TSD_OUT
      -- Dismiss the OUT number in N1, allowing for an empty NEST, treated as OUT 0.
      if the_nest_depth > 0 then
         pop;
      end if;

      case OUT_number is

         when 0 =>
            -- Terminate program.
            log_API_message("OUT 0: end of run");
            notify_termination;
            dispose_all_allocated_tapes;
            raise program_exit;

         when 1 =>
            ensure_that_the_nest_holds_2_operands;
            P := pop;
            P := CPU.shift_logical(P, 24);
            overlay_a_new_program(program_name => trimmed(to_string(P)));

         when 2 =>
            -- Restart a newly self-overwritten program.
            ensure_that_the_nest_holds_an_operand;
            W := pop;
            the_trace_operand := W;
            restart_this_program_with_new_time_limit(W);
            notify_termination;
            dispose_all_allocated_tapes;

         when 3 =>
            -- Get the virtual CPU time used, allowing for previous overlays.
            W := OUT_time(the_CPU_time);
            push(W);
            the_trace_operand := W;

         when 4 =>
            allocate_a_tape_with_1_word_label;

         when 5 =>
            allocate_a_device;

         when 6 =>
            deallocate_a_device_and_unload_a_tape;

         when 7 =>
            deallocate_a_deck_leaving_the_tape_in_situ;

         when 8 =>
            do_an_OUT_8;

         when 9 =>
            -- Get the time of day, in seconds since midnight to 23 integral places.
            -- A TOD clock is simulated using the real TOD at which the program was
            --    loaded, and the virtual time that has elapsed since.
            W := OUT_time(the_time_of_loading + the_clock_time);
            push(W);
            the_trace_operand := W;

         when 10 =>
            allocate_a_tape_with_2_word_label;

         when 17 =>
            -- Get the CPU Time and the Notional Elapsed Time in seconds to 23 integral places.
            -- In program mode, the Notional Elapsed Time is the same thing as the_clock_time.
            ensure_that_the_nest_has_room_for_2_results;
            W := OUT_time(the_CPU_time);
            push(OUT_time(the_clock_time));
            push(W);
            the_trace_operand := W;

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

end KDF9.Directors;
