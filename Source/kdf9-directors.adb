-- kdf9.directors.adb
--
-- Implement the APIs  (OUTs) of the supported KDF9 Directors.
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

with dumping;
with exceptions;
with formatting;
with HCI;
with IO;
with IOC;
with IOC.assignment;
with IOC.two_shift.FW;
with IOC.two_shift.TR;
with IOC.magtape;
with KDF9.CPU;
with KDF9.store;
with logging.file;
with settings;
with state_display;
with toggle_the_shared_buffer;
with tracing;

use  dumping;
use  exceptions;
use  formatting;
use  HCI;
use  IOC;
use  IOC.assignment;
use  IOC.two_shift.FW;
use  IOC.two_shift.TR;
use  IOC.magtape;
use  KDF9.store;
use  logging.file;
use  settings;
use  state_display;
use  tracing;

package body KDF9.Directors is

   pragma Unsuppress(All_Checks);

   procedure log_API_message (message  : in String;
                              skip     : in Natural := 1;
                              complete : in Boolean := True) is
   begin
      if API_logging_is_requested then
         log_ee9_status(message, skip, complete);
      end if;
   end log_API_message;

   -- This is the actual wall clock time at which the program was loaded.
   -- If signature hashing is enabled, it stays at zero to get a repeatable hash.
   the_time_of_loading : KDF9.microseconds := 0;

   -- Set the base for virtual elapsed time reckoning.
   procedure set_the_time_of_loading (the_time : in KDF9.microseconds) is
   begin
      the_time_of_loading := the_time;
   end set_the_time_of_loading;

   -- Emulate a subset of the EGDON Director's OUT API.
   procedure do_an_EGDON_OUT (OUT_number : in KDF9.word) is
   begin
      -- STUB: EGDON OUTs are not yet supported.
      raise NYI_trap with "EGDON OUTs are not yet supported";
   end do_an_EGDON_OUT;

   -- Implement a subset of the Time Sharing Director's OUT 8 API.
   procedure do_an_OUT_8 is

      function destination_device_for (the_stream : KDF9.word)
      return IOC.device_number is
      begin
         case the_stream is
            when 8#00#..8#07# =>
               return FW0_number;
            when 8#10#..8#16# =>
               return TP0_number;
            when 8#17# =>
               return TP1_number;
            when 8#20#..8#27# | 8#60#..8#67# =>
               return TP1_number;
            when 8#30#..8#37# | 8#70#..8#77# =>
               return LP0_number;
            when others =>
               log_ee9_status("OUT 8: invalid stream #" & oct_of(the_stream));
               trap_invalid_instruction("OUT 8: invalid stream number");
               return(ND0_number);  -- Should never happen, due to trap above.
         end case;
      end destination_device_for;

      the_stream : KDF9.word;
      Q          : KDF9.Q_register;
      FW_query   : Boolean;

      procedure prepare_OUT8_to_FW0 is
      begin
         -- The logic of FW streams is rather complex, to preserve the layout of the typescript.
         -- There are three significant aspects.

         -- 1. The message is truncated if longer than 8 words.
         if Q.M - Q.I > 8 then
            Q.M := Q.I + 8;
         end if;

         -- 2. It must not contain LS or HT; ';' in the last word; nor ';' other than in byte 7;
         --    but anything after an End Message can safely be ignored.
         word_loop: for w in Q.I+1 .. Q.M loop
            for c in KDF9.symbol_number'Range loop
               declare
                  s : constant KDF9.symbol := fetch_symbol(w, c);
               begin
                  if s = KDF9.Line_Shift                       or else
                        s = KDF9.Tabulation                    or else
                           ((s = KDF9.Semi_Colon) and
                            (c /= 7 or  w = Q.M or not FW_query)) then
                     log_ee9_status("OUT 8: failure 730, invalid data for FW");
                     trap_invalid_instruction("OUT 8: invalid data for FW");
                  end if;
         exit word_loop when s = KDF9.Semi_Colon or s = KDF9.End_Message;
               end;
            end loop;
         end loop word_loop;

         -- 3. The Director takes a new line for each OUT 8 message to the FW.
         -- It sets up the format effector(s) in the first word of the OUT 8 area.
         declare
            FW_prefix : constant KDF9.word := 8#77_77_77_77_77_77_07_02#;  -- CN LS
            package FW renames IOC.two_shift.FW;
            the_FW : FW.device renames FW.device(buffer(FW0_number).all);
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

   begin
      pop;
      Q := as_Q(pop);  -- the N2 parameter

      -- An OUT 8 to the FW for a query must have D0 of the control word  set.
      FW_query := (Q.C and 8#1_00000#) /= 0;
      if FW_query then
         Q.C := FW0_number;
      end if;

      --
         -- OUT 8 'output spooling'.
      --

      if Q.C = Q.I and Q.I = Q.M then
         -- The N2 parameter specifies stream closure.
         the_trace_operand := as_word(Q);
         flush(buffer(destination_device_for(KDF9.word(Q.C))).all);
         log_API_message("OUT 8: closes stream #" & oct_of(Q.C, 2));
         return;
      end if;

      take_note_of(as_word(Q));

      -- The N2 parameter specifies a block starting with the stream number.
      validate_access(Q.I);
      the_stream := fetch_word(Q.I);
      the_trace_operand := the_stream;
      Q.C := destination_device_for(the_stream);

      if is_unallocated(buffer(Q.C)) then
         set_state_of(buffer(Q.C), allocated => True);
      end if;

      if Q.M <= Q.I then
         log_ee9_status("OUT 8: invalid M-part #" & oct_of(Q.M));
         trap_invalid_instruction("OUT 8: invalid M-part");
      end if;

      -- Perform the transfer at once (no spooling is implemented).

      if Q.C /= FW0_number then
         -- For non-FW streams, the first word of the OUT 8 area is not transferred.
         Q.I := Q.I + 1;
      else
         -- The logic for FW streams is more complex, to preserve the layout of the typescript.
         prepare_OUT8_to_FW0;
      end if;

      -- OUT 8 transfers always go to End Message.
      POB(Q, False);

   exception
      when IO.end_of_stream =>
         log_ee9_status("OUT 8: no device file for stream #"
                      & oct_of(KDF9.Q_part(the_stream))
                       );
         trap_invalid_instruction("OUT 8: no device file for stream");
   end do_an_OUT_8;

   -- Emulate a subset of the Time Sharing Director's OUT API.
   procedure do_a_TSD_OUT (OUT_number : in KDF9.word) is

      -- Return a time in µs as 48-bit seconds to 23 integral places.
      function OUT_time (microseconds : KDF9.microseconds)
      return KDF9.word is
      begin
           -- 2**18 / 15625 = 2**24 / 1E6, with reduced risk of overflow.
         return KDF9.word(microseconds * 2**18 / 15625);
      end OUT_time;

      procedure validate_buffer_number (parameter : in KDF9.Q_part) is
      begin
         if parameter > 15 then
            log_ee9_status("the given device #"
                          & oct_of(parameter)
                          & " is invalid in OUT "
                          & KDF9.word'Image(out_number)
                           );
            trap_invalid_instruction("invalid buffer number");
         end if;
      end validate_buffer_number;

      procedure select_the_next_device_from_among
         (device_0, device_1 : in  KDF9.buffer_number;
          wanted_device_type : in  KDF9.word;
          chosen_device      : out KDF9.buffer_number) is
      begin
         if is_unallocated(buffer(device_0)) then
            chosen_device := device_0;
         elsif is_unallocated(buffer(device_1)) then
            chosen_device := device_1;
         else
            trap_invalid_instruction("OUT 5: no device of type"
                                   & KDF9.word'Image(wanted_device_type)
                                   & " is available");
         end if;
      end select_the_next_device_from_among;

      the_tape_punch_WAS_configured : constant Boolean := not the_graph_plotter_is_configured;
      B : KDF9.Q_part;
      W : KDF9.word;
      P : KDF9.pair;

   begin
      case OUT_number is

         when 0 =>
            -- Terminate program.
            ensure_that_the_nest_holds_an_operand;
            pop;
            log_API_message("OUT 0: end of run");
            raise program_exit;

         when 1 =>
            -- Terminate program and overlay a new program.
            ensure_that_the_nest_holds(at_least => 3);
            pop;
            P := pop;
            P := CPU.shift_logical(P, 24);
            if trimmed(to_string(P)) = "KMW0301--UPU" then
               complete_all_extant_transfers;  -- To get an accurate elapsed time.
               log_API_message("OUT 1: ICR ="
                             & KDF9.order_counter'Image(ICR)
                             & "; RAN/EL ="
                             & KDF9.microseconds'Image(the_CPU_time)
                             & " /"
                             & KDF9.microseconds'Image(the_clock_time)
                             & " KDF9 us"
                              );
               if the_log_is_wanted and nr_of_post_dumping_areas /= 0 then
                  log_rule;
                  log_title("Post-run Dump:");
                  print_postrun_dump_areas;
               end if;

               remove_prerun_dump_areas;
               remove_postrun_dump_areas;
               get_settings_from_file("2");

               if the_tape_punch_WAS_configured then
                  toggle_the_shared_buffer;
               end if;

               log_API_message("OUT 1: the Whetstone Controller overlays the Translator",
                               skip => 0);
               W := fetch_word(1);
               log_new_line;
               reattach_TR0(to_the_file => "Binary/KMW0301--UPU");
               load_a_program;
               store_word(W, 1);
               the_V_bit := 0;
               the_T_bit := 0;
               -- Setting NIA must follow program loading, as it fetches E0 into the IWBs.
               set_NIA_to((0, 0));
               clear_retro_FIFO;
               clear_IOC_FIFO;
               clear_interrupt_FIFO;
               display_execution_modes;

               if the_external_trace_is_enabled then
                  log_new_line(the_external_trace_file);
                  log(the_external_trace_file,
                      "ee9: Running overlay KMW0301--UPU, the Whetstone Controller.");
                  log_new_line(the_external_trace_file);
                  log_an_external_trace_header;
               end if;

               the_code_space_has_been_marked := False;
               raise mode_change_request;

            elsif trimmed(to_string(P)) = "KMW0201--UPU" then

               -- Thw Whetstone Controller is trying to overlay itself with the Translator.
               -- This is so inconvenient in practice that I simply prevent it.
               log_API_message("OUT 1: ee9 will not return to the Whetstone Translator");
               raise program_exit;

            else

               -- Some other overlay is being attempted, but this is not yet implemented.
               log_API_message("OUT 1: ee9 does not yet support an overlay by '"
                             & trimmed(to_string(P))
                             & "'");
               raise NYI_trap;

            end if;  -- OUT 1

         when 2 =>
            -- Restart newly self-overwritten program.
            ensure_that_the_nest_holds_2_operands;
            pop;
            W := pop;
            the_trace_operand := W;
            reset_the_program_state;
            for b in buffer'Range loop
               if not is_unallocated(buffer(b)) then
                  set_state_of(buffer(b), allocated => False);
               end if;
            end loop;
            set_state_of(buffer(0), allocated => True);  -- FW0 is always pre-allocated.
             -- Set the new time limit in E1U.
            store_halfword(W * 2**24, KDF9.address'(1), 0);
             -- Set the new store limit in E1L.
            store_halfword((KDF9.word'(max_address)) * 2**24, KDF9.address'(1), 1);
            log_API_message("OUT 2: restart with time limit = " & KDF9.word'Image(W));

         when 3 =>
            -- Get the virtual CPU time used, allowing for previous overlays.
            ensure_that_the_nest_holds_an_operand;
            W := OUT_time(the_CPU_time);
            write_top(W);
            the_trace_operand := W;

         when 4 =>
            -- Allocate the deck with a tape having a 1-word label.
            ensure_that_the_nest_holds_2_operands;
            pop;
            W := pop;
            take_note_of(W);
            declare
               label : constant magtape.short_label := magtape.short_label(to_string(W));
            begin
               find_tape_labelled(label, B, W);  -- W is not used in OUT 4
               push(KDF9.word(B));
               the_trace_operand := read_top;
               validate_buffer_number(B);
               if W = 0 then
                  log_API_message("OUT 4: requests MT labelled ""_Z_E_R_O""; gets "
                                & logical_device_name_of(buffer(B).all)
                                & ", TSN """
                                & trimmed(to_string(W))
                                & """"
                                 );
               else
                  log_API_message("OUT 4: requests MT labelled """
                                & trimmed(String(label))
                                & """; gets "
                                & logical_device_name_of(buffer(B).all)
                                & ", TSN """
                                & trimmed(to_string(W))
                                & """"
                                 );
               end if;
            end;
            set_state_of(buffer(B), allocated => True);

         when 5 =>
            -- Allocate an I/O device.
            ensure_that_the_nest_holds_2_operands;
            pop;
            W := read_top;
            take_note_of(W);

            case W is
               -- 8 was added to the code to pre-allocate a device.
               -- I treat pre-allocating and allocating the same way here.
               when FW_OUT5_code | FW_OUT5_code+8 =>
                  B := FW0_number;  -- Always allowed, no checking performed.
               when TP_OUT5_code | TP_OUT5_code+8 =>
                  select_the_next_device_from_among(TP0_number, TP1_number, W, B);
               when TR_OUT5_code | TR_OUT5_code+8 =>
                  select_the_next_device_from_among(TR1_number, TR0_number, W, B);
                  set_case(IOC.two_shift.TR.device(buffer(B).all));
               when LP_OUT5_code | LP_OUT5_code+8 =>
                  select_the_next_device_from_among(LP0_number, LP0_number, W, B);
               when CR_OUT5_code | CR_OUT5_code+8 =>
                  select_the_next_device_from_among(CR0_number, CR0_number, W, B);
               when CP_OUT5_code | CP_OUT5_code+8 =>
                  select_the_next_device_from_among(CP0_number, CP0_number, W, B);
               when GP_OUT5_code | GP_OUT5_code+8 =>
                  if the_graph_plotter_is_configured then
                     select_the_next_device_from_among(GP0_number, GP0_number, W, B);
                  else
                     trap_invalid_instruction("OUT 5: no graph plotter has been configured");
                  end if;
               when others =>
                  trap_invalid_instruction("OUT 5: requested an invalid device type #"
                                         & oct_of(W));
            end case;

            pop;
            push(KDF9.word(B));
            the_trace_operand := read_top;
            set_state_of(buffer(B), allocated => True);
            log_API_message("OUT 5: requests a device of type #"
                          & oct_of(KDF9.Q_part(W), 2)
                          & "; gets "
                          & logical_device_name_of(buffer(B).all)
                           );

         when 6 =>
            -- Deallocate an I/O device.
            ensure_that_the_nest_holds_2_operands;
            pop;
            W := pop;
            the_trace_operand := W;
            B := KDF9.Q_part(W);
            validate_buffer_number(B);
            if is_unallocated(buffer(B)) then
               trap_invalid_instruction("OUT 6: device #"
                                      & oct_of(B)
                                      & ", i.e. "
                                      & logical_device_name_of(buffer(B).all)
                                      & ", is not allocated to this program"
                                       );
            elsif buffer(B).kind = MT_kind then
               -- Rewind the tape and unload it.
               PMD(buffer(B).all, KDF9.Q_register'(B, 0, 0), set_offline => True);
            end if;
            set_state_of(buffer(B), allocated => False);
            log_API_message("OUT 6: releases " & logical_device_name_of(buffer(B).all));

         when 7 =>
            -- Deallocate an allocated MT.
            ensure_that_the_nest_holds_2_operands;
            pop;
            W := pop;
            the_trace_operand := W;
            B := KDF9.Q_part(W);
            validate_buffer_number(B);
            if is_unallocated(buffer(B)) then
               trap_invalid_instruction("OUT 7: device #"
                                      & oct_of(B)
                                      & ", i.e. "
                                      & logical_device_name_of(buffer(B).all)
                                      & ", is not allocated to this program"
                                       );
            elsif buffer(B).kind = MT_kind then
               -- Rewind the tape, but do not unload it.
               PMD(buffer(B).all, KDF9.Q_register'(B, 0, 0), set_offline => False);
               set_state_of(buffer(B), allocated => False);
               log_API_message("OUT 7: releases " & logical_device_name_of(buffer(B).all));
            else
               trap_invalid_instruction("OUT 7: device #"
                                      & oct_of(B)
                                      & ", i.e. "
                                      & logical_device_name_of(buffer(B).all)
                                      & ", is not a MT"
                                       );
            end if;

         when 8 =>
            -- Spool output.
            ensure_that_the_nest_holds_2_operands;
            do_an_OUT_8;

         when 9 =>
            -- Get the time of day, in seconds since midnight to 23 integral places.
            -- A TOD clock is simulated using the real TOD at which the program was
            --    loaded, and the virtual time that has elapsed since.
            ensure_that_the_nest_holds_an_operand;
            W := OUT_time(the_time_of_loading + the_clock_time);
            write_top(W);
            the_trace_operand := W;

         when 10 =>
            -- Allocate the deck with a tape having a 2-word label.
            ensure_that_the_nest_holds(at_least => 3);
            pop;
            P := pop;
            declare
               label : constant magtape.long_label := magtape.long_label(to_string(P));
            begin
               find_tape_labelled(label, B, W);
               push(KDF9.word(B));
               take_note_of(read_top);
               push(W);
               the_trace_operand := read_top;
               validate_buffer_number(B);
               log_API_message("OUT 10: requests MT labelled """
                             & trimmed(String(label))
                             & """; gets "
                             & logical_device_name_of(buffer(B).all)
                             & ", TSN """
                             & trimmed(to_string(W))
                             & """"
                              );
            end;
            set_state_of(buffer(B), allocated => True);

         when 17 =>
            -- Get the CPU Time and the Notional Elapsed Time in seconds to 23 integral places.
            -- In program mode, the Notional Elapsed Time is the same as the_clock_time.
            ensure_that_the_nest_holds_an_operand;
            pop;
            ensure_that_the_nest_has_room_for_2_results;
            W := OUT_time(the_CPU_time);
            push(OUT_time(the_clock_time));
            push(W);
            the_trace_operand := W;

         when others =>
            raise NYI_trap;

      end case;


   end do_a_TSD_OUT;

end KDF9.Directors;

