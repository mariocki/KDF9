-- state_display.adb
--
-- Provide the comprehensive machine-state display panel KDF9 never had.
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
with Ada.Long_Float_Text_IO;
--
with disassembly;
with dumping;
with exceptions;
with FD_layout;
with formatting;
with generic_sets; pragma Elaborate_All(generic_sets);
with HCI;
with IOC;
with KDF9.compressed_opcodes;
with KDF9.CPU;
with KDF9.PHU_store;
with KDF9.store;
with Latin_1;
with logging.file;
with logging.panel;
with settings;
with tracing;


use  Ada.Exceptions;
use  Ada.Long_Float_Text_IO;
--
use  disassembly;
use  dumping;
use  exceptions;
use  FD_layout;
use  formatting;
use  HCI;
use  IOC;
use  KDF9.compressed_opcodes;
use  KDF9.CPU;
use  KDF9.PHU_store;
use  KDF9.store;
use  Latin_1;
use  logging.file;
use  logging.panel;
use  settings;
use  tracing;

package body state_display is

   pragma Unsuppress(All_Checks);

   procedure show_Q_register (the_Q_register : in KDF9.Q_register;
                              width          : in Positive := 8;
                              with_FD_C_part : in Boolean  := False) is
      the_buffer : constant KDF9.Q_part := the_Q_register.C mod 2**4;
   begin
      log('Q');
      if with_FD_C_part and the_buffer = FD0_number then
         log(justified(formatted_as_FD_command(the_Q_register), width));
      else
         log(justified("#" & oct_of(the_Q_register.C, width-2), width));
      end if;
      log("/");
      log(justified("#" & oct_of(the_Q_register.I, width-2), width));
      log("/");
      log(justified("#" & oct_of(the_Q_register.M, width-2), width));
   end show_Q_register;

   procedure show_Q_in_decimal (the_Q_register : in KDF9.Q_register;
                                width          : in Positive := 7) is
   begin
      log('Q');
      log(justified(CPU.signed_Q_part'Image(resign(the_Q_register.C)), width));
      log("/");
      log(justified(CPU.signed_Q_part'Image(resign(the_Q_register.I)), width));
      log("/");
      log(justified(CPU.signed_Q_part'Image(resign(the_Q_register.M)), width));
   end show_Q_in_decimal;

   procedure show_in_syllables_form (the_word : in KDF9.word) is
      word : KDF9.word := the_word;
      syllable : KDF9.syllable;
   begin
      for b in 0 .. 5 loop
         word := rotate_word_left(word, 8);
         syllable := KDF9.syllable(word and 8#377#);
         log("#");
         log(justified(oct_of(syllable), 3));
         log(" ");
      end loop;
   end show_in_syllables_form;

   function glyph_for (char : Character)
   return Character is
   begin
      if char = LF then
         return '®';
      elsif char = FF then
         return '©';
      elsif char = HT then
         return '¬';
      elsif char = SUB then
         return KDF9.W_F;
      else
         return char;
      end if;
   end glyph_for;

   procedure show_in_LP_form (the_word : in KDF9.word) is
      word : KDF9.word := the_word;
      data : String(1 .. 8);
   begin
      for b in reverse data'Range loop
         data(b) := glyph_for(to_LP(KDF9.symbol(word and 8#77#)));
         word := shift_logical(word, -6);
      end loop;
      log(data);
   end show_in_LP_form;

  procedure show_in_various_formats (the_word : in KDF9.word;
                                      column   : in Positive := 5) is
      image : String(1 .. 18);
   begin
      log_octal(the_word);
      log(" = ");
      log(justified(trimmed(CPU.signed'Image(resign(the_word))), 16));
      log(" = ");
      Put(image, host_float(CPU.float(the_word)), Aft => 11, Exp => 2);
      log(trimmed(image));
      log_new_line;
      tab_log_to(column);
      log(" = ");
      show_Q_register(as_Q(the_word));
      log("   = ");
      show_Q_in_decimal(as_Q(the_word));
      log_new_line;
      tab_log_to(column);
      log(" = ");
      show_in_syllables_form(the_word);
      log("= """);
      show_in_LP_form(the_word);
      log("""");
   end show_in_various_formats;

   procedure show_progress is
   begin
      log("ORDERS:     ");
      log(justified(KDF9.order_counter'Image(ICR), 10));
      log_line(" executed (ICR)");
      log("CPU TIME:   ");
      log(justified(KDF9.microseconds'Image(the_CPU_time), 10));
      log_line(" KDF9 us. (RAN)");
      log("CLOCK TIME: ");
      log(justified(KDF9.microseconds'Image(the_clock_time), 10));
      log_line(" KDF9 us. (EL)");
   end show_progress;

   procedure show_Director_registers is
   begin
      log("The CPU is in ");
      log_line(KDF9.CPU_state'Image(the_CPU_state));
      log("CONTEXT:  ");
      log_line(justified(KDF9.context'Image(the_context), 1));
      log("PRIORITY: ");
      log_line(justified(KDF9.priority'Image(CPL), 1));
      log("BA:       ");
      log_line(justified("#" & oct_of(BA), 6));
      log("NOL:      ");
      log_line(justified("#" & oct_of(NOL), 6));
      log("CPDAR:    ");
      for i in KDF9.buffer_number loop
         if the_CPDAR(i) = 1 then log("A"); else log("U"); end if;
      end loop;
      log_new_line;
      log_line("PHU stores:");
      for p in KDF9.priority loop
         log("CPL" & KDF9.priority'Image(p) & " is ");
         if PHU(p).is_held_up then
            if PHU(p).blockage.reason = buffer_busy then
               log("waiting for ");
               log(logical_device_name_of(IOC.device_number(PHU(p).blockage.buffer_nr)));
               log(" on buffer #");
               log(oct_of(PHU(p).blockage.buffer_nr, 2));
               if PHU(p).blockage.INTQq_wait = 1 then
                  log(", because of INTQq");
               end if;
            else
               log("locked out at");
               log(KDF9.PHU_store.group_address'Image(PHU(p).blockage.group_nr));
            end if;
         else
            log("idle");
         end if;
         log_new_line;
      end loop;
      log_line("RFIR (Interrupt Flags):");
      log("PR:       ");
      log_line(Boolean'Image(the_RFIR(PR_flag)));
      log("FLEX:     ");
      log_line(Boolean'Image(the_RFIR(FLEX_flag)));
      log("LIV:      ");
      log_line( Boolean'Image(the_RFIR(LIV_flag)));
      log("NOUV:     ");
      log_line( Boolean'Image(the_RFIR(NOUV_flag)));
      log("EDT:      ");
      log_line(Boolean'Image(the_RFIR(EDT_flag)));
      log("OUT:      ");
      log_line( Boolean'Image(the_RFIR(OUT_flag)));
      log("LOV:      ");
      log_line( Boolean'Image(the_RFIR(LOV_flag)));
      log("RESET:    ");
      log_line(Boolean'Image(the_RFIR(RESET_flag)));
   end show_Director_registers;

   procedure show_V_and_T is
   begin
      if the_V_bit/= 0 or the_T_bit /= 0 then
         log_new_line;
         if the_V_bit /= 0 then
            log("V is set. ");
         else
            log("V is clear. ");
         end if;
         if the_T_bit /= 0 then
            log("T is set. ");
         else
            log("T is clear. ");
         end if;
      end if;
   end show_V_and_T;

   procedure show_nest is
   begin
      if the_nest_depth = 0 then
         log_line("The NEST is empty.");
      else
         log_line("NEST:");
         for i in reverse KDF9.nest_depth loop
            if i < the_nest_depth then
               log(justified("N" & trimmed(KDF9.nest_depth'Image(the_nest_depth-i)), 3) & ": ");
               log_new_line;
               show_in_various_formats(the_nest(i));
               log_new_line;
            end if;
         end loop;
      end if;
   end show_nest;

   procedure show_sjns is
   begin
      if the_sjns_depth = 0 then
         log_line("The SJNS is empty.");
      else
         log_line("SJNS:");
      end if;
      for i in reverse KDF9.sjns_depth loop
         if i < the_sjns_depth then
            log(justified("S" & trimmed(KDF9.sjns_depth'Image(the_sjns_depth-i)), 3) & ": ");
            log_line(oct_of(the_sjns(i)) & " (" & dec_of(KDF9.code_point(the_sjns(i))) & ")");
         end if;
      end loop;
   end show_sjns;

   procedure show_Q_store is
      Q_bits  : KDF9.word := 0;
   begin
      for i in KDF9.Q_store'Range loop
         Q_bits := Q_bits or as_word(the_Q_store(i));
      end loop;
      if Q_bits = 0 then
         log_line("Q store: all zero");
         return;
      else
         log_line("Q store:");
      end if;
      for i in KDF9.Q_store'Range loop
         if as_word(the_Q_store(i)) /= KDF9.word'(0) then
            log(justified("Q" & trimmed(KDF9.Q_number'Image(i)), 3) & ": ");
            show_Q_register(the_Q_store(i));
            log("  = ");
            show_Q_in_decimal(the_Q_store(i));
            log_new_line;
         end if;
      end loop;
   end show_Q_store;

   procedure show_registers is
   begin
      show_progress;
      log_new_line;
      if the_CPU_state = Director_state then
         show_Director_registers;
         log_new_line;
      end if;
      show_sjns;
      log_new_line;
      show_Q_store;
      show_V_and_T;
      log_new_line;
      show_nest;
   end show_registers;

   procedure show_order is
   begin
      log(machine_code(INS));
      log(", i.e. ");
      log(the_name_of(INS));
   end show_order;

   procedure show_execution_context is
   begin
      log("At ");
      log(oct_of(CIA));
      log(" (");
      log(dec_of(CIA));
      log(")");
      log("; ICR =");
      log(KDF9.order_counter'Image(ICR));
      log("; the instruction was ");
      show_order;
      log_new_line;
   end show_execution_context;

   procedure log_to_external_trace is
   begin
      log(the_external_trace_file, oct_of(CIA));
      tab_log_to(the_external_trace_file, 10);
      log(the_external_trace_file, KDF9.order_counter'Image(ICR));
      tab_log_to(the_external_trace_file, 20);
      if only_signature_tracing then
         log(the_external_trace_file, "#");
         log(the_external_trace_file, oct_of(the_digital_signature));
         if the_V_bit /= 0 then
            log(the_external_trace_file, "V");
         else
            log(the_external_trace_file, " ");
         end if;
         if the_nest_depth > 0 then
            log(the_external_trace_file, "#");
            log(the_external_trace_file, oct_of(read_top));
         end if;
      else
         log(the_external_trace_file, KDF9.microseconds'Image(the_CPU_time));
         tab_log_to(the_external_trace_file, 40);
         log(the_external_trace_file, KDF9.nest_depth'Image(the_nest_depth));
         tab_log_to(the_external_trace_file, 43);
         log(the_external_trace_file, KDF9.sjns_depth'Image(the_sjns_depth));
         tab_log_to(the_external_trace_file, 46);
         if the_V_bit /= 0 then
            log(the_external_trace_file, "V");
         else
            log(the_external_trace_file, " ");
         end if;
         if the_T_bit /= 0 then
            log(the_external_trace_file, "T ");
         else
            log(the_external_trace_file, "  ");
         end if;
         tab_log_to(the_external_trace_file, 50);
         if the_nest_depth > 0 then
            log(the_external_trace_file, "#");
            log(the_external_trace_file, oct_of(read_top));
         end if;
         tab_log_to(the_external_trace_file, 68);
      end if;
      log(the_external_trace_file, " |");
      log(the_external_trace_file, the_name_of(INS));
      log_new_line(the_external_trace_file);
   end log_to_external_trace;

   procedure log_an_external_trace_header is
   begin
      log(the_external_trace_file, "LOCATION");
      tab_log_to(the_external_trace_file, 11);
      log(the_external_trace_file, "ICR");
      tab_log_to(the_external_trace_file, 20);
      if only_signature_tracing then
         log(the_external_trace_file, "DIGITAL SIGNATURE");
      else
         log(the_external_trace_file, " CPU");
         tab_log_to(the_external_trace_file, 40);
         log(the_external_trace_file, "ND");
         tab_log_to(the_external_trace_file, 43);
         log(the_external_trace_file, "SD");
         tab_log_to(the_external_trace_file, 46);
         log(the_external_trace_file, "VT");
         tab_log_to(the_external_trace_file, 50);
         log(the_external_trace_file, "[N1]");
         tab_log_to(the_external_trace_file, 68);
      end if;
      log(the_external_trace_file, " |INSTRUCTION");
      log_new_line(the_external_trace_file);
   end log_an_external_trace_header;

   procedure show_CIA_and_NIA is
   begin
      log("CIA:        ");
      log_line(justified(oct_of(CIA), 10) & " (" & justified(dec_of(CIA) & ")"));
      log("NIA:        ");
      log_line(justified(oct_of(NIA), 10) & " (" & justified(dec_of(NIA) & ")"));
   end show_CIA_and_NIA;

   procedure long_witness is
   begin
      log_new_line;
      show_execution_context;
      show_CIA_and_NIA;
      show_registers;
   end long_witness;

   procedure short_witness is
   begin
      log_new_line;
      show_execution_context;
      if the_sjns_depth > 0 then
         log(" S1: ");
         log(oct_of(the_sjns(the_sjns_depth-1)));
         log("; SJNS depth: ");
         log(justified(KDF9.sjns_depth'Image(the_sjns_depth), 3));
         log_new_line;
      end if;
      if INS.Qq /= 0 then
         log(justified("Q" & trimmed(KDF9.Q_number'Image(INS.Qq)), 3) & ": ");
         show_Q_register(the_Q_store(INS.Qq));
         log_new_line;
      end if;
      if this_op_uses_2_Q_stores(INS.syndrome) and (INS.Qq /= INS.Qk) and (INS.Qk /= 0) then
         log(justified("Q" & trimmed(KDF9.Q_number'Image(INS.Qk)), 3) & ": ");
         show_Q_register(the_Q_store(INS.Qk));
         log_new_line;
      end if;
      show_V_and_T;
      show_nest;
   end short_witness;

   procedure show_histogram is

      function summed_counts (from, to : KDF9.syllable)
      return KDF9.order_counter is
         sum : KDF9.order_counter := 0;
      begin
         for i in from .. to loop
            sum := sum + the_histogram(i);
         end loop;
         return sum;
      end summed_counts;

      total : KDF9.order_counter;

      procedure log_bin (bin    : in KDF9.syllable;
                         sum    : in KDF9.order_counter;
                         bound  : in Long_Float := 0.0;
                         barred : in Boolean := True) is
         percent : Long_Float;
         image   : String(1 .. 6);
      begin
         if sum /= 0 then
            percent := Long_Float(sum)/Long_Float(total)*100.0;
            if percent < bound then
               return;
            end if;
            log(oct_of(bin) & ": ");
            log(the_skeleton_order(bin));
            tab_log_to(30);
            log(KDF9.order_counter'Image(sum));
            tab_log_to(40);
            Put(image, percent, Aft => 2, Exp => 0);
            log(image & "%");
            if barred then
               log(" |");
               for i in 1 .. Integer(percent) loop
                  log("#");
               end loop;
            end if;
            log_new_line;
         end if;
      end log_bin;

      procedure log_histogram (bound          : in Long_Float;
                               with_bar_chart : in Boolean) is
      begin
         for i in KDF9.syllable'(0) .. 8#167# loop
            log_bin(i, the_histogram(i), bound, barred => with_bar_chart);
         end loop;
         for i in KDF9.syllable'(8#170#) .. 8#237# loop
            log_bin(i, the_histogram(i), bound, barred => with_bar_chart);
         end loop;
         log_bin(8#240#, summed_counts(from => 8#240#, to => 8#257#), bound, with_bar_chart);
         log_bin(8#260#, summed_counts(from => 8#240#, to => 8#277#), bound, with_bar_chart);
         for i in KDF9.syllable'(8#300#) .. 8#377# loop
            log_bin(i, the_histogram(i), bound, with_bar_chart);
         end loop;
      end log_histogram;

   begin
      total := summed_counts(from => the_histogram'First, to => the_histogram'Last);
      if total = 0 then
         log_title("The histogram of executed instructions is empty.");
         return;
      end if;
      -- Print the instruction execution-frequency histogram.
      log_title("Histogram of"
              & KDF9.order_counter'Image(total)
              & " executed instructions.");
      log_histogram(bound => 0.0, with_bar_chart => True);
      log_new_line;
      log_rule;
   end show_histogram;

   function as_RFIR (K4_word : KDF9.word)
   return KDF9.RFIR is
      mask : KDF9.word := 2**16;
      RFIR : KDF9.RFIR := (others => False);
   begin
      for r in reverse KDF9.interrupt_number loop
         if (K4_word and mask) /= 0 then
            RFIR(r) := True;
         end if;
         mask := 2 * mask;
      end loop;
      return RFIR;
   end as_RFIR;

   procedure show_retro_FIFO is
      RFIR_id : constant array (KDF9.interrupt_number) of Character
              := ('P', 'F', 'I', 'N', 'E', 'S', 'O', 'R', 'Y', 'Z');
      image   : String(1 .. 18);
      RFIR    : KDF9.RFIR;
   begin
      if retro_FIFO_count = 0 then
         return;
      end if;
      log_title("Retrospective trace of all instructions.");
      tab_log_to(60);
      log_line(" ND SD VT  CPU TIME    ICR");
      for i in 1 .. retro_FIFO_count loop
         if i = 1 then
            log("Ended ");
         else
            log("After ");
         end if;
         declare
            this    : tracing.retro_FIFO_entry renames retro_FIFO(retro_FIFO_index);
            decoded : KDF9.decoded_order;
         begin
            log(oct_of(this.location) & ":");
            tab_log_to(17);
            flush;
            decoded.order := this.order;
            decode(decoded);
            log(the_name_of(decoded));
            tab_log_to(33);
            case decoded.kind is
               when one_syllable_order =>
                  if this.nested > 0 then
                     case decoded.syndrome is
                        when DIV
                           | DIVR
                           | DIVD
                           | X_frac =>
                           log(CPU.fraction'Image(fractional(this.parameter)));
                        when DIVI =>
                           log(CPU.signed'Image(resign(this.parameter)));
                        when STAND
                           | ABSF
                           | DIVDF
                           | DIVF
                           | FLOAT_9
                           | FLOATD
                           | MINUSDF
                           | MINUSF
                           | NEGDF
                           | NEGF
                           | PLUSDF
                           | PLUSF
                           | ROUNDF
                           | ROUNDHF
                           | XDF
                           | XF
                           | XPLUSF
                           | MAXF =>
                           Put(image,
                               host_float(CPU.float(this.parameter)), Aft => 11, Exp => 2);
                           log(trimmed(image));
                        when others =>
                           if this.nested > 0 then
                              log_octal(this.parameter);
                           end if;
                     end case;
                  end if;
               when two_syllable_order =>
                  case decoded.syndrome is
                     when PARQq
                        | PIAQq_PICQq_CLOQq_TLOQq
                        | PIBQq_PIDQq
                        | PIEQq_PIGQq
                        | PIFQq_PIHQq
                        | PMAQq_PMKQq_INTQq
                        | CTQq_PMBQq_PMCQq_BUSYQq
                        | PMDQq_PMEQq_PMLQq
                        | PMFQq
                        | POAQq_POCQq_POEQq_POFQq
                        | POBQq_PODQq
                        | POGQq_POLQq
                        | POHQq_POKQq =>
                        show_Q_register(as_Q(this.parameter), with_FD_C_part => True);
                     when M_PLUS_Iq
                        | M_MINUS_Iq
                        | NCq
                        | DCq
                        | POS1_TO_Iq
                        | NEG1_TO_Iq
                        | POS2_TO_Iq
                        | NEG2_TO_Iq
                        | CqTOQk
                        | IqTOQk
                        | MqTOQk
                        | QqTOQk
                        | CIqTOQk
                        | IMqTOQk
                        | CMqTOQk
                        | TO_RCIMq
                        | ADD_TO_QCIMq
                        | JCqNZS =>
                        show_Q_register(as_Q(this.parameter));
                     when Kk =>
                        case decoded.Qk is
                           when K4 =>
                              log(KDF9.word'Image(32*KDF9.word(as_Q(this.parameter).C)));
                              log("us");
                              if as_Q(this.parameter).I /= 0 then
                                 log("; RFIR: ");
                                 RFIR := as_RFIR(this.parameter);
                                 for r in KDF9.interrupt_number loop
                                    if RFIR(r) then
                                       log(RFIR_id(r)&"");
                                    end if;
                                 end loop;
                              end if;
                              if resign(this.parameter) < 0 then
                                 log("C");
                              end if;
                           when K5 | K7 =>
                              log_octal(this.parameter);
                           when others =>
                              trap_invalid_instruction;
                        end case;
                     when TO_LINK =>
                        log(oct_of(as_link(this.parameter)));
                     when LINK =>
                        log(oct_of(as_link(this.parameter)));
                     when TO_MkMq
                        | TO_MkMqQ
                        | TO_MkMqH
                        | TO_MkMqQH
                        | TO_MkMqN
                        | TO_MkMqQN
                        | TO_MkMqHN
                        | TO_MkMqQHN =>
                        log_octal(this.parameter);
                     when others =>
                        if this.nested > 0 then
                           log_octal(this.parameter);
                        end if;
                  end case;
               when normal_jump_order =>
                  case decoded.syndrome is
                     when Jr
                        | JSr =>
                        log(oct_of(as_link(this.parameter)));
                     when EXIT_9 =>
                        if this.parameter < 8 then
                           log(KDF9.word'Image(this.parameter));
                        else
                           log(oct_of(as_link(this.parameter)));
                        end if;
                     when JrCqZ
                        | JrCqNZ =>
                        show_Q_register(as_Q(this.parameter));
                     when OUT_9 =>
                        if this.parameter < 64 then
                           log(KDF9.word'Image(this.parameter));
                        elsif this.parameter > 2**47 then
                           log_octal(this.parameter);
                        else
                           show_Q_register(as_Q(this.parameter));
                        end if;
                     when JrEJ
                        | JrNEJ
                        | JrEN
                        | JrNEN =>
                           log(KDF9.word'Image(this.parameter));
                     when JrTR
                        | JrV =>
                           log(Boolean'Image(Boolean'Val(this.parameter)));
                     when JrNTR
                        | JrNV =>
                           log(Boolean'Image(not Boolean'Val(this.parameter)));
                     when others =>
                        if this.nested > 0 then
                           log_octal(this.parameter);
                        end if;
                     end case;
               when others =>
                  if this.nested > 0 then
                     log_octal(this.parameter);
                  end if;
            end case;
            tab_log_to(60);
            log(justified(KDF9.nest_depth'Image(this.nested),3));
            log(justified(KDF9.sjns_depth'Image(this.called),3));
            log(" ");
            if this.V /= 0 then
               log("V");
            end if;
            if this.T /= 0 then
               log("T");
            end if;
            tab_log_to(70);
            log(KDF9.microseconds'Image(this.CPU_time));
            tab_log_to(82);
            log(KDF9.order_counter'Image(this.ICR_value));
            log_new_line;
         end;
         retro_FIFO_index := retro_FIFO_index - 1;
      end loop;
      if retro_FIFO_count = FIFO_size then
         log("After earlier instructions, whose tracing is now lost.");
      else
         log("After the start of traced execution.");
      end if;
      log_new_line;
      log_rule;
   end show_retro_FIFO;

   procedure show_IOC_FIFO is
   begin
      if IOC_FIFO_count = 0 then return; end if;
      log_title("Retrospective trace of peripheral I/O events.");
      tab_log_to(64);
      log_line(" CPL     EL. TIME    ICR");
      for i in 1 .. IOC_FIFO_count loop
         if i = 1 then
            log("Ended ");
         else
            log("After ");
         end if;
         declare
            this : tracing.IOC_FIFO_entry renames IOC_FIFO(IOC_FIFO_index);
         begin
            log(oct_of(this.order_address) & ":");
            tab_log_to(17);
            if the_name_of(this.decoded_order) ="OUT" then
               log(the_name_of(this.decoded_order)&"8");
            else
               log(the_name_of(this.decoded_order));
            end if;
            tab_log_to(30);
            log(this.device_name);
            case this.kind is
               when store_lockout =>
                  tab_log_to(34);
                  log("Store Lockout at #");
                  log(oct_of(this.data_address));
                  tab_log_to(70);
                  log(" @"
                    & KDF9.microseconds'Image(this.initiation_time));
                  tab_log_to(84);
                  log(KDF9.order_counter'Image(this.ICR_value));
                when buffer_lockout =>
                  tab_log_to(34);
                  log("Buffer Lockout");
                  tab_log_to(70);
                  log(" @"
                    & KDF9.microseconds'Image(this.initiation_time));
                  tab_log_to(84);
                  log(KDF9.order_counter'Image(this.ICR_value));
               when start_transfer =>
                  tab_log_to(34);
                  show_Q_register(this.control_word, with_FD_C_part => True);
                  tab_log_to(62);
                  if this.is_for_Director then
                     log(" D");
                  else
                     log(" P");
                  end if;
                  log(KDF9.priority'Image(this.priority_level));
                  tab_log_to(70);
                  log(" S"
                    & KDF9.microseconds'Image(this.initiation_time));
                  tab_log_to(84);
                  log(KDF9.order_counter'Image(this.ICR_value));
               when finis_transfer =>
                  tab_log_to(34);
                  show_Q_register(this.control_word, with_FD_C_part => True);
                  tab_log_to(62);
                  if this.is_for_Director then
                     log(" D");
                  else
                     log(" P");
                  end if;
                  log(KDF9.priority'Image(this.priority_level));
                  tab_log_to(70);
                  log(" E"
                    & KDF9.microseconds'Image(this.completion_time));
                  tab_log_to(84);
                  log(KDF9.order_counter'Image(this.ICR_value));
               when test_buffer_status =>
                  tab_log_to(34);
                  show_Q_register(this.Q_register, with_FD_C_part => True);
                  tab_log_to(62);
                  log(" = ");
                  log(Boolean'Image(this.status /= 0));
                  tab_log_to(70);
                  log(" @"
                    & KDF9.microseconds'Image(this.initiation_time));
                  tab_log_to(84);
                  log(KDF9.order_counter'Image(this.ICR_value));
            end case;
            log_new_line;
         end;
         IOC_FIFO_index := IOC_FIFO_index - 1;
      end loop;
      if IOC_FIFO_count = FIFO_size then
         log_line("After earlier instructions, whose tracing is now lost.");
      else
         log_line("After the start of traced execution.");
      end if;
      log_line("Total time waiting for unoverlapped I/O to finish ="
             & KDF9.microseconds'Image((the_clock_time-the_CPU_time+500) / 1000)
             & "ms.");
      log_rule;
   end show_IOC_FIFO;

   procedure show_interrupt_FIFO is
   begin
      if interrupt_FIFO_count = 0 then return; end if;
      log_title("Retrospective trace of interrupt requests.");
      tab_log_to(54);
      log_line(" CPL     EL. TIME    ICR");
      for i in 1 .. interrupt_FIFO_count loop
         if i = 1 then
            log("Ended ");
         else
            log("After ");
         end if;
         declare
            this : tracing.interrupt_FIFO_entry renames interrupt_FIFO(interrupt_FIFO_index);
         begin
            log(oct_of(this.order_address) & ": ");
            case this.interrupt_code is
               when PR_flag =>
                  log("PR ");
               when FLEX_flag =>
                  log("FLEX ");
               when LIV_flag =>
                  log("LIV ");
               when NOUV_flag =>
                  log("NOUV ");
               when EDT_flag =>
                  log("EDT ");
               when OUT_flag =>
                  log("OUT ");
               when LOV_flag =>
                  log("LOV ");
               when RESET_flag =>
                  log("RESET");
               when others =>
                  log("?? ");
                  log(KDF9.interrupt_number'Image(this.interrupt_code));
            end case;
            tab_log_to(52);
            if this.in_Director then
               log(" D");
            else
               log(" P");
            end if;
            log(KDF9.priority'Image(this.priority_level));
            tab_log_to(60);
            log(" @"
              & KDF9.microseconds'Image(this.busy_time));
            tab_log_to(74);
            log(KDF9.order_counter'Image(this.ICR_value));
            log_new_line;
         end;
         interrupt_FIFO_index := interrupt_FIFO_index - 1;
      end loop;
      if interrupt_FIFO_count = FIFO_size then
         log("After earlier interrupts, whose tracing is now lost.");
      else
         log("After the start of traced execution.");
      end if;
      log_new_line;
      log_new_line;
   end show_interrupt_FIFO;

   procedure show_retrospective_traces is
   begin
      if the_histogram_is_enabled  then
         show_histogram;
      end if;
      if the_interrupt_trace_is_enabled then
         show_interrupt_FIFO;
      end if;
      if the_peripheral_trace_is_enabled then
         show_IOC_FIFO;
      end if;
      if the_retrospective_trace_is_enabled then
         show_retro_FIFO;
      end if;
   exception
      when error : others =>
         log_new_line;
         log_rule;
         log_error_message("Failure in ee9: unexpected exception "
                         & Exception_Information(error)
                         & " was raised in 'show_retrospective_traces'!");
         raise emulation_failure;
   end show_retrospective_traces;

   procedure show_current_state is
   begin
      show_execution_context;
      log_rule;
      show_registers;
      log_rule;
   end show_current_state;

   procedure show_final_state is
   begin
      if the_signature_is_enabled then
         log_title("Digital signature of traced orders = #"
                 & oct_of(the_digital_signature)
                 & ".");
      end if;
      if the_log_is_wanted and the_final_state_is_wanted then
         log_new_line;
         log_rule;
         log_title("Final State:");
         long_witness;
         log_rule;
         if ICR = 0 then
            return;
         end if;
         if nr_of_post_dumping_areas /= 0 then
            log_title("Post-run Dump:");
            print_postrun_dump_areas;
         end if;
         show_retrospective_traces;
      end if;
      if the_log_is_wanted then
         log_title("End of Run.");
      end if;
   end show_final_state;

   procedure show_all_prerun_dump_areas is
   begin
      if the_log_is_wanted and nr_of_pre_dumping_areas /= 0 then
         log_title("Pre-run Dump:");
         print_prerun_dump_areas;
         remove_prerun_dump_areas;
      end if;
   exception
      when error : others =>
         log_new_line;
         log_rule;
         log_error_message("Failure in ee9: unexpected exception "
                         & Exception_Information(error)
                         & " was raised in 'show_all_prerun_dump_areas'!");
         raise emulation_failure;
   end show_all_prerun_dump_areas;

   quantum     : constant := 8;
   jump_tab    : constant := 12;
   first_tab   : constant := 16;
   last_column : constant := 80;

   function is_non_blank (first : in KDF9.address)
   return Boolean is
      result : Boolean := False;
   begin
      for address in first .. first+quantum-1 loop
         result := result or (fetch_word(address) /= 0);
      end loop;
      return result;
   end is_non_blank;

   subtype converted_word is String(1..8);

   type convertor is not null access function (address : KDF9.address) return converted_word;

   procedure show_core (first, last : in KDF9.address;
                        head, side  : in String;
                        conversion  : in convertor) is

      procedure show_group (first : in KDF9.address) is
         address : KDF9.address := first;
      begin
         while address <= first+quantum-1 loop
            log(conversion(address));
            address := address + 1;
            exit when address < first;
         end loop;
      end show_group;

      address, last_address : KDF9.address := first;

   begin
      if (last-first+1) < 1 then
         return;
      end if;
      log_title("Core store interpreted as " & head & ":");
      while address <= last loop
         if is_non_blank(address) then
            log_octal(KDF9.field_of_16_bits(address));
            log(":");
            tab_log_to(jump_tab);
            log(side);
            log(" """);
            show_group(address);
            log("""");
            log_new_line;
         elsif is_non_blank(last_address) then
            log_line("======  blank  ======");
         end if;
         last_address := address;
      exit when address >= KDF9.address'Last - quantum;
         address := address + quantum;
      end loop;
      log_new_line;
   end show_core;

   function encoding_of (address : KDF9.address; code_table : output_code_table)
   return converted_word is
      result : converted_word;
   begin
      for b in KDF9.symbol_number loop
         result(Natural(b)+1) := glyph_for(code_table(fetch_symbol(address, b)));
      end loop;
      return result;
   end encoding_of;

   current_case : KDF9.symbol := KDF9.Case_Normal;

   function interpretation_of (address : KDF9.address)
   return converted_word is
      result : converted_word;
      symbol : KDF9.symbol;
      char   : Character;
   begin
      for b in KDF9.symbol_number loop
         symbol := fetch_symbol(address, b);
         if current_case = KDF9.Case_Normal then
            char := TP_CN(symbol);
         else
            char := TP_CS(symbol);
         end if;
         if symbol = KDF9.Case_Normal then
            current_case := KDF9.Case_Normal;
         elsif symbol = KDF9.Case_Shift then
            current_case := KDF9.Case_Shift;
         end if;
         result(Natural(b)+1) := glyph_for(char);
      end loop;
      return result;
   end interpretation_of;

   function case_visible (address : KDF9.address)
   return converted_word is
   begin
      return interpretation_of(address);
   end case_visible;

   function case_normal (address : KDF9.address)
   return converted_word is
   begin
      return encoding_of(address, code_table => TP_CN);
   end case_normal;

   function case_shift (address : KDF9.address)
   return converted_word is
   begin
      return encoding_of(address, code_table => TP_CS);
   end case_shift;

   function printer_code (address : KDF9.address)
   return converted_word is
   begin
      return encoding_of(address, code_table => to_LP);
   end printer_code;

   function card_code (address : KDF9.address)
   return converted_word is
   begin
      return encoding_of(address, code_table => to_CP);
   end card_code;

   function Latin_1_code (address : KDF9.address)
   return converted_word is
   begin
      return converted_word'(1..7 => Space,
                                8 => glyph_for(Character'Val(fetch_word(address) and 8#377#)));
   end Latin_1_code;

   procedure show_core_in_case_visible (first, last : in KDF9.address) is
   begin
      show_core(first, last,
                head => "characters in TR/TP code with case shifting",
                side => "  ",
                conversion => case_visible'Access);
   end show_core_in_case_visible;

   procedure show_core_in_case_normal (first, last : in KDF9.address) is
   begin
      show_core(first, last,
                head => "characters in TR/TP Normal Case code",
                side => "NC",
                conversion => case_normal'Access);
   end show_core_in_case_normal;

   procedure show_core_in_case_shift (first, last : in KDF9.address) is
   begin
      show_core(first, last,
                head => "characters in TR/TP Shift Case code",
                side => "SC",
                conversion => case_shift'Access);
   end show_core_in_case_shift;

   procedure show_core_in_print_code (first, last : in KDF9.address) is
   begin
      show_core(first, last,
                head => "characters in LP code",
                side => "LP",
                conversion => printer_code'Access);
   end show_core_in_print_code;

   procedure show_core_in_card_code (first, last : in KDF9.address) is
   begin
      show_core(first, last,head => "characters in CR/CP code",
                side => "CP",
                conversion => card_code'Access);
   end show_core_in_card_code;

   procedure show_core_in_Latin_1 (first, last : in KDF9.address) is
   begin
      show_core(first, last,
                head => "words with bits 40-47 of each in Latin_1 code",
                side => "L1",
                conversion => Latin_1_code'Access);
   end show_core_in_Latin_1;

   procedure show_core_in_tape_code (first, last : in KDF9.address) is
   begin
      show_core_in_case_visible(first, last);
   end show_core_in_tape_code;

   procedure show_core_as_word_forms (first, last  : KDF9.address) is

      procedure show_word (address : KDF9.address) is
         word : constant KDF9.word := fetch_word(address);
      begin
         log_octal(KDF9.field_of_16_bits(address));
         log(":");
         tab_log_to(jump_tab);
         show_in_various_formats(word, column => jump_tab);
         log_new_line;
         log_new_line;
      end show_word;

      procedure show_word_group (first, last  : KDF9.address) is
         last_address : KDF9.address := first;
         this_word, last_word : KDF9.word;
      begin
         this_word := fetch_word(first);
         last_word := this_word;
         show_word(first);
         for address in first+1 .. last-1 loop
            this_word := fetch_word(address);
            if this_word = last_word and address = last_address+1 then
               log_line("========  ditto  ========");
            elsif this_word /= last_word then
               show_word(address);
               last_word := this_word;
               last_address := address;
            end if;
         end loop;
         if last > first then
            show_word(last);
         end if;
      end show_word_group;

   begin
      if first > last then
         return;
      end if;
      log_title("Core store interpreted as 48-bit words:");
      show_word_group(first, last);
      log_new_line;
   end show_core_as_word_forms;

   -- Each word of code space is described by a set of flags.
   -- Flags 0 .. 5 are set iff a jump order has that syllable as target.
   -- Flag 6 is set if the word is though to be code, but not a target.
   -- Flag 7 is set if the word is though to be addressed as data.

   is_a_code_word : constant KDF9.syllable_code := 6;
   is_a_data_word : constant KDF9.syllable_code := 7;

   package word_flags is new generic_sets(member => KDF9.syllable_code);
   use word_flags;

   all_jump_targets : constant word_flags.set := (0 .. 5 => True, 6|7 => False);

   code_space_word : array (KDF9.code_location) of word_flags.set;

   procedure show_code_space_marks_for (the_operand : in KDF9.code_location) is
   begin
      log(oct_of(KDF9.code_point'(0, the_operand)));
      for s in KDF9.syllable_code range 0 .. 5 loop
         if code_space_word(the_operand)(s) then
            log("" & Character'Val(Character'Pos('0')+Natural(s)));
         else
            log(" ");
         end if;
      end loop;
      if code_space_word(the_operand)(is_a_code_word) then
         log("CODE");
      else
         log("    ");
      end if;
      if code_space_word(the_operand)(is_a_data_word) then
         log(" DATA");
      else
         log("     ");
      end if;
      log_new_line;
   end show_code_space_marks_for;
   pragma Unreferenced(show_code_space_marks_for);

   -- This is the entry point to the program, designated by the jump in E0.
   P0_start_point : KDF9.code_point;
   P0_start_word  : KDF9.code_location; -- = P0_start_point.word_number

   function "/" (word : KDF9.code_location; flag : KDF9.syllable_code)
   return Boolean is
   begin
      return code_space_word(word)(flag);
   end "/";

   function is_a_jump_target (the_point : in KDF9.code_point)
   return Boolean;
   pragma Inline(is_a_jump_target);

   function is_a_jump_target (the_operand : in KDF9.code_location)
   return Boolean;

   pragma Inline(is_a_jump_target);

   function is_a_jump_target (the_point : in KDF9.code_point)
   return Boolean is
   begin
      return the_point.word_number >= P0_start_word and then
             code_space_word(the_point.word_number)(the_point.syllable_number);
   end is_a_jump_target;

   function is_a_jump_target (the_operand : in KDF9.code_location)
   return Boolean is
   begin
      return the_operand >= P0_start_word and then
             (code_space_word(the_operand) and all_jump_targets) /= empty_set;
   end is_a_jump_target;

   procedure clear_all_code_space_words is
   begin
      code_space_word := (others => empty_set);
   end clear_all_code_space_words;

   procedure unmark_as_a_data_word (the_operand : in KDF9.code_location) is
   begin
      code_space_word(the_operand)(is_a_data_word) := False;
   end unmark_as_a_data_word;

   procedure unmark_as_a_code_word (the_operand : in KDF9.code_location) is
   begin
      code_space_word(the_operand)(is_a_code_word) := False;
   end unmark_as_a_code_word;

   procedure mark_as_a_code_word (the_operand : in KDF9.code_location) is
   begin
      code_space_word(the_operand)(is_a_code_word) := True;
      unmark_as_a_data_word(the_operand);
   end mark_as_a_code_word;

   procedure mark_as_a_jump_target (the_point : in KDF9.code_point) is
   begin
      code_space_word(the_point.word_number)(the_point.syllable_number) := True;
      mark_as_a_code_word(the_point.word_number);
   end mark_as_a_jump_target;

   procedure mark_as_a_data_word (the_operand : in KDF9.code_location) is
   begin
      code_space_word(the_operand)(is_a_data_word) := True;
      unmark_as_a_code_word(the_operand);
   end mark_as_a_data_word;

   procedure mark_all_code_blocks_and_data_blocks is

      procedure mark_all_code_blocks (the_beginning : in KDF9.code_point) is
         address : KDF9.code_point := the_beginning;
      begin
         if address.syllable_number > 5 then
            return;  -- We have blundered into non-code words.
         end if;
         -- Mark the first syllable of the block.
         mark_as_a_jump_target(the_beginning);
         -- Mark the destinations of all jumps in the block as code.
         loop
            set_NIA_to(address);
            decode_the_next_order;
            if is_an_invalid_order(INS)                                             or else
                  (address.word_number/is_a_data_word and address.syllable_number = 0) then
               return;
            else
               -- Assuming a valid code word, act on it.
               mark_as_a_code_word(address.word_number);
               case INS.kind is
                  when normal_jump_order =>
                     if not is_a_jump_target((INS.target.syllable_number, INS.target.word_number))
                            and INS.target.word_number >= P0_start_word then
                        -- Mark the jump's destination recursively.
                        -- N.B. EXIT is actioned only if it is of EXIT ARr type.
                        mark_all_code_blocks((INS.target.syllable_number, INS.target.word_number));
                     end if;
                     increment_by_3(address);
                     if INS.syndrome = JSr  then
                        -- Mark its return point.
                        mark_as_a_jump_target(address);
                     end if;
                  when one_syllable_order =>
                     increment_by_1(address);
                  when two_syllable_order =>
                     if INS.syndrome = JCqNZS then
                        -- Mark the preceding word.
                        mark_as_a_jump_target((0, address.word_number-1));
                     end if;
                     increment_by_2(address);
                  when data_access_order =>
                     increment_by_3(address);
               end case;
            end if;
            exit when address.word_number = KDF9.code_location'Last;
         end loop;
      end mark_all_code_blocks;

      procedure mark_all_data_blocks (the_beginning : in KDF9.code_point) is
         address : KDF9.code_point := the_beginning;
      begin
         if address.syllable_number > 5 then
            return;  -- We have blundered into non-code words.
         end if;
         the_code_block_handler: loop
            -- Process orders, starting at an established code word.
            set_NIA_to(address);
            decode_the_next_order;
            if (is_an_invalid_order(INS)                   or else
                  address.word_number/is_a_data_word)     and then
                     not (address.word_number/is_a_code_word) then
               -- This word is data: make sure it is not designated as code;
               --    and find the start of the next code block.
               for a in address.word_number .. KDF9.code_location'Last loop
                  address := (0, a);
                  exit when is_a_jump_target(a);
                  unmark_as_a_code_word(a);
                  mark_as_a_data_word(a);
               end loop;

               exit the_code_block_handler
                  when address.word_number = KDF9.code_location'Last;

               -- Find the syllable at which the block starts.
               for s in KDF9.syllable_code'(0) .. 5 loop
                  address.syllable_number := s;
                  exit when is_a_jump_target(address);
               end loop;

            else

               -- Assuming a valid code word, act on it.
               case INS.kind is
                  when data_access_order =>
                     if INS.operand < 8192 then
                        declare
                           operand : constant KDF9.code_location
                                   := KDF9.code_location(INS.operand);
                        begin
                           if INS.syndrome /= KDF9.compressed_opcodes.SET and then
                                 not is_a_jump_target(operand)                then
                              mark_as_a_data_word(operand);
                           end if;
                        end;
                     end if;
                     increment_by_3(address);
                  when one_syllable_order =>
                     increment_by_1(address);
                  when two_syllable_order =>
                     increment_by_2(address);
                  when normal_jump_order =>
                     increment_by_3(address);
               end case;
            end if;

            exit the_code_block_handler
               when address.word_number = KDF9.code_location'Last;

         end loop the_code_block_handler;
      end mark_all_data_blocks;

      procedure reset_wrong_data_marks (the_beginning : in KDF9.code_point) is
         address : KDF9.code_point := the_beginning;
         locus   : KDF9.code_location;
      begin
         if address.syllable_number > 5 then
            return;  -- We have blundered into non-code words.
         end if;
         -- Unmark the first instruction of the block.
         unmark_as_a_data_word(address.word_number);

         -- Unmark data marks on destinations of jumps.
         loop
            set_NIA_to(address);
            decode_the_next_order;
            if is_an_invalid_order(INS)                    or else
                  address.word_number/is_a_data_word       or else
                     not (address.word_number/is_a_code_word) then
               -- We have reached the end of the code block.
               return;
            else
               -- Assuming a valid code word, act on it.
               case INS.kind is
                  when normal_jump_order =>
                     locus := address.word_number;
                     increment_by_3(address);
                     if INS.target.word_number >= P0_start_word and then
                           INS.target.word_number/is_a_data_word    then
                        -- UNmark the jump's destination recursively.
                        reset_wrong_data_marks((INS.target.syllable_number, INS.target.word_number));
                     end if;
                     if INS.syndrome /= Jr                           and then
                           (INS.syndrome /= EXIT_9 or
                            INS.target.word_number >= P0_start_word) and then
                              locus /= address.word_number               then
                        -- It flows on, so the next word cannot be data.
                        unmark_as_a_data_word(address.word_number);
                     elsif not (address.word_number/is_a_data_word) then
                        -- The next syllable starts a block, iff it is not the end of a block.
                        set_NIA_to(address);
                        decode_the_next_order;
                        if not is_an_invalid_order(INS) then
                           mark_as_a_jump_target(address);
                        end if;
                     end if;
                  when one_syllable_order =>
                     increment_by_1(address);
                  when two_syllable_order =>
                     increment_by_2(address);
                  when data_access_order =>
                     increment_by_3(address);
               end case;
            end if;
            exit when address.word_number = KDF9.code_location'Last;
         end loop;
      end reset_wrong_data_marks;

   begin
      if the_code_space_has_been_marked then
         return;
      end if;
      clear_all_code_space_words;

      if the_execution_mode /= boot_mode and the_initial_jump_was_corrupted then
         -- We cannot sensibly locate the order words using E0  ...
         log_new_line;
         log_line("The initial jump, in E0U, was corrupted by the program!");
         log_new_line;
         show_core_as_syllables((0, 0), (5, 0));
         --  ... so restore it to the value it had on loading.
         restore_the_initial_jump;
         log_line("E0U has been restored to the value it had on loading.");
         log_new_line;
      end if;

      -- Ensure the right output format for the initial jump  ...
      mark_as_a_jump_target((0, 0));
      set_NIA_to((0, 0));
      loop
         decode_the_next_order;
         mark_as_a_code_word(CIA.word_number);
         exit when INS.kind = normal_jump_order;
      end loop;

      --  ... and for subsequent words.
      for d in CIA.word_number+1 .. INS.target.word_number-1 loop
         mark_as_a_data_word(d);
      end loop;

      -- Mark all code blocks reachable from the initial jump.
      set_NIA_to((0, 0));
      decode_the_next_order;
      P0_start_word  := INS.target.word_number;
      P0_start_point := (INS.target.syllable_number, INS.target.word_number);
      mark_all_code_blocks(P0_start_point);

      -- Mark all words clearly referenced by data fetch/store orders.
      mark_all_data_blocks(P0_start_point);

      -- Correct over-zealous marking of code as data or vice versa.

      -- Unmark any order that was accidentally marked as data.
      reset_wrong_data_marks(P0_start_point);

      -- Ensure right format for the initial jump  ...
      mark_as_a_jump_target((0, 0));
      set_NIA_to((0, 0));
      loop
         decode_the_next_order;
         mark_as_a_code_word(CIA.word_number);
         exit when INS.kind = normal_jump_order;
      end loop;

      --  ... and for subsequent words.
      for d in CIA.word_number+1 .. INS.target.word_number-1 loop
         mark_as_a_data_word(d);
      end loop;

      the_code_space_has_been_marked := True;
   end mark_all_code_blocks_and_data_blocks;

   procedure show_core_as_Usercode (first, last  : in KDF9.code_point;
                                    octal_option : in Boolean) is

      six_DUMMIES : constant KDF9.word := 8#0360741703607417#;
      saved_CIA   : constant KDF9.code_point := CIA;
      last_word   : KDF9.word := 8#0706050403020100#; -- invalid opcodes
      comparator  : KDF9.word := last_word;
      this_word   : KDF9.word;
      address     : KDF9.code_point;

      procedure show_a_block_of_orders is

         function is_a_store_order (decoded : KDF9.decoded_order)
         return Boolean is
         begin
            if decoded.kind = one_syllable_order then
               return False;
            elsif decoded.kind = two_syllable_order then
               case decoded.syndrome is
                  when TO_MkMq   | TO_MkMqQ
                     | TO_MkMqH  | TO_MkMqQH
                     | TO_MkMqN  | TO_MkMqQN
                     | TO_MkMqHN | TO_MkMqQHN =>
                     return True;
                  when others =>
                     return False;
               end case;
            elsif decoded.kind = data_access_order then
               case decoded.syndrome is
                  when TO_EaMq | TO_EaMqQ =>
                     return True;
                  when others =>
                     return False;
               end case;
            else
               return False;
            end if;
         end is_a_store_order;

         procedure set_line_at_minimum (tab : in Natural) is
         begin
            if panel_logger.column < tab then
               tab_log_to(tab);
            end if;
         end set_line_at_minimum;

         procedure set_line_at (tab : in Natural) is
         begin
            if panel_logger.column > tab then
               log_new_line;
            end if;
            if panel_logger.column < tab then
               tab_log_to(tab);
            end if;
         end set_line_at;

         procedure set_at_new_line is
         begin
            if panel_logger.column > 1 then
               log_new_line;
            end if;
         end set_at_new_line;

      begin -- show_a_block_of_orders
         this_word := fetch_word(KDF9.address(address.word_number));

         if this_word+1 < 2 or this_word = six_DUMMIES then
            -- The word is not worth logging.
            address := (0, address.word_number+1);
            return;
         end if;

         -- Log useful information about data words.
         if address.word_number/is_a_data_word then
            set_at_new_line;
         end if;
         loop
            if address.word_number/is_a_data_word then
               -- Display a line of data.
               log_new_line;
               log(oct_or_dec_of(address, octal_option) & ": ");
               set_line_at(jump_tab);
               show_in_various_formats(fetch_word(KDF9.address(address.word_number)),
                                       column => jump_tab);
               log_new_line;
               if address.word_number = last.word_number then
                  return;
               end if;
               address := (0, address.word_number+1);
            else
               log_new_line;
               exit;
            end if;
         end loop;

         loop

            this_word := fetch_word(KDF9.address(address.word_number));
            if this_word = comparator and this_word = last_word then
               -- The word is not worth logging.
               address := (0, address.word_number+1);
               return;
            end if;

            if this_word+1 < 2 or this_word = six_DUMMIES then
               comparator := this_word;
            end if;

            set_NIA_to(address);
            decode_the_next_order;
            if is_an_invalid_order(INS) then
               -- The word is not worth logging.
               address := (0, address.word_number+1);
               return;
            end if;

            if is_a_jump_target(address) then
               -- Start a code paragraph, with its address for easy reference.
               set_at_new_line;
               log(oct_or_dec_of(address, octal_option) & ": ");
               log_new_line;
            end if;


            -- Set the tab position appropriately for the order type.
            case INS.kind is
               when one_syllable_order | data_access_order =>
                  set_line_at_minimum(first_tab);
               when two_syllable_order =>
                  case INS.syndrome is
                     when JCqNZS =>
                        set_line_at(jump_tab);
                     when  CTQq_PMBQq_PMCQq_BUSYQq
                        |  PARQq
                        |  PMFQq
                        |  PIAQq_PICQq_CLOQq_TLOQq
                        |  PIBQq_PIDQq
                        |  PIEQq_PIGQq
                        |  PIFQq_PIHQq
                        |  POAQq_POCQq_POEQq_POFQq
                        |  POBQq_PODQq
                        |  POGQq_POLQq
                        |  POHQq_POKQq
                        |  PMAQq_PMKQq_INTQq
                        |  PMAQq_PMKQq_INTQq+1
                        |  PMDQq_PMEQq_PMLQq
                        |  PMDQq_PMEQq_PMLQq+1 =>
                        set_line_at(first_tab);
                     when others =>
                        if panel_logger.column < first_tab then
                           set_line_at_minimum(first_tab);
                        end if;
                  end case;
               when normal_jump_order =>
                  set_line_at(jump_tab);
            end case;

            -- Show the order in pseudo-Usercode format.
            log(the_name_of(INS, octal_option) &  "; ");

            case INS.kind is
               when one_syllable_order =>
                  increment_by_1(address);
               when two_syllable_order =>
                  increment_by_2(address);
               when normal_jump_order | data_access_order =>
                  increment_by_3(address);
            end case;

            if address.word_number = last.word_number then
               log_new_line;
               return;
            end if;

            if (address.word_number+1)/is_a_data_word or
                  address.word_number > last.word_number then
               return;
            end if;

            if is_a_store_order(INS)                   or else
                  INS.syndrome = JCqNZS                or else
                     INS.kind = normal_jump_order      or else
                        panel_logger.column > last_column then
               log_new_line;
            elsif this_word = comparator and this_word /= last_word then
               log_new_line;
               log_line("========  #"
                        & oct_of(KDF9.syllable(this_word and 255))
                        & "  ========");
               address := (0, address.word_number+1);
               if address.word_number > last.word_number or
                     address.word_number/is_a_data_word then
                  return;
               end if;
            end if;

            last_word := this_word;

         end loop;

      end show_a_block_of_orders;

   begin
      if the_code_space_has_been_marked then
         log_line("Core store interpreted as instructions.");
         address := first;
         loop
            show_a_block_of_orders;
            exit when address.word_number >= last.word_number;
         end loop;
         log_new_line;
         log_rule;
         CIA := saved_CIA;
         decode_the_next_order;
      else
         log_line(" ... Core store cannot be interpreted as instructions!");
         log_new_line;
      end if;
   end show_core_as_Usercode;

   procedure show_core_as_syllables (first, last  : KDF9.code_point) is

      address     :   KDF9.code_point;

      procedure show_a_block is

         procedure set_line_at (tab : Natural) is
         begin  -- set_line_at
            if panel_logger.column > tab then
               log_new_line;
            end if;
            if panel_logger.column < tab then
               tab_log_to(tab);
            end if;
         end set_line_at;

      begin  -- show_a_block
         loop
            if address.syllable_number = 0 then
               log_new_line;
               log(oct_of(address) & ": ");
               set_line_at(jump_tab);
            end if;
            log(oct_of(fetch_syllable(address)) &  "; ");
            increment_by_1(address);
         exit when address.word_number > last.word_number;
         end loop;
         log_new_line;
      end show_a_block;

    begin  -- show_core_as_syllables
      log_line("Core store interpreted as order syllables.");
      address := first;
      loop
         show_a_block;
         exit when address.word_number > last.word_number;
      end loop;
      log_new_line;
      log_rule;
   end show_core_as_syllables;

end state_display;
