-- Provide the comprehensive machine-state display panel KDF9 never had.
--
-- This file is part of ee9 (6.2r), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Long_Float_Text_IO;
--
with disassembly;
with exceptions;
with formatting;
with generic_sets;
with HCI;
with IOC;
with IOC.fast.DR;
with IOC.fast.FD;
with KDF9_char_sets;
with KDF9.CPU;
with KDF9.decoding;
with KDF9.PHU_store;
with KDF9.store;
with logging.file;
with settings;
with tracing;

with IOC.diagnostics;

use  Ada.Characters.Latin_1;
use  Ada.Exceptions;
use  Ada.Long_Float_Text_IO;
--
use  disassembly;
use  exceptions;
use  formatting;
use  HCI;
use  IOC;
use  IOC.fast.DR;
use  IOC.fast.FD;
use  KDF9_char_sets;
use  KDF9.CPU;
use  KDF9.decoding;
use  KDF9.PHU_store;
use  KDF9.store;
use  logging.file;
use  settings;
use  tracing;

package body state_display is

   procedure show_IM_parts (the_Q_register : in KDF9.Q_register;
                            width          : in Positive := 8) is
   begin
      log(
          "/"
        & just_right("#" & oct_of(the_Q_register.I, width-2), width)
        & "/"
        & just_right("#" & oct_of(the_Q_register.M, width-2), width)
         );
   end show_IM_parts;

   procedure show_IO_register (the_Q_register : in KDF9.Q_register;
                               width          : in Positive := 8;
                               for_DR,
                               for_FD,
                               for_FH,
                               for_seek,
                               for_OUT         : in Boolean  := False) is
   begin
      log('Q');
      if for_FD then
         log(just_right(as_FD_command(the_Q_register, for_FD and for_seek, for_FD and for_FH), width));
      elsif for_DR then
         log(just_right(as_DR_command(the_Q_register, for_OUT), width));
      else
         log(just_right("#" & oct_of(the_Q_register.C, width-2), width));
      end if;
      show_IM_parts(the_Q_register, width);
   end show_IO_register;

   procedure show_Q_register (the_Q_register : in KDF9.Q_register;
                              width          : in Positive := 8) is
   begin
      log('Q' & just_right("#" & oct_of(the_Q_register.C, width-2), width));
      show_IM_parts(the_Q_register, width);
   end show_Q_register;

   procedure show_Q_in_decimal (the_Q_register : in KDF9.Q_register;
                                width          : in Positive := 7) is
   begin
      log(
          'Q'
        & just_right(CPU.signed_Q_part'Image(resign(the_Q_register.C)), width)
        & "/"
        & just_right(CPU.signed_Q_part'Image(resign(the_Q_register.I)), width)
        & "/"
        & just_right(CPU.signed_Q_part'Image(resign(the_Q_register.M)), width)
         );
   end show_Q_in_decimal;

   procedure show_in_syllables_form (the_word : in KDF9.word) is
      word : KDF9.word := the_word;
      syllable : KDF9.syllable;
   begin
      for b in 0 .. 5 loop
         word := rotate_word_left(word, 8);
         syllable := KDF9.syllable(word and 8#377#);
         log("#" & just_right(oct_of(syllable), 3) & " ");
      end loop;
   end show_in_syllables_form;

   procedure show_as_characters (the_word : in KDF9.word) is
      word : KDF9.word := the_word;
      data : String(1 .. 8);
   begin
      for b of reverse data loop
         b := glyph_for(to_CP(KDF9_char_sets.symbol(word and 8#77#)));
         word := shift_logical(word, -6);
      end loop;
      log(data);
   end show_as_characters;

   procedure log_padded_string (text  : in String;
                                width : in Positive := 1) is
      pad_width   : constant Natural := Integer'Max (0, width - text'Length);
      padding     : constant String (1 .. pad_width) := (others => ' ');
      padded_text : constant String := padding & text;
   begin
      log(padded_text);
   end log_padded_string;

   procedure log_octal (number : in KDF9.field_of_16_bits;
                        width  : in Positive := 1) is
   begin
      log_padded_string("#" & oct_of(number), width);
   end log_octal;

   procedure log_octal (number : in KDF9.word;
                        width  : in Positive := 1) is
   begin
      log_padded_string("#" & oct_of(number), width);
   end log_octal;

  procedure show_in_various_formats (the_word : in KDF9.word;
                                     column   : in Positive := 5) is
      image : String(1 .. 21);
   begin
      log_octal(the_word);
      log(" = " & just_right(trimmed(CPU.signed'Image(resign(the_word))), 16) & " = ");
      Put(image, host_float(CPU.f48(the_word)), Aft => 12, Exp => 2);
      log(trimmed(image) & " = ");
      log(as_fraction(the_word)'Image);
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
      show_as_characters(the_word);
      log("""");
   end show_in_various_formats;

   procedure show_progress is

      function readable (t : KDF9.us)
      return String is
         t_plus_5E2 : constant KDF9.us := (t + 5E2)/ 1E3;
         t_plus_5E5 : constant KDF9.us := (t + 5E5)/ 1E6;
      begin
         return (if t < 1E3 then
                    ""
              elsif t < 1E6 then
                    " about" & t_plus_5E2'Image & " ms"
              else  " about" & t_plus_5E5'Image & " sec" );
      end readable;

      CPU : constant String := " KDF9 us  (RAN)" & readable(the_CPU_time);
      EL  : constant String := " KDF9 us  (EL) " & readable(the_clock_time);

   begin
      log_line("ORDERS:     " & just_right(ICR'Image, 10) & " executed (ICR)");
      log_line("CPU TIME:   " & just_right(the_CPU_time'Image, 10) & CPU);
      log_line("CLOCK TIME: " & just_right(KDF9.us'Image(the_clock_time), 10) & EL);
   end show_progress;

   slot_name : constant array (KDF9.context) of String(1..1)  := ("P", "Q", "R", "S");

   procedure show_Director_registers is
      interval : constant KDF9.us := the_clock_time - the_last_K4_time;
   begin
      log_line("The CPU is in " & the_CPU_state'Image);
      log_line("CONTEXT:  " & slot_name(the_context));
      log_line("PRIORITY: " & just_right(CPL'Image, 1));
      log_line("BA:       " & just_right("#" & oct_of(BA), 6));
      log_line("NOL:      " & just_right("#" & oct_of(NOL), 6));
      log("CPDAR:    ");
      for i in KDF9.buffer_number loop
         log(if the_CPDAR(i) then device_name_of(buffer(i).all) & " " else "");
      end loop;
      log_new_line;
      log_new_line;
      log_line("PHU stores:");
      for p in KDF9.priority loop
         log("PHU" & p'Image & " is ");
         if PHU(p).is_held_up then
            if PHU(p).blockage.reason = buffer_busy then
               log("waiting for " & device_name_of(IOC.device_number(PHU(p).blockage.buffer_nr)));
               log(" on buffer #" & oct_of(PHU(p).blockage.buffer_nr, 2));
               if PHU(p).blockage.by_INTQq then
                  log(", because of INTQq");
               end if;
            else
               log("locked out of group" & KDF9.store.group_address'Image(PHU(p).blockage.group_nr));
            end if;
         else
            log("idle");
         end if;
         log_new_line;
      end loop;
      log_new_line;
      log("RFIR: ");
      if the_RFIR(caused_by_PR)     then log("PR, ");    end if;
      if the_RFIR(caused_by_FLEX)   then log("FLEX, ");  end if;
      if the_RFIR(caused_by_LIV)    then log("LIV, ");   end if;
      if the_RFIR(caused_by_NOUV)   then log("NOUV, ");  end if;
      if the_RFIR(caused_by_EDT)    then log("EDT, ");   end if;
      if the_RFIR(caused_by_OUT)    then log("OUT, ");   end if;
      if the_RFIR(caused_by_LOV)    then log("LOV, ");   end if;
      if the_RFIR(caused_by_RESET)  then log("RESET, "); end if;
      if interval >= 2**20          then log("CLOCK, "); end if;
      log_line(trimmed(KDF9.us'Image(interval/32*32)) & " KDF9 us since last CLOCK");
      log_new_line;
   end show_Director_registers;

   procedure show_V_and_T is
   begin
      if the_V_bit_is_set or the_T_bit_is_set then
         log_new_line;
         if the_V_bit_is_set then
            log("V is set. ");
         else
            log("V is clear. ");
         end if;
         if the_T_bit_is_set then
            log("T is set. ");
         else
            log("T is clear. ");
         end if;
         log_new_line;
      end if;
   end show_V_and_T;

   procedure show_NEST is
   begin
      if the_NEST_depth = 0 then
         log_line("The NEST is empty.");
         return;
      else
         log_line("NEST:");
         for i in reverse KDF9.NEST_depth loop
            if i < the_NEST_depth then
               log(just_right("N" & trimmed(KDF9.NEST_depth'Image(the_NEST_depth-i)), 3) & ": ");
               log_new_line;
               show_in_various_formats(the_NEST(i));
               log_new_line;
            end if;
         end loop;
      end if;
   end show_NEST;

   procedure show_SJNS is
   begin
      if the_SJNS_depth = 0 then
         log_line("The SJNS is empty.");
         return;
      else
         log_line("SJNS:");
      end if;
      for i in reverse KDF9.SJNS_depth loop
         if i < the_SJNS_depth then
            log(just_right("S" & trimmed(KDF9.SJNS_depth'Image(the_SJNS_depth-i)), 3) & ": ");
            log_line(oct_of(the_SJNS(i)) & " (" & dec_of(KDF9.syllable_address(the_SJNS(i))) & ")");
         end if;
      end loop;
   end show_SJNS;

   procedure show_Q_store is
      Q_bits  : KDF9.word := 0;
   begin
      for Qq of the_Q_store loop
         Q_bits := Q_bits or as_word(Qq);
      end loop;
      if Q_bits = 0 then
         log_line("Q store: all zero.");
         return;
      else
         log_line("Q store:");
      end if;
      for q in KDF9.Q_store'Range loop
         if as_word(the_Q_store(q)) /= KDF9.word'(0) then
            log(just_right("Q" & trimmed(q'Image), 3) & ": ");
            show_Q_register(the_Q_store(q));
            log("  = ");
            show_Q_in_decimal(the_Q_store(q));
            log_new_line;
         end if;
      end loop;
   end show_Q_store;

   procedure show_registers is
   begin
      show_progress;
      log_new_line;
      if the_execution_mode = boot_mode then
         show_Director_registers;
      end if;
      show_SJNS;
      log_new_line;
      show_Q_store;
      show_V_and_T;
      log_new_line;
      show_NEST;
   end show_registers;

   procedure show_order is
   begin
      log(the_code_and_name_of_INS);
   end show_order;

   procedure show_execution_context is
   begin
      log("At "
        & oct_of(CIA)
        & " ("
        & dec_of(CIA)
        & ")"
        & "; ICR ="
        & ICR'Image
        & "; EL ="
        & the_clock_time'Image
        & "; the instruction was ");
      show_order;
      log_new_line;
   end show_execution_context;

   procedure log_to_external_trace is
   begin
      log(the_external_trace_file, oct_of(CIA));
      tab_log_to(the_external_trace_file, 10);
      log(the_external_trace_file, ICR'Image);
      tab_log_to(the_external_trace_file, 20);
      if only_signature_tracing then
         log(
             the_external_trace_file,
             "#"
           & oct_of(the_digital_signature)
           & (if the_V_bit_is_set then "V" else " ")
           & (if the_T_bit_is_set then "T" else " ")
            );
         tab_log_to(the_external_trace_file, 40);
         if the_NEST_depth > 0 then
            log(the_external_trace_file, "#" & oct_of(read_top));
         end if;
         tab_log_to(the_external_trace_file, 58);
      else
         log(the_external_trace_file, the_CPU_time'Image);
         tab_log_to(the_external_trace_file, 40);
         log(the_external_trace_file, the_NEST_depth'Image);
         tab_log_to(the_external_trace_file, 43);
         log(the_external_trace_file, the_SJNS_depth'Image);
         tab_log_to(the_external_trace_file, 46);
         log(the_external_trace_file, (if the_V_bit_is_set then "V" else " "));
         log(the_external_trace_file, (if the_T_bit_is_set then "T" else " "));
         tab_log_to(the_external_trace_file, 50);
         if the_NEST_depth > 0 then
            log(the_external_trace_file, "#" & oct_of(read_top));
         end if;
         tab_log_to(the_external_trace_file, 68);
      end if;
      log(the_external_trace_file, " |" & the_full_name_of(INS));
      tab_log_to(the_external_trace_file, 90);
      log(the_external_trace_file, KDF9.us'Image(the_clock_time));
      log_new_line(the_external_trace_file);
   end log_to_external_trace;

   procedure log_an_external_trace_header (caption : in String := "") is
   begin
      if caption /= "" then
         log_new_line(the_external_trace_file);
         log(the_external_trace_file, caption);
         log_new_line(the_external_trace_file);
      end if;
      log(the_external_trace_file, "LOCATION");
      tab_log_to(the_external_trace_file, 11);
      log(the_external_trace_file, "ICR");
      tab_log_to(the_external_trace_file, 20);
      if only_signature_tracing then
         log(the_external_trace_file, "DIGITAL SIGNATURE");
         tab_log_to(the_external_trace_file, 40);
         log(the_external_trace_file, "[N1]");
         tab_log_to(the_external_trace_file, 58);
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
      log_line("CIA:        " & just_right(oct_of(CIA), 10) & " (" & just_right(dec_of(CIA) & ")"));
      log_line("NIA:        " & just_right(oct_of(NIA), 10) & " (" & just_right(dec_of(NIA) & ")"));
   end show_CIA_and_NIA;

   procedure long_witness is
   begin
      log_new_line;
      log("At " & oct_of(CIA) & " (" & dec_of(CIA) & ") the instruction was ");
      show_order;
      log_new_line;
      show_registers;
   end long_witness;

   procedure short_witness is

      type register_usage is array (KDF9.compressed_opcode) of Boolean
         with Size => 64, Component_Size => 1;

      it_uses_JB : constant register_usage
                 := (
                      LINK
                    | TO_LINK
                    | OS_OUT
                    | JrNEJ
                    | JSr
                    | EXIT_n
                    | JrEJ
                    | EXITD     => True,
                      others    => False
                    );

      it_uses_Qq : constant register_usage
                 := (
                      MkMq
                    | MkMqQ
                    | MkMqH
                    | MkMqQH
                    | MkMqN
                    | MkMqQN
                    | MkMqHN
                    | MkMqQHN
                    | TO_MkMq
                    | TO_MkMqQ
                    | TO_MkMqH
                    | TO_MkMqQH
                    | TO_MkMqN
                    | TO_MkMqQN
                    | TO_MkMqHN
                    | TO_MkMqQHN
                    | MqTOQk
                    | IqTOQk
                    | IMqTOQk
                    | CqTOQk
                    | CMqTOQk
                    | CIqTOQk
                    | QqTOQk
                    | M_PLUS_Iq
                    | M_MINUS_Iq
                    | NCq
                    | DCq
                    | POS1_TO_Iq
                    | NEG1_TO_Iq
                    | POS2_TO_Iq
                    | NEG2_TO_Iq
                    | SHA
                    | SHAD
                    | MACC
                    | SHL
                    | SHLD
                    | SHC
                    | TO_RCIMq
                    | QCIMq
                    | ADD_TO_QCIMq
                    | JCqNZS
                    | PAR_Qq
                    | PIA_PIC_CLO_TLO_Qq
                    | PIB_PID_Qq
                    | PIE_PIG_Qq
                    | PIF_PIH_Qq
                    | PMA_PMK_INT_Qq
                    | CT_PMB_PMC_BUSY_Qq
                    | PMD_PME_PML_Qq
                    | PMF_PMG_Qq
                    | POA_POC_POE_POF_PMH_Qq
                    | POB_POD_Qq
                    | POG_POL_Qq
                    | POH_POK_Qq
                    | JrCqNZ    => True,
                      others    => False
                    );

      is_modified : constant register_usage
                  := (
                       EaMq
                     | TO_EaMq
                     | EaMqQ
                     | TO_EaMqQ  => True,
                       others    => False
                     );

      it_uses_Qk : constant register_usage
                 := (
                      MkMq
                    | MkMqQ
                    | MkMqH
                    | MkMqQH
                    | MkMqN
                    | MkMqQN
                    | MkMqHN
                    | MkMqQHN
                    | TO_MkMq
                    | TO_MkMqQ
                    | TO_MkMqH
                    | TO_MkMqQH
                    | TO_MkMqN
                    | TO_MkMqQN
                    | TO_MkMqHN
                    | TO_MkMqQHN
                    | MqTOQk
                    | IqTOQk
                    | IMqTOQk
                    | CqTOQk
                    | CMqTOQk
                    | CIqTOQk
                    | QqTOQk    => True,
                      others    => False
                    );

      function INS_uses_Qq
      return Boolean is
         (
          -- A compressed_opcode may be ambiguous: to know which opcode it represents,
          --   further attributes of the order may need to be considered.
          case INS.kind is
             when two_syllable_order =>
                it_uses_Qq(INS.compressed_opcode)
                  and
                -- If a shift, exclude fixed-amount shifts.
                ((INS.order.syllable_1 and 1) = 0 or else INS.compressed_opcode not in SHA..SHC),
             when normal_jump_order =>
                INS.compressed_opcode in JrCqZ | JrCqNZ,
             when data_access_order =>
                is_modified(INS.compressed_opcode),
             when others =>
                False
         );

      need_new_line : Boolean := False;

   begin  -- short_witness
      log_new_line;
      show_execution_context;
      if the_CPU_state = Director_state then
         show_Director_registers;
      end if;

      if it_uses_JB(INS.compressed_opcode)                     and then
            INS.kind in two_syllable_order | normal_jump_order and then
               the_SJNS_depth > 0                                  then
         log_line(
                  " JB: "
                & oct_of(the_SJNS(the_SJNS_depth-1))
                & "; SJNS depth: " & just_right(the_SJNS_depth'Image, 3)
                 );
      end if;

      if INS.Qq /= 0 and then
            INS_uses_Qq  then
         log(just_right("Q" & trimmed(INS.Qq'Image), 3) & ": ");
         show_Q_register(the_Q_store(INS.Qq));
         log("  = ");
         show_Q_in_decimal(the_Q_store(INS.Qq));
         log_new_line;
         need_new_line := True;
      end if;
      if INS.Qk /= 0                       and then
            INS.kind in two_syllable_order and then
               it_uses_Qk(INS.compressed_opcode)    and then
                  INS.Qq /= INS.Qk             then
         log(just_right("Q" & trimmed(INS.Qk'Image), 3) & ": ");
         show_Q_register(the_Q_store(INS.Qk));
         log("  = ");
         show_Q_in_decimal(the_Q_store(INS.Qk));
         log_new_line;
         need_new_line := True;
      end if;
      if need_new_line then
         log_new_line;
      end if;

      show_V_and_T;
      show_NEST;
      log_rule;
   end short_witness;

   procedure show_frequency_plots is

      function summed_counts (from, to : KDF9.syllable)
      return KDF9.order_counter is
         sum : KDF9.order_counter := 0;
      begin
         for i in from .. to loop
            sum := sum + the_histogram(i);
         end loop;
         return sum;
      end summed_counts;

      procedure log_opcode_bin (bin    : in KDF9.syllable;
                                sum    : in KDF9.order_counter;
                                bound  : in Long_Float) is
         percent : Long_Float;
         image   : String(1 .. 6);
      begin
         if sum /= 0 then
            percent := Long_Float(sum)/Long_Float(ICR)*100.0;
            if percent < bound then
               return;
            end if;
            log(oct_of(bin) & ": " & the_short_name_of(bin));
            tab_log_to(32);
            log(sum'Image);
            tab_log_to(42);
            Put(image, percent, Aft => 2, Exp => 0);
            log(image & "% :");
            for i in 1 .. Integer(percent) loop
               log("|");
            end loop;
            log_new_line;
         end if;
      end log_opcode_bin;

      procedure log_opcode_usage (bound : in Long_Float) is
      begin
         for i in KDF9.syllable'(0) .. 8#167# loop
            log_opcode_bin(i, the_histogram(i), bound);
         end loop;
         for i in KDF9.syllable'(8#170#) .. 8#237# loop
            log_opcode_bin(i, the_histogram(i), bound);
         end loop;
         log_opcode_bin(8#240#, summed_counts(from => 8#240#, to => 8#257#), bound);
         log_opcode_bin(8#260#, summed_counts(from => 8#240#, to => 8#277#), bound);
         for i in KDF9.syllable'(8#300#) .. 8#377# loop
            log_opcode_bin(i, the_histogram(i), bound);
         end loop;
      end log_opcode_usage;

      accounted_for : Long_Float;
      cutoff_image  : String(1 .. 7) := "      %";
      percent_image : String(1 .. 7) := "      %";

      procedure log_order_word_bin (bin    : in KDF9.order_word_number;
                                    sum    : in KDF9.order_counter;
                                    bound  : in Long_Float) is
         percent : Long_Float;
      begin
         if sum /= 0 then
            percent := Long_Float(sum)/Long_Float(ICR)*100.0;
            if percent < bound then
               return;
            end if;
            accounted_for := accounted_for + percent;
            log("#" & oct_of(bin) & ": ");
            tab_log_to(32);
            log(sum'Image);
            tab_log_to(42);
            Put(percent_image, percent, Aft => 2, Exp => 0);
            percent_image(7) := '%';
            log(percent_image);
            log(" :");
            for i in 1 .. Integer(percent) loop
               log("|");
            end loop;
            log_new_line;
         end if;
      end log_order_word_bin;

      procedure log_profile (bound : in Long_Float) is
      begin
         accounted_for := 0.0;
         for w in KDF9.order_word_number loop
            if the_profile(w) /= 0 then
               log_order_word_bin(w, the_profile(w), bound);
            end if;
         end loop;
      end log_profile;

      procedure sum_logged_frequencies (bound  : in Long_Float) is
         percent : Long_Float;
      begin
         accounted_for := 0.0;
         for w in KDF9.order_word_number loop
            percent := Long_Float(the_profile(w))/Long_Float(ICR)*100.0;
            if percent >= bound then
               accounted_for := accounted_for + percent;
            end if;
         end loop;
      end sum_logged_frequencies;

   begin -- show_frequency_plots
      Put(cutoff_image(1..6), histogram_cutoff, Aft => 2, Exp => 0);
      cutoff_image(7) := '%';
      if the_INS_plot_is_wanted and ICR /= 0 and the_diagnostic_mode /= fast_mode then
         -- Print the instruction execution-frequency histogram.
         log_title(
                   "Histogram of the opcodes of"
                 & ICR'Image
                 & " executed instructions with frequency >="
                 & cutoff_image
                  );
         log_opcode_usage(bound => histogram_cutoff);
         log_new_line;
      end if;
      if the_profile_is_wanted and ICR /= 0 and the_diagnostic_mode /= fast_mode then
         log_title(
                   "Histogram of the loci of"
                 & ICR'Image
                 & " executed instructions with frequency >="
                 & cutoff_image
                  );
         log_profile(bound => histogram_cutoff);
         log_new_line;
      end if;
      sum_logged_frequencies(bound => histogram_cutoff);
      Put(percent_image(1..6), accounted_for, Aft =>1, Exp => 0);
      log_line("Executions accounted for in the profile:" & percent_image);
      log_rule;
   end show_frequency_plots;

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

   function for_FH_disc (compressed_opcode : KDF9.compressed_opcode; Pxy_bits : KDF9.Q_number)
   return Boolean
   is (case compressed_opcode is
          when PIA_PIC_CLO_TLO_Qq     => Pxy_bits = PIC_bits,
          when PIB_PID_Qq             => Pxy_bits = PID_bits,
          when PIE_PIG_Qq             => Pxy_bits = PIG_bits,
          when PIF_PIH_Qq             => Pxy_bits = PIH_bits,
          when POA_POC_POE_POF_PMH_Qq => Pxy_bits = POC_bits,
          when POB_POD_Qq             => Pxy_bits = POD_bits,
          when POG_POL_Qq             => Pxy_bits = POL_bits,
          when POH_POK_Qq             => Pxy_bits = POK_bits,
          when others                 => False
      );


   first_col   : constant := 17;
   device_col  : constant := first_col + 20;
   operand_col : constant := device_col;
   event_col   : constant := operand_col + 4;
   is_D_col    : constant := event_col + 29;
   depth_col   : constant := operand_col + 29;
   time_col    : constant := depth_col + 11;
   ICR_col     : constant := time_col + 13;

   procedure show_retro_FIFO is

      RFIR_id : constant array (KDF9.interrupt_number) of Character
              := ('P', 'F', 'I', 'N', 'E', 'S', 'O', 'R', 'Y', 'Z');
      image   : String(1 .. 21);
      RFIR    : KDF9.RFIR;
   begin
      if retro_FIFO_count = 0 then
         return;
      end if;
      log_title("Retrospective trace of all instructions.");
      tab_log_to(depth_col);
      log_line("ND SD VTD   CPU TIME     ICR");
      for i in 1 .. retro_FIFO_count loop
         if i = 1 then
            log("Ended ");
         else
            log("After ");
         end if;
         declare
            this      : tracing.retro_FIFO_entry renames retro_FIFO(retro_FIFO_index);
            Q         : constant KDF9.Q_register := as_Q(this.parameter);
            decoded   : KDF9.decoded_order;
         begin
            log(oct_of(this.location) & ":");
            tab_log_to(first_col);
            decoded.order := this.order;
            decode(decoded);
            log(the_full_name_of(decoded,
                                 octal_option => decoded.kind = normal_jump_order,
                                 both_bases   => False));
            tab_log_to(operand_col);
            case decoded.kind is
               when one_syllable_order =>
                  if this.nested > 0 then
                     case decoded.compressed_opcode is
                        when DIV
                           | DIVD
                           | X_frac =>
                           log(CPU.fraction'Image(as_fraction(this.parameter)));
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
                           Put(image, host_float(CPU.f48(this.parameter)), Aft => 12, Exp => 2);
                           log(trimmed(image));
                        when others =>
                           if this.nested > 0 then
                              log_octal(this.parameter);
                           end if;
                     end case;
                  end if;
               when two_syllable_order =>
                  case decoded.compressed_opcode is
                     when PAR_Qq =>
                        show_IO_register(Q, for_DR => False, for_FD => False);
                     when CT_PMB_PMC_BUSY_Qq
                        | PMA_PMK_INT_Qq
                        | PMD_PME_PML_Qq
                        | PMF_PMG_Qq =>
                        show_IO_register(
                                         Q,
                                         for_DR   => device_kind_of(Q.C mod 16) = DR_kind,
                                         for_FD   => device_kind_of(Q.C mod 16) = FD_kind,
                                         for_seek => decoded.Qk = PMA_bits
                                        );
                     when PIA_PIC_CLO_TLO_Qq
                        | PIB_PID_Qq
                        | PIE_PIG_Qq
                        | PIF_PIH_Qq
                        | POA_POC_POE_POF_PMH_Qq
                        | POB_POD_Qq
                        | POG_POL_Qq
                        | POH_POK_Qq =>
                        show_IO_register(
                                         Q,
                                         for_DR   => device_kind_of(Q.C mod 16) = DR_kind,
                                         for_FD   => device_kind_of(Q.C mod 16) = FD_kind,
                                         for_FH   => for_FH_disc(decoded.compressed_opcode, decoded.Qk)
                                        );
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
                        show_Q_register(Q);
                     when Kk =>
                        case decoded.Qk is
                           when K4 =>
                              log(KDF9.word'Image(32*KDF9.word(Q.C)));
                              log("us");
                              if Q.I /= 0 then
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
                              log("invalid K order: #" & oct_of(decoded.compressed_opcode));
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
                  case decoded.compressed_opcode is
                     when Jr
                        | JSr =>
                        log(oct_of(as_link(this.parameter)));
                     when EXIT_n =>
                        if this.parameter < 8 then
                           log(this.parameter'Image);
                        else
                           log(oct_of(as_link(this.parameter)));
                        end if;
                     when EXITD =>
                        log(oct_of(as_link(this.parameter)));
                     when JrCqZ
                        | JrCqNZ =>
                        show_Q_register(Q);
                     when OS_OUT =>
                        if this.parameter < 16 then
                           log_octal(this.parameter);
                        elsif this.parameter < 64 then
                           log(this.parameter'Image);
                        elsif this.parameter > 2**47 then
                           log_octal(this.parameter);
                        else
                           show_Q_register(Q);
                        end if;
                     when JrEJ
                        | JrNEJ
                        | JrEN
                        | JrNEN =>
                           log(this.parameter'Image);
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
            tab_log_to(depth_col);
            log(just_right(this.nested'Image,2));
            log(" ");
            log(just_right(this.called'Image,2));
            log(" ");
            log(if this.V then "V" else " ");
            log(if this.T then "T" else " ");
            log(if this.D then "D" else " ");
            tab_log_to(time_col);
            log(this.CPU_time'Image);
            tab_log_to(ICR_col);
            log(this.ICR_value'Image);
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

   the_final_ICR : KDF9.order_counter := 0;

   procedure notify_state_display_of_final_ICR is
   begin
      the_final_ICR := ICR;
   end notify_state_display_of_final_ICR;

   procedure show_IOC_FIFO is
   begin
      if IOC_FIFO_count = 0 then return; end if;
      log_title("Retrospective trace of peripheral I/O events.");
      tab_log_to(is_D_col);
      log_line("CPL T   EL. TIME     ICR");
      for i in 1 .. IOC_FIFO_count loop
         if i = 1 then
            log("Ended ");
         else
            log("After ");
         end if;

         declare
            this    : tracing.IOC_FIFO_entry renames IOC_FIFO(IOC_FIFO_index);
            decoded : constant KDF9.decoded_order := this.decoded_order;

            procedure show_transfer (
                                     Q                 : in KDF9.Q_register;
                                     for_OUT, for_seek : in Boolean := False
                                    ) is
            begin
               case decoded.compressed_opcode is
                  when PAR_Qq =>
                     show_IO_register(Q, for_DR => False, for_FD => False);
                  when CT_PMB_PMC_BUSY_Qq
                     | PMA_PMK_INT_Qq
                     | PMD_PME_PML_Qq
                     | PMF_PMG_Qq =>
                     show_IO_register(
                                      Q,
                                      for_DR   => device_kind_of(Q.C mod 16) = DR_kind,
                                      for_FD   => device_kind_of(Q.C mod 16) = FD_kind,
                                      for_seek => (decoded.Qk in PMA_bits) or show_transfer.for_seek,
                                      for_OUT  => show_transfer.for_OUT
                                     );
                  when PIA_PIC_CLO_TLO_Qq
                     | PIB_PID_Qq
                     | PIE_PIG_Qq
                     | PIF_PIH_Qq
                     | POA_POC_POE_POF_PMH_Qq
                     | POB_POD_Qq
                     | POG_POL_Qq
                     | POH_POK_Qq
                     | OS_OUT =>
                     show_IO_register(
                                      Q,
                                      for_DR   => device_kind_of(Q.C mod 16) = DR_kind,
                                      for_FD   => device_kind_of(Q.C mod 16) = FD_kind,
                                      for_FH   => for_FH_disc(decoded.compressed_opcode, decoded.Qk),
                                      for_OUT  => show_transfer.for_OUT
                                     );
                  when others =>
                     log("invalid IO order: #" & oct_of(decoded.compressed_opcode));
               end case;
            end show_transfer;

            shown_ICR : KDF9.order_counter := this.ICR_value;
            FD_seek   : Boolean := False;
            FD_xfer   : Boolean := False;

         begin -- show_IOC_FIFO
            log(oct_of(this.order_address) & ":");
            tab_log_to(first_col);
            if the_full_name_of(this.decoded_order) = "OUT"  then
                if this.device_name(1..2) in "MT" | "ST" and then
                      this.ICR_value >= the_final_ICR        then
                  log("OUT 0/2 rewind");
                  shown_ICR := the_final_ICR + 1;
               elsif this.device_name(1..2) in "MT" | "ST" then
                  log("OUT 6/7 rewind");
               elsif this.device_name(1..2) in "LP" | "TP" then
                  log("OUT 8");
               elsif this.device_name(1..2) = "DR" then
                  if this.kind in start_transfer | finis_transfer | buffer_lockout | store_lockout then
                     log(if this.operation = output_operation then "OUT 11" else "OUT 12");
                  else
                     log("OUT 11/12");
                  end if;
               elsif this.device_name(1..2) = "FD" then
                  if this.kind in start_transfer | finis_transfer | buffer_lockout | store_lockout then
                     log(if this.operation = output_operation then "OUT 41" else "OUT 42");
                  else
                     log("OUT 41/42 seek"); FD_seek := True;
                  end if;
               elsif this.device_name(1..2) = "FW" then
                  log("OUT 8/16");
               else
                  log("OUT ?");
               end if;
            else
               log(mnemonic(the_full_name_of(this.decoded_order), this.device_name));
            end if;
            tab_log_to(device_col);
            log(this.device_name);
            case this.kind is
               when store_lockout =>
                  tab_log_to(event_col);
                  log("locks out #");
                  log(oct_of(this.data_address));
                  log(" = E");
                  log(dec_of(this.data_address));
                  tab_log_to(is_D_col);
                  log(if this.is_for_Director then "D" else slot_name(this.context));
                  log(this.priority_level'Image);
                  tab_log_to(time_col);
                  log(this.initiation_time'Image);
                  tab_log_to(ICR_col);
                  log(shown_ICR'Image);
                when buffer_lockout =>
                  tab_log_to(event_col);
                  log("buffer lockout");
                  tab_log_to(is_D_col);
                  log(if this.is_for_Director then "D" else slot_name(this.context));
                  log(this.priority_level'Image);
                  tab_log_to(time_col);
                  log(this.initiation_time'Image);
                  tab_log_to(ICR_col);
                  log(shown_ICR'Image);
               when start_transfer =>
                  tab_log_to(event_col);
                  show_transfer(this.control_word);
                  tab_log_to(is_D_col);
                  log(if this.is_for_Director then "D" else slot_name(this.context));
                  log(this.priority_level'Image);
                  tab_log_to(time_col-2);
                  log(" S" & this.initiation_time'Image);
                  tab_log_to(ICR_col);
                  log(shown_ICR'Image);
               when finis_transfer =>
                  tab_log_to(event_col);
                  show_transfer(this.control_word);
                  tab_log_to(is_D_col);
                  log(if this.is_for_Director then "D" else slot_name(this.context));
                  log(this.priority_level'Image);
                  tab_log_to(time_col-2);
                  log(" E" & this.completion_time'Image);
                  tab_log_to(ICR_col);
                  log(shown_ICR'Image);
               when buffer_status =>
                  tab_log_to(event_col);
                  FD_xfer := this.device_name(1..2) = "FD";
                  -- PMFQq entails no data transfer or seek, but has a sector address parameter.
                  FD_seek := (FD_seek or (decoded.Qk in PMA_bits)) and FD_xfer;
                  FD_seek := FD_seek and decoded.compressed_opcode /= PMF_PMG_Qq;
                  show_IO_register(this.Q_register, for_FD => FD_xfer, for_seek => FD_seek);
                  tab_log_to(is_D_col);
                  log(if this.is_for_Director then "D" else slot_name(this.context));
                  log(this.priority_level'Image);
                  log(if this.status then " Y" else " N");
                  tab_log_to(time_col);
                  log(this.initiation_time'Image);
                  tab_log_to(ICR_col);
                  log(shown_ICR'Image);
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
             & KDF9.us'Image((the_clock_time-the_CPU_time+500) / 1000)
             & " ms.");
      log_rule;
   end show_IOC_FIFO;

   procedure show_interrupt_FIFO is
   begin
      if interrupt_FIFO_count = 0 then return; end if;
      log_title("Retrospective trace of interrupt requests.");
      tab_log_to(is_D_col);
      log_line("CPL     EL. TIME     ICR");
      for i in 1 .. interrupt_FIFO_count loop
         log(if i = 1 then "Ended " else "After ");
         declare
            this : tracing.interrupt_FIFO_entry renames interrupt_FIFO(interrupt_FIFO_index);
         begin
            log(oct_of(this.order_address) & ": ");
            tab_log_to(first_col);
            log(case this.interrupt_code is
                   when caused_by_PR     => "PR   ",
                   when caused_by_FLEX   => "FLEX ",
                   when caused_by_LIV    => "LIV  ",
                   when caused_by_NOUV   => "NOUV ",
                   when caused_by_EDT    => "EDT  ",
                   when caused_by_OUT    => "OUT  ",
                   when caused_by_LOV     => "LOV  ",
                   when caused_by_RESET   => "RESET",
                   when caused_by_CLOCK   => "CLOCK",
                   when EXITD_flag       => "EXITD"
               );
            tab_log_to(event_col-4);
            log(trimmed(this.message));
            tab_log_to(is_D_col);
            log(slot_name(this.context));
            log(this.priority_level'Image);
            tab_log_to(time_col);
            log(this.busy_time'Image);
            tab_log_to(ICR_col);
            log(this.ICR_value'Image);
            log_new_line;
         end;
         interrupt_FIFO_index := interrupt_FIFO_index - 1;
      end loop;
      log(
          if interrupt_FIFO_count = FIFO_size then
             "After earlier interrupts, whose tracing is now lost."
          else
            "After the start of traced execution."
         );
      log_new_line;
      log_new_line;
   end show_interrupt_FIFO;

   procedure show_retrospective_traces is
   begin
      if peripheral_tracing_is_enabled then
         pragma Debug(IOC.diagnostics);
      end if;
      if interrupt_tracing_is_enabled then
         show_interrupt_FIFO;
      end if;
      if peripheral_tracing_is_enabled then
         show_IOC_FIFO;
      end if;
      if retrospective_tracing_is_enabled then
         show_retro_FIFO;
      end if;
   end show_retrospective_traces;

   procedure show_current_state is
   begin
      show_execution_context;
      log_rule;
      show_registers;
      log_rule;
   end show_current_state;

   procedure show_final_state (because : in String) is
   begin
      if the_final_state_is_wanted then
         if loading_was_successful then
            -- make sure there is at least one NL after any FW output.
            if the_log_is_wanted then
               log_new_line;
               log_rule;
            else
               log_new_line;
            end if;
            log_line("Final State: " & because & ".");
            if not the_log_is_wanted then return; end if;
            long_witness;
            log_rule;

            if nr_of_post_dumping_areas /= 0 then
               log_title("Post-run Dump:");
               print_postrun_dump_areas;
            end if;

            if the_INS_plot_is_wanted or the_profile_is_wanted then
               if histogramming_is_enabled then
                  show_frequency_plots;
               end if;
            end if;

            show_retrospective_traces;

            if the_signature_is_enabled then
               log_title("Digital signature of traced orders = #"
                       & oct_of(the_digital_signature)
                       & ".");
            end if;
         else
            log_line("ee9: " & because & ".");
            show_all_prerun_dump_areas;
            return;
         end if;
      end if;
   end show_final_state;

   procedure show_all_prerun_dump_areas is
   begin
      if the_log_is_wanted and nr_of_pre_dumping_areas /= 0 then
         log_title("Pre-run Dump:");
         print_prerun_dump_areas;
         remove_prerun_dump_areas;
      end if;
   end show_all_prerun_dump_areas;

   increment   : constant := 8;
   jump_tab    : constant := 12;
   first_tab   : constant := 16;
   last_column : constant := 96;

   function is_non_blank (first : in KDF9.address)
   return Boolean is
      result : Boolean := False;
   begin
      for address in first .. first+increment-1 loop
         result := result or (fetch_word(address) /= 0);
      end loop;
      return result;
   end is_non_blank;

   subtype converted_word is String(1..8);

   type convertor is
      not null access function (address : KDF9.address) return converted_word;

   procedure show_core (first, last : in KDF9.address;
                        head, side  : in String;
                        converted   : in convertor) is

      procedure show_group (first : in KDF9.address) is
         address : KDF9.address := first;
      begin
         while address <= first+increment-1 loop
            log(converted(address));
            address := address + 1;
            exit when address < first;
         end loop;
      end show_group;

      address : KDF9.address := first;

   begin
      if (last-first+1) < 1 then
         return;
      end if;
      BA := 0; -- Ensure that physical store is examined when running in boot mode.
      log_title("Core store [#" & oct_of(first) & " .. #" & oct_of(last) & "] interpreted as " & head & ":");
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
         else
            log_line("========  blank  ========");
         end if;
      exit when address >= KDF9.address'Last - increment;
         address := address + increment;
      end loop;
      log_new_line;
   end show_core;

   function encoding_of (address : KDF9.address; code_table : output_code_table)
   return converted_word is
      result : converted_word;
   begin
      for b in KDF9_char_sets.symbol_index loop
         result(Natural(b)+1) := glyph_for(code_table(fetch_symbol(address, b)));
      end loop;
      return result;
   end encoding_of;

   current_case : KDF9_char_sets.symbol := KDF9_char_sets.Case_Normal;

   function interpretation_of (address : KDF9.address)
   return converted_word is
      result : converted_word;
      symbol : KDF9_char_sets.symbol;
      char   : Character;
   begin
      for b in KDF9_char_sets.symbol_index loop
         symbol := fetch_symbol(address, b);
         if current_case = KDF9_char_sets.Case_Normal then
            char := TP_CN(symbol);
         else
            char := TP_CS(symbol);
         end if;
         if symbol = KDF9_char_sets.Case_Normal then
            current_case := KDF9_char_sets.Case_Normal;
         elsif symbol = KDF9_char_sets.Case_Shift then
            current_case := KDF9_char_sets.Case_Shift;
         end if;
         result(Natural(b)+1) := glyph_for(char);
      end loop;
      return result;
   end interpretation_of;

   function case_visible (address : KDF9.address)
   return converted_word
   is (interpretation_of(address));

   function case_normal (address : KDF9.address)
   return converted_word
   is (encoding_of(address, code_table => TP_CN));

   function case_shift (address : KDF9.address)
   return converted_word
   is (encoding_of(address, code_table => TP_CS));

   function printer_code (address : KDF9.address)
   return converted_word
   is (encoding_of(address, code_table => to_LP));

   function card_code (address : KDF9.address)
   return converted_word
   is (encoding_of(address, code_table => to_CP));

   function Latin_1_code (address : KDF9.address)
   return converted_word
   is (converted_word'(1..7 => Space,
                       8    => glyph_for(Character'Val(fetch_word(address) and 8#377#))));

   procedure show_core_in_case_visible (first, last : in KDF9.address) is
   begin
      show_core(first, last,
                head => "characters in TR/TP code with case shifting",
                side => "  ",
                converted => case_visible'Access);
   end show_core_in_case_visible;

   procedure show_core_in_case_normal (first, last : in KDF9.address) is
   begin
      show_core(first, last,
                head => "characters in TR/TP Normal Case code",
                side => "NC",
                converted => case_normal'Access);
   end show_core_in_case_normal;

   procedure show_core_in_case_shift (first, last : in KDF9.address) is
   begin
      show_core(first, last,
                head => "characters in TR/TP Shift Case code",
                side => "SC",
                converted => case_shift'Access);
   end show_core_in_case_shift;

   procedure show_core_in_print_code (first, last : in KDF9.address) is
   begin
      show_core(first, last,
                head => "characters in LP code",
                side => "LP",
                converted => printer_code'Access);
   end show_core_in_print_code;

   procedure show_core_in_card_code (first, last : in KDF9.address) is
   begin
      show_core(first, last,head => "characters in CR/CP code",
                side => "CP",
                converted => card_code'Access);
   end show_core_in_card_code;

   procedure show_core_in_Latin_1 (first, last : in KDF9.address) is
   begin
      show_core(first, last,
                head => "words with bits 40-47 of each in Latin-1 code",
                side => "L1",
                converted => Latin_1_code'Access);
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
      end show_word;

      procedure show_word_group (first, last  : KDF9.address) is
         last_address : KDF9.address := first;
         this_word, last_word : KDF9.word;
      begin
         if last in first | 0 then
            show_word(last);
            return;
         end if;
         this_word := fetch_word(first);
         last_word := this_word;
         show_word(first);
         for address in first+1 .. last-1 loop
            this_word := fetch_word(address);
            if this_word = last_word and address = last_address+1 then
               log_line("==========  ditto  ========");
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
      BA := 0; -- Ensure that physical store is examined when running in boot mode.
      log_title("Core store interpreted as 48-bit words:");
      show_word_group(first, last);
      log_new_line;
   end show_core_as_word_forms;

   -- Each word of code space is described by a set of flags.
   -- Flags 0 .. 5 are set iff a jump order has that syllable as target.
   -- Flag 6 is set if the word is thought to be code, but not a target.
   -- Flag 7 is set if the word is thought to be addressed as data.

   package word_flags is new generic_sets(member => KDF9.syllable_index);
   use word_flags;

   is_a_code_word   : constant KDF9.syllable_index := 6;
   is_a_data_word   : constant KDF9.syllable_index := 7;
   all_jump_targets : constant word_flags.set := (6|7 => False, others => True);

   analysis_flags   : array (KDF9.order_word_number) of word_flags.set;

   function "/" (word : KDF9.order_word_number; flag : KDF9.syllable_index)
   return Boolean
   is (analysis_flags(word)/flag);

   function is_a_jump_target (the_point : in KDF9.syllable_address)
   return Boolean
   is (analysis_flags(the_point.order_word_number)/the_point.syllable_index);

   function is_a_jump_target (the_operand : in KDF9.order_word_number)
   return Boolean
   is ((analysis_flags(the_operand) and all_jump_targets) /= empty_set);

   procedure clear_all_analysis_flags is
   begin
      analysis_flags := (others => empty_set);
   end clear_all_analysis_flags;

   procedure unmark_as_a_data_word (the_operand : in KDF9.order_word_number) is
   begin
      analysis_flags(the_operand)(is_a_data_word) := False;
   end unmark_as_a_data_word;

   procedure unmark_as_a_code_word (the_operand : in KDF9.order_word_number) is
   begin
      analysis_flags(the_operand)(is_a_code_word) := False;
   end unmark_as_a_code_word;

   procedure mark_as_a_code_word (the_operand : in KDF9.order_word_number) is
   begin
      analysis_flags(the_operand)(is_a_code_word) := True;
      unmark_as_a_data_word(the_operand);
   end mark_as_a_code_word;

   procedure mark_as_a_jump_target (the_point : in KDF9.syllable_address) is
   begin
      analysis_flags(the_point.order_word_number)(the_point.syllable_index) := True;
      mark_as_a_code_word(the_point.order_word_number);
   end mark_as_a_jump_target;

   procedure mark_as_a_data_word (the_operand : in KDF9.order_word_number) is
   begin
      analysis_flags(the_operand)(is_a_data_word) := True;
      unmark_as_a_code_word(the_operand);
   end mark_as_a_data_word;

   procedure mark_all_code_blocks (the_beginning : in KDF9.syllable_address) is
      address : KDF9.syllable_address := the_beginning;
   begin
      if address.syllable_index > 5 then
         return;  -- We have blundered into non-code words.
      end if;
      -- Mark the first syllable of the block.
      mark_as_a_jump_target(the_beginning);
      -- Mark the destinations of all jumps in the block as code.
      loop
      -- Setting NIA to 8191 LIVs, in accordance with the hardware, so avoid that.
      exit when address.order_word_number = 8191;
         set_NIA_to(address);
         decode_the_next_order;
         if is_an_invalid_order(INS)                                                  or else
               (address.order_word_number/is_a_data_word and address.syllable_index = 0) then
            return;
         else
            -- Assuming a valid code word, act on it.
            mark_as_a_code_word(address.order_word_number);
            case INS.kind is
               when normal_jump_order =>
                  if not is_a_jump_target((INS.target.order_word_number, INS.target.syllable_index)) then
                     -- Mark the jump's destination recursively.
                     -- N.B. EXIT is actioned only if it is of EXIT ARr type.
                     mark_all_code_blocks((INS.target.order_word_number, INS.target.syllable_index));
                  end if;
                  increment_by_3(address);
                  if INS.compressed_opcode = JSr  then
                     -- Mark its return point.
                     mark_as_a_jump_target(address);
                  end if;
               when one_syllable_order =>
                  increment_by_1(address);
               when two_syllable_order =>
                  if INS.compressed_opcode = JCqNZS then
                     -- Mark the preceding word.
                     mark_as_a_jump_target((address.order_word_number-1, 0));
                  end if;
                  increment_by_2(address);
               when data_access_order =>
                  increment_by_3(address);
            end case;
         end if;
      end loop;
   end mark_all_code_blocks;

   procedure mark_all_data_blocks (the_beginning : in KDF9.syllable_address) is
      address : KDF9.syllable_address := the_beginning;
   begin
      if address.syllable_index > 5 then
         return;  -- We have blundered into non-code words.
      end if;

   the_code_block_handler: loop
      -- Setting NIA to 8191 LIVs, in accordance with the hardware, so avoid that.
   exit the_code_block_handler when address.order_word_number = 8191;

         -- Deal with the possibility that we actually have a word of instruction code.
         set_NIA_to(address);
         decode_the_next_order;

         if (is_an_invalid_order(INS)                         or else
               address.order_word_number/is_a_data_word)     and then
                  not (address.order_word_number/is_a_code_word) then

            -- This word is data: make sure it is not designated as code;
            --    and find the start of the next code block.
            for a in address.order_word_number .. 8190 loop
               address := (a, 0);
            exit when is_a_jump_target(a);
               unmark_as_a_code_word(a);
               mark_as_a_data_word(a);
            end loop;
   exit the_code_block_handler when address.order_word_number = 8190;
            -- Find the syllable at which the block starts.
            for s in KDF9.syllable_index'(0) .. 5 loop
               address.syllable_index := s;
            exit when is_a_jump_target(address);
            end loop;

         else

            -- We have a order, so act on it.
            case INS.kind is
               when data_access_order =>
                  -- Note the operand word as data, not code.
                  if INS.operand < 8192 then
                     declare
                        operand : constant KDF9.order_word_number
                                := KDF9.order_word_number(INS.operand);
                     begin
                        if INS.compressed_opcode /= KDF9.decoding.SET and then
                              not is_a_jump_target(operand)               then
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

      exit the_code_block_handler when address.order_word_number = KDF9.order_word_number'Last;
      end loop the_code_block_handler;
   end mark_all_data_blocks;

   procedure reset_wrong_data_marks (the_beginning : in KDF9.syllable_address) is
      address : KDF9.syllable_address := the_beginning;
      locus   : KDF9.order_word_number;
   begin
      if address.syllable_index > 5 then
         return;  -- We have blundered into non-code words.
      end if;
      -- Unmark the first instruction of the block.
      unmark_as_a_data_word(address.order_word_number);

      -- Unmark data marks on destinations of jumps.
      loop
      -- Setting NIA to 8191 LIVs, in accordance with the hardware, so avoid that.
      exit when address.order_word_number = 8191;
         set_NIA_to(address);
         decode_the_next_order;

         if is_an_invalid_order(INS)                          or else
               address.order_word_number/is_a_data_word       or else
                  not (address.order_word_number/is_a_code_word) then
            -- We have reached the end of the code block.
            return;

         else

            -- We have a valid order, so act on it.
            case INS.kind is
               when normal_jump_order =>
                  locus := address.order_word_number;
                  increment_by_3(address);
                  if INS.target.order_word_number/is_a_data_word    then
                     -- UNmark the jump's destination recursively.
                     reset_wrong_data_marks((INS.target.order_word_number, INS.target.syllable_index));
                  end if;
                  if INS.compressed_opcode /= Jr          and then
                        INS.compressed_opcode /= EXIT_n   and then
                           locus /= address.order_word_number then
                     -- The order flows on, so the next word cannot be data.
                     unmark_as_a_data_word(address.order_word_number);
                  elsif not (address.order_word_number/is_a_data_word) then
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
      exit when address.order_word_number = KDF9.order_word_number'Last;
      end loop;
   end reset_wrong_data_marks;

   procedure mark_the_words_reachable_from (address : in KDF9.syllable_address) is
      start_point : KDF9.syllable_address;
   begin
      mark_as_a_jump_target(address);
      set_NIA_to(address);
      decode_the_next_order;
      if INS.kind = normal_jump_order then
         start_point := (INS.target.order_word_number, INS.target.syllable_index);
         mark_all_code_blocks(start_point);
         mark_all_data_blocks(start_point);
         reset_wrong_data_marks(start_point);
      end if;
   end mark_the_words_reachable_from;

   procedure markup_a_problem_program is
   begin
      if the_initial_jump_was_corrupted then
         -- We cannot sensibly locate the order words using E0  ...
         log_new_line;
         log_line("The initial jump, in E0U, has been corrupted!");
         log_new_line;
         show_core_as_syllables((0, syllable_index => 0), (0, syllable_index => 5));
         --  ... so restore it to the value it had on loading.
         restore_the_initial_jump;
         log_line("E0U has been restored to the value it had on loading.");
         log_new_line;
      end if;

      -- Mark all orders reachable from the initial jump in E0 and the restart jumps in E4.

      mark_the_words_reachable_from((0, syllable_index => 0));
      mark_the_words_reachable_from((4, syllable_index => 0));
      mark_the_words_reachable_from((4, syllable_index => 3));

      -- Mark the words between E0 and P0 as data, skipping E4.
      mark_as_a_data_word(1);
      mark_as_a_data_word(2);
      mark_as_a_data_word(3);
      set_NIA_to((0, syllable_index => 0));
      decode_the_next_order;
      for d in 5 .. INS.target.order_word_number-1 loop
         mark_as_a_data_word(d);
      end loop;

      the_program_has_been_analysed := True;
   end markup_a_problem_program;

   -- This analysis assumes that the Director has much the same structure as KKT40E007UPU.
   procedure markup_a_Director (pre_run : in Boolean) is

--        procedure deal_with_a_3_syllable_order (address : in KDF9.syllable_address) is
--        begin
--           set_NIA_to(address);
--           decode_the_next_order;
--
--           if INS.kind = normal_jump_order then
--              mark_the_words_reachable_from(address);
--           elsif INS.kind = data_access_order then
--              -- Mark a feasible operand as data.
--              if INS.operand < 8192 then
--                 declare
--                    operand : constant KDF9.order_word_number := KDF9.order_word_number(INS.operand);
--                 begin
--                    if INS.compressed_opcode /= KDF9.decoding.SET and then
--                          not is_a_jump_target(operand)               then
--                       mark_as_a_data_word(operand);
--                    end if;
--                 end;
--              end if;
--           else
--              -- Treat it as code.
--              mark_as_a_jump_target(address);
--           end if;
--        end deal_with_a_3_syllable_order;
--
--        procedure show_flags (a : in KDF9.order_word_number) is
--        begin
--           log_line(a'Image & ":  "
--            & (if analysis_flags(a)(is_a_code_word) then "CODE " else "DATA ")
--            & (if analysis_flags(a)(0) then "T" else " ")
--            & (if analysis_flags(a)(1) then "T" else " ")
--            & (if analysis_flags(a)(2) then "T" else " ")
--            & (if analysis_flags(a)(3) then "T" else " ")
--            & (if analysis_flags(a)(4) then "T" else " ")
--            & (if analysis_flags(a)(5) then "T" else " ")
--             );
--        end show_flags;

   begin
      the_program_has_been_analysed := False;
      BA := 0;  -- Director starts at physical word 0.

      -- It does not have a jump in E0U, unlike problem programs.
      -- Instead it has a fixed sequence of orders to reset the hardware.
      -- Before intialization, it is: K4; SHL+63; =+Q0; with the code: #3740416437675016.
      -- After intialization,  it is: Q0; SHL+63; =+Q0; with the code: #3620716437675016.

       -- Check that we do actually have a Director to examine.

      if pre_run then
         if fetch_word(0) /= 8#3740416437675016# then
            log_line("The loaded program is not a Director bootstrap!");
            return;
         else
            log_line("The loaded program is a Director bootstrap.");
         end if;

         -- The jumps in E2 and E4 are absent until after Director's initialzation,
         --    so they are not present in a pre-run state.
         -- All 9 words of the bootstrap are filled with instructions.
         for w in order_word_number'(0) .. 8 loop
            mark_as_a_code_word(w);
         end loop;
         mark_as_a_jump_target((0, 0));
         the_program_has_been_analysed := True;
         return;
      end if;

      if fetch_word(0) /= 8#3620716437675016# then
         log_line("The loaded program is not a Director!");
         return;
      else
         log_line("The loaded program is a Director.");
      end if;

      -- An initialised Director has a number of jumps in words 2 and 4, but not word 0.
      -- Give up if these are absent.

      set_NIA_to((2, syllable_index => 0));
      decode_the_next_order;
      if INS.kind /= normal_jump_order then
         log_line("An expected jump, in E2U, has not been found!");
         return;
      end if;
      mark_as_a_jump_target((2, syllable_index => 0));
      mark_the_words_reachable_from((2, syllable_index => 0));
      mark_as_a_jump_target((INS.target.order_word_number, INS.target.syllable_index));
      set_NIA_to((4, syllable_index => 0));
      decode_the_next_order;
      if INS.kind /= normal_jump_order then
         log_line("An expected jump, in E4U, has not been found!");
         return;
      end if;
      mark_as_a_jump_target((4, syllable_index => 0));
      mark_the_words_reachable_from((4, syllable_index => 0));
      mark_as_a_jump_target((INS.target.order_word_number, INS.target.syllable_index));

      set_NIA_to((4, syllable_index => 3));
      decode_the_next_order;
      if INS.kind /= normal_jump_order then
         log_line("An expected jump, in E4L, has not been found!");
         return;
      end if;
      mark_as_a_jump_target((4, syllable_index => 3));
      mark_the_words_reachable_from((4, syllable_index => 3));
      mark_as_a_jump_target((INS.target.order_word_number, INS.target.syllable_index));

      -- E0 is marked because interrupts cause a jump to it, in effect.
      mark_as_a_jump_target((0, syllable_index => 0));

      -- Mark E3 and E5 through AP0 - 1 as data words.
      mark_as_a_data_word(3);
      set_NIA_to((2, 0));
      decode_the_next_order;
      for d in 5 .. INS.target.order_word_number-1 loop
         mark_as_a_data_word(d);
      end loop;

      the_program_has_been_analysed := True;
   end markup_a_Director;

   procedure mark_all_code_blocks_and_data_blocks (pre_run : in Boolean) is
   begin
      clear_all_analysis_flags;
      if the_execution_mode = boot_mode  then
         markup_a_Director(pre_run);
      else
         markup_a_problem_program;
      end if;
      -- Do a second marking for (e.g.) an overlaid area of a Director, at a specified address.
      if root_address < 8191 then
         mark_the_words_reachable_from((root_address, 0));
      end if;
   end mark_all_code_blocks_and_data_blocks;

   procedure show_core_as_Usercode (first, last  : in KDF9.syllable_address;
                                    octal_option : in Boolean) is

      six_DUMMIES : constant KDF9.word := 8#0360741703607417#;
      prev_word   : KDF9.word := 8#0706050403020100#; -- invalid opcodes
      comparator  : KDF9.word := prev_word;
      address     : KDF9.syllable_address;
      this_word   : KDF9.word;

      procedure show_a_block_of_orders (address : in out KDF9.syllable_address) is

         function is_a_store_order (decoded : KDF9.decoded_order)
         return Boolean
         is (
             if decoded.kind = one_syllable_order then
                False
             elsif decoded.kind = two_syllable_order then
                (
                 case decoded.compressed_opcode is
                   when TO_MkMq   | TO_MkMqQ
                      | TO_MkMqH  | TO_MkMqQH
                      | TO_MkMqN  | TO_MkMqQN
                      | TO_MkMqHN | TO_MkMqQHN
                      | TO_Kq     | TO_LINK    => True,
                   when others                 => False
                )
             elsif decoded.kind = data_access_order then
                (
                 case decoded.compressed_opcode is
                    when TO_EaMq | TO_EaMqQ => True,
                    when others             => False
                )
             else
                False
            );

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

         last_nz_location : KDF9.syllable_address;

      begin -- show_a_block_of_orders
         this_word := fetch_word(KDF9.address(address.order_word_number));

         if this_word in not 0 | 0 | six_DUMMIES then
            -- The word is not worth logging.  Step on.
            address := (address.order_word_number+1, 0);
            return;
         end if;

         -- Handle data words.
         if address.order_word_number/is_a_data_word then
            set_at_new_line;
         end if;
         loop
            if address.order_word_number/is_a_data_word then
               last_nz_location := address;

               -- Display a line of data.
               log(oct_or_dec_of(address, octal_option) & ": ");
               set_line_at(jump_tab);
               show_in_various_formats(fetch_word(KDF9.address(address.order_word_number)),
                                       column => jump_tab);
               log_new_line;

               -- Skip over any following zero words (DUMMY0 or uninitialized V stores).
               loop
                  if address.order_word_number = last.order_word_number then
                     -- The whole block has been processed.
                     return;
                  end if;
                  address := (address.order_word_number+1, 0);
               exit when fetch_word(KDF9.address(address.order_word_number)) /= 0;
               end loop;
               if address.order_word_number > last_nz_location.order_word_number+1 then
                  log("========  zeros  ========");
                  log_new_line;
               end if;
            else
               -- We have found an instruction word.
               log_new_line;
         exit;
            end if;
         end loop;

         -- Handle instruction words.
         loop
         -- Setting NIA to 8191 LIVs, in accordance with the hardware, so avoid that.
         exit when address.order_word_number = 8191;
            this_word := fetch_word(KDF9.address(address.order_word_number));

            -- Suppress output of more than 1 of a block of equal values.
            if this_word = comparator and this_word = prev_word then
               -- The word is not worth logging.  Step on.
               address := (address.order_word_number+1, 0);
               return;
            end if;

            if this_word in not 0 | 0 | six_DUMMIES then
               -- Take this word as the new basis for comparison.
               comparator := this_word;
            end if;

            -- This updates the Instruction word buffers.  Nasty side effect that has to be undone!
            set_NIA_to(address);
            decode_the_next_order;
            if is_an_invalid_order(INS) then
               -- The word is not worth logging.  Step on.
               address := (address.order_word_number+1, 0);
               return;
            end if;

            if is_a_jump_target(address) then
               -- Jumps go at the start of a fresh line for best visibility.
               -- Label the jump with its address at the start of the line for easy reference.
               set_at_new_line;
               log(oct_or_dec_of(address, octal_option) & ": ");
               log_new_line;
            end if;

            -- Set the tab position appropriately for the order type.
            case INS.kind is
               when one_syllable_order | data_access_order =>
                  set_line_at_minimum(first_tab);
               when two_syllable_order =>
                  case INS.compressed_opcode is
                     when JCqNZS =>
                        set_line_at(jump_tab);
                     when CT_PMB_PMC_BUSY_Qq
                        | PAR_Qq
                        | PMF_PMG_Qq
                        | PIA_PIC_CLO_TLO_Qq
                        | PIB_PID_Qq
                        | PIE_PIG_Qq
                        | PIF_PIH_Qq
                        | POA_POC_POE_POF_PMH_Qq
                        | POB_POD_Qq
                        | POG_POL_Qq
                        | POH_POK_Qq
                        | PMA_PMK_INT_Qq
                        | PMA_PMK_INT_Qq+1
                        | PMD_PME_PML_Qq
                        | PMD_PME_PML_Qq+1 =>
                        set_line_at(first_tab);
                     when TO_Kq
                        | TO_LINK =>
                        set_line_at(jump_tab);
                     when others =>
                        if panel_logger.column < first_tab then
                           set_line_at_minimum(first_tab);
                        end if;
                  end case;
               when normal_jump_order =>
                  set_line_at(jump_tab);
            end case;

            -- Show the order in pseudo-Usercode format.
            log(the_full_name_of(INS, octal_option) &  "; ");

            case INS.kind is
               when one_syllable_order =>
                  increment_by_1(address);
               when two_syllable_order =>
                  increment_by_2(address);
               when normal_jump_order | data_access_order =>
                  increment_by_3(address);
            end case;

            if address.order_word_number = last.order_word_number then
               -- All done with this block.
               log_new_line;
               return;
            end if;

            if (address.order_word_number+1)/is_a_data_word then
               -- We have reached the end of the orders and are about to run into data.
               return;
            end if;

            if is_a_store_order(INS)                   or else
                  INS.compressed_opcode = JCqNZS       or else
                     INS.kind = normal_jump_order      or else
                        panel_logger.column > last_column then
               -- Make store to core and jump orders end their line for best visibility.
               log_new_line;
            elsif this_word = comparator and this_word /= prev_word then
               -- Display the placeholder of a suppressed group of equal words.
               log_new_line;
               log_line("==========  #"
                      & oct_of(KDF9.syllable(this_word and 255))
                      & "  ==========");
               address := (address.order_word_number+1, 0);
               if address.order_word_number > last.order_word_number or else
                     address.order_word_number/is_a_data_word           then
                  -- We have reached the end of the orders or are about to run into data.
                  return;
               end if;
            end if;
            prev_word := this_word;

         end loop;

      end show_a_block_of_orders;

   begin -- show_core_as_Usercode
      if not the_program_has_been_analysed then
         mark_all_code_blocks_and_data_blocks(pre_run => False);
      end if;
      if the_program_has_been_analysed then
         log_line("Core store interpreted as instructions.");
          -- Ensure that physical store is examined when running in boot mode.
         BA := 0;
         address := first;
         loop
            show_a_block_of_orders(address);
            exit when address.order_word_number >= last.order_word_number;
         end loop;
         log_new_line;
         log_rule;
      else
         log_line(" ... Core store cannot be interpreted as instructions!");
         log_new_line;
      end if;
   end show_core_as_Usercode;

   procedure show_core_as_syllables (first, last  : KDF9.syllable_address) is

      address     :   KDF9.syllable_address;

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
            if address.syllable_index = 0 then
               log_new_line;
               log(oct_of(address) & ": ");
               set_line_at(jump_tab);
            end if;
            log(oct_of(fetch_syllable(address)) &  "; ");
            increment_by_1(address);
         exit when address.order_word_number > last.order_word_number;
         end loop;
         log_new_line;
      end show_a_block;

    begin  -- show_core_as_syllables
       BA := 0; -- Ensure that physical store is examined when running in boot mode.
      log_line("Core store interpreted as order syllables.");
      address := first;
      loop
         show_a_block;
         exit when address.order_word_number > last.order_word_number;
      end loop;
      log_new_line;
      log_rule;
   end show_core_as_syllables;

   procedure poke (address    : in KDF9.address;
                   sub_word   : in sub_word_flag;
                   position   : in KDF9.address;
                   value      : in KDF9.word) is
   begin
      case sub_word is
         when 'W' | 'w' =>
            store_word(value, address);
         when 'U' | 'u' =>
            store_halfword(value*2**24, address, 0);
         when 'L' | 'l' =>
            store_halfword(value*2**24, address, 1);
         when 'S' | 's' =>
            store_syllable(KDF9.syllable(value), address, KDF9.syllable_index(position));
         when 'C' | 'c' =>
            store_symbol(KDF9_char_sets.symbol(value), address, KDF9_char_sets.symbol_index(position));
      end case;
   end poke;

end state_display;
