-- The architecturally-defined data and register formats of the KDF9 computer.
--
-- This file is part of ee9 (8.2a), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2022, W. Findlay; all rights reserved.
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

with System;
--
private with KDF9_char_sets;
private with string_editing;

package KDF9 is

   --
   --
   -- The fundamental storage unit is the 48-bit word.
   --
   --

   --
   -- The 48-bit word, considered as an unsigned integer.
   --

   type word is mod 2**48;

   word_mask     : constant := 8#7777777777777777#;
   min_word      : constant := 8#4000000000000000#;
   max_word      : constant := 8#3777777777777777#;
   all_zero_bits : constant := 0;
   sign_bit      : constant := KDF9.min_word;
   not_sign_bit  : constant := KDF9.max_word;
   all_one_bits  : constant := KDF9.word_mask;


   --
   -- The 96-bit double word, considered as a pair of words.
   --

   type pair is
      record
         msw, lsw : KDF9.word;
      end record;


   --
   -- The basic 16-bit operand.
   --

   type field_of_16_bits is mod 2**16;

   --
   -- The 16-bit word, considered as a field of a Q register.
   --

   type Q_part is new KDF9.field_of_16_bits;

   Q_part_mask : constant := KDF9.Q_part'Last;

   function sign_extended (Q : KDF9.Q_part)
   return KDF9.word
      with Inline;

   --
   -- The 16-bit word, considered as a buffer (DMA channel) number.
   --

   subtype buffer_number is KDF9.Q_part range 0 .. 15;

   buffer_number_mask : constant := buffer_number'Last;

   --
   -- The 16-bit word, considered as a core-store address.
   --

   subtype address is KDF9.Q_part range 0 .. 8#77777#;

   --
   -- The Q-store element.
   --

   type Q_register is
      record
         C, I, M : KDF9.Q_part;
      end record;

   function as_Q (the_word : KDF9.word)
   return KDF9.Q_register
      with Inline;

   function as_word (the_Q : KDF9.Q_register)
   return KDF9.word
      with Inline;


   --
   -- The 8-bit instruction syllable and its components.
   --

   type syllable is mod 2**8;

   subtype compressed_opcode is KDF9.syllable range 0 .. 63;
   subtype Q_number          is KDF9.syllable range 0 .. 15;

   type syllable_group is
      record
         syllable_0, syllable_1, syllable_2 : KDF9.syllable := 0;
      end record;


   --
   -- An instruction address.
   --
   -- N.B. 5 is the hardware's largest valid syllable address.
   -- The values 6 and 7 are used as diagnostic flags by ee9.
   -- They cause a RESET trap if encountered during execution.
   --

   type code_address   is mod 2**13;
   type syllable_index is mod 2**3;

   type syllable_address is
      record
         code_address   : KDF9.code_address;
         syllable_index : KDF9.syllable_index;
      end record;

   --
   -- An instruction address, in the packed format of a hardware (SJNS) link.
   --

   type SJNS_link is new KDF9.syllable_address
      with Size => 16;
   for SJNS_link'Bit_Order use System.Low_Order_First;
   for SJNS_link use
      record
         code_address   at 0 range  0 .. 12;
         syllable_index at 0 range 13 .. 15;
      end record;

   function as_word (the_link : KDF9.SJNS_link)
   return KDF9.word;

   function as_link (the_word : KDF9.word)
   return KDF9.SJNS_link;

   procedure increment_by_1 (the_link : in out KDF9.syllable_address)
      with Inline;

   procedure increment_by_2 (the_link : in out KDF9.syllable_address)
      with Inline;

   procedure increment_by_3 (the_link : in out KDF9.syllable_address)
      with Inline;


   --
   -- The KDF9 halfword. Each occupies 24 bits, packed 2 per word.
   --

   type halfword is mod 2**24;
   halfword_mask : constant := 8#77_77_77_77#;

   subtype halfword_number is KDF9.address range 0 .. 1;


   --
   -- The KDF9 character occupies six bits, and they are packed 8 per word.
   -- The various character sets, and the symbol type, are defined in the package KDF9_char_sets.
   --


   --
   --
   --
   -- The following types define the structure of the KDF9's programmable registers.
   --
   --
   --


   --
   -- authenticity_mode is declared here instead of in settings, to avoid a cyclic dependency.
   --

   type authenticity_mode is (modern_times_mode, authentic_time_mode);

   the_authenticity_default : constant KDF9.authenticity_mode := modern_times_mode;
   the_authenticity_mode    :          KDF9.authenticity_mode := the_authenticity_default;


   --
   --
   --
   -- The following variables (the_NEST, the_SJNS and the_Q_store) constitute
   --    the emulation microcode's fixed working set of registers.
   -- ee9 (unlike the real KDF9) swaps them with register_bank(the_context)
   --    when a context switch is made by the =K3 instruction.
   -- The real KDF9 used register_bank(the_context) directly for operands.
   -- ee9's approach improves host cache locality and avoids indexing overheads,
   --    the trade off being microscopically increased context-switching time.
   --
   --
   --

   --
   -- The NEST.
   --

   type NEST_depth is mod 19;

   type NEST is array (KDF9.NEST_depth) of KDF9.word;

   the_NEST       : KDF9.NEST;
   the_NEST_depth : KDF9.NEST_depth  := 0;

   -- The ensure_that_the_NEST_holds* procedures trap NOUV.
   -- They are used to validate operations that reduce the NEST depth.

   procedure ensure_that_the_NEST_holds (at_least : in KDF9.NEST_depth)
      with Inline;

   procedure ensure_that_the_NEST_holds_an_operand
      with Inline;

   procedure ensure_that_the_NEST_holds_2_operands
      with Inline;

   function operand_words_needed (need : KDF9.NEST_depth)
   return String
      with Inline => False;

   function pop
   return KDF9.word
      with Inline;

   procedure pop
      with Inline;

   procedure write_top (the_word : in KDF9.word)
      with Inline;

   function read_top
   return KDF9.word
      with Inline;

   function pop
   return KDF9.pair
      with Inline;

   procedure pop_pair
      with Inline;

   procedure write_top (the_pair : in KDF9.pair)
      with Inline;

   function read_top
   return KDF9.pair
      with Inline;

   -- The ensure_that_the_NEST_has_room_for* procedures trap NOUV.
   -- They are used to validate operations that increase the NEST depth.

   procedure ensure_that_the_NEST_has_room_for (at_least : in KDF9.NEST_depth)
      with Inline;

   procedure ensure_that_the_NEST_has_room_for_a_result
      with Inline;

   procedure push (the_word : in KDF9.word)
      with Inline;

   procedure ensure_that_the_NEST_has_room_for_2_results
      with Inline;

   procedure push (the_pair : in KDF9.pair)
      with Inline;


   --
   -- The SJNS.
   --

   type SJNS_depth is mod 17;

   type SJNS is array (KDF9.SJNS_depth) of KDF9.SJNS_link;

   the_SJNS       : KDF9.SJNS;
   JB             : KDF9.SJNS_link renames the_SJNS(16);
   the_SJNS_depth : KDF9.SJNS_depth := 0;

   procedure ensure_that_the_SJNS_is_not_empty
      with Inline;

   function pop
   return KDF9.syllable_address
      with Inline;

   function SJNS_top
   return KDF9.SJNS_link
      with Inline;

   procedure ensure_that_the_SJNS_is_not_full
      with Inline;

   procedure push (the_link : in KDF9.syllable_address)
      with Inline;


   --
   -- The Q Store.
   -- Q0 is kept permanently zeroised.
   --

   type Q_store is array (KDF9.Q_number) of KDF9.Q_register;

   the_Q_store : KDF9.Q_store;


   --
   -- The Boolean registers.
   --

   the_V_bit_is_set : Boolean := False;
   the_T_bit_is_set : Boolean := False;


   --
   --
   -- The following are to do with maintaining the virtual time.
   --
   --

   -- The date a multiple of 28 years ago has the same day/date correspondence as today.
   -- To avoid exposing KDF9's lack of Y2K compliance, ee9 uses such a date before 2000.
   -- 8-)
   -- todays_date_28n_years_ago returns a word of 8 KDF9 characters in the format DD/MM/YY.

   type us is mod 2**64;  -- The emulation clocks tick in microseconds (unlike KDF9's clock).

   -- The virtual processor time.
   the_CPU_time  : KDF9.us := 0;

   -- The amount by which the_CPU_time is increased by an instruction execution.
   the_CPU_delta : KDF9.us := 0;

   -- The virtual elapsed time at which the next IO interrupt is expected.
   the_next_interrupt_time : KDF9.us := KDF9.us'Last;

   -- The virtual elapsed time, capped to prevent a spurious double-clock (RESET) interrupt.
   function the_clock_time
   return KDF9.us
      with Inline;

   -- Advance to the largest of the_CPU_time, the_elapsed_time, the_last_delay_time, and past.
   -- Cap the increase to prevent a spurious double-clock (RESET) interrupt in Director.
   -- If necessary, pause execution until the real time equals the virtual elapsed time.
   procedure advance_the_clock (past : in KDF9.us);

   -- If necessary, pause execution until the real time equals the virtual elapsed time.
   procedure synchronize_the_real_and_virtual_times;

   -- Getthe date as a word of 8 characters, thus: dd/mm/yy, where 19yy = this year-28n.
   function todays_date_28n_years_ago
   return KDF9.word;

   -- The real time in microseconds since midnight.
   function the_time_of_day
   return KDF9.us;

------------------------------------------------------------------------------------------------

   --
   --
   -- The following registers are used only in Director state.
   --
   --


   --
   -- The following are to do with the K1 order.
   --

   type priority is mod 2**2;

   -- CPL = priority level of the currently-executing problem program.

   CPL : KDF9.priority;

   -- BA = word address of first allocated word (NOT group number as in the KDF9).

   BA  : KDF9.address;

   -- NOL = word address of last allocated word (NOT group number as in the KDF9).

   NOL : KDF9.address;

   -- Set BA (setting bits D38:47), CPL (D34:35) and NOL (D24:33).

   procedure set_K1_register (setting : in KDF9.word);


   --
   -- The following are to do with the =K2 order.
   --

   -- The Current Peripheral Device Allocation Register.

   type CPDAR is array (KDF9.buffer_number) of Boolean
      with Component_Size => 8, Convention => C;

   the_CPDAR : KDF9.CPDAR;

   -- Set CPDAR (setting bits D32 .. D47).

   procedure set_K2_register (setting : in KDF9.word);


   --
   -- The following are to do with the =K3 and K7 orders.
   --

   type user_register_set is
      record
         NEST     : KDF9.NEST;
         SJNS     : KDF9.SJNS;
         Q_store  : KDF9.Q_store;
      end record;


   -- There are 4 sets of user registers.
   -- The execution context is the number of the register set in active use.

   type context is mod 2**2;

   -- register_bank holds the currently inactive register sets.

   register_bank : array(KDF9.context) of KDF9.user_register_set;

   -- KDF9 actually indexed the register bank with the value of the_context,
   --   but the emulator swaps register sets between register_bank and
   --      the_NEST, the_SJNS, and the_Q_store (q.v.).

   the_context : KDF9.context := 0;

   -- Set context (bits D46:47), NEST_depth (D41:45) and SJNS_depth (D36:41).

   procedure set_K3_register (setting : in KDF9.word);

   -- Get BA (bits D0 .. D9), CPL (D12 .. D13) and NOL (D14 .. D23).

   function get_K7_operand
   return KDF9.word;


   --
   -- The following are to do with the K4 order.
   --

   type interrupt_number is range 22 .. 31;

   -- higher PRiority PRogram unblocked by end of I/O, or INTQq on busy device
   caused_by_PR    : constant KDF9.interrupt_number := 22;
   PR_exception    : exception;

   -- FLEXowriter interrupt from operator
   caused_by_FLEX  : constant KDF9.interrupt_number := 23;
   FLEX_exception  : exception;

   -- Lock-In Violation (attempt at a disallowed operation)
   caused_by_LIV   : constant KDF9.interrupt_number := 24;
   LIV_exception   : exception;

   -- Nest (or SJNS) Over/Underflow Violation
   caused_by_NOUV  : constant KDF9.interrupt_number := 25;
   NOUV_exception  : exception;

   -- End of Director Transfer, or I/O priority inversion
   caused_by_EDT   : constant KDF9.interrupt_number := 26;
   EDT_exception   : exception;

   -- OUT system call
   caused_by_OUT   : constant KDF9.interrupt_number := 27;
   OUT_exception   : exception;

   -- Lock-Out Violation
   caused_by_LOV   : constant KDF9.interrupt_number := 28;
   LOV_exception   : exception;

   -- invalid syllable number or 'double-clock'
   caused_by_RESET : constant KDF9.interrupt_number := 29;
   RESET_exception : exception;

   type RFIR is array (KDF9.interrupt_number) of Boolean;

   the_RFIR : KDF9.RFIR := (others => False);

   -- The time at which the last K4 order was executed.
   the_last_K4_time : KDF9.us := 0;

   -- Get clock (bits D0:15) and RFIR (D16:31), clearing both.
   function get_K4_operand
   return KDF9.word
      with Inline => False;

   -- An interrupt is raised when 1 second expires outside Director;
   --    the flag does not correspond to any RFIR bit.
   caused_by_CLOCK : constant KDF9.interrupt_number := 31;
   CLOCK_exception : exception;

   -- This is for tracing a return from Director;
   --    the flag does not correspond to any RFIR bit.
   EXITD_flag : constant KDF9.interrupt_number := 30;

   -- abandon_this_order is raised
   -- 1. when an interrupt is punted to Director
   -- OR
   -- 2. when an operation is attempted, in a program running under Director,
   --    that is invalid but is not defined to cause a LIV interrupt.
   abandon_this_order : exception;

   --
   -- The following are to do with the K5 order.
   --

   -- The Program Hold-Up register is internal to I/O Control.
   -- Get PHUi (bits D6i .. 6i+5), i = 0 .. 3.

   function get_K5_operand
   return KDF9.word;


   --
   -- The following are to do with management of the CPU's internal state.
   --

   type CPU_state is (Director_state, program_state);

   the_CPU_state : KDF9.CPU_state;

   procedure reset_V_and_T;

   procedure reset_the_CPU_state (initial_entry : KDF9.syllable_address);

   procedure reset_the_internal_registers (the_new_state : in CPU_state);

   procedure fail_in_problem_program_state;

   procedure LOV_if_user_mode (cause : in String);

   procedure return_from_Director_to (new_IAR : in KDF9.syllable_address);

   procedure effect_interrupt (caused_by_this : in KDF9.interrupt_number; message : in String)
      with Inline => False;

   procedure check_for_a_clock_interrupt
      with Inline;

   procedure trap_illegal_instruction (the_message : in String := "invalid opcode")
      with Inline => False;

   procedure trap_operator_error (the_message : in String)
      with Inline => False, No_Return;

   procedure trap_unimplemented_feature (the_message : in String)
      with Inline => False, No_Return;

   procedure trap_failing_OUT (OUT_number : in KDF9.word; the_message : in String)
      with Inline => False, No_Return;

   procedure trap_invalid_paper_tape (the_message : in String)
      with Inline => False, No_Return;

   procedure reset_the_program_state;


   --
   --
   -- Instruction fetch and decode.
   --
   --

   -- These Instruction Address Registers are the nearest KDF9 has
   --    to a conventional 'Program Counter' register.
   -- NIA is significant only after an instruction has been decoded.

   function NIA
   return KDF9.syllable_address  -- the Next Instruction Address
      with Inline;

   function NIA_word_number
   return KDF9.code_address
      with Inline;

   CIA : KDF9.syllable_address := (0, 0);  -- the Current Instruction Address

   -- IWB0 and IWB1 in KDF9 contained the current 2 instruction words.
   -- A 'short' loop, initiated by the JCqNZS instruction, ran entirely
   --    inside the IWBs, obviating repeated instruction-fetch overhead.
   -- Director exploits this in a loop that zeroizes the whole of core,
   --    including that loop, which runs, immune to overwriting, in the IWBs.

   procedure set_NIA_to (new_NIA : in KDF9.syllable_address)
      with Inline;

   procedure set_NIA_to_the_INS_target_address
      with Inline;

   procedure set_IWB0_and_IWB1_for_a_JCqNZS_loop
      with Inline;

   procedure go_back_to_the_start_of_IWB0
      with Inline;

   procedure continue_after_JCqNZS
      with Inline;

   -- Bits 0-1 of every order indicate its type as follows.

   type INS_kind is mod 2**2;

   one_syllable_order : constant := 0;
   two_syllable_order : constant := 1;
   normal_jump_order  : constant := 2;
   data_access_order  : constant := 3;

   type decoded_order is
      record
         order : KDF9.syllable_group := (0, 0, 0);
         kind  : KDF9.INS_kind := 0;

         -- The compressed_opcode is:
         --    bits 2-7 of 1- and 2-syllable orders
         --    bits 2-3|8-11 of normal jumps
         --    bits 5-7 of SET and directly-addressed store access orders.
         -- See the KDF9.decoding package.
         compressed_opcode : KDF9.compressed_opcode := 0;

          -- Qq is bits 8-11, Qk is bits 12-15.
         Qq, Qk : KDF9.Q_number := 0;

         -- For an jump instruction, syllable_index is bits 5-7.
         target : KDF9.syllable_address;

         -- For a data address or value (SET), operand is bits 2-4|12-23.
         operand : KDF9.Q_part := 0;
      end record;

   INS : KDF9.decoded_order;  -- analogous to the INS register in Main Control

   -- After decode_the_next_order:
   --    INS contains the whole instruction at the address given by CIA,
   --       with its components unpacked (not all are significant in every case).

   procedure decode_the_next_order
      with Inline;

   procedure decode (the_order : in out KDF9.decoded_order)
      with Inline;

   procedure process_syllable_0_of_INS
      with Inline;

   procedure process_syllable_1_of_INS
      with Inline;

   procedure process_syllables_1_and_2_of_a_jump_order
      with Inline;

   procedure process_syllables_1_and_2_of_a_data_access_order
      with Inline;

   -- the_order_at_NIA gets three syllables starting at [NIA].
   -- It is FOR DIAGNOSTIC USE ONLY!
   -- It does NOT update the CPU time properly and must not be used inside an instruction cycle!

   function the_order_at_NIA
   return KDF9.syllable_group
      with Inline;

   -- Save E0U, lest the initial jump in E0 be corrupted during the run.
   procedure save_the_initial_jump;

   -- Restore E0U to its saved value.
   procedure restore_the_initial_jump;

   -- Check whether E0U has changed.
   function the_initial_jump_was_corrupted
   return Boolean;

   -- True if the parameter is not a valid KDF9 instruction.
   function is_an_invalid_order (decoded : KDF9.decoded_order)
   return Boolean;

   -- True if the parameter is a well-formed unconditional jump.
   function is_an_unconditional_jump (decoded : KDF9.decoded_order)
   return Boolean;

   --
   -- The Instruction Counter Register, ICR, (N.B. NOT a 'PROGRAM counter')
   --   indicates the number of instructions executed by the KDF9.
   --

   type order_counter is mod 2**64;

   ICR : KDF9.order_counter := 0;


   --
   -- The following support hashed execution-signature checking,
   --    mainly for self-checking of new versions and ports.
   --

   function the_digital_signature
   return KDF9.word;

   procedure update_the_digital_signature
      with Inline;

private

   use KDF9_char_sets; pragma Warnings(Off, KDF9_char_sets);
   use string_editing; pragma Warnings(Off, string_editing);

   the_elapsed_time    : KDF9.us := 0;

   fetching_normally   : Boolean := True;

end KDF9;
