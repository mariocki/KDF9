-- Emulation of the common functionality of a KDF9 IOC "buffer" (DMA channel),
--    with fail-stop stubs for operations having device-specific behaviour.
--
-- This file is part of ee9 (8.0k), the GNU Ada emulator of the English Electric KDF9.
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

with Ada.Finalization;
--
with KDF9;

private with Ada.Exceptions;
--
private with exceptions;
private with formatting;
private with HCI;
private with host_IO;
private with KDF9_char_sets;
private with KDF9.store;
private with POSIX;
private with settings;

use  Ada.Finalization;
--
use  KDF9;

package IOC is

   -- N.B. the KDF9 'buffer' is a DMA controller in more modern terminology.

   -- Each KDF9 buffer is externally characterized by:
   --    its (absolute) number,
   --    its (attached-device) kind, and
   --    its unit (the number of that device within its kind).

   -- A device of AD_kind is attached to a buffer with No Device connected.
   -- If commanded, it performs a basic default action,
   --    which is to cause a LIV interrupt in the case of transfers,
   --       but is both benign and appropriate for all other operations.

   type device_kind is
      (CP_kind,  -- Card Punch
       CR_kind,  -- Card Reader
       DR_kind,  -- Drum
       FD_kind,  -- Fixed Disc
       FW_kind,  -- FlexoWriter (monitor typewriter)
       GP_kind,  -- Graph Plotter (Calcomp 120' by 29.5" model)
       LP_kind,  -- Line Printer
       MT_kind,  -- Magnetic Tape
       SI_kind,  -- Standard Interface buffer
       ST_kind,  -- Seven Track (IBM) magnetic Tape
       TP_kind,  -- Tape Punch
       TR_kind,  -- Tape Reader
       AD_kind   -- Absent Device
      );

   -- This is the number of the buffer a device is connected to.
   subtype device_number is KDF9.Q_part range 0 .. 15;

   -- This is the index of a device within devices of the type in the configuration.
   subtype unit_number   is KDF9.Q_part range 0 .. 15;

   -- An IOC.device_name is of the form XYu, where XY is a two-letter device-type code
   --    and u is the logical unit number, in the range 0..F, of the device within its type.

   subtype device_name is String(1..3);

--
--
   -- This is the root for all I/O device types.
--
--

   type device (number : IOC.device_number; unit : IOC.unit_number)
   is abstract new Limited_Controlled with private;

   -- The quantum is the time, in µs, taken to transfer a basic datum.
   -- For unit-record devices (CR, CP, LP) this is the card/line.
   -- For other devices it is the KDF9 character.
   function quantum (the_buffer : IOC.device)
   return KDF9.us
   is abstract;

   function kind (the_buffer : IOC.device)
   return IOC.device_kind
   is abstract;

   -- This is overridden separately for fast and slow devices.
   procedure add_in_the_IO_CPU_time (the_buffer  : in IOC.device;
                                     bytes_moved : in KDF9.word)
   is abstract;

   -- True iff the_buffer has been opened but not yet closed.
   -- It is overridden separately for magnetic tapes and all other devices.
   function is_open (the_buffer : IOC.device)
   return Boolean
   is abstract;

   -- A measure of the I/O volume transferred by the_buffer, so far.
   function usage (the_buffer : IOC.device)
   return KDF9.word;

   -- Ensure that all output to the_buffer has been transmitted.
   procedure flush (the_buffer : in out IOC.device);

   -- Make the_buffer unavailable for further I/O use, after flushing if necessary.
   procedure close (the_buffer : in out IOC.device);

   -- A IOC.device_name is of the form XYn, where XY is a two-letter device-type
   --    code (e.g., "LP" or "CR"); and n is the one-digit logical unit number
   --       of a device within its category (n may be in hexadecimal).

   function device_name_of (the_buffer : IOC.device)
   return IOC.device_name;

   function device_name_of (the_number : IOC.device_number)
   return IOC.device_name;

   function device_kind_of (the_number : IOC.device_number)
   return IOC.device_kind;

   -- Get the device-specific name of the I/O order, or the generic name if there isn't one.
   function mnemonic (order : in String; class : in IOC.device_name)
   return String;

   -- An I/O operation may fail for two distict reasons:
   -- 1. the order is illegal per se
   -- 2. the order is legal, but is attempting an impossible effect.

   -- trap_illegal_IO_operation fails the run because of an attempt to use an I/O order
   --   that is illegal or undefined for the device concerned.
   procedure trap_illegal_IO_operation (order       : in String;
                                        buffer      : in IOC.device;
                                        Q_operand   : in KDF9.Q_register;
                                        set_offline : in Boolean)
      with Inline => False;

   -- trap_failing_IO_operation fails the run iff either:
   -- 1. ee9 is running in a non-boot mode, because nothing more can usefully be done
   -- OR
   -- 2. Director is running, because an impossible operation implies a serious failure in Director.
   --
   -- In boot mode, when Director is not running, it sets the buffer abnormal and abandons the order.
   -- It is then up to the problem program to act accordingly.  Failure to do so may LIV.

   procedure trap_failing_IO_operation (the_buffer : in out IOC.device; the_message : in String)
      with Inline => False;

   procedure trap_failing_IO_operation (the_culprit : in String; the_message : in String)
      with Inline => False;

   -- The elapsed time for the I/O of the given number of atomic_items
   --    which may be, e.g., bytes, or card images, or printer lines.
   function IO_elapsed_time (the_buffer   : IOC.device;
                             atomic_items : KDF9.word)
   return KDF9.us;

   -- The total elapsed time taken, so far, by transfers on the attached device.
   function IO_elapsed_time_total (the_buffer : IOC.device)
   return KDF9.us;


   --
   -- The CLOQq, SLOQq and TLOQq operations do NOT address a buffer,
   --    and so are fully implemented elsewhere.
   --

   --
   -- The INTQq, BUSYQq, PARQq and MANUALQq/CTQq operations DO address a buffer,
   --    but do NOT initiate an I/O transfer, and are common to all devices,
   --       so they operate on a class-wide parameter.
   --

   procedure INT (the_buffer  : in out IOC.device'Class;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure BUSY (the_buffer  : in out IOC.device'Class;
                   Q_operand   : in KDF9.Q_register;
                   set_offline : in Boolean;
                   result      : out Boolean);

   procedure PAR (the_buffer   : in out IOC.device'Class;
                   Q_operand   : in KDF9.Q_register;
                   set_offline : in Boolean;
                   result      : out Boolean);

   procedure MANUAL_CT (the_buffer  : in out IOC.device'Class;
                        Q_operand   : in KDF9.Q_register;
                        set_offline : in Boolean);

   -- These KDF9 data-transfer operations must be overridden for non-trivial functionality.
   -- Invoking any of them raises a LIV exception. This exactly mirrors the action of the
   --    KDF9 in causing a LIV interrupt when an invalid operation was applied to a device.
   -- A device without some of these operations inherits them from this list and so
   --    implements correctly the original semantics of the KDF9.

   --
   -- The PI* are input operations.
   --

   procedure PIA (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIB (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIC (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PID (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIE (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIF (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIG (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PIH (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   --
   -- The PM* are device-status operations.
   --

   procedure PMA (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PMB (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PMC (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PMD (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PME (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PMF (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   -- This is the mysterious "read C store" order.
   procedure PMG (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

-- procedure PMH is implemented by SLO

   procedure PMK (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure PML (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   --
   -- The PO* are output operations.
   --

   procedure POA (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POB (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POC (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POD (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POE (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POF (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POG (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POH (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POK (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);

   procedure POL (the_buffer  : in out IOC.device;
                  Q_operand   : in KDF9.Q_register;
                  set_offline : in Boolean);


--
--
   -- The buffer_configuration type enables the dynamic setting-up of a complement of I/O devices.
--
--

   type device_class_access  is access all IOC.device'Class;

   type buffer_configuration is array (KDF9.buffer_number) of IOC.device_class_access;

   -- These are the I/O devices installed in this configuration.
   -- Each device installs itself into the configuration when the device is initialized.

   buffer : buffer_configuration;

--
   -- These operations are used by Directors to manage device allocation to problem programs.
--

   procedure set_state_of (the_buffer : in device_class_access;
                           allocated  : in Boolean);

   function is_allocated (the_buffer : device_class_access)  -- N.B. IS_allocated.
   return Boolean;

   function is_unallocated (the_buffer : device_class_access)  -- N.B. is_UNallocated.
   return Boolean;

--
   -- These buffer-implementation operations are used outside IOC and apply to all device types.
--

   -- Complete all extant transfers, then Finalize each buffer.
   procedure finalize_all_KDF9_buffers;

   -- Advance the elapsed time to a point after all extant transfer have terminated.
   procedure complete_all_extant_transfers;

   -- Complete any terminated transfer operations and take any needed interrupts.
   procedure act_on_pending_interrupts;

   -- Handle non-data transfer operations on busy device.
   procedure deal_with_a_busy_device (the_buffer  : in out IOC.device'Class;
                                      order_time  : in KDF9.us;
                                      set_offline : in Boolean);

   -- A LOV interupt caused by an attempted store access must arrange
   --    for the interrupted instruction to be resumed.
   -- In boot mode, effect the LOV interrupt to Director.
   -- In other modes, advance the elapsed time to the end-of-transfer time
   --    for the_locked_out_address, then act on pending interrupts.
   procedure handle_a_main_store_lockout;

   type transfer_kind  is (input_operation,
                           output_operation,
                           control_operation,
                           some_other_operation);

   -- Take note of the start of a transfer.
   -- For I/O operations that do not entail an actual data transfer,
   --    such as testing a buffer for a graph plotter,
   --    set the busy time to the order's MC execution time.
   -- This keeps elapsed time in sync with CPU time,
   --    and ensures that the operation waits for any preceding transfer
   --    on the same buffer to complete before the test is actioned.
   procedure start_data_transfer (the_buffer  : in out IOC.device'Class;
                                  Q_operand   : in KDF9.Q_register;
                                  set_offline : in Boolean;
                                  busy_time   : in KDF9.us;
                                  operation   : in IOC.transfer_kind := IOC.some_other_operation);

private

   use Ada.Exceptions; pragma Warnings(Off, Ada.Exceptions);
   --
   use exceptions;     pragma Warnings(Off, exceptions);
   use formatting;     pragma Warnings(Off, formatting);
   use HCI;            pragma Warnings(Off, HCI);
   use host_IO;        pragma Warnings(Off, host_IO);
   use KDF9_char_sets; pragma Warnings(Off, KDF9_char_sets);
   use KDF9.store;     pragma Warnings(Off, KDF9.store);
   use settings;       pragma Warnings(Off, settings);

   use POSIX;          -- Used here, so no need to suppress warnings.

   type device (number : IOC.device_number; unit : IOC.unit_number)
   is abstract new Limited_Controlled with
      record
         is_abnormal,
         is_busy,
         is_offline,
         is_allocated,
         is_for_Director : Boolean := False;
         operation       : IOC.transfer_kind := IOC.some_other_operation;
         initiation_time : KDF9.us := KDF9.us'Last;
         transfer_time   : KDF9.us := KDF9.us'Last;
         completion_time : KDF9.us := KDF9.us'Last;
         priority_level  : KDF9.priority;
         control_word    : KDF9.Q_register;
         decoded_order   : KDF9.decoded_order;
         device_name     : IOC.device_name;
         order_address   : KDF9.syllable_address;
         order_count     : KDF9.order_counter;
         stream          : host_IO.stream;
      end record;

   overriding
   procedure Initialize (the_buffer : in out IOC.device);

   procedure open (the_buffer : in out IOC.device'Class;
                   the_mode   : in POSIX.access_mode);

   overriding
   procedure Finalize (the_buffer : in out IOC.device)
      with Inline => False;

   -- Operations, used only within the IOC hierarchy, that apply to all device types.

   -- Check that the buffer for the_device is unused, then set it to the_device.
   procedure install (the_device : in out IOC.device'Class);

   -- LIV if the_buffer is in the abnormal state.
   procedure validate_parity (the_buffer : in IOC.device'Class)
      with Inline => False;

   -- Check that the_buffer is online, and that access to it is permitted; LIV if not.
   procedure validate_device (the_buffer : in IOC.device'Class)
      with Inline => False;

   -- Check that the device and the transfer address bounds are valid;
   --    LIV if not.
   procedure validate_transfer (the_buffer : in IOC.device'Class;
                                Q_operand  : in KDF9.Q_register);

   -- When the real duration of a variable-length transfer is known,
   --    its completion time can be made accurate by giving its actual_time.
   -- correct_transfer_time must be called before finalize_transfer is called.
   procedure correct_transfer_time (the_buffer  : in out IOC.device'Class;
                                    actual_time : in KDF9.us);

   procedure correct_transfer_time (the_buffer    : in out IOC.device'Class;
                                    actual_length : in KDF9.word);

   -- LIV if the repetition count is negative.
   procedure require_nonnegative_count (count : in KDF9.Q_part);

   -- LIV if the repetition count is negative or zero.
   procedure require_positive_count (count : in KDF9.Q_part);

   -- Account for the CPU time taken by the buffer in setting store lockouts.
   procedure add_in_the_IO_lockout_CPU_time (Q_operand : in KDF9.Q_register);

   -- These are handy to have in the child packages.
   NUL : constant Character := Character'Val(0);
   BEL : constant Character := Character'Val(7);
   HT  : constant Character := Character'Val(9);
   LF  : constant Character := Character'Val (10);
   FF  : constant Character := Character'Val (12);
   ESC : constant Character := Character'Val (27);
   SP  : constant Character := ' ';
   DEL : constant Character := Character'Val (127);

end IOC;
