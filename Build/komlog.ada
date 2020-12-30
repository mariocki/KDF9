Version V5p1a of ee9 for macOS, built on Thu 12 Nov 2020 18:12:11 GMT.
mk9 'ee9' build: optimised and with full language checks, using configuration options:

pragma Unsuppress(All_Checks);
pragma Optimize_Alignment(Time);
pragma Assertion_Policy(Ignore);
pragma Validity_Checks(Off);
pragma Check_Policy(Debug, Off);
pragma Restrictions(Max_Asynchronous_Select_Nesting => 0);
pragma Restrictions(Max_Tasks => 0);
pragma Restrictions(No_Abort_Statements);
pragma Restrictions(No_Implementation_Attributes);
pragma Restrictions(No_Obsolescent_Features);

Using the build command:
gnatmake -aI../Source -aO../Build -funwind-tables -gnatl12j96 -gnatw.e -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.W -gnatw.B -gnatwC -gnatw.u -gnatyO -gnatw.Y -gnatw.N -fdata-sections -ffunction-sections -gnatf -mtune=native -O3 -flto -j0 ee9 -bargs -static -Sin -largs -Wl,-dead_strip -Wl,-dead_strip -largs -flto

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.
GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.




GNAT Community 2020 (20200429-84)
GNAT Community 2020 (20200429-84)
GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.
Copyright 1992-2020, Free Software Foundation, Inc.
Copyright 1992-2020, Free Software Foundation, Inc.
GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/generic_logger.adb
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- generic_logger.adb
     2. --
     3. -- Provide operations supporting replicated output
     4. --    output to a list of logging interfaces.
     5. --
     6. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     7. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     8. --
     9. -- The ee9 program is free software; you can redistribute it and/or
    10. -- modify it under terms of the GNU General Public License as published
    11. -- by the Free Software Foundation; either version 3, or (at your option)
    12. -- any later version. This program is distributed in the hope that it
    13. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    14. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    15. -- See the GNU General Public License for more details. You should have
    16. -- received a copy of the GNU General Public License distributed with
    17. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    18. --
    19.
    20. -- generic
    21. --    max_logger_list_size : in Positive;
    22. package body generic_logger is
    23.
    24.    not overriding
    25.    procedure set_logger_list (logger : in out replicator; list : in distribution_list) is
    26.    begin
    27.       logger.data := (list'Length, list);
    28.    end set_logger_list;
    29.
    30.    overriding
    31.    procedure tab_log (logger   : in out replicator;
    32.                       at_least : in Natural;
    33.                       spacing  : in Positive;
    34.                       iff      : in Boolean := True) is
    35.    begin
    36.       for l in logger.data.list'Range loop
    37.          logger.data.list(l).tab_log(at_least, spacing, iff);
    38.       end loop;
    39.    end tab_log;
    40.
    41.    overriding
    42.    procedure tab_log_to (logger : in out replicator;
    43.                          column : in Positive;
    44.                          iff    : in Boolean := True) is
    45.    begin
    46.       for l in logger.data.list'Range loop
    47.          logger.data.list(l).tab_log_to(column, iff);
    48.       end loop;
    49.    end tab_log_to;
    50.
    51.    overriding
    52.    procedure log (logger : in out replicator;
    53.                   char   : in Character;
    54.                   iff    : in Boolean := True) is
    55.    begin
    56.       for l in logger.data.list'Range loop
    57.          logger.data.list(l).log(char, iff);
    58.       end loop;
    59.    end log;
    60.
    61.    overriding
    62.    procedure log (logger : in out replicator;
    63.                   text   : in String;
    64.                   iff    : in Boolean := True) is
    65.    begin
    66.       for l in logger.data.list'Range loop
    67.          logger.data.list(l).log(text, iff);
    68.       end loop;
    69.    end log;
    70.
    71.    overriding
    72.    procedure log_new_line (logger : in out replicator;
    73.                            iff    : in Boolean := True) is
    74.    begin
    75.       for l in logger.data.list'Range loop
    76.          logger.data.list(l).log_new_line(iff);
    77.       end loop;
    78.    end log_new_line;
    79.
    80.    overriding
    81.    procedure open (logger : in out replicator; log_name : in String) is
    82.    begin
    83.       for l in logger.data.list'Range loop
    84.          logger.data.list(l).open(log_name);
    85.       end loop;
    86.    end open;
    87.
    88.    overriding
    89.    procedure close (logger : in out replicator; log_name : in String) is
    90.    begin
    91.       for l in logger.data.list'Range loop
    92.          logger.data.list(l).close(log_name);
    93.       end loop;
    94.    end close;
    95.
    96.    overriding
    97.    procedure flush (logger : in out replicator; iff : in Boolean := True) is
    98.    begin
    99.       for l in logger.data.list'Range loop
   100.          logger.data.list(l).flush(iff);
   101.       end loop;
   102.    end flush;
   103.
   104. end generic_logger;

Compiling: ../Source/generic_logger.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- generic_logger.ads
     2. --
     3. -- Provide operations supporting replicated output to a list of logging interfaces.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with logging;
    20.
    21. generic
    22.    max_logger_list_size : in Positive;
    23. package generic_logger is
    24.
    25.    type distribution_list is array (Positive range <>) of access logging.output'Class;
    26.
    27.    type replicator is new logging.output with private;
    28.
    29.    not overriding
    30.    procedure set_logger_list (logger : in out replicator; list : in distribution_list);
    31.
    32.    overriding
    33.    procedure tab_log (logger   : in out replicator;
    34.                       at_least : in Natural;
    35.                       spacing  : in Positive;
    36.                       iff      : in Boolean := True);
    37.
    38.    overriding
    39.    procedure tab_log_to (logger : in out replicator;
    40.                          column : in Positive;
    41.                          iff    : in Boolean := True);
    42.
    43.    overriding
    44.    procedure log (logger : in out replicator;
    45.                   char   : in Character;
    46.                   iff    : in Boolean := True);
    47.
    48.    overriding
    49.    procedure log (logger : in out replicator;
    50.                   text   : in String;
    51.                   iff    : in Boolean := True);
    52.
    53.    overriding
    54.    procedure log_new_line (logger : in out replicator;
    55.                            iff    : in Boolean := True);
    56.
    57.    overriding
    58.    procedure open  (logger : in out replicator; log_name : in String);
    59.
    60.    overriding
    61.    procedure close (logger : in out replicator; log_name : in String);
    62.
    63.    overriding
    64.    procedure flush (logger : in out replicator; iff : in Boolean := True);
    65.
    66. private
    67.
    68.    subtype logger_list_size is Natural range 0 .. max_logger_list_size;
    69.
    70.    -- This type is needed because tagged types cannot have discriminants.
    71.    type replica_list (length : logger_list_size := 0) is
    72.       record
    73.          list : distribution_list(1 .. length);
    74.       end record;
    75.
    76.    type replicator is new logging.output with
    77.       record
    78.          data : replica_list;
    79.       end record;
    80.
    81. end generic_logger;

 104 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/logging.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- logging.ads
     2. --
     3. -- Define an abstract log output device.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package logging is
    20.
    21.    type output is interface;
    22.
    23.    procedure tab_log (logger   : in out logging.output;
    24.                       at_least : in Natural;
    25.                       spacing  : in Positive;
    26.                       iff      : in Boolean := True) is abstract;
    27.
    28.    procedure tab_log_to (logger : in out logging.output;
    29.                          column : in Positive;
    30.                          iff    : in Boolean := True) is abstract;
    31.
    32.    procedure log (logger : in out logging.output;
    33.                   char   : in Character;
    34.                   iff    : in Boolean := True) is abstract;
    35.
    36.    procedure log (logger : in out logging.output;
    37.                   text   : in String;
    38.                   iff    : in Boolean := True) is abstract;
    39.
    40.    procedure log_new_line (logger : in out logging.output;
    41.                            iff    : in Boolean := True) is abstract;
    42.
    43.    procedure open  (logger : in out logging.output; log_name : in String) is abstract;
    44.
    45.    procedure close (logger : in out logging.output; log_name : in String) is abstract;
    46.
    47.    procedure flush (logger : in out logging.output; iff : in Boolean := True) is abstract;
    48.
    49. end logging;

 49 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/dumping.adb
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- dumping.adb
     2. --
     3. -- Provide support for diagnostic core-dumping area descriptions.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with formatting;
    20. with state_display;
    21.
    22. use  formatting;
    23. use  state_display;
    24.
    25. package body dumping is
    26.
    27.    type poke_list_entry is
    28.       record
    29.          address  : KDF9.address;
    30.          sub_word : Character;
    31.          position : KDF9.address;
    32.          value    : KDF9.word;
    33.       end record;
    34.
    35.    length_of_poke_list : constant := 100;
    36.    poke_list_count     : Natural range 0 .. length_of_poke_list := 0;
    37.    poke_list           : array (Positive range 1 .. length_of_poke_list) of poke_list_entry;
    38.
    39.    use dumping.flag_support;
    40.
    41.    function dumping_flag (c : Character)
    42.    return dumping.flag
    43.    is (dumping.flag(to_upper(c)));
    44.
    45.    type area is
    46.       record
    47.          format_set  : dumping.format_set := no_dumping_flag;
    48.          first, last : KDF9.address;
    49.       end record;
    50.
    51.    no_dumping_area : constant dumping.area := (no_dumping_flag, 0, 0);
    52.
    53.    dumping_areas : array (dumping.area_number) of dumping.area := (others => no_dumping_area);
    54.
    55.    pre_dumping_area_count  : area_count := 0;
    56.    post_dumping_area_count : area_count := 0;
    57.
    58.    function nr_of_pre_dumping_areas
    59.    return dumping.area_count
    60.    is (pre_dumping_area_count);
    61.
    62.    function nr_of_post_dumping_areas
    63.    return dumping.area_count
    64.    is (post_dumping_area_count);
    65.
    66.    procedure request_a_dumping_area (format_set  : in dumping.format_set;
    67.                                      first, last : in KDF9.address;
    68.                                      was_stored  : out Boolean) is
    69.    begin
    70.       was_stored := False;
    71.       if pre_dumping_area_count+post_dumping_area_count = nr_of_dumping_areas then
    72.          return;
    73.       end if;
    74.       for d of dumping_areas loop
    75.          if d = (format_set, first, last) then
    76.             was_stored := True;
    77.             return;
    78.          end if;
    79.       end loop;
    80.       if format_set/expunge_flag then
    81.          remove_specified_areas(format_set - expunge_flag, first, last);
    82.       end if;
    83.       for d of dumping_areas loop
    84.          if d.format_set = no_dumping_flag then
    85.             d := (format_set, first, last);
    86.             was_stored := True;
    87.             if initial_flag/format_set then
    88.                pre_dumping_area_count := pre_dumping_area_count + 1;
    89.             end if;
    90.             if final_flag/format_set then
    91.                post_dumping_area_count := post_dumping_area_count + 1;
    92.             end if;
    93.             return;
    94.          end if;
    95.       end loop;
    96.    end request_a_dumping_area;
    97.
    98.    max_types : constant Positive := abs is_dumping_flag - 1; -- P XOR Q
    99.
   100.    function format_image (format_set : dumping.format_set)
   101.    return String is
   102.       image_set  : dumping.format_set := format_set;
   103.       result     : String(1 .. max_types) := (others => ' ');
   104.       p          : Positive range 2 .. max_types := 2;
   105.    begin
   106.       if image_set = no_dumping_flag then
   107.          return result;
   108.       elsif image_set/initial_flag then
   109.           image_set := image_set - initial_flag;
   110.           result(1) := Character(initial_flag);
   111.       else
   112.           image_set := image_set - final_flag;
   113.           result(1) := Character(final_flag);
   114.       end if;
   115.       for f in dumping.flag loop
   116.          if image_set/f then
   117.             result(p) := Character(f);
   118.             p := p + 1;
   119.          end if;
   120.       end loop;
   121.       return trimmed(result);
   122.    end format_image;
   123.
   124.    function area_image (d : dumping.area_number)
   125.    return String is
   126.       first       : constant KDF9.address := dumping_areas(d).first;
   127.       last        : constant KDF9.address := dumping_areas(d).last;
   128.       format_set  : constant dumping.format_set := dumping_areas(d).format_set;
   129.       result      : String(1 .. max_types+2*(7)) := (others => ' ');
   130.    begin
   131.       if pre_dumping_area_count+post_dumping_area_count = 0 then
   132.          return no_specification;
   133.       end if;
   134.       result(1 .. max_types)             := format_image(format_set);
   135.       result(max_types+2 .. max_types+7) := oct_of(first);
   136.       result(max_types+9 .. result'Last) := oct_of(last);
   137.       return result;
   138.    end area_image;
   139.
   140.    procedure remove_specified_areas (format_set  : in dumping.format_set;
   141.                                      first, last : in KDF9.address) is
   142.    begin
   143.       if pre_dumping_area_count+post_dumping_area_count = 0 then
   144.          return;
   145.       end if;
   146.       for d of dumping_areas loop
   147.          if d.first >= first and d.last <= last then
   148.             d.format_set := d.format_set - format_set;
   149.             if d.format_set-initial_flag-final_flag = no_dumping_flag then
   150.                d := no_dumping_area;
   151.             end if;
   152.             if initial_flag/d.format_set then
   153.                pre_dumping_area_count := Integer'Max(pre_dumping_area_count - 1, 0);
   154.             end if;
   155.             if final_flag/d.format_set then
   156.                post_dumping_area_count := Integer'Max(post_dumping_area_count - 1, 0);
   157.             end if;
   158.          end if;
   159.       end loop;
   160.    end remove_specified_areas;
   161.
   162.    procedure print_formatted_area (d : in dumping.area) is
   163.       format_set  : constant dumping.format_set := d.format_set;
   164.       first       : constant KDF9.address := d.first;
   165.       last        : constant KDF9.address := d.last;
   166.    begin
   167.       if format_set/tape_code_flag then
   168.          show_core_in_tape_code(first, last);
   169.       end if;
   170.       if format_set/normal_flag then
   171.          show_core_in_case_normal(first, last);
   172.       end if;
   173.       if format_set/shift_flag then
   174.          show_core_in_case_shift(first, last);
   175.       end if;
   176.       if format_set/ card_code_flag then
   177.          show_core_in_card_code(first, last);
   178.       end if;
   179.       if format_set/printer_flag then
   180.          show_core_in_print_code(first, last);
   181.       end if;
   182.       if format_set/ASCII_flag then
   183.          show_core_in_Latin_1(first, last);
   184.       end if;
   185.       if format_set/word_flag then
   186.          show_core_as_word_forms(first, last);
   187.       end if;
   188.       if format_set/Usercode_flag then
   189.          show_core_as_Usercode((KDF9.order_word_number(first), 0),
   190.                                (KDF9.order_word_number(last), 0),
   191.                                 octal_option => not format_set/decimal_flag);
   192.       end if;
   193.       if format_set/orders_flag then
   194.          show_core_as_syllables((KDF9.order_word_number(first), 0),
   195.                                 (KDF9.order_word_number( last), 0));
   196.       end if;
   197.    end print_formatted_area;
   198.
   199.    procedure print_dump_areas (flag : in dumping.flag; count : in dumping.area_count) is
   200.       Usercode_wanted : Boolean := False;
   201.    begin
   202.       if count = 0 then
   203.          return;
   204.       end if;
   205.       for d of dumping_areas loop
   206.          Usercode_wanted := Usercode_wanted or d.format_set/Usercode_flag;
   207.       end loop;
   208.       if Usercode_wanted then
   209.          mark_all_code_blocks_and_data_blocks;
   210.       end if;
   211.       for d of dumping_areas loop
   212.          if d.format_set/flag then
   213.             print_formatted_area(d);
   214.          end if;
   215.       end loop;
   216.    end print_dump_areas;
   217.
   218.    procedure print_prerun_dump_areas is
   219.    begin
   220.       print_dump_areas(initial_flag, pre_dumping_area_count);
   221.    end print_prerun_dump_areas;
   222.
   223.    procedure print_postrun_dump_areas is
   224.    begin
   225.       print_dump_areas(final_flag, post_dumping_area_count);
   226.    end print_postrun_dump_areas;
   227.
   228.    procedure remove_dump_areas (flag : in dumping.flag; count : in out dumping.area_count) is
   229.    begin
   230.       if count = 0 then
   231.          return;
   232.       end if;
   233.       for d of dumping_areas loop
   234.          if d.format_set/flag then
   235.             d := (no_dumping_flag, 0, 0);
   236.          end if;
   237.       end loop;
   238.       count := 0;
   239.    end remove_dump_areas;
   240.
   241.    procedure remove_prerun_dump_areas is
   242.    begin
   243.       remove_dump_areas(initial_flag, pre_dumping_area_count);
   244.    end remove_prerun_dump_areas;
   245.
   246.    procedure remove_postrun_dump_areas is
   247.    begin
   248.       remove_dump_areas(final_flag, post_dumping_area_count);
   249.    end remove_postrun_dump_areas;
   250.
   251.    procedure add_to_poke_list (address    : in KDF9.address;
   252.                                sub_word   : in Character;
   253.                                position   : in KDF9.address;
   254.                                value      : in KDF9.word;
   255.                                was_stored : out Boolean) is
   256.    begin
   257.       if poke_list_count < length_of_poke_list then
   258.          poke_list_count := poke_list_count + 1;
   259.          poke_list(poke_list_count) := (address, sub_word, position, value);
   260.          was_stored := True;
   261.       else
   262.          was_stored := False;
   263.       end if;
   264.    end add_to_poke_list;
   265.
   266.    procedure poke_all_amendments is
   267.    begin
   268.       for p in 1..poke_list_count loop
   269.          poke(poke_list(p).address, poke_list(p).sub_word, poke_list(p).position, poke_list(p).value);
   270.       end loop;
   271.       poke_list_count := 0;
   272.    end poke_all_amendments;
   273.
   274. end dumping;

Compiling: ../Source/dumping.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- dumping.ads
     2. --
     3. -- Provide support for diagnostic core-dumping area descriptions.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with generic_sets;
    20. with KDF9;
    21.
    22. use  KDF9;
    23.
    24. package dumping is
    25.
    26.    type flag is new Character range '@' .. 'Z';
    27.
    28.    no_flag        : constant dumping.flag := '@';
    29.    ASCII_flag     : constant dumping.flag := 'A';
    30.    card_code_flag : constant dumping.flag := 'C';
    31.    decimal_flag   : constant dumping.flag := 'D';
    32.    single_flag    : constant dumping.flag := 'E';
    33.    final_flag     : constant dumping.flag := 'F';
    34.    half_flag      : constant dumping.flag := 'H';
    35.    initial_flag   : constant dumping.flag := 'I';
    36.    normal_flag    : constant dumping.flag := 'N';
    37.    orders_flag    : constant dumping.flag := 'O';
    38.    printer_flag   : constant dumping.flag := 'L';
    39.    shift_flag     : constant dumping.flag := 'S';
    40.    tape_code_flag : constant dumping.flag := 'T';
    41.    Usercode_flag  : constant dumping.flag := 'U';
    42.    word_flag      : constant dumping.flag := 'W';
    43.    expunge_flag   : constant dumping.flag := 'X';
    44.
    45.    function dumping_flag (c : Character)
    46.    return dumping.flag;
    47.
    48.    package flag_support is new generic_sets(member => dumping.flag);
    49.
    50.    subtype format_set is flag_support.set;
    51.    use type format_set;
    52.
    53.    is_parameter_flag : constant dumping.format_set
    54.                      := (  decimal_flag
    55.                          | single_flag
    56.                          | half_flag
    57.                          | ASCII_flag
    58.                          | orders_flag
    59.                          | printer_flag
    60.                          | tape_code_flag
    61.                          | Usercode_flag
    62.                          | card_code_flag
    63.                          | normal_flag
    64.                          | shift_flag
    65.                          | word_flag     => True,
    66.                            others        => False
    67.                         );
    68.
    69.
    70.    is_epoch_flag : constant dumping.format_set
    71.                  := (  initial_flag
    72.                      | final_flag => True,
    73.                        others     => False
    74.                     );
    75.
    76.    is_dumping_flag  : constant dumping.format_set
    77.                     := is_parameter_flag or is_epoch_flag;
    78.
    79.    no_dumping_flag  : constant dumping.format_set
    80.                     := flag_support.empty_set;
    81.
    82.    nr_of_dumping_areas : constant := 100;
    83.    subtype area_count  is Natural  range 0 .. nr_of_dumping_areas;
    84.    subtype area_number is Positive range 1 .. nr_of_dumping_areas;
    85.
    86.    procedure request_a_dumping_area (format_set  : in dumping.format_set;
    87.                                      first, last : in KDF9.address;
    88.                                      was_stored  : out Boolean);
    89.
    90.    procedure remove_specified_areas (format_set  : in dumping.format_set;
    91.                                      first, last : in KDF9.address);
    92.
    93.    procedure print_prerun_dump_areas;
    94.
    95.    procedure remove_prerun_dump_areas;
    96.
    97.    procedure print_postrun_dump_areas;
    98.
    99.    procedure remove_postrun_dump_areas;
   100.
   101.    function nr_of_pre_dumping_areas
   102.    return dumping.area_count;
   103.
   104.    function nr_of_post_dumping_areas
   105.    return dumping.area_count;
   106.
   107.    no_specification : constant String := "";
   108.
   109.    -- area_image returns no_specification if area(d) is undefined or empty.
   110.    function area_image (d : dumping.area_number)
   111.    return String;
   112.
   113.    -- format_image returns blanks if format_set is empty.
   114.    function format_image (format_set : dumping.format_set)
   115.    return String;
   116.
   117.    -- poke support is in dumping because it is needed at the same time during initialization.
   118.    -- was_stored := (the requested poke could not be saved); because the list is full.
   119.    procedure add_to_poke_list (address    : in KDF9.address;
   120.                                sub_word   : in Character;
   121.                                position   : in KDF9.address;
   122.                                value      : in KDF9.word;
   123.                                was_stored : out Boolean);
   124.
   125.    -- poke_all_amendments effects all stored pokes and then clears the poke list for reuse.
   126.    procedure poke_all_amendments;
   127.
   128. end dumping;

 274 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/formatting.adb
Source file time stamp: 2020-09-06 22:42:50
Compiled at: 2020-11-12 18:12:11

     1. -- formatting.adb
     2. --
     3. -- Provide basic data-formatting operations for KDF9 data types.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Characters.Handling;
    20. with Ada.Strings;
    21. with Ada.Strings.Fixed;
    22. --
    23. with KDF9_char_sets;
    24. with KDF9.CPU;
    25.
    26. use  Ada.Characters.Handling;
    27. use  Ada.Strings;
    28. use  Ada.Strings.Fixed;
    29. --
    30. use  KDF9_char_sets;
    31. use  KDF9.CPU;
    32.
    33. package body formatting is
    34.
    35.    digit_map : constant array (KDF9.halfword range 0 .. 15) of Character := "0123456789ABCDEF";
    36.
    37.    -- Return N as 3 octal digits.
    38.    function oct_of (N : KDF9.syllable)
    39.    return String
    40.    is (oct_of(KDF9.halfword(N))(6 .. 8));
    41.
    42.    -- Return N as 6 octal digits.
    43.    function oct_of (N : KDF9.field_of_16_bits)
    44.    return String is
    45.       value : KDF9.field_of_16_bits := N;
    46.       oct   : String(1 .. 6);
    47.    begin
    48.       for i in reverse oct'Range loop
    49.          oct(i) := digit_map(KDF9.halfword(value mod 8));
    50.          value := value / 8;
    51.       end loop;
    52.       return oct;
    53.    end oct_of;
    54.
    55.    -- Return N as 1 .. min_digits octal digits, with (partial) zero suppression.
    56.    function oct_of (N : KDF9.Q_part; min_digits : octal_width := 6)
    57.    return String is
    58.       oct : constant String(octal_width) := oct_of(KDF9.field_of_16_bits(N));
    59.    begin
    60.      if N = 0 then return (1..min_digits => '0'); end if;
    61.      for i in 1 .. 6-min_digits loop
    62.         if oct(i) /= '0' then
    63.            return oct(i .. 6);
    64.         end if;
    65.       end loop;
    66.       return oct(7-min_digits .. 6);
    67.    end oct_of;
    68.
    69.    -- Return N as 1 .. 5 decimal digits, with zero suppression and sign when neagtive.
    70.    function signed_dec_of (N : KDF9.Q_part)
    71.    return String is
    72.       dec : constant String := resign(N)'Image;
    73.    begin
    74.       return trimmed(dec);
    75.    end signed_dec_of;
    76.
    77.    -- Return N as decimal digits, with zero suppression.
    78.    function dec_of (N : KDF9.Q_part)
    79.    return String is
    80.       dec : constant String := N'Image;
    81.    begin
    82.       return trimmed(dec);
    83.    end dec_of;
    84.
    85.    -- Return N as up to 5 octal digits.
    86.    function oct_of (N : KDF9.order_word_number)
    87.    return String is
    88.       value : KDF9.order_word_number := N;
    89.       j     : Positive := 5;
    90.       oct   : String(1 .. 5);
    91.    begin
    92.       for i in reverse oct'Range loop
    93.          oct(i) := digit_map(KDF9.halfword(value mod 8));
    94.          value := value / 8;
    95.       end loop;
    96.       for i in oct'Range loop
    97.          if oct(i) /= '0' then j := i; exit; end if;
    98.       end loop;
    99.       return oct(j..5);
   100.    end oct_of;
   101.
   102.    -- Return N as decimal digits, with zero suppression.
   103.    function dec_of (N : KDF9.order_word_number)
   104.    return String
   105.    is (trimmed(N'Image));
   106.
   107.    -- Return N as 8 octal digits.
   108.    function oct_of (N : KDF9.halfword)
   109.    return String is
   110.       value : KDF9.halfword := N;
   111.       oct   : String(1 .. 8);
   112.    begin
   113.       for i in reverse oct'Range loop
   114.          oct(i) := digit_map(value mod 8);
   115.          value := value / 8;
   116.       end loop;
   117.       return oct;
   118.    end oct_of;
   119.
   120.    -- Return N as #wwwww/s, where w and s are octal digits.
   121.    function oct_of (N : KDF9.sjns_link)
   122.    return String
   123.    is (
   124.        "#"
   125.       &  oct_of(N.order_word_number)
   126.       & '/'
   127.       & digit_map(KDF9.halfword(N.syllable_index))
   128.       );
   129.
   130.    -- Return N as #wwwww/s, where w and s are octal digits.
   131.    function oct_of (N : KDF9.syllable_address)
   132.    return String
   133.    is (oct_of(KDF9.sjns_link(N)));
   134.
   135.    -- Return N as dddd/d, where d is a decimal digit.
   136.    function dec_of (N : KDF9.syllable_address)
   137.    return String
   138.    is (
   139.        trimmed(N.order_word_number'Image)
   140.             & '/'
   141.             & digit_map(KDF9.halfword(N.syllable_index))
   142.       );
   143.
   144.    -- Return N as #wwwww/s, where w and s are octal digits;
   145.    --    or as dddd/s, where d is a decimal digit, according to octal_option.
   146.    function oct_or_dec_of (N : KDF9.syllable_address; octal_option : Boolean)
   147.    return String
   148.    is (if octal_option then oct_of(N) else dec_of(N));
   149.
   150.    -- Return N as 16 octal digits
   151.    function oct_of (N : KDF9.word)
   152.    return String is
   153.       value : KDF9.word := N;
   154.       oct   : String(1 .. 16);
   155.    begin
   156.       for i in reverse oct'Range loop
   157.          oct(i) := digit_map(KDF9.halfword(value mod 8));
   158.          value := value / 8;
   159.       end loop;
   160.       return oct;
   161.    end oct_of;
   162.
   163.    function as_DR_command (Q_operand : KDF9.Q_register)
   164.    return String is
   165.       sector : constant KDF9.Q_part := Q_operand.C/64;
   166.       drive  : constant KDF9.Q_part := Q_operand.C/16 mod 4;
   167.    begin
   168.       -- The drum geometry and I/O command bits are as defined in the DR package.
   169.       return "SECT"
   170.            & (if sector < 10 then "00" elsif sector < 100 then "0" else "")
   171.            & dec_of(sector)
   172.            & "D"
   173.            & dec_of(drive);
   174.    end as_DR_command;
   175.
   176.    function as_FD_command (Q_operand : KDF9.Q_register; for_seek, for_FH : Boolean := False)
   177.    return String is
   178.       parameter : constant KDF9.Q_part := Q_operand.C/16;
   179.       seek_bits : constant := 6;
   180.       cylinder  : constant KDF9.Q_part := parameter mod 2**seek_bits;
   181.       disk_bits : constant := 4;
   182.       platter   : constant KDF9.Q_part := parameter  /  2**seek_bits mod 2**disk_bits;
   183.       drive     : constant KDF9.Q_part := parameter  /  2**seek_bits  /  2**disk_bits;
   184.    begin
   185.       -- The disc geometry and I/O command bits are as defined in the FD package.
   186.       if for_seek then
   187.          return "D" & dec_of(drive)
   188.               & "P" & dec_of(if for_FH then KDF9.Q_part'(16) else platter)
   189.               & "C" & dec_of(cylinder);
   190.       else -- for data transfer, parameter is sector #, with maximum 96 sectors per track.
   191.          return "SECT" & (if parameter < 10 then "0" else "") & dec_of(parameter);
   192.       end if;
   193.    end as_FD_command;
   194.
   195.    -- Return "L', R'", or "L'" if R' is empty; "'" indicates removal of trailing blanks.
   196.    function "-" (L, R : String)
   197.    return String is
   198.       trim_R : constant String := trim(R, right);
   199.    begin
   200.       if trim_R /= "" then
   201.          return trim(L, right) & ", " & trim_R;
   202.       else
   203.          return trim(L, right);
   204.       end if;
   205.    end "-";
   206.
   207.    -- Return S with all leading an trailing blanks removed.
   208.    function trimmed (S : String)
   209.    return String
   210.    is (Trim(S, Ada.Strings.Both));
   211.
   212.    -- Return trimmed(S), right-just_right in a field of width at least W.
   213.    function just_right (S : String; W : Positive := 3)
   214.    return String is
   215.      image   : constant String   := Trim(S, Ada.Strings.Both);
   216.      columns : constant Positive := Positive'Max(W, image'Length);
   217.    begin
   218.      return Ada.Strings.Fixed.Tail(image, columns, ' ');
   219.    end just_right;
   220.
   221.    -- Return trimmed(S), left-justified in a field of width at least W.
   222.    function just_left (S : String; W : Positive := 3)
   223.    return String is
   224.      image   : constant String   := Trim(S, Ada.Strings.Both);
   225.      columns : constant Positive := Positive'Max(W, image'Length);
   226.    begin
   227.      return Ada.Strings.Fixed.Head(image, columns, ' ');
   228.    end just_left;
   229.
   230.    function plurality (count : KDF9.word; for_1 : String := ""; for_more : String := "s")
   231.    return String
   232.    is (if count /= 1 then for_more else for_1);
   233.
   234.    -- Return C converted to a 1-character string.
   235.    function "+" (C : Character)
   236.    return unit_string
   237.    is ((1 => C));
   238.
   239.    -- Return C with all Latin-1 lower-case letters converted to upper-case.
   240.    function to_upper (C : Character)
   241.    return Character
   242.    renames Ada.Characters.Handling.to_upper;
   243.
   244.    -- Return S with all Latin-1 lower-case letters converted to upper-case.
   245.    function to_upper (S : String)
   246.    return String
   247.    renames Ada.Characters.Handling.to_upper;
   248.
   249.    -- Return C with all Latin-1 upper-case letters converted to lower-case.
   250.    function to_lower (C : Character)
   251.    return Character
   252.    renames Ada.Characters.Handling.to_lower;
   253.
   254.    -- Return S with all Latin-1 upper-case letters converted to lower-case.
   255.    function to_lower (S : String)
   256.    return String
   257.    renames Ada.Characters.Handling.to_lower;
   258.
   259.    -- Return the 8-character Latin-1 string representing the 8 Case Normal characters in N.
   260.    function to_string (N : KDF9.word)
   261.    return word_as_byte_string is
   262.       word   : KDF9.word := N;
   263.       result : word_as_byte_string;
   264.    begin
   265.       for i in reverse 1 .. 8 loop
   266.          result(i) := KDF9_char_sets.TP_CN(KDF9_char_sets.symbol(word and 8#77#));
   267.          word := word / 64;
   268.       end loop;
   269.       return result;
   270.    end to_string;
   271.
   272.    -- Return the result of applying to_string to each word of a double-word.
   273.    function to_string (P : KDF9.pair)
   274.    return pair_as_byte_string is
   275.       result : pair_as_byte_string;
   276.    begin
   277.       result(1 ..  8) := to_string(P.msw);
   278.       result(9 .. 16) := to_string(P.lsw);
   279.       return result;
   280.    end to_string;
   281.
   282.    -- Take a string and ignore it.
   283.    procedure discard (S : String) is null;
   284.
   285. end formatting;

Compiling: ../Source/formatting.ads
Source file time stamp: 2020-09-06 22:40:20
Compiled at: 2020-11-12 18:12:11

     1. -- formatting.ads
     2. --
     3. -- Provide basic data-formatting operations for KDF9 data types.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with KDF9;
    20.
    21. use  KDF9;
    22.
    23. package formatting is
    24.
    25.    subtype unit_string         is String(1 .. 1);
    26.    subtype word_as_byte_string is String(1 .. 8);
    27.    subtype pair_as_byte_string is String(1 .. 16);
    28.
    29.    -- Return N as 3 octal digits.
    30.    function oct_of (N : KDF9.syllable)
    31.    return String;
    32.
    33.    -- Return N as 6 octal digits.
    34.    function oct_of (N : KDF9.field_of_16_bits)
    35.    return String;
    36.
    37.    subtype octal_width is Positive range 1 .. 6;
    38.
    39.    -- Return N as octal digits, with (partial) zero suppression.
    40.    -- The first (6-min_digits) are elided if '0'; all remaining digits are returned.
    41.    -- Up to 6 digits can be returned if the result is longer than min_digits.
    42.    -- If N is 0, the String (1..min_digits => '0') is returned.
    43.    function oct_of (N : KDF9.Q_part; min_digits : octal_width := 6)
    44.    return String;
    45.
    46.    -- Return N as 1 .. 5 decimal digits, with zero suppression and sign when neagtive.
    47.    function signed_dec_of (N : KDF9.Q_part)
    48.    return String;
    49.
    50.    -- Return N as 1 .. 6 decimal digits, with zero suppression.
    51.    function dec_of (N : KDF9.Q_part)
    52.    return String;
    53.
    54.    -- Return N as up to 5 octal digits.
    55.    function oct_of (N : KDF9.order_word_number)
    56.    return String;
    57.
    58.    -- Return N as decimal digits, with zero suppression.
    59.    function dec_of (N : KDF9.order_word_number)
    60.    return String ;
    61.
    62.    -- Return N as 8 octal digits.
    63.    function oct_of (N : KDF9.halfword)
    64.    return String;
    65.
    66.    -- Return N as #wwwww/s, where w and s are octal digits.
    67.    function oct_of (N : KDF9.sjns_link)
    68.    return String;
    69.
    70.    -- Return N as #wwwww/s, where w and s are octal digits.
    71.    function oct_of (N : KDF9.syllable_address)
    72.    return String;
    73.
    74.    -- Return N as dddd/d, where d is a decimal digit.
    75.    function dec_of (N : KDF9.syllable_address)
    76.    return String;
    77.
    78.    -- Return N as #wwwww/s, where w and s are octal digits;
    79.    --    or as dddd/s, where d is a decimal digit, according to octal_option.
    80.    function oct_or_dec_of (N : KDF9.syllable_address; octal_option : Boolean)
    81.    return String;
    82.
    83.    -- Return N as 16 octal digits.
    84.    function oct_of (N : KDF9.word)
    85.    return String;
    86.
    87.    function as_DR_command (Q_operand : KDF9.Q_register)
    88.    return String;
    89.
    90.    function as_FD_command (Q_operand : KDF9.Q_register; for_seek, for_FH : Boolean := False)
    91.    return String;
    92.
    93.    -- Return "L', R'", or "L'" if R' is empty: "'" indicates removal of trailing blanks.
    94.    function "-" (L, R : String)
    95.    return String;
    96.
    97.    -- Return S with all leading and trailing blanks removed.
    98.    function trimmed (S : String)
    99.    return String;
   100.
   101.    -- Return trimmed(S), right-justified in a field of width at least W.
   102.    function just_right (S : String; W : Positive := 3)
   103.    return String;
   104.
   105.    -- Return trimmed(S), left-justified in a field of width at least W.
   106.    function just_left (S : String; W : Positive := 3)
   107.    return String;
   108.
   109.    -- Return the (pluralizing) suffix if count /= 1.
   110.    function plurality (count : KDF9.word; for_1 : String := ""; for_more : String := "s")
   111.    return String;
   112.
   113.    -- Return C converted to a 1-character string.
   114.    function "+" (C : Character)
   115.    return unit_string;
   116.
   117.    -- Return C with all Latin-1 lower-case letters converted to upper-case.
   118.    function to_upper (C : Character)
   119.    return Character;
   120.
   121.    -- Return S with all Latin-1 lower-case letters converted to upper-case.
   122.    function to_upper (S : String)
   123.    return String;
   124.
   125.    -- Return C with all Latin-1 upper-case letters converted to lower-case.
   126.    function to_lower (C : Character)
   127.    return Character;
   128.
   129.    -- Return S with all Latin-1 upper-case letters converted to lower-case.
   130.    function to_lower (S : String)
   131.    return String;
   132.
   133.    -- Return the 8-character Latin-1 string representing the 8 Case Normal characters in N.
   134.    function to_string (N : KDF9.word)
   135.    return word_as_byte_string;
   136.
   137.    -- Return the result of applying to_string to each word of a double-word.
   138.    function to_string (P : KDF9.pair)
   139.    return pair_as_byte_string;
   140.
   141.    -- Take a string and ignore it.
   142.    procedure discard (S : String);
   143.
   144. end formatting;

 285 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/break_in.adb
Source file time stamp: 2020-08-17 00:05:13
Compiled at: 2020-11-12 18:12:11

     1. -- break_in.adb
     2. --
     3. -- This communicates a break-in to the microcode.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with exceptions;
    20. with HCI;
    21. with KDF9;
    22. with POSIX;
    23. with finalize_ee9;
    24. with settings;
    25. with state_display;
    26.
    27. use  exceptions;
    28. use  HCI;
    29. use  KDF9;
    30. use  settings;
    31. use  state_display;
    32.
    33. package body break_in is
    34.
    35.    requested : Boolean := False
    36.       with Atomic, Volatile;
    37.
    38.    procedure note_user_interrupt is
    39.    begin
    40.       if requested then return; end if;  -- The handler is already running.
    41.       requested := True;
    42.    end note_user_interrupt;
    43.
    44.    function has_been_requested
    45.    return Boolean is
    46.    begin
    47.       return requested;
    48.    end has_been_requested;
    49.
    50.    procedure handler is
    51.    begin
    52.       requested := False;
    53.       interact("Break-in");
    54.       quit_if_requested;
    55.       if the_execution_mode = boot_mode then
    56.          effect(FLEX_interrupt);
    57.       else
    58.          show_current_state;
    59.       end if;
    60.       flush;
    61.    exception
    62.       when quit_request =>
    63.          finalize_ee9("Quit requested by the user");
    64.          POSIX.exit_program(0);
    65.    end handler;
    66.
    67. end break_in;

Compiling: ../Source/break_in.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- break_in.ads
     2. --
     3. -- This conveys a break-in to the microcode.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package break_in is
    20.
    21.    function has_been_requested
    22.    return Boolean
    23.       with Inline;
    24.
    25.    procedure note_user_interrupt;
    26.
    27.    procedure handler;
    28.
    29. end break_in;

 67 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/kdf9-microcode.adb
Source file time stamp: 2020-11-02 19:40:52
Compiled at: 2020-11-12 18:12:11

     1. -- kdf9-microcode.adb
     2. --
     3. -- KDF9 ISP emulation - CPU microcode routines.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with break_in;
    20. with exceptions;
    21. with IOC;
    22. with IOC.dispatcher;
    23. with KDF9.CPU;
    24. with KDF9.Directors;
    25. with KDF9.decoding;
    26. with KDF9.store;
    27. with POSIX;
    28. with settings;
    29. with state_display;
    30. with tracing;
    31.
    32. use  exceptions;
    33. use  IOC;
    34. use  IOC.dispatcher;
    35. use  KDF9.CPU;
    36. use  KDF9.Directors;
    37. use  KDF9.decoding;
    38. use  KDF9.store;
    39. use  POSIX;
    40. use  settings;
    41. use  state_display;
    42. use  tracing;
    43.
    44. package body KDF9.microcode is
    45.
    46.    procedure do_a_one_syllable_order is
    47.       A, B, C, E : KDF9.word;
    48.       bit_count  : KDF9.word;
    49.       AB, CD     : KDF9.pair;
    50.       X, Y       : CPU.f48;
    51.       XY, ZT     : CPU.f96;
    52.    begin
    53.       case INS.compressed_opcode is
    54.
    55.          when 0 =>
    56.             -- The DUMMY order originally had code 0, before being changed to #17.
    57.             -- The Kidsgrove compiler continued to use code 0, so we must assume that,
    58.             --    despite the Manual, 0 continued to be a valid no-op order.
    59.             the_CPU_delta := the_CPU_delta + 1;
    60.
    61.          when VR =>
    62.             the_V_bit_is_set := False;
    63.             the_CPU_delta := the_CPU_delta + 1;
    64.
    65.          when TO_TR =>
    66.             ensure_that_the_nest_holds_an_operand;
    67.             if resign(pop) < 0 then
    68.                the_T_bit_is_set := True;
    69.             end if;
    70.             the_CPU_delta := the_CPU_delta + 2;
    71.
    72.          when BITS =>
    73.             write_top(cardinality(read_top));
    74.             the_CPU_delta := the_CPU_delta + 27;
    75.
    76.          when XF =>
    77.             ensure_that_the_nest_holds_2_operands;
    78.             X := pop;
    79.             Y := read_top;
    80.             write_top(CPU.f48'(Y * X));
    81.             the_CPU_delta := the_CPU_delta + 15;
    82.
    83.          when XDF =>
    84.             ensure_that_the_nest_holds_2_operands;
    85.             XY := read_top;
    86.             ZT := XY.lsw * XY.msw;
    87.             write_top(ZT);
    88.             the_CPU_delta := the_CPU_delta + 16;
    89.
    90.          when XPLUSF =>
    91.             ensure_that_the_nest_holds(at_least => 4);
    92.             XY := pop;
    93.             ZT := XY.lsw * XY.msw;
    94.             XY := read_top;
    95.             write_top(XY + ZT);
    96.             the_CPU_delta := the_CPU_delta + 18;
    97.
    98.          when NEGD =>
    99.             AB := read_top;
   100.             write_top( - AB);
   101.             the_CPU_delta := the_CPU_delta + 2;
   102.
   103.          when OR_9 =>
   104.             ensure_that_the_nest_holds_2_operands;
   105.             A := pop;
   106.             write_top(read_top or A);
   107.             the_CPU_delta := the_CPU_delta + 1;
   108.
   109.          when PERM =>
   110.             A := pop;
   111.             CD := pop;
   112.             push(A);
   113.             push(CD);
   114.             the_CPU_delta := the_CPU_delta + 2;
   115.
   116.          when TOB =>
   117.             ensure_that_the_nest_holds_2_operands;
   118.             A := pop;      -- the value
   119.             bit_count := cardinality(A);
   120.             B := read_top; -- the radixes
   121.             C := 0;
   122.
   123.             for i in 1 .. 8 loop
   124.                A := rotate_word_left(A, 6);
   125.                B := rotate_word_left(B, 6);
   126.                E := B and 8#77#; -- this radix
   127.                C := C*E + (A and 8#77#);
   128.             end loop;
   129.
   130.             write_top(C);
   131.             the_CPU_delta := the_CPU_delta + 2 + 4*KDF9.us(bit_count);
   132.
   133.          when ROUNDH =>
   134.             A := read_top;
   135.             write_top(resign(A) + 2**23);
   136.             the_CPU_delta := the_CPU_delta + 22;
   137.
   138.          when NEV =>
   139.             ensure_that_the_nest_holds_2_operands;
   140.             A := pop;
   141.             write_top(read_top xor A);
   142.             the_CPU_delta := the_CPU_delta + 2;
   143.
   144.          when ROUND =>
   145.             ensure_that_the_nest_holds_2_operands;
   146.             A := pop;
   147.             write_top(resign(A) + resign(shift_word_right(read_top, 46) and 1));
   148.             the_CPU_delta := the_CPU_delta + 1;
   149.
   150.          when DUMMY =>
   151.             the_CPU_delta := the_CPU_delta + 1;
   152.
   153.          when ROUNDF =>
   154.             ensure_that_the_nest_holds_2_operands;
   155.             XY := pop;
   156.             ZT := XY;
   157.             push(narrowed(XY));
   158.             the_CPU_delta := the_CPU_delta + 3;
   159.
   160.          when ROUNDHF =>
   161.             X := pop;
   162.             push(narrowed(X));
   163.             the_CPU_delta := the_CPU_delta + 3;
   164.
   165.          when MINUSDF =>
   166.             ensure_that_the_nest_holds(at_least => 4);
   167.             XY := pop;
   168.             ZT := read_top;
   169.             write_top(ZT - XY);
   170.             the_CPU_delta := the_CPU_delta + 12;
   171.
   172.          when PLUSDF =>
   173.             ensure_that_the_nest_holds(at_least => 4);
   174.             XY := pop;
   175.             ZT := read_top;
   176.             write_top(ZT + XY);
   177.             the_CPU_delta := the_CPU_delta + 12;
   178.
   179.          when FLOAT_9 =>
   180.             ensure_that_the_nest_holds_2_operands;
   181.             -- There is great uncertainty as to how the FLOAT/FLOATD orders handled scale factors
   182.             --   in N1 that lie outside the range allowed by the Manual, namely -128 <= N1 <= +127.
   183.             -- ee9 here takes a safety-first approach which is consistent with the
   184.             --    behaviour of the Kidsgrove sqrt function with arguments < 0.5º0.
   185.             A := shift_arithmetic(shift_logical(pop, +40), -40);
   186.             B := read_top;
   187.             write_top(KDF9.word(normalized(full_fraction => B, scaler => A)));
   188.
   189.          when FLOATD =>
   190.             ensure_that_the_nest_holds(at_least => 3);
   191.             A := shift_arithmetic(shift_logical(pop, +40), -40);
   192.             CD := read_top;
   193.             -- See §3.4 of Report K/GD.y.83, dated 6/12/1962.  It would seem to require this:
   194.             -- CD.lsw := CD.lsw and not 8#77#;  -- The 6 l.s.b. are lost.
   195.             -- The above is commented out because it gives the wrong answer with KAA01.
   196.             -- A post-document hardware modification is suspected.
   197.             reconstruct(CD, scaler => A);
   198.             write_top(CD);
   199.             the_CPU_delta := the_CPU_delta + 8;
   200.
   201.          when ABS_9 =>
   202.             write_top( abs resign(read_top));
   203.             the_CPU_delta := the_CPU_delta + 1;
   204.
   205.          when NEG =>
   206.             write_top( - resign(read_top));
   207.             the_CPU_delta := the_CPU_delta + 1;
   208.
   209.          when ABSF =>
   210.             X := read_top;
   211.             if resign(KDF9.word(X)) < 0 then
   212.                write_top( - X);
   213.                the_CPU_delta := the_CPU_delta + 4;
   214.             else
   215.                the_CPU_delta := the_CPU_delta + 1;
   216.             end if;
   217.
   218.          when NEGF =>
   219.             X := read_top;
   220.             write_top( - X);
   221.             the_CPU_delta := the_CPU_delta + 3;
   222.
   223.          when MAX =>
   224.             AB := read_top;
   225.             if resign(AB.lsw) >= resign(AB.msw) then
   226.                write_top(KDF9.pair'(msw => AB.lsw, lsw =>AB.msw));
   227.                the_V_bit_is_set := True;
   228.             end if;
   229.             the_CPU_delta := the_CPU_delta + 4;
   230.
   231.          when NOT_9 =>
   232.             A := read_top;
   233.             write_top(not A);
   234.             the_CPU_delta := the_CPU_delta + 1;
   235.
   236.          when XD =>
   237.             AB := read_top;
   238.             CD := AB.msw * AB.lsw;
   239.             write_top(CD);
   240.             the_CPU_delta := the_CPU_delta + 14;
   241.
   242.          when X_frac =>
   243.             ensure_that_the_nest_holds_2_operands;
   244.             A := pop;
   245.             write_top(as_word(CPU.fraction'(read_top * A)));
   246.             the_CPU_delta := the_CPU_delta + 15;
   247.
   248.          when MINUS =>
   249.             ensure_that_the_nest_holds_2_operands;
   250.             A := pop;
   251.             B := read_top;
   252.             write_top(resign(B) - resign(A));
   253.             the_CPU_delta := the_CPU_delta + 1;
   254.
   255.          when SIGN =>
   256.             ensure_that_the_nest_holds_2_operands;
   257.             A := pop;
   258.             B := read_top;
   259.             if B = A then
   260.                write_top(KDF9.word'(0));
   261.             elsif resign(B) > resign(A) then
   262.                write_top(KDF9.word'(1));
   263.             else
   264.                write_top(all_one_bits);
   265.             end if;
   266.             the_CPU_delta := the_CPU_delta + 3;
   267.
   268.          when ZERO =>
   269.             ensure_that_the_nest_has_room_for_a_result;
   270.             push(all_zero_bits);
   271.             the_CPU_delta := the_CPU_delta + 2;
   272.
   273.          when DUP =>
   274.             ensure_that_the_nest_has_room_for_a_result;
   275.             A := read_top;
   276.             push(A);
   277.             the_CPU_delta := the_CPU_delta + 2;
   278.
   279.          when DUPD =>
   280.             ensure_that_the_nest_has_room_for_2_results;
   281.             AB := read_top;
   282.             push(AB);
   283.             the_CPU_delta := the_CPU_delta + 4;
   284.
   285.          when DIVI =>
   286.             AB := read_top;
   287.             do_DIVI(L => AB.lsw,
   288.                     R => AB.msw,
   289.                     Quotient  => CD.lsw,
   290.                     Remainder => CD.msw);
   291.             write_top(CD);
   292.             the_CPU_delta := the_CPU_delta + 36;
   293.
   294.          when FIX =>
   295.             ensure_that_the_nest_holds_an_operand;
   296.             ensure_that_the_nest_has_room_for_a_result;
   297.             X := read_top;
   298.             write_top(fraction_word(X));
   299.             push(scaler(X));
   300.             the_CPU_delta := the_CPU_delta + 6;
   301.
   302.          when STR =>
   303.             ensure_that_the_nest_has_room_for_a_result;
   304.             A := read_top;
   305.             if resign(A) < 0 then
   306.                write_top(A and not_sign_bit);
   307.                push(all_one_bits);
   308.             else
   309.                push(all_zero_bits);
   310.             end if;
   311.             the_CPU_delta := the_CPU_delta + 3;
   312.
   313.          when CONT =>
   314.             ensure_that_the_nest_holds_2_operands;
   315.             A := pop;
   316.             B := read_top;
   317.             write_top(contracted(msw => A, lsw => B));
   318.             the_CPU_delta := the_CPU_delta + 2;
   319.
   320.          when REVD =>
   321.             AB := pop;
   322.             CD := pop;
   323.             push(AB);
   324.             push(CD);
   325.             the_CPU_delta := the_CPU_delta + 4;
   326.
   327.          when ERASE =>
   328.             ensure_that_the_nest_holds_an_operand;
   329.             pop;
   330.             the_CPU_delta := the_CPU_delta + 1;
   331.
   332.          when MINUSD =>
   333.             ensure_that_the_nest_holds(at_least => 4);
   334.             AB := pop;
   335.             CD := read_top;
   336.             write_top(CD - AB);
   337.             the_CPU_delta := the_CPU_delta + 3;
   338.
   339.          when AND_9 =>
   340.             ensure_that_the_nest_holds_2_operands;
   341.             A := pop;
   342.             write_top(read_top and A);
   343.             the_CPU_delta := the_CPU_delta + 1;
   344.
   345.          when PLUS =>
   346.             ensure_that_the_nest_holds_2_operands;
   347.             A := pop;
   348.             B := read_top;
   349.             write_top(resign(B) + resign(A));
   350.             the_CPU_delta := the_CPU_delta + 1;
   351.
   352.          when PLUSD =>
   353.             ensure_that_the_nest_holds(at_least => 4);
   354.             AB := pop;
   355.             CD := read_top;
   356.             write_top(CD + AB);
   357.             the_CPU_delta := the_CPU_delta + 3;
   358.
   359.          when DIV =>
   360.             ensure_that_the_nest_holds_2_operands;
   361.             AB := pop;
   362.             push(as_word(CPU.fraction'(AB.lsw / AB.msw)));
   363.             the_CPU_delta := the_CPU_delta + 36;
   364.
   365.          when DIVD =>
   366.             ensure_that_the_nest_holds(at_least => 3);
   367.             A := pop;
   368.             CD := pop;
   369.             do_DIVD(L => CD,
   370.                     R => A,
   371.                     Q => E);
   372.             push(E);
   373.             the_CPU_delta := the_CPU_delta + 36;
   374.
   375.          when DIVF =>
   376.             X := pop;
   377.             Y := read_top;
   378.             write_top(Y / X);
   379.             the_CPU_delta := the_CPU_delta + 36;
   380.
   381.          when DIVDF =>
   382.             ensure_that_the_nest_holds(at_least => 3);
   383.             Y := pop;
   384.             XY := pop;
   385.             push(XY / Y);
   386.             the_CPU_delta := the_CPU_delta + 35;
   387.
   388.          when DIVR =>
   389.             ensure_that_the_nest_holds(at_least => 3);
   390.             A := pop;
   391.             CD := read_top;
   392.             do_DIVR(L => CD,
   393.                     R => A,
   394.                     Quotient  => AB.msw,
   395.                     Remainder => AB.lsw);
   396.             write_top(AB);
   397.             the_CPU_delta := the_CPU_delta + 36;
   398.
   399.          when REV =>
   400.             AB := read_top;
   401.             write_top(KDF9.pair'(msw => AB.lsw, lsw =>AB.msw));
   402.             the_CPU_delta := the_CPU_delta + 1;
   403.
   404.          when CAB =>
   405.             AB := pop;
   406.             C := pop;
   407.             push(AB);
   408.             push(C);
   409.             the_CPU_delta := the_CPU_delta + 2;
   410.
   411.          when FRB =>
   412.             ensure_that_the_nest_holds_2_operands;
   413.             A := pop;      -- the value
   414.             bit_count := cardinality(A);
   415.             B := read_top; -- the radixes
   416.             C := 0;
   417.
   418.             for i in 1 .. 8 loop
   419.                E := B and 8#77#;
   420.                if E /= 0 then
   421.                   C := C or (A mod E);
   422.                   A := A / E;
   423.                else
   424.                   if A /= 0 then the_V_bit_is_set := True; end if;
   425.                end if;
   426.                B := shift_word_right(B, 6);
   427.                C := rotate_word_right(C, 6);
   428.             end loop;
   429.
   430.             if A /= 0 then
   431.                -- The value was too big for the representation; see Manual.
   432.                the_V_bit_is_set := True;
   433.             end if;
   434.             write_top(C);
   435.             the_CPU_delta := the_CPU_delta + 8 + 3*KDF9.us(bit_count);
   436.
   437.          when STAND =>
   438.             X := read_top;
   439.             write_top(normalized(X));
   440.             the_CPU_delta := the_CPU_delta + 5;
   441.
   442.          when NEGDF =>
   443.             XY := read_top;
   444.             write_top( - XY);
   445.             the_CPU_delta := the_CPU_delta + 9;
   446.
   447.          when MAXF =>
   448.             XY := read_top;
   449.             if XY.lsw >= XY.msw then
   450.                write_top(CPU.f96'(msw => XY.lsw, lsw =>XY.msw));
   451.                the_V_bit_is_set := True;
   452.             end if;
   453.             the_CPU_delta := the_CPU_delta + 6;
   454.
   455.          when PLUSF =>
   456.             ensure_that_the_nest_holds_2_operands;
   457.             X := pop;
   458.             Y := read_top;
   459.             write_top(Y + X);
   460.             the_CPU_delta := the_CPU_delta + 7;
   461.
   462.          when MINUSF =>
   463.             ensure_that_the_nest_holds_2_operands;
   464.             X := pop;
   465.             Y := read_top;
   466.             write_top(Y - X);
   467.             the_CPU_delta := the_CPU_delta + 7;
   468.
   469.          when SIGNF =>
   470.             ensure_that_the_nest_holds_2_operands;
   471.             XY := pop;
   472.             if KDF9.word(XY.lsw) = KDF9.word(XY.msw) then
   473.                push(all_zero_bits);
   474.             elsif XY.lsw < XY.msw then
   475.                push(all_one_bits);
   476.             else
   477.                push(KDF9.word(1));
   478.             end if;
   479.             the_CPU_delta := the_CPU_delta + 5;
   480.
   481.          when others =>
   482.             trap_invalid_instruction;
   483.
   484.       end case;
   485.    end do_a_one_syllable_order;
   486.
   487.
   488.    procedure do_an_IO_order is
   489.       IO_opcode   : constant KDF9.compressed_opcode := (INS.Qk and not manual_bit);
   490.       IO_operand  : constant KDF9.Q_register := the_Q_store(INS.Qq);
   491.       set_offline : constant Boolean         := (INS.Qk and manual_bit) /= 0;
   492.    begin
   493.       case INS.compressed_opcode is
   494.
   495.          when PAR_Qq =>
   496.             the_CPU_delta := the_CPU_delta + 11;
   497.             PAR(IO_operand, set_offline, the_T_bit_is_set);
   498.             the_CPU_delta := the_CPU_delta + 3;
   499.
   500.          when PIA_PIC_CLO_TLO_Qq =>
   501.             the_CPU_delta := the_CPU_delta + 15;
   502.             case IO_opcode is
   503.                when PIA_bits =>
   504.                   PIA(IO_operand, set_offline);
   505.                   the_CPU_delta := the_CPU_delta + 7;
   506.                when PIC_bits =>
   507.                   PIC(IO_operand, set_offline);
   508.                   the_CPU_delta := the_CPU_delta + 7;
   509.                when CLO_bits =>
   510.                   fail_in_problem_program_state;
   511.                   CLO(IO_operand, set_offline);
   512.                   the_CPU_delta := the_CPU_delta + 1;
   513.                when TLO_bits =>
   514.                   TLO(IO_operand, the_T_bit_is_set);
   515.                when others =>
   516.                   trap_invalid_instruction;
   517.             end case;
   518.
   519.          when PIB_PID_Qq =>
   520.             the_CPU_delta := the_CPU_delta + 15;
   521.             case IO_opcode is
   522.                when PIB_bits =>
   523.                   PIB(IO_operand, set_offline);
   524.                   the_CPU_delta := the_CPU_delta + 7;
   525.                when PID_bits =>
   526.                   PID(IO_operand, set_offline);
   527.                   the_CPU_delta := the_CPU_delta + 7;
   528.                when others =>
   529.                   trap_invalid_instruction;
   530.             end case;
   531.
   532.          when PIE_PIG_Qq =>
   533.             the_CPU_delta := the_CPU_delta + 15;
   534.             case IO_opcode is
   535.                when PIE_bits =>
   536.                   PIE(IO_operand, set_offline);
   537.                   the_CPU_delta := the_CPU_delta + 7;
   538.                when PIG_bits =>
   539.                   PIG(IO_operand, set_offline);
   540.                   the_CPU_delta := the_CPU_delta + 7;
   541.                when others =>
   542.                   trap_invalid_instruction;
   543.             end case;
   544.
   545.          when PIF_PIH_Qq =>
   546.             the_CPU_delta := the_CPU_delta + 15;
   547.             case IO_opcode is
   548.                when PIF_bits =>
   549.                   PIF(IO_operand, set_offline);
   550.                   the_CPU_delta := the_CPU_delta + 7;
   551.                when PIH_bits =>
   552.                   PIH(IO_operand, set_offline);
   553.                   the_CPU_delta := the_CPU_delta + 7;
   554.                when others =>
   555.                   trap_invalid_instruction;
   556.             end case;
   557.
   558.          when PMA_PMK_INT_Qq =>
   559.             the_CPU_delta := the_CPU_delta + 11;
   560.             case IO_opcode is
   561.                when PMA_bits =>
   562.                   PMA(IO_operand, set_offline);
   563.                when PMK_bits =>
   564.                   PMK(IO_operand, set_offline);
   565.                when INT_bits =>
   566.                   INT(IO_operand, set_offline);
   567.                when others =>
   568.                   trap_invalid_instruction;
   569.             end case;
   570.
   571.          when CT_PMB_PMC_BUSY_Qq =>
   572.             the_CPU_delta := the_CPU_delta + 11;
   573.             case IO_opcode is
   574.                when CTQ_bits =>
   575.                   -- if set_offline then MANUALQq else CTQq
   576.                   if set_offline                      or else
   577.                         the_CPU_state = Director_state   then
   578.                      MANUAL_CT(IO_operand, set_offline);
   579.                      the_CPU_delta := the_CPU_delta + 2;
   580.                   else
   581.                      trap_invalid_instruction; -- This will always LIV, as we are not in Director.
   582.                   end if;
   583.                when PMB_bits =>
   584.                   PMB(IO_operand, set_offline);
   585.                   the_CPU_delta := the_CPU_delta + 3;
   586.                when PMC_bits =>
   587.                   PMC(IO_operand, set_offline);
   588.                   the_CPU_delta := the_CPU_delta + 3;
   589.                when BUSY_bits =>
   590.                   BUSY(IO_operand, set_offline, the_T_bit_is_set);
   591.                   the_CPU_delta := the_CPU_delta + 2;
   592.                when others =>
   593.                   trap_invalid_instruction;
   594.             end case;
   595.
   596.          when PMD_PME_PML_Qq =>
   597.             the_CPU_delta := the_CPU_delta + 14;
   598.             case IO_opcode is
   599.                when PMD_bits =>
   600.                   PMD(IO_operand, set_offline);
   601.                   the_CPU_delta := the_CPU_delta + 5;
   602.                when PME_bits =>
   603.                   PME(IO_operand, set_offline);
   604.                   the_CPU_delta := the_CPU_delta + 5;
   605.                when PML_bits =>
   606.                   PML(IO_operand, set_offline);
   607.                   the_CPU_delta := the_CPU_delta + 5;
   608.                when others =>
   609.                   trap_invalid_instruction;
   610.             end case;
   611.
   612.          when PMF_PMG_Qq =>
   613.             the_CPU_delta := the_CPU_delta + 11;
   614.             case IO_opcode is
   615.                when PMF_bits =>
   616.                   PMF(IO_operand, set_offline);
   617.                   the_CPU_delta := the_CPU_delta + 3;
   618.                when PMG_bits =>
   619.                   fail_in_problem_program_state;
   620.                   the_CPU_delta := the_CPU_delta + 14;  -- ??
   621.                   PMG(IO_operand, set_offline);
   622.                 when others =>
   623.                   trap_invalid_instruction;
   624.             end case;
   625.
   626.          when POA_POC_POE_POF_PMH_Qq =>
   627.             the_CPU_delta := the_CPU_delta + 15;
   628.             case IO_opcode is
   629.                when POA_bits =>
   630.                   POA(IO_operand, set_offline);
   631.                   the_CPU_delta := the_CPU_delta + 7;
   632.                when POC_bits =>
   633.                   POC(IO_operand, set_offline);
   634.                   the_CPU_delta := the_CPU_delta + 7;
   635.                when POE_bits =>
   636.                   POE(IO_operand, set_offline);
   637.                   the_CPU_delta := the_CPU_delta + 4;
   638.                when POF_bits =>
   639.                   POF(IO_operand, set_offline);
   640.                   the_CPU_delta := the_CPU_delta + 4;
   641.                when PMH_bits =>
   642.                   fail_in_problem_program_state;
   643.                   SLO(IO_operand, set_offline);
   644.                   the_CPU_delta := the_CPU_delta + 1;
   645.                when others =>
   646.                   trap_invalid_instruction;
   647.             end case;
   648.
   649.          when POB_POD_Qq =>
   650.             the_CPU_delta := the_CPU_delta + 15;
   651.             case IO_opcode is
   652.                when POB_bits =>
   653.                   POB(IO_operand, set_offline);
   654.                   the_CPU_delta := the_CPU_delta + 7;
   655.                when POD_bits =>
   656.                   POD(IO_operand, set_offline);
   657.                   the_CPU_delta := the_CPU_delta + 7;
   658.                when others =>
   659.                   trap_invalid_instruction;
   660.             end case;
   661.
   662.          when POG_POL_Qq =>
   663.             the_CPU_delta := the_CPU_delta + 15;
   664.             case IO_opcode is
   665.                when POG_bits =>
   666.                   POG(IO_operand, set_offline);
   667.                   the_CPU_delta := the_CPU_delta + 7;
   668.                when POL_bits =>
   669.                   POL(IO_operand, set_offline);
   670.                   the_CPU_delta := the_CPU_delta + 7;
   671.                when others =>
   672.                   trap_invalid_instruction;
   673.             end case;
   674.
   675.          when POH_POK_Qq =>
   676.             the_CPU_delta := the_CPU_delta + 15;
   677.             case IO_opcode is
   678.                when POH_bits =>
   679.                   POH(IO_operand, set_offline);
   680.                   the_CPU_delta := the_CPU_delta + 7;
   681.                when POK_bits =>
   682.                   POK(IO_operand, set_offline);
   683.                   the_CPU_delta := the_CPU_delta + 7;
   684.                when others =>
   685.                   trap_invalid_instruction;
   686.             end case;
   687.
   688.          when others =>
   689.             trap_invalid_instruction;
   690.
   691.       end case;
   692.    end do_an_IO_order;
   693.
   694.
   695.    all_zero_Q_store : constant KDF9.Q_register := (C | I | M => 0);
   696.
   697.    procedure ensure_that_Q0_contains_zero (suspect : KDF9.Q_number)
   698.       with Inline;
   699.
   700.    procedure ensure_that_Q0_contains_zero (suspect : KDF9.Q_number) is
   701.    begin
   702.       if suspect = 0 then
   703.          the_Q_store(0) := all_zero_Q_store;  -- Override any assignment to Q0.
   704.       end if;
   705.    end ensure_that_Q0_contains_zero;
   706.
   707.    procedure auto_increment
   708.       with Inline;
   709.
   710.    procedure auto_increment is
   711.    begin
   712.       if INS.Qq /= 0 then
   713.          the_Q_store(INS.Qq).M := the_Q_store(INS.Qq).M + the_Q_store(INS.Qq).I;
   714.          the_Q_store(INS.Qq).C := the_Q_store(INS.Qq).C - 1;
   715.       end if;
   716.    end auto_increment;
   717.
   718.    function shift_count
   719.    return CPU.signed_Q_part
   720.       with Inline;
   721.
   722.    function shift_count
   723.    return CPU.signed_Q_part
   724.    is (
   725.        if (INS.order.syllable_1 and constant_bit) /= 0  then
   726.           resign(KDF9.Q_part(INS.order.syllable_1/2 xor 64)) - 64
   727.        else
   728.           resign((the_Q_store(INS.Qq).C and 255) xor 128) - 128
   729.       );
   730.
   731.
   732.    procedure do_a_two_syllable_order is
   733.       A  : KDF9.word;
   734.       AB : KDF9.pair;
   735.       CD : KDF9.pair;
   736.    begin
   737.       case INS.compressed_opcode is
   738.
   739.          when JCqNZS =>
   740.             if CIA.syllable_index = 5 then
   741.                -- KDF9 did not actually detect this error, and the JCqNZS instruction often worked,
   742.                --    unless broken-into by an interrupt, which returned to the word following that
   743.                --       containing the first syllable of the JCqNZS instruction.
   744.                -- I see no case for reproducing this behaviour.
   745.                trap_invalid_instruction ("JCqNZS instruction at syllable 5");
   746.             end if;
   747.             if the_Q_store(INS.Qq).C /= 0 then
   748.                if fetching_normally then
   749.                   set_IWB0_and_IWB1_for_a_JCqNZS_loop;
   750.                   the_CPU_delta := the_CPU_delta + 7;  -- Takes 11µs the first time it jumps.
   751.                end if;
   752.                -- The IWBs now contain the loop, so go to syllable 0 of IWB0.
   753.                go_back_to_the_start_of_IWB0;
   754.             else
   755.                continue_after_JCqNZS;
   756.             end if;
   757.             the_CPU_delta := the_CPU_delta + 4;
   758.
   759.          when MkMq =>
   760.             the_trace_address := valid_word_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
   761.             check_address_and_lockout(the_trace_address);
   762.             ensure_that_the_nest_has_room_for_a_result;
   763.             the_trace_operand := fetch_word(the_trace_address);
   764.             push(the_trace_operand);
   765.             the_CPU_delta := the_CPU_delta + 7;
   766.
   767.          when MkMqQ =>
   768.             the_trace_address := valid_word_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
   769.             check_address_and_lockout(the_trace_address);
   770.             ensure_that_the_nest_has_room_for_a_result;
   771.             the_trace_operand := fetch_word(the_trace_address);
   772.             push(the_trace_operand);
   773.             auto_increment;
   774.             the_CPU_delta := the_CPU_delta + 8;
   775.
   776.          when MkMqH =>
   777.             the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
   778.             check_address_and_lockout(the_trace_address);
   779.             the_trace_operand := fetch_halfword(the_trace_address, the_Q_store(INS.Qq).M mod 2);
   780.             ensure_that_the_nest_has_room_for_a_result;
   781.             push(the_trace_operand);
   782.             the_CPU_delta := the_CPU_delta + 7;
   783.
   784.          when MkMqQH =>
   785.             the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
   786.             check_address_and_lockout(the_trace_address);
   787.             the_trace_operand := fetch_halfword(the_trace_address, the_Q_store(INS.Qq).M mod 2);
   788.             ensure_that_the_nest_has_room_for_a_result;
   789.             push(the_trace_operand);
   790.             auto_increment;
   791.             the_CPU_delta := the_CPU_delta + 8;
   792.
   793.          when MkMqN =>
   794.             the_trace_address := valid_word_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
   795.             check_address_and_lockout(the_trace_address);
   796.             ensure_that_the_nest_has_room_for_a_result;
   797.             the_trace_operand := fetch_word(the_trace_address);
   798.             push(the_trace_operand);
   799.             the_CPU_delta := the_CPU_delta + 7;
   800.
   801.          when MkMqQN =>
   802.             the_trace_address := valid_word_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
   803.             check_address_and_lockout(the_trace_address);
   804.             ensure_that_the_nest_has_room_for_a_result;
   805.             the_trace_operand := fetch_word(the_trace_address);
   806.             push(the_trace_operand);
   807.             auto_increment;
   808.             the_CPU_delta := the_CPU_delta + 8;
   809.
   810.          when MkMqHN =>
   811.             the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
   812.             check_address_and_lockout(the_trace_address);
   813.             the_trace_operand := fetch_halfword(the_trace_address, the_Q_store(INS.Qq).M mod 2);
   814.             ensure_that_the_nest_has_room_for_a_result;
   815.             push(the_trace_operand);
   816.             the_CPU_delta := the_CPU_delta + 7;
   817.
   818.          when MkMqQHN =>
   819.             the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
   820.             check_address_and_lockout(the_trace_address);
   821.             the_trace_operand := fetch_halfword(the_trace_address, the_Q_store(INS.Qq).M mod 2);
   822.             ensure_that_the_nest_has_room_for_a_result;
   823.             push(the_trace_operand);
   824.             auto_increment;
   825.             the_CPU_delta := the_CPU_delta + 8;
   826.
   827.          when TO_MkMq =>
   828.             the_trace_address := valid_word_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
   829.             check_address_and_lockout(the_trace_address);
   830.             ensure_that_the_nest_holds_an_operand;
   831.             the_trace_operand := pop;
   832.             store_word(the_trace_operand, the_trace_address);
   833.             the_CPU_delta := the_CPU_delta + 7;
   834.
   835.          when TO_MkMqQ =>
   836.             the_trace_address := valid_word_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
   837.             check_address_and_lockout(the_trace_address);
   838.             ensure_that_the_nest_holds_an_operand;
   839.             the_trace_operand := pop;
   840.             store_word(the_trace_operand, the_trace_address);
   841.             auto_increment;
   842.             the_CPU_delta := the_CPU_delta + 8;
   843.
   844.          when TO_MkMqH =>
   845.             the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
   846.             check_address_and_lockout(the_trace_address);
   847.             ensure_that_the_nest_holds_an_operand;
   848.             the_trace_operand := pop;
   849.             store_halfword(the_trace_operand, the_trace_address, the_Q_store(INS.Qq).M mod 2);
   850.             the_CPU_delta := the_CPU_delta + 7;
   851.
   852.          when TO_MkMqQH =>
   853.             the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M, the_Q_store(INS.Qq).M);
   854.             check_address_and_lockout(the_trace_address);
   855.             ensure_that_the_nest_holds_an_operand;
   856.             the_trace_operand := pop;
   857.             store_halfword(the_trace_operand, the_trace_address, the_Q_store(INS.Qq).M mod 2);
   858.             auto_increment;
   859.             the_CPU_delta := the_CPU_delta + 8;
   860.
   861.          when TO_MkMqN =>
   862.             the_trace_address := valid_word_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
   863.             check_address_and_lockout(the_trace_address);
   864.             ensure_that_the_nest_holds_an_operand;
   865.             the_trace_operand := pop;
   866.             store_word(the_trace_operand, the_trace_address);
   867.             the_CPU_delta := the_CPU_delta + 7;
   868.
   869.          when TO_MkMqQN =>
   870.             the_trace_address := valid_word_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
   871.             check_address_and_lockout(the_trace_address);
   872.             ensure_that_the_nest_holds_an_operand;
   873.             the_trace_operand := pop;
   874.             store_word(the_trace_operand, the_trace_address);
   875.             auto_increment;
   876.             the_CPU_delta := the_CPU_delta + 8;
   877.
   878.          when TO_MkMqHN =>
   879.             the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
   880.             check_address_and_lockout(the_trace_address);
   881.             ensure_that_the_nest_holds_an_operand;
   882.             the_trace_operand := pop;
   883.             store_halfword(the_trace_operand, the_trace_address, the_Q_store(INS.Qq).M mod 2);
   884.             the_CPU_delta := the_CPU_delta + 7;
   885.
   886.          when TO_MkMqQHN =>
   887.             the_trace_address := valid_halfword_address(the_Q_store(INS.Qk).M+1, the_Q_store(INS.Qq).M);
   888.             check_address_and_lockout(the_trace_address);
   889.             ensure_that_the_nest_holds_an_operand;
   890.             the_trace_operand := pop;
   891.             store_halfword(the_trace_operand, the_trace_address, the_Q_store(INS.Qq).M mod 2);
   892.             auto_increment;
   893.             the_CPU_delta := the_CPU_delta + 8;
   894.
   895.          when M_PLUS_Iq =>
   896.             the_CPU_delta := the_CPU_delta + 4;
   897.             the_Q_store(INS.Qq).M := the_Q_store(INS.Qq).M + the_Q_store(INS.Qq).I;
   898.
   899.          when M_MINUS_Iq =>
   900.             the_CPU_delta := the_CPU_delta + 5;
   901.             the_Q_store(INS.Qq).M := the_Q_store(INS.Qq).M - the_Q_store(INS.Qq).I;
   902.
   903.          when NCq =>
   904.             the_CPU_delta := the_CPU_delta + 5;
   905.             the_Q_store(INS.Qq).C := - the_Q_store(INS.Qq).C;
   906.
   907.          when DCq =>
   908.             the_CPU_delta := the_CPU_delta + 3;
   909.             if INS.Qq /= 0 then
   910.                the_Q_store(INS.Qq).C := the_Q_store(INS.Qq).C - 1;
   911.             end if;
   912.
   913.          when POS1_TO_Iq =>
   914.             the_CPU_delta := the_CPU_delta + 3;
   915.             if INS.Qq /= 0 then
   916.                the_Q_store(INS.Qq).I := + 1;
   917.             end if;
   918.
   919.          when NEG1_TO_Iq =>
   920.             the_CPU_delta := the_CPU_delta + 3;
   921.             if INS.Qq /= 0 then
   922.                the_Q_store(INS.Qq).I := - 1;
   923.             end if;
   924.
   925.          when POS2_TO_Iq =>
   926.             the_CPU_delta := the_CPU_delta + 3;
   927.             if INS.Qq /= 0 then
   928.                the_Q_store(INS.Qq).I := + 2;
   929.             end if;
   930.
   931.          when NEG2_TO_Iq =>
   932.             the_CPU_delta := the_CPU_delta + 3;
   933.             if INS.Qq /= 0 then
   934.                the_Q_store(INS.Qq).I := - 2;
   935.             end if;
   936.
   937.          when CqTOQk =>
   938.             the_CPU_delta := the_CPU_delta + 4;
   939.             if INS.Qk /= 0 then
   940.                the_Q_store(INS.Qk).C := the_Q_store(INS.Qq).C;
   941.             end if;
   942.
   943.          when IqTOQk =>
   944.             the_CPU_delta := the_CPU_delta + 4;
   945.             if INS.Qk /= 0 then
   946.                the_Q_store(INS.Qk).I := the_Q_store(INS.Qq).I;
   947.             end if;
   948.
   949.          when MqTOQk =>
   950.             the_CPU_delta := the_CPU_delta + 4;
   951.             if INS.Qk /= 0 then
   952.                the_Q_store(INS.Qk).M := the_Q_store(INS.Qq).M;
   953.             end if;
   954.
   955.          when QqTOQk =>
   956.             the_CPU_delta := the_CPU_delta + 4;
   957.             if INS.Qk /= 0 then
   958.                the_Q_store(INS.Qk) := the_Q_store(INS.Qq);
   959.             end if;
   960.
   961.          when CIqTOQk =>
   962.            the_CPU_delta := the_CPU_delta + 4;
   963.            if INS.Qk /= 0 then
   964.               the_Q_store(INS.Qk).C := the_Q_store(INS.Qq).C;
   965.               the_Q_store(INS.Qk).I := the_Q_store(INS.Qq).I;
   966.            end if;
   967.
   968.          when IMqTOQk =>
   969.             the_CPU_delta := the_CPU_delta + 4;
   970.             if INS.Qk /= 0 then
   971.                the_Q_store(INS.Qk).I := the_Q_store(INS.Qq).I;
   972.                the_Q_store(INS.Qk).M := the_Q_store(INS.Qq).M;
   973.             end if;
   974.
   975.          when CMqTOQk =>
   976.             the_CPU_delta := the_CPU_delta + 4;
   977.             if INS.Qk /= 0 then
   978.                the_Q_store(INS.Qk).C := the_Q_store(INS.Qq).C;
   979.                the_Q_store(INS.Qk).M := the_Q_store(INS.Qq).M;
   980.             end if;
   981.
   982.          when QCIMq =>
   983.             ensure_that_the_nest_has_room_for_a_result;
   984.             if (INS.Qk and all_Q_choice) = all_Q_choice then -- Qq
   985.                push(as_word(the_Q_store(INS.Qq)));
   986.                the_CPU_delta := the_CPU_delta + 4;
   987.             elsif (INS.Qk and M_part_choice) /= 0 then       -- Mq
   988.                push(sign_extended(the_Q_store(INS.Qq).M));
   989.                the_CPU_delta := the_CPU_delta + 4;
   990.             elsif (INS.Qk and C_part_choice) /= 0 then       -- Cq
   991.                push(sign_extended(the_Q_store(INS.Qq).C));
   992.                the_CPU_delta := the_CPU_delta + 5;
   993.             elsif (INS.Qk and I_part_choice) /= 0 then       -- Iq
   994.                push(sign_extended(the_Q_store(INS.Qq).I));
   995.                the_CPU_delta := the_CPU_delta + 6;
   996.             else
   997.                trap_invalid_instruction;
   998.             end if;
   999.
  1000.          when TO_RCIMq =>
  1001.             ensure_that_the_nest_holds_an_operand;
  1002.             if (INS.Qk and all_Q_choice) = all_Q_choice then -- =Qq
  1003.                the_Q_store(INS.Qq) := as_Q(pop);
  1004.                the_CPU_delta := the_CPU_delta + 2;
  1005.             elsif (INS.Qk and M_part_choice) /= 0 then       -- =[R]Mq
  1006.                the_Q_store(INS.Qq).M := KDF9.Q_part(pop and Q_part_mask);
  1007.                if (INS.Qk and reset_choice) /= 0 then
  1008.                   the_Q_store(INS.Qq).C := 0;
  1009.                   the_Q_store(INS.Qq).I := 1;
  1010.                   the_CPU_delta := the_CPU_delta + 3;
  1011.                else
  1012.                   the_CPU_delta := the_CPU_delta + 2;
  1013.                end if;
  1014.             elsif (INS.Qk and C_part_choice) /= 0 then       -- =[R]Cq
  1015.                the_Q_store(INS.Qq).C := KDF9.Q_part(pop and Q_part_mask);
  1016.                if (INS.Qk and reset_choice) /= 0 then
  1017.                   the_Q_store(INS.Qq).I := 1;
  1018.                   the_Q_store(INS.Qq).M := 0;
  1019.                   the_CPU_delta := the_CPU_delta + 3;
  1020.                else
  1021.                   the_CPU_delta := the_CPU_delta + 2;
  1022.                end if;
  1023.             elsif (INS.Qk and I_part_choice) /= 0 then       -- =[R]Iq
  1024.                the_Q_store(INS.Qq).I := KDF9.Q_part(pop and Q_part_mask);
  1025.                if (INS.Qk and reset_choice) /= 0 then
  1026.                   the_Q_store(INS.Qq).C := 0;
  1027.                   the_Q_store(INS.Qq).M := 0;
  1028.                   the_CPU_delta := the_CPU_delta + 3;
  1029.                else
  1030.                   the_CPU_delta := the_CPU_delta + 2;
  1031.                end if;
  1032.             else
  1033.                trap_invalid_instruction;
  1034.             end if;
  1035.             ensure_that_Q0_contains_zero(suspect => INS.Qq);
  1036.
  1037.          when ADD_TO_QCIMq =>
  1038.             ensure_that_the_nest_has_room_for_a_result;
  1039.             ensure_that_the_nest_holds_an_operand;
  1040.
  1041.             -- Because the following does not push the Q operand on to the NEST,
  1042.             --   it will not leave an authentic bit pattern in the NEST core stack,
  1043.             --      in the event of a subsequent NOUV.
  1044.             -- I take this to be of no importance.
  1045.
  1046.             if (INS.Qk and all_Q_choice) = all_Q_choice then -- =+Qq
  1047.                the_Q_store(INS.Qq) := as_Q(as_word(the_Q_store(INS.Qq)) + pop);
  1048.                the_CPU_delta := the_CPU_delta + 5;
  1049.             elsif (INS.Qk and M_part_choice) /= 0 then       -- =+Mq
  1050.                the_Q_store(INS.Qq).M := KDF9.Q_part(Q_part_mask and
  1051.                                              (sign_extended(the_Q_store(INS.Qq).M) + pop));
  1052.                the_CPU_delta := the_CPU_delta + 5;
  1053.             elsif (INS.Qk and C_part_choice) /= 0 then       -- =+Cq
  1054.                the_Q_store(INS.Qq).C := KDF9.Q_part(Q_part_mask and
  1055.                                              (sign_extended(the_Q_store(INS.Qq).C) + pop));
  1056.                the_CPU_delta := the_CPU_delta + 6;
  1057.             elsif (INS.Qk and I_part_choice) /= 0 then       -- =+Iq
  1058.                the_Q_store(INS.Qq).I := KDF9.Q_part(Q_part_mask and
  1059.                                              (sign_extended(the_Q_store(INS.Qq).I) + pop));
  1060.                the_CPU_delta := the_CPU_delta + 7;
  1061.             else
  1062.                trap_invalid_instruction;
  1063.             end if;
  1064.             ensure_that_Q0_contains_zero(suspect => INS.Qq);
  1065.
  1066.          when SHA =>
  1067.             A := read_top;
  1068.             write_top(KDF9.word'(shift_arithmetic(A, shift_count)));
  1069.             the_CPU_delta := the_CPU_delta + 2 + shift_time(Natural(abs shift_count));
  1070.
  1071.          when SHAD =>
  1072.             AB := read_top;
  1073.             write_top(KDF9.pair'(shift_arithmetic(AB, shift_count)));
  1074.             the_CPU_delta := the_CPU_delta + 2 + shift_time(Natural(abs shift_count));
  1075.
  1076.          when MACC =>
  1077.             ensure_that_the_nest_holds(at_least => 4);
  1078.             AB := pop;
  1079.             AB := AB.msw * AB.lsw;
  1080.             CD := read_top;
  1081.             write_top(CD + shift_arithmetic(AB, shift_count));
  1082.             the_CPU_delta := the_CPU_delta + 15 + shift_time(Natural(abs shift_count));
  1083.
  1084.          when SHL =>
  1085.             write_top(KDF9.word'(shift_logical(read_top, shift_count)));
  1086.             the_CPU_delta := the_CPU_delta + 2 + shift_time(Natural(abs shift_count));
  1087.
  1088.          when SHLD =>
  1089.             write_top(KDF9.pair'(shift_logical(read_top, shift_count)));
  1090.             the_CPU_delta := the_CPU_delta + 2 + shift_time(Natural(abs shift_count));
  1091.
  1092.          when SHC =>
  1093.             write_top(shift_circular(read_top, shift_count));
  1094.             the_CPU_delta := the_CPU_delta + 3 + shift_time(Natural(abs shift_count));
  1095.
  1096.          when TO_Kq =>
  1097.             fail_in_problem_program_state;
  1098.             case INS.Qq is
  1099.                when K0 =>
  1100.                   if read_top /= all_zero_bits then
  1101.                      for w in all_zero_bits .. read_top mod 8 loop
  1102.                         POSIX.output_line("BLEEP!");
  1103.                      end loop;
  1104.                      delay 10.0;
  1105.                   end if;
  1106.                when K1 =>
  1107.                   set_K1_register(read_top);
  1108.                when K2 =>
  1109.                   set_K2_register(read_top);
  1110.                when K3 =>
  1111.                   set_K3_register(read_top);
  1112.                when others =>
  1113.                   trap_invalid_instruction;
  1114.             end case;
  1115.             the_CPU_delta := the_CPU_delta + 3;
  1116.
  1117.          when Kk =>
  1118.             fail_in_problem_program_state;
  1119.             ensure_that_the_nest_has_room_for_a_result;
  1120.             case INS.Qk is
  1121.                when K4 =>
  1122.                   push(get_K4_operand);
  1123.                   the_RFIR := (others => False);
  1124.                when K5 =>
  1125.                   push(get_K5_operand);
  1126.                when K7 =>
  1127.                   push(get_K7_operand);
  1128.                when others =>
  1129.                   trap_invalid_instruction;
  1130.             end case;
  1131.             the_CPU_delta := the_CPU_delta + 3;
  1132.
  1133.          when LINK =>
  1134.             if the_CPU_state = Director_state and the_sjns_depth = 0 then -- clear out JB
  1135.                push(all_zero_bits);
  1136.                the_sjns_depth := 0 - 1;
  1137.             else
  1138.                ensure_that_the_nest_has_room_for_a_result;
  1139.                ensure_that_the_sjns_is_not_empty;
  1140.                push(as_word(KDF9.sjns_link(KDF9.syllable_address'(pop))));
  1141.             end if;
  1142.             the_CPU_delta := the_CPU_delta + 4;
  1143.
  1144.          when TO_LINK =>
  1145.             ensure_that_the_sjns_is_not_full;
  1146.             ensure_that_the_nest_holds_an_operand;
  1147.             push(KDF9.syllable_address(as_link(pop)));
  1148.             the_CPU_delta := the_CPU_delta + 3;
  1149.
  1150.          when others =>
  1151.             do_an_IO_order;
  1152.
  1153.       end case;
  1154.    end do_a_two_syllable_order;
  1155.
  1156.    procedure do_a_jump_order is
  1157.       RA        : KDF9.syllable_address;
  1158.       A         : KDF9.word;
  1159.    begin
  1160.       fetching_normally := True;
  1161.
  1162.       case INS.compressed_opcode is
  1163.
  1164.          when Jr =>
  1165.             set_NIA_to_the_INS_target_address;
  1166.             the_CPU_delta := the_CPU_delta + 8;
  1167.
  1168.          when JSr =>
  1169.             if the_sjns_depth < 16 or else the_CPU_state = Director_state  then
  1170.                push(CIA);
  1171.                set_NIA_to_the_INS_target_address;
  1172.                the_CPU_delta := the_CPU_delta + 11;
  1173.             else
  1174.                effect(NOUV_interrupt, "full SJNS");
  1175.             end if;
  1176.
  1177.          when JrEQ =>
  1178.             if the_nest_depth >= 2 or else the_CPU_state = Director_state then
  1179.                A := pop;
  1180.                if A = read_top then
  1181.                   set_NIA_to_the_INS_target_address;
  1182.                   the_CPU_delta := the_CPU_delta + 12;
  1183.                else
  1184.                   the_CPU_delta := the_CPU_delta + 5;
  1185.                end if;
  1186.             else
  1187.                effect(NOUV_interrupt, words_needed(need => 2-the_nest_depth));
  1188.             end if;
  1189.
  1190.          when JrNE =>
  1191.             if the_nest_depth >= 2 or else the_CPU_state = Director_state then
  1192.                A := pop;
  1193.                if A /= read_top then
  1194.                   set_NIA_to_the_INS_target_address;
  1195.                   the_CPU_delta := the_CPU_delta + 12;
  1196.                else
  1197.                   the_CPU_delta := the_CPU_delta + 5;
  1198.                end if;
  1199.             else
  1200.                effect(NOUV_interrupt, words_needed(need => 2-the_nest_depth));
  1201.             end if;
  1202.
  1203.          when JrGTZ =>
  1204.             if the_nest_depth >= 1 or else the_CPU_state = Director_state then
  1205.                if resign(pop) > 0 then
  1206.                   set_NIA_to_the_INS_target_address;
  1207.                   the_CPU_delta := the_CPU_delta + 11;
  1208.                else
  1209.                   the_CPU_delta := the_CPU_delta + 4;
  1210.                end if;
  1211.             else
  1212.                effect(NOUV_interrupt, "empty NEST");
  1213.             end if;
  1214.
  1215.          when JrLTZ =>
  1216.             if the_nest_depth >= 1 or else the_CPU_state = Director_state then
  1217.                A := pop;
  1218.                if resign(A) < 0 then
  1219.                   set_NIA_to_the_INS_target_address;
  1220.                   the_CPU_delta := the_CPU_delta + 11;
  1221.                else
  1222.                   the_CPU_delta := the_CPU_delta + 4;
  1223.                end if;
  1224.             else
  1225.                effect(NOUV_interrupt, "empty NEST");
  1226.             end if;
  1227.
  1228.          when JrEQZ =>
  1229.             if the_nest_depth >= 1 or else the_CPU_state = Director_state then
  1230.                 if pop = all_zero_bits then
  1231.                   set_NIA_to_the_INS_target_address;
  1232.                   the_CPU_delta := the_CPU_delta + 11;
  1233.                else
  1234.                   the_CPU_delta := the_CPU_delta + 4;
  1235.                end if;
  1236.             else
  1237.                effect(NOUV_interrupt, "empty NEST");
  1238.             end if;
  1239.
  1240.          when JrLEZ =>
  1241.             if the_nest_depth >= 1 or else the_CPU_state = Director_state then
  1242.                if resign(pop) <= 0 then
  1243.                   set_NIA_to_the_INS_target_address;
  1244.                   the_CPU_delta := the_CPU_delta + 11;
  1245.                else
  1246.                   the_CPU_delta := the_CPU_delta + 4;
  1247.                end if;
  1248.             else
  1249.                effect(NOUV_interrupt, "empty NEST");
  1250.             end if;
  1251.
  1252.          when JrGEZ =>
  1253.             if the_nest_depth >= 1 or else the_CPU_state = Director_state then
  1254.                if resign(pop) >= 0 then
  1255.                   set_NIA_to_the_INS_target_address;
  1256.                   the_CPU_delta := the_CPU_delta + 11;
  1257.                else
  1258.                   the_CPU_delta := the_CPU_delta + 4;
  1259.                end if;
  1260.             else
  1261.                effect(NOUV_interrupt, "empty NEST");
  1262.             end if;
  1263.
  1264.          when JrNEZ =>
  1265.             if the_nest_depth >= 1 or else the_CPU_state = Director_state then
  1266.                if pop /= all_zero_bits then
  1267.                   set_NIA_to_the_INS_target_address;
  1268.                   the_CPU_delta := the_CPU_delta + 11;
  1269.                else
  1270.                   the_CPU_delta := the_CPU_delta + 4;
  1271.                end if;
  1272.             else
  1273.                effect(NOUV_interrupt, "empty NEST");
  1274.             end if;
  1275.
  1276.          when JrV =>
  1277.             if the_V_bit_is_set then
  1278.                the_V_bit_is_set := False;
  1279.                set_NIA_to_the_INS_target_address;
  1280.                the_CPU_delta := the_CPU_delta + 10;
  1281.             else
  1282.                the_CPU_delta := the_CPU_delta + 3;
  1283.             end if;
  1284.
  1285.          when JrNV =>
  1286.             if the_V_bit_is_set then
  1287.                the_V_bit_is_set := False;
  1288.                the_CPU_delta := the_CPU_delta + 3;
  1289.             else
  1290.                set_NIA_to_the_INS_target_address;
  1291.                the_CPU_delta := the_CPU_delta + 10;
  1292.             end if;
  1293.
  1294.          when JrEN =>
  1295.             the_trace_operand := KDF9.word(the_nest_depth);
  1296.             if the_nest_depth = 0 then
  1297.                set_NIA_to_the_INS_target_address;
  1298.                the_CPU_delta := the_CPU_delta + 10;
  1299.             else
  1300.                the_CPU_delta := the_CPU_delta + 3;
  1301.             end if;
  1302.
  1303.          when JrNEN =>
  1304.             the_trace_operand := KDF9.word(the_nest_depth);
  1305.             if the_nest_depth /= 0 then
  1306.                set_NIA_to_the_INS_target_address;
  1307.                the_CPU_delta := the_CPU_delta + 10;
  1308.             else
  1309.                the_CPU_delta := the_CPU_delta + 3;
  1310.             end if;
  1311.
  1312.          when JrEJ =>
  1313.             the_trace_operand := KDF9.word(the_sjns_depth);
  1314.             if the_sjns_depth = 0 then
  1315.                set_NIA_to_the_INS_target_address;
  1316.                the_CPU_delta := the_CPU_delta + 10;
  1317.             end if;
  1318.             the_CPU_delta := the_CPU_delta + 3;
  1319.
  1320.          when JrNEJ =>
  1321.             the_trace_operand := KDF9.word(the_sjns_depth);
  1322.             if the_sjns_depth /= 0 then
  1323.                set_NIA_to_the_INS_target_address;
  1324.                the_CPU_delta := the_CPU_delta + 10;
  1325.             end if;
  1326.             the_CPU_delta := the_CPU_delta + 3;
  1327.
  1328.          when JrTR =>
  1329.             if the_T_bit_is_set then
  1330.                the_T_bit_is_set := False;
  1331.                set_NIA_to_the_INS_target_address;
  1332.                the_CPU_delta := the_CPU_delta + 10;
  1333.             else
  1334.                the_CPU_delta := the_CPU_delta + 3;
  1335.             end if;
  1336.
  1337.          when JrNTR =>
  1338.             if the_T_bit_is_set then
  1339.                the_T_bit_is_set := False;
  1340.                the_CPU_delta := the_CPU_delta + 3;
  1341.             else
  1342.                set_NIA_to_the_INS_target_address;
  1343.                the_CPU_delta := the_CPU_delta + 10;
  1344.             end if;
  1345.
  1346.          when EXIT_n =>
  1347.             if the_sjns_depth > 0 or else the_CPU_state = Director_state then
  1348.                RA := pop;
  1349.                if INS.target.syllable_index = 3 then  -- c.f. decode_a_jump_order.
  1350.                   increment_by_3(RA);
  1351.                end if;
  1352.                RA.order_word_number := RA.order_word_number+INS.target.order_word_number;
  1353.                set_NIA_to(RA);
  1354.                the_CPU_delta := the_CPU_delta + 12 + KDF9.us(INS.target.syllable_index mod 2);
  1355.             else
  1356.                effect(NOUV_interrupt, "empty SJNS");
  1357.             end if;
  1358.
  1359.          when EXITD =>
  1360.             fail_in_problem_program_state;
  1361.             if the_sjns_depth = 0 then
  1362.                -- This indicates a serious failure in Director; best to abandon it at once.
  1363.                trap_invalid_instruction("empty SJNS in Director");
  1364.             end if;
  1365.             RA := pop;
  1366.             the_CPU_delta := the_CPU_delta + 11;
  1367.             return_from_Director_to(RA);


  1368.
  1369.          when JrCqZ =>
  1370.             if the_Q_store(INS.Qq).C = 0 then
  1371.                set_NIA_to_the_INS_target_address;
  1372.                the_CPU_delta := the_CPU_delta + 11;
  1373.             else
  1374.                the_CPU_delta := the_CPU_delta + 4;
  1375.             end if;
  1376.
  1377.          when JrCqNZ =>
  1378.             if the_Q_store(INS.Qq).C /= 0 then
  1379.                set_NIA_to_the_INS_target_address;
  1380.                the_CPU_delta := the_CPU_delta + 11;
Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/kdf9.adb
Source file time stamp: 2020-11-03 00:59:53
Compiled at: 2020-11-12 18:12:11

  1381.             else
  1382.                the_CPU_delta := the_CPU_delta + 4;
  1383.             end if;
  1384.
  1385.          when OS_OUT =>
  1386.             the_CPU_delta := the_CPU_delta + 13;
  1387.             if the_sjns_depth < 16 or else the_CPU_state = Director_state then
  1388.                A := (if the_nest_depth = 0 then 0 else read_top);
  1389.                if the_execution_mode = boot_mode then
  1390.                   effect(OUT_interrupt, A'Image);
  1391.                   -- We get here only in Director state, when the OUT does not interrupt.
  1392.                   -- Arguably, this should be notified as an error.
  1393.                   return; -- OUT has the effect of a no-op in Director state.
  1394.                end if;
  1395.                -- Emulate a subset of the appropriate Director's API.
  1396.                if A <= 99 then
  1397.                   do_a_TSD_OUT(OUT_number => A);
  1398.                elsif A <= 199 then
  1399.                   do_an_EGDON_OUT(OUT_number => A);
  1400.                else
  1401.                   -- Other Directors are not handled yet.
  1402.                   trap_invalid_operand("invalid OUT number");
  1403.                end if;
  1404.             else
     1. -- kdf9.adb
     2. --
     3. -- The machine-state manipulations used by the CPU microcode.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Unchecked_Conversion;
    20. --
    21. with exceptions;
    22. with KDF9.decoding;
    23. with KDF9.CPU;
    24. with KDF9.PHU_store;
    25. with KDF9.store;
    26. with settings;
    27. with tracing;
    28.
    29. use  exceptions;
    30. use  KDF9.decoding;
    31. use  KDF9.CPU;
    32. use  KDF9.PHU_store;
    33. use  KDF9.store;
    34. use  settings;
    35. use  tracing;
    36.
    37. package body KDF9 is
    38.
    39.    C_part_scale : constant := 2**32;
    40.    I_part_scale : constant := 2**16;
    41.
    42.    function as_Q (the_word : KDF9.word)
    43.    return KDF9.Q_register
  1405.                effect(NOUV_interrupt, "full SJNS in OUT");
  1406.             end if;
  1407.
  1408.          when others =>
  1409.             trap_invalid_instruction;
  1410.
  1411.       end case;
  1412.    end do_a_jump_order;
  1413.
  1414.    procedure do_a_data_access_order is
  1415.    begin
  1416.       case INS.compressed_opcode is
  1417.
  1418.          when EaMq =>
  1419.             the_trace_address := valid_word_address(the_Q_store(INS.Qq).M, INS.operand);
  1420.             check_address_and_lockout(the_trace_address);
  1421.             ensure_that_the_nest_has_room_for_a_result;
  1422.             the_trace_operand := fetch_word(the_trace_address);
  1423.             push(the_trace_operand);
  1424.             the_CPU_delta := the_CPU_delta + 6;
  1425.
  1426.          when TO_EaMq =>
  1427.             the_trace_address := valid_word_address(the_Q_store(INS.Qq).M, INS.operand);
  1428.             check_address_and_lockout(the_trace_address);
  1429.             ensure_that_the_nest_holds_an_operand;
  1430.             the_trace_operand := pop;
  1431.             store_word(the_trace_operand, the_trace_address);
  1432.             the_CPU_delta := the_CPU_delta + 6;
  1433.
  1434.          when EaMqQ =>
  1435.             the_trace_address := valid_word_address(the_Q_store(INS.Qq).M, INS.operand);
  1436.             check_address_and_lockout(the_trace_address);
  1437.             ensure_that_the_nest_has_room_for_a_result;
  1438.             the_trace_operand := fetch_word(the_trace_address);
    44.    is (
    45.        (C => KDF9.Q_part(KDF9.word'(the_word / C_part_scale)),
    46.         I => KDF9.Q_part(KDF9.word'(the_word / I_part_scale) and Q_part_mask),
    47.         M => KDF9.Q_part(the_word and Q_part_mask)
    48.        )
    49.       );
    50.
    51.    function as_word (the_Q : KDF9.Q_register)
    52.    return KDF9.word
    53.    is (KDF9.word(the_Q.C)*C_part_scale + KDF9.word(the_Q.I)*I_part_scale + KDF9.word(the_Q.M));
    54.
    55.    function sign_extended (Q : KDF9.Q_part)
    56.    return KDF9.word
    57.    is (unsign(CPU.signed(resign(Q))));
    58.
    59.    function as_word (the_link : KDF9.sjns_link)
    60.    return KDF9.word is
    61.       function link_Q_part is new Ada.Unchecked_Conversion(KDF9.sjns_link, KDF9.Q_part);
    62.    begin
    63.       return KDF9.word(link_Q_part(the_link));
    64.    end as_word;
    65.
    66.    function as_link (the_word : KDF9.word)
    67.    return KDF9.sjns_link is
    68.       function Q_part_link is new Ada.Unchecked_Conversion(KDF9.Q_part, KDF9.sjns_link);
    69.    begin
    70.       return Q_part_link(KDF9.Q_part(the_word and Q_part_mask));
    71.    end as_link;
    72.
    73.    procedure ensure_that_the_sjns_is_not_empty is
    74.    begin
    75.       if the_sjns_depth > 0             or else
    76.             the_CPU_state = Director_state then
    77.          return;
    78.       end if;
    79.       effect(NOUV_interrupt, "empty SJNS");
    80.    end ensure_that_the_sjns_is_not_empty;
    81.
    82.    procedure ensure_that_the_sjns_is_not_full is
    83.    begin
    84.       if the_sjns_depth < 16             or else
    85.             the_CPU_state = Director_state  then
    86.          return;
    87.       end if;
    88.       effect(NOUV_interrupt, "full SJNS");
    89.    end ensure_that_the_sjns_is_not_full;
    90.
    91.    procedure push (the_link : in KDF9.syllable_address) is
    92.    begin
    93.       the_sjns(the_sjns_depth) := KDF9.sjns_link(the_link);
    94.       the_sjns_depth := the_sjns_depth + 1;
    95.    end push;
    96.
    97.    function pop
    98.    return KDF9.syllable_address is
    99.    begin
   100.       the_sjns_depth := the_sjns_depth - 1;
   101.       return KDF9.syllable_address(the_sjns(the_sjns_depth));
   102.    end pop;
   103.
   104.    function sjns_top
   105.    return KDF9.sjns_link
   106.    is (the_sjns(the_sjns_depth-1));
   107.
   108.    function words_needed (need : KDF9.nest_depth)
   109.    return String
   110.    is ("NEST lacks" & need'Image & " operand" & (if need > 1 then "s" else ""));
   111.
   112.    procedure ensure_that_the_nest_holds (at_least : in KDF9.nest_depth) is
   113.    begin
   114.       if the_nest_depth >= at_least          or else
   115.             the_CPU_state = Director_state      then
   116.          return;
   117.       end if;
   118.       effect(NOUV_interrupt, words_needed(need => at_least-the_nest_depth));
   119.    end ensure_that_the_nest_holds;
   120.
   121.    procedure ensure_that_the_nest_holds_an_operand is
   122.    begin
   123.       ensure_that_the_nest_holds (at_least => 1);
   124.    end ensure_that_the_nest_holds_an_operand;
   125.
   126.    procedure ensure_that_the_nest_holds_2_operands is
   127.    begin
   128.       ensure_that_the_nest_holds (at_least => 2);
   129.    end ensure_that_the_nest_holds_2_operands;
   130.
   131.    function space_needed (need : KDF9.nest_depth)
   132.    return String
   133.    is (if need = 1 then "full NEST" else "NEST too full for" & need'Image & " operands");
   134.
   135.    procedure ensure_that_the_nest_has_room_for (at_least : in KDF9.nest_depth) is
   136.    begin
   137.       if the_nest_depth <= 16-at_least     or else
   138.             the_CPU_state = Director_state    then
   139.          return;
   140.       end if;
   141.       effect(NOUV_interrupt, space_needed(need => at_least - (16-the_nest_depth)));
   142.    end ensure_that_the_nest_has_room_for;
   143.
   144.    procedure ensure_that_the_nest_has_room_for_a_result is
   145.    begin
   146.       ensure_that_the_nest_has_room_for (at_least => 1);
   147.    end ensure_that_the_nest_has_room_for_a_result;
   148.
   149.    procedure ensure_that_the_nest_has_room_for_2_results is
   150.    begin
   151.       ensure_that_the_nest_has_room_for (at_least => 2);
   152.    end ensure_that_the_nest_has_room_for_2_results;
   153.
   154.    procedure push (the_word : in KDF9.word) is
   155.    begin
   156.       the_nest(the_nest_depth) := the_word;
   157.       the_nest_depth := the_nest_depth + 1;
   158.    end push;
   159.
   160.    function pop
   161.    return KDF9.word is
   162.    begin
   163.       return result : constant KDF9.word := the_nest(the_nest_depth - 1) do
   164.          the_nest(the_nest_depth - 1) := 0;
   165.          the_nest_depth := the_nest_depth - 1;
   166.       end return;
   167.    end pop;
   168.
   169.    procedure pop is
   170.    begin
   171.       the_nest(the_nest_depth - 1) := 0;
   172.       the_nest_depth := the_nest_depth - 1;
   173.    end pop;
   174.
   175.    function read_top
   176.    return KDF9.word
   177.    is (the_nest(the_nest_depth-1));
   178.
   179.    procedure write_top (the_word : in KDF9.word) is
   180.    begin
   181.       the_nest(the_nest_depth-1) := the_word;
   182.    end write_top;
   183.
   184.    procedure push (the_pair : in KDF9.pair) is
   185.    begin
   186.       the_nest(the_nest_depth+0) := the_pair.lsw;
   187.       the_nest(the_nest_depth+1) := the_pair.msw;
   188.       the_nest_depth := the_nest_depth + 2;
   189.    end push;
   190.
   191.    function pop
   192.    return KDF9.pair is
   193.    begin
   194.       return result : constant KDF9.pair := (msw => the_nest(the_nest_depth-1),
   195.                                              lsw => the_nest(the_nest_depth-2)) do
   196.          the_nest(the_nest_depth-1) := 0;
   197.          the_nest(the_nest_depth-2) := 0;
   198.          the_nest_depth := the_nest_depth - 2;
   199.       end return;
   200.    end pop;
   201.
   202.    function read_top
   203.    return KDF9.pair
   204.    is ((msw => the_nest(the_nest_depth-1), lsw => the_nest(the_nest_depth-2)));
   205.
   206.    procedure write_top (the_pair : in KDF9.pair) is
   207.    begin
   208.       the_nest(the_nest_depth-1) := the_pair.msw;
   209.       the_nest(the_nest_depth-2) := the_pair.lsw;
   210.    end write_top;
   211.
   212.
   213. --
   214.    -- Support for Director-only operations.
   215. --
   216.
   217.    -- Set BA (bits D38:47), CPL (D34:35) and NOL (D24:33).
   218.
   219.    procedure set_K1_register (setting : in KDF9.word) is
   220.    begin
   221.       BA  := KDF9.address(setting mod 2**10) * 2**5;
   222.       CPL := KDF9.priority((setting / 2**12) and 2#11#);
   223.       NOL := KDF9.address((setting / 2**14) mod 2**10) * 2**5 + 31;
   224.    end set_K1_register;
   225.
   226.    -- Set CPDAR (bits D32:47).
   227.
   228.    procedure set_K2_register (setting : in KDF9.word) is
   229.       CPDAR_Q : KDF9.Q_part := as_Q(setting).M;
   230.    begin
   231.       for i in KDF9.buffer_number loop
   232.          the_CPDAR(i) := (CPDAR_Q mod 2) = 1;
   233.          CPDAR_Q := CPDAR_Q / 2;
   234.       end loop;
   235.    end set_K2_register;
   236.
   237.    -- Set context (bits D0:1), nest_depth (D2:6) and sjns_depth (D7:11).
   238.
   239.    procedure set_K3_register (setting : in KDF9.word) is
   240.    begin
   241.       -- Save the current register values in the register bank.
   242.       register_bank(the_context).NEST := the_nest;
   243.       register_bank(the_context).SJNS := the_sjns;
   244.       register_bank(the_context).Q_store := the_Q_store;
   245.       -- Set the new context.
   246.       the_context := KDF9.context(KDF9.word'(setting / 2**46));
   247.       the_nest_depth := KDF9.nest_depth(setting / 2**41 mod 2**5);
   248.       the_sjns_depth := KDF9.sjns_depth(setting / 2**36 mod 2**5);
   249.       -- Restore the register values for the new context.
   250.       the_nest := register_bank(the_context).NEST;
   251.       the_sjns := register_bank(the_context).SJNS;
   252.       the_Q_store := register_bank(the_context).Q_store;
   253.    end set_K3_register;
   254.
   255.    a_jiffy : constant := 1.0 / 2.0**20;  -- a bit less than a microsecond
   256.
   257.    type seconds is delta a_jiffy range 0.0 .. 1000.0*366.0*24.0*3600.0;  -- 1000 leap years!
   258.
   259.    procedure update_the_elapsed_time;
   260.
   261.    -- Let the real elapsed time catch up with the_real_time virtual seconds.
   262.
   263.    procedure delay_until (the_real_time : in KDF9.us) is
   264.       quantum : constant seconds := seconds(2**10) * a_jiffy;  -- ca. TR character-read time of 1ms
   265.       the_lag : seconds;
   266.    begin
   267.        if the_real_time < the_last_delay_time then
   268.           the_last_delay_time := the_real_time;
   269.        end if;
   270.       the_lag := seconds(the_real_time - the_last_delay_time) * a_jiffy;
   271.       if the_lag >= quantum then  -- More than a quantum of virtual elapsed time has passed.
   272.          delay Duration(the_lag);
   273.          the_last_delay_time := the_real_time;
   274.       end if;
   275.      -- the_elapsed_time := the_real_time;
   276.       update_the_elapsed_time;
   277.    end delay_until;
   278.
   279.    procedure delay_by (the_delay_time : in KDF9.us) is
   280.    begin
   281.       if authentic_timing_is_enabled then
   282.          delay_until(the_clock_time + the_delay_time);
   283.       end if;
   284.    end delay_by;
   285.
   286.    -- Advance to the larger of the_CPU_time, the_elapsed_time, and the_last_delay_time.
   287.    -- Cap the increase to prevent a spurious double-clock (RESET) interrupt in Director.
   288.
   289.    procedure update_the_elapsed_time is
   290.       max_elapsed_time : constant KDF9.us := the_last_K4_time + 2**20 - 1;
   291.    begin
   292.       the_elapsed_time := KDF9.us'Max(the_elapsed_time, the_last_delay_time);
   293.       the_elapsed_time := KDF9.us'Max(the_elapsed_time, the_CPU_time);
   294.       if the_execution_mode = boot_mode and the_CPU_state = Director_state then
   295.          the_elapsed_time := KDF9.us'Min(the_elapsed_time, max_elapsed_time);
   296.       end if;
   297.    end update_the_elapsed_time;
   298.
   299.    -- The virtual elapsed time.
   300.
   301.    function the_clock_time
   302.    return KDF9.us is
   303.    begin
   304.       update_the_elapsed_time;
   305.       return the_elapsed_time;
   306.    end the_clock_time;
   307.
   308.    procedure advance_the_clock (past : in KDF9.us) is
   309.    begin
   310.       the_elapsed_time := KDF9.us'Max(the_elapsed_time, past);
   311.       update_the_elapsed_time;
   312.       if authentic_timing_is_enabled then
   313.          delay_until(the_elapsed_time);
   314.       end if;
   315.    end advance_the_clock;
   316.
   317.    procedure synchronize_the_real_and_virtual_times is
   318.    begin
   319.       if authentic_timing_is_enabled then
   320.          update_the_elapsed_time;
   321.          delay_until(the_elapsed_time);
   322.       end if;
   323.    end synchronize_the_real_and_virtual_times;
   324.
   325.    -- Get clock (bits D0:15) and RFIR (D16:31).
   326.
   327.    function get_K4_operand
   328.    return KDF9.word is
   329.
   330.       function RFIR_in_a_word
   331.       return KDF9.word is
   332.          result : KDF9.word := 0;
   333.       begin
   334.          for r of the_RFIR loop
   335.             result := result*2;
   336.             if r then
   337.                result := result or 1;
   338.             end if;
   339.          end loop;
   340.          return result;
   341.       end RFIR_in_a_word;
   342.
   343.       -- The KDF9's interval timing clock ticks once per 32 µs;
   344.       --    the emulator virtual time has a resolution of 1 µs.
   345.
   346.       time_now : constant KDF9.us := the_clock_time;
   347.       interval : constant KDF9.us := (time_now - the_last_K4_time);
   348.
   349.    begin
   350.       the_last_K4_time := time_now;
   351.       if interval / 32 >= 2**16 then
   352.          effect(RESET_interrupt, "double clock");
   353.          the_RFIR(RESET_interrupt) := True;
   354.       elsif interval / 32 >= 2**15 then
   355.          effect(CLOCK_interrupt, "time since a K4" & interval'Image & "us");
   356.          the_RFIR(CLOCK_interrupt) := True;  --?? why is this needed?
   357.       end if;
   358.       return (KDF9.word(interval / 32) * 2**32) or (RFIR_in_a_word * 2**16);
   359.    end get_K4_operand;
   360.
   361.    -- Get PHUi (bits D6i:6i+5, i = 0 .. 3).
   362.
   363.    function get_K5_operand
   364.    return KDF9.word
   365.    is (K5_operand);
   366.
   367.    -- Get context (bits D0:1), nest_depth (D2:6) and sjns_depth (D7:11).
   368.
   369.    function get_K7_operand
   370.    return KDF9.word
   371.    is (
   372.        (KDF9.word(the_context)    * 2**46) or
   373.        (KDF9.word(the_nest_depth) * 2**41) or
   374.        (KDF9.word(the_sjns_depth) * 2**36)
   375.       );
   376.
  1439.             push(the_trace_operand);
  1440.             auto_increment;
  1441.             the_CPU_delta := the_CPU_delta + 7;
  1442.
  1443.          when TO_EaMqQ =>
  1444.             the_trace_address := valid_word_address(the_Q_store(INS.Qq).M, INS.operand);
  1445.             check_address_and_lockout(the_trace_address);
  1446.             ensure_that_the_nest_holds_an_operand;
  1447.             the_trace_operand := pop;
  1448.             store_word(the_trace_operand, the_trace_address);
  1449.             auto_increment;
  1450.             the_CPU_delta := the_CPU_delta + 7;
  1451.
  1452.          when SET =>
  1453.             ensure_that_the_nest_has_room_for_a_result;
  1454.             the_trace_operand := sign_extended(INS.operand);
  1455.             push(the_trace_operand);
  1456.             the_CPU_delta := the_CPU_delta + 4;
  1457.
  1458.          when others =>
  1459.             trap_invalid_instruction;
  1460.
  1461.       end case;
  1462.    end do_a_data_access_order;
  1463.
  1464.    procedure update_the_virtual_clocks
  1465.       with Inline;
  1466.
  1467.    procedure update_the_virtual_clocks is
  1468.    begin
  1469.       the_CPU_time := the_CPU_time + the_CPU_delta;
  1470.       the_elapsed_time := the_elapsed_time + the_CPU_delta;
  1471.       if the_CPU_time > the_elapsed_time then
  1472.          the_elapsed_time := the_CPU_time;
  1473.       end if;
  1474.       ICR := ICR + 1;
  1475.    end update_the_virtual_clocks;
  1476.
  1477.    procedure do_a_fast_time_slice is
  1478.    begin
  1479.
  1480.       if break_in.has_been_requested then
  1481.          break_in.handler;
  1482.       end if;
  1483.
  1484.       for i in 1 .. time_slice loop
  1485.
  1486.          the_CPU_delta := 0;
  1487.
  1488.          process_syllable_0_of_INS;
  1489.          case INS.kind is
  1490.             when one_syllable_order =>
  1491.                do_a_one_syllable_order;
  1492.             when two_syllable_order =>
  1493.                process_syllable_1_of_INS;
  1494.                do_a_two_syllable_order;
  1495.             when normal_jump_order =>
  1496.                process_syllables_1_and_2_of_a_jump_order;
  1497.                do_a_jump_order;
  1498.             when data_access_order =>
  1499.                process_syllables_1_and_2_of_a_data_access_order;
  1500.                do_a_data_access_order;
  1501.          end case;
  1502.
  1503.          update_the_virtual_clocks;
  1504.          check_for_a_clock_interrupt;
  1505.          if the_elapsed_time > the_next_interrupt_time then
  1506.             act_on_pending_interrupts;
  1507.          end if;
  1508.
  1509.       end loop;
  1510.
  1511.    exception
  1512.
  1513.       when program_exit =>
  1514.          complete_all_extant_transfers;
  1515.          update_the_virtual_clocks;
  1516.          synchronize_the_real_and_virtual_times;
  1517.          raise;
  1518.
  1519.       -- These traps represent interrupts that are handled by Director.
  1520.       -- Other traps pass through, to preserve their diagnostic message.
  1521.
  1522.       when PR_trap =>
  1523.          effect(PR_interrupt);
  1524.
  1525.       when FLEX_trap =>
  1526.          effect(FLEX_interrupt);
  1527.
  1528.       when EDT_trap =>
  1529.          effect(EDT_interrupt);
  1530.
  1531.       when OUT_trap =>
  1532.          effect(OUT_interrupt);
  1533.
  1534.       when LOV_trap =>
  1535.          effect(LOV_interrupt);
  1536.
  1537.    end do_a_fast_time_slice;
  1538.
  1539.    procedure do_a_traced_instruction_cycle is
  1540.       use tracing.order_flags;
  1541.
  1542.       procedure finalize_the_traced_instruction_execution is
  1543.       begin
  1544.          update_the_virtual_clocks;
  1545.          synchronize_the_real_and_virtual_times;
  1546.
  1547.          if ICR in low_count .. high_count            and then
  1548.                NIA_word_number in low_bound .. high_bound then
  1549.             take_note_of(the_trace_operand);
  1550.             if the_signature_is_enabled then
  1551.                update_the_digital_signature;
  1552.             end if;
  1553.             if the_histogram_is_enabled then
  1554.                add_INS_to_the_histogram;
  1555.                add_CIA_to_the_profile;
  1556.             end if;
  1557.             if the_external_trace_is_enabled then
  1558.                log_to_external_trace;
  1559.             end if;
  1560.             case INS.kind is
  1561.                when two_syllable_order =>
  1562.                   act_on_any_two_syllable_order_watchpoints;
  1563.                when data_access_order =>
  1564.                   act_on_any_data_access_order_watchpoints;
  1565.                when others =>
  1566.                   null;
  1567.             end case;
  1568.          end if;
  1569.       end finalize_the_traced_instruction_execution;
  1570.
  1571.    begin  -- do_a_traced_instruction_cycle
  1572.
  1573.       if break_in.has_been_requested then
  1574.          break_in.handler;
  1575.       end if;
  1576.
  1577.       the_trace_operand := 0;
  1578.       the_trace_address := 0;
  1579.       the_CPU_delta := 0;
  1580.
  1581.       process_syllable_0_of_INS;
  1582.
  1583.       case INS.kind is
  1584.          when one_syllable_order =>
  1585.             preview_a_one_syllable_order;
  1586.                do_a_one_syllable_order;
  1587.             look_back_at_a_one_syllable_order;
  1588.          when two_syllable_order =>
  1589.             process_syllable_1_of_INS;
  1590.             preview_a_two_syllable_order;
  1591.                do_a_two_syllable_order;
  1592.             look_back_at_a_two_syllable_order;
  1593.          when normal_jump_order =>
  1594.             process_syllables_1_and_2_of_a_jump_order;
   377.    procedure reset_the_internal_registers (the_new_state : in CPU_state) is
   378.    begin
   379.       -- Set the state of a newly bootstrapped CPU.
   380.       the_V_bit_is_set := False;
   381.       the_T_bit_is_set := False;
   382.       CIA := (0, 0);
   383.       CPL := 0;
   384.       BA  := 0;
   385.       NOL := max_address;
   386.       the_RFIR := (others => False);
   387.       ICR := 0;
   388.       the_CPU_time := 0;
   389.       the_elapsed_time := 0;
   390.       the_last_delay_time := 0;
   391.       the_last_K4_time := 0;
   392.       the_CPU_state := the_new_state;
   393.       the_CPDAR := (0 => True, others => False);  -- FW0 is always allocated.
   394.    end reset_the_internal_registers;
   395.
   396.    empty_nest : constant NEST := (others => 0);
   397.    empty_sjns : constant SJNS := (others => (0, 0));
   398.    empty_Q_s  : constant Q_store := (others => (0, 0, 0));
   399.
   400.    procedure reset_the_CPU_state is
   401.    begin
   402.       the_context := 0;
   403.       for bank of register_bank loop
   404.          bank := (NEST => empty_nest, SJNS => empty_sjns, Q_store => empty_Q_s);
   405.       end loop;
   406.       the_nest_depth := 0;
   407.       the_nest       := empty_nest;
   408.       the_sjns_depth := 0;
   409.       the_sjns       := empty_sjns;
   410.       the_Q_store    := empty_Q_s;
   411.       if the_execution_mode = program_mode then
   412.          reset_the_internal_registers(program_state);
   413.       else
   414.          reset_the_internal_registers(Director_state);
   415.       end if;
   416.       -- Setting NIA must follow program loading, as it fetches E0 into the IWBs.
   417.       set_NIA_to((0, 0));
   418.    end reset_the_CPU_state;
   419.
   420.    procedure reset_the_program_state is
   421.    begin
   422.       the_nest_depth := 0;
   423.       the_nest       := empty_nest;
  1595.             preview_a_jump_order;
  1596.                do_a_jump_order;
  1597.             look_back_at_a_jump_order;
  1598.          when data_access_order =>
  1599.             process_syllables_1_and_2_of_a_data_access_order;
  1600.             preview_a_data_access_order;
  1601.                do_a_data_access_order;
  1602.             look_back_at_a_data_access_order;
  1603.       end case;
  1604.
  1605.       finalize_the_traced_instruction_execution;
  1606.
  1607.       if ICR >= time_limit then
  1608.          raise time_expired;
  1609.       end if;
  1610.
  1611.       if (NIA_word_number / breakpoints      and then
  1612.              ICR in low_count .. high_count)  or else
  1613.                 the_diagnostic_mode = pause_mode then
  1614.          handle_breakpoint;
  1615.       end if;
  1616.
  1617.       check_for_a_clock_interrupt;
  1618.       if the_elapsed_time > the_next_interrupt_time then
  1619.          act_on_pending_interrupts;
  1620.       end if;
  1621.
  1622.    exception
  1623.
  1624.       when program_exit =>
  1625.          case INS.kind is
  1626.             when one_syllable_order =>
  1627.                look_back_at_a_one_syllable_order;
  1628.             when two_syllable_order =>
  1629.                look_back_at_a_two_syllable_order;
  1630.             when normal_jump_order =>
  1631.                look_back_at_a_jump_order;
  1632.             when data_access_order =>
  1633.                look_back_at_a_data_access_order;
  1634.          end case;
  1635.          complete_all_extant_transfers;
  1636.          finalize_the_traced_instruction_execution;
  1637.          raise;
  1638.
  1639.       -- These traps represent interrupts that are handled by Director.
  1640.       -- Other traps pass through, to preserve their diagnostic message.
  1641.
  1642.       when PR_trap =>
  1643.          effect(PR_interrupt);
  1644.
  1645.       when FLEX_trap =>
  1646.          effect(FLEX_interrupt);
  1647.
  1648.       when EDT_trap =>
  1649.          effect(EDT_interrupt);
  1650.
  1651.       when OUT_trap =>
  1652.          effect(OUT_interrupt);
  1653.
  1654.       when LOV_trap =>
  1655.           effect(LOV_interrupt);
  1656.
  1657.    end do_a_traced_instruction_cycle;
  1658.
  1659. end KDF9.microcode;
   424.       the_sjns_depth := 0;
   425.       the_sjns := empty_sjns;
   426.       the_V_bit_is_set := False;
   427.       the_T_bit_is_set := False;
   428.       the_CPDAR := (0 => True, others => False);  -- FW0 is always allocated.
   429.       -- Setting NIA must follow program loading, as it fetches E0 into the IWBs.
   430.       set_NIA_to((0, 0));
   431.    end reset_the_program_state;
   432.
   433.    procedure effect (this_interrupt : in KDF9.interrupt_number; message : in String := "") is
   434.       return_address : KDF9.syllable_address;
   435.    begin
   436.       take_note_of_interrupt(this_interrupt, message);
   437.       the_RFIR(this_interrupt) := True;
   438.       case the_execution_mode is
   439.          when boot_mode =>
   440.             -- Interrupts are either effected or deferred to Director.
   441.             if the_CPU_state = program_state or else this_interrupt = RESET_interrupt then
   442.                -- Effect an actual interrupt into Director.
   443.                if this_interrupt in LOV_interrupt | OUT_interrupt then
   444.                   return_address := CIA;  -- Restart the interrupted instruction.
   445.                else
   446.                   return_address := NIA;  -- Proceed after the interrupted instruction.
   447.                end if;
   448.                if the_sjns_depth < 16 then
   449.                   push(return_address);                  -- The program link fits into the SJNS.
   450.                else
   451.                   JB := KDF9.sjns_link(return_address);  -- The program link overwrites JB.
   452.                end if;
   453.                BA := 0;
   454.                fetching_normally := True;
   455.                set_NIA_to((0, 0));
   456.                the_CPU_state := Director_state;
   457.                raise abandon_this_order;
   458.             else
   459.                -- Defer: Director will eventually find any request left in the_RFIR.
   460.                -- NOUV is completely suppressed in Director state.
   461.                the_RFIR(NOUV_interrupt) := False;
   462.             end if;
   463.
   464.          when test_program_mode =>
   465.             -- Interrupts other than LOV and RESET are ignored.
   466.             -- There is no need to accurately emulate the address placed by the hardware in JB.
   467.             case this_interrupt is
   468.                when LOV_interrupt =>
   469.                   raise LOV_trap with message;
   470.                when RESET_interrupt =>

   471.                   raise RESET_trap with message;
   472.                when others =>
   473.                   null;
   474.             end case;
   475.
   476.          when program_mode =>
   477.             -- Interrupts other than LOV are treated as failures.
   478.             -- There is no need to accurately emulate the address placed by the hardware in JB.
   479.             case this_interrupt is
   480.                when PR_interrupt =>
   481.                   raise PR_trap with message;
   482.                when FLEX_interrupt =>
   483.                   raise FLEX_trap with message;
   484.                when LIV_interrupt =>
   485.                   raise LIV_trap with message;
   486.                when NOUV_interrupt =>
   487.                   raise NOUV_trap with message;
   488.                when EDT_interrupt =>
   489.                   raise EDT_trap with message;
   490.                when OUT_interrupt =>
   491.                   raise OUT_trap with message;
   492.                when LOV_interrupt =>
   493.                   raise LOV_trap with message;
   494.                when RESET_interrupt =>
   495.                   raise RESET_trap with message;
   496.                when others =>
   497.                   raise emulation_failure with "invalid RFI in KDF9.effect";
   498.             end case;
   499.       end case;
   500.    end effect;
   501.
   502.    procedure effect_clock_interrupt (interval : in KDF9.us)
   503.       with Inline => False;
   504.
   505.    procedure effect_clock_interrupt (interval : in KDF9.us) is
Compiling: ../Source/kdf9-microcode.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- kdf9-microcode.ads
     2. --
     3. -- KDF9 ISP emulation - CPU microcode routines.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package KDF9.microcode is
    20.
    21.    procedure do_a_fast_time_slice;
    22.
    23.    procedure do_a_traced_instruction_cycle;
    24.
    25. end KDF9.microcode;
   506.    begin
   507.       effect(CLOCK_interrupt, interval'Image & " KDF9 us");
   508.    end effect_clock_interrupt;
   509.
   510.    procedure check_for_a_clock_interrupt is
   511.       interval : KDF9.us;
   512.    begin
   513.       -- Clock ticks are ignored in program_mode and test_program_mode.
   514.       -- In boot_mode:
   515.       --    they are actioned in program_state;
   516.       --    they are deferred in Director_state: Director will eventually find the time for itself.
   517.       if the_execution_mode = boot_mode and then
   518.             the_CPU_state = program_state   then
   519.          interval := (the_clock_time - the_last_K4_time);
   520.          if interval >= 2**20 then
   521.             effect_clock_interrupt(interval);
   522.          end if;
   523.       end if;
   524.    end check_for_a_clock_interrupt;
   525.
   526.    procedure fail_in_problem_program_state is
   527.    begin
   528.       case the_execution_mode is
   529.          when program_mode =>
   530.             -- The unprivileged program has attempted a privileged operation.
   531.             raise LIV_trap with "%Director-only instruction";
   532.          when test_program_mode =>
   533.             -- The privileged program is allowed to use privileged instructions.
   534.             return;
   535.          when boot_mode =>
   536.             if the_CPU_state = program_state then
   537.                -- Punt the error to Director.
   538.                effect(LIV_interrupt);

 1659 lines: No errors
   539.             else
   540.                -- All privileged operations are permitted to Director.
   541.                return;
   542.             end if;
   543.       end case;
   544.    end fail_in_problem_program_state;
   545.
   546.    procedure LOV_if_user_mode (device_name : in String) is
   547.    begin
   548.       -- LOV was TOTALLY suppressed in Director state.
   549.       if the_CPU_state /= Director_state then
   550.          set_NIA_to(CIA);
   551.          effect(LOV_interrupt, device_name);
   552.       end if;
   553.    end LOV_if_user_mode;
   554.
   555.    -- The %  prepended to the_message aids parsing of exception error messages in failure shutdown.
   556.
   557.    procedure trap_invalid_instruction (the_message : in String := "invalid opcode") is
   558.    begin
   559.       -- The program has failed in a manner that could cause a LIV interrupt.
   560.       case the_execution_mode is
   561.          when program_mode
   562.             | test_program_mode =>
   563.             raise LIV_trap with "%" & the_message;
   564.          when boot_mode =>
   565.             if the_CPU_state = program_state then
   566.                -- Punt the problem to Director.
   567.                effect(LIV_interrupt, the_message);
   568.             else
   569.                -- The Director itself has gone seriously wrong.
   570.                -- LIV is impossible in Director, so ee9 takes responsibility for stopping the run
   571.                --    to avert consequential emulation failure.
   572.                raise Director_failure with "%" & the_message;
   573.             end if;
   574.       end case;
   575.    end trap_invalid_instruction;
   576.
   577.    procedure trap_invalid_operand (the_message : in String := "invalid operand") is
   578.    begin
   579.       -- The program has failed in a manner that does not cause a LIV interrupt,
   580.       --    but needs to be handled in a similar manner by ee9.
   581.       case the_execution_mode is
   582.          when program_mode
   583.             | test_program_mode =>
   584.             raise operand_error with "%" & the_message;
   585.          when boot_mode =>
   586.             if the_CPU_state = program_state then
   587.                raise operand_error with "%" & the_message;
   588.             else
   589.                -- The Director itself has gone seriously wrong.
   590.                raise Director_operand_error with "%" & the_message;
   591.             end if;
   592.       end case;
   593.    end trap_invalid_operand;
   594.
   595.    procedure trap_operator_error (the_message : in String) is
   596.    begin
   597.       -- The program has failed for a reason, such as a misconfigured environment,
   598.       --    that is beyond its control and prevents further execution.
   599.       raise operator_error with "%" & the_message;
   600.    end trap_operator_error;
   601.
   602.    procedure trap_unimplemented_feature (the_message : in String) is
   603.    begin
   604.       -- The program has attempted to use something that ee9 does not (yet) support.
   605.       raise not_yet_implemented with "%" & the_message;
   606.    end trap_unimplemented_feature;
   607.
   608.    procedure trap_invalid_paper_tape (the_message : in String) is
   609.    begin
   610.       -- The paper tape file given to load or boot has defects.
   611.       raise invalid_paper_tape_file with "%" & the_message;
   612.    end trap_invalid_paper_tape;
   613.
   614.    procedure return_from_Director_to (new_IAR : in KDF9.syllable_address) is
   615.    begin
   616.       the_CPU_state := program_state;
   617.       set_NIA_to(new_IAR);
   618.    end return_from_Director_to;
   619.
   620.    procedure increment_by_1 (the_link : in out KDF9.syllable_address) is
   621.    begin
   622.       if the_link.syllable_index < 5 then
   623.          the_link.syllable_index := the_link.syllable_index + 1;
   624.       else
   625.          the_link.syllable_index := 0;
   626.          the_link.order_word_number     := the_link.order_word_number + 1;
   627.       end if;
   628.    end increment_by_1;
   629.
   630.    procedure increment_by_2 (the_link : in out KDF9.syllable_address) is
   631.    begin
   632.       if the_link.syllable_index < 4 then
   633.          the_link.syllable_index := the_link.syllable_index + 2;
   634.       else
   635.          the_link.syllable_index := the_link.syllable_index - 4;
   636.          the_link.order_word_number     := the_link.order_word_number + 1;
   637.       end if;
   638.    end increment_by_2;
   639.
   640.    procedure increment_by_3 (the_link : in out KDF9.syllable_address) is
   641.    begin
   642.       if the_link.syllable_index < 3 then
   643.          the_link.syllable_index := the_link.syllable_index + 3;
   644.       else
   645.          the_link.syllable_index := the_link.syllable_index - 3;
   646.          the_link.order_word_number     := the_link.order_word_number + 1;
   647.       end if;
   648.    end increment_by_3;
   649.
   650.    -- the_syllable_cache holds two instruction words, pre-split into syllables.
   651.    -- They would have been held in IWB0 and IWB1 by Main Control in the KDF9.
   652.
   653.    subtype syllable_cache_range is Natural range 0 .. 11;
   654.
   655.    the_syllable_cache  : array (syllable_cache_range) of KDF9.syllable;
   656.    the_cache_index     : syllable_cache_range   := 0;
   657.    the_cached_location : KDF9.order_word_number := 0;
   658.
   659.    function NIA
   660.    return KDF9.syllable_address
   661.    is (
   662.        if the_cache_index > 5 then
   663.           (the_cached_location, KDF9.syllable_index(the_cache_index-6))
   664.        else
   665.           (the_cached_location-1, KDF9.syllable_index(the_cache_index))
   666.       );
   667.
   668.    function NIA_word_number
   669.    return KDF9.order_word_number
   670.    is (the_cached_location - (if the_cache_index > 5 then 0 else 1));
   671.
   672.    procedure trap_an_invalid_order_address (new_NIA : in KDF9.syllable_address)
   673.       with Inline => False;
   674.
   675.    procedure trap_an_invalid_order_address (new_NIA : in KDF9.syllable_address) is
   676.    begin
   677.       if new_NIA.syllable_index = 6 then
   678.          effect(RESET_interrupt, "syllable number = 6");
   679.       elsif new_NIA.syllable_index = 7 then
   680.          effect(RESET_interrupt, "syllable number = 7");
   681.       else
   682.          effect(LIV_interrupt, "jump to 8191");  -- See EE Report K/GD y 82.
   683.       end if;
   684.    end trap_an_invalid_order_address;
   685.
   686.    procedure set_NIA_to (new_NIA : in KDF9.syllable_address) is
   687.       mask        : constant := 8#377#;
   688.       shift       : constant := 8#400#;
   689.       IWB0, IWB1  : KDF9.word;
   690.    begin
   691.       if new_NIA.order_word_number = 8191 or else
   692.             new_NIA.syllable_index > 5       then
   693.          trap_an_invalid_order_address(new_NIA);
   694.       end if;
   695.
   696.       IWB0 := fetch_word(KDF9.address(new_NIA.order_word_number) + 0);
   697.       IWB1 := fetch_word(KDF9.address(new_NIA.order_word_number) + 1);
   698.
   699.       the_cache_index := syllable_cache_range(new_NIA.syllable_index);
   700.       the_cached_location := new_NIA.order_word_number + 1;
   701.
   702.       the_syllable_cache(5+0) := KDF9.syllable(IWB0 and mask);
   703.       IWB0 := IWB0 / shift;
   704.       the_syllable_cache(4+0) := KDF9.syllable(IWB0 and mask);
   705.       IWB0 := IWB0 / shift;
   706.       the_syllable_cache(3+0) := KDF9.syllable(IWB0 and mask);
   707.       IWB0 := IWB0 / shift;
   708.       the_syllable_cache(2+0) := KDF9.syllable(IWB0 and mask);
   709.       IWB0 := IWB0 / shift;
   710.       the_syllable_cache(1+0) := KDF9.syllable(IWB0 and mask);
   711.       IWB0 := IWB0 / shift;
   712.       the_syllable_cache(0+0) := KDF9.syllable(IWB0);
   713.
   714.       the_syllable_cache(5+6) := KDF9.syllable(IWB1 and mask);
   715.       IWB1 := IWB1 / shift;
   716.       the_syllable_cache(4+6) := KDF9.syllable(IWB1 and mask);
   717.       IWB1 := IWB1 / shift;
   718.       the_syllable_cache(3+6) := KDF9.syllable(IWB1 and mask);
   719.       IWB1 := IWB1 / shift;
   720.       the_syllable_cache(2+6) := KDF9.syllable(IWB1 and mask);
   721.       IWB1 := IWB1 / shift;
   722.       the_syllable_cache(1+6) := KDF9.syllable(IWB1 and mask);
   723.       IWB1 := IWB1 / shift;
   724.       the_syllable_cache(0+6) := KDF9.syllable(IWB1);
   725.    end set_NIA_to;
   726.
   727.    procedure set_NIA_to_the_INS_target_address is
   728.    begin
   729.       set_NIA_to(INS.target);
   730.    end set_NIA_to_the_INS_target_address;
   731.
   732.    procedure set_IWB0_and_IWB1_for_a_JCqNZS_loop is
   733.    begin
   734.       set_NIA_to((order_word_number => CIA.order_word_number-1, syllable_index => 0));
   735.       fetching_normally := False;
   736.    end set_IWB0_and_IWB1_for_a_JCqNZS_loop;
   737.
   738.    procedure go_back_to_the_start_of_IWB0 is
   739.    begin
   740.       the_cache_index := 0;
   741.    end go_back_to_the_start_of_IWB0;
   742.
   743.    procedure continue_after_JCqNZS is
   744.    begin
   745.       if CIA.syllable_index = 4 and the_cached_location = CIA.order_word_number then
   746.          set_NIA_to((order_word_number => CIA.order_word_number+1, syllable_index => 0));
   747.       end if;
   748.       fetching_normally := True;
   749.    end continue_after_JCqNZS;
   750.
   751.    function next_order_syllable
   752.    return KDF9.syllable
   753.       with Inline;
   754.
   755.    -- The amount by which the_CPU_time is increased, for a refill of both Instruction Word Buffers.
   756.
   757.    the_IWB01_reload_time : constant KDF9.us := 7;  -- microseconds
   758.
   759.    function next_order_syllable
   760.    return KDF9.syllable is
   761.       the_next_syllable : KDF9.syllable;
   762.    begin
   763.       the_next_syllable := the_syllable_cache(the_cache_index);
   764.       if the_cache_index < 11 then
   765.          the_cache_index := the_cache_index + 1;
   766.       elsif fetching_normally then
   767.          set_NIA_to((order_word_number => CIA.order_word_number+1, syllable_index => 0));
   768.          -- Part-overlapped order-word fetch: can happen only once per instruction,
   769.          --    and only before the instruction is executed, so no need to ADD to the_CPU_delta.
   770.          if (CIA.order_word_number and 15) < 10 then
   771.             -- The fudge factor applied here gives the Whetstone Benchmark its historical run time.
   772.             the_CPU_delta := the_IWB01_reload_time + 1;
   773.          else
   774.             the_CPU_delta := the_IWB01_reload_time;
   775.          end if;
   776.       else
   777.          go_back_to_the_start_of_IWB0;
   778.       end if;
   779.       return the_next_syllable;
   780.    end next_order_syllable;
   781.
   782.    procedure decode_syllable_0 (decoded : in out KDF9.decoded_order)
   783.       with Inline;
   784.
   785.    procedure decode_syllable_1 (decoded : in out KDF9.decoded_order)
   786.       with Inline;
   787.
   788.    procedure decode_a_jump_order (decoded : in out KDF9.decoded_order)
   789.       with Inline;
   790.
   791.    procedure decode_a_store_access_order (decoded : in out KDF9.decoded_order)
   792.       with Inline;
   793.
   794.    procedure decode_a_set_literal_order (decoded : in out KDF9.decoded_order)
   795.       with Inline;
   796.
   797.    procedure decode_syllable_0 (decoded : in out KDF9.decoded_order) is
   798.    begin
   799.       decoded.compressed_opcode := decoded.order.syllable_0 and 8#77#;
   800.       decoded.kind := KDF9.INS_kind(decoded.order.syllable_0 / 2**6);
   801.    end decode_syllable_0;
   802.
   803.    procedure process_syllable_0_of_INS is
   804.    begin
   805.       if the_cache_index > 5 then
   806.          CIA.order_word_number := the_cached_location;
   807.          CIA.syllable_index   := KDF9.syllable_index(the_cache_index-6);
   808.       else
   809.          CIA.order_word_number := the_cached_location - 1;
   810.          CIA.syllable_index   := KDF9.syllable_index(the_cache_index);
   811.       end if;
   812.       INS.order.syllable_0 := next_order_syllable;
   813.       INS.compressed_opcode := INS.order.syllable_0 and 8#77#;
   814.       INS.kind := KDF9.INS_kind(INS.order.syllable_0 / 2**6);
   815.    end process_syllable_0_of_INS;
   816.
   817.    procedure decode_syllable_1 (decoded : in out KDF9.decoded_order) is
   818.    begin
   819.       decoded.Qq := KDF9.Q_number(decoded.order.syllable_1 / 2**4);
   820.       decoded.Qk := KDF9.Q_number(decoded.order.syllable_1 and 8#17#);
   821.    end decode_syllable_1;
   822.
   823.    procedure process_syllable_1_of_INS is
   824.    begin
   825.       INS.order.syllable_1 := next_order_syllable;
   826.       INS.Qq := KDF9.Q_number(INS.order.syllable_1 / 2**4);
   827.       INS.Qk := KDF9.Q_number(INS.order.syllable_1 and 8#17#);
   828.    end process_syllable_1_of_INS;
   829.
   830.    syllable_nr_mask : constant := 2#111#;
   831.    D4_mask          : constant := 2#1000#;
   832.    D2_mask          : constant := 2#00_100_000#;
   833.    D0_thru_3_mask   : constant := 2#11_110_000#;
   834.
   835.    procedure decode_a_jump_order (decoded : in out KDF9.decoded_order) is
   836.    begin
   837.       decoded.target.syllable_index
   838.          := KDF9.syllable_index(decoded.order.syllable_0 and syllable_nr_mask);
   839.       decoded.target.order_word_number
   840.          := KDF9.order_word_number(decoded.order.syllable_2)
   841.           + KDF9.order_word_number(decoded.Qk) * 2**8
   842.           + KDF9.order_word_number(decoded.order.syllable_0 and D4_mask) * 2**9;
   843.       if (decoded.compressed_opcode and D2_mask) /= 0 then -- not JrCq ...
   844.          decoded.compressed_opcode := decoded.compressed_opcode and D0_thru_3_mask;
   845.       else
   846.          decoded.compressed_opcode := (decoded.compressed_opcode and D0_thru_3_mask) or KDF9.syllable(decoded.Qq);
   847.       end if;
   848.       if decoded.compressed_opcode = EXIT_n then
   849.          -- The syllable part of EXIT is actually a halfword offset,
   850.          --    so convert it to an actual syllable number.
   851.          if decoded.target.syllable_index = 2 then
   852.             decoded.target.syllable_index := 0;
   853.          else
   854.             decoded.target.syllable_index := 3;
   855.          end if;
   856.       end if;
   857.    end decode_a_jump_order;
   858.
   859.    procedure process_syllables_1_and_2_of_a_jump_order is
   860.    begin
   861.       process_syllable_1_of_INS;
   862.       INS.order.syllable_2 := next_order_syllable;
   863.       decode_a_jump_order(INS);
   864.    end process_syllables_1_and_2_of_a_jump_order;
   865.
   866.    D5_thru_7_mask : constant := 2#111#;
   867.    D5_and_7_mask  : constant := 2#101#;
   868.    D2_thru_4_mask : constant := 2#111000#;
   869.
   870.    procedure decode_a_store_access_order (decoded : in out KDF9.decoded_order) is
   871.    begin
   872.       decoded.operand := KDF9.Q_part(decoded.order.syllable_2) + KDF9.Q_part(decoded.Qk)*2**8
   873.                        + KDF9.Q_part((decoded.order.syllable_0 and D2_thru_4_mask)) * 2**9;
   874.       decoded.compressed_opcode := decoded.compressed_opcode and D5_thru_7_mask;
   875.    end decode_a_store_access_order;
   876.
   877.    procedure decode_a_set_literal_order (decoded : in out KDF9.decoded_order) is
   878.    begin
   879.       decoded.operand := KDF9.Q_part(decoded.order.syllable_2)
   880.                        + KDF9.Q_part(decoded.order.syllable_1)*2**8;
   881.       decoded.compressed_opcode := 2#100#;
   882.    end decode_a_set_literal_order;
   883.
   884.    procedure process_syllables_1_and_2_of_a_data_access_order is
   885.    begin
   886.       if (INS.compressed_opcode and D5_thru_7_mask) < SET then
   887.          process_syllable_1_of_INS;
   888.          INS.order.syllable_2 := next_order_syllable;
   889.          decode_a_store_access_order(INS);
   890.       elsif (INS.compressed_opcode and D5_and_7_mask) = SET then
   891.          -- SET n
   892.          INS.order.syllable_1 := next_order_syllable;
   893.          INS.order.syllable_2 := next_order_syllable;
   894.          decode_a_set_literal_order(INS);
   895.       else
   896.          INS.order.syllable_1 := next_order_syllable;
   897.          INS.order.syllable_2 := next_order_syllable;
   898.          decode_a_set_literal_order(INS);
   899.          INS.compressed_opcode := 7;  -- an invalid compression.
   900.       end if;
   901.    end process_syllables_1_and_2_of_a_data_access_order;
   902.
   903.    procedure decode_the_next_order is
   904.    begin
   905.       -- The CPU time is adjusted by a fudge factor to account for
   906.       --    the instruction-fetch time being partly overlapped.
   907.       process_syllable_0_of_INS;
   908.       case INS.kind is
   909.          when one_syllable_order =>
   910.             return;
   911.          when two_syllable_order =>
   912.             process_syllable_1_of_INS;
   913.          when normal_jump_order =>
   914.             process_syllables_1_and_2_of_a_jump_order;
   915.          when data_access_order =>
   916.             process_syllables_1_and_2_of_a_data_access_order;
   917.       end case;
   918.    end decode_the_next_order;
   919.
   920.    procedure decode (the_order : in out KDF9.decoded_order) is
   921.    begin
   922.       decode_syllable_0(the_order);
   923.       case the_order.kind is
   924.          when one_syllable_order =>
   925.             null;
   926.          when two_syllable_order =>
   927.             decode_syllable_1(the_order);
   928.          when normal_jump_order =>
   929.             decode_syllable_1(the_order);
   930.             decode_a_jump_order(the_order);
   931.          when data_access_order =>
   932.             if (the_order.compressed_opcode and D5_thru_7_mask) < SET then
   933.                decode_syllable_1(the_order);
   934.                decode_a_store_access_order(the_order);
   935.             elsif (INS.compressed_opcode and D5_and_7_mask) = SET then
   936.                -- SET n
   937.                decode_a_set_literal_order(the_order);
   938.             else
   939.                decode_a_set_literal_order(the_order);
   940.                INS.compressed_opcode := 7;  -- an invalid compression.
   941.             end if;
   942.       end case;
   943.    end decode;
   944.
   945.    -- the_order_at_NIA gets three syllables starting at [NIA].  It is FOR DIAGNOSTIC USE ONLY!
   946.    -- It does NOT update the CPU time properly and MUST NOT be used inside an instruction cycle.
   947.
   948.    function the_order_at_NIA
   949.    return KDF9.syllable_group is
   950.       saved_NIA : constant KDF9.syllable_address := NIA;
   951.       result    : KDF9.syllable_group;
   952.    begin
   953.       result.syllable_0 := next_order_syllable;
   954.       result.syllable_1 := next_order_syllable;
   955.       result.syllable_2 := next_order_syllable;
   956.       set_NIA_to(saved_NIA);
   957.       return result;
   958.    end the_order_at_NIA;
   959.
   960.    -- This is the initial jump from the top halfword of E0 just after loading.
   961.
   962.    E0U : KDF9.word := 0;  -- N.B. the lower halfword is used for option flags.
   963.
   964.    procedure save_the_initial_jump is
   965.    begin
   966.       E0U := fetch_halfword(0, 0);
   967.    end save_the_initial_jump;
   968.
   969.    procedure restore_the_initial_jump is
   970.    begin
   971.        store_halfword(E0U, 0, 0);
   972.    end restore_the_initial_jump;
   973.
   974.    function the_initial_jump_was_corrupted
   975.    return Boolean
   976.    is (E0U /= fetch_halfword(0, 0));
   977.
   978.    function is_an_invalid_order (decoded : KDF9.decoded_order)
   979.    return Boolean
   980.    is (
   981.        (decoded.kind = data_access_order and then (decoded.order.syllable_0 and 2#101#) > 2#100#)
   982.          or else (decoded.kind = normal_jump_order and decoded.target.syllable_index > 5)
   983.             -- 0 is now treated as a valid DUMMY0 order for KAlgol
   984.                or else decoded.order.syllable_0 = 8#006#
   985.                   or else decoded.order.syllable_0 = 8#040#
   986.                      or else decoded.order.syllable_0 = 8#046#
   987.                         or else decoded.order.syllable_0 = 8#055#
   988.                            or else decoded.order.syllable_0 = 8#073#
   989.                               or else decoded.order.syllable_0 = 8#076#
   990.                                  or else decoded.order.syllable_0 = 8#150#
   991.       );
   992.
   993.    the_signature_hash : KDF9.word := 0;
   994.
   995.    function the_digital_signature
   996.    return KDF9.word
   997.    is (the_signature_hash);
   998.
   999.    function visible_state_hash
  1000.    return KDF9.word
  1001.       with Inline;
  1002.
  1003.    function visible_state_hash
  1004.    return KDF9.word is
  1005.       hash : KDF9.word;
  1006.    begin
  1007.       hash := rotate_word_right(the_signature_hash, 1) xor KDF9.word(ICR);
  1008.       hash := rotate_word_right(hash, 1) xor as_word(the_Q_store(INS.Qq));
  1009.       hash := rotate_word_right(hash, 1) xor as_word(the_Q_store(INS.Qk));
  1010.       if the_sjns_depth > 0 then
  1011.          for s in reverse KDF9.sjns_depth range 0 .. the_sjns_depth-1 loop
  1012.                hash := rotate_word_right(hash, 1) xor as_word(the_sjns(s));
  1013.          end loop;
  1014.       end if;
  1015.       if the_nest_depth > 0 then
  1016.          for n in reverse KDF9.nest_depth range 0 .. the_nest_depth-1 loop
  1017.                hash := rotate_word_right(hash, 1) xor the_nest(n);
  1018.          end loop;
  1019.       end if;
  1020.       return hash;
  1021.    end visible_state_hash;
  1022.
  1023.    procedure update_the_digital_signature is
  1024.    begin
  1025.       the_signature_hash := visible_state_hash;
  1026.    end update_the_digital_signature;
  1027.
  1028. end KDF9;

Compiling: ../Source/kdf9.ads
Source file time stamp: 2020-10-02 17:38:33
Compiled at: 2020-11-12 18:12:11

     1. -- kdf9.ads
     2. --
     3. -- The architecturally-defined data and register formats of the KDF9 computer.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with System;
    20.
    21. package KDF9 is
    22.
    23.    --
    24.    --
    25.    -- The fundamental storage unit is the 48-bit word.
    26.    --
    27.    --
    28.
    29.    --
    30.    -- The 48-bit word, considered as an unsigned integer.
    31.    --
    32.
    33.    type word is mod 2**48;  -- Let the compiler choose the best Size for this.
    34.
    35.    word_mask : constant := 8#7777777777777777#;
    36.    min_word  : constant := 8#4000000000000000#;
    37.    max_word  : constant := 8#3777777777777777#;
    38.
    39.    all_zero_bits : constant KDF9.word := 0;
    40.    sign_bit      : constant KDF9.word := KDF9.min_word;
    41.    not_sign_bit  : constant KDF9.word := KDF9.max_word;
    42.    all_one_bits  : constant KDF9.word := KDF9.word_mask;
    43.
    44.
    45.    --
    46.    -- The 96-bit double word, considered as a pair of words.
    47.    --
    48.
    49.    type pair is
    50.       record
    51.          msw, lsw : KDF9.word;
    52.       end record;
    53.
    54.
    55.    --
    56.    -- The basic 16-bit operand.
    57.    --
    58.
    59.    type field_of_16_bits is mod 2**16;
    60.
    61.    --
    62.    -- The 16-bit word, considered as a field of a Q register.
    63.    --
    64.
    65.    type Q_part is new KDF9.field_of_16_bits;
    66.
    67.    Q_part_mask : constant := KDF9.Q_part'Last;
    68.
    69.    function sign_extended (Q : KDF9.Q_part)
    70.    return KDF9.word
    71.       with Inline;
    72.
    73.    --
    74.    -- The 16-bit word, considered as a buffer (DMA channel) number.
    75.    --
    76.
    77.    subtype buffer_number is KDF9.Q_part range 0 .. 15;
    78.
    79.    buffer_number_mask : constant := buffer_number'Last;
    80.
    81.    --
    82.    -- The 16-bit word, considered as a core-store address.
    83.    --
    84.
    85.    subtype address is KDF9.Q_part range 0 .. 8#77777#;
    86.
    87.    --
    88.    -- The Q-store element.
    89.    --
    90.
    91.    type Q_register is
    92.       record
    93.          C, I, M : KDF9.Q_part;
    94.       end record;
    95.
    96.    function as_Q (the_word : KDF9.word)
    97.    return KDF9.Q_register
    98.       with Inline;
    99.
   100.    function as_word (the_Q : KDF9.Q_register)
   101.    return KDF9.word
   102.       with Inline;
   103.
   104.
   105.    --
   106.    -- The 8-bit instruction syllable and its components.
   107.    --
   108.
   109.    type syllable is mod 2**8;
   110.
   111.    subtype compressed_opcode is KDF9.syllable range 0 .. 63;
   112.    subtype Q_number          is KDF9.syllable range 0 .. 15;
   113.
   114.    type syllable_group is
   115.       record
   116.          syllable_0, syllable_1, syllable_2 : KDF9.syllable := 0;
   117.       end record;
   118.
   119.
   120.    --
   121.    -- An instruction address.
   122.    --
   123.    -- N.B. 5 is the hardware's largest valid syllable address.
   124.    -- The values 6 and 7 are used as diagnostic flags by ee9.
   125.    -- They cause a RESET trap if encountered during execution.
   126.    --
   127.
   128.    type syllable_index    is mod 2**3;
   129.    type order_word_number is mod 2**13;
   130.
   131.    type syllable_address is
   132.       record
   133.          order_word_number : KDF9.order_word_number;
   134.          syllable_index    : KDF9.syllable_index;
   135.       end record;
   136.
   137.    --
   138.    -- An instruction address, in the packed format of a hardware (SJNS) link.
   139.    --
   140.
   141.    type sjns_link is new KDF9.syllable_address
   142.       with Size => 16;
   143.    for sjns_link'Bit_Order use System.Low_Order_First;
   144.    for sjns_link use
   145.       record
   146.          order_word_number at 0 range  0 .. 12;
   147.          syllable_index    at 0 range 13 .. 15;
   148.       end record;
   149.
   150.    function as_word (the_link : KDF9.sjns_link)
   151.    return KDF9.word;
   152.
   153.    function as_link (the_word : KDF9.word)
   154.    return KDF9.sjns_link;
   155.
   156.    procedure increment_by_1 (the_link : in out KDF9.syllable_address)
   157.       with Inline;
   158.
   159.    procedure increment_by_2 (the_link : in out KDF9.syllable_address)
   160.       with Inline;
   161.
   162.    procedure increment_by_3 (the_link : in out KDF9.syllable_address)
   163.       with Inline;
   164.
   165.
   166.    --
   167.    -- The KDF9 halfword. Each occupies 24 bits, packed 2 per word.
   168.    --
   169.
   170.    type halfword is mod 2**24;
   171.    halfword_mask : constant := 8#77_77_77_77#;
   172.
   173.    subtype halfword_number is KDF9.address range 0 .. 1;
   174.
   175.
   176.    --
   177.    -- The KDF9 character occupies six bits, and they are packed 8 per word.
   178.    -- The various character sets, and the symbol type, are defined in the package KDF9_char_sets.
   179.    --
   180.
   181.
   182.    --
   183.    --
   184.    --
   185.    -- The following types define the structure of the KDF9's programmable registers.
   186.    --
   187.    --
   188.    --
   189.
   190.
   191.    --
   192.    -- authenticity_mode is declared here instead of in settings, to avoid a cyclic dependency.
   193.    --
   194.
   195.    type authenticity_mode is (modern_times_mode, authentic_time_mode);
   196.
   197.    the_authenticity_default : constant KDF9.authenticity_mode := modern_times_mode;
   198.    the_authenticity_mode    :          KDF9.authenticity_mode := the_authenticity_default;
   199.
   200.
   201.    --
   202.    --
   203.    --
   204.    -- The following variables (the_nest, the_sjns and the_Q_store) constitute
   205.    --    the emulation microcode's fixed working set of registers.
   206.    -- ee9 (unlike the real KDF9) swaps them with register_bank(the_context)
   207.    --    when a context switch is made by the =K3 instruction.
   208.    -- The real KDF9 used register_bank(the_context) directly for operands.
   209.    -- ee9's approach improves host cache locality and avoids indexing overheads,
   210.    --    the trade off being microscopically increased context-switching time.
   211.    --
   212.    --
   213.    --
   214.
   215.    --
   216.    -- The NEST.
   217.    --
   218.
   219.    type nest_depth is mod 19;
   220.
   221.    type NEST is array (KDF9.nest_depth) of KDF9.word;
   222.
   223.    the_nest       : KDF9.NEST;
   224.    the_nest_depth : KDF9.nest_depth  := 0;
   225.
   226.    -- The ensure_that_the_nest_holds* procedures trap NOUV.
   227.    -- They are used to validate operations that reduce the NEST depth.
   228.
   229.    procedure ensure_that_the_nest_holds (at_least : in KDF9.nest_depth)
   230.       with Inline;
   231.
   232.    procedure ensure_that_the_nest_holds_an_operand
   233.       with Inline;
   234.
   235.    procedure ensure_that_the_nest_holds_2_operands
   236.       with Inline;
   237.
   238.    function words_needed (need : KDF9.nest_depth)
   239.    return String
   240.       with Inline => False;
   241.
   242.    function space_needed (need : KDF9.nest_depth)
   243.    return String
   244.       with Inline => False;
   245.
   246.    function pop
   247.    return KDF9.word
   248.       with Inline;
   249.
   250.    procedure pop
   251.       with Inline;
   252.
   253.    procedure write_top (the_word : in KDF9.word)
   254.       with Inline;
   255.
   256.    function read_top
   257.    return KDF9.word
   258.       with Inline;
   259.
   260.
   261.    function pop
   262.    return KDF9.pair
   263.       with Inline;
   264.
   265.    procedure write_top (the_pair : in KDF9.pair)
   266.       with Inline;
   267.
   268.    function read_top
   269.    return KDF9.pair
   270.       with Inline;
   271.
   272.    -- The ensure_that_the_nest_has_room_for* procedures trap NOUV.
   273.    -- They are used to validate operations that increase the NEST depth.
   274.
   275.    procedure ensure_that_the_nest_has_room_for (at_least : in KDF9.nest_depth)
   276.       with Inline;
   277.
   278.    procedure ensure_that_the_nest_has_room_for_a_result
   279.       with Inline;
   280.
   281.    procedure push (the_word : in KDF9.word)
   282.       with Inline;
   283.
   284.    procedure ensure_that_the_nest_has_room_for_2_results
   285.       with Inline;
   286.
   287.    procedure push (the_pair : in KDF9.pair)
   288.       with Inline;
   289.
   290.
   291.    --
   292.    -- The SJNS.
   293.    --
   294.
   295.    type sjns_depth is mod 17;
   296.
   297.    type SJNS is array (KDF9.sjns_depth) of KDF9.sjns_link;
   298.
   299.    the_sjns       : KDF9.SJNS;
   300.    JB             : KDF9.sjns_link renames the_sjns(16);
   301.    the_sjns_depth : KDF9.sjns_depth := 0;
   302.
   303.    procedure ensure_that_the_sjns_is_not_empty
   304.       with Inline;
   305.
   306.    function pop
   307.    return KDF9.syllable_address
   308.       with Inline;
   309.
   310.    function sjns_top
   311.    return KDF9.sjns_link
   312.       with Inline;
   313.
   314.    procedure ensure_that_the_sjns_is_not_full
   315.       with Inline;
   316.
   317.    procedure push (the_link : in KDF9.syllable_address)
   318.       with Inline;
   319.
   320.
   321.    --
   322.    -- The Q Store.
   323.    -- Q0 is kept permanently zeroised.
   324.    --
   325.
   326.    type Q_store is array (KDF9.Q_number) of KDF9.Q_register;
   327.
   328.    the_Q_store : KDF9.Q_store;
   329.
   330.
   331.    --
   332.    -- The Boolean registers.
   333.    --
   334.
   335.    the_V_bit_is_set : Boolean := False;
   336.    the_T_bit_is_set : Boolean := False;
   337.
   338.
   339.    --
   340.    --
   341.    -- The following are to do with maintaining the virtual time.
   342.    --
   343.    --
   344.
   345.    type us is mod 2**64;  -- The emulation clocks tick in microseconds (unlike KDF9's clock).
   346.
   347.    -- The virtual processor time.
   348.
   349.    the_CPU_time  : KDF9.us := 0;
   350.
   351.    -- The amount by which the_CPU_time is increased by an instruction execution.
   352.
   353.    the_CPU_delta : KDF9.us := 0;
   354.
   355.    -- The virtual elapsed time, capped to prevent a spurious double-clock (RESET) interrupt.
   356.
   357.    function the_clock_time
   358.    return KDF9.us
   359.       with Inline;
   360.
   361.    -- Advance to the largest of the_CPU_time, the_elapsed_time, the_last_delay_time, and past.
   362.    -- Cap the increase to prevent a spurious double-clock (RESET) interrupt in Director.
   363.    -- If necessary, pause execution until the real time equals the virtual elapsed time.
   364.
   365.    procedure advance_the_clock (past : in KDF9.us);
   366.
   367.    -- The virtual clock time at which the next IO interrupt is expected.
   368.
   369.    the_next_interrupt_time : KDF9.us := KDF9.us'Last;
   370.
   371.    -- Pause execution for the_delay_time in virtual microseconds.
   372.
   373.    procedure delay_by (the_delay_time : in KDF9.us);
   374.
   375.    -- If necessary, pause execution until the real time equals the virtual elapsed time.
   376.
   377.    procedure synchronize_the_real_and_virtual_times;
   378.
   379. ------------------------------------------------------------------------------------------------
   380.
   381.    --
   382.    --
   383.    -- The following registers are used only in Director state.
   384.    --
   385.    --
   386.
   387.
   388.    --
   389.    -- The following are to do with the K1 order.
   390.    --
   391.
   392.    type priority is mod 2**2;
   393.
   394.    -- CPL = priority level of the currently-executing problem program.
   395.
   396.    CPL : KDF9.priority;
   397.
   398.    -- BA = word address of first allocated word (NOT group number as in the KDF9).
   399.
   400.    BA  : KDF9.address;
   401.
   402.    -- NOL = word address of last allocated word (NOT group number as in the KDF9).
   403.
   404.    NOL : KDF9.address;
   405.
   406.    -- Set BA (setting bits D38:47), CPL (D34:35) and NOL (D24:33).
   407.
   408.    procedure set_K1_register (setting : in KDF9.word);
   409.
   410.
   411.    --
   412.    -- The following are to do with the =K2 order.
   413.    --
   414.
   415.    -- The Current Peripheral Device Allocation Register.
   416.
   417.    type CPDAR is array (KDF9.buffer_number) of Boolean
   418.       with Component_Size => 8, Convention => C;
   419.
   420.    the_CPDAR : KDF9.CPDAR;
   421.
   422.    -- Set CPDAR (setting bits D32 .. D47).
   423.
   424.    procedure set_K2_register (setting : in KDF9.word);
   425.
   426.
   427.    --
   428.    -- The following are to do with the =K3 and K7 orders.
   429.    --
   430.
   431.    type user_register_set is
   432.       record
   433.          NEST     : KDF9.NEST;
   434.          SJNS     : KDF9.SJNS;
   435.          Q_store  : KDF9.Q_store;
   436.       end record;
   437.
   438.
   439.    -- There are 4 sets of user registers.
   440.    -- The execution context is the number of the register set in active use.
   441.
   442.    type context is mod 2**2;
   443.
   444.    -- register_bank holds the currently inactive register sets.
   445.
   446.    register_bank : array(KDF9.context) of KDF9.user_register_set;
   447.
   448.    -- KDF9 actually indexed the register bank with the value of the_context,
   449.    --   but the emulator swaps register sets between register_bank and
   450.    --      the_nest, the_sjns, and the_Q_store (q.v.).
   451.
   452.    the_context : KDF9.context := 0;
   453.
   454.    -- Set context (bits D46:47), nest_depth (D41:45) and sjns_depth (D36:41).
   455.
   456.    procedure set_K3_register (setting : in KDF9.word);
   457.
   458.    -- Get BA (bits D0 .. D9), CPL (D12 .. D13) and NOL (D14 .. D23).
   459.
   460.    function get_K7_operand
   461.    return KDF9.word;
   462.
   463.
   464.    --
   465.    -- The following are to do with the K4 order.
   466.    --
   467.
   468.    type interrupt_number is range 22 .. 31;
   469.
   470.    -- higher PRiority PRogram unblocked by end of I/O, or INTQq on busy device
   471.    PR_interrupt    : constant KDF9.interrupt_number := 22;
   472.    PR_trap         : exception;
   473.
   474.    -- FLEXowriter interrupt from operator
   475.    FLEX_interrupt  : constant KDF9.interrupt_number := 23;
   476.    FLEX_trap       : exception;
   477.
   478.    -- Lock-In Violation (attempt at a disallowed operation)
   479.    LIV_interrupt   : constant KDF9.interrupt_number := 24;
   480.    LIV_trap        : exception;
   481.
   482.    -- Nest (or SJNS) Over/Underflow Violation
   483.    NOUV_interrupt  : constant KDF9.interrupt_number := 25;
   484.    NOUV_trap       : exception;
   485.
   486.    -- End of Director Transfer, or I/O priority inversion
   487.    EDT_interrupt   : constant KDF9.interrupt_number := 26;
   488.    EDT_trap        : exception;
   489.
   490.    -- OUT system call
   491.    OUT_interrupt   : constant KDF9.interrupt_number := 27;
   492.    OUT_trap        : exception;
   493.
   494.    -- Lock-Out Violation
   495.    LOV_interrupt   : constant KDF9.interrupt_number := 28;
   496.    LOV_trap        : exception;
   497.
   498.    -- invalid syllable number or 'double-clock'
   499.    RESET_interrupt : constant KDF9.interrupt_number := 29;
   500.    RESET_trap      : exception;
   501.
   502.    type RFIR is array (KDF9.interrupt_number) of Boolean;
   503.
   504.    the_RFIR : KDF9.RFIR := (others => False);
   505.
   506.    -- The time at which the last K4 order was executed.
   507.    the_last_K4_time : KDF9.us := 0;
   508.
   509.    -- Get clock (bits D0:15) and RFIR (D16:31), clearing both.
   510.    function get_K4_operand
   511.    return KDF9.word;
   512.
   513.    -- An interrupt is raised when 1 second expires outside Director;
   514.    --    the flag does not correspond to any RFIR bit.
   515.    CLOCK_interrupt : constant KDF9.interrupt_number := 31;
   516.    CLOCK_trap      : exception;
   517.
   518.    -- This is for tracing a return from Director;
   519.    --    the flag does not correspond to any RFIR bit.
   520.    EXITD_flag : constant KDF9.interrupt_number := 30;
   521.
   522.    -- abandon_this_order is raised when an interrupt is punted to Director.
   523.    abandon_this_order : exception;
   524.
   525.    --
   526.    -- The following are to do with the K5 order.
   527.    --
   528.
   529.    -- The Program Hold-Up register is internal to I/O Control.
   530.    -- Get PHUi (bits D6i .. 6i+5), i = 0 .. 3.
   531.
   532.    function get_K5_operand
   533.    return KDF9.word;
   534.
   535.
   536.    --
   537.    -- The following are to do with management of the CPU's internal state.
   538.    --
   539.
   540.    type CPU_state is (Director_state, program_state);
   541.
   542.    the_CPU_state : KDF9.CPU_state;
   543.
   544.    procedure reset_the_CPU_state;
   545.
   546.    procedure reset_the_internal_registers (the_new_state : in CPU_state);
   547.
   548.    procedure fail_in_problem_program_state;
   549.
   550.    procedure LOV_if_user_mode (device_name : in String);
   551.
   552.    procedure return_from_Director_to (new_IAR : in KDF9.syllable_address);
   553.
   554.    procedure effect (this_interrupt : in KDF9.interrupt_number; message : in String := "")
   555.       with Inline => False;
   556.
   557.    procedure check_for_a_clock_interrupt
   558.       with Inline;
   559.
   560.    procedure trap_invalid_instruction (the_message : in String := "invalid opcode")
   561.       with Inline => False;
   562.
   563.    procedure trap_invalid_operand (the_message : in String := "invalid operand")
   564.       with Inline => False, No_Return;
   565.
   566.    procedure trap_operator_error (the_message : in String)
   567.       with Inline => False, No_Return;
   568.
   569.    procedure trap_unimplemented_feature (the_message : in String)
   570.       with Inline => False, No_Return;
   571.
   572.    procedure trap_invalid_paper_tape (the_message : in String)
   573.       with Inline => False, No_Return;
   574.
   575.    procedure reset_the_program_state;
   576.
   577.
   578.    --
   579.    --
   580.    -- Instruction fetch and decode.
   581.    --
   582.    --
   583.
   584.    -- These Instruction Address Registers are the nearest KDF9 has
   585.    --    to a conventional 'Program Counter' register.
   586.    -- NIA is significant only after an instruction has been decoded.
   587.
   588.    function NIA
   589.    return KDF9.syllable_address  -- the Next Instruction Address
   590.       with Inline;
   591.
   592.    function NIA_word_number
   593.    return KDF9.order_word_number
   594.       with Inline;
   595.
   596.    CIA : KDF9.syllable_address;  -- the Current Instruction Address
   597.
   598.    -- IWB0 and IWB1 in KDF9 contained the current 2 instruction words.
   599.    -- A 'short' loop, initiated by the JCqNZS instruction, ran entirely
   600.    --    inside the IWBs, obviating repeated instruction-fetch overhead.
   601.    -- Director exploits this in a loop that zeroizes the whole of core,
   602.    --    including that loop, which runs, immune to overwriting, in the IWBs.
   603.
   604.    procedure set_NIA_to (new_NIA : in KDF9.syllable_address)
   605.       with Inline;
   606.
   607.    procedure set_NIA_to_the_INS_target_address
   608.       with Inline;
   609.
   610.    procedure set_IWB0_and_IWB1_for_a_JCqNZS_loop
   611.       with Inline;
   612.
   613.    procedure go_back_to_the_start_of_IWB0
   614.       with Inline;
   615.
   616.    procedure continue_after_JCqNZS
   617.       with Inline;
   618.
   619.    -- Bits 0-1 of every order indicate its type as follows.
   620.
   621.    type INS_kind is mod 2**2;
   622.
   623.    one_syllable_order : constant := 0;
   624.    two_syllable_order : constant := 1;
   625.    normal_jump_order  : constant := 2;
   626.    data_access_order  : constant := 3;
   627.
   628.    type decoded_order is
   629.       record
   630.          order : KDF9.syllable_group := (0, 0, 0);
   631.          kind  : KDF9.INS_kind := 0;
   632.
   633.          -- The compressed_opcode is:
   634.          --    bits 2-7 of 1- and 2-syllable orders
   635.          --    bits 2-3|8-11 of normal jumps
   636.          --    bits 5-7 of SET and directly-addressed store access orders.
   637.          -- See the KDF9.decoding package.
   638.          compressed_opcode : KDF9.compressed_opcode := 0;
   639.
   640.           -- Qq is bits 8-11, Qk is bits 12-15.
   641.          Qq, Qk : KDF9.Q_number := 0;
   642.
   643.          -- For an jump instruction, syllable_index is bits 5-7.
   644.          target : KDF9.syllable_address;
   645.
   646.          -- For a data address or value (SET), operand is bits 2-4|12-23.
   647.          operand : KDF9.Q_part := 0;
   648.       end record;
   649.
   650.    INS : KDF9.decoded_order;  -- analogous to the INS register in Main Control
   651.
   652.    -- After decode_the_next_order:
   653.    --    INS contains the whole instruction at the address given by CIA,
   654.    --       with its components unpacked (not all are significant in every case).
   655.
   656.    procedure decode_the_next_order
   657.       with Inline;
   658.
   659.    procedure decode (the_order : in out KDF9.decoded_order)
   660.       with Inline;
   661.
   662.    procedure process_syllable_0_of_INS
   663.       with Inline;
   664.
   665.    procedure process_syllable_1_of_INS
   666.       with Inline;
   667.
   668.    procedure process_syllables_1_and_2_of_a_jump_order
   669.       with Inline;
   670.
   671.    procedure process_syllables_1_and_2_of_a_data_access_order
   672.       with Inline;
   673.
   674.    -- the_order_at_NIA gets three syllables starting at [NIA].
   675.    -- It is FOR DIAGNOSTIC USE ONLY!
   676.    -- It does NOT update the CPU time properly and must not be used inside an instruction cycle!
   677.
   678.    function the_order_at_NIA
   679.    return KDF9.syllable_group
   680.       with Inline;
   681.
   682.    -- Save E0U, lest the initial jump in E0 be corrupted during the run.
   683.    procedure save_the_initial_jump;
   684.
   685.    -- Restore E0U to its saved value.
   686.    procedure restore_the_initial_jump;
   687.
   688.    -- Check whether E0U has changed.
   689.    function the_initial_jump_was_corrupted
   690.    return Boolean;
   691.
   692.    -- True if the parameter is not a valid KDF9 instruction.
   693.
   694.    function is_an_invalid_order (decoded : KDF9.decoded_order)
   695.    return Boolean;
   696.
   697.
   698.    --
   699.    -- The Instruction Counter Register, ICR, (N.B. NOT a 'PROGRAM counter')
   700.    --   indicates the number of instructions executed by the KDF9.
   701.    --
   702.
   703.    type order_counter is mod 2**64;
   704.
   705.    ICR : KDF9.order_counter := 0;
   706.
   707.
   708.    --
   709.    -- The following support hashed execution-signature checking,
   710.    --    mainly for self-checking of new versions and ports.
   711.    --
   712.
   713.    function the_digital_signature
   714.    return KDF9.word;
   715.
   716.    procedure update_the_digital_signature
   717.       with Inline;
   718.
   719. private
   720.
   721.    the_elapsed_time    : KDF9.us := 0;
   722.    the_last_delay_time : KDF9.us := 0;
   723.
   724.    fetching_normally   : Boolean := True;
   725.
   726. end KDF9;

 1028 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/state_display.adb
Source file time stamp: 2020-11-03 21:46:38
Compiled at: 2020-11-12 18:12:11

     1. -- state_display.adb
     2. --
     3. -- Provide the comprehensive machine-state display panel KDF9 never had.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Characters.Latin_1;
    20. with Ada.Exceptions;
    21. with Ada.Long_Float_Text_IO;
    22. --
    23. with disassembly;
    24. with dumping;
    25. with exceptions;
    26. with formatting;
    27. with generic_sets;
    28. with HCI;
    29. with IOC;
    30. with KDF9_char_sets;
    31. with KDF9.CPU;
    32. with KDF9.decoding;
    33. with KDF9.PHU_store;
    34. with KDF9.store;
    35. with logging.file;
    36. with settings;
    37. with tracing;
    38.
    39.
    40. use  Ada.Characters.Latin_1;
    41. use  Ada.Exceptions;
    42. use  Ada.Long_Float_Text_IO;
    43. --
    44. use  disassembly;
    45. use  dumping;
    46. use  exceptions;
    47. use  formatting;
    48. use  HCI;
    49. use  IOC;
    50. use  KDF9_char_sets;
    51. use  KDF9.CPU;
    52. use  KDF9.decoding;
    53. use  KDF9.PHU_store;
    54. use  KDF9.store;
    55. use  logging.file;
    56. use  settings;
    57. use  tracing;
    58.
    59. package body state_display is
    60.
    61.    procedure show_IM_parts (the_Q_register : in KDF9.Q_register;
    62.                             width          : in Positive := 8) is
    63.    begin
    64.       log(
    65.           "/"
    66.         & just_right("#" & oct_of(the_Q_register.I, width-2), width)
    67.         & "/"
    68.         & just_right("#" & oct_of(the_Q_register.M, width-2), width)
    69.          );
    70.    end show_IM_parts;
    71.
    72.    procedure show_IO_register (the_Q_register : in KDF9.Q_register;
    73.                                width          : in Positive := 8;
    74.                                for_DR,
    75.                                for_FD,
    76.                                for_FH,
    77.                                for_seek       : in Boolean  := False) is
    78.    begin
    79.       log('Q');
    80.       if for_FD then
    81.          log(just_right(as_FD_command(the_Q_register, for_seek, for_FH), width));
    82.       elsif for_DR then
    83.          log(just_right(as_DR_command(the_Q_register), width));
    84.       else
    85.          log(just_right("#" & oct_of(the_Q_register.C, width-2), width));
    86.       end if;
    87.       show_IM_parts(the_Q_register, width);
    88.    end show_IO_register;
    89.
    90.    procedure show_Q_register (the_Q_register : in KDF9.Q_register;
    91.                               width          : in Positive := 8) is
    92.    begin
    93.       log('Q' & just_right("#" & oct_of(the_Q_register.C, width-2), width));
    94.       show_IM_parts(the_Q_register, width);
    95.    end show_Q_register;
    96.
    97.    procedure show_Q_in_decimal (the_Q_register : in KDF9.Q_register;
    98.                                 width          : in Positive := 7) is
    99.    begin
   100.       log(
   101.           'Q'
   102.         & just_right(CPU.signed_Q_part'Image(resign(the_Q_register.C)), width)
   103.         & "/"
   104.         & just_right(CPU.signed_Q_part'Image(resign(the_Q_register.I)), width)
   105.         & "/"
   106.         & just_right(CPU.signed_Q_part'Image(resign(the_Q_register.M)), width)
   107.          );
   108.    end show_Q_in_decimal;
   109.
   110.    procedure show_in_syllables_form (the_word : in KDF9.word) is
   111.       word : KDF9.word := the_word;
   112.       syllable : KDF9.syllable;
   113.    begin
   114.       for b in 0 .. 5 loop
   115.          word := rotate_word_left(word, 8);
   116.          syllable := KDF9.syllable(word and 8#377#);
   117.          log("#" & just_right(oct_of(syllable), 3) & " ");
   118.       end loop;
   119.    end show_in_syllables_form;
   120.
   121.    procedure show_as_characters (the_word : in KDF9.word) is
   122.       word : KDF9.word := the_word;
   123.       data : String(1 .. 8);
   124.    begin
   125.       for b of reverse data loop
   126.          b := glyph_for(to_CP(KDF9_char_sets.symbol(word and 8#77#)));
   127.          word := shift_logical(word, -6);
   128.       end loop;
   129.       log(data);
   130.    end show_as_characters;
   131.
   132.   procedure show_in_various_formats (the_word : in KDF9.word;
   133.                                      column   : in Positive := 5) is
   134.       image : String(1 .. 21);
   135.    begin
   136.       log_octal(the_word);
   137.       log(" = " & just_right(trimmed(CPU.signed'Image(resign(the_word))), 16) & " = ");
   138.       Put(image, host_float(CPU.f48(the_word)), Aft => 12, Exp => 2);
   139.       log(trimmed(image) & " = ");
   140.       log(as_fraction(the_word)'Image);
   141.       log_new_line;
   142.       tab_log_to(column);
   143.       log(" = ");
   144.       show_Q_register(as_Q(the_word));
   145.       log("   = ");
   146.       show_Q_in_decimal(as_Q(the_word));
   147.       log_new_line;
   148.       tab_log_to(column);
   149.       log(" = ");
   150.       show_in_syllables_form(the_word);
   151.       log("= """);
   152.       show_as_characters(the_word);
   153.       log("""");
   154.    end show_in_various_formats;
   155.
   156.    procedure show_progress is
   157.
   158.       function readable (t : KDF9.us)
   159.       return String is
   160.          t_plus_5E2 : constant KDF9.us := (t + 5E2)/ 1E3;
   161.          t_plus_5E5 : constant KDF9.us := (t + 5E5)/ 1E6;
   162.       begin
   163.          return (if t < 1E3 then
   164.                     ""
   165.               elsif t < 1E6 then
   166.                     " about" & t_plus_5E2'Image & " ms"
   167.               else  " about" & t_plus_5E5'Image & " sec" );
   168.       end readable;
   169.
   170.       CPU : constant String := " KDF9 us. (RAN)" & readable(the_CPU_time);
   171.       EL  : constant String := " KDF9 us. (EL) " & readable(the_clock_time);
   172.
   173.    begin
   174.       log_line("ORDERS:     " & just_right(ICR'Image, 10) & " executed (ICR)");
   175.       log_line("CPU TIME:   " & just_right(the_CPU_time'Image, 10) & CPU);
   176.       log_line("CLOCK TIME: " & just_right(KDF9.us'Image(the_clock_time), 10) & EL);
   177.    end show_progress;
   178.
   179.    slot_name : constant array (KDF9.context) of String(1..1)  := ("P", "Q", "R", "S");
   180.
   181.    procedure show_Director_registers is
   182.       interval : constant KDF9.us := the_clock_time - the_last_K4_time;
   183.    begin
   184.       log_line("The CPU is in " & the_CPU_state'Image);
   185.       log_line("CONTEXT:  " & slot_name(the_context));
   186.       log_line("PRIORITY: " & just_right(CPL'Image, 1));
   187.       log_line("BA:       " & just_right("#" & oct_of(BA), 6));
   188.       log_line("NOL:      " & just_right("#" & oct_of(NOL), 6));
   189.       log("CPDAR:    ");
   190.       for i in KDF9.buffer_number loop
   191.          log(if the_CPDAR(i) then device_name_of(buffer(i).all) & " " else "");
   192.       end loop;
   193.       log_new_line;
   194.       log_new_line;
   195.       log_line("PHU stores:");
   196.       for p in KDF9.priority loop
   197.          log("PHU" & p'Image & " is ");
   198.          if PHU(p).is_held_up then
   199.             if PHU(p).blockage.reason = buffer_busy then
   200.                log("waiting for " & device_name_of(IOC.device_number(PHU(p).blockage.buffer_nr)));
   201.                log(" on buffer #" & oct_of(PHU(p).blockage.buffer_nr, 2));
   202.                if PHU(p).blockage.by_INTQq then
   203.                   log(", because of INTQq");
   204.                end if;
   205.             else
   206.                log("locked out of group" & KDF9.store.group_address'Image(PHU(p).blockage.group_nr));
   207.             end if;
   208.          else
   209.             log("idle");
   210.          end if;
   211.          log_new_line;
   212.       end loop;
   213.       log_new_line;
   214.       log_line("RFIR (Interrupt Flags):");
   215.       log_line(
   216.                "CLOCK:    "
   217.              & Boolean'Image(interval >= 2**20)
   218.              & "; time since last CLOCK interrupt ="
   219.              & KDF9.us'Image(interval)
   220.              & " KDF9 us."
   221.               );
   222.       log_line("PR:       " & Boolean'Image(the_RFIR(PR_interrupt)));
   223.       log_line("FLEX:     " & Boolean'Image(the_RFIR(FLEX_interrupt)));
   224.       log_line("LIV:      " & Boolean'Image(the_RFIR(LIV_interrupt)));
   225.       log_line("NOUV:     " & Boolean'Image(the_RFIR(NOUV_interrupt)));
   226.       log_line("EDT:      " & Boolean'Image(the_RFIR(EDT_interrupt)));
   227.       log_line("OUT:      " & Boolean'Image(the_RFIR(OUT_interrupt)));
   228.       log_line("LOV:      " & Boolean'Image(the_RFIR(LOV_interrupt)));
   229.       log_line("RESET:    " & Boolean'Image(the_RFIR(RESET_interrupt)));
   230.    end show_Director_registers;
   231.
   232.    procedure show_V_and_T is
   233.    begin
   234.       if the_V_bit_is_set or the_T_bit_is_set then
   235.          log_new_line;
   236.          if the_V_bit_is_set then
   237.             log("V is set. ");
   238.          else
   239.             log("V is clear. ");
   240.          end if;
   241.          if the_T_bit_is_set then
   242.             log("T is set. ");
   243.          else
   244.             log("T is clear. ");
   245.          end if;
   246.          log_new_line;
   247.       end if;
   248.    end show_V_and_T;
   249.
   250.    procedure show_nest is
   251.    begin
   252.       if the_nest_depth = 0 then
   253.          log_line("The NEST is empty.");
   254.       else
   255.          log_line("NEST:");
   256.          for i in reverse KDF9.nest_depth loop
   257.             if i < the_nest_depth then
   258.                log(just_right("N" & trimmed(KDF9.nest_depth'Image(the_nest_depth-i)), 3) & ": ");
   259.                log_new_line;
   260.                show_in_various_formats(the_nest(i));
   261.                log_new_line;
   262.             end if;
   263.          end loop;
   264.       end if;
   265.    end show_nest;
   266.
   267.    procedure show_sjns is
   268.    begin
   269.       if the_sjns_depth = 0 then
   270.          log_line("The SJNS is empty.");
   271.       else
   272.          log_line("SJNS:");
   273.       end if;
   274.       for i in reverse KDF9.sjns_depth loop
   275.          if i < the_sjns_depth then
   276.             log(just_right("S" & trimmed(KDF9.sjns_depth'Image(the_sjns_depth-i)), 3) & ": ");
   277.             log_line(oct_of(the_sjns(i)) & " (" & dec_of(KDF9.syllable_address(the_sjns(i))) & ")");
   278.          end if;
   279.       end loop;
   280.    end show_sjns;
   281.
   282.    procedure show_Q_store is
   283.       Q_bits  : KDF9.word := 0;
   284.    begin
   285.       for Qq of the_Q_store loop
   286.          Q_bits := Q_bits or as_word(Qq);
   287.       end loop;
   288.       if Q_bits = 0 then
   289.          log_line("Q store: all zero.");
   290.          return;
   291.       else
   292.          log_line("Q store:");
   293.       end if;
   294.       for q in KDF9.Q_store'Range loop
   295.          if as_word(the_Q_store(q)) /= KDF9.word'(0) then
   296.             log(just_right("Q" & trimmed(q'Image), 3) & ": ");
   297.             show_Q_register(the_Q_store(q));
   298.             log("  = ");
   299.             show_Q_in_decimal(the_Q_store(q));
   300.             log_new_line;
   301.          end if;
   302.       end loop;
   303.    end show_Q_store;
   304.
   305.    procedure show_registers is
   306.    begin
   307.       show_progress;
   308.       log_new_line;
   309.       if the_execution_mode = boot_mode then
   310.          show_Director_registers;
   311.          log_new_line;
   312.       end if;
   313.       show_sjns;
   314.       log_new_line;
   315.       show_Q_store;
   316.       show_V_and_T;
   317.       log_new_line;
   318.       show_nest;
   319.    end show_registers;
   320.
   321.    procedure show_order is
   322.    begin
   323.       log(the_code_and_name_of_INS);
   324.    end show_order;
   325.
   326.    procedure show_execution_context is
   327.    begin
   328.       log("At "
   329.         & oct_of(CIA)
   330.         & " ("
   331.         & dec_of(CIA)
   332.         & ")"
   333.         & "; ICR ="
   334.         & ICR'Image
   335.         & "; EL ="
   336.         & the_clock_time'Image
   337.         & "; the instruction was ");
   338.       show_order;
   339.       log_new_line;
   340.    end show_execution_context;
   341.
   342.    procedure log_to_external_trace is
   343.    begin
   344.       log(the_external_trace_file, oct_of(CIA));
   345.       tab_log_to(the_external_trace_file, 10);
   346.       log(the_external_trace_file, ICR'Image);
   347.       tab_log_to(the_external_trace_file, 20);
   348.       if only_signature_tracing then
   349.          log(
   350.              the_external_trace_file,
   351.              "#"
   352.            & oct_of(the_digital_signature)
   353.            & (if the_V_bit_is_set then "V" else " ")
   354.            & (if the_T_bit_is_set then "T" else " ")
   355.             );
   356.          tab_log_to(the_external_trace_file, 40);
   357.          if the_nest_depth > 0 then
   358.             log(the_external_trace_file, "#" & oct_of(read_top));
   359.          end if;
   360.          tab_log_to(the_external_trace_file, 58);
   361.       else
   362.          log(the_external_trace_file, the_CPU_time'Image);
   363.          tab_log_to(the_external_trace_file, 40);
   364.          log(the_external_trace_file, the_nest_depth'Image);
   365.          tab_log_to(the_external_trace_file, 43);
   366.          log(the_external_trace_file, the_sjns_depth'Image);
   367.          tab_log_to(the_external_trace_file, 46);
   368.          log(the_external_trace_file, (if the_V_bit_is_set then "V" else " "));
   369.          log(the_external_trace_file, (if the_T_bit_is_set then "T" else " "));
   370.          tab_log_to(the_external_trace_file, 50);
   371.          if the_nest_depth > 0 then
   372.             log(the_external_trace_file, "#" & oct_of(read_top));
   373.          end if;
   374.          tab_log_to(the_external_trace_file, 68);
   375.       end if;
   376.       log(the_external_trace_file, " |" & the_full_name_of(INS));
   377.       tab_log_to(the_external_trace_file, 90);
   378.       log(the_external_trace_file, KDF9.us'Image(the_clock_time));
   379.       log_new_line(the_external_trace_file);
   380.    end log_to_external_trace;
   381.
   382.    procedure log_an_external_trace_header is
   383.    begin
   384.       log(the_external_trace_file, "LOCATION");
   385.       tab_log_to(the_external_trace_file, 11);
   386.       log(the_external_trace_file, "ICR");
   387.       tab_log_to(the_external_trace_file, 20);
   388.       if only_signature_tracing then
   389.          log(the_external_trace_file, "DIGITAL SIGNATURE");
   390.          tab_log_to(the_external_trace_file, 40);
   391.          log(the_external_trace_file, "[N1]");
   392.          tab_log_to(the_external_trace_file, 58);
   393.       else
   394.          log(the_external_trace_file, " CPU");
   395.          tab_log_to(the_external_trace_file, 40);
   396.          log(the_external_trace_file, "ND");
   397.          tab_log_to(the_external_trace_file, 43);
   398.          log(the_external_trace_file, "SD");
   399.          tab_log_to(the_external_trace_file, 46);
   400.          log(the_external_trace_file, "VT");
   401.          tab_log_to(the_external_trace_file, 50);
   402.          log(the_external_trace_file, "[N1]");
   403.          tab_log_to(the_external_trace_file, 68);
   404.       end if;
   405.       log(the_external_trace_file, " |INSTRUCTION");
   406.       log_new_line(the_external_trace_file);
   407.    end log_an_external_trace_header;
   408.
   409.    procedure show_CIA_and_NIA is
   410.    begin
   411.       log_line("CIA:        " & just_right(oct_of(CIA), 10) & " (" & just_right(dec_of(CIA) & ")"));
   412.       log_line("NIA:        " & just_right(oct_of(NIA), 10) & " (" & just_right(dec_of(NIA) & ")"));
   413.    end show_CIA_and_NIA;
   414.
   415.    procedure long_witness is
   416.    begin
   417.       log_new_line;
   418.       log("At " & oct_of(CIA) & " (" & dec_of(CIA) & ") the instruction was ");
   419.       show_order;
   420.       log_new_line;
   421.       show_registers;
   422.    end long_witness;
   423.
   424.    procedure short_witness is
   425.
   426.       type register_usage is array (KDF9.compressed_opcode) of Boolean
   427.          with Size => 64, Component_Size => 1;
   428.
   429.       it_uses_JB : constant register_usage
   430.                  := (
   431.                       LINK
   432.                     | TO_LINK
   433.                     | OS_OUT
   434.                     | JrNEJ
   435.                     | JSr
   436.                     | EXIT_n
   437.                     | JrEJ
   438.                     | EXITD     => True,
   439.                       others    => False
   440.                     );
   441.
   442.       it_uses_Qq : constant register_usage
   443.                  := (
   444.                       MkMq
   445.                     | MkMqQ
   446.                     | MkMqH
   447.                     | MkMqQH
   448.                     | MkMqN
   449.                     | MkMqQN
   450.                     | MkMqHN
   451.                     | MkMqQHN
   452.                     | TO_MkMq
   453.                     | TO_MkMqQ
   454.                     | TO_MkMqH
   455.                     | TO_MkMqQH
   456.                     | TO_MkMqN
   457.                     | TO_MkMqQN
   458.                     | TO_MkMqHN
   459.                     | TO_MkMqQHN
   460.                     | MqTOQk
   461.                     | IqTOQk
   462.                     | IMqTOQk
   463.                     | CqTOQk
   464.                     | CMqTOQk
   465.                     | CIqTOQk
   466.                     | QqTOQk
   467.                     | M_PLUS_Iq
   468.                     | M_MINUS_Iq
   469.                     | NCq
   470.                     | DCq
   471.                     | POS1_TO_Iq
   472.                     | NEG1_TO_Iq
   473.                     | POS2_TO_Iq
   474.                     | NEG2_TO_Iq
   475.                     | SHA
   476.                     | SHAD
   477.                     | MACC
   478.                     | SHL
   479.                     | SHLD
   480.                     | SHC
   481.                     | TO_RCIMq
   482.                     | QCIMq
   483.                     | ADD_TO_QCIMq
   484.                     | JCqNZS
   485.                     | PAR_Qq
   486.                     | PIA_PIC_CLO_TLO_Qq
   487.                     | PIB_PID_Qq
   488.                     | PIE_PIG_Qq
   489.                     | PIF_PIH_Qq
   490.                     | PMA_PMK_INT_Qq
   491.                     | CT_PMB_PMC_BUSY_Qq
   492.                     | PMD_PME_PML_Qq
   493.                     | PMF_PMG_Qq
   494.                     | POA_POC_POE_POF_PMH_Qq
   495.                     | POB_POD_Qq
   496.                     | POG_POL_Qq
   497.                     | POH_POK_Qq
   498.                     | JrCqNZ    => True,
   499.                       others    => False
   500.                     );
   501.
   502.       is_modified : constant register_usage
   503.                   := (
   504.                        EaMq
   505.                      | TO_EaMq
   506.                      | EaMqQ
   507.                      | TO_EaMqQ  => True,
   508.                        others    => False
   509.                      );
   510.
   511.       it_uses_Qk : constant register_usage
   512.                  := (
   513.                       MkMq
   514.                     | MkMqQ
   515.                     | MkMqH
   516.                     | MkMqQH
   517.                     | MkMqN
   518.                     | MkMqQN
   519.                     | MkMqHN
   520.                     | MkMqQHN
   521.                     | TO_MkMq
   522.                     | TO_MkMqQ
   523.                     | TO_MkMqH
   524.                     | TO_MkMqQH
   525.                     | TO_MkMqN
   526.                     | TO_MkMqQN
   527.                     | TO_MkMqHN
   528.                     | TO_MkMqQHN
   529.                     | MqTOQk
   530.                     | IqTOQk
   531.                     | IMqTOQk
   532.                     | CqTOQk
   533.                     | CMqTOQk
   534.                     | CIqTOQk
   535.                     | QqTOQk    => True,
   536.                       others    => False
   537.                     );
   538.
   539.       function INS_uses_Qq
   540.       return Boolean is
   541.          (
   542.           -- A compressed_opcode may be ambiguous: to know which opcode it represents,
   543.           --   further attributes of the order may need to be considered.
   544.           case INS.kind is
   545.              when two_syllable_order =>
   546.                 it_uses_Qq(INS.compressed_opcode)
   547.                   and
   548.                 -- If a shift, exclude fixed-amount shifts.
   549.                 ((INS.order.syllable_1 and 1) = 0 or else INS.compressed_opcode not in SHA..SHC),
   550.              when normal_jump_order =>
   551.                 INS.compressed_opcode in JrCqZ | JrCqNZ,
   552.              when data_access_order =>
   553.                 is_modified(INS.compressed_opcode),
   554.              when others =>
   555.                 False
   556.          );
   557.
   558.    begin  -- short_witness
   559.       log_new_line;
   560.       show_execution_context;
   561.       if the_CPU_state = Director_state then
   562.          show_Director_registers;
   563.       end if;
   564.       if it_uses_JB(INS.compressed_opcode)                     and then
   565.             INS.kind in two_syllable_order | normal_jump_order and then
   566.                the_sjns_depth > 0                                  then
   567.          log_line(
   568.                   " JB: "
   569.                 & oct_of(the_sjns(the_sjns_depth-1))
   570.                 & "; SJNS depth: " & just_right(the_sjns_depth'Image, 3)
   571.                  );
   572.       end if;
   573.       if INS.Qq /= 0 and then
   574.             INS_uses_Qq  then
   575.          log(just_right("Q" & trimmed(INS.Qq'Image), 3) & ": ");
   576.          show_Q_register(the_Q_store(INS.Qq));
   577.          log("  = ");
   578.          show_Q_in_decimal(the_Q_store(INS.Qq));
   579.          log_new_line;
   580.       end if;
   581.       if INS.Qk /= 0                       and then
   582.             INS.kind in two_syllable_order and then
   583.                it_uses_Qk(INS.compressed_opcode)    and then
   584.                   INS.Qq /= INS.Qk             then
   585.          log(just_right("Q" & trimmed(INS.Qk'Image), 3) & ": ");
   586.          show_Q_register(the_Q_store(INS.Qk));
   587.          log("  = ");
   588.          show_Q_in_decimal(the_Q_store(INS.Qk));
   589.          log_new_line;
   590.          log_new_line;
   591.       end if;
   592.       show_V_and_T;
   593.       show_nest;
   594.       log_rule;
   595.    end short_witness;
   596.
   597.    procedure show_frequency_plots is
   598.
   599.       function summed_counts (from, to : KDF9.syllable)
   600.       return KDF9.order_counter is
   601.          sum : KDF9.order_counter := 0;
   602.       begin
   603.          for i in from .. to loop
   604.             sum := sum + the_histogram(i);
   605.          end loop;
   606.          return sum;
   607.       end summed_counts;
   608.
   609.       procedure log_opcode_bin (bin    : in KDF9.syllable;
   610.                                 sum    : in KDF9.order_counter;
   611.                                 bound  : in Long_Float) is
   612.          percent : Long_Float;
   613.          image   : String(1 .. 6);
   614.       begin
   615.          if sum /= 0 then
   616.             percent := Long_Float(sum)/Long_Float(ICR)*100.0;
   617.             if percent < bound then
   618.                return;
   619.             end if;
   620.             log(oct_of(bin) & ": " & the_short_name_of(bin));
   621.             tab_log_to(32);
   622.             log(sum'Image);
   623.             tab_log_to(42);
   624.             Put(image, percent, Aft => 2, Exp => 0);
   625.             log(image & "% :");
   626.             for i in 1 .. Integer(percent) loop
   627.                log("|");
   628.             end loop;
   629.             log_new_line;
   630.          end if;
   631.       end log_opcode_bin;
   632.
   633.       procedure log_opcode_usage (bound : in Long_Float) is
   634.       begin
   635.          for i in KDF9.syllable'(0) .. 8#167# loop
   636.             log_opcode_bin(i, the_histogram(i), bound);
   637.          end loop;
   638.          for i in KDF9.syllable'(8#170#) .. 8#237# loop
   639.             log_opcode_bin(i, the_histogram(i), bound);
   640.          end loop;
   641.          log_opcode_bin(8#240#, summed_counts(from => 8#240#, to => 8#257#), bound);
   642.          log_opcode_bin(8#260#, summed_counts(from => 8#240#, to => 8#277#), bound);
   643.          for i in KDF9.syllable'(8#300#) .. 8#377# loop
   644.             log_opcode_bin(i, the_histogram(i), bound);
   645.          end loop;
   646.       end log_opcode_usage;
   647.
   648.       accounted_for : Long_Float;
   649.       cutoff_image  : String(1 .. 7) := "      %";
   650.       percent_image : String(1 .. 7) := "      %";
   651.
   652.       procedure log_order_word_bin (bin    : in KDF9.order_word_number;
   653.                                     sum    : in KDF9.order_counter;
   654.                                     bound  : in Long_Float) is
   655.          percent : Long_Float;
   656.       begin
   657.          if sum /= 0 then
   658.             percent := Long_Float(sum)/Long_Float(ICR)*100.0;
   659.             if percent < bound then
   660.                return;
   661.             end if;
   662.             accounted_for := accounted_for + percent;
   663.             log("#" & oct_of(bin) & ": ");
   664.             tab_log_to(32);
   665.             log(sum'Image);
   666.             tab_log_to(42);
   667.             Put(percent_image, percent, Aft => 2, Exp => 0);
   668.             percent_image(7) := '%';
   669.             log(percent_image);
   670.             log(" :");
   671.             for i in 1 .. Integer(percent) loop
   672.                log("|");
   673.             end loop;
   674.             log_new_line;
   675.          end if;
   676.       end log_order_word_bin;
   677.
   678.       procedure log_profile (bound : in Long_Float) is
   679.       begin
   680.          accounted_for := 0.0;
   681.          for w in KDF9.order_word_number loop
   682.             if the_profile(w) /= 0 then
   683.                log_order_word_bin(w, the_profile(w), bound);
   684.             end if;
   685.          end loop;
   686.       end log_profile;
   687.
   688.       procedure sum_logged_frequencies (bound  : in Long_Float) is
   689.          percent : Long_Float;
   690.       begin
   691.          accounted_for := 0.0;
   692.          for w in KDF9.order_word_number loop
   693.             percent := Long_Float(the_profile(w))/Long_Float(ICR)*100.0;
   694.             if percent >= bound then
   695.                accounted_for := accounted_for + percent;
   696.             end if;
   697.          end loop;
   698.       end sum_logged_frequencies;
   699.
   700.    begin -- show_frequency_plots
   701.       Put(cutoff_image(1..6), histogram_cutoff, Aft => 2, Exp => 0);
   702.       cutoff_image(7) := '%';
   703.       if the_INS_plot_is_wanted and ICR /= 0 and the_diagnostic_mode /= fast_mode then
   704.          -- Print the instruction execution-frequency histogram.
   705.          log_title(
   706.                    "Histogram of the opcodes of"
   707.                  & ICR'Image
   708.                  & " executed instructions with frequency >="
   709.                  & cutoff_image
   710.                   );
   711.          log_opcode_usage(bound => histogram_cutoff);
   712.          log_new_line;
   713.       end if;
   714.       if the_profile_is_wanted and ICR /= 0 and the_diagnostic_mode /= fast_mode then
   715.          log_title(
   716.                    "Histogram of the loci of"
   717.                  & ICR'Image
   718.                  & " executed instructions with frequency >="
   719.                  & cutoff_image
   720.                   );
   721.          log_profile(bound => histogram_cutoff);
   722.          log_new_line;
   723.       end if;
   724.       sum_logged_frequencies(bound => histogram_cutoff);
   725.       Put(percent_image(1..6), accounted_for, Aft =>1, Exp => 0);
   726.       log_line("Executions accounted for in the profile:" & percent_image);
   727.       log_rule;
   728.    end show_frequency_plots;
   729.
   730.    function as_RFIR (K4_word : KDF9.word)
   731.    return KDF9.RFIR is
   732.       mask : KDF9.word := 2**16;
   733.       RFIR : KDF9.RFIR := (others => False);
   734.    begin
   735.       for r in reverse KDF9.interrupt_number loop
   736.          if (K4_word and mask) /= 0 then
   737.             RFIR(r) := True;
   738.          end if;
   739.          mask := 2 * mask;
   740.       end loop;
   741.       return RFIR;
   742.    end as_RFIR;
   743.
   744.    function for_FH_disc (compressed_opcode : KDF9.compressed_opcode; Pxy_bits : KDF9.Q_number)
   745.    return Boolean
   746.    is (case compressed_opcode is
   747.           when PIA_PIC_CLO_TLO_Qq     => Pxy_bits = PIC_bits,
   748.           when PIB_PID_Qq             => Pxy_bits = PID_bits,
   749.           when PIE_PIG_Qq             => Pxy_bits = PIG_bits,
   750.           when PIF_PIH_Qq             => Pxy_bits = PIH_bits,
   751.           when POA_POC_POE_POF_PMH_Qq => Pxy_bits = POC_bits,
   752.           when POB_POD_Qq             => Pxy_bits = POD_bits,
   753.           when POG_POL_Qq             => Pxy_bits = POL_bits,
   754.           when POH_POK_Qq             => Pxy_bits = POK_bits,
   755.           when others                 => False
   756.       );
   757.
   758.
   759.    first_col   : constant := 17;
   760.    device_col  : constant := first_col + 20;
   761.    operand_col : constant := device_col;
   762.    event_col   : constant := operand_col + 4;
   763.    is_D_col    : constant := event_col + 29;
   764.    depth_col   : constant := operand_col + 29;
   765.    time_col    : constant := depth_col + 11;
   766.    ICR_col     : constant := time_col + 13;
   767.
   768.    procedure show_retro_FIFO is
   769.
   770.       RFIR_id : constant array (KDF9.interrupt_number) of Character
   771.               := ('P', 'F', 'I', 'N', 'E', 'S', 'O', 'R', 'Y', 'Z');
   772.       image   : String(1 .. 21);
   773.       RFIR    : KDF9.RFIR;
   774.    begin
   775.       if retro_FIFO_count = 0 then
   776.          return;
   777.       end if;
   778.       log_title("Retrospective trace of all instructions.");
   779.       tab_log_to(depth_col);
   780.       log_line("ND SD VTD   CPU TIME     ICR");
   781.       for i in 1 .. retro_FIFO_count loop
   782.          if i = 1 then
   783.             log("Ended ");
   784.          else
   785.             log("After ");
   786.          end if;
   787.          declare
   788.             this      : tracing.retro_FIFO_entry renames retro_FIFO(retro_FIFO_index);
   789.             Q         : constant KDF9.Q_register := as_Q(this.parameter);
   790.             decoded   : KDF9.decoded_order;
   791.          begin
   792.             log(oct_of(this.location) & ":");
   793.             tab_log_to(first_col);
   794.             decoded.order := this.order;
   795.             decode(decoded);
   796.             log(the_full_name_of(decoded,
   797.                                  octal_option => decoded.kind = normal_jump_order,
   798.                                  both_bases   => False));
   799.             tab_log_to(operand_col);
   800.             case decoded.kind is
   801.                when one_syllable_order =>
   802.                   if this.nested > 0 then
   803.                      case decoded.compressed_opcode is
   804.                         when DIV
   805.                            | DIVD
   806.                            | X_frac =>
   807.                            log(CPU.fraction'Image(as_fraction(this.parameter)));
   808.                         when DIVI =>
   809.                            log(CPU.signed'Image(resign(this.parameter)));
   810.                         when STAND
   811.                            | ABSF
   812.                            | DIVDF
   813.                            | DIVF
   814.                            | FLOAT_9
   815.                            | FLOATD
   816.                            | MINUSDF
   817.                            | MINUSF
   818.                            | NEGDF
   819.                            | NEGF
   820.                            | PLUSDF
   821.                            | PLUSF
   822.                            | ROUNDF
   823.                            | ROUNDHF
   824.                            | XDF
   825.                            | XF
   826.                            | XPLUSF
   827.                            | MAXF =>
   828.                            Put(image, host_float(CPU.f48(this.parameter)), Aft => 12, Exp => 2);
   829.                            log(trimmed(image));
   830.                         when others =>
   831.                            if this.nested > 0 then
   832.                               log_octal(this.parameter);
   833.                            end if;
   834.                      end case;
   835.                   end if;
   836.                when two_syllable_order =>
   837.                   case decoded.compressed_opcode is
   838.                      when PAR_Qq =>
   839.                         show_IO_register(Q, for_DR => False, for_FD => False);
   840.                      when CT_PMB_PMC_BUSY_Qq
   841.                         | PMA_PMK_INT_Qq
   842.                         | PMD_PME_PML_Qq
   843.                         | PMF_PMG_Qq =>
   844.                         show_IO_register(
   845.                                          Q,
   846.                                          for_DR   => device_kind_of(Q.C mod 16) = DR_kind,
   847.                                          for_FD   => device_kind_of(Q.C mod 16) = FD_kind,
   848.                                          for_seek => decoded.Qk = PMA_bits
   849.                                         );
   850.                      when PIA_PIC_CLO_TLO_Qq
   851.                         | PIB_PID_Qq
   852.                         | PIE_PIG_Qq
   853.                         | PIF_PIH_Qq
   854.                         | POA_POC_POE_POF_PMH_Qq
   855.                         | POB_POD_Qq
   856.                         | POG_POL_Qq
   857.                         | POH_POK_Qq =>
   858.                         show_IO_register(
   859.                                          Q,
   860.                                          for_DR   => device_kind_of(Q.C mod 16) = DR_kind,
   861.                                          for_FD   => device_kind_of(Q.C mod 16) = FD_kind,
   862.                                          for_FH   => for_FH_disc(decoded.compressed_opcode, decoded.Qk)
   863.                                         );
   864.                      when M_PLUS_Iq
   865.                         | M_MINUS_Iq
   866.                         | NCq
   867.                         | DCq
   868.                         | POS1_TO_Iq
   869.                         | NEG1_TO_Iq
   870.                         | POS2_TO_Iq
   871.                         | NEG2_TO_Iq
   872.                         | CqTOQk
   873.                         | IqTOQk
   874.                         | MqTOQk
   875.                         | QqTOQk
   876.                         | CIqTOQk
   877.                         | IMqTOQk
   878.                         | CMqTOQk
   879.                         | TO_RCIMq
   880.                         | ADD_TO_QCIMq
   881.                         | JCqNZS =>
   882.                         show_Q_register(Q);
   883.                      when Kk =>
   884.                         case decoded.Qk is
   885.                            when K4 =>
   886.                               log(KDF9.word'Image(32*KDF9.word(Q.C)));
   887.                               log("us");
   888.                               if Q.I /= 0 then
   889.                                  log("; RFIR: ");
   890.                                  RFIR := as_RFIR(this.parameter);
   891.                                  for r in KDF9.interrupt_number loop
   892.                                     if RFIR(r) then
   893.                                        log(RFIR_id(r)&"");
   894.                                     end if;
   895.                                  end loop;
   896.                               end if;
   897.                               if resign(this.parameter) < 0 then
   898.                                  log("C");
   899.                               end if;
   900.                            when K5 | K7 =>
   901.                               log_octal(this.parameter);
   902.                            when others =>
   903.                               raise emulation_failure
   904.                                  with "invalid K-group order in show_retro_FIFO";
   905.                         end case;
   906.                      when TO_LINK =>
   907.                         log(oct_of(as_link(this.parameter)));
   908.                      when LINK =>
   909.                         log(oct_of(as_link(this.parameter)));
   910.                      when TO_MkMq
   911.                         | TO_MkMqQ
   912.                         | TO_MkMqH
   913.                         | TO_MkMqQH
   914.                         | TO_MkMqN
   915.                         | TO_MkMqQN
   916.                         | TO_MkMqHN
   917.                         | TO_MkMqQHN =>
   918.                         log_octal(this.parameter);
   919.                      when others =>
   920.                         if this.nested > 0 then
   921.                            log_octal(this.parameter);
   922.                         end if;
   923.                   end case;
   924.                when normal_jump_order =>
   925.                   case decoded.compressed_opcode is
   926.                      when Jr
   927.                         | JSr =>
   928.                         log(oct_of(as_link(this.parameter)));
   929.                      when EXIT_n =>
   930.                         if this.parameter < 8 then
   931.                            log(this.parameter'Image);
   932.                         else
   933.                            log(oct_of(as_link(this.parameter)));
   934.                         end if;
   935.                      when EXITD =>
   936.                         log(oct_of(as_link(this.parameter)));
   937.                      when JrCqZ
   938.                         | JrCqNZ =>
   939.                         show_Q_register(Q);
   940.                      when OS_OUT =>
   941.                         if this.parameter < 16 then
   942.                            log_octal(this.parameter);
   943.                         elsif this.parameter < 64 then
   944.                            log(this.parameter'Image);
   945.                         elsif this.parameter > 2**47 then
   946.                            log_octal(this.parameter);
   947.                         else
   948.                            show_Q_register(Q);
   949.                         end if;
   950.                      when JrEJ
   951.                         | JrNEJ
   952.                         | JrEN
   953.                         | JrNEN =>
   954.                            log(this.parameter'Image);
   955.                      when JrTR
   956.                         | JrV =>
   957.                            log(Boolean'Image(Boolean'Val(this.parameter)));
   958.                      when JrNTR
   959.                         | JrNV =>
   960.                            log(Boolean'Image(not Boolean'Val(this.parameter)));
   961.                      when others =>
   962.                         if this.nested > 0 then
   963.                            log_octal(this.parameter);
   964.                         end if;
   965.                      end case;
   966.                when others =>
   967.                   if this.nested > 0 then
   968.                      log_octal(this.parameter);
   969.                   end if;
   970.             end case;
   971.             tab_log_to(depth_col);
   972.             log(just_right(this.nested'Image,2));
   973.             log(" ");
   974.             log(just_right(this.called'Image,2));
   975.             log(" ");
   976.             log(if this.V then "V" else " ");
   977.             log(if this.T then "T" else " ");
   978.             log(if this.D then "D" else " ");
   979.             tab_log_to(time_col);
   980.             log(this.CPU_time'Image);
   981.             tab_log_to(ICR_col);
   982.             log(this.ICR_value'Image);
   983.             log_new_line;
   984.          end;
   985.          retro_FIFO_index := retro_FIFO_index - 1;
   986.       end loop;
   987.       if retro_FIFO_count = FIFO_size then
   988.          log("After earlier instructions, whose tracing is now lost.");
   989.       else
   990.          log("After the start of traced execution.");
   991.       end if;
   992.       log_new_line;
   993.       log_rule;
   994.    end show_retro_FIFO;
   995.
   996.    the_final_ICR : KDF9.order_counter := 0;
   997.
   998.    procedure notify_termination is
   999.    begin
  1000.       the_final_ICR := ICR;
  1001.    end notify_termination;
  1002.
  1003.    procedure show_IOC_FIFO is
  1004.    begin
  1005.       if IOC_FIFO_count = 0 then return; end if;
  1006.       log_title("Retrospective trace of peripheral I/O events.");
  1007.       tab_log_to(is_D_col);
  1008.       log_line("CPL T   EL. TIME     ICR");
  1009.       for i in 1 .. IOC_FIFO_count loop
  1010.          if i = 1 then
  1011.             log("Ended ");
  1012.          else
  1013.             log("After ");
  1014.          end if;
  1015.
  1016.          declare
  1017.             this    : tracing.IOC_FIFO_entry renames IOC_FIFO(IOC_FIFO_index);
  1018.             decoded : constant KDF9.decoded_order := this.decoded_order;
  1019.
  1020.             procedure show_transfer (Q : in KDF9.Q_register) is
  1021.             begin
  1022.                case decoded.compressed_opcode is
  1023.                   when PAR_Qq =>
  1024.                      show_IO_register(Q, for_DR => False, for_FD => False);
  1025.                   when CT_PMB_PMC_BUSY_Qq
  1026.                      | PMA_PMK_INT_Qq
  1027.                      | PMD_PME_PML_Qq
  1028.                      | PMF_PMG_Qq =>
  1029.                      show_IO_register(
  1030.                                       Q,
  1031.                                       for_DR   => device_kind_of(Q.C mod 16) = DR_kind,
  1032.                                       for_FD   => device_kind_of(Q.C mod 16) = FD_kind,
  1033.                                       for_seek => decoded.Qk = PMA_bits
  1034.                                      );
  1035.                   when PIA_PIC_CLO_TLO_Qq
  1036.                      | PIB_PID_Qq
  1037.                      | PIE_PIG_Qq
  1038.                      | PIF_PIH_Qq
  1039.                      | POA_POC_POE_POF_PMH_Qq
  1040.                      | POB_POD_Qq
  1041.                      | POG_POL_Qq
  1042.                      | POH_POK_Qq =>
  1043.                      show_IO_register(
  1044.                                       Q,
  1045.                                       for_DR   => device_kind_of(Q.C mod 16) = DR_kind,
  1046.                                       for_FD   => device_kind_of(Q.C mod 16) = FD_kind,
  1047.                                       for_FH   => for_FH_disc(decoded.compressed_opcode, decoded.Qk)
  1048.                                      );
  1049.                   when OS_OUT =>
  1050.                      show_IO_register(Q, for_DR => False, for_FD => False);
  1051.                   when others =>
  1052.                      raise emulation_failure with "in show_IOC_FIFO.show_transfer";
  1053.                end case;
  1054.             end show_transfer;
  1055.
  1056.          begin
  1057.             log(oct_of(this.order_address) & ":");
  1058.             tab_log_to(first_col);
  1059.             if the_full_name_of(this.decoded_order) = "OUT" then
  1060.                 if this.device_name(1..2) in "MT" | "ST" and then
  1061.                       this.ICR_value >= the_final_ICR        then
  1062.                   log("final rewind");
  1063.                elsif this.device_name(1..2) in "MT" | "ST" then
  1064.                   log("OUT 6/7 rewind");
  1065.                elsif this.device_name(1..2) in "FW" | "LP" | "TP" then
  1066.                   log("OUT 8");
  1067.                else
  1068.                   log("OUT ?");
  1069.                end if;
  1070.             else
  1071.                log(mnemonic(the_full_name_of(this.decoded_order), this.device_name));
  1072.             end if;
  1073.             tab_log_to(device_col);
  1074.             log(this.device_name);
  1075.             case this.kind is
  1076.                when store_lockout =>
  1077.                   tab_log_to(event_col);
  1078.                   log("lockout at #");
  1079.                   log(oct_of(this.data_address));
  1080.                   log(" = E");
  1081.                   log(dec_of(this.data_address));
  1082.                   tab_log_to(is_D_col);
  1083.                   log(if this.is_for_Director then "D" else slot_name(this.context));
  1084.                   log(this.priority_level'Image);
  1085.                   tab_log_to(time_col);
  1086.                   log(this.initiation_time'Image);
  1087.                   tab_log_to(ICR_col);
  1088.                   log(this.ICR_value'Image);
  1089.                 when buffer_lockout =>
  1090.                   tab_log_to(event_col);
  1091.                   log("buffer lockout");
  1092.                   tab_log_to(is_D_col);
  1093.                   log(if this.is_for_Director then "D" else slot_name(this.context));
  1094.                   log(this.priority_level'Image);
  1095.                   tab_log_to(time_col);
  1096.                   log(this.initiation_time'Image);
  1097.                   tab_log_to(ICR_col);
  1098.                   log(this.ICR_value'Image);
  1099.                when start_transfer =>
  1100.                   tab_log_to(event_col);
  1101.                   show_transfer(this.control_word);
  1102.                   tab_log_to(is_D_col);
  1103.                   log(if this.is_for_Director then "D" else slot_name(this.context));
  1104.                   log(this.priority_level'Image);
  1105.                   tab_log_to(time_col-2);
  1106.                   log(" S"
  1107.                     & this.initiation_time'Image);
  1108.                   tab_log_to(ICR_col);
  1109.                   log(this.ICR_value'Image);
  1110.                when finis_transfer =>
  1111.                   tab_log_to(event_col);
  1112.                   show_transfer(this.control_word);
  1113.                   tab_log_to(is_D_col);
  1114.                   log(if this.is_for_Director then "D" else slot_name(this.context));
  1115.                   log(this.priority_level'Image);
  1116.                   tab_log_to(time_col-2);
  1117.                   log(" E"
  1118.                     & this.completion_time'Image);
  1119.                   tab_log_to(ICR_col);
  1120.                   log(this.ICR_value'Image);
  1121.                when buffer_status =>
  1122.                   tab_log_to(event_col);
  1123.                   show_Q_register(this.Q_register);
  1124.                   tab_log_to(is_D_col);
  1125.                   log(if this.is_for_Director then "D" else slot_name(this.context));
  1126.                   log(this.priority_level'Image);
  1127.                   log(if this.status then " Y" else " N");
  1128.                   tab_log_to(time_col);
  1129.                   log(this.initiation_time'Image);
  1130.                   tab_log_to(ICR_col);
  1131.                   log(this.ICR_value'Image);
  1132.             end case;
  1133.             log_new_line;
  1134.          end;
  1135.          IOC_FIFO_index := IOC_FIFO_index - 1;
  1136.       end loop;
  1137.       if IOC_FIFO_count = FIFO_size then
  1138.          log_line("After earlier instructions, whose tracing is now lost.");
  1139.       else
  1140.          log_line("After the start of traced execution.");
  1141.       end if;
  1142.       log_line("Total time waiting for unoverlapped I/O to finish ="
  1143.              & KDF9.us'Image((the_clock_time-the_CPU_time+500) / 1000)
  1144.              & "ms.");
  1145.       log_rule;
  1146.    end show_IOC_FIFO;
  1147.
  1148.    procedure show_interrupt_FIFO is
  1149.    begin
  1150.       if interrupt_FIFO_count = 0 then return; end if;
  1151.       log_title("Retrospective trace of interrupt requests.");
  1152.       tab_log_to(is_D_col);
  1153.       log_line("CPL     EL. TIME     ICR");
  1154.       for i in 1 .. interrupt_FIFO_count loop
  1155.          log(if i = 1 then "Ended " else "After ");
  1156.          declare
  1157.             this : tracing.interrupt_FIFO_entry renames interrupt_FIFO(interrupt_FIFO_index);
  1158.          begin
  1159.             log(oct_of(this.order_address) & ": ");
  1160.             tab_log_to(first_col);
  1161.             log(case this.interrupt_code is
  1162.                    when PR_interrupt     => "PR   ",
  1163.                    when FLEX_interrupt   => "FLEX ",
  1164.                    when LIV_interrupt    => "LIV  ",
  1165.                    when NOUV_interrupt   => "NOUV ",
  1166.                    when EDT_interrupt    => "EDT  ",
  1167.                    when OUT_interrupt    => "OUT  ",
  1168.                    when LOV_interrupt    => "LOV  ",
  1169.                    when RESET_interrupt  => "RESET",
  1170.                    when CLOCK_interrupt  => "CLOCK",
  1171.                    when EXITD_flag       => "EXITD"
  1172.                );
  1173.             tab_log_to(event_col-4);
  1174.             log(trimmed(this.message));
  1175.             tab_log_to(is_D_col);
  1176.             log(slot_name(this.context));
  1177.             log(this.priority_level'Image);
  1178.             tab_log_to(time_col);
  1179.             log(this.busy_time'Image);
  1180.             tab_log_to(ICR_col);
  1181.             log(this.ICR_value'Image);
  1182.             log_new_line;
  1183.          end;
  1184.          interrupt_FIFO_index := interrupt_FIFO_index - 1;
  1185.       end loop;
  1186.       log(
  1187.           if interrupt_FIFO_count = FIFO_size then
  1188.              "After earlier interrupts, whose tracing is now lost."
  1189.           else
  1190.             "After the start of traced execution."
  1191.          );
  1192.       log_new_line;
  1193.       log_new_line;
  1194.    end show_interrupt_FIFO;
  1195.
  1196.    procedure show_retrospective_traces is
  1197.    begin
  1198.       if the_peripheral_trace_is_enabled then
  1199.          pragma Debug(IOC.diagnosis);
  1200.       end if;
  1201.       if the_interrupt_trace_is_enabled then
  1202.          show_interrupt_FIFO;
  1203.       end if;
  1204.       if the_peripheral_trace_is_enabled then
  1205.          show_IOC_FIFO;
  1206.       end if;
  1207.       if the_retrospective_trace_is_enabled then
  1208.          show_retro_FIFO;
  1209.       end if;
  1210.    end show_retrospective_traces;
  1211.
  1212.    procedure show_current_state is
  1213.    begin
  1214.       show_execution_context;
  1215.       log_rule;
  1216.       show_registers;
  1217.       log_rule;
  1218.    end show_current_state;
  1219.
  1220.    procedure show_final_state (because : in String) is
  1221.    begin
  1222.       if the_final_state_is_wanted then
  1223.          if loading_was_successful then
  1224.             -- make sure there is at least one NL after any FW output.
  1225.             if the_log_is_wanted then
  1226.                log_new_line;
  1227.                log_rule;
  1228.             else
  1229.                log_new_line;
  1230.             end if;
  1231.             log_line("Final State: " & because & ".");
  1232.             if not the_log_is_wanted then return; end if;
  1233.             long_witness;
  1234.             log_rule;
  1235.
  1236.             if nr_of_post_dumping_areas /= 0 then
  1237.                log_title("Post-run Dump:");
  1238.                print_postrun_dump_areas;
  1239.             end if;
  1240.
  1241.             if the_INS_plot_is_wanted or the_profile_is_wanted then
  1242.                if the_histogram_is_enabled then
  1243.                   show_frequency_plots;
  1244.                end if;
  1245.             end if;
  1246.
  1247.             if the_peripheral_trace_is_enabled       or else
  1248.                   the_interrupt_trace_is_enabled     or else
  1249.                      the_retrospective_trace_is_enabled then
  1250.                log_title("Traces:");
  1251.             end if;
  1252.
  1253.             show_retrospective_traces;
  1254.
  1255.             if the_signature_is_enabled then
  1256.                log_title("Digital signature of traced orders = #"
  1257.                        & oct_of(the_digital_signature)
  1258.                        & ".");
  1259.             end if;
  1260.          else
  1261.             log_line("ee9 cannot run: " & because & ".");
  1262.             show_all_prerun_dump_areas;
  1263.             return;
  1264.          end if;
  1265.       end if;
  1266.    end show_final_state;
  1267.
  1268.    procedure show_all_prerun_dump_areas is
  1269.    begin
  1270.       if the_log_is_wanted and nr_of_pre_dumping_areas /= 0 then
  1271.          log_title("Pre-run Dump:");
  1272.          print_prerun_dump_areas;
  1273.          remove_prerun_dump_areas;
  1274.       end if;
  1275.    end show_all_prerun_dump_areas;
  1276.
  1277.    quantum     : constant := 8;
  1278.    jump_tab    : constant := 12;
  1279.    first_tab   : constant := 16;
  1280.    last_column : constant := 80;
  1281.
  1282.    function is_non_blank (first : in KDF9.address)
  1283.    return Boolean is
  1284.       result : Boolean := False;
  1285.    begin
  1286.       for address in first .. first+quantum-1 loop
  1287.          result := result or (fetch_word(address) /= 0);
  1288.       end loop;
  1289.       return result;
  1290.    end is_non_blank;
  1291.
  1292.    subtype converted_word is String(1..8);
  1293.
  1294.    type convertor is
  1295.       not null access function (address : KDF9.address) return converted_word;
  1296.
  1297.    procedure show_core (first, last : in KDF9.address;
  1298.                         head, side  : in String;
  1299.                         converted   : in convertor) is
  1300.
  1301.       procedure show_group (first : in KDF9.address) is
  1302.          address : KDF9.address := first;
  1303.       begin
  1304.          while address <= first+quantum-1 loop
  1305.             log(converted(address));
  1306.             address := address + 1;
  1307.             exit when address < first;
  1308.          end loop;
  1309.       end show_group;
  1310.
  1311.       address : KDF9.address := first;
  1312.
  1313.    begin
  1314.       if (last-first+1) < 1 then
  1315.          return;
  1316.       end if;
  1317.       BA := 0; -- Ensure that physical store is examined when running in boot mode.
  1318.       log_title("Core store [#" & oct_of(first) & " .. #" & oct_of(last) & "] interpreted as " & head & ":");
  1319.       while address <= last loop
  1320.          if is_non_blank(address) then
  1321.             log_octal(KDF9.field_of_16_bits(address));
  1322.             log(":");
  1323.             tab_log_to(jump_tab);
  1324.             log(side);
  1325.             log(" """);
  1326.             show_group(address);
  1327.             log("""");
  1328.             log_new_line;
  1329.          else
  1330.             log_line("========  blank  ========");
  1331.          end if;
  1332.       exit when address >= KDF9.address'Last - quantum;
  1333.          address := address + quantum;
  1334.       end loop;
  1335.       log_new_line;
  1336.    end show_core;
  1337.
  1338.    function encoding_of (address : KDF9.address; code_table : output_code_table)
  1339.    return converted_word is
  1340.       result : converted_word;
  1341.    begin
  1342.       for b in KDF9_char_sets.symbol_index loop
  1343.          result(Natural(b)+1) := glyph_for(code_table(fetch_symbol(address, b)));
  1344.       end loop;
  1345.       return result;
  1346.    end encoding_of;
  1347.
  1348.    current_case : KDF9_char_sets.symbol := KDF9_char_sets.Case_Normal;
  1349.
  1350.    function interpretation_of (address : KDF9.address)
  1351.    return converted_word is
  1352.       result : converted_word;
  1353.       symbol : KDF9_char_sets.symbol;
  1354.       char   : Character;
  1355.    begin
  1356.       for b in KDF9_char_sets.symbol_index loop
  1357.          symbol := fetch_symbol(address, b);
  1358.          if current_case = KDF9_char_sets.Case_Normal then
  1359.             char := TP_CN(symbol);
  1360.          else
  1361.             char := TP_CS(symbol);
  1362.          end if;
  1363.          if symbol = KDF9_char_sets.Case_Normal then
  1364.             current_case := KDF9_char_sets.Case_Normal;
  1365.          elsif symbol = KDF9_char_sets.Case_Shift then
  1366.             current_case := KDF9_char_sets.Case_Shift;
  1367.          end if;
  1368.          result(Natural(b)+1) := glyph_for(char);
  1369.       end loop;
  1370.       return result;
  1371.    end interpretation_of;
  1372.
  1373.    function case_visible (address : KDF9.address)
  1374.    return converted_word
  1375.    is (interpretation_of(address));
  1376.
  1377.    function case_normal (address : KDF9.address)
  1378.    return converted_word
  1379.    is (encoding_of(address, code_table => TP_CN));
  1380.
  1381.    function case_shift (address : KDF9.address)
  1382.    return converted_word
  1383.    is (encoding_of(address, code_table => TP_CS));
  1384.
  1385.    function printer_code (address : KDF9.address)
  1386.    return converted_word
  1387.    is (encoding_of(address, code_table => to_LP));
  1388.
  1389.    function card_code (address : KDF9.address)
  1390.    return converted_word
  1391.    is (encoding_of(address, code_table => to_CP));
  1392.
  1393.    function Latin_1_code (address : KDF9.address)
  1394.    return converted_word
  1395.    is (converted_word'(1..7 => Space,
  1396.                        8    => glyph_for(Character'Val(fetch_word(address) and 8#377#))));
  1397.
  1398.    procedure show_core_in_case_visible (first, last : in KDF9.address) is
  1399.    begin
  1400.       show_core(first, last,
  1401.                 head => "characters in TR/TP code with case shifting",
  1402.                 side => "  ",
  1403.                 converted => case_visible'Access);
  1404.    end show_core_in_case_visible;
  1405.
  1406.    procedure show_core_in_case_normal (first, last : in KDF9.address) is
  1407.    begin
  1408.       show_core(first, last,
  1409.                 head => "characters in TR/TP Normal Case code",
  1410.                 side => "NC",
  1411.                 converted => case_normal'Access);
  1412.    end show_core_in_case_normal;
  1413.
  1414.    procedure show_core_in_case_shift (first, last : in KDF9.address) is
  1415.    begin
  1416.       show_core(first, last,
  1417.                 head => "characters in TR/TP Shift Case code",
  1418.                 side => "SC",
  1419.                 converted => case_shift'Access);
  1420.    end show_core_in_case_shift;
  1421.
  1422.    procedure show_core_in_print_code (first, last : in KDF9.address) is
  1423.    begin
  1424.       show_core(first, last,
  1425.                 head => "characters in LP code",
  1426.                 side => "LP",
  1427.                 converted => printer_code'Access);
  1428.    end show_core_in_print_code;
  1429.
  1430.    procedure show_core_in_card_code (first, last : in KDF9.address) is
  1431.    begin
  1432.       show_core(first, last,head => "characters in CR/CP code",
  1433.                 side => "CP",
  1434.                 converted => card_code'Access);
  1435.    end show_core_in_card_code;
  1436.
  1437.    procedure show_core_in_Latin_1 (first, last : in KDF9.address) is
  1438.    begin
  1439.       show_core(first, last,
  1440.                 head => "words with bits 40-47 of each in Latin-1 code",
  1441.                 side => "L1",
  1442.                 converted => Latin_1_code'Access);
  1443.    end show_core_in_Latin_1;
  1444.
  1445.    procedure show_core_in_tape_code (first, last : in KDF9.address) is
  1446.    begin
  1447.       show_core_in_case_visible(first, last);
  1448.    end show_core_in_tape_code;
  1449.
  1450.    procedure show_core_as_word_forms (first, last  : KDF9.address) is
  1451.
  1452.       procedure show_word (address : KDF9.address) is
  1453.          word : constant KDF9.word := fetch_word(address);
  1454.       begin
  1455.          log_octal(KDF9.field_of_16_bits(address));
  1456.          log(":");
  1457.          tab_log_to(jump_tab);
  1458.          show_in_various_formats(word, column => jump_tab);
  1459.          log_new_line;
  1460.       end show_word;
  1461.
  1462.       procedure show_word_group (first, last  : KDF9.address) is
  1463.          last_address : KDF9.address := first;
  1464.          this_word, last_word : KDF9.word;
  1465.       begin
  1466.          if last = first or last = 0 then
  1467.             show_word(last);
  1468.             return;
  1469.          end if;
  1470.          this_word := fetch_word(first);
  1471.          last_word := this_word;
  1472.          show_word(first);
  1473.          for address in first+1 .. last-1 loop
  1474.             this_word := fetch_word(address);
  1475.             if this_word = last_word and address = last_address+1 then
  1476.                log_line("==========  ditto  ========");
  1477.             elsif this_word /= last_word then
  1478.                show_word(address);
  1479.                last_word := this_word;
  1480.                last_address := address;
  1481.             end if;
  1482.          end loop;
  1483.          if last > first then
  1484.             show_word(last);
  1485.          end if;
  1486.       end show_word_group;
  1487.
  1488.    begin
  1489.       if first > last then
  1490.          return;
  1491.       end if;
  1492.       BA := 0; -- Ensure that physical store is examined when running in boot mode.
  1493.       log_title("Core store interpreted as 48-bit words:");
  1494.       show_word_group(first, last);
  1495.       log_new_line;
  1496.    end show_core_as_word_forms;
  1497.
  1498.    -- Each word of code space is described by a set of flags.
  1499.    -- Flags 0 .. 5 are set iff a jump order has that syllable as target.
  1500.    -- Flag 6 is set if the word is thought to be code, but not a target.
  1501.    -- Flag 7 is set if the word is thought to be addressed as data.
  1502.
  1503.    is_a_code_word : constant KDF9.syllable_index := 6;
  1504.    is_a_data_word : constant KDF9.syllable_index := 7;
  1505.
  1506.    package word_flags is new generic_sets(member => KDF9.syllable_index);
  1507.    use word_flags;
  1508.
  1509.    all_jump_targets : constant word_flags.set := (0 .. 5 => True, 6|7 => False);
  1510.
  1511.    analysis_flags : array (KDF9.order_word_number) of word_flags.set;
  1512.
  1513.    function "/" (word : KDF9.order_word_number; flag : KDF9.syllable_index)
  1514.    return Boolean
  1515.    is (analysis_flags(word)(flag));
  1516.
  1517.    function is_a_jump_target (the_point : in KDF9.syllable_address)
  1518.    return Boolean
  1519.    is (analysis_flags(the_point.order_word_number)(the_point.syllable_index));
  1520.
  1521.    function is_a_jump_target (the_operand : in KDF9.order_word_number)
  1522.    return Boolean
  1523.    is ((analysis_flags(the_operand) and all_jump_targets) /= empty_set);
  1524.
  1525.    procedure clear_all_analysis_flags is
  1526.    begin
  1527.       analysis_flags := (others => empty_set);
  1528.    end clear_all_analysis_flags;
  1529.
  1530.    procedure unmark_as_a_data_word (the_operand : in KDF9.order_word_number) is
  1531.    begin
  1532.       analysis_flags(the_operand)(is_a_data_word) := False;
  1533.    end unmark_as_a_data_word;
  1534.
  1535.    procedure unmark_as_a_code_word (the_operand : in KDF9.order_word_number) is
  1536.    begin
  1537.       analysis_flags(the_operand)(is_a_code_word) := False;
  1538.    end unmark_as_a_code_word;
  1539.
  1540.    procedure mark_as_a_code_word (the_operand : in KDF9.order_word_number) is
  1541.    begin
  1542.       analysis_flags(the_operand)(is_a_code_word) := True;
  1543.       unmark_as_a_data_word(the_operand);
  1544.    end mark_as_a_code_word;
  1545.
  1546.    procedure mark_as_a_jump_target (the_point : in KDF9.syllable_address) is
  1547.    begin
  1548.       analysis_flags(the_point.order_word_number)(the_point.syllable_index) := True;
  1549.       mark_as_a_code_word(the_point.order_word_number);
  1550.    end mark_as_a_jump_target;
  1551.
  1552.    procedure mark_as_a_data_word (the_operand : in KDF9.order_word_number) is
  1553.    begin
  1554.       analysis_flags(the_operand)(is_a_data_word) := True;
  1555.       unmark_as_a_code_word(the_operand);
  1556.    end mark_as_a_data_word;
  1557.
  1558.    procedure mark_all_code_blocks_and_data_blocks is
  1559.
  1560.       procedure mark_all_code_blocks (the_beginning : in KDF9.syllable_address) is
  1561.          address : KDF9.syllable_address := the_beginning;
  1562.       begin
  1563.          if address.syllable_index > 5 then
  1564.             return;  -- We have blundered into non-code words.
  1565.          end if;
  1566.          -- Mark the first syllable of the block.
  1567.          mark_as_a_jump_target(the_beginning);
  1568.          -- Mark the destinations of all jumps in the block as code.
  1569.          loop
  1570.          -- Setting NIA to 8191 LIVs, in accordance with the hardware, so avoid that.
  1571.          exit when address.order_word_number = 8191;
  1572.             set_NIA_to(address);
  1573.             decode_the_next_order;
  1574.             if is_an_invalid_order(INS)                                                  or else
  1575.                   (address.order_word_number/is_a_data_word and address.syllable_index = 0) then
  1576.                return;
  1577.             else
  1578.                -- Assuming a valid code word, act on it.
  1579.                mark_as_a_code_word(address.order_word_number);
  1580.                case INS.kind is
  1581.                   when normal_jump_order =>
  1582.                      if not is_a_jump_target((INS.target.order_word_number, INS.target.syllable_index)) then
  1583.                         -- Mark the jump's destination recursively.
  1584.                         -- N.B. EXIT is actioned only if it is of EXIT ARr type.
  1585.                         mark_all_code_blocks((INS.target.order_word_number, INS.target.syllable_index));
  1586.                      end if;
  1587.                      increment_by_3(address);
  1588.                      if INS.compressed_opcode = JSr  then
  1589.                         -- Mark its return point.
  1590.                         mark_as_a_jump_target(address);
  1591.                      end if;
  1592.                   when one_syllable_order =>
  1593.                      increment_by_1(address);
  1594.                   when two_syllable_order =>
  1595.                      if INS.compressed_opcode = JCqNZS then
  1596.                         -- Mark the preceding word.
  1597.                         mark_as_a_jump_target((address.order_word_number-1, 0));
  1598.                      end if;
  1599.                      increment_by_2(address);
  1600.                   when data_access_order =>
  1601.                      increment_by_3(address);
  1602.                end case;
  1603.             end if;
  1604.          end loop;
  1605.       end mark_all_code_blocks;
  1606.
  1607.       procedure mark_all_data_blocks (the_beginning : in KDF9.syllable_address) is
  1608.          address : KDF9.syllable_address := the_beginning;
  1609.       begin
  1610.          if address.syllable_index > 5 then
  1611.             return;  -- We have blundered into non-code words.
  1612.          end if;
  1613.          the_code_block_handler: loop
  1614.          -- Setting NIA to 8191 LIVs, in accordance with the hardware, so avoid that.
  1615.          exit when address.order_word_number = 8191;
  1616.             -- Process orders, starting at an established code word.
  1617.             set_NIA_to(address); -- **
  1618.             decode_the_next_order;
  1619.             if (is_an_invalid_order(INS)                         or else
  1620.                   address.order_word_number/is_a_data_word)     and then
  1621.                      not (address.order_word_number/is_a_code_word) then
  1622.                -- This word is data: make sure it is not designated as code;
  1623.                --    and find the start of the next code block.
  1624.                for a in address.order_word_number .. 8190 loop
  1625.                   address := (a, 0);
  1626.                   exit when is_a_jump_target(a);
  1627.                   unmark_as_a_code_word(a);
  1628.                   mark_as_a_data_word(a);
  1629.                end loop;
  1630.
  1631.                exit the_code_block_handler
  1632.                   when address.order_word_number = 8190;
  1633.
  1634.                -- Find the syllable at which the block starts.
  1635.                for s in KDF9.syllable_index'(0) .. 5 loop
  1636.                   address.syllable_index := s;
  1637.                   exit when is_a_jump_target(address);
  1638.                end loop;
  1639.
  1640.             else
  1641.
  1642.                -- Assuming a valid code word, act on it.
  1643.                case INS.kind is
  1644.                   when data_access_order =>
  1645.                      if INS.operand < 8192 then
  1646.                         declare
  1647.                            operand : constant KDF9.order_word_number
  1648.                                    := KDF9.order_word_number(INS.operand);
  1649.                         begin
  1650.                            if INS.compressed_opcode /= KDF9.decoding.SET and then
  1651.                                  not is_a_jump_target(operand)               then
  1652.                               mark_as_a_data_word(operand);
  1653.                            end if;
  1654.                         end;
  1655.                      end if;
  1656.                      increment_by_3(address);
  1657.                   when one_syllable_order =>
  1658.                      increment_by_1(address);
  1659.                   when two_syllable_order =>
  1660.                      increment_by_2(address);
  1661.                   when normal_jump_order =>
  1662.                      increment_by_3(address);
  1663.                end case;
  1664.             end if;
  1665.
  1666.             exit the_code_block_handler
  1667.                when address.order_word_number = KDF9.order_word_number'Last;
  1668.
  1669.          end loop the_code_block_handler;
  1670.       end mark_all_data_blocks;
  1671.
  1672.       procedure reset_wrong_data_marks (the_beginning : in KDF9.syllable_address) is
  1673.          address : KDF9.syllable_address := the_beginning;
  1674.          locus   : KDF9.order_word_number;
  1675.       begin
  1676.          if address.syllable_index > 5 then
  1677.             return;  -- We have blundered into non-code words.
  1678.          end if;
  1679.          -- Unmark the first instruction of the block.
  1680.          unmark_as_a_data_word(address.order_word_number);
  1681.
  1682.          -- Unmark data marks on destinations of jumps.
  1683.          loop
  1684.          -- Setting NIA to 8191 LIVs, in accordance with the hardware, so avoid that.
  1685.          exit when address.order_word_number = 8191;
  1686.             set_NIA_to(address);
  1687.             decode_the_next_order;
  1688.             if is_an_invalid_order(INS)                          or else
  1689.                   address.order_word_number/is_a_data_word       or else
  1690.                      not (address.order_word_number/is_a_code_word) then
  1691.                -- We have reached the end of the code block.
  1692.                return;
  1693.             else
  1694.                -- Assuming a valid code word, act on it.
  1695.                case INS.kind is
  1696.                   when normal_jump_order =>
  1697.                      locus := address.order_word_number;
  1698.                      increment_by_3(address);
  1699.                      if INS.target.order_word_number/is_a_data_word    then
  1700.                         -- UNmark the jump's destination recursively.
  1701.                         reset_wrong_data_marks((INS.target.order_word_number, INS.target.syllable_index));
  1702.                      end if;
  1703.                      if INS.compressed_opcode /= Jr          and then
  1704.                            INS.compressed_opcode /= EXIT_n   and then
  1705.                               locus /= address.order_word_number then
  1706.                         -- It flows on, so the next word cannot be data.
  1707.                         unmark_as_a_data_word(address.order_word_number);
  1708.                      elsif not (address.order_word_number/is_a_data_word) then
  1709.                         -- The next syllable starts a block, iff it is not the end of a block.
  1710.                         set_NIA_to(address);
  1711.                         decode_the_next_order;
  1712.                         if not is_an_invalid_order(INS) then
  1713.                            mark_as_a_jump_target(address);
  1714.                         end if;
  1715.                      end if;
  1716.                   when one_syllable_order =>
  1717.                      increment_by_1(address);
  1718.                   when two_syllable_order =>
  1719.                      increment_by_2(address);
  1720.                   when data_access_order =>
  1721.                      increment_by_3(address);
  1722.                end case;
  1723.             end if;
  1724.             exit when address.order_word_number = KDF9.order_word_number'Last;
  1725.          end loop;
  1726.       end reset_wrong_data_marks;
  1727.
  1728.       procedure mark_the_words_reachable_from (address : in KDF9.syllable_address) is
  1729.          start_point : KDF9.syllable_address;
  1730.       begin
  1731.          mark_as_a_jump_target(address);
  1732.          set_NIA_to(address);
  1733.          decode_the_next_order;
  1734.          if INS.kind =normal_jump_order then
  1735.          start_point := (INS.target.order_word_number, INS.target.syllable_index);
  1736.          mark_all_code_blocks(start_point);
  1737.          mark_all_data_blocks(start_point);
  1738.          reset_wrong_data_marks(start_point);
  1739.          end if;
  1740.       end mark_the_words_reachable_from;
  1741.
  1742.       procedure markup_a_problem_program is
  1743.       begin
  1744.          if the_initial_jump_was_corrupted then
  1745.             -- We cannot sensibly locate the order words using E0  ...
  1746.             log_new_line;
  1747.             log_line("The initial jump, in E0U, has been corrupted!");
  1748.             log_new_line;
  1749.             show_core_as_syllables((0, 0), (5, 0));
  1750.             --  ... so restore it to the value it had on loading.
  1751.             restore_the_initial_jump;
  1752.             log_line("E0U has been restored to the value it had on loading.");
  1753.             log_new_line;
  1754.          end if;
  1755.
  1756.          -- Mark all orders reachable from the initial jump in E0 and the restart jumps in E4.
  1757.
  1758.          mark_the_words_reachable_from((0, 0));
  1759.          mark_the_words_reachable_from((0, 4));
  1760.          mark_the_words_reachable_from((1, 4));
  1761.
  1762.          -- Mark the words between E0 and P0 as data, skipping E4.
  1763.          mark_as_a_data_word(1);
  1764.          mark_as_a_data_word(2);
  1765.          mark_as_a_data_word(3);
  1766.          set_NIA_to((0, 0));
  1767.          decode_the_next_order;
  1768.          for d in 5 .. INS.target.order_word_number-1 loop
  1769.             mark_as_a_data_word(d);
  1770.          end loop;
  1771.
  1772.          the_program_has_been_analysed := True;
  1773.       end markup_a_problem_program;
  1774.
  1775.       -- This analysis assumes that the Director has much the same structure as KKT40E007UPU.
  1776.       procedure markup_a_Director is
  1777.
  1778.       begin
  1779.          BA := 0;  -- Director starts at physical word 0.
  1780.
  1781.          mark_as_a_code_word(0);
  1782.          mark_as_a_code_word(1);
  1783.          mark_as_a_code_word(2);
  1784.          mark_as_a_data_word(3);
  1785.          mark_as_a_code_word(4);
  1786.
  1787.          for a in nominated_address .. 3200 loop  -- 3200 was the size of the Eldon 2 Director.
  1788.             -- mark_as_a_code_word(a);
  1789.             -- mark_as_a_jump_target((a, 0));
  1790.             set_NIA_to((a, 0));
  1791.             decode_the_next_order;
  1792.             if INS.kind = normal_jump_order then
  1793.                mark_the_words_reachable_from((a, 0));
  1794.             elsif INS.kind = data_access_order then
  1795.                if INS.operand < 8192 then
  1796.                   declare
  1797.                      operand : constant KDF9.order_word_number
  1798.                              := KDF9.order_word_number(INS.operand);
  1799.                   begin
  1800.                      if INS.compressed_opcode /= KDF9.decoding.SET and then
  1801.                            not is_a_jump_target(operand)               then
  1802.                         mark_as_a_data_word(operand);
  1803.                      end if;
  1804.                   end;
  1805.                end if;
  1806.              else
  1807.                 mark_as_a_jump_target((a, 0));
  1808.             end if;
  1809.             set_NIA_to((a, 3));
  1810.             decode_the_next_order;
  1811.             if INS.kind = normal_jump_order then
  1812.                mark_the_words_reachable_from((a, 3));
  1813.             elsif INS.kind = data_access_order then
  1814.                if INS.operand < 8192 then
  1815.                   declare
  1816.                      operand : constant KDF9.order_word_number
  1817.                              := KDF9.order_word_number(INS.operand);
  1818.                   begin
  1819.                      if INS.compressed_opcode /= KDF9.decoding.SET and then
  1820.                            not is_a_jump_target(operand)               then
  1821.                         mark_as_a_data_word(operand);
  1822.                      end if;
  1823.                   end;
  1824.                end if;
  1825.              else
  1826.                 mark_as_a_jump_target((a, 3));
  1827.             end if;
  1828.          end loop;
  1829.
  1830.          -- Mark all orders reachable from the initial jump(s).
  1831.
  1832.          set_NIA_to((order_word_number => 2, syllable_index => 0));
  1833.          decode_the_next_order;
  1834.          if INS.kind /= normal_jump_order then
  1835.             log_line("An initial jump, in E2U, has not been found!");
  1836.          else
  1837.             mark_the_words_reachable_from((0, 2));
  1838.             -- Mark the words between E4 and P0 as data.
  1839.             set_NIA_to((0, 2));
  1840.             decode_the_next_order;
  1841.             for d in 5 .. INS.target.order_word_number-1 loop
  1842.                mark_as_a_data_word(d);
  1843.             end loop;
  1844.             the_program_has_been_analysed := True;
  1845.          end if;
  1846.
  1847.          set_NIA_to((0, 4));
  1848.          decode_the_next_order;
  1849.          if INS.kind /= normal_jump_order then
  1850.             log_line("An expected jump, in E4U, has not been found!");
  1851.             return;
  1852.          else
  1853.             mark_the_words_reachable_from((0, 4));
  1854.             the_program_has_been_analysed := True;
  1855.          end if;
  1856.
  1857.          set_NIA_to((1, 4));
  1858.          decode_the_next_order;
  1859.          if INS.kind /= normal_jump_order then
  1860.             log_line("An expected jump, in E4L, has not been found!");
  1861.             return;
  1862.          else
  1863.             mark_the_words_reachable_from((1, 4));
  1864.             the_program_has_been_analysed := True;
  1865.          end if;
  1866.       end markup_a_Director;
  1867.
  1868.    begin -- mark_all_code_blocks_and_data_blocks
  1869.       if the_program_has_been_analysed then
  1870.          return;
  1871.       end if;
  1872.
  1873.       clear_all_analysis_flags;
  1874.
  1875.       if the_execution_mode = boot_mode  then
  1876.          markup_a_Director;
  1877.       else
  1878.          markup_a_problem_program;
  1879.       end if;
  1880.
  1881.       if nominated_address < invalid_address then
  1882.          mark_the_words_reachable_from((nominated_address, 0));
  1883.       end if;
  1884.
  1885.    end mark_all_code_blocks_and_data_blocks;
  1886.
  1887.    procedure show_core_as_Usercode (first, last  : in KDF9.syllable_address;
  1888.                                     octal_option : in Boolean) is
  1889.
  1890.       six_DUMMIES : constant KDF9.word := 8#0360741703607417#;
  1891.       saved_CIA   : constant KDF9.syllable_address := CIA;
  1892.       last_word   : KDF9.word := 8#0706050403020100#; -- invalid opcodes
  1893.       comparator  : KDF9.word := last_word;
  1894.       this_word   : KDF9.word;
  1895.       address     : KDF9.syllable_address;
  1896.
  1897.       procedure show_a_block_of_orders is
  1898.
  1899.          function is_a_store_order (decoded : KDF9.decoded_order)
  1900.          return Boolean
  1901.          is (
  1902.              if decoded.kind = one_syllable_order then
  1903.                 False
  1904.              elsif decoded.kind = two_syllable_order then
  1905.                 (
  1906.                  case decoded.compressed_opcode is
  1907.                    when TO_MkMq   | TO_MkMqQ
  1908.                       | TO_MkMqH  | TO_MkMqQH
  1909.                       | TO_MkMqN  | TO_MkMqQN
  1910.                       | TO_MkMqHN | TO_MkMqQHN => True,
  1911.                    when others                 => False
  1912.                 )
  1913.              elsif decoded.kind = data_access_order then
  1914.                 (
  1915.                  case decoded.compressed_opcode is
  1916.                     when TO_EaMq | TO_EaMqQ => True,
  1917.                     when others             => False
  1918.                 )
  1919.              else
  1920.                 False
  1921.             );
  1922.
  1923.          procedure set_line_at_minimum (tab : in Natural) is
  1924.          begin
  1925.             if panel_logger.column < tab then
  1926.                tab_log_to(tab);
  1927.             end if;
  1928.          end set_line_at_minimum;
  1929.
  1930.          procedure set_line_at (tab : in Natural) is
  1931.          begin
  1932.             if panel_logger.column > tab then
  1933.                log_new_line;
  1934.             end if;
  1935.             if panel_logger.column < tab then
  1936.                tab_log_to(tab);
  1937.             end if;
  1938.          end set_line_at;
  1939.
  1940.          procedure set_at_new_line is
  1941.          begin
  1942.             if panel_logger.column > 1 then
  1943.                log_new_line;
  1944.             end if;
  1945.          end set_at_new_line;
  1946.
  1947.       last_nz_location : KDF9.syllable_address;
  1948.
  1949.       begin -- show_a_block_of_orders
  1950.          this_word := fetch_word(KDF9.address(address.order_word_number));
  1951.
  1952.          if this_word+1 < 2 or this_word = six_DUMMIES then
  1953.             -- The word is not worth logging.
  1954.             address := (address.order_word_number+1, 0);
  1955.             return;
  1956.          end if;
  1957.
  1958.          -- Log useful information about data words.
  1959.          if address.order_word_number/is_a_data_word then
  1960.             set_at_new_line;
  1961.          end if;
  1962.          loop
  1963.             if address.order_word_number/is_a_data_word then
  1964.                last_nz_location := address;
  1965.                -- Display a line of data.
  1966.                log(oct_or_dec_of(address, octal_option) & ": ");
  1967.                set_line_at(jump_tab);
  1968.                show_in_various_formats(fetch_word(KDF9.address(address.order_word_number)),
  1969.                                        column => jump_tab);
  1970.                log_new_line;
  1971.                loop
  1972.                   if address.order_word_number = last.order_word_number then
  1973.                      return;
  1974.                   end if;
  1975.                   address := (address.order_word_number+1, 0);
  1976.                exit when fetch_word(KDF9.address(address.order_word_number)) /= 0;
  1977.                end loop;
  1978.                if address.order_word_number > last_nz_location.order_word_number+1 then
  1979.                   log("========  zeros  ========");
  1980.                   log_new_line;
  1981.                end if;
  1982.             else
  1983.                log_new_line;
  1984.                exit;
  1985.             end if;
  1986.          end loop;
  1987.
  1988.          loop
  1989.          -- Setting NIA to 8191 LIVs, in accordance with the hardware, so avoid that.
  1990.          exit when address.order_word_number = 8191;
  1991.             this_word := fetch_word(KDF9.address(address.order_word_number));
  1992.             if this_word = comparator and this_word = last_word then
  1993.                -- The word is not worth logging.
  1994.                address := (address.order_word_number+1, 0);
  1995.                return;
  1996.             end if;
  1997.
  1998.             if this_word+1 < 2 or this_word = six_DUMMIES then
  1999.                comparator := this_word;
  2000.             end if;
  2001.
  2002.             set_NIA_to(address);
  2003.             decode_the_next_order;
  2004.             if is_an_invalid_order(INS) then
  2005.                -- The word is not worth logging.
  2006.                address := (address.order_word_number+1, 0);
  2007.                return;
  2008.             end if;
  2009.
  2010.             if is_a_jump_target(address) then
  2011.                -- Start a code paragraph, with its address for easy reference.
  2012.                set_at_new_line;
  2013.                log(oct_or_dec_of(address, octal_option) & ": ");
  2014.                log_new_line;
  2015.             end if;
  2016.
  2017.             -- Set the tab position appropriately for the order type.
  2018.             case INS.kind is
  2019.                when one_syllable_order | data_access_order =>
  2020.                   set_line_at_minimum(first_tab);
  2021.                when two_syllable_order =>
  2022.                   case INS.compressed_opcode is
  2023.                      when JCqNZS =>
  2024.                         set_line_at(jump_tab);
  2025.                      when  CT_PMB_PMC_BUSY_Qq
  2026.                         |  PAR_Qq
  2027.                         |  PMF_PMG_Qq
  2028.                         |  PIA_PIC_CLO_TLO_Qq
  2029.                         |  PIB_PID_Qq
  2030.                         |  PIE_PIG_Qq
  2031.                         |  PIF_PIH_Qq
  2032.                         |  POA_POC_POE_POF_PMH_Qq
  2033.                         |  POB_POD_Qq
  2034.                         |  POG_POL_Qq
  2035.                         |  POH_POK_Qq
  2036.                         |  PMA_PMK_INT_Qq
  2037.                         |  PMA_PMK_INT_Qq+1
  2038.                         |  PMD_PME_PML_Qq
  2039.                         |  PMD_PME_PML_Qq+1 =>
  2040.                         set_line_at(first_tab);
  2041.                      when others =>
  2042.                         if panel_logger.column < first_tab then
  2043.                            set_line_at_minimum(first_tab);
  2044.                         end if;
  2045.                   end case;
  2046.                when normal_jump_order =>
  2047.                   set_line_at(jump_tab);
  2048.             end case;
  2049.
  2050.             -- Show the order in pseudo-Usercode format.
  2051.             log(the_full_name_of(INS, octal_option) &  "; ");
  2052.
  2053.             case INS.kind is
  2054.                when one_syllable_order =>
  2055.                   increment_by_1(address);
  2056.                when two_syllable_order =>
  2057.                   increment_by_2(address);
  2058.                when normal_jump_order | data_access_order =>
  2059.                   increment_by_3(address);
  2060.             end case;
  2061.
  2062.             if address.order_word_number = last.order_word_number then
  2063.                log_new_line;
  2064.                return;
  2065.             end if;
  2066.
  2067.             if (address.order_word_number+1)/is_a_data_word or
  2068.                   address.order_word_number > last.order_word_number then
  2069.                return;
  2070.             end if;
  2071.
  2072.             if is_a_store_order(INS)                   or else
  2073.                   INS.compressed_opcode = JCqNZS                or else
  2074.                      INS.kind = normal_jump_order      or else
  2075.                         panel_logger.column > last_column then
  2076.                log_new_line;
  2077.             elsif this_word = comparator and this_word /= last_word then
  2078.                log_new_line;
  2079.                log_line("==========  #"
  2080.                       & oct_of(KDF9.syllable(this_word and 255))
  2081.                       & "  ==========");
  2082.                address := (address.order_word_number+1, 0);
  2083.                if address.order_word_number > last.order_word_number or
  2084.                      address.order_word_number/is_a_data_word then
  2085.                   return;
  2086.                end if;
  2087.             end if;
  2088.
  2089.             last_word := this_word;
  2090.
  2091.          end loop;
  2092.
  2093.       end show_a_block_of_orders;
  2094.
  2095.    begin
  2096.       if the_program_has_been_analysed then
  2097.          log_line("Core store interpreted as instructions.");
  2098.          BA := 0; -- Ensure that physical store is examined when running in boot mode.
  2099.          address := first;
  2100.          loop
  2101.             show_a_block_of_orders;
  2102.             exit when address.order_word_number >= last.order_word_number;
  2103.          end loop;
  2104.          log_new_line;
  2105.          log_rule;
  2106.          CIA := saved_CIA;
  2107.          decode_the_next_order;
  2108.       else
  2109.          log_line(" ... Core store cannot be interpreted as instructions!");
  2110.          log_new_line;
  2111.       end if;
  2112.    end show_core_as_Usercode;
  2113.
  2114.    procedure show_core_as_syllables (first, last  : KDF9.syllable_address) is
  2115.
  2116.       address     :   KDF9.syllable_address;
  2117.
  2118.       procedure show_a_block is
  2119.
  2120.          procedure set_line_at (tab : Natural) is
  2121.          begin  -- set_line_at
  2122.             if panel_logger.column > tab then
  2123.                log_new_line;
  2124.             end if;
  2125.             if panel_logger.column < tab then
  2126.                tab_log_to(tab);
  2127.             end if;
  2128.          end set_line_at;
  2129.
  2130.       begin  -- show_a_block
  2131.          loop
  2132.             if address.syllable_index = 0 then
  2133.                log_new_line;
  2134.                log(oct_of(address) & ": ");
  2135.                set_line_at(jump_tab);
  2136.             end if;
  2137.             log(oct_of(fetch_syllable(address)) &  "; ");
  2138.             increment_by_1(address);
  2139.          exit when address.order_word_number > last.order_word_number;
  2140.          end loop;
  2141.          log_new_line;
  2142.       end show_a_block;
  2143.
  2144.     begin  -- show_core_as_syllables
  2145.        BA := 0; -- Ensure that physical store is examined when running in boot mode.
  2146.       log_line("Core store interpreted as order syllables.");
  2147.       address := first;
  2148.       loop
  2149.          show_a_block;
  2150.          exit when address.order_word_number > last.order_word_number;
  2151.       end loop;
  2152.       log_new_line;
  2153.       log_rule;
  2154.    end show_core_as_syllables;
  2155.
  2156.    procedure poke (address    : in KDF9.address;
  2157.                    sub_word   : in Character;
  2158.                    position   : in KDF9.address;
  2159.                    value      : in KDF9.word) is
  2160.    begin
  2161.       case sub_word is
  2162.          when 'W' | 'w' =>
  2163.             store_word(value, address);
  2164.          when 'U' | 'u' =>
  2165.             store_halfword(value*2**24, address, 0);
  2166.          when 'L' | 'l' =>
  2167.             store_halfword(value*2**24, address, 1);
  2168.          when 'S' | 's' =>
  2169.             store_syllable(KDF9.syllable(value), address, KDF9.syllable_index(position));
  2170.          when 'C' | 'c' =>
  2171.             store_symbol(KDF9_char_sets.symbol(value), address, KDF9_char_sets.symbol_index(position));
  2172.          when others =>
  2173.             raise emulation_failure
  2174.                with "invalid poke position " & sub_word & ".";
  2175.       end case;
  2176.    exception
  2177.       when error : others =>
  2178.          raise emulation_failure
  2179.             with "invalid poke operation: "
  2180.                & Ada.Exceptions.Exception_Information(error);
  2181.    end poke;
  2182.
  2183. end state_display;

Compiling: ../Source/state_display.ads
Source file time stamp: 2020-11-02 19:11:12
Compiled at: 2020-11-12 18:12:11

     1. -- state_display.ads
     2. --
     3. -- Provide the comprehensive machine-state display panel KDF9 never had.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with KDF9;
    20.
    21. use  KDF9;
    22.
    23. package state_display is
    24.
    25.    procedure show_all_prerun_dump_areas;
    26.
    27.    procedure show_CIA_and_NIA;
    28.
    29.    procedure show_V_and_T;
    30.
    31.    procedure show_nest;
    32.
    33.    procedure show_sjns;
    34.
    35.    procedure show_IO_register (the_Q_register : in KDF9.Q_register;
    36.                                width          : in Positive := 8;
    37.                                for_DR,
    38.                                for_FD,
    39.                                for_FH,
    40.                                for_seek       : in Boolean  := False);
    41.
    42.    procedure show_Q_register (the_Q_register : in KDF9.Q_register;
    43.                               width          : in Positive := 8);
    44.
    45.    procedure show_Q_store;
    46.
    47.    procedure show_registers;
    48.
    49.    procedure show_execution_context;
    50.
    51.    procedure long_witness;
    52.
    53.    procedure short_witness;
    54.
    55.    procedure log_an_external_trace_header;
    56.
    57.    procedure log_to_external_trace;
    58.
    59.    procedure show_progress;
    60.
    61.    procedure show_Director_registers;
    62.
    63.    procedure show_retrospective_traces;
    64.
    65.    procedure show_current_state;
    66.
    67.    procedure show_final_state (because : String);
    68.
    69.    procedure mark_all_code_blocks_and_data_blocks;
    70.
    71.    the_program_has_been_analysed : Boolean := False;
    72.
    73.    procedure show_core_as_word_forms (first, last : in KDF9.address);
    74.
    75.    procedure show_core_as_syllables (first, last : in KDF9.syllable_address);
    76.
    77.    procedure show_core_as_Usercode (first, last  : in KDF9.syllable_address;
    78.                                     octal_option : in Boolean);
    79.
    80.    procedure show_core_in_print_code (first, last : in KDF9.address);
    81.
    82.    procedure show_core_in_card_code (first, last : in KDF9.address);
    83.
    84.    procedure show_core_in_tape_code (first, last : in KDF9.address);
    85.
    86.    procedure show_core_in_case_normal (first, last : in KDF9.address);
    87.
    88.    procedure show_core_in_case_shift (first, last : in KDF9.address);
    89.
    90.    procedure show_core_in_Latin_1 (first, last : in KDF9.address);
    91.
    92.    -- poke is included here as it has the same relationship to dumping as show_core_*.
    93.    procedure poke (address    : in KDF9.address;
    94.                    sub_word   : in Character;
    95.                    position   : in KDF9.address;
    96.                    value      : in KDF9.word);
    97.
    98.    -- Take note that an OUT 2 or OUT 0 has been obeyed.
    99.    procedure notify_termination;
   100.
   101. end state_display;

 2183 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/logging-file.adb
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- file.adb
     2. --
     3. -- Provide logging output to a named text file.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Text_IO;
    20. with Ada.Unchecked_Deallocation;
    21. --
    22. with file_interfacing;
    23.
    24. use  Ada.Text_IO;
    25. --
    26. use  file_interfacing;
    27.
    28. package body logging.file is
    29.
    30.    overriding
    31.    procedure tab_log (logger   : in out file.output;
    32.                       at_least : in Natural;
    33.                       spacing  : in Positive;
    34.                       iff      : in Boolean := True) is
    35.       column_nr : constant Positive_Count := Col(logger.the_log.all) + Count(at_least);
    36.       excess    : constant Count          := column_nr mod Count(spacing);
    37.    begin
    38.       if not iff or logger.log_file_is_shut then return; end if;
    39.       Set_Col(logger.the_log.all, column_nr);
    40.       if excess /= 0 then
    41.          Set_Col(logger.the_log.all, column_nr + Count(spacing) - excess);
    42.       end if;
    43.    end tab_log;
    44.
    45.    overriding
    46.    procedure tab_log_to (logger : in out file.output;
    47.                          column : in Positive;
    48.                          iff    : in Boolean := True) is
    49.    begin
    50.       if not iff or logger.log_file_is_shut then return; end if;
    51.       Set_Col(logger.the_log.all, Positive_Count(column));
    52.    end tab_log_to;
    53.
    54.    overriding
    55.    procedure log_new_line (logger : in out file.output;
    56.                            iff    : in Boolean := True) is
    57.    begin
    58.       if not iff or logger.log_file_is_shut then return; end if;
    59.       New_Line(logger.the_log.all);
    60.    end log_new_line;
    61.
    62.    overriding
    63.    procedure log (logger : in out file.output;
    64.                   char   : in Character;
    65.                   iff    : in Boolean := True) is
    66.    begin
    67.       if not iff or logger.log_file_is_shut then return; end if;
    68.       Put(logger.the_log.all, char);
    69.    end log;
    70.
    71.    overriding
    72.    procedure log (logger : in out file.output;
    73.                   text   : in String;
    74.                   iff    : in Boolean := True) is
    75.    begin
    76.       if not iff or logger.log_file_is_shut then return; end if;
    77.       Put(logger.the_log.all, text);
    78.    end log;
    79.
    80.    overriding
    81.    procedure open (logger : in out file.output; logfile_name : in String) is
    82.    begin
    83.       if logger.log_file_is_shut then
    84.          logger.the_log := new Ada.Text_IO.File_Type;
    85.          file_interfacing.initialize(logger.the_log.all, out_file, logfile_name);
    86.          logger.log_file_is_shut := False;
    87.       end if;
    88.    end open;
    89.
    90.    overriding
    91.    procedure close (logger : in out file.output; logfile_name : in String) is
    92.
    93.       procedure free_log_file is
    94.          new Ada.Unchecked_Deallocation(Ada.Text_IO.File_Type, File_Type_access);
    95.
    96.    begin
    97.       if logger.log_file_is_shut then return; end if;
    98.       file_interfacing.finalize(logger.the_log.all, logfile_name);
    99.       free_log_file(logger.the_log);
   100.       logger.log_file_is_shut := True;
   101.    end close;
   102.
   103.    overriding
   104.    procedure flush (logger : in out file.output; iff : in Boolean := True) is
   105.    begin
   106.       if not iff or logger.log_file_is_shut then return; end if;
   107.       Flush(logger.the_log.all);
   108.    end flush;
   109.
   110. end logging.file;

Compiling: ../Source/logging-file.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- logging-file.ads
     2. --
     3. -- Provide logging output to a named text file.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. private with Ada.Text_IO;
    20.
    21. package logging.file is
    22.
    23.    type output is new logging.output with private;
    24.
    25.    overriding
    26.    procedure tab_log (logger   : in out file.output;
    27.                       at_least : in Natural;
    28.                       spacing  : in Positive;
    29.                       iff      : in Boolean := True);
    30.
    31.    overriding
    32.    procedure tab_log_to (logger : in out file.output;
    33.                          column : in Positive;
    34.                          iff    : in Boolean := True);
    35.
    36.    overriding
    37.    procedure log (logger : in out file.output;
    38.                   char   : in Character;
    39.                   iff    : in Boolean := True);
    40.
    41.    overriding
    42.    procedure log (logger : in out file.output;
    43.                   text   : in String;
    44.                   iff    : in Boolean := True);
    45.
    46.    overriding
    47.    procedure log_new_line (logger : in out file.output;
    48.                            iff    : in Boolean := True);
    49.
    50.    overriding
    51.    procedure open  (logger : in out file.output; logfile_name : in String);
    52.
    53.    overriding
    54.    procedure close (logger : in out file.output; logfile_name : in String);
    55.
    56.    overriding
    57.    procedure flush (logger : in out file.output; iff    : in Boolean := True);
    58.
    59. private
    60.
    61.    type File_Type_access is access Ada.Text_IO.File_Type;
    62.
    63.    type output is new logging.output with
    64.       record
    65.          log_file_is_shut : Boolean := True;
    66.          the_log          : file.File_Type_access;
    67.       end record;
    68.
    69. end logging.file;

 110 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/kdf9-cpu.adb
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- kdf9-cpu.adb
     2. --
     3. -- Support for KDF9 CPU/ALU operations that are not automatically inherited from
     4. --   Ada types; and for types used in the internal functioning of the microcode.
     5. --
     6. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     7. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     8. --
     9. -- The ee9 program is free software; you can redistribute it and/or
    10. -- modify it under terms of the GNU General Public License as published
    11. -- by the Free Software Foundation; either version 3, or (at your option)
    12. -- any later version. This program is distributed in the hope that it
    13. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    14. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    15. -- See the GNU General Public License for more details. You should have
    16. -- received a copy of the GNU General Public License distributed with
    17. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    18. --
    19. --
    20.
    21. with exceptions;
    22.
    23. use  exceptions;
    24.
    25. package body KDF9.CPU is
    26.
    27. --
    28. --
    29.    --
    30.    -- KDF9 Arithmetic Control (AC) primitives representing Shift Control and associated units.
    31.    -- These units implement shifting, multiplication, division, and floating-point arithmetic.
    32.    --
    33. --
    34. --
    35.
    36.    -- Count the leading zeros of the absolute value of y, omitting the sign bit.
    37.    -- If y is 0, return 47.
    38.    function nr_leading_zeros (y : KDF9.word)
    39.    return Natural is
    40.       x : CPU.u_64;
    41.       r : Natural;
    42.    begin
    43.       if y = 0 then return 47; end if;
    44.       if resign(y) < 0 then
    45.          x := CPU.u_64(16#FFFF_FFFF_FFFF# and not y);
    46.       else
    47.          x := CPU.u_64(y);
    48.       end if;
    49.       -- Only 48 bits of x need be tested.
    50.       if (x and 16#FFFF_0000_0000#) /= 0 then
    51.          r := 32; x := shift_right(x, 32);
    52.       elsif (x and 16#FFFF_0000#) /= 0 then
    53.          r := 16; x := shift_right(x, 16);
    54.       else
    55.          r := 0;
    56.       end if;
    57.       if (x and 16#FF00#) /= 0 then
    58.          r := r + 8; x := shift_right(x, 8);
    59.       end if;
    60.       if (x and 16#F0#) /= 0 then
    61.          r := r + 4; x := shift_right(x, 4);
    62.       end if;
    63.       if (x and 16#C#) /= 0 then
    64.          r := r + 2; x := shift_right(x, 2);
    65.       end if;
    66.       if (x and 16#2#) /= 0 then
    67.          r := r + 1;
    68.       end if;
    69.       r := 47 - r - 1;  -- -1 discounts the sign bit.
    70.       return r;
    71.    end nr_leading_zeros;
    72.
    73.    function nr_one_bits (u : CPU.u_64)
    74.    return CPU.u_64 is
    75.       n : CPU.u_64 := shift_right(u, 1) and 16#77_77_77_77_77_77_77_77#;
    76.       x : CPU.u_64 := u - n;
    77.    begin
    78.       n := shift_right(n, 1) and 16#77_77_77_77_77_77_77_77#;
    79.       x := x - n;
    80.       n := shift_right(n, 1) and 16#77_77_77_77_77_77_77_77#;
    81.       x := x - n;
    82.       x := (x + shift_right(x, 4)) and 16#0F_0F_0F_0F_0F_0F_0F_0F#;
    83.       x := x * 16#01_01_01_01_01_01_01_01#;
    84.       return shift_right(x, CPU.u_64'Size-8);
    85.    end nr_one_bits;
    86.
    87.    KDF9_max_signed : constant CPU.s_64 := CPU.s_64(CPU.signed'Last);
    88.    KDF9_min_signed : constant CPU.s_64 := CPU.s_64(CPU.signed'First);
    89.
    90.    function as_word (u : CPU.u_64)
    91.    return KDF9.word
    92.    is (KDF9.word(u and KDF9.word_mask));
    93.
    94.    function as_word (s : CPU.s_64)
    95.    return KDF9.word is
    96.    begin
    97.       if s > KDF9_max_signed or s < KDF9_min_signed then
    98.          the_V_bit_is_set := True;
    99.       end if;
   100.       return as_word(unsign(s));
   101.    end as_word;
   102.
   103.    function contracted (msw, lsw : KDF9.word)
   104.    return KDF9.word is
   105.    begin
   106.       if resign(lsw) < 0 or (msw+1) > 1 then
   107.          the_V_bit_is_set := True;
   108.       end if;
   109.       return (lsw and not_sign_bit) or (msw and sign_bit);
   110.    end contracted;
   111.
   112.    function contracted (P : KDF9.pair)
   113.    return KDF9.word
   114.    is (contracted(msw => P.msw, lsw => P.lsw));
   115.
   116.    function shift_time (amount : Natural)
   117.    return KDF9.us
   118.    is (KDF9.us(amount/16 + amount/8 mod 2 + (if amount mod 8 > 0 then 1 else 0)));
   119.
   120.    function normalize_time (amount : Natural)
   121.    return KDF9.us
   122.    is (KDF9.us(amount/8 + (if amount mod 8 > 0 then 1 else 0)));
   123.
   124.    function shift_word_left (W : KDF9.word; amount : word_shift_length)
   125.    return KDF9.word
   126.    is (as_word(shift_left(CPU.u_64(W), amount)));
   127.
   128.    function shift_word_right (W : KDF9.word; amount : word_shift_length)
   129.    return KDF9.word
   130.    is (KDF9.word(shift_right(CPU.u_64(W), amount)));  -- This cannot be out of range.
   131.
   132.    function rotate_word_left (W : KDF9.word; amount : word_shift_length)
   133.    return KDF9.word
   134.    is (shift_word_left(W, amount) or shift_word_right(W, 48-amount));
   135.
   136.    function rotate_word_right (W : KDF9.word; amount : word_shift_length)
   137.    return KDF9.word
   138.    is (shift_word_right(W, amount) or shift_word_left(W, 48-amount));
   139.
   140.    function shift_circular (W : KDF9.word; L : CPU.signed_Q_part)
   141.    return KDF9.word
   142.       -- The logic here conforms to ¶1.1 of EE Report K/GD.y.80, entitled
   143.       --    "KDF 9: SHIFTING AND SHIFT CONTROL".
   144.       -- Circular shifts were implemented by duplicating the operand, doing a double-length
   145.       --    shift of the two words, and selecting the appropriate word from the result.
   146.    is (
   147.        if abs L > 95 then 0
   148.        elsif L < -48 then shift_word_right(W, Natural(-L-48))
   149.        elsif L > +48 then shift_word_left(W, Natural(+L-48))
   150.        elsif L < 0   then rotate_word_right(W, Natural(-L))
   151.        else               rotate_word_left(W, Natural(L))
   152.       );
   153.
   154.    function shift_logical (W : KDF9.word; L : CPU.signed_Q_part)
   155.    return KDF9.word
   156.    is
   157.       (
   158.        if abs L > 47 then 0
   159.        elsif  L < 0  then shift_word_right(W, Natural(-L))
   160.        else               shift_word_left(W, Natural(L))
   161.       );
   162.
   163.    function shift_pair_left (P : KDF9.pair; L : Natural)
   164.    return KDF9.pair
   165.       with Inline;
   166.
   167.    function shift_pair_left (P : KDF9.pair; L : Natural)
   168.    return KDF9.pair is
   169.       result    : KDF9.pair;
   170.       crossover : KDF9.word;
   171.    begin
   172.       -- The logic here conforms to ¶3.2 of EE Report K/GD.y.80.
   173.       if L < 48 then
   174.          result.lsw := shift_word_left(P.lsw, L);
   175.          crossover  := shift_word_right(P.lsw, 48-L);
   176.          result.msw := shift_word_left(P.msw, L) or crossover;
   177.       else
   178.          result.lsw := 0;
   179.          result.msw := shift_word_left(P.lsw, L-48);
   180.       end if;
   181.       return result;
   182.    end shift_pair_left;
   183.
   184.    function shift_pair_right (P : KDF9.pair; L : Natural)
   185.    return KDF9.pair
   186.       with Inline;
   187.
   188.    function shift_pair_right (P : KDF9.pair; L : Natural)
   189.    return KDF9.pair is
   190.       result    : KDF9.pair;
   191.       crossover : KDF9.word;
   192.    begin
   193.       -- The logic here conforms to ¶3.2 of EE Report K/GD.y.80.
   194.       if L < 48 then
   195.          result.msw := shift_word_right(P.msw, L);
   196.          crossover  := shift_word_left(P.msw, 48-L);
   197.          result.lsw := shift_word_right(P.lsw, L) or crossover;
   198.       else
   199.          result.msw := 0;
   200.          result.lsw := shift_word_right(P.msw, L-48);
   201.       end if;
   202.       return result;
   203.    end shift_pair_right;
   204.
   205.    function shift_logical (P : KDF9.pair; L : CPU.signed_Q_part)
   206.    return KDF9.pair
   207.    is (
   208.        if    L > 0 then shift_pair_left(P, Natural(L))
   209.        elsif L < 0 then shift_pair_right(P, Natural(-L))
   210.        else        P
   211.       );
   212.
   213.    function scale_down (W : KDF9.word; amount : Natural)
   214.    return KDF9.word is
   215.       unrounded, clearing : CPU.u_64;
   216.    begin
   217.       if amount = 0 then
   218.          return W;
   219.       elsif amount > 46 then
   220.          if resign(W) < 0 then
   221.             return KDF9.all_one_bits;
   222.          else
   223.             return 0;
   224.          end if;
   225.       else
   226.          -- It is undefined whether the intrinsic shift_right_arithmetic function,
   227.          --    operating on CPU.u_64, yields a rounded result.
   228.          -- So, any rounding it might do is completely suppressed.
   229.          unrounded := shift_right_arithmetic(shift_left(CPU.u_64(W),16), 16);
   230.          clearing  := - shift_left(1, amount);
   231.          return as_word(shift_right_arithmetic(unrounded and clearing, amount));
   232.       end if;
   233.    end scale_down;
   234.
   235.    function scale_down_and_round (W : KDF9.word; amount : Natural)
   236.    return KDF9.word is
   237.       unrounded, clearing, rounding : CPU.u_64;
   238.    begin
   239.       if amount = 0 then
   240.          return W;
   241.       elsif amount > 46 then
   242.          if resign(W) < 0 then
   243.             return KDF9.all_one_bits;
   244.          else
   245.             return 0;
   246.          end if;
   247.       else
   248.          -- It is undefined whether the intrinsic shift_right_arithmetic,
   249.          --    operating on CPU.u_64, yields a rounded result.
   250.          -- So, any rounding it might do is suppressed,
   251.          --    and correct rounding is explicitly computed.
   252.          unrounded := shift_right_arithmetic(shift_left(CPU.u_64(W),16), 16);
   253.          rounding  := shift_right(unrounded, amount-1) and 1;
   254.          clearing  := - shift_left(1, amount);
   255.          unrounded := unrounded and clearing;
   256.          return as_word(shift_right_arithmetic(unrounded, amount) + rounding);
   257.       end if;
   258.    end scale_down_and_round;
   259.
   260.    function scale_up (W : KDF9.word; amount : Natural)
   261.    return KDF9.word is
   262.       M : constant Natural := Natural'Min(amount, 47);
   263.    begin
   264.       if resign(W) < 0 then
   265.          if scale_down(W, 47-M) /= all_one_bits or
   266.                resign(shift_word_left(W, M)) >= 0 then
   267.             -- See EE Report K/GD.y.80., ¶ 1.1.
   268.             the_V_bit_is_set := True;
   269.          end if;
   270.          return shift_word_left(W, M);
   271.       else
   272.          if shift_word_right(W, 47-M) /= all_zero_bits or
   273.                resign(shift_word_left(W, M)) < 0 then
   274.             -- See EE Report K/GD.y.80., ¶ 1.1.
   275.             the_V_bit_is_set := True;
   276.          end if;
   277.          return shift_word_left(W, M);
   278.       end if;
   279.    end scale_up;
   280.
   281.    function shift_arithmetic (I : KDF9.word; L : CPU.signed_Q_part)
   282.    return KDF9.word
   283.    is (
   284.        if L < 0 then scale_down_and_round(I, Natural(-L))
   285.        else          scale_up(I, Natural(L))
   286.       );
   287.
   288.    function scale_up (P : KDF9.pair; L : Natural)
   289.    return KDF9.pair is
   290.       result    : KDF9.pair;
   291.       crossover : KDF9.word;
   292.    begin
   293.       -- The logic here conforms to ¶3.2 of EE Report K/GD.y.80.
   294.       if L < 48 then
   295.          result.lsw := shift_word_left(P.lsw, L) and KDF9.max_word;
   296.          crossover  := shift_word_right(P.lsw and KDF9.max_word, 47-L);
   297.          result.msw := scale_up(P.msw, L) or crossover;
   298.       else
   299.          result.lsw := 0;
   300.          result.msw := scale_up(P.msw, 47) or P.lsw;
   301.          result.msw := scale_up(result.msw, Natural'Min(L, 94)-47);
   302.       end if;
   303.       return result;
   304.    end scale_up;
   305.
   306.    function scale_down (P : KDF9.pair; L : Natural)
   307.    return KDF9.pair is
   308.       result    : KDF9.pair;
   309.       crossover : KDF9.word;
   310.    begin
   311.       -- The logic here conforms to ¶3.2 of EE Report K/GD.y.80.
   312.       -- SHAD-n does NOT round, according to the Manual.
   313.       if L < 48 then
   314.          result.msw := scale_down(P.msw, L);
   315.          crossover  := shift_word_left(P.msw, 47-L);
   316.          result.lsw := (shift_word_right(P.lsw, L) or crossover) and KDF9.max_word;
   317.       else
   318.          result.msw := scale_down(P.msw, 47);
   319.          result.lsw := shift_word_right(P.msw, Natural'Min(L, +94)-47) and KDF9.max_word;
   320.       end if;
   321.       return result;
   322.    end scale_down;
   323.
   324.    function shift_arithmetic (P : KDF9.pair; L : CPU.signed_Q_part)
   325.    return KDF9.pair
   326.    is (
   327.        if    L < 0 then scale_down(P, Natural(-L))
   328.        elsif L > 0 then scale_up(P, Natural(L))
   329.        else             P -- See ¶1.1 of EE Report K/GD.y.80: this avoids clearing D0 of P.lsw.
   330.       );
   331.
   332.    procedure normalize (fraction, exponent : in out KDF9.word) is
   333.       sign_flag  : constant KDF9.word := shift_word_right(fraction and sign_bit, 1);
   334.       normalizer : Natural;
   335.    begin
   336.       if fraction = 0 then
   337.          exponent := 2#10_000_000#;  -- This yields 0 when biased positive.
   338.          return;
   339.       end if;
   340.
   341.       normalizer := nr_leading_zeros(fraction);
   342.       exponent := exponent - KDF9.word(normalizer);
   343.
   344.       validate_scaler(exponent, "normalizing F number");
   345.
   346.       -- shift_word_left is used, not _arithmetic, as D[1..normalizer] = D0
   347.       fraction := shift_word_left(fraction, normalizer);
   348.
   349.       the_CPU_delta := the_CPU_delta + normalize_time(normalizer);
   350.
   351.       -- scale_down_and_round may round up and overflow the fraction bits ...
   352.       fraction := scale_down_and_round(fraction, 8);
   353.       if (fraction and overflow_mask) /= shift_word_right(sign_flag, 7) then
   354.           -- ... so re-normalize; scale_down cannot round here.
   355.          fraction := scale_down(fraction, 1);
   356.          the_CPU_delta := the_CPU_delta + normalize_time(1);
   357.          exponent := exponent + 1;
   358.          the_CPU_delta := the_CPU_delta + 1;
   359.       end if;
   360.       fraction := fraction and mantissa_mask;
   361.
   362.       if resign(exponent) < -128 then
   363.          -- Deal with underflow.
   364.          fraction := 0;
   365.          exponent := 2#10_000_000#;  -- This yields 0 when biased positive.
   366.       elsif resign(exponent) > +127 then
   367.          -- Deal with overflow.
   368.          the_V_bit_is_set := True;
   369.          exponent := 2#01_111_111#;
   370.       end if;
   371.    end normalize;
   372.
   373.    function fraction_word (mantissa : CPU.f48)
   374.    return KDF9.word
   375.     -- shift_word_left must be used instead of scale_up to avoid a spurious overflow.
   376.    is (
   377.        (shift_word_left(as_word(mantissa), 8) and KDF9.max_word)
   378.           or
   379.        (as_word(mantissa) and sign_bit)
   380.       );
   381.
   382.    function masked_mantissa (F : CPU.f48)
   383.    return CPU.f48
   384.    is (as_f48(as_word(F) and mantissa_mask));
   385.
   386.    function scaler (F : CPU.f48)
   387.    return KDF9.word
   388.    is ((shift_word_right(as_word(F), 39) and 2#11_111_111#) - 128);
   389.
   390.    procedure validate_scaler (E : in KDF9.word; where : in String) is
   391.    begin
   392.       if resign(E) < -254 or resign(E) > +256 then
   393.          -- This is an impossible exponent - something has gone seriously wrong.
   394.          -- E may reach -254 in L/R from exponent(L) = -128, and exponent(R) = +128. See "/".
   395.          -- E may reach +256 in L*R from exponent(L) = +127, and exponent(R) = +127. See "*".
   396.          -- In both cases there may be adjustment of +2 for prescaling.
   397.          trap_invalid_operand("scaler in " & where & " = " & resign(E)'Image);
   398.       end if;
   399.    end validate_scaler;
   400.
   401.    function normalized (full_fraction, scaler : KDF9.word)
   402.    return CPU.f48 is
   403.       E : KDF9.word := scaler;
   404.       F : KDF9.word := full_fraction;
   405.    begin
   406.       normalize(fraction => F, exponent => E);
   407.       return CPU.f48(shift_word_left((E + 128) and 2#11_111_111#, 39) or F);
   408.    end normalized;
   409.
   410.    function normalized  (R : CPU.f48)
   411.    return CPU.f48
   412.    is (normalized(full_fraction => fraction_word(R), scaler => scaler(R)));
   413.
   414.    function cardinality (W : KDF9.word)
   415.    return KDF9.word
   416.    is (KDF9.word(nr_one_bits(CPU.u_64(W))));
   417.
   418.    function "-" (I : CPU.signed)
   419.    return KDF9.word
   420.    is (as_word(-CPU.s_64(I)));
   421.
   422.    function "abs" (I : CPU.signed)
   423.    return KDF9.word
   424.    is (as_word(abs CPU.s_64(I)));
   425.
   426.    function "+" (L, R : CPU.signed)
   427.    return KDF9.word
   428.    is (as_word(CPU.s_64(L) + CPU.s_64(R)));
   429.
   430.    function "-" (L, R : CPU.signed)
   431.    return KDF9.word
   432.    is (as_word(CPU.s_64(L) - CPU.s_64(R)));
   433.
   434.    function "*" (L, R : CPU.signed)
   435.    return KDF9.word
   436.    is (contracted(KDF9.pair'(unsign(L) * unsign(R))));
   437.
   438.    procedure do_DIVI (L : in KDF9.word;
   439.                       R : in KDF9.word;
   440.                       Quotient, Remainder : out KDF9.word) is
   441.    begin
   442.       if R /= 0 then
   443.          Remainder := as_word(CPU.s_64(resign(L)) mod CPU.s_64(resign(R)));
   444.          Quotient  :=
   445.             as_word((CPU.s_64(resign(L)) - CPU.s_64(resign(Remainder))) / CPU.s_64(resign(R)));
   446.       else
   447.          the_V_bit_is_set := True;
   448.          Quotient  := L;  -- ??
   449.          Remainder := R;  -- ??
   450.       end if;
   451.    end do_DIVI;
   452.
   453.    function "*" (L, R : KDF9.word)
   454.    return CPU.fraction is
   455.    begin
   456.       if L = sign_bit and R = sign_bit then
   457.          the_V_bit_is_set := True;
   458.          return as_fraction(sign_bit);  -- The only case is L = R = -1.0 = L*R.
   459.       else
   460.          return as_fraction(L) * as_fraction(R);
   461.       end if;
   462.    end "*";
   463.
   464.    function "/" (L, R : KDF9.word)
   465.    return CPU.fraction is
   466.    begin
   467.       if R = 0 or L = sign_bit then
   468.          the_V_bit_is_set := True;
   469.          return as_fraction(L); -- ??
   470.       elsif R = sign_bit then
   471.          return -as_fraction(L);
   472.       elsif abs as_fraction(L) < abs as_fraction(R) then  -- abs is safe now.
   473.          return as_fraction(L) / as_fraction(R);
   474.       else
   475.          the_V_bit_is_set := True;
   476.          return as_fraction(L); -- ??
   477.       end if;
   478.    end "/";
   479.
   480.    function "+" (L, R : KDF9.pair)
   481.    return KDF9.pair is
   482.       carry, sum : CPU.s_64;
   483.       result     : KDF9.pair;
   484.    begin
   485.       sum := CPU.s_64(L.lsw) + CPU.s_64(R.lsw);
   486.       if unsign(sum) > KDF9.max_word then -- carry into msw
   487.          carry := 1;
   488.          result.lsw := KDF9.word(unsign(sum) and KDF9.max_word);
   489.       else
   490.          carry := 0;
   491.          result.lsw := KDF9.word(sum);
   492.       end if;
   493.       sum := CPU.s_64(resign(L.msw)) + CPU.s_64(resign(R.msw)) + carry;
   494.       result.msw := as_word(sum);
   495.       return result;
   496.    end "+";
   497.
   498.    function "-" (J : KDF9.pair)
   499.    return KDF9.pair is
   500.       borrow,
   501.       negative : CPU.s_64;
   502.       result   : KDF9.pair;
   503.    begin
   504.       negative := - CPU.s_64(J.lsw);
   505.       if unsign(negative) > KDF9.max_word then -- borrow from msw
   506.          borrow := 1;
   507.          result.lsw := KDF9.word(unsign(negative) and KDF9.max_word);
   508.       else
   509.          borrow := 0;
   510.          result.lsw := KDF9.word(negative);
   511.       end if;
   512.       negative := - CPU.s_64(resign(J.msw)) - borrow;
   513.       result.msw := as_word(negative);
   514.       return result;
   515.    end "-";
   516.
   517.    function "-" (L, R : KDF9.pair)
   518.    return KDF9.pair is
   519.       borrow,
   520.       difference : CPU.s_64;
   521.       result     : KDF9.pair;
   522.    begin
   523.       difference := CPU.s_64(L.lsw) - CPU.s_64(R.lsw);
   524.       if unsign(difference) > KDF9.max_word then -- borrow from msw
   525.          borrow := 1;
   526.          result.lsw := KDF9.word(unsign(difference) and KDF9.max_word);
   527.       else
   528.          borrow := 0;
   529.          result.lsw := KDF9.word(difference);
   530.       end if;
   531.       difference := CPU.s_64(resign(L.msw)) - CPU.s_64(resign(R.msw)) - borrow;
   532.       result.msw := as_word(difference);
   533.       return result;
   534.    end "-";
   535.
   536.    function "*" (L, R : KDF9.word)
   537.    return KDF9.pair is
   538.       S, T, U, V, W : KDF9.word;
   539.       H, M, B       : KDF9.pair;
   540.    begin
   541.       if L = sign_bit then
   542.          if R = L then
   543.             -- L*R = (+1.0), which is not a valid fraction, so deal with overflow.
   544.             the_V_bit_is_set := True;
   545.             return (L, 0);
   546.          else
   547.             -- L*R = -R.
   548.             return - (R, 0);
   549.          end if;
   550.       end if;
   551.       if R = sign_bit then
   552.          -- L*R = -L.
   553.          return - (L, 0);
   554.       end if;
   555.       -- Now it is safe to take absolute values, as they cannot overflow.
   556.       S := scale_down(abs resign(L), 24);
   557.       T := abs resign(L) and halfword_mask;
   558.       U := scale_down(abs resign(R), 24);
   559.       V := abs resign(R) and halfword_mask;
   560.       H := ((S*U)*2, 0);
   561.       M := scale_down((KDF9.word'(S*V), 0), 1) + scale_down((KDF9.word'(T*U), 0), 1);
   562.       M := scale_down(M, 22);
   563.       W := rotate_word_left(KDF9.word'(T*V), 1);
   564.       B := (W and 1, shift_word_right(W, 1));
   565.       if resign(L xor R) < 0 then
   566.          return - (H + M + B);
   567.       else
   568.          return    H + M + B;
   569.       end if;
   570.    end "*";
   571.
   572.    f_64_small : constant := 2.0**(-63);
   573.    type f_64 is delta f_64_small range -1.0 .. +1.0 - f_64_small with Size => 64;
   574.
   575.    function scale_down (f : CPU.f_64; N : Natural)
   576.    return f_64
   577.    is (if N > 62 then 0.0 else f / 2**N);
   578.
   579.    function to_f_64 (w : KDF9.word)
   580.    return CPU.f_64
   581.    is (CPU.f_64(as_fraction(w)));
   582.
   583.    function to_word (f : CPU.f_64)
   584.    return KDF9.word
   585.    is (as_word(CPU.fraction(f)));
   586.
   587.    procedure do_DIVD (L : in KDF9.pair;
   588.                       R : in KDF9.word;
   589.                       Q : out KDF9.word
   590.                      ) is
   591.       to_normalize_L : Natural;
   592.       to_normalize_R : Natural;
   593.       to_normalize_Q : Integer;
   594.       N              : KDF9.pair;
   595.       D              : KDF9.word;
   596.       Ls, Rs, Qs     : CPU.f_64;
   597.    begin
   598.       -- Deal very quickly with a zero result.
   599.       if (L.msw or L.lsw) = 0 then
   600.          Q := 0;
   601.          return;
   602.       end if;
   603.
   604.       -- Deal with division by 0.
   605.       if R = 0 then
   606.          the_V_bit_is_set := True;
   607.          Q := L.msw;  -- This is a guess at the result for division by zero ?? !!
   608.          return;
   609.       end if;
   610.
   611.       -- Check for an invalid numerator; D0 of L.lsw must be 0.
   612.       if resign(L.lsw) < 0 then -- L is not a valid double-length number.
   613.          the_V_bit_is_set := True;
   614.          Q := L.msw;  -- This is a guess at the result for an invalid numerator ?? !!
   615.          return;
   616.       end if;
   617.
   618.       to_normalize_L := nr_leading_zeros(L.msw);
   619.       if to_normalize_L > 46 then -- insignificant top half
   620.          N := scale_up(L, 47);
   621.          to_normalize_L := nr_leading_zeros(N.msw);
   622.          N := scale_up(N, to_normalize_L);
   623.          to_normalize_L := to_normalize_L + 47;
   624.       else
   625.          N := scale_up(L, to_normalize_L);
   626.       end if;
   627.
   628.       to_normalize_R := nr_leading_zeros(R);
   629.       D := scale_up(R, to_normalize_R);
   630.
   631.       -- Scale Ls and Rs so that the Ada fractional division cannot overflow.
   632.       Ls := scale_down(to_f_64(N.msw), 2);
   633.       Rs := scale_down(to_f_64(D), 1);
   634.
   635.       Qs := Ls / Rs;  -- "/" cannot overflow here.
   636.
   637.       to_normalize_Q := 1 + to_normalize_R - to_normalize_L;
   638.
   639.       if to_normalize_Q <= 0 then
   640.          -- Overflow is impossible.
   641.          Qs := scale_down(Qs, -to_normalize_Q);
   642.          Q := to_word(Qs);
   643.       else
   644.          -- If Qs >= 0.5, then L/R >= 1.0 is not a representable result fraction.
   645.          -- If Qs < -0.5, then L/R < -1.0 is not a representable result fraction.
   646.          if Qs >= 0.5 or Qs < -0.5 then
   647.             the_V_bit_is_set := True;
   648.             Q := L.msw / R;  -- This is a guess at the result when it overflows ?? !!
   649.             return;
   650.          end if;
   651.          Q := scale_up(to_word(Qs), to_normalize_Q);
   652.       end if;
   653.    end do_DIVD;
   654.
   655.    procedure do_DIVR (L : in KDF9.pair;
   656.                       R : in KDF9.word;
   657.                       Quotient, Remainder : out KDF9.word
   658.                      ) is
   659.       correction_count_limit : constant := 3;
   660.       correction_count       : Natural  := 0;
   661.       V  : constant Boolean := the_V_bit_is_set;
   662.       N  : KDF9.pair := L;
   663.       D  : KDF9.word := R;
   664.       S  : KDF9.word := +1;
   665.       P,
   666.       T  : KDF9.pair;
   667.    begin
   668.       if (N.msw or N.lsw) = 0 then
   669.          Quotient  := 0;
   670.          Remainder := 0;
   671.          return;
   672.       end if;
   673.
   674.       if D = 0 then
   675.          the_V_bit_is_set := True;
   676.          Quotient  := L.msw;  -- This is a guess at the result for division by zero ?? !!
   677.          Remainder := L.lsw;  -- This is a guess at the result for division by zero ?? !!
   678.          return;
   679.       end if;
   680.
   681.       -- Check for an invalid numerator; D0 of N1 must be 0.
   682.       if resign(L.lsw) < 0 then -- L is not a valid double-length number.
   683.          the_V_bit_is_set := True;
   684.          Quotient  := L.msw;  -- This is a guess at the result for invalid numerator ?? !!
   685.          Remainder := L.lsw;  -- This is a guess at the result for invalid numerator ?? !!
   686.          return;
   687.       end if;
   688.
   689.       -- Convert to an unsigned division problem, and note whether it needs to be be converted back.
   690.       if resign(N.msw) < 0 then -- L is negative.
   691.          N := - N;
   692.          S := - S;
   693.       end if;
   694.
   695.       if resign(D) < 0 then  -- R is negative.
   696.          D := - D;
   697.          S := - S;
   698.       end if;
   699.
   700.       -- Check for inevitable overflow, and deal with it separately.
   701.       if N.msw > D then
   702.          Quotient := N.msw / D * S;  -- This is a guess at the result when it overflows ?? !!
   703.          T := L - (msw => Quotient*R, lsw => 0);
   704.          Remainder := T.msw;         -- This is a guess at the result when it overflows ?? !!
   705.          the_V_bit_is_set := True;
   706.          return;
   707.       end if;
   708.
   709.       -- Overflow should not now be possible.
   710.       -- Ensure that an overflow in DIVD is trapped as a failure
   711.       the_V_bit_is_set := False;
   712.       do_DIVD(N, D, Quotient);
   713.       if the_V_bit_is_set then
   714.          raise emulation_failure
   715.             with "DIVR overflows in DIVD";
   716.       end if;
   717.       -- Restore the input value of the overflow register.
   718.       the_V_bit_is_set := V;
   719.
   720.       -- Adjust Quotient until the difference between N and Quotient*D fits in one word.
   721.       correction_count := 0;
   722.       loop
   723.          P := Quotient * D;
   724.          T := N - P;
   725.       exit when T.msw = 0;
   726.          correction_count := correction_count + 1;
   727.          if correction_count > correction_count_limit then
   728.              raise emulation_failure
   729.                 with "DIVR exceeds correction_count_limit A";
   730.          end if;
   731.          Quotient := Quotient + 1;
   732.       end loop;
   733.
   734.       Remainder := T.lsw;
   735.
   736.       -- Adjust Quotient and Remainder until Remainder is less than the divisor in absolute value.
   737.       correction_count := 0;
   738.       while Remainder >= D loop
   739.          correction_count := correction_count + 1;
   740.          if resign(Remainder) > 0 then
   741.             Remainder := Remainder - D;
   742.             Quotient := Quotient + 1;
   743.          else
   744.             Remainder := Remainder + D;
   745.             Quotient := Quotient - 1;
   746.          end if;
   747.          if correction_count > correction_count_limit then
   748.              raise emulation_failure
   749.                 with "DIVR exceeds correction_count_limit B";
   750.          end if;
   751.       end loop;
   752.
   753.       Quotient  := Quotient * S;
   754.       Remainder := contracted(L - Quotient*R);
   755.    end do_DIVR;
   756.
   757.    function host_float (X : CPU.f48)
   758.    return Long_Float is
   759.       -- Warn if Long_Float does not have at least the range of a KDF9 floating point number.
   760.       pragma Compile_Time_Warning(Long_Float'Last < 2.0**127,
   761.                                   "Long_Float does not have enough range for KDF9 f.p.");
   762.       -- Warn if Long_Float does not have at least the precision of a KDF9 floating point number.
   763.       pragma Compile_Time_Warning(Long_Float'Small > 1.0/2.0**39,
   764.                                   "Long_Float does not have enough precision for KDF9 f.p.");
   765.       W : constant KDF9.word  := fraction_word(masked_mantissa(X));
   766.       S : constant Long_Float := 2.0**Integer(resign(scaler(X)));
   767.    begin
   768.       return Long_Float(as_fraction(W)) * S;  -- Cannot overflow if warnings are absent.
   769.    end host_float;
   770.
   771.    -- Round a 48-bit floating-point number to 24-bit format.
   772.    function narrowed (R : CPU.f48)
   773.    return CPU.f48
   774.    is (normalized(fraction_word(R) + 2**23, scaler(R)));
   775.
   776.    overriding
   777.    function "-" (R : CPU.f48)
   778.    return CPU.f48 is
   779.       -- F is made half of a true fraction to prevent overflow when negating:
   780.       --    the result exponent is offset by 1, accordingly.
   781.       E : constant KDF9.word := scaler(R) + 1;
   782.       F : KDF9.word := scale_down_and_round(fraction_word(R), 1);
   783.    begin
   784.       F := as_word(CPU.u_64(-F));  -- "-" cannot overflow here.
   785.       return normalized(full_fraction => F, scaler => E);
   786.    end "-";
   787.
   788.    overriding
   789.    function "abs" (R : CPU.f48)
   790.    return CPU.f48
   791.    is (if resign(KDF9.word(R)) < 0 then - R else + R);
   792.
   793.    overriding
   794.    function "+" (L, R : CPU.f48)
   795.    return CPU.f48 is
   796.       -- B and D are made half of a true fraction to prevent overflow when
   797.       --    adding; the result exponent is offset by 1, accordingly.
   798.       A : constant KDF9.word := scaler(R);
   799.       B : KDF9.word := scale_down(fraction_word(R), 1);
   800.       C : constant KDF9.word := scaler(L);
   801.       D : KDF9.word := scale_down(fraction_word(L), 1);
   802.       E : KDF9.word;
   803.       F : KDF9.word;
   804.       N : Natural;
   805.    begin
   806.       if resign(A) >= resign(C) then
   807.          N := Natural'Min(Natural(resign(A-C)), 48);
   808.          D := scale_down_and_round(D, N);
   809.          E := A + 1;
   810.       else
   811.          N := Natural'Min(Natural(resign(C-A)), 48);
   812.          B := scale_down_and_round(B, N);
   813.          E := C + 1;
   814.       end if;
   815.       the_CPU_delta := the_CPU_delta + shift_time(N);
   816.       F := as_word(CPU.u_64(D + B));  -- "+" cannot overflow here.
   817.       return normalized(full_fraction => F, scaler => E);
   818.    end "+";
   819.
   820.    overriding
   821.    function "-" (L, R : CPU.f48)
   822.    return CPU.f48 is
   823.       -- See "+".
   824.       A : constant KDF9.word := scaler(R);
   825.       B : KDF9.word := scale_down(fraction_word(R), 1);
   826.       C : constant KDF9.word := scaler(L);
   827.       D : KDF9.word := scale_down(fraction_word(L), 1);
   828.       E : KDF9.word;
   829.       F : KDF9.word;


   830.       N : Natural;
   831.    begin
   832.       if resign(A) >= resign(C) then
   833.          N := Natural'Min(Natural(resign(A-C)), 48);
   834.          D := scale_down_and_round(D, N);
   835.          E := A + 1;
   836.       else
   837.          N := Natural'Min(Natural(resign(C-A)), 48);
   838.          B := scale_down_and_round(B, N);
   839.          E := C + 1;
   840.       end if;
   841.       the_CPU_delta := the_CPU_delta + shift_time(N);
   842.       F := as_word(CPU.u_64(D - B));  -- "-" cannot overflow here.
   843.       return normalized(full_fraction => F, scaler => E);
   844.    end "-";
   845.
   846.    overriding
   847.    function "*" (L, R : CPU.f48)
   848.    return CPU.f48 is
   849.       B, D, E, F : KDF9.word;
   850.    begin
   851.       if (KDF9.word(L) or KDF9.word(R)) = 0 then
   852.          return 0;
   853.       end if;
   854.       B := fraction_word(R);
   855.       D := fraction_word(L);
   856.       E := scaler(L) + scaler(R);
   857.       if (B = sign_bit) and (B = D) then
   858.           -- D*B = (+1), which is not a valid fraction, so treat specially.
   859.           B := B / 2;
   860.           D := D / 2;
   861.           E := E + 2;
   862.        end if;
   863.       F := as_word(as_fraction(D) * as_fraction(B));  -- "*" cannot overflow here.
   864.       return normalized(full_fraction => F, scaler => E);
   865.    end "*";
   866.
   867.    overriding
   868.    function "/" (L, R : CPU.f48)
   869.    return CPU.f48 is
   870.       D, N   : CPU.fraction;
   871.       Ls, Rs : KDF9.word;
   872.       E, F   : KDF9.word;
   873.    begin
   874.       if R = 0 then
   875.          the_V_bit_is_set := True;
   876.          return L;  -- ?? This result is not well defined in the Manual.
   877.       end if;
   878.       -- If L>=R, L/R>= 1, which is not a valid fraction; so Ls and Rs are
   879.       --    scaled so that the division cannot overflow.
   880.       Ls := scale_down(fraction_word(L), 3);
   881.       Rs := scale_down(fraction_word(R), 1);
   882.       N := abs as_fraction(Ls);  -- Ls is scaled down by 1/8, so "abs" cannot overflow.
   883.       D := abs as_fraction(Rs);  -- Rs is scaled down by 1/2, so "abs" cannot overflow.
   884.       -- E is increased by 2 to compensate the quotient's scaling by 1/4.
   885.       E := scaler(L) - scaler(R) + 2;
   886.       F := as_word(N / D);  -- "/" cannot overflow here.
   887.       if resign(KDF9.word(L) xor KDF9.word(R)) < 0 then
   888.          -- The result is negative.
   889.          F := -F;
   890.       end if;
   891.       return normalized(full_fraction => F, scaler => E);
   892.    end "/";
   893.
   894.    overriding
   895.    function "<" (L, R : CPU.f48)
   896.    return Boolean is
   897.       s : constant KDF9.word := KDF9.word(L) xor KDF9.word(R);
   898.    begin
   899.       if resign(s) < 0 then
   900.          -- The signs differ: L<R iff L is negative.
   901.          return resign(KDF9.word(L)) < 0;
   902.       elsif resign(KDF9.word(L)) < 0 then
   903.          -- L and R are both negative, so invert lexicographical order.
   904.          return not (KDF9.word(L) < KDF9.word(R));
   905.       else
   906.          -- L and R are both non-negative: so use lexicographical order.
   907.          return KDF9.word(L) < KDF9.word(R);
   908.       end if;
   909.    end "<";
   910.
   911.    function fraction_pair (DF : CPU.f96)
   912.    return KDF9.pair
   913.    is (scale_up((msw => scale_down(fraction_word(DF.msw), 8), lsw => fraction_word(DF.lsw)), 8));
   914.
   915.    function scaler (DF : CPU.f96)
   916.    return KDF9.word
   917.    is (scaler(DF.msw));
   918.
   919.    function narrowed (DF : CPU.f96)
   920.    return CPU.f48 is
   921.       fraction : KDF9.pair := fraction_pair(DF) + (0, 2**46);
   922.    begin
   923.       reconstruct(fraction, scaler(DF));
   924.       return CPU.f48(fraction.msw);
   925.    end narrowed;
   926.
   927.    procedure reconstruct (frac   : in out KDF9.pair;
   928.                           scaler : in KDF9.word) is
   929.       KDF9_exponent :  KDF9.word := scaler + 128;
   930.       normalizer    : Natural;
   931.    begin
   932.       if (frac.msw or frac.lsw) = 0 then
   933.          return; -- frac is already normalized.
   934.       end if;
   935.
   936.       normalizer := nr_leading_zeros(frac.msw);
   937.
   938.       if normalizer = 47 then  -- frac.msw is zero, so frac.lsw is non-zero.
   939.          normalizer := 47 + nr_leading_zeros(frac.lsw);
   940.       end if;
   941.       validate_scaler(scaler - KDF9.word(normalizer), "constructing DF number");
   942.
   943.       KDF9_exponent := KDF9_exponent - KDF9.word(normalizer);
   944.
   945.       frac := scale_up(frac, normalizer);
   946.       the_CPU_delta := the_CPU_delta + shift_time(normalizer);
   947.
   948.       -- 96-bit shift_arithmetic does not round and so cannot overflow here.
   949.       frac := scale_down(frac, 8);
   950.       frac.lsw := scale_down(frac.lsw, 8);
   951.       -- Clear both scaler fields.
   952.       frac.msw := frac.msw and mantissa_mask;
   953.       frac.lsw := frac.lsw and mantissa_mask;
   954.       if resign(KDF9_exponent) < 0 then
   955.          -- Deal with underflow.
   956.          frac := (0, 0);
   957.          return;
   958.       elsif KDF9_exponent > 255 then
   959.          -- Deal with overflow.
   960.          the_V_bit_is_set := True;
   961.          KDF9_exponent := 255;
   962.       end if;
   963.
   964.       frac.msw := frac.msw or shift_word_left(KDF9_exponent and 8#377#, 39);
   965.       if KDF9_exponent < 39 then
   966.          frac.lsw := 0;
   967.       else
   968.          frac.lsw := frac.lsw or shift_word_left((KDF9_exponent-39) and 8#377#, 39);
   969.       end if;
   970.    end reconstruct;
   971.
   972.    function "-" (R : CPU.f96)
   973.    return CPU.f96
   974.    is (CPU.f96'(0, 0) - R);
   975.
   976.    function "+" (L, R : CPU.f96)
   977.    return CPU.f96 is
   978.       -- Scale fractions to prevent overflow; must adjust exponent accordingly.
   979.       L_exponent : constant KDF9.word := scaler(L);
   980.       R_exponent : constant KDF9.word := scaler(R);
   981.       L_fraction : KDF9.pair := scale_down(fraction_pair(L), 1);
   982.       R_fraction : KDF9.pair := scale_down(fraction_pair(R), 1);
   983.       exponent   : KDF9.word;
   984.       the_result : KDF9.pair;
   985.       aligner    : Natural;
   986.    begin
   987.       if resign(R_exponent) >= resign(L_exponent) then
   988.          aligner := Natural(resign(R_exponent-L_exponent));
   989.          aligner := Natural'Min(95, aligner);
   990.          L_fraction := scale_down(L_fraction, aligner);
   991.          exponent := R_exponent + 1;
   992.       else
   993.          aligner := Natural(resign(L_exponent-R_exponent));
   994.          aligner := Natural'Min(95, aligner);
   995.          R_fraction := scale_down(R_fraction, aligner);
   996.          exponent := L_exponent + 1;
   997.       end if;
   998.       the_CPU_delta := the_CPU_delta + shift_time(aligner);
   999.       the_result := L_fraction + R_fraction;  -- "+" cannot overflow here.
  1000.       reconstruct(the_result, scaler => exponent);
  1001.       return as_f96(the_result);
  1002.    end "+";
  1003.
  1004.    function "-" (L, R : CPU.f96)
  1005.    return CPU.f96 is
  1006.       -- See "+".
  1007.       L_exponent : constant KDF9.word := scaler(L);
  1008.       R_exponent : constant KDF9.word := scaler(R);
  1009.       L_fraction : KDF9.pair := scale_down(fraction_pair(L), 1);
  1010.       R_fraction : KDF9.pair := scale_down(fraction_pair(R), 1);
  1011.       exponent   : KDF9.word;
  1012.       the_result : KDF9.pair;
  1013.       aligner    : Natural;
  1014.    begin
  1015.       if resign(R_exponent) >= resign(L_exponent) then
  1016.          aligner := Natural(resign(R_exponent-L_exponent));
  1017.          aligner := Natural'Min(95, aligner);
  1018.          L_fraction := scale_down(L_fraction, aligner);
  1019.          exponent := R_exponent + 1;
  1020.       else
  1021.          aligner := Natural(resign(L_exponent-R_exponent));
  1022.          aligner := Natural'Min(95, aligner);
  1023.          R_fraction := scale_down(R_fraction, aligner);
  1024.          exponent := L_exponent + 1;
  1025.       end if;
  1026.       the_CPU_delta := the_CPU_delta + shift_time(aligner);
  1027.       the_result := L_fraction - R_fraction;  -- "-" cannot overflow here.
  1028.       reconstruct(the_result, scaler => exponent);
  1029.       return as_f96(the_result);
  1030.    end "-";
  1031.
  1032.    function "*" (L, R : CPU.f48)
  1033.    return CPU.f96 is
  1034.       V_was_set : constant Boolean := the_V_bit_is_set;
  1035.       LR        : KDF9.pair;
  1036.    begin
  1037.       the_V_bit_is_set := False;
  1038.       LR := fraction_word(L) * fraction_word(R);
  1039.       if the_V_bit_is_set then
  1040.          -- The product is not a valid fixed-point fraction, but is actually OK,
  1041.          --    so restore the orginal overflow state, and  ...
  1042.          the_V_bit_is_set := V_was_set;
  1043.          --  ... construct +1.0 in double-precision floating-point.
  1044.          return as_f96((shift_word_left(2#0_10_000_001_1#, 38), 0));
  1045.       end if;
  1046.       reconstruct(LR, scaler => scaler(L) + scaler(R));
  1047.       return as_f96(LR);
  1048.    end "*";
  1049.
  1050.    function "/" (L : CPU.f96;
  1051.                  R : CPU.f48)
  1052.    return CPU.f48 is  -- aka DIVDF
  1053.       -- If L>=R, L/R>= 1, which is not a valid fraction; so Ls and Rs are
  1054.       --    scaled so that the division cannot overflow.
  1055.       Ls     : constant KDF9.pair := scale_down(fraction_pair(L), 3);
  1056.       Rs     : constant KDF9.word := scale_down(fraction_word(R), 1);
  1057.       -- E is increased by 2 to compensate the quotient's scaling by 1/4.
  1058.       E      : constant KDF9.word := scaler(L) - scaler(R) + 2;
  1059.       F      : KDF9.word;
  1060.    begin
  1061.       if R = 0 then
  1062.          the_V_bit_is_set := True;
  1063.          return L.msw;  -- ?? This result is not well defined in the Manual.
  1064.       end if;
  1065.       do_DIVD(Ls, Rs, F);  -- Division cannot overflow here.
  1066.       return normalized(full_fraction => F, scaler => E);
  1067.    end "/";
  1068.
  1069.    procedure push (F : in CPU.f48) is
  1070.    begin
  1071.       push(KDF9.word(F));
  1072.    end push;
  1073.
  1074.    function pop
  1075.    return CPU.f48
  1076.    is (CPU.f48(KDF9.word'(pop)));
  1077.
  1078.    function read_top
  1079.    return CPU.f48
  1080.    is (CPU.f48(KDF9.word'(read_top)));
  1081.
  1082.    procedure write_top (F : in CPU.f48) is
  1083.    begin
  1084.       write_top(KDF9.word(F));
  1085.    end write_top;
  1086.
  1087.    procedure push (DF : in CPU.f96) is
  1088.       AB : constant KDF9.pair := as_pair(DF);
  1089.    begin
  1090.       push(AB);
  1091.    end push;
  1092.
  1093.    function pop
  1094.    return CPU.f96
  1095.    is (as_f96(pop));
  1096.
  1097.    function read_top
  1098.    return CPU.f96
  1099.    is (as_f96(read_top));
  1100.
  1101.    procedure write_top (DF : in CPU.f96) is
  1102.       AB : constant KDF9.pair := as_pair(DF);
  1103.    begin
  1104.       write_top(AB);
  1105.    end write_top;
  1106.
  1107. end KDF9.CPU;
Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/logging-panel.adb

Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

Compiling: ../Source/kdf9-cpu.ads
     1. -- logging-panel.adb
     2. --
     3. -- Provide logging output to an interactive terminal/control panel.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with POSIX;
    20. with settings;
    21.
    22. use  POSIX;
    23. use  settings;
    24.
    25. package body logging.panel is
    26.
    27.    not overriding
    28.    function column (logger : panel.display)
    29.    return Positive
    30.    is (logger.column_number);
    31.
    32.    overriding
    33.    procedure tab_log (logger   : in out panel.display;
    34.                       at_least : in Natural;
    35.                       spacing  : in Positive;
    36.                       iff      : in Boolean := True) is
    37.       new_col : constant Natural := logger.column_number + at_least;
    38.       deficit : constant Natural := (spacing - new_col mod spacing) mod spacing;
    39.    begin
    40.       if not iff then return; end if;
    41.       for i in logger.column_number .. (new_col + deficit) loop
    42.          POSIX.output(' ');
    43.       end loop;
    44.       logger.column_number := new_col + deficit;
    45.    end tab_log;
    46.
    47.    overriding
    48.    procedure tab_log_to (logger : in out panel.display;
    49.                          column : in Positive;
    50.                          iff    : in Boolean := True) is
    51.    begin
    52.       if not iff then return; end if;
    53.       if column < logger.column_number then
    54.          logger.log_new_line;
    55.       end if;
    56.       for i in logger.column_number .. column-1 loop
    57.          POSIX.output(' ');
    58.       end loop;
    59.       logger.column_number := column;
    60.    end tab_log_to;
    61.
    62.    overriding
    63.    procedure log (logger : in out panel.display;
    64.                   char   : in Character;
    65.                   iff    : in Boolean := True) is
    66.    begin
    67.       if not iff then return; end if;
    68.       POSIX.output(char);
    69.       logger.column_number := logger.column_number + 1;
    70.    end log;
    71.
    72.    overriding
    73.    procedure log (logger : in out panel.display;
    74.                   text   : in String;
    75.                   iff    : in Boolean := True) is
    76.    begin
    77.       if not iff then return; end if;
    78.       if text /= "" then
    79.          POSIX.output(text);
    80.       end if;
    81.       logger.column_number := logger.column_number + text'Length;
    82.    end log;
    83.
    84.    overriding
    85.    procedure log_new_line (logger : in out panel.display;
    86.                            iff    : in Boolean := True) is
    87.    begin
    88.       if not iff then return; end if;
    89.       POSIX.output_line;
    90.       logger.column_number := 1;
    91.    end log_new_line;
    92.
    93.    not overriding
    94.    procedure show (logger : in out panel.display; message : in String := "") is
    95.    begin
    96.       if message /= "" then
    97.          logger.log(message);
    98.       end if;
    99.    end show;
   100.
   101.    not overriding
   102.    procedure show_line (logger : in out panel.display; message : in String := "") is
   103.    begin
   104.       if message /= "" then
   105.          logger.log(message);
   106.       end if;
   107.       logger.log_new_line;
   108.    end show_line;
   109.
   110.    not overriding
   111.    procedure interact (logger : in out panel.display; reason : in String := "Mode") is
   112.       old_mode : constant settings.diagnostic_mode := the_diagnostic_mode;
   113.       response : response_kind;
   114.       choice   : Character;
   115.    begin
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- kdf9-cpu.ads
     2. --
     3. -- Support for KDF9 CPU/ALU operations that are not automatically inherited from
     4. --   Ada types; and for types used in the internal functioning of the microcode.
     5. --
     6. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     7. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     8. --
     9. -- The ee9 program is free software; you can redistribute it and/or
    10. -- modify it under terms of the GNU General Public License as published
    11. -- by the Free Software Foundation; either version 3, or (at your option)
    12. -- any later version. This program is distributed in the hope that it
    13. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    14. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    15. -- See the GNU General Public License for more details. You should have
    16. -- received a copy of the GNU General Public License distributed with
    17. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    18. --
    19.
    20. with Ada.Unchecked_Conversion;
    21.
    22. package KDF9.CPU is
    23.
    24. --
    25. --
    26.    --
    27.    -- 48-bit integer and fractional ALU types and operations
    28.    --
    29. --
    30. --
    31.
    32.    type signed is range -2**47 .. +2**47 - 1 with Size => KDF9.word'Size;
    33.
    34.    function unsign is new Ada.Unchecked_Conversion (CPU.signed, KDF9.word);
    35.
    36.    function resign is new Ada.Unchecked_Conversion (KDF9.word, CPU.signed);
    37.
    38.    function "-" (I : CPU.signed)
    39.    return KDF9.word
    40.       with Inline;
    41.
    42.    function "abs" (I : CPU.signed)
    43.    return KDF9.word
    44.       with Inline;
    45.
    46.    function "+" (L, R : CPU.signed)
    47.    return KDF9.word
    48.       with Inline;
    49.
    50.    function "-" (L, R : CPU.signed)
    51.    return KDF9.word
    52.       with Inline;
    53.
    54.    function "*" (L, R : CPU.signed)
    55.    return KDF9.word;
    56.
    57.    -- Determine the Quotient and Remainder of L/R, where:
    58.    --    sign(Remainder) = sign(R) and |Remainder| < |R|, i.e. Remainder = L mod R;
    59.    --    Quotient = (L - Remainder) / R.
    60.
    61.    procedure do_DIVI (L : in KDF9.word;
    62.                       R : in KDF9.word;
    63.                       Quotient, Remainder : out KDF9.word);
    64.
    65.    -- Signed single-length integer substrate division is removed from consideration.
    66.
    67.    function "/" (L, R : CPU.signed)
    68.    return KDF9.word is abstract;
    69.
    70.    function "mod" (L, R : CPU.signed)
    71.    return KDF9.word is abstract;
    72.
    73.    -- Contract a double-word, setting the V bit if necessary.
    74.
    75.    function contracted (P : KDF9.pair)
    76.    return KDF9.word
    77.       with Inline;
    78.
    79.    -- Contract a double-word, represented by its components, setting the V bit if necessary.
    80.
    81.    function contracted (msw, lsw : KDF9.word)
    82.    return KDF9.word
    83.       with Inline;
    84.
    85. --
    86. --
    87.    -- Shifting operations with KDF9 semantics.
    88. --
    89. --
    90.
    91.    type signed_Q_part is range  -2**15 .. +2**15 - 1 with Size => KDF9.Q_part'Size;
    92.
    93.    function resign is new Ada.Unchecked_Conversion (KDF9.Q_part, CPU.signed_Q_part);
    94.
    95.    -- L>0 for left-shift, L<0 for right-shift.
    96.
    97.    function shift_logical (W : KDF9.word; L : CPU.signed_Q_part)
    98.    return KDF9.word
    99.       with Inline;
   100.
   101.    function shift_circular (W : KDF9.word; L : CPU.signed_Q_part)
   102.    return KDF9.word
   103.       with Inline;
   104.
   105.    -- shift_arithmetic rounds the result correctly.
   106.    function shift_arithmetic (I : KDF9.word; L : CPU.signed_Q_part)
   107.    return KDF9.word
   108.       with Inline;
   109.
   110.    -- cardinality yields the number of 1-bits in W.
   111.    function cardinality (W : KDF9.word)
   112.    return KDF9.word
   113.       with Inline;
   114.
   115. --
   116. --
   117.    -- A fraction is a word W interpreted as the value W / 2**47;
   118. --
   119. --
   116.    interaction_loop:
   117.       loop
   118.          logger.column_number := 1;
   119.          POSIX.debug_prompt(noninteractive_usage_is_enabled, reason, response, choice);
   120.          if response = name_response then
   120.
   121.             case choice is
   122.                when 'q' | 'Q' =>
   123.                   quit_was_requested := True;
   124.                   exit interaction_loop;
   125.                when 'd' | 'D' =>
   126.                   debugging_is_enabled := not debugging_is_enabled;
   127.                   exit interaction_loop;
   128.                when 'f' | 'F' =>
   129.                   set_diagnostic_mode(fast_mode);
   130.                   exit interaction_loop;
   131.                when 'p' | 'P' =>
   132.                   set_diagnostic_mode(pause_mode);
   133.                   exit interaction_loop;
   134.                when 't' | 'T' =>
   135.                   set_diagnostic_mode(trace_mode);
   136.                   exit interaction_loop;
   137.                when others =>
   138.                   null; -- An invalid choice, try again.
   139.             end case;
   140.          elsif response = EOF_response then
   141.             exit;
   142.          end if;
   143.       end loop interaction_loop;
   144.       the_diagnostic_mode_changed := (the_diagnostic_mode /= old_mode) or quit_was_requested;
   145.    end interact;
   121.    KDF9_small : constant := 2.0**(-47);
   122.
   123.    type fraction is delta KDF9_small range -1.0 .. +1.0 - KDF9_small with Size => KDF9.word'Size;
   124.
   125.    function as_fraction is new Ada.Unchecked_Conversion (KDF9.word, CPU.fraction);
   126.
   127.    function as_word     is new Ada.Unchecked_Conversion (CPU.fraction, KDF9.word);
   128.
   129.    -- These operations treat the KDF9.word operands as full-word fractions,
   130.
   131.    function "*" (L, R : KDF9.word)
   132.    return CPU.fraction;
   133.
   134.    function "/" (L, R : KDF9.word)
   135.    return CPU.fraction;
   136.
   137.
   138. --
   139. --
   140.    --
   141.    -- 48-bit integer and fractional ALU operations
   142.    --
   143. --
   144. --
   145.
   146.    function "+" (L, R : KDF9.pair)
   147.    return KDF9.pair
   148.       with Inline;
   149.
   150.    function "-" (J : KDF9.pair)
   151.    return KDF9.pair
   152.       with Inline;
   153.
   154.    function "-" (L, R : KDF9.pair)
   155.    return KDF9.pair
   156.       with Inline;
   157.
   158.    -- 48 * 48 -> 96-bit, for XD, etc.
   159.
   160.    function "*" (L, R : KDF9.word)
   161.    return KDF9.pair;
   162.
   163.    -- 96 / 48 -> 48-bit, for DIVD, DIVR and DIVDF.
   164.
   165.    procedure do_DIVD (L : in KDF9.pair;
   166.                       R : in KDF9.word;
   167.                       Q : out KDF9.word
   168.                      );
   169.
   170.    procedure do_DIVR (L : in KDF9.pair;
   171.                       R : in KDF9.word;
   172.                       Quotient,
   173.                       Remainder : out KDF9.word
   174.                      );
   175.
   176.    function shift_logical (P : KDF9.pair; L : CPU.signed_Q_part)
   177.    return KDF9.pair
   178.       with Inline;
   179.
   180.    function shift_arithmetic (P : KDF9.pair; L : CPU.signed_Q_part)
   181.    return KDF9.pair
   182.       with Inline;
   183.
   184.
   185. --
   186. --
   187.    --
   188.    -- 48-bit floating point ALU types and operations
   189.    --
   190. --
   191. --
   192.
   193.    -- This is a substrate for KDF9 floating point, not an Ada f.p. type.
   194.
   195.    type f48 is mod 2**48 with Size => KDF9.word'Size;
   196.
   197.    -- Remove useless substrate modular operations not, and, or, xor and mod.
   198.
   146.
   147. end logging.panel;
   199.    overriding
   200.    function "not" (R : CPU.f48)
   201.    return CPU.f48 is abstract;
   202.
   203.    overriding
   204.    function "and" (L, R : CPU.f48)
   205.    return CPU.f48 is abstract;
   206.
   207.    overriding
   208.    function "or" (L, R : CPU.f48)
   209.    return CPU.f48 is abstract;
   210.
   211.    overriding
   212.    function "xor" (L, R : CPU.f48)
   213.    return CPU.f48 is abstract;
   214.
   215.    overriding
   216.    function "mod" (L, R : CPU.f48)
   217.    return CPU.f48 is abstract;
   218.

Compiling: ../Source/logging-panel.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- logging-panel.ads
     2. --
     3. -- Provide logging output to an interactive terminal/control panel.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package logging.panel is
    20.
    21.    type display is new logging.output with private;
    22.
    23.    not overriding
    24.    function column (logger : panel.display)
    25.    return Positive;
    26.
    27.    overriding
   219.    function as_word is new Ada.Unchecked_Conversion (CPU.f48, KDF9.word);
   220.
   221.    function as_f48  is new Ada.Unchecked_Conversion (KDF9.word, CPU.f48);
   222.
   223.    procedure push (F : in CPU.f48);
   224.
   225.    function pop
   226.    return CPU.f48
   227.       with Inline;
   228.
   229.    procedure write_top (F : in CPU.f48)
   230.       with Inline;
   231.
   232.    function read_top
   233.    return CPU.f48
   234.       with Inline;
   235.
   236.     -- Standardize a (possibly) non-normalized floating-point number.
   237.
   238.    function normalized  (R : CPU.f48)
   239.    return CPU.f48;
   240.
   241.    -- Convert a 47-bit fraction to a rounded, standardized 39-bit mantissa,
   242.    --    and adjust its exponent accordingly, setting overflow when necessary.
   243.
   244.    procedure normalize (fraction, exponent : in out KDF9.word)
   245.       with Inline;
   246.
   247.    -- Convert a 39-bit mantissa to a 47-bit fraction, preserving the sign.
   248.
   249.    function fraction_word (mantissa : CPU.f48)
   250.    return KDF9.word
   251.       with Inline;
   252.
   253.    -- The floating-point number with the exponent field set to 0.
   254.
   255.    function masked_mantissa (F : CPU.f48)
   256.    return CPU.f48
   257.       with Inline;
   258.
   259.    -- The algebraic scale-factor, not the hardware exponent, -128 <= scaler < +128.
   260.
   261.    function scaler (F : CPU.f48)
   262.    return KDF9.word
   263.       with Inline;
   264.
   265.     -- Synthesize a normalized floating-point number from its components.
   266.
   267.    function normalized (full_fraction, scaler : KDF9.word)
   268.    return CPU.f48
   269.       with Inline;
   270.
   271.    -- Round a 48-bit floating-point number to 24-bit format.
   272.
   273.    function narrowed (R : CPU.f48)
   274.    return CPU.f48;
   275.
   276.    overriding
   277.    function "-" (R : CPU.f48)
   278.    return CPU.f48;
   279.
   280.    overriding
   281.    function "abs" (R : CPU.f48)
   282.    return CPU.f48;
    28.    procedure tab_log (logger   : in out panel.display;
    29.                       at_least : in Natural;
    30.                       spacing  : in Positive;
    31.                       iff      : in Boolean := True);
    32.
    33.    overriding
    34.    procedure tab_log_to (logger : in out panel.display;
    35.                          column : in Positive;
   283.
   284.    overriding
   285.    function "+" (L, R : CPU.f48)
   286.    return CPU.f48;
   287.
   288.    overriding
   289.    function "-" (L, R : CPU.f48)
   290.    return CPU.f48;
   291.
   292.    overriding
   293.    function "*" (L, R : CPU.f48)
   294.    return CPU.f48;
   295.
   296.    overriding
   297.    function "/" (L, R : CPU.f48)
   298.    return CPU.f48;
   299.
   300.    overriding
   301.    function "<" (L, R : CPU.f48)
    36.                          iff    : in Boolean := True);
    37.
    38.    overriding
   302.    return Boolean;
   303.
   304.    function host_float (X : CPU.f48)
   305.    return Long_Float;
   306.
   307.    exponent_mask : constant KDF9.word := KDF9.word'(2#11_111_111#) * 2**39;
   308.    mantissa_mask : constant KDF9.word := not exponent_mask;
   309.    frac_msb_mask : constant KDF9.word := 2**46;  -- M.S.B. of a 47-bit fraction
    39.    procedure log (logger : in out panel.display;
    40.                   char   : in Character;
    41.                   iff    : in Boolean := True);
    42.
    43.    overriding
    44.    procedure log (logger : in out panel.display;
    45.                   text   : in String;
    46.                   iff    : in Boolean := True);
    47.
    48.    overriding
   310.    mant_msb_mask : constant KDF9.word := 2**38;  -- M.S.B. of a 39-bit mantissa
   311.    overflow_mask : constant KDF9.word := 2**39;  -- bit set on rounding overflow
   312.
   313.
   314. --
   315. --
   316.    --
   317.    -- 96-bit floating point ALU types and operations
   318.    --
   319. --
   320. --
   321.
    49.    procedure log_new_line (logger : in out panel.display;
    50.                            iff    : in Boolean := True);
    51.
    52.    not overriding
    53.    procedure show (logger : in out panel.display; message : in String := "");
    54.
    55.    not overriding
    56.    procedure show_line (logger : in out panel.display; message : in String := "");
    57.
    58.    not overriding
    59.    procedure interact (logger : in out panel.display; reason : in String := "Mode");
    60.
    61.    overriding
   322.    type f96 is
   323.       record
   324.          msw, lsw : CPU.f48;
   325.       end record;
   326.
   327.    function as_pair is new Ada.Unchecked_Conversion (CPU.f96, KDF9.pair);
   328.
   329.    function as_f96  is new Ada.Unchecked_Conversion (KDF9.pair, CPU.f96);
   330.
   331.    procedure push (DF : in CPU.f96)
   332.       with Inline,
   333.            Pre => the_nest_depth < 15
   334.                or else the_CPU_state = Director_state;
   335.
    62.    procedure open (logger : in out panel.display; logfile_name : in String) is null;
    63.
    64.    overriding
    65.    procedure close (logger : in out panel.display; logfile_name : in String) is null;
    66.
    67.    overriding
    68.    procedure flush (logger : in out panel.display; iff : in Boolean := True) is null;
    69.
    70. private
    71.
    72.    type display is new logging.output with
    73.       record
    74.          column_number : Positive := 1;
   336.    function pop
   337.    return CPU.f96
   338.       with Inline;
   339.
   340.    procedure write_top (DF : in CPU.f96)
   341.       with Inline;
   342.
   343.    function read_top
    75.       end record;
    76.
    77. end logging.panel;
   344.    return CPU.f96
   345.       with Inline;
   346.
   347.    -- The algebraic scale-factor, not the hardware exponent, -128 <= scaler < +128.
   348.
   349.    function scaler (DF : CPU.f96)
   350.    return KDF9.word
   351.       with Inline;
   352.
   353.    procedure validate_scaler (E : in KDF9.word; where : in String)
   354.       with Inline => False,

 147 lines: No errors
   355.            Post   =>  resign(E) in -254 .. +256;
   356.
   357.    -- Round a 96-bit double-precision floating-point number to 48 bit format.
   358.
   359.    function narrowed (DF : CPU.f96)
   360.    return CPU.f48
   361.       with Inline;
   362.
   363.    -- Derive a 96-bit fraction from the double-precision floating-point number,
   364.    --    with the mantissa bits in D9-D47 and D49-D87,
   365.    --       and with D1-D8 copies of the sign, D48 zero, and D87-D95 zero.
   366.
   367.    function fraction_pair (DF : CPU.f96)
   368.    return KDF9.pair
   369.       with Inline;
   370.
   371.    -- Convert 96-bit fraction, and an algebraic scale-factor exponent,
   372.    --    into a 96-bit floating point number, setting overflow when necessary.
   373.
   374.    procedure reconstruct (frac   : in out KDF9.pair;
   375.                           scaler : in KDF9.word);
   376.
   377.    function "-" (R : CPU.f96)
   378.    return CPU.f96;
   379.
   380.    function "+" (L, R : CPU.f96)
   381.    return CPU.f96;
   382.
   383.    function "-" (L, R : CPU.f96)
   384.    return CPU.f96;
   385.
   386.    function "*" (L, R : CPU.f48)
   387.    return CPU.f96;
   388.
   389.    function "/" (L : CPU.f96;
   390.                  R : CPU.f48)
   391.    return CPU.f48;
   392.
   393. ------------------------------------------------------------------------------------------------
   394.
   395. --
   396. --
   397.    --
   398.    -- These are the emulation host's register types and their operations.
   399.    --
   400. --
   401. --
   402.
   403.    type u_64 is mod 2**64 with Size => 64;
   404.
   405.    pragma Provide_Shift_Operators (u_64);
   406.
   407.    function as_word (u : CPU.u_64)
   408.    return KDF9.word
   409.       with Inline;
   410.
   411.    type s_64 is range -2**63 .. +2**63-1 with Size => 64;
   412.
   413.    -- The signed as_word sets the V bit if necessary.
   414.
   415.    function as_word (s : CPU.s_64)
   416.    return KDF9.word
   417.       with Inline;
   418.
   419.    function unsign is new Ada.Unchecked_Conversion(CPU.s_64, CPU.u_64);
   420.
   421.    function resign is new Ada.Unchecked_Conversion(CPU.u_64, CPU.s_64);
   422.
   423. --
   424. --
   425.    --
   426.    -- These are the 48-bit primitive, fixed-direction, shift operations.
   427.    --
   428. --
   429. --
   430.
   431.    function shift_time (amount : Natural)
   432.    return KDF9.us
   433.       with Inline;
   434.
   435.    subtype word_shift_length is Natural range 0..48;
   436.
   437.    function shift_word_left (W : KDF9.word; amount : word_shift_length)
   438.    return KDF9.word
   439.       with Inline;
   440.
   441.    function shift_word_right (W : KDF9.word; amount : word_shift_length)
   442.    return KDF9.word
   443.       with Inline;
   444.
   445.    function rotate_word_left (W : KDF9.word; amount : word_shift_length)
   446.    return KDF9.word
   447.       with Inline;
   448.
   449.    function rotate_word_right (W : KDF9.word; amount : word_shift_length)
   450.    return KDF9.word
   451.       with Inline;
   452.
   453.    -- scale_up may set the V bit.
   454.
   455.    function scale_up (W : KDF9.word; amount : Natural)
   456.    return KDF9.word
   457.       with Inline;
   458.
   459.    -- scale_down_and_round rounds correctly.
   460.
   461.    function scale_down_and_round (W : KDF9.word; amount : Natural)
   462.    return KDF9.word
   463.       with Inline;
   464.
   465.    -- scale_down never rounds.
   466.
   467.    function scale_down (W : KDF9.word; amount : Natural)
   468.    return KDF9.word
   469.       with Inline;
   470.
   471. end KDF9.CPU;

 1107 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/disassembly.adb
Source file time stamp: 2020-11-02 19:40:27
Compiled at: 2020-11-12 18:12:11

     1. -- disassembly.adb
     2. --
     3. -- Produce dis-assembled instructions in an approximation to KDF9 Usercode.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with exceptions;
    20. with formatting;
    21. with KDF9.CPU;
    22. with KDF9.decoding;
    23.
    24. use  exceptions;
    25. use  formatting;
    26. use  KDF9.CPU;
    27. use  KDF9.decoding;
    28.
    29. package body disassembly is
    30.
    31.    function flagged (flag : String; s : KDF9.syllable)
    32.    return String
    33.    is (flag & oct_of(s));
    34.
    35.    function machine_code (decoded : KDF9.decoded_order)
    36.    return String
    37.    is (
    38.        case decoded.kind is
    39.           when one_syllable_order => flagged("#", decoded.order.syllable_0),
    40.
    41.           when two_syllable_order => flagged("#", decoded.order.syllable_0)
    42.                                    & flagged(":", decoded.order.syllable_1),
    43.           when normal_jump_order
    44.              | data_access_order  => flagged("#", decoded.order.syllable_0)
    45.                                    & flagged(":", decoded.order.syllable_1)
    46.                                    & flagged(":", decoded.order.syllable_2)
    47.       );
    48.
    49.    function one_syllable_order_name (decoded : KDF9.decoded_order)
    50.    return String
    51.    is (
    52.        case decoded.compressed_opcode is
    53.           when ABS_9   => "ABS",
    54.           when ABSF    => "ABSF",
    55.           when AND_9   => "AND",
    56.           when BITS    => "BITS",
    57.           when CAB     => "CAB",
    58.           when CONT    => "CONT",
    59.           when DIV     => "/",
    60.           when DIVD    => "/D",
    61.           when DIVDF   => "/DF",
    62.           when DIVF    => "/F",
    63.           when DIVI    => "/I",
    64.           when DIVR    => "/R",
    65.           when DUMMY   => "DUMMY",
    66.           when DUP     => "DUP",
    67.           when DUPD    => "DUPD",
    68.           when ERASE   => "ERASE",
    69.           when FIX     => "FIX",
    70.           when FLOAT_9 => "FLOAT",
    71.           when FLOATD  => "FLOATD",
    72.           when FRB     => "FRB",
    73.           when MAX     => "MAX",
    74.           when MAXF    => "MAXF",
    75.           when MINUS   => "-",
    76.           when MINUSD  => "-D",
    77.           when MINUSDF => "-DF",
    78.           when MINUSF  => "-F",
    79.           when NEG     => "NEG",
    80.           when NEGD    => "NEGD",
    81.           when NEGDF   => "NEGDF",
    82.           when NEGF    => "NEGF",
    83.           when NEV     => "NEV",
    84.           when NOT_9   => "NOT",
    85.           when OR_9    => "OR",
    86.           when PERM    => "PERM",
    87.           when PLUS    => "+",
    88.           when PLUSD   => "+D",
    89.           when PLUSDF  => "+DF",
    90.           when PLUSF   => "+F",
    91.           when REV     => "REV",
    92.           when REVD    => "REVD",
    93.           when ROUND   => "ROUND",
    94.           when ROUNDF  => "ROUNDF",
    95.           when ROUNDH  => "ROUNDH",
    96.           when ROUNDHF => "ROUNDHF",
    97.           when SIGN    => "SIGN",
    98.           when SIGNF   => "SIGNF",
    99.           when STAND   => "STAND",
   100.           when STR     => "STR",
   101.           when TO_TR   => "=TR",
   102.           when TOB     => "TOB",
   103.           when VR      => "VR",
   104.           when X_frac  => "×",
   105.           when XD      => "×D",
   106.           when XDF     => "×DF",
   107.           when XF      => "×F",
   108.           when XPLUSF  => "×+F",
   109.           when ZERO    => "ZERO",
   110.           when 0       => "DUMMY0",
   111.           when others  =>  machine_code(decoded)
   112.        );
   113.
   114.    function two_syllable_order_name (decoded : KDF9.decoded_order)
   115.    return String is
   116.
   117.       default : constant String := machine_code(decoded);
   118.       invalid : constant String := "";
   119.       k       : constant String := trimmed(decoded.Qk'Image);
   120.       q       : constant String := trimmed(decoded.Qq'Image);
   121.       opcode  : constant KDF9.compressed_opcode := (decoded.Qk and not manual_bit);
   122.       CT      : constant Boolean := (decoded.Qk and manual_bit) = 0;
   123.
   124.       function IO_order (stem : String)
   125.       return String
   126.       is (if stem = invalid then default else stem & "Q" & q);
   127.
   128.       function IO_order_name
   129.       return String
   130.       is (
   131.           case decoded.compressed_opcode is
   132.                when PIA_PIC_CLO_TLO_Qq =>
   133.                                    IO_order(case opcode is
   134.                                                when PIA_bits => "PIA",
   135.                                                when PIC_bits => "PIC",
   136.                                                when CLO_bits => "CLO",
   137.                                                when TLO_bits => "TLO",
   138.                                                when others   => invalid),
   139.                when PIB_PID_Qq =>
   140.                                    IO_order(case opcode is
   141.                                                when PIB_bits => "PIB",
   142.                                                when PID_bits => "PID",
   143.                                                when others   => invalid),
   144.                when PIE_PIG_Qq =>
   145.                                    IO_order(case opcode is
   146.                                                when PIE_bits => "PIE",
   147.                                                when PIG_bits => "PIG",
   148.                                                when others   => invalid),
   149.                when PIF_PIH_Qq =>
   150.                                    IO_order(case opcode is
   151.                                                when PIF_bits => "PIF",
   152.                                                when PIH_bits => "PIH",
   153.                                                when others   => invalid),
   154.                when PMA_PMK_INT_Qq =>
   155.                                    IO_order(case opcode is
   156.                                                when PMA_bits => "PMA",
   157.                                                when PMK_bits => "PMK",
   158.                                                when INT_bits => "INT",
   159.                                                when others   => invalid),
   160.                when CT_PMB_PMC_BUSY_Qq =>
   161.                                    IO_order(case opcode is
   162.                                                when PMB_bits  => "PMB",
   163.                                                when PMC_bits  => "PMC",
   164.                                                when BUSY_bits => "BUSY",
   165.                                                when CTQ_bits => (if CT then "CT" else "MANUAL"),
   166.                                                when others    => invalid),
   167.                when PMD_PME_PML_Qq =>
   168.                                    IO_order(case opcode is
   169.                                                when PMD_bits => "PMD",
   170.                                                when PME_bits => "PME",
   171.                                                when PML_bits => "PML",
   172.                                                when others   => invalid),
   173.                when PMF_PMG_Qq =>
   174.                                    IO_order(case opcode is
   175.                                                when PMF_bits => "PMF",
   176.                                                when PMG_bits => "PMG",
   177.                                                when others   => invalid),
   178.                when POA_POC_POE_POF_PMH_Qq =>
   179.                                    IO_order(case opcode is
   180.                                                when POA_bits => "POA",
   181.                                                when POC_bits => "POC",
   182.                                                when POE_bits => "POE",
   183.                                                when POF_bits => "POF",
   184.                                                when PMH_bits => "PMH",
   185.                                                when others   => invalid),
   186.                when POB_POD_Qq =>
   187.                                    IO_order(case opcode is
   188.                                                when POB_bits => "POB",
   189.                                                when POD_bits => "POD",
   190.                                                when others   => invalid),
   191.                when POG_POL_Qq =>
   192.                                    IO_order(case opcode is
   193.                                                when POG_bits => "POG",
   194.                                                when POL_bits => "POL",
   195.                                                when others   => invalid),
   196.                when POH_POK_Qq =>
   197.                                    IO_order(case opcode is
   198.                                                when POH_bits => "POH",
   199.                                                when POL_bits => "POK",
   200.                                                when others   => invalid),
   201.                when PAR_Qq =>      IO_order("PAR"),
   202.                when others =>      IO_order(invalid)
   203.          );
   204.
   205.       function indirect_store_order_name (suffix : String := "")
   206.       return String
   207.       is ("=M" & k & "M" & q & suffix);
   208.
   209.       function indirect_fetch_order_name (suffix : String := "")
   210.       return String
   211.       is ("M" & k & "M" & q & suffix);
   212.
   213.       function Qq_to_Qk_name (part : String)
   214.       return String
   215.       is (part & q & " TO Q" & k);
   216.
   217.       function Qq_order_name (action : String; suffix : String := "")
   218.       return String
   219.       is (action & q & suffix);
   220.
   221.       function shift_count
   222.       return String is
   223.          constant_flag : constant := 1;
   224.          fixed_shift   : CPU.signed_Q_part;
   225.       begin
   226.          if (decoded.order.syllable_1 and constant_flag) /= 0  then
   227.             fixed_shift := resign(KDF9.Q_part(decoded.order.syllable_1/2));
   228.             if fixed_shift > 63 then
   229.                fixed_shift := fixed_shift - 128;
   230.             end if;
   231.             return (if fixed_shift < 0 then "" else "+") & trimmed(fixed_shift'Image);
   232.          else
   233.             return "C" & q;
   234.          end if;
   235.       end shift_count;
   236.
   237.       function shift_order_name (action : String)
   238.       return String
   239.       is (action & shift_count);
   240.
   241.    begin -- two_syllable_order_name
   242.       return
   243.          (
   244.           case decoded.compressed_opcode is
   245.              when MkMq       => indirect_fetch_order_name,
   246.              when MkMqQ      => indirect_fetch_order_name(suffix => "Q"),
   247.              when MkMqH      => indirect_fetch_order_name(suffix => "H"),
   248.              when MkMqQH     => indirect_fetch_order_name(suffix => "QH"),
   249.              when MkMqN      => indirect_fetch_order_name(suffix => "N"),
   250.              when MkMqQN     => indirect_fetch_order_name(suffix => "QN"),
   251.              when MkMqHN     => indirect_fetch_order_name(suffix => "HN"),
   252.              when MkMqQHN    => indirect_fetch_order_name(suffix => "QHN"),
   253.
   254.              when TO_MkMq    => indirect_store_order_name,
   255.              when TO_MkMqQ   => indirect_store_order_name(suffix => "Q"),
   256.              when TO_MkMqH   => indirect_store_order_name(suffix => "H"),
   257.              when TO_MkMqQH  => indirect_store_order_name(suffix => "QH"),
   258.              when TO_MkMqN   => indirect_store_order_name(suffix => "N"),
   259.              when TO_MkMqQN  => indirect_store_order_name(suffix => "QN"),
   260.              when TO_MkMqHN  => indirect_store_order_name(suffix => "HN"),
   261.              when TO_MkMqQHN => indirect_store_order_name(suffix => "QHN"),
   262.
   263.              when M_PLUS_Iq  => Qq_order_name("M+I"),
   264.              when M_MINUS_Iq => Qq_order_name("M-I"),
   265.              when NCq        => Qq_order_name("NC"),
   266.              when DCq        => Qq_order_name("DC"),
   267.              when POS1_TO_Iq => Qq_order_name("I",  suffix => "=+1"),
   268.              when NEG1_TO_Iq => Qq_order_name("I",  suffix => "=-1"),
   269.              when POS2_TO_Iq => Qq_order_name("I",  suffix => "=+2"),
   270.              when NEG2_TO_Iq => Qq_order_name("I",  suffix => "=+2"),
   271.              when JCqNZS     => Qq_order_name("JC", suffix => "NZS"),
   272.
   273.              when MqTOQk     => Qq_to_Qk_name("M"),
   274.              when IqTOQk     => Qq_to_Qk_name("I"),
   275.              when IMqTOQk    => Qq_to_Qk_name("IM"),
   276.              when CqTOQk     => Qq_to_Qk_name("C"),
   277.              when CMqTOQk    => Qq_to_Qk_name("CM"),
   278.              when CIqTOQk    => Qq_to_Qk_name("CI"),
   279.              when QqTOQk     => Qq_to_Qk_name("Q"),
   280.              when QCIMq =>
   281.                 (
   282.                  if (decoded.Qk and all_Q_choice) = all_Q_choice then  Qq_order_name("Q")
   283.                  elsif (decoded.Qk and M_part_choice) /= 0       then  Qq_order_name("M")
   284.                  elsif (decoded.Qk and C_part_choice) /= 0       then  Qq_order_name("C")
   285.                  elsif (decoded.Qk and I_part_choice) /= 0       then  Qq_order_name("I")
   286.                  else  default
   287.                 ),
   288.              when TO_RCIMq =>
   289.                 (
   290.                  if (decoded.Qk and all_Q_choice) = all_Q_choice then Qq_order_name("=Q")
   291.                  elsif (decoded.Qk and M_part_choice) /= 0 then
   292.                     Qq_order_name(if (decoded.Qk and reset_choice) /= 0 then "=RM" else "=M")
   293.                  elsif (decoded.Qk and C_part_choice) /= 0 then
   294.                     Qq_order_name(if (decoded.Qk and reset_choice) /= 0 then "=RC" else "=C")
   295.                  elsif (decoded.Qk and I_part_choice) /= 0 then
   296.                     Qq_order_name(if (decoded.Qk and reset_choice) /= 0 then "=RI" else "=I")
   297.                  else default
   298.                 ),
   299.              when ADD_TO_QCIMq =>
   300.                 (
   301.                  if (decoded.Qk and all_Q_choice) = all_Q_choice then Qq_order_name("=+Q")
   302.                  elsif (decoded.Qk and M_part_choice) /= 0       then Qq_order_name("=+M")
   303.                  elsif (decoded.Qk and C_part_choice) /= 0       then Qq_order_name("=+C")
   304.                  elsif (decoded.Qk and I_part_choice) /= 0       then Qq_order_name("=+I")
   305.                  else  default
   306.                 ),
   307.
   308.              when SHA   => shift_order_name("SHA"),
   309.              when SHAD  => shift_order_name("SHAD"),
   310.              when MACC  => shift_order_name("×+"),
   311.              when SHL   => shift_order_name("SHL"),
   312.              when SHLD  => shift_order_name("SHLD"),
   313.              when SHC   => shift_order_name("SHC"),
   314.
   315.              when TO_Kq =>
   316.                 (
   317.                  case decoded.Qq is
   318.                     when K0 => "=K0",
   319.                     when K1 => "=K1",
   320.                     when K2 => "=K2",
   321.                     when K3 => "=K3",
   322.                     when others => default
   323.                 ),
   324.              when Kk =>
   325.                 (
   326.                  case decoded.Qk is
   327.                    when K4 => "K4",
   328.                    when K5 => "K5",
   329.                    when K7 => "K7",
   330.                    when others => default
   331.                 ),
   332.
   333.              when LINK    => "LINK",
   334.              when TO_LINK => "=LINK",
   335.
   336.              when others  => IO_order_name
   337.           );
   338.    end two_syllable_order_name;
   339.
   340.    function normal_jump_order_name (decoded      : KDF9.decoded_order;
   341.                                     octal_option : Boolean := True;
   342.                                     both_bases   : Boolean := True)
   343.    return String is
   344.
   345.       the_target  : syllable_address renames decoded.target;
   346.       the_address : constant String := oct_or_dec_of(the_target, octal_option);
   347.       remark      : constant String
   348.                   := ";("
   349.                    & (if   octal_option
   350.                       then dec_of(KDF9.Q_part(the_target.order_word_number))
   351.                       else "#" & oct_of(the_target.order_word_number))
   352.                    & ")";
   353.
   354.       function jump (on_condition : String)
   355.       return String
   356.       is ("JE" & the_address & on_condition & (if both_bases then remark else ""));
   357.
   358.       function leave (and_how : String)
   359.       return String
   360.       is ("EXIT" & and_how & remark);
   361.
   362.    begin  -- normal_jump_order_name
   363.       return (
   364.               case decoded.compressed_opcode is
   365.                  when JrEQ   => jump("EQ"),
   366.                  when JrGTZ  => jump("GTZ"),
   367.                  when JrLTZ  => jump("LTZ"),
   368.                  when JrEQZ  => jump("EQZ"),
   369.                  when JrV    => jump("V"),
   370.                  when JrEN   => jump("EN"),
   371.                  when Jr     => jump(""),
   372.                  when JrEJ   => jump("EJ"),
   373.                  when JrTR   => jump("TR"),
   374.                  when JrNE   => jump("NE"),
   375.                  when JrLEZ  => jump("LEZ"),
   376.                  when JrGEZ  => jump("GEZ"),
   377.                  when JrNEZ  => jump("NEZ"),
   378.                  when JrNV   => jump("NV"),
   379.                  when JrNEN  => jump("NEN"),
   380.                  when JrNEJ  => jump("NEJ"),
   381.                  when JrNTR  => jump("NTR"),
   382.
   383.                  when JrCqZ  => jump("C" & trimmed(decoded.Qq'Image) & "Z"),
   384.                  when JrCqNZ => jump("C" & trimmed(decoded.Qq'Image) & "NZ"),
   385.
   386.                  when JSr    => "JSE" & the_address,
   387.                  when OS_OUT => "OUT",
   388.
   389.                  when EXITD  => leave("D"),
   390.                  when EXIT_n =>
   391.                     -- Try to give the most helpful interpretation of the operand.
   392.                     (
   393.                      if the_target.syllable_index = 0 then  -- c.f. decode_a_jump_order.
   394.                         -- No halfword offset applies.
   395.                         (
   396.                          if the_target.order_word_number < 4 then
   397.                            leave(
   398.                                  if the_target.order_word_number = 0
   399.                                  then ""
   400.                                  else oct_of(KDF9.Q_part(2*the_target.order_word_number), 1)
   401.                                 )
   402.                          else
   403.                            leave("AE" & oct_or_dec_of((the_target.order_word_number, 0), octal_option))
   404.                         )
   405.                      elsif the_target.order_word_number < 4 then
   406.                         leave(oct_of(KDF9.Q_part(2*the_target.order_word_number + 1), 1))
   407.                      else
   408.                         leave("AE" & oct_or_dec_of((the_target.order_word_number, 3), octal_option))
   409.                     ),
   410.
   411.                  when others =>  machine_code(decoded)
   412.              );
   413.    end normal_jump_order_name;
   414.
   415.    function data_access_order_name (decoded      : KDF9.decoded_order;
   416.                                     octal_option : Boolean;
   417.                                     both_bases   : Boolean := True)
   418.    return String is
   419.
   420.       operand      : KDF9.Q_part   renames decoded.operand;
   421.       Qq           : KDF9.Q_number renames decoded.Qq;
   422.       the_address  : constant String
   423.                    := (if octal_option then "#" & oct_of(operand, 1) else dec_of(operand));
   424.       remark       : constant String
   425.                    := ";(" & (if octal_option then dec_of(operand) else "#" & oct_of(operand, 1)) & ")";
   426.       any_modifier : constant String
   427.                    := (if Qq /= 0 then "M" & trimmed(Qq'Image) else "");
   428.    begin
   429.       return (
   430.               case decoded.compressed_opcode is
   431.                  when EaMq     => "E"   & the_address & any_modifier & remark,
   432.                  when TO_EaMq  => "=E"  & the_address & any_modifier & remark,
   433.                  when EaMqQ    => "E"   & the_address & any_modifier & "Q" & remark,
   434.                  when TO_EaMqQ => "=E"  & the_address & any_modifier & "Q" & remark,
   435.                  when SET      => "SET" & (
   436.                                            if octal_option
   437.                                            then "B" & oct_of(operand, 2)
   438.                                               & (
   439.                                                  if operand > 7 and both_bases
   440.                                                  then ";(" & signed_dec_of(operand) & ")"
   441.                                                  else ""
   442.                                                 )
   443.                                            else signed_dec_of(operand)
   444.                                               & (
   445.                                                  if operand > 9 and both_bases
   446.                                                  then ";(B" & oct_of(operand, 2) & ")"
   447.                                                  else ""
   448.                                                 )
   449.                                           ),
   450.                  when others   => "?"
   451.              );
   452.    end data_access_order_name;
   453.
   454.    function the_full_name_of (order        : KDF9.decoded_order;
   455.                               octal_option : Boolean := True;
   456.                               both_bases   : Boolean := True)
   457.    return String is
   458.       result : constant String
   459.          := (
   460.              case order.kind is
   461.                 when one_syllable_order => one_syllable_order_name(order),
   462.                 when two_syllable_order => two_syllable_order_name(order),
   463.                 when normal_jump_order  => normal_jump_order_name(order, octal_option, both_bases),
   464.                 when data_access_order  => data_access_order_name(order, octal_option, both_bases)
   465.             );
   466.    begin
   467.       return (if result(1) /= '?' then result else "an INVALID order");
   468.    end the_full_name_of;
   469.
   470.    function the_code_and_name_of_INS
   471.    return String
   472.    is (machine_code(INS) & ", i.e. " & the_full_name_of(INS));
   473.
   474.    function two_syllable_skeleton (encoding : KDF9.syllable)
   475.    return String is
   476.
   477.       function IO_skeleton
   478.       return String
   479.       is (
   480.           case encoding and 8#77# is
   481.              when POA_POC_POE_POF_PMH_Qq => "{POA|POC|POE|POF|PMH}Qq",
   482.              when PIA_PIC_CLO_TLO_Qq     => "{PIA|PIC|CLO|TLO}Qq",
   483.              when CT_PMB_PMC_BUSY_Qq     => "{BUSY|CT|MANUAL|PMB|PMC}Qq",
   484.              when PAR_Qq                 => "PARQq",
   485.              when PIB_PID_Qq             => "{PIB|PID}Qq",
   486.              when PIE_PIG_Qq             => "{PIE|PIG}Qq",
   487.              when PIF_PIH_Qq             => "{PIF|PIH}Qq",
   488.              when PMA_PMK_INT_Qq         => "{INT|PMA|PMK}Qq",
   489.              when PMD_PME_PML_Qq         => "{PMD|PME}Qq",
   490.              when PMF_PMG_Qq             => "{PMF|PMG}Qq",
   491.              when POB_POD_Qq             => "{POB|POD}Qq",
   492.              when POG_POL_Qq             => "{POG|POL}Qq",
   493.              when POH_POK_Qq             => "{POH|POK}Qq",
   494.              when others                 => raise emulation_failure
   495.                                                with "invalid code in IO_skeleton"
   496.          );
   497.
   498.    begin  -- two_syllable_skeleton
   499.       return
   500.          (
   501.           case encoding and 8#77# is
   502.              when MkMq         => "MkMq",
   503.              when MkMqQ        => "MkMqQ",
   504.              when MkMqH        => "MkMqH",
   505.              when MkMqQH       => "MkMqQH",
   506.              when MkMqN        => "MkMqN",
   507.              when MkMqQN       => "MkMqQN",
   508.              when MkMqHN       => "MkMqHN",
   509.              when MkMqQHN      => "MkMqQHN",
   510.
   511.              when TO_MkMq      => "=MkMq",
   512.              when TO_MkMqQ     => "=MkMqQ",
   513.              when TO_MkMqH     => "=MkMqH",
   514.              when TO_MkMqQH    => "=MkMqQH",
   515.              when TO_MkMqN     => "=MkMqN",
   516.              when TO_MkMqQN    => "=MkMqQN",
   517.              when TO_MkMqHN    => "=MkMqHN",
   518.              when TO_MkMqQHN   => "=MkMqQHN",
   519.
   520.              when JCqNZS       => "JCqNZS",
   521.              when M_PLUS_Iq    => "M+Iq",
   522.              when M_MINUS_Iq   => "M-Iq",
   523.              when NCq          => "NCq",
   524.              when DCq          => "DCq",
   525.              when POS1_TO_Iq   => "Iq=+1",
   526.              when NEG1_TO_Iq   => "Iq=-1",
   527.              when POS2_TO_Iq   => "Iq=+2",
   528.              when NEG2_TO_Iq   => "Iq=-2",
   529.
   530.              when MqTOQk       => "MqTOQk",
   531.              when IqTOQk       => "IqTOQk",
   532.              when IMqTOQk      => "IMqTOQk",
   533.              when CqTOQk       => "CqTOQk",
   534.              when CMqTOQk      => "CMqTOQk",
   535.              when CIqTOQk      => "CIqTOQk",
   536.              when QqTOQk       => "QqTOQk",
   537.
   538.              when QCIMq        => "{Q|C|I|M}q",
   539.              when TO_RCIMq     => "=[R]{Q|C|I|M}q",
   540.              when ADD_TO_QCIMq => "=+{Q|C|I|M}q",
   541.
   542.              when SHA          => "SHA",
   543.              when SHAD         => "SHAD",
   544.              when MACC         => "×+",
   545.              when SHL          => "SHL",
   546.              when SHLD         => "SHLD",
   547.              when SHC          => "SHC",
   548.
   549.              when TO_Kq =>
   550.                 (
   551.                  case encoding / 16 mod 16 is
   552.                     when K0 => "=K0",
   553.                     when K1 => "=K1",
   554.                     when K2 => "=K2",
   555.                     when K3 => "=K3",
   556.                     when others => "=K?"
   557.                 ),
   558.              when Kk =>
   559.                 (
   560.                  case encoding mod 16 is
   561.                     when K4 => "K4",
   562.                     when K5 => "K5",
   563.                     when K7 => "K7",
   564.                     when others => "K?"
   565.                 ),
   566.
   567.              when LINK =>    "LINK",
   568.              when TO_LINK => "=LINK",
   569.
   570.              when others =>  IO_skeleton
   571.          );
   572.    end two_syllable_skeleton;
   573.
   574.    function normal_jump_skeleton (encoding : KDF9.syllable)
   575.    return String
   576.    is (
   577.        case encoding and 8#77# is
   578.           when JrCqZ  .. JrCqZ+2#1111#  => "JrCqZ",
   579.           when JrCqNZ .. JrCqNZ+2#1111# => "JrCqNZ",
   580.           when JrEQ   => "JrEQ",
   581.           when JrGTZ  => "JrGTZ",
   582.           when JrLTZ  => "JrLTZ",
   583.           when JrEQZ  => "JrEQZ",
   584.           when JrV    => "JrV",
   585.           when JrEN   => "JrEN",
   586.           when Jr     => "Jr",
   587.           when JrEJ   => "JrEJ",
   588.           when JSr    => "JSr",
   589.           when JrTR   => "JrTR",
   590.           when EXIT_n => "EXIT",
   591.           when JrNE   => "JrNE",
   592.           when JrLEZ  => "JrLEZ",
   593.           when JrGEZ  => "JrGEZ",
   594.           when JrNEZ  => "JrNEZ",
   595.           when JrNV   => "JrNV",
   596.           when JrNEN  => "JrNEN",
   597.           when JrNEJ  => "JrNEJ",
   598.           when JrNTR  => "JrNTR",
   599.           when OS_OUT => "OUT",
   600.           when EXITD  => "EXITD",
   601.           when others => raise emulation_failure
   602.                             with "invalid code in normal_jump_skeleton"
   603.       );
   604.
   605.    function data_access_skeleton (compressed_opcode : KDF9.compressed_opcode)
   606.    return String
   607.    is (
   608.        case compressed_opcode is
   609.           when EaMq     => "EeMq",
   610.           when TO_EaMq  => "=EeMq",
   611.           when EaMqQ    => "EeMqQ",
   612.           when TO_EaMqQ => "=EeMqQ",
   613.           when SET      => "SET",
   614.           when others   => raise emulation_failure
   615.                               with "invalid compressed opcode in data_access_skeleton"
   616.       );
   617.
   618.    function the_short_name_of (syllable_0 : KDF9.syllable)
   619.    return String is
   620.       its_INS : KDF9.decoded_order := (order => (syllable_0, 0, 0), others => <>);
   621.    begin
   622.       decode(its_INS);
   623.       return
   624.          (
   625.           case KDF9.INS_kind(syllable_0 / 2**6) is
   626.              when one_syllable_order   => one_syllable_order_name(its_INS),
   627.              when two_syllable_order   => two_syllable_skeleton(syllable_0),
   628.              when normal_jump_order    => normal_jump_skeleton(syllable_0),
   629.              when data_access_order    => data_access_skeleton(its_INS.compressed_opcode)
   630.          );
   631.    end the_short_name_of;
   632.
   633. end disassembly;

Compiling: ../Source/disassembly.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- disassembly.ads
     2. --
     3. -- Produce dis-assembled instructions in an approximation to KDF9 Usercode.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with KDF9;
    20.
    21. use  KDF9;
    22.
    23. package disassembly is
    24.
    25.    function the_code_and_name_of_INS
    26.    return String;
    27.
    28.    function the_full_name_of (order        : KDF9.decoded_order;
    29.                               octal_option : Boolean := True;
    30.                               both_bases   : Boolean := True)
    31.    return String;
    32.
    33.    function the_short_name_of (syllable_0 : KDF9.syllable)
    34.    return String;
    35.
    36. end disassembly;

 633 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/kdf9-phu_store.adb
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:11

     1. -- kdf9-PHU_store.adb
     2. --
     3. -- The K5 operation data formats.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Unchecked_Conversion;
    20. --
    21. with KDF9.CPU;
    22.
    23. package body KDF9.PHU_store is
    24.
    25.    function short_PHU (p : KDF9.priority)
    26.    return KDF9.word is
    27.
    28.       use type KDF9.store.group_address;
    29.
    30.       type PHU_as_6_bits is mod 2**6
    31.          with Size => 6;
    32.
    33.       function as_6_bits is new Ada.Unchecked_Conversion(Source => PHU_store.PHU_subset,
    34.                                                          Target => short_PHU.PHU_as_6_bits);
    35.
    36.       the_reason    : PHU_store.blockage_kind;
    37.       the_parameter : KDF9.buffer_number;
    38.
    39.    begin
    40.       if not PHU(p).is_held_up then
    41.          return 0;  -- All fields are non-significant.
    42.       end if;
    43.
    44.       -- PHU(p).is_held_up, so other fields are valid.
    45.       the_reason := PHU(p).blockage.reason;
    46.       if the_reason = buffer_busy then
    47.          the_parameter := PHU(p).blockage.buffer_nr;
    48.       else
    49.          -- This is next to useless, but is what the K5 order actually did.
    50.          the_parameter := KDF9.buffer_number(PHU(p).blockage.group_nr mod 2**4);
    51.       end if;
    52.
    53.       return KDF9.word(as_6_bits((the_parameter, the_reason, True)));
    54.    end short_PHU;
    55.
    56.    function K5_operand
    57.    return KDF9.word
    58.    is (
    59.        KDF9.CPU.shift_word_left(short_PHU(0), 48-06) or
    60.        KDF9.CPU.shift_word_left(short_PHU(1), 48-12) or
    61.        KDF9.CPU.shift_word_left(short_PHU(2), 48-18) or
    62.        KDF9.CPU.shift_word_left(short_PHU(3), 48-24)
    63.       );
    64.
    65. end KDF9.PHU_store;

Compiling: ../Source/kdf9-phu_store.ads
Source file time stamp: 2020-10-02 17:39:37
Compiled at: 2020-11-12 18:12:11

     1. -- kdf9-PHU_store.ads
     2. --
     3. -- The K5 operation data formats.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with System;
    20. --
    21. with KDF9.store;
    22.
    23. package KDF9.PHU_store is
    24.
    25.    -- PHU, the Program Hold-Up register is internal to I/O Control.
    26.    -- It has one element for each of the 4 program priority levels, 0..3.
    27.    -- A subset of its content is exposed to Director by means of the K5 order.
    28.
    29.    type blockage_kind is (buffer_busy, locked_core) with Size => 1;
    30.
    31.    type PHU_reason (reason : PHU_store.blockage_kind := buffer_busy) is
    32.       record
    33.          case reason is
    34.             when buffer_busy =>
    35.                buffer_nr : KDF9.buffer_number;
    36.                by_INTQq  : Boolean;
    37.             when locked_core =>
    38.                group_nr  : KDF9.store.group_address;
    39.          end case;
    40.       end record;
    41.
    42.    type PHU_register (is_held_up : Boolean := False) is
    43.       record
    44.          case is_held_up is
    45.             when False =>
    46.                null;
    47.             when True =>
    48.                blockage : PHU_reason;
    49.          end case;
    50.       end record;
    51.
    52.    idle_PHU : constant PHU_register := (is_held_up => False);
    53.
    54.    PHU : array (KDF9.priority) of PHU_store.PHU_register := (others => idle_PHU);
    55.
    56.    type PHU_subset is
    57.       record
    58.          parameter  : KDF9.buffer_number;
    59.          reason     : PHU_store.blockage_kind;
    60.          is_held_up : Boolean;
    61.       end record
    62.    with Size => 6, Bit_Order => System.Low_Order_First;
    63.
    64.    for  PHU_subset use
    65.       record
    66.          parameter  at 0 range 0 .. 3;
    67.          reason     at 0 range 4 .. 4;
    68.          is_held_up at 0 range 5 .. 5;
    69.       end record;
    70.
    71.    -- A K5_operand is a KDF9 word, D00-D47, with the content:
    72.    --    PHU_subset(0) in D00 .. D05
    73.    --    PHU_subset(1) in D06 .. D11
    74.    --    PHU_subset(2) in D12 .. D17
    75.    --    PHU_subset(3) in D18 .. D23
    76.    --    zeros         in D24 .. D47
    77.
    78.    function K5_operand
    79.    return KDF9.word;
    80.
    81. end KDF9.PHU_store;

 65 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/host_io.adb
Source file time stamp: 2020-10-27 18:36:22
Compiled at: 2020-11-12 18:12:12

     1. -- host_IO.adb
     2. --
     3. -- Buffered I/O streams to support KDF9 device I/O.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Characters.Latin_1;
    20. with Ada.Exceptions;
    21. --
    22. with exceptions;
    23. with OS_specifics;
    24.
    25. use  Ada.Characters.Latin_1;
    26. --
    27. use  exceptions;
    28. use  OS_specifics;
    29.
    30. package body host_IO is
    31.
    32.    function fd_of (the_stream : host_IO.stream)
    33.    return Natural
    34.    is (the_stream.fd);
    35.
    36.    NL : constant String := OS_specifics.EOL;
    37.
    38.    function image_of (the_stream : host_IO.stream;
    39.                       caption    : String := "")
    40.    return String
    41.    is (if the_stream.is_open then
    42.           caption
    43.         & ":"
    44.         & NL
    45.         & "base_name = "
    46.         & the_stream.base_name
    47.         & NL
    48.         & "is_open = "
    49.         & the_stream.is_open'Image
    50.         & NL
    51.         & "block_size ="
    52.         & the_stream.block_size'Image
    53.         & NL
    54.         & "bytes_moved ="
    55.         & the_stream.bytes_moved'Image
    56.         & NL
    57.         & "fd ="
    58.         & the_stream.fd'Image
    59.         & NL
    60.         & "IO_mode ="
    61.         & the_stream.IO_mode'Image
    62.         & NL
    63.         & "last_IO ="
    64.         & the_stream.last_IO'Image
    65.         & NL
    66.         & "next_byte ="
    67.         & the_stream.next_byte'Image
    68.         & NL
    69.         & "position ="
    70.         & the_stream.position'Image
    71.         & NL
    72.         & "column = "
    73.         & the_stream.column'Image
    74.        else
    75.           caption
    76.       );
    77.
    78.    procedure diagnose (the_stream : host_IO.stream;
    79.                        caption    : String := "")
    80.       with Inline => False;
    81.
    82.    procedure diagnose (the_stream : host_IO.stream;
    83.                        caption    : String := "") is
    84.    begin
    85.       raise stream_IO_error
    86.          with image_of(the_stream, caption);
    87.    end diagnose;
    88.
    89.    procedure open (the_stream : in out host_IO.stream;
    90.                    file_name  : in String;
    91.                    mode       : in POSIX.access_mode;
    92.                    fd         : in Integer) is
    93.    begin
    94.       if fd >= 0 then
    95.          make_transparent(fd);
    96.          the_stream.base_name := file_name(file_name'First .. file_name'First+2);
    97.          the_stream.IO_mode := mode;
    98.          the_stream.last_IO := read_mode;
    99.          the_stream.fd := fd;
   100.          the_stream.is_open := True;
   101.       else
   102.          the_stream.is_open := False;
   103.       end if;
   104.    end open;
   105.
   106.    procedure open (the_stream : in out host_IO.stream;
   107.                    file_name  : in String;
   108.                    mode       : in POSIX.access_mode) is
   109.       fd : Integer;
   110.    begin -- open
   111.       fd := POSIX.open(file_name, mode);
   112.       open(the_stream, file_name, mode, fd);
   113.    exception
   114.       when POSIX_IO_error =>
   115.          trap_operator_error(file_name & " cannot be "
   116.                            & (case mode is
   117.                                  when read_mode  => "read",
   118.                                  when write_mode => "written",
   119.                                  when rd_wr_mode => "read or written")
   120.                             );
   121.    end open;
   122.
   123.    procedure truncate (the_stream : in out host_IO.stream;
   124.                        to_length  : in KDF9.word := 0) is
   125.    begin
   126.       truncate(the_stream.fd, POSIX.file_position(to_length));
   127.    end truncate;
   128.
   129.    procedure close (the_stream : in out host_IO.stream) is
   130.       response : Integer;
   131.    begin
   132.       if the_stream.is_open then
   133.          flush(the_stream);
   134.          response := close(the_stream.fd);
   135.          pragma Unreferenced(response);
   136.          the_stream.is_open := False;
   137.       end if;
   138.    end close;
   139.
   140.    procedure flush (the_stream  : in out host_IO.stream;
   141.                     a_byte_time : in KDF9.us := 0) is
   142.       response : Integer;
   143.    begin
   144.       if the_stream.is_open      and then
   145.             the_stream.next_byte > 0 then
   146.          if the_stream.IO_mode > read_mode and the_stream.last_IO = write_mode then
   147.             if a_byte_time = 0 then
   148.                response := write(the_stream.fd, the_stream.buffer, the_stream.next_byte);
   149.             else
   150.                for p in 1 .. the_stream.next_byte loop
   151.                   response := write(the_stream.fd, the_stream.buffer(p..p), 1);
   152.                   KDF9.delay_by(a_byte_time);
   153.                end loop;
   154.             end if;
   155.             pragma Unreferenced(response);
   156.          end if;
   157.          the_stream.next_byte := 0;
   158.          the_stream.block_size := 0;
   159.       end if;
   160.    exception
   161.       when error : others =>
   162.          diagnose(the_stream, "FLUSH: " & Ada.Exceptions.Exception_Message(error));
   163.    end flush;
   164.
   165.    function a_LF_was_just_read (the_stream : host_IO.stream)
   166.    return Boolean
   167.    is (
   168.        the_stream.is_open                   and then
   169.           the_stream.bytes_moved > 0        and then
   170.              the_stream.last_IO = read_mode and then
   171.                 the_stream.next_byte = 0    and then
   172.                    the_stream.block_size = 0
   173.       );
   174.
   175.    function a_LF_was_just_written (the_stream : host_IO.stream)
   176.    return Boolean
   177.    is (
   178.        the_stream.is_open                    and then
   179.           the_stream.bytes_moved > 0         and then
   180.              the_stream.last_IO /= read_mode and then
   181.                 the_stream.column = 0
   182.       );
   183.
   184.    procedure reattach (the_stream : in out host_IO.stream;
   185.                        file_name  : in String;
   186.                        mode       : in POSIX.access_mode) is
   187.       old_fd   : constant Integer := the_stream.fd;
   188.    begin
   189.       if mode /= the_stream.IO_mode      and then
   190.             the_stream.IO_mode /= rd_wr_mode then
   191.          diagnose(the_stream, "REATTACH: the new mode is incompatible");
   192.       end if;
   193.       close(the_stream);
   194.       open(the_stream, file_name, (if the_stream.IO_mode = rd_wr_mode then rd_wr_mode else mode));
   195.       if old_fd = 0 and the_stream.fd /= 0 then
   196.          diagnose(the_stream, "REATTACH: standard input cannot be reopened");
   197.       end if;
   198.       if the_stream.is_open then
   199.          the_stream.last_char := ' ';
   200.          the_stream.block_size := 0;
   201.          the_stream.next_byte := 0;
   202.          the_stream.position := 0;
   203.       end if;
   204.    exception
   205.       when stream_IO_error =>
   206.          raise;
   207.       when error : operator_error =>
   208.          raise operator_error
   209.             with Ada.Exceptions.Exception_Information(error);
   210.       when error : others =>
   211.          diagnose(the_stream, "REATTACH: " & Ada.Exceptions.Exception_Information(error));
   212.    end reattach;
   213.
   214.    function is_open (the_stream : host_IO.stream)
   215.    return Boolean
   216.    is (the_stream.is_open);
   217.
   218.    function bytes_moved (the_stream : host_IO.stream)
   219.    return KDF9.word
   220.    is (the_stream.bytes_moved);
   221.
   222.    function file_size (the_stream : host_IO.stream)
   223.    return Natural is
   224.       here  : constant POSIX.file_position := seek(the_stream.fd, 0, from_here);
   225.       size  : constant POSIX.file_position := seek(the_stream.fd, 0, from_end);
   226.       there : constant POSIX.file_position := seek(the_stream.fd, here, from_start);
   227.    begin
   228.       if here /= there then
   229.          diagnose(the_stream, "FILE_SIZE: seek failure");
   230.       end if;
   231.       return Natural(size);
   232.    end file_size;
   233.
   234.    function column (the_stream : host_IO.stream)
   235.    return Natural
   236.    is (the_stream.column);
   237.
   238.    procedure get_position (position   : out Natural;
   239.                            the_stream : in out host_IO.stream) is
   240.    begin
   241.       flush(the_stream);
   242.       position := the_stream.position;
   243.    end get_position;
   244.
   245.    function buffer_is_empty (the_stream : host_IO.stream)
   246.    return Boolean
   247.    is (not the_stream.is_open or else the_stream.next_byte >= the_stream.block_size);
   248.
   249.    function buffer_is_full (the_stream : host_IO.stream)
   250.    return Boolean
   251.    is (the_stream.is_open and then the_stream.next_byte = the_stream.buffer'Last);
   252.
   253.    procedure set_position (position   : in Natural;
   254.                            the_stream : in out host_IO.stream;
   255.                            whence     : in POSIX.seek_origin := from_start) is
   256.       response : POSIX.file_position;
   257.    begin
   258.       flush(the_stream);
   259.       response := seek(the_stream.fd, POSIX.file_position(position), whence);
   260.       pragma Unreferenced(response);
   261.       the_stream.position := position;
   262.    exception
   263.       when error : others =>
   264.          diagnose(the_stream, "SET_POSITION: " & Ada.Exceptions.Exception_Message(error));
   265.    end set_position;
   266.
   267.    procedure clear (the_stream : in out host_IO.stream) is
   268.    begin
   269.       the_stream.next_byte := 0;
   270.       the_stream.block_size := 0;
   271.    end clear;
   272.
   273.    procedure reset (the_stream : in out host_IO.stream) is
   274.    begin
   275.       flush(the_stream);
   276.       if the_stream.is_open then
   277.          the_stream.last_IO := read_mode;
   278.          the_stream.position := 0;
   279.          the_stream.next_byte := 0;
   280.          the_stream.block_size := 0;
   281.       end if;
   282.    end reset;
   283.
   284.    procedure back_off (the_stream : in out host_IO.stream) is
   285.    begin
   286.       if the_stream.is_open                   and then
   287.             the_stream.next_byte > 0          and then
   288.                the_stream.last_IO = read_mode     then
   289.          the_stream.next_byte := the_stream.next_byte - 1;
   290.          the_stream.position := the_stream.position - 1;
   291.       else
   292.          diagnose(the_stream, "cannot back_off");
   293.       end if;
   294.    end back_off;
   295.
   296.    procedure get_byte (char       : out Character;
   297.                        the_stream : in out host_IO.stream) is
   298.       response : Integer;
   299.    begin
   300.       if not the_stream.is_open then
   301.          raise end_of_stream;
   302.       end if;
   303.       if buffer_is_empty(the_stream) then
   304.          response := read(the_stream.fd, the_stream.buffer, the_stream.buffer'Size);
   305.          the_stream.block_size := response;
   306.          if response <= 0 then
   307.             raise end_of_stream;
   308.          end if;
   309.          the_stream.next_byte := 0;
   310.       end if;
   311.       the_stream.next_byte := the_stream.next_byte + 1;
   312.       the_stream.position := the_stream.position + 1;
   313.       the_stream.bytes_moved := the_stream.bytes_moved + 1;
   314.       the_stream.last_IO := read_mode;
   315.       char := the_stream.buffer(the_stream.next_byte);
   316.       if char = LF then
   317.          the_stream.column := 0;
   318.       else
   319.          the_stream.column := the_stream.column + 1;
   320.       end if;
   321.    exception
   322.       when end_of_stream =>
   323.          raise;
   324.       when POSIX_IO_error =>
   325.          diagnose(the_stream, "GET_BYTE: POSIX_IO_error");
   326.       when error : others =>
   327.          diagnose(the_stream, Ada.Exceptions.Exception_Message(error));
   328.    end get_byte;
   329.
   330.    procedure get_bytes (the_string : out String;
   331.                         the_stream : in out host_IO.stream;
   332.                         uncounted  : in Boolean := True) is
   333.       old_bytes_moved : constant KDF9.word := the_stream.bytes_moved;
   334.    begin
   335.       for b of the_string loop
   336.          get_byte(b, the_stream);
   337.       end loop;
   338.       if uncounted then
   339.          the_stream.bytes_moved := old_bytes_moved;
   340.       end if;
   341.    end get_bytes;
   342.
   343.    procedure get_char (char       : out Character;
   344.                        the_stream : in out host_IO.stream) is
   345.    begin
   346.       get_byte(char, the_stream);
   347.       if char = CR then
   348.          char := LF;
   349.          the_stream.last_char := CR;
   350.       elsif char = LF and the_stream.last_char = CR then
   351.          the_stream.last_char := LF;
   352.          get_byte(char, the_stream);
   353.       else
   354.          the_stream.last_char := char;
   355.       end if;
   356.    end get_char;
   357.
   358.    procedure peek_at_char (char       : out Character;
   359.                            the_stream : in out host_IO.stream) is
   360.    begin
   361.       get_char(char, the_stream);
   362.       back_off(the_stream);
   363.    end peek_at_char;
   364.
   365.    -- put_escape_code writes directly to the stream's device, avoiding the stream's buffers.
   366.    procedure put_escape_code (the_string : in String;
   367.                               the_stream : in out host_IO.stream) is
   368.       response : Integer;
   369.    begin
   370.       if not the_stream.is_open then
   371.          raise end_of_stream;
   372.       end if;
   373.       response := write(the_stream.fd,
   374.                         the_string,
   375.                         the_string'Length);
   376.       if response <= 0 then
   377.          raise end_of_stream;
   378.       end if;
   379.    exception


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/kdf9-store.adb
Source file time stamp: 2020-09-09 22:51:12
Compiled at: 2020-11-12 18:12:12

     1. -- kdf9-store.adb
     2. --
     3. -- KDF9 core store operations.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Unchecked_Conversion;
    20. --
    21. with formatting;
    22. with KDF9.CPU;
    23.
    24. use formatting;
    25. use  KDF9.CPU;
    26.
    27. package body KDF9.store is
    28.
    29.    -- diagnose_invalid_address avoids secondary stack usage in the address validation procedures.
    30.    procedure diagnose_invalid_address (message : in String; address : in KDF9.word)
    31.       with Inline => False;
    32.
    33.    procedure diagnose_invalid_address (message : in String; address : in KDF9.word) is
    34.    begin
    35.       trap_invalid_instruction(message & " =" & address'Image);
    36.    end diagnose_invalid_address;
    37.
    38.    -- Check that EA, EA+BA are valid; LIV if invalid.
    39.    procedure validate_virtual_and_real_addresses (EA : in KDF9.Q_part)
    40.       with Inline;
    41.
    42.    procedure validate_virtual_and_real_addresses (EA : in KDF9.Q_part) is
    43.       PA : constant KDF9.word := (KDF9.word(EA) + KDF9.word(BA)) and Q_part_mask;
    44.    begin
    45.       if EA > NOL and then the_CPU_state = program_state then
    46.          diagnose_invalid_address("NOL < virtual address", KDF9.word(EA));
    47.       end if;
    48.       if PA > max_address and then the_CPU_state = program_state then
    49.          diagnose_invalid_address("32K-1 < physical address", PA);
    50.       end if;
    51.    end validate_virtual_and_real_addresses;
    52.
   380.       when POSIX_IO_error =>
   381.          diagnose(the_stream, "PUT_ESCAPE_CODE: POSIX_IO_error");
   382.       when error : others =>
   383.          diagnose(the_stream, Ada.Exceptions.Exception_Message(error));
   384.    end put_escape_code;
   385.
   386.    procedure put_byte (char       : in Character;
   387.                        the_stream : in out host_IO.stream) is
   388.       response : Integer;
   389.    begin
   390.       if not the_stream.is_open then
   391.          raise end_of_stream;
   392.       end if;
    53.    procedure if_user_mode_then_LOV (address_1 : KDF9.Q_part;
    54.                                     address_2 : KDF9.Q_part := 0;
    55.                                     solo      : Boolean     := True) is
    56.    begin
    57.       LOV_if_user_mode(
    58.                        if solo
    59.                        then "at #" & oct_of(address_1) & " (E" & dec_of(address_1) & ")"
    60.                        else "in #" & oct_of(address_1) & "/#" & oct_of(address_2)
    61.                       );
    62.    end if_user_mode_then_LOV;
    63.
    64.    function group (PA : KDF9.Q_part)
    65.    return KDF9.Q_part
    66.    is (PA / group_size);
    67.
    68.    procedure check_address_and_lockout (EA : in KDF9.Q_part) is
    69.       PA : constant KDF9.Q_part := EA + BA;
    70.    begin
    71.       validate_virtual_and_real_addresses(EA);
    72.       if locked_out(group(PA)) then
    73.          the_locked_out_address := PA;
    74.          if the_CPU_state /= Director_state then
    75.             if_user_mode_then_LOV(PA);
    76.          end if;
    77.       end if;
    78.    end check_address_and_lockout;
    79.
    80.    procedure validate_address_range (EA1, EA2 : in KDF9.Q_part) is
    81.    begin
    82.       validate_virtual_and_real_addresses(EA1);
    83.       validate_virtual_and_real_addresses(EA2);
    84.       if EA1 > EA2 then
    85.          diagnose_invalid_address("initial address > final address", KDF9.word(EA2));
    86.       end if;
    87.    end validate_address_range;
    88.
    89.    procedure check_addresses_and_lockouts (EA1, EA2 : in KDF9.Q_part) is
    90.        PA1 : constant KDF9.Q_part := EA1 + BA;
    91.        PA2 : constant KDF9.Q_part := EA2 + BA;
    92.    begin
    93.       validate_address_range (EA1, EA2);
    94.       if there_are_locks_in_physical_addresses(KDF9.Q_register'(C => 0, I => PA1, M => PA2)) then
    95.          if the_CPU_state /= Director_state then
    96.             if_user_mode_then_LOV(PA1, PA2, solo => False);
    97.          end if;
    98.       end if;
    99.    end check_addresses_and_lockouts;
   100.
   101.    -- Check that A1+A2 is valid; trap if it is invalid.
   102.    function valid_word_address (A1, A2 : in KDF9.Q_part)
   103.    return KDF9.address is
   104.       V : constant KDF9.word := (KDF9.word(A1) + KDF9.word(A2)) and Q_part_mask;
   105.    begin
   106.       if V > max_address then
   107.          diagnose_invalid_address("32K-1 < virtual address", V);
   108.       end if;
   109.       return KDF9.address(V);
   110.    end valid_word_address;
   111.
   112.    function signed is new Ada.Unchecked_Conversion (KDF9.Q_part, CPU.signed_Q_part);
   113.    function design is new Ada.Unchecked_Conversion (CPU.signed_Q_part, KDF9.Q_part);
   114.
   115.    -- Check that A1+A2/2 is valid; trap if it is invalid.  A2 must be treated as a signed number.
   116.    function valid_halfword_address (A1, A2 : in KDF9.Q_part)
   117.    return KDF9.address is
   118.       V : constant KDF9.word := (KDF9.word(A1) + KDF9.word(design(signed(A2)/2))) and Q_part_mask;
   119.    begin
   120.       if V > max_address then
   121.          diagnose_invalid_address("32K-1 < virtual address", V);
   122.       end if;
   123.       return KDF9.address(V);
   393.       if the_stream.buffer_is_full then
   394.          response := write(the_stream.fd,
   395.                            the_stream.buffer,
   396.                            the_stream.buffer'Size);
   397.          if response <= 0 then
   398.             raise end_of_stream;
   399.          end if;
   124.    end valid_halfword_address;
   125.
   126.    function fetch_symbol (EA : KDF9.address; index : KDF9_char_sets.symbol_index)
   127.    return KDF9_char_sets.symbol
   128.    is (KDF9_char_sets.symbol(shift_word_right(core(EA+BA), 42 - 6*Natural(index)) and 8#77#));
   129.
   130.    procedure store_symbol (value : in KDF9_char_sets.symbol;
   131.                            EA    : in KDF9.address;
   132.                            index : in KDF9_char_sets.symbol_index) is
   133.       place  : constant Natural   := 42 - 6*Natural(index);
   134.       mask   : constant KDF9.word := not shift_word_left(8#77#, place);
   135.       symbol : constant KDF9.word := shift_word_left(KDF9.word(value), place);
   136.    begin
   137.       core(EA+BA) := (core(EA+BA) and mask) or symbol;
   138.    end store_symbol;
   139.
   140.    function fetch_octet (EA : KDF9.address; index : KDF9_char_sets.octet_index)
   141.    return KDF9_char_sets.octet is
   142.       place : constant Natural := 40 - 8*Natural(index);
   143.    begin
   144.       return KDF9_char_sets.octet(shift_word_right(core(EA+BA), place) and 8#377#);
   145.    end fetch_octet;
   146.
   147.    procedure store_octet  (value : in KDF9_char_sets.octet;
   148.                            EA    : in KDF9.address;
   149.                            index : in KDF9_char_sets.octet_index) is
   150.       place : constant Natural   := 40 - 8*Natural(index);
   151.       octet : constant KDF9.word := shift_word_left(KDF9.word(value), place);
   152.       mask  : constant KDF9.word := not shift_word_left(8#377#, place);
   153.    begin
   154.       core(EA+BA) := (core(EA+BA) and mask) or octet;
   155.    end store_octet;
   156.
   157.    function fetch_syllable (EA : KDF9.syllable_address)
   158.    return KDF9.syllable is
   159.       address : constant KDF9.address := Q_part(EA.order_word_number) + BA;
   160.       place   : constant Natural      := 40 - 8*Natural(EA.syllable_index);
   161.    begin
   162.       return KDF9.syllable(shift_word_right(core(address), place) and 8#377#);
   163.    end fetch_syllable;
   164.
   165.    procedure store_syllable (value : in KDF9.syllable;
   166.                              EA    : in KDF9.address;
   167.                              index : in KDF9.syllable_index) is
   168.       place    : constant Natural   := 40 - 8*Natural(index);
   169.       syllable : constant KDF9.word := shift_word_left(KDF9.word(value), place);
   170.       mask     : constant KDF9.word := not shift_word_left(8#377#, place);
   171.    begin
   172.       core(EA+BA) := (core(EA+BA) and mask) or syllable;
   173.    end store_syllable;
   174.
   175.    function fetch_halfword (EA : KDF9.address; index : KDF9.halfword_number)
   176.    return KDF9.word
   177.    is (shift_word_left(shift_word_right(core(EA+BA), 24 - 24*Natural(index)), 24));
   178.
   179.    procedure store_halfword (value : in KDF9.word;
   180.                              EA    : in KDF9.address;
   181.                              index : in KDF9.halfword_number) is
   182.       place   : constant Natural   := 24 - 24*Natural(index);
   183.       half    : constant KDF9.word := shift_word_left(shift_word_right(value, 24), place);
   184.       mask    : constant KDF9.word := not shift_word_left(halfword_mask, place);
   185.    begin
   186.       core(EA+BA) := (core(EA+BA) and mask) or half;
   187.    end store_halfword;
   188.
   189.    function fetch_word (EA : KDF9.address)
   190.    return KDF9.word
   191.    is (core(EA+BA));
   192.
   193.    procedure store_word (value : in KDF9.word; EA : in KDF9.address) is
   194.    begin
   195.       core(EA+BA) := value;
   196.    end store_word;
   197.
   198.    function there_are_locks_in_relative_addresses (Q : KDF9.Q_register)
   199.    return Boolean is
   200.    begin
   201.       validate_address_range (Q.I, Q.M);
   202.       return there_are_locks_in_physical_addresses((0, Q.I+BA, Q.M+BA));
   203.    end there_are_locks_in_relative_addresses;
   204.
   205.    function there_are_locks_in_physical_addresses (Q : KDF9.Q_register)
   206.    return Boolean is
   207.    begin
   208.       for g in group(Q.I) .. group(Q.M) loop
   209.          if locked_out(g) then
   210.             the_locked_out_address := g * group_size;
   211.             return True;
   212.          end if;
   213.       end loop;
   214.       return False;
   215.    end there_are_locks_in_physical_addresses;
   216.
   217.    function is_unlocked (G : KDF9.store.group_address)
   218.    return Boolean is
   219.    begin
   220.       return not locked_out(KDF9.Q_part(G));
   221.    end is_unlocked;
   222.
   223.    procedure lock_out_relative_addresses (Q : in KDF9.Q_register) is
   224.    begin
   225.       validate_address_range (Q.I, Q.M);
   226.       lock_out_absolute_addresses((0, Q.I+BA, Q.M+BA));
   227.    end lock_out_relative_addresses;
   228.
   229.    procedure lock_out_absolute_addresses (Q : in KDF9.Q_register) is
   230.    begin
   231.       for g in group(Q.I) .. group(Q.M) loop
   232.          locked_out(g) := True;
   233.       end loop;
   234.    end lock_out_absolute_addresses;
   235.
   236.    procedure unlock_absolute_addresses (Q : in KDF9.Q_register) is
   237.    begin
   238.       for g in group(Q.I) .. group(Q.M) loop
   239.          locked_out(g) := False;
   240.       end loop;
   241.    end unlock_absolute_addresses;
   242.
   243. end KDF9.store;

Compiling: ../Source/kdf9-store.ads
Source file time stamp: 2020-09-09 22:33:22
Compiled at: 2020-11-12 18:12:12

     1. -- kdf9-store.ads
     2. --
     3. -- KDF9 core store operations.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with KDF9_char_sets;
    20.
    21. use  KDF9_char_sets;
    22.
    23. package KDF9.store is
    24.
    25.    --
    26.    -- Relative addresses may be either virtual or physical.
    27.    -- Virtual addresses are generated by problem programs and must be relativized by adding BA.
    28.    -- These addresses must also be validated to ensure they do not breach store limits.
    29.    -- Physical "relative" addresses are generated by Director, when BA is guaranteed to be 0,
    30.    --   so it has no effect, thus allowing the relative address routines to be used.
    31.    --
    32.    -- Absolute addresses are generated by I/O Control, which may be doing a transfer for either
    33.    --   a Director or a problem program, and must therefore ensure that BA is not added.
    34.    --
    35.
    36.    --
    37.    -- Parameters named EA are Effective "relative" Addresses.
    38.    --
    39.
    40.    function fetch_symbol (EA : KDF9.address; index : KDF9_char_sets.symbol_index)
    41.    return KDF9_char_sets.symbol;
    42.
    43.    function fetch_octet (EA : KDF9.address; index : KDF9_char_sets.octet_index)
    44.    return KDF9_char_sets.octet
    45.       with Inline;
    46.
    47.    function fetch_syllable (EA : KDF9.syllable_address)
    48.    return KDF9.syllable
    49.       with Inline;
    50.
    51.    function fetch_halfword (EA : KDF9.address; index : KDF9.halfword_number)
    52.    return KDF9.word
    53.       with Inline;
    54.
    55.    function fetch_word (EA : KDF9.address)
    56.    return KDF9.word
    57.       with Inline;
    58.
    59.    procedure store_symbol (value : in KDF9_char_sets.symbol;
    60.                            EA    : in KDF9.address;
    61.                            index : in KDF9_char_sets.symbol_index)
    62.       with Inline;
    63.
    64.    procedure store_octet  (value : in KDF9_char_sets.octet;
    65.                            EA    : in KDF9.address;
    66.                            index : in KDF9_char_sets.octet_index)
    67.       with Inline;
    68.
    69.    procedure store_syllable (value : in KDF9.syllable;
    70.                              EA    : in KDF9.address;
    71.                              index : in KDF9.syllable_index)
    72.       with Inline;
    73.
    74.    procedure store_halfword (value : in KDF9.word;
    75.                              EA    : in KDF9.address;
    76.                              index : in KDF9.halfword_number)
    77.       with Inline;
    78.
    79.    procedure store_word (value : in KDF9.word; EA : in KDF9.address)
    80.       with Inline;
    81.
    82.    -- Check that A1+A2 is a valid word address; LIV if it is invalid.
    83.    function valid_word_address (A1, A2 : in KDF9.Q_part)
    84.    return KDF9.address
    85.       with Inline;
    86.
    87.    -- Check that A1+A2/2 is valid; LIV if it is invalid.  A2 is treated as a signed number.
    88.    function valid_halfword_address (A1, A2 : in KDF9.Q_part)
    89.    return KDF9.address
    90.       with Inline;
    91.
    92.    -- If a store access is locked out, its physical address is left here.
    93.    the_locked_out_address : KDF9.Q_part;
    94.
    95.    procedure if_user_mode_then_LOV (address_1 : KDF9.Q_part;
    96.                                     address_2 : KDF9.Q_part := 0;
    97.                                     solo      : Boolean     := True)
    98.       with Inline => False;
    99.
   100.    -- Check EA and lockout for EA.
   101.    procedure check_address_and_lockout (EA : in KDF9.Q_part)
   102.       with Inline;
   103.
   104.    -- Check that EA1, EA2, EA1+BA, EA2+BA are valid, and EA1 <= EA2.
   105.    --    LIV in any invalid case.
   106.    procedure validate_address_range (EA1, EA2 : in KDF9.Q_part);
   107.
   108.    -- Check EA1, EA2, and lockouts for EA1+BA .. EA2+BA.
   109.    procedure check_addresses_and_lockouts (EA1, EA2 : in KDF9.Q_part);
   110.
   111.    function there_are_locks_in_relative_addresses (Q : KDF9.Q_register)
   112.    return Boolean;
   113.
   114.    function there_are_locks_in_physical_addresses (Q : KDF9.Q_register)
   115.    return Boolean;
   116.
   117.    procedure lock_out_relative_addresses (Q : in KDF9.Q_register);
   118.
   119.    procedure lock_out_absolute_addresses (Q : in KDF9.Q_register);
   120.
   121.    procedure unlock_absolute_addresses (Q : in KDF9.Q_register);
   122.
   123.    -- The maximum size KDF9 core store has 32Kibiwords.
   124.    max_address   : constant := 2**15 - 1;
   125.
   126.    -- The group size of 32 words is 1 physical core allocation unit and physical lockout unit.
   127.    group_size : constant := 32;
   128.
   129.    type group_address is mod 1024;
   130.
   131.     -- is_unlocked yields True if the designated group is NOT locked out.
   132.    function is_unlocked (G : KDF9.store.group_address)
   133.    return Boolean;
   134.
   135.    function group (PA : KDF9.Q_part)
   136.    return KDF9.Q_part
   137.       with Inline;
   138.
   139. private
   140.
   141.    type word_array is array (KDF9.Q_part range <>) of KDF9.word
   142.       with Component_Size => 64, Convention => C;
   143.
   144.    -- The core store of KDF9.  Must be zeroized before loading any software.
   145.    core : word_array (KDF9.Q_part range 0 .. max_address) := (others => 0);
   146.
   147.    -- The lockout store has one bit for every group_size words.
   148.    last_lockout : constant := max_address / group_size;
   149.    locked_out   : array (KDF9.Q_part range 0 .. last_lockout) of Boolean := (others => False);
   150.
   151. end KDF9.store;

 243 lines: No errors
   400.          the_stream.next_byte := 0;
   401.       end if;
   402.       the_stream.next_byte := the_stream.next_byte + 1;
   403.       the_stream.position := the_stream.position + 1;
   404.       the_stream.bytes_moved := the_stream.bytes_moved + 1;
   405.       the_stream.buffer(the_stream.next_byte) := char;
   406.       the_stream.last_IO := write_mode;
   407.       if char = LF then
   408.          the_stream.column := 0;
   409.       else
   410.          the_stream.column := the_stream.column + 1;
   411.       end if;
   412.    exception
   413.       when POSIX_IO_error =>
   414.          diagnose(the_stream, "PUT_BYTE: POSIX_IO_error");
   415.       when error : others =>
   416.          diagnose(the_stream, Ada.Exceptions.Exception_Message(error));
   417.    end put_byte;
   418.
   419.    procedure do_not_put_byte (char       : in Character;
   420.                               the_stream : in out host_IO.stream) is
   421.    begin
   422.       if not the_stream.is_open then
   423.          raise end_of_stream;
   424.       end if;
   425.       the_stream.bytes_moved := the_stream.bytes_moved + 1;
   426.       the_stream.last_IO := write_mode;
   427.       if char = LF then
   428.          the_stream.column := 0;
   429.       else
   430.          the_stream.column := the_stream.column + 1;
   431.       end if;
   432.    exception
   433.       when error : others =>
   434.          diagnose(the_stream, Ada.Exceptions.Exception_Message(error));
   435.    end do_not_put_byte;
   436.
   437.    procedure put_bytes (the_string : in String;
   438.                         the_stream : in out host_IO.stream;
   439.                         uncounted  : in Boolean := True) is
   440.       old_bytes_moved : constant KDF9.word := the_stream.bytes_moved;
   441.    begin
   442.       for c of the_string loop
   443.          put_byte(c, the_stream);
   444.       end loop;
   445.       if uncounted then
   446.          the_stream.bytes_moved := old_bytes_moved;
   447.       end if;
   448.    end put_bytes;
   449.
   450.    procedure put_EOL (the_stream : in out host_IO.stream;
   451.                       uncounted  : in Boolean := True) is
   452.    begin
   453.       put_bytes(NL, the_stream, uncounted);
   454.    end put_EOL;
   455.
   456.    procedure put_char (char       : in Character;
   457.                        the_stream : in out host_IO.stream) is
   458.    begin
   459.       if char = LF then
   460.          put_EOL(the_stream, uncounted => False);
   461.       else
   462.          put_byte(char, the_stream);
   463.       end if;
   464.    end put_char;
   465.
   466.    procedure put_chars (the_string : in String;
   467.                         the_stream : in out host_IO.stream) is
   468.    begin
   469.       for c of the_string loop
   470.          put_char(c, the_stream);
   471.       end loop;
   472.    end put_chars;
   473.
   474.    function contents (the_stream : host_IO.stream)
   475.    return String is
   476.    begin
   477.       return the_stream.buffer(1..the_stream.next_byte);
   478.    end contents;
   479.
   480.    procedure inject (the_string : in String;
   481.                      the_stream : in out host_IO.stream) is
   482.       the_length : constant Natural := the_string'Length;
   483.    begin
   484.       if not the_stream.is_open then
   485.          diagnose(the_stream,
   486.                   NL
   487.                 & "injecting:"
   488.                 & NL
   489.                 & the_string
   490.                 & NL
   491.                 & "into the closed "
   492.                 & the_stream.base_name
   493.                  );
   494.       end if;
   495.       if the_length + 1 > IO_buffer_size then
   496.          diagnose(the_stream,
   497.                   NL
   498.                 & "injecting a string of excessive length ="
   499.                 & the_length'Image
   500.                 & " into the stream "
   501.                 & the_stream.base_name
   502.                  );
   503.       elsif the_length > 0 then
   504.          the_stream.block_size := the_length + 1;
   505.          the_stream.buffer(1 .. the_length) := the_string;
   506.          the_stream.buffer(the_length + 1)  := LF;
   507.       else
   508.          diagnose(the_stream,
   509.                   NL
   510.                 & "injecting a string of length = 0 into the stream "
   511.                 & the_stream.base_name
   512.                  );
   513.       end if;
   514.    end inject;
   515.
   516. end host_IO;

Compiling: ../Source/host_io.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:12

     1. -- host_IO.ads
     2. --
     3. -- Buffered I/O streams to support KDF9 device I/O.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with KDF9;
    20. with POSIX;
    21.
    22. use  KDF9;
    23. use  POSIX;
    24.
    25. package host_IO is
    26.
    27.    stream_IO_error, end_of_stream : exception;
    28.
    29.    type stream is tagged limited private;
    30.
    31.    function image_of (the_stream : host_IO.stream;
    32.                       caption    : String := "")
    33.    return String
    34.       with Inline => False;
    35.
    36.    function fd_of (the_stream : host_IO.stream)
    37.    return Natural;
    38.
    39.    -- Open a stream with an established fd.
    40.    procedure open (the_stream : in out host_IO.stream;
    41.                    file_name  : in String;
    42.                    mode       : in POSIX.access_mode;
    43.                    fd         : in Integer);
    44.
    45.    -- Open a base file then use its fd to open a stream.
    46.    procedure open (the_stream : in out host_IO.stream;
    47.                    file_name  : in String;
    48.                    mode       : in POSIX.access_mode);
    49.
    50.    procedure truncate (the_stream : in out host_IO.stream;
    51.                        to_length  : in KDF9.word := 0);
    52.
    53.    procedure close (the_stream : in out host_IO.stream);
    54.
    55.    procedure flush (the_stream  : in out host_IO.stream;
    56.                     a_byte_time : in KDF9.us := 0);
    57.
    58.    -- Reassign an open stream to another file.
    59.    procedure reattach (the_stream : in out host_IO.stream;
    60.                        file_name  : in String;
    61.                        mode       : in POSIX.access_mode);
    62.
    63.    function is_open(the_stream : host_IO.stream)
    64.    return Boolean;
    65.
    66.    function bytes_moved(the_stream : host_IO.stream)
    67.    return KDF9.word;
    68.
    69.    function file_size (the_stream : host_IO.stream)
    70.    return Natural;
    71.
    72.    function column (the_stream : host_IO.stream)
    73.    return Natural;
    74.
    75.    procedure get_position (position   : out Natural;
    76.                            the_stream : in out host_IO.stream);
    77.
    78.    procedure set_position (position   : in Natural;
    79.                            the_stream : in out host_IO.stream;
    80.                            whence     : in POSIX.seek_origin := from_start);
    81.
    82.    procedure clear (the_stream : in out host_IO.stream);
    83.
    84.    procedure reset (the_stream : in out host_IO.stream);
    85.
    86.    procedure back_off (the_stream : in out host_IO.stream)
    87.       with Inline;
    88.
    89.    procedure get_byte (char       : out Character;
    90.                        the_stream : in out host_IO.stream);
    91.
    92.    -- get_bytes iterates get_byte over the_string, for convenience.
    93.    -- If uncounted then the output is not included in the_stream.bytes_moved.
    94.    procedure get_bytes (the_string : out String;
    95.                         the_stream : in out host_IO.stream;
    96.                         uncounted  : in Boolean := True);
    97.
    98.    -- True iff the last get_byte obtained a LF.
    99.    function a_LF_was_just_read (the_stream : host_IO.stream)
   100.    return Boolean;
   101.
   102.    -- get_char differs from get_byte in the treatment of line terminators.
   103.    -- CR, LF, and CRLF are all returned as a single LF character, so catering
   104.    --    for old MacOS, MSDOS, and macOS/UNIX/Linux external text-file formats.
   105.    procedure get_char (char       : out Character;
   106.                        the_stream : in out host_IO.stream);
   107.
   108.    -- peek_at_char uses get_char to inspect the next char to be delivered,
   109.    --    then invokes back_off so that it is left in the input stream.
   110.    procedure peek_at_char (char       : out Character;
   111.                            the_stream : in out host_IO.stream);
   112.
   113.    -- do_not_put_byte does the same as put_byte, except for actually writing it to the_stream.
   114.    procedure do_not_put_byte (char       : in Character;
   115.                               the_stream : in out host_IO.stream);
   116.
   117.    procedure put_byte (char       : in Character;
   118.                        the_stream : in out host_IO.stream);
   119.
   120.    -- put_escape_code writes directly to the stream's device, avoiding the stream's buffers.
   121.    procedure put_escape_code (the_string : in String;
   122.                               the_stream : in out host_IO.stream);
   123.
   124.    -- put_bytes iterates put_byte over the_string, for convenience.
   125.    -- If uncounted then the output is not included in the_stream.bytes_moved.
   126.    procedure put_bytes (the_string : in String;
   127.                         the_stream : in out host_IO.stream;
   128.                         uncounted  : in Boolean := True);
   129.
   130.    -- put_EOL writes the host-appropriate line terminator (CRLF, or just LF)
   131.    procedure put_EOL (the_stream : in out host_IO.stream;
   132.                       uncounted  : in Boolean := True);
   133.
   134.    -- put_char differs from put_byte only in the treatment of line terminators.
   135.    -- If char is LF, then put_EOL is used to output a host-appropriate line terminator.
   136.    procedure put_char (char       : in Character;
   137.                        the_stream : in out host_IO.stream);
   138.
   139.    -- put_chars iterates put_bytes over the_string, for convenience.
   140.    procedure put_chars (the_string : in String;
   141.                         the_stream : in out host_IO.stream);
   142.
   143.    -- True iff the last put_byte wrote out a LF.
   144.    function a_LF_was_just_written (the_stream : host_IO.stream)
   145.    return Boolean;
   146.
   147.    function buffer_is_empty (the_stream : host_IO.stream)
   148.    return Boolean
   149.       with Inline;
   150.
   151.    function buffer_is_full (the_stream : host_IO.stream)
   152.    return Boolean
   153.       with Inline;
   154.
   155.    -- Return the currently buffered output as a single string.
   156.    function contents (the_stream : host_IO.stream)
   157.    return String;
   158.
   159.    -- Make the_string appear to be input for the_stream (which must be empty).
   160.    procedure inject (the_string : in String;
   161.                      the_stream : in out host_IO.stream);
   162.
   163. private
   164.
   165.    -- N.B. in host_IO the term 'buffer' is used conventionally.
   166.    -- It does NOT refer to a KDF9 DMA channel.
   167.
   168.    -- IO_buffer_size is enough for a complete FD sector, lacking any better criterion.
   169.    IO_buffer_size : constant Positive := 320;
   170.
   171.    type stream is tagged limited
   172.       record
   173.          base_name   : String (1 .. 3) := "???";
   174.          is_open     : Boolean := False;
   175.          last_char   : Character := ' ';
   176.          block_size,
   177.          next_byte,
   178.          saved_size,
   179.          position,
   180.          column      : Natural := 0;
   181.          bytes_moved : KDF9.word := 0;
   182.          fd          : Natural := Natural'Last;
   183.          IO_mode     : POSIX.access_mode range read_mode .. rd_wr_mode;
   184.          last_IO     : POSIX.access_mode range read_mode .. write_mode;
   185.          buffer,
   186.          look_behind : String(1 .. IO_buffer_size);
   187.       end record;
   188.
   189. end host_IO;

 516 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/tracing.adb
Source file time stamp: 2020-11-02 19:41:31
Compiled at: 2020-11-12 18:12:12

     1. -- tracing.adb
     2. --
     3. -- Provide diagnostic trace, breakpoint, and watchpoint support.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with exceptions;
    20. with formatting;
    21. with HCI;
    22. with KDF9.decoding;
    23. with KDF9.store;
    24. with state_display;
    25.
    26. use exceptions;
    27. use formatting;
    28. use HCI;
    29. use KDF9;
    30. use KDF9.decoding;
    31. use KDF9.store;
    32. use state_display;
    33.
    34. package body tracing is
    35.
    36.    procedure clear_the_histogram is
    37.    begin
    38.       the_histogram := (others => 0);
    39.    end clear_the_histogram;
    40.
    41.    procedure clear_the_profile is
    42.    begin
    43.       the_profile := (others => 0);
    44.    end clear_the_profile;
    45.
    46.    procedure set_breakpoints (first, last : in KDF9.order_word_number) is
    47.    begin
    48.       for p in first .. last loop
    49.          breakpoints(p) := True;
    50.       end loop;
    51.    end set_breakpoints;
    52.
    53.    procedure handle_breakpoint is
    54.    begin
    55.       short_witness;
    56.       interact;
    57.       quit_if_requested;
    58.       change_diagnostic_mode_if_requested;
    59.    end handle_breakpoint;
    60.
    61.    procedure clear_all_watchpoints is
    62.    begin
    63.       fetchpoints := (others => False);
    64.       storepoints := (others => False);
    65.    end clear_all_watchpoints;
    66.
    67.    procedure set_fetch_points (first, last : in KDF9.address) is
    68.    begin
    69.       for p in first .. last loop
    70.          fetchpoints(p) := True;
    71.       end loop;
    72.    end set_fetch_points;
    73.
    74.    procedure set_store_points (first, last : in KDF9.address) is
    75.    begin
    76.       for p in first .. last loop
    77.          storepoints(p) := True;
    78.       end loop;
    79.    end set_store_points;
    80.
    81.    procedure clear_retro_FIFO is
    82.    begin
    83.       retro_FIFO_count := 0; retro_FIFO_index := 0;
    84.    end clear_retro_FIFO;
    85.
    86.    procedure take_note_of (the_IAR   : in KDF9.syllable_address;
    87.                            the_value : in KDF9.word) is
    88.    begin
    89.       if the_retrospective_trace_is_enabled           and then
    90.             ICR in low_count .. high_count            and then
    91.                NIA_word_number in low_bound .. high_bound then
    92.          declare
    93.             the_note : constant retro_FIFO_entry
    94.                      := (
    95.                          location   => the_IAR,
    96.                          order      => INS.order,
    97.                          parameter  => the_value,
    98.                          ICR_value  => ICR,
    99.                          CPU_time   => the_CPU_time,
   100.                          nested     => the_nest_depth,
   101.                          called     => the_sjns_depth,
   102.                          V          => the_V_bit_is_set,
   103.                          T          => the_T_bit_is_set,
   104.                          D          => the_CPU_state = Director_state,
   105.                          level      => CPL
   106.                         );
   107.          begin
   108.             if retro_FIFO_count = 0 then
   109.                retro_FIFO(0) := the_note;
   110.                retro_FIFO_count := 1;
   111.             else
   112.                retro_FIFO_index := retro_FIFO_index + 1;
   113.                retro_FIFO(retro_FIFO_index) := the_note;
   114.                if retro_FIFO_count < FIFO_size then
   115.                   retro_FIFO_count := retro_FIFO_count + 1;
   116.                end if;
   117.             end if;
   118.          end;
   119.       end if;
   120.    end take_note_of;
   121.
   122.    procedure take_note_of (the_value : in KDF9.word) is
   123.    begin
   124.       take_note_of(CIA, the_value);
   125.    end take_note_of;
   126.
   127.    procedure clear_IOC_FIFO is
   128.    begin
   129.       IOC_FIFO_count := 0; IOC_FIFO_index := 0;
   130.    end clear_IOC_FIFO;
   131.
   132.    procedure register_IO_event (the_note : in IOC_FIFO_entry) is
   133.    begin
   134.       if the_peripheral_trace_is_enabled              and then
   135.             ICR in low_count .. high_count            and then
   136.                NIA_word_number in low_bound .. high_bound then
   137.          if IOC_FIFO_count = 0 then
   138.             IOC_FIFO(0) := the_note;
   139.             IOC_FIFO_count := 1;
   140.          else
   141.             IOC_FIFO_index := IOC_FIFO_index + 1;
   142.             IOC_FIFO(IOC_FIFO_index) := the_note;
   143.             if IOC_FIFO_count < FIFO_size then
   144.                IOC_FIFO_count := IOC_FIFO_count + 1;
   145.             end if;
   146.          end if;
   147.       end if;
   148.    end register_IO_event;
   149.
   150.    procedure take_note_of_IO_start (
   151.                                     device_name     : in IOC.device_name;
   152.                                     completion_time : in KDF9.us;
   153.                                     control_word    : in KDF9.Q_register
   154.                                    )
   155.    is
   156.       the_note : constant  IOC_FIFO_entry
   157.                :=
   158.                 (
   159.                  kind            => start_transfer,
   160.                  ICR_value       => ICR,
   161.                  order_address   => CIA,
   162.                  decoded_order   => INS,
   163.                  initiation_time => the_clock_time,
   164.                  device_name     => take_note_of_IO_start.device_name,
   165.                  completion_time => take_note_of_IO_start.completion_time,
   166.                  is_for_Director => (the_CPU_state = Director_state),
   167.                  priority_level  => CPL,
   168.                  context         => the_context,
   169.                  control_word    => take_note_of_IO_start.control_word
   170.                 );
   171.    begin
   172.       register_IO_event(the_note);
   173.    end take_note_of_IO_start;
   174.
   175.    procedure take_note_of_IO_finis (
   176.                                     ICR_value       : in KDF9.order_counter;
   177.                                     order_address   : in KDF9.syllable_address;
   178.                                     decoded_order   : in KDF9.decoded_order;
   179.                                     initiation_time : in KDF9.us;
   180.                                     device_name     : in IOC.device_name;
   181.                                     is_for_Director : Boolean;
   182.                                     priority_level  : in KDF9.priority;
   183.                                     completion_time : in KDF9.us;
   184.                                     control_word    : in KDF9.Q_register
   185.                                    )
   186.    is
   187.       the_note : constant  IOC_FIFO_entry
   188.                :=
   189.                 (
   190.                  kind            => finis_transfer,
   191.                  ICR_value       => take_note_of_IO_finis.ICR_value,
   192.                  order_address   => take_note_of_IO_finis.order_address,
   193.                  decoded_order   => take_note_of_IO_finis.decoded_order,
   194.                  initiation_time => take_note_of_IO_finis.initiation_time,
   195.                  device_name     => take_note_of_IO_finis.device_name,
   196.                  is_for_Director => take_note_of_IO_finis.is_for_Director,
   197.                  priority_level  => take_note_of_IO_finis.priority_level,
   198.                  context         => the_context,
   199.                  completion_time => take_note_of_IO_finis.completion_time,
   200.                  control_word    => take_note_of_IO_finis.control_word
   201.                 );
   202.
   203.    begin
   204.       register_IO_event(the_note);
   205.    end take_note_of_IO_finis;
   206.
   207.    procedure take_note_of_store_lockout (device_name : in IOC.device_name) is
   208.       the_note : constant  IOC_FIFO_entry
   209.                :=
   210.                 (
   211.                  kind            => store_lockout,
   212.                  ICR_value       => ICR,
   213.                  order_address   => CIA,
   214.                  decoded_order   => INS,
   215.                  initiation_time => the_clock_time,
   216.                  device_name     => take_note_of_store_lockout.device_name,
   217.                  is_for_Director => False,
   218.                  priority_level  => CPL,
   219.                  context         => the_context,
   220.                  data_address    => the_locked_out_address
   221.                 );
   222.    begin
   223.       register_IO_event(the_note);
   224.    end take_note_of_store_lockout;
   225.
   226.    procedure take_note_of_buffer_lockout (device_name : in IOC.device_name) is
   227.       the_note : constant  IOC_FIFO_entry
   228.                :=
   229.                 (
   230.                  kind            => buffer_lockout,
   231.                  ICR_value       => ICR,
   232.                  order_address   => CIA,
   233.                  decoded_order   => INS,
   234.                  initiation_time => the_clock_time,
   235.                  device_name     => take_note_of_buffer_lockout.device_name,
   236.                  is_for_Director => False,
   237.                  priority_level  => CPL,
   238.                  context         => the_context
   239.                 );
   240.    begin
   241.       register_IO_event(the_note);
   242.    end take_note_of_buffer_lockout;
   243.
   244.    procedure take_note_of_test (
   245.                                 device_name : in IOC.device_name;
   246.                                 Q_register  : in KDF9.Q_register;
   247.                                 status      : in Boolean
   248.                                )
   249.    is
   250.       the_note : constant  IOC_FIFO_entry
   251.                :=
   252.                 (
   253.                  kind            => buffer_status,
   254.                  ICR_value       => ICR+1,  -- ICR is not incremented until the end of an order.
   255.                  order_address   => CIA,
   256.                  decoded_order   => INS,
   257.                  initiation_time => the_clock_time,
   258.                  device_name     => take_note_of_test.device_name,
   259.                  is_for_Director => (the_CPU_state = Director_state),
   260.                  priority_level  => CPL,
   261.                  context         => the_context,
   262.                  Q_register      => take_note_of_test.Q_register,
   263.                  status          => take_note_of_test.status
   264.                 );
   265.    begin
   266.       register_IO_event(the_note);
   267.    end take_note_of_test;
   268.
   269.    procedure clear_interrupt_FIFO is
   270.    begin
   271.       interrupt_FIFO_count := 0; interrupt_FIFO_index := 0;
   272.    end clear_interrupt_FIFO;
   273.
   274.    procedure take_note_of_interrupt (interrupt_code : in KDF9.interrupt_number; message : in String)
   275.    is
   276.       length : constant Natural := message'Length;
   277.       memo   : String(1..max_interrupt_message_length) := (others => ' ');
   278.    begin
   279.       if length > max_interrupt_message_length then
   280.          raise Program_Error
   281.             with "interrupt note message is too long '" & message & "'" & length'Image;
   282.       end if;
   283.       memo(1..length) := message;
   284.       declare
   285.          the_note : constant interrupt_FIFO_entry
   286.                   :=
   287.                    (
   288.                     interrupt_code => take_note_of_interrupt.interrupt_code,
   289.                     ICR_value      => ICR,
   290.                     order_address  => CIA,
   291.                     busy_time      => the_clock_time,
   292.                     priority_level => CPL,
   293.                     context        => the_context,
   294.                     message        => memo
   295.                    );
   296.       begin
   297.          if the_interrupt_trace_is_enabled               and then
   298.                ICR in low_count .. high_count            and then
   299.                   NIA_word_number in low_bound .. high_bound then
   300.             if interrupt_FIFO_count = 0 then
   301.                interrupt_FIFO(0) := the_note;
   302.                interrupt_FIFO_count := 1;
   303.             else
   304.                interrupt_FIFO_index := interrupt_FIFO_index + 1;
   305.                interrupt_FIFO(interrupt_FIFO_index) := the_note;
   306.                if interrupt_FIFO_count < FIFO_size then
   307.                   interrupt_FIFO_count := interrupt_FIFO_count + 1;
   308.                end if;
   309.             end if;
   310.          end if;
   311.       end;
   312.    end take_note_of_interrupt;
   313.
   314.    procedure add_INS_to_the_histogram is
   315.       syllable_0 : KDF9.syllable := INS.order.syllable_0;
   316.    begin
   317.       if INS.kind = normal_jump_order then
   318.          syllable_0 := (syllable_0 and 2#1111_0000#) or INS.Qq;
   319.       elsif INS.kind = data_access_order then
   320.          syllable_0 := (syllable_0 and 2#11_000_111#);
   321.       end if;
   322.       the_histogram(syllable_0) := the_histogram(syllable_0) + 1;
   323.    end add_INS_to_the_histogram;
   324.
   325.    procedure add_CIA_to_the_profile is
   326.    begin
   327.       the_profile(CIA.order_word_number) := the_profile(CIA.order_word_number) + 1;
   328.    end add_CIA_to_the_profile;
   329.
   330.    procedure preview_a_one_syllable_order is null;
   331.
   332.    procedure preview_a_two_syllable_order is
   333.    begin
   334.       case INS.compressed_opcode is
   335.          when TO_MkMq
   336.             | TO_MkMqQ
   337.             | TO_MkMqH
   338.             | TO_MkMqQH
   339.             | TO_MkMqN
   340.             | TO_MkMqQN
   341.             | TO_MkMqHN
   342.             | TO_MkMqQHN =>
   343.             the_trace_operand := read_top;
   344.          when others =>
   345.             the_trace_operand := as_word(the_Q_store(INS.Qq));
   346.       end case;
   347.    end preview_a_two_syllable_order;
   348.
   349.    procedure preview_a_jump_order is
   350.    begin
   351.       case INS.compressed_opcode is
   352.          when JrEQ
   353.             | JrNE
   354.             | JrGTZ
   355.             | JrLTZ
   356.             | JrEQZ
   357.             | JrLEZ
   358.             | JrGEZ
   359.             | JrNEZ
   360.             | OS_OUT =>
   361.             if the_nest_depth > 0 then
   362.                the_trace_operand := read_top;
   363.             end if;
   364.          when JrEN
   365.             | JrNEN =>
   366.             the_trace_operand := KDF9.word(the_nest_depth);
   367.          when JrEJ
   368.             | JrNEJ =>
   369.             the_trace_operand := KDF9.word(the_sjns_depth);
   370.          when EXIT_n
   371.             | EXITD =>
   372.             if the_sjns_depth > 0 then
   373.                the_trace_operand := as_word(sjns_top);
   374.             else
   375.                the_trace_operand := -1;
   376.             end if;
   377.          when JrCqZ
   378.             | JrCqNZ =>
   379.             the_trace_operand := as_word(the_Q_store(INS.Qq));
   380.          when JrV
   381.             | JrNV =>
   382.             the_trace_operand := (if the_V_bit_is_set then 1 else 0);
   383.          when JrTR
   384.             | JrNTR =>
   385.             the_trace_operand := (if the_T_bit_is_set then 1 else 0);
   386.          when others =>
   387.             null;
   388.       end case;
   389.    end preview_a_jump_order;
   390.
   391.    procedure preview_a_data_access_order is
   392.    begin
   393.       case INS.compressed_opcode is
   394.          when TO_EaMq
   395.             | TO_EaMqQ =>
   396.             the_trace_operand := read_top;
   397.          when others =>
   398.             null;
   399.       end case;
   400.    end preview_a_data_access_order;
   401.
   402.    procedure look_back_at_a_one_syllable_order is
   403.       AB : KDF9.pair;
   404.    begin
   405.       case INS.compressed_opcode is
   406.          when XDF
   407.             | XPLUSF
   408.             | MINUSDF
   409.             | PLUSDF
   410.             | FLOATD
   411.             | NEGDF
   412.             | MAXF
   413.             | PERM
   414.             | CAB
   415.             | MAX
   416.             | XD
   417.             | NEGD
   418.             | DUPD
   419.             | DIVI
   420.             | STR
   421.             | REVD
   422.             | MINUSD
   423.             | PLUSD
   424.             | DIVR =>
   425.             AB := read_top;
   426.             the_trace_operand := AB.msw;
   427.          when others =>
   428.             if the_nest_depth > 0 then
   429.                the_trace_operand := read_top;
   430.             end if;
   431.       end case;
   432.    end look_back_at_a_one_syllable_order;
   433.
   434.    procedure look_back_at_an_IO_order is null;
   435.
   436.    procedure look_back_at_a_two_syllable_order is
   437.       AB : KDF9.pair;
   438.    begin
   439.       case INS.compressed_opcode is
   440.          when MkMq
   441.             | MkMqQ
   442.             | MkMqH
   443.             | MkMqQH
   444.             | MkMqQN
   445.             | MkMqHN
   446.             | MkMqQHN
   447.             | QCIMq
   448.             | SHA
   449.             | SHL
   450.             | SHC
   451.             | TO_Kq
   452.             | Kk
   453.             | LINK =>
   454.             the_trace_operand := read_top;
   455.          when TO_MkMq
   456.             | TO_MkMqQ
   457.             | TO_MkMqH
   458.             | TO_MkMqQH
   459.             | TO_MkMqN
   460.             | TO_MkMqQN
   461.             | TO_MkMqHN
   462.             | TO_MkMqQHN =>
   463.             null;
   464.          when M_PLUS_Iq
   465.             | M_MINUS_Iq
   466.             | NCq
   467.             | DCq
   468.             | POS1_TO_Iq
   469.             | NEG1_TO_Iq
   470.             | POS2_TO_Iq
   471.             | NEG2_TO_Iq
   472.             | TO_RCIMq
   473.             | ADD_TO_QCIMq
   474.             | JCqNZS =>
   475.             the_trace_operand := as_word(the_Q_store(INS.Qq));
   476.          when CqTOQk
   477.             | IqTOQk
   478.             | MqTOQk
   479.             | QqTOQk
   480.             | CIqTOQk
   481.             | IMqTOQk
   482.             | CMqTOQk =>
   483.             the_trace_operand := as_word(the_Q_store(INS.Qk));
   484.          when SHLD
   485.             | SHAD
   486.             | MACC =>
   487.             AB := read_top;
   488.             the_trace_operand := AB.msw;
   489.          when TO_LINK =>
   490.             the_trace_operand := as_word(sjns_top);
   491.          when others =>
   492.             look_back_at_an_IO_order;
   493.       end case;
   494.    end look_back_at_a_two_syllable_order;
   495.
   496.    procedure look_back_at_a_jump_order is
   497.       BA_image  : constant String := "BA #" & oct_of(BA);
   498.       NOL_image : constant String := "NOL"  & NOL'Image;
   499.    begin
   500.       case INS.compressed_opcode is
   501.          when Jr =>
   502.             the_trace_operand := as_word(sjns_link(NIA));
   503.          when JSr =>
   504.             the_trace_operand := as_word(sjns_top);
   505.          when EXITD =>
   506.             take_note_of_interrupt(EXITD_flag, BA_image & " " & NOL_image & " @ " & oct_of(NIA));
   507.          when others =>
   508.             null;
   509.       end case;
   510.    end look_back_at_a_jump_order;
   511.
   512.    procedure look_back_at_a_data_access_order is
   513.    begin
   514.       case INS.compressed_opcode is
   515.          when EaMq
   516.             | EaMqQ
   517.             | SET =>
   518.             the_trace_operand := read_top;
   519.          when others =>
   520.             null;
   521.       end case;
   522.    end look_back_at_a_data_access_order;
   523.
   524.    procedure act_on_any_fetchpoint is
   525.       use type watch_flags.set;
   526.    begin
   527.       if the_trace_address/fetchpoints then
   528.          log_new_line;
   529.          log("Fetch watchhpoint: N1 := [#" & oct_of(the_trace_address) & "]");
   530.          short_witness;
   531.          interact;
   532.          quit_if_requested;
   533.          change_diagnostic_mode_if_requested;
   534.       end if;
   535.    end act_on_any_fetchpoint;
   536.
   537.    procedure act_on_any_storepoint is
   538.       use type watch_flags.set;
   539.    begin
   540.       if the_trace_address/storepoints then
   541.          log_new_line;
   542.          log(
   543.              "Store watchpoint: #"
   544.            & oct_of(the_trace_address)
   545.            & " := [N1] = #"
   546.            & oct_of(the_trace_operand)
   547.             );
   548.          short_witness;
   549.          interact;
   550.          quit_if_requested;
   551.          change_diagnostic_mode_if_requested;
   552.       end if;
   553.    end act_on_any_storepoint;
   554.
   555.    procedure act_on_any_two_syllable_order_watchpoints is
   556.    begin
   557.       case INS.compressed_opcode is
   558.          when MkMq
   559.             | MkMqQ
   560.             | MkMqH
   561.             | MkMqQH
   562.             | MkMqQN
   563.             | MkMqHN
   564.             | MkMqQHN =>
   565.             act_on_any_fetchpoint;
   566.          when TO_MkMq
   567.             | TO_MkMqQ
   568.             | TO_MkMqH
   569.             | TO_MkMqQH
   570.             | TO_MkMqN
   571.             | TO_MkMqQN
   572.             | TO_MkMqHN
   573.             | TO_MkMqQHN =>
   574.             act_on_any_storepoint;
   575.          when others =>
   576.             null;
   577.       end case;
   578.    end act_on_any_two_syllable_order_watchpoints;
   579.
   580.    procedure act_on_any_data_access_order_watchpoints is
   581.    begin
   582.       case INS.compressed_opcode is
   583.          when EaMq
   584.             | EaMqQ =>
   585.             act_on_any_fetchpoint;
   586.          when TO_EaMq
   587.             | TO_EaMqQ =>
   588.             act_on_any_storepoint;
   589.          when others =>
   590.             null;
   591.       end case;
   592.    end act_on_any_data_access_order_watchpoints;
   593.
   594. end tracing;

Compiling: ../Source/tracing.ads
Source file time stamp: 2020-10-20 01:17:42
Compiled at: 2020-11-12 18:12:12

     1. -- tracing.ads
     2. --
     3. -- Provide diagnostic trace, breakpoint, and watchpoint support.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with generic_sets;
    20. with IOC;
    21. with KDF9;
    22. with settings;
    23.
    24. use  settings;
    25.
    26. package tracing is
    27.
    28.    -- Support for significant-operand evaluation and tracing.
    29.
    30.    the_trace_operand : KDF9.word;
    31.    the_trace_address : KDF9.address;
    32.
    33.    procedure preview_a_one_syllable_order;
    34.
    35.    procedure preview_a_two_syllable_order;
    36.
    37.    procedure preview_a_jump_order;
    38.
    39.    procedure preview_a_data_access_order;
    40.
    41.    procedure look_back_at_a_one_syllable_order;
    42.
    43.    procedure look_back_at_a_two_syllable_order;
    44.
    45.    procedure look_back_at_a_jump_order;
    46.
    47.    procedure look_back_at_a_data_access_order;
    48.
    49.    -- Support for breakpoints.
    50.
    51.    package order_flags is new generic_sets(member => KDF9.order_word_number);
    52.
    53.    breakpoints : order_flags.set := order_flags.empty_set;
    54.
    55.    procedure set_breakpoints (first, last : in KDF9.order_word_number);
    56.
    57.    procedure handle_breakpoint;
    58.
    59.    -- Support for watchpoints.
    60.
    61.    package watch_flags is new generic_sets(member => KDF9.address);
    62.
    63.    fetchpoints : watch_flags.set := watch_flags.empty_set;
    64.    storepoints : watch_flags.set := watch_flags.empty_set;
    65.
    66.    procedure clear_all_watchpoints;
    67.
    68.    procedure set_fetch_points (first, last : in KDF9.address);
    69.
    70.    procedure set_store_points (first, last : in KDF9.address);
    71.
    72.    procedure act_on_any_two_syllable_order_watchpoints
    73.       with Pre => the_diagnostic_mode /= fast_mode;
    74.
    75.    procedure act_on_any_data_access_order_watchpoints
    76.       with Pre => the_diagnostic_mode /= fast_mode;
    77.
    78.
    79.    --
    80.    -- Retrospective tracing.
    81.    --
    82.
    83.    FIFO_size : constant := 256;
    84.
    85.    type FIFO_index is mod FIFO_size;
    86.
    87.    -- Support for all-instruction retrospective tracing.
    88.
    89.    type retro_FIFO_entry is
    90.       record
    91.          location  : KDF9.syllable_address;
    92.          order     : KDF9.syllable_group;
    93.          parameter : KDF9.word;
    94.          ICR_value : KDF9.order_counter;
    95.          CPU_time  : KDF9.us;
    96.          nested    : KDF9.nest_depth;
    97.          called    : KDF9.sjns_depth;
    98.          V, T, D   : Boolean;
    99.          level     : KDF9.priority;
   100.       end record;
   101.
   102.    retro_FIFO  : array (tracing.FIFO_index) of tracing.retro_FIFO_entry;
   103.
   104.    retro_FIFO_index : tracing.FIFO_index := 0;
   105.
   106.    retro_FIFO_count : Natural range 0 .. FIFO_size := 0;
   107.
   108.    procedure clear_retro_FIFO;
   109.
   110.    procedure take_note_of (the_value : in KDF9.word);
   111.
   112.    -- Support for retrospective peripheral I/O tracing.
   113.
   114.    type IOC_event_kind is (start_transfer,
   115.                            finis_transfer,
   116.                            store_lockout,
   117.                            buffer_lockout,
   118.                            buffer_status);
   119.
   120.    type IOC_FIFO_entry (kind : IOC_event_kind := start_transfer) is
   121.       record
   122.          ICR_value       : KDF9.order_counter;
   123.          order_address   : KDF9.syllable_address;
   124.          decoded_order   : KDF9.decoded_order;
   125.          initiation_time : KDF9.us;
   126.          device_name     : IOC.device_name;
   127.          is_for_Director : Boolean;
   128.          priority_level  : KDF9.priority;
   129.          context         : KDF9.context;
   130.          case kind is
   131.             when start_transfer | finis_transfer =>
   132.                completion_time : KDF9.us;
   133.                control_word    : KDF9.Q_register;
   134.             when store_lockout =>
   135.                data_address : KDF9.Q_part;
   136.             when buffer_lockout =>
   137.                null;
   138.             when buffer_status =>
   139.                Q_register : KDF9.Q_register;
   140.                status     : Boolean;
   141.          end case;
   142.       end record;
   143.
   144.    IOC_FIFO  : array (tracing.FIFO_index) of tracing.IOC_FIFO_entry;
   145.
   146.    IOC_FIFO_index : tracing.FIFO_index := 0;
   147.
   148.    IOC_FIFO_count : Natural range 0 .. FIFO_size := 0;
   149.
   150.    procedure clear_IOC_FIFO;
   151.
   152.    procedure take_note_of_IO_start (
   153.                                     device_name     : in IOC.device_name;
   154.                                     completion_time : in KDF9.us;
   155.                                     control_word    : in KDF9.Q_register
   156.                                    );
   157.
   158.    procedure take_note_of_IO_finis (
   159.                                     ICR_value       : in KDF9.order_counter;
   160.                                     order_address   : in KDF9.syllable_address;
   161.                                     decoded_order   : in KDF9.decoded_order;
   162.                                     initiation_time : in KDF9.us;
   163.                                     device_name     : in IOC.device_name;
   164.                                     is_for_Director : Boolean;
   165.                                     priority_level  : in KDF9.priority;
   166.                                     completion_time : in KDF9.us;
   167.                                     control_word    : in KDF9.Q_register
   168.                                    );
   169.
   170.    procedure take_note_of_store_lockout  (device_name : in IOC.device_name);
   171.
   172.    procedure take_note_of_buffer_lockout (device_name : in IOC.device_name);
   173.
   174.    procedure take_note_of_test (
   175.                                 device_name : in IOC.device_name;
   176.                                 Q_register  : in KDF9.Q_register;
   177.                                 status      : in Boolean
   178.                                 );
   179.
   180.
   181.    -- Support for retrospective interrupt-request tracing.
   182.
   183.    max_interrupt_message_length : constant := 100;
   184.    type interrupt_FIFO_entry is
   185.       record
   186.          interrupt_code : KDF9.interrupt_number;
   187.          ICR_value      : KDF9.order_counter;
   188.          order_address  : KDF9.syllable_address;
   189.          busy_time      : KDF9.us;
   190.          priority_level : KDF9.priority;
   191.          context        : KDF9.context;
   192.          message        : String (1..max_interrupt_message_length);
   193.       end record;
   194.
   195.    interrupt_FIFO  : array (tracing.FIFO_index) of tracing.interrupt_FIFO_entry;
   196.
   197.    interrupt_FIFO_index : tracing.FIFO_index := 0;
   198.
   199.    interrupt_FIFO_count : Natural range 0 .. FIFO_size := 0;
   200.
   201.    procedure clear_interrupt_FIFO;
   202.
   203.    procedure take_note_of_interrupt (interrupt_code : in KDF9.interrupt_number; message : in String);
   204.
   205.
   206.    -- Support for the instruction-type and instruction-word frequency histograms.
   207.
   208.    the_histogram : array (KDF9.syllable) of KDF9.order_counter;
   209.
   210.    procedure clear_the_histogram;
   211.
   212.    procedure add_INS_to_the_histogram
   213.       with Inline;
   214.
   215.    the_profile   : array (KDF9.order_word_number) of KDF9.order_counter;
   216.
   217.    procedure clear_the_profile;
   218.
   219.    procedure add_CIA_to_the_profile
   220.       with Inline;
   221.
   222. end tracing;

 594 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/kdf9_char_sets.ads
Source file time stamp: 2020-08-01 23:48:31
Compiled at: 2020-11-12 18:12:12

     1. -- kdf9_char_sets.ads
     2. --
     3. -- The architecturally-defined character codes of the KDF9 computer.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Characters.Latin_1;
    20.
    21. use  Ada.Characters.Latin_1;
    22.
    23. package KDF9_char_sets is
    24.
    25.    --
    26.    -- The KDF9 character. Each symbol occupies six bits, and they are packed 8 per word.
    27.    --
    28.
    29.    type symbol is mod 2**6;
    30.
    31.    type symbol_index is mod 8;
    32.
    33.    Blank_Space  : constant KDF9_char_sets.symbol := 8#00#;
    34.    Line_Shift   : constant KDF9_char_sets.symbol := 8#02#;
    35.    Page_Change  : constant KDF9_char_sets.symbol := 8#03#;
    36.    Tabulation   : constant KDF9_char_sets.symbol := 8#04#;
    37.    Case_Shift   : constant KDF9_char_sets.symbol := 8#06#;
    38.    Case_Normal  : constant KDF9_char_sets.symbol := 8#07#;
    39.    Tape_Mark    : constant KDF9_char_sets.symbol := 8#17#;
    40.    Semi_Colon   : constant KDF9_char_sets.symbol := 8#34#;
    41.    Upper_Case_D : constant KDF9_char_sets.symbol := 8#44#;
    42.    Upper_Case_M : constant KDF9_char_sets.symbol := 8#55#;
    43.    Upper_Case_P : constant KDF9_char_sets.symbol := 8#60#;
    44.    End_Message  : constant KDF9_char_sets.symbol := 8#75#;
    45.    Word_Filler  : constant KDF9_char_sets.symbol := 8#77#;
    46.    Group_Mark   : constant KDF9_char_sets.symbol := 8#77#;
    47.
    48.    --
    49.    -- These are the 8 bits of a paper tape frame containing these characters.
    50.    --
    51.    Semi_Colon_tape_bits  : constant := 8#074#;
    52.    End_Message_tape_bits : constant := 8#175#;
    53.
    54.    --
    55.    -- KDF9 <=> ISO Latin-1 character code inter-relationaships.
    56.    --
    57.
    58.    type output_code_table is array (KDF9_char_sets.symbol) of Character;
    59.    type input_code_table  is array (Character)   of KDF9_char_sets.symbol;
    60.
    61.    C_N : constant Character := 'ñ';  -- Models KDF9's Case_Normal in legible Latin-1.
    62.    C_S : constant Character := 'ß';  -- Models KDF9's Case_Shift  in legible Latin-1.
    63.    E_M : constant Character := '|';  -- Models KDF9's End_Message in legible Latin-1.
    64.    W_F : constant Character := 'Ø';  -- Models KDF9's Word Filler in legible Latin-1.
    65.
    66.    -- The Line Printer code:
    67.    --    W_F is used for values that have no printable representation.
    68.    to_LP : constant output_code_table
    69.          :=  (' ',  W_F,   LF,   FF,  W_F,  W_F,  '%',  ''',
    70.               ':',  '=',  '(',  ')',  '£',  '*',  ',',  '/',
    71.               '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
    72.               '8',  '9',  W_F,  'º',  ';',  '+',  '-',  '.',
    73.               W_F,  'A',  'B',  'C',  'D',  'E',  'F',  'G',
    74.               'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
    75.               'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
    76.               'X',  'Y',  'Z',  W_F,  W_F,  W_F,  W_F,  W_F
    77.              );
    78.
    79.    -- The Card Reader code:
    80.    --    W_F is used for external characters that have no assigned punching.
    81.    CR_in : constant input_code_table
    82.          := (' ' => 8#00#,  '"' => 8#01#,  '®' => 8#02#,  '©' => 8#03#,
    83.              '¬' => 8#04#,  '#' => 8#05#,  '%' => 8#06#,  ''' => 8#07#,
    84.              ':' => 8#10#,  '=' => 8#11#,  '(' => 8#12#,  ')' => 8#13#,
    85.              '£' => 8#14#,  '*' => 8#15#,  ',' => 8#16#,  '/' => 8#17#,
    86.              '0' => 8#20#,  '1' => 8#21#,  '2' => 8#22#,  '3' => 8#23#,
    87.              '4' => 8#24#,  '5' => 8#25#,  '6' => 8#26#,  '7' => 8#27#,
    88.              '8' => 8#30#,  '9' => 8#31#,  '_' => 8#32#,  'º' => 8#33#,
    89.              ';' => 8#34#,  '+' => 8#35#,  '-' => 8#36#,  '.' => 8#37#,
    90.
    91.              '@' => 8#40#,  'A' => 8#41#,  'B' => 8#42#,  'C' => 8#43#,
    92.              'D' => 8#44#,  'E' => 8#45#,  'F' => 8#46#,  'G' => 8#47#,
    93.              'H' => 8#50#,  'I' => 8#51#,  'J' => 8#52#,  'K' => 8#53#,
    94.              'L' => 8#54#,  'M' => 8#55#,  'N' => 8#56#,  'O' => 8#57#,
    95.              'P' => 8#60#,  'Q' => 8#61#,  'R' => 8#62#,  'S' => 8#63#,
    96.              'T' => 8#64#,  'U' => 8#65#,  'V' => 8#66#,  'W' => 8#67#,
    97.              'X' => 8#70#,  'Y' => 8#71#,  'Z' => 8#72#,  '{' => 8#73#,
    98.              '}' => 8#74#,  E_M => 8#75#,  '\' => 8#76#,  W_F => 8#77#,
    99.
   100.                             'a' => 8#41#,  'b' => 8#42#,  'c' => 8#43#,
   101.              'd' => 8#44#,  'e' => 8#45#,  'f' => 8#46#,  'g' => 8#47#,
   102.              'h' => 8#50#,  'i' => 8#51#,  'j' => 8#52#,  'k' => 8#53#,
   103.              'l' => 8#54#,  'm' => 8#55#,  'n' => 8#56#,  'o' => 8#57#,
   104.              'p' => 8#60#,  'q' => 8#61#,  'r' => 8#62#,  's' => 8#63#,
   105.              't' => 8#64#,  'u' => 8#65#,  'v' => 8#66#,  'w' => 8#67#,
   106.              'x' => 8#70#,  'y' => 8#71#,  'z' => 8#72#,
   107.              others => Word_Filler
   108.             );
   109.
   110.    -- The Card Punch code:
   111.    to_CP : constant output_code_table
   112.          := (' ',  '"',  '®',  '©',  '¬',  '#',  '%',  ''',
   113.              ':',  '=',  '(',  ')',  '£',  '*',  ',',  '/',
   114.              '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
   115.              '8',  '9',  '_',  'º',  ';',  '+',  '-',  '.',
   116.              '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
   117.              'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
   118.              'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
   119.              'X',  'Y',  'Z',  '{',  '}',  E_M,  '\',  W_F
   120.             );
   121.
   122.    -- Two-shift devices expand the code by adopting alternative representations
   123.    --    depending on the current "shift".
   124.    -- The Flexowriter type cage really did shift up and down to bring the
   125.    --    appropriate glyph set into position.
   126.
   127.    subtype letter_case is KDF9_char_sets.symbol range Blank_Space .. Case_Normal;
   128.    both   : constant KDF9_char_sets.symbol := Blank_Space;
   129.    normal : constant KDF9_char_sets.symbol := Case_Normal;
   130.    shift  : constant KDF9_char_sets.symbol := Case_Shift;
   131.
   132.    case_of : constant input_code_table
   133.            := (' ' =>  both,  '"' =>  both,   LF =>  both,   FF =>  both,
   134.                 HT =>  both,  '#' =>  both,  C_S =>  both,  C_N =>  both,
   135.                '&' =>  both,  '?' =>  both,  '!' =>  both,  '%' =>  both,
   136.                ''' =>  both,  '$' =>  both,  '~' =>  both,  ':' => shift,
   137.                '^' => shift,  '[' => shift,  ']' => shift,  '<' => shift,
   138.                '>' => shift,  '=' => shift,  '×' => shift,  '÷' => shift,
   139.                '(' => shift,  ')' => shift,  '_' =>  both,  '£' => shift,
   140.                ';' =>  both,  '±' => shift,  '*' => shift,  ',' => shift,
   141.
   142.                '@' =>  both,  'a' => shift,  'b' => shift,  'c' => shift,
   143.                'd' => shift,  'e' => shift,  'f' => shift,  'g' => shift,
   144.                'h' => shift,  'i' => shift,  'j' => shift,  'k' => shift,
   145.                'l' => shift,  'm' => shift,  'n' => shift,  'o' => shift,
   146.                'p' => shift,  'q' => shift,  'r' => shift,  's' => shift,
   147.                't' => shift,  'u' => shift,  'v' => shift,  'w' => shift,
   148.                'x' => shift,  'y' => shift,  'z' => shift,  '{' =>  both,
   149.                '}' =>  both,  E_M =>  both,  '\' =>  both,  W_F =>  both,
   150.                others => normal
   151.               );
   152.
   153.    next_case : constant array (shift .. normal) of Character := (normal => C_S, shift => C_N);
   154.
   155.    -- The Case Normal shift paper tape code:
   156.    TP_CN : constant output_code_table
   157.          := (' ',  '"',   LF,   FF,   HT,  '#',  C_S,  C_N,
   158.              '&',  '?',  '!',  '%',  ''',  '$',  '~',  '/',
   159.              '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
   160.              '8',  '9',  '_',  'º',  ';',  '+',  '-',  '.',
   161.              '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
   162.              'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
   163.              'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
   164.              'X',  'Y',  'Z',  '{',  '}',  E_M,  '\',  W_F
   165.             );
   166.
   167.    CN_TR : constant input_code_table
   168.          := (' ' => 8#00#,  '"' => 8#01#,   LF => 8#02#,   FF => 8#03#,
   169.               HT => 8#04#,  '#' => 8#05#,  C_S => 8#06#,  C_N => 8#07#,
   170.              '&' => 8#10#,  '?' => 8#11#,  '!' => 8#12#,  '%' => 8#13#,
   171.              ''' => 8#14#,  '$' => 8#15#,  '~' => 8#16#,  '/' => 8#17#,
   172.              '0' => 8#20#,  '1' => 8#21#,  '2' => 8#22#,  '3' => 8#23#,
   173.              '4' => 8#24#,  '5' => 8#25#,  '6' => 8#26#,  '7' => 8#27#,
   174.              '8' => 8#30#,  '9' => 8#31#,  '_' => 8#32#,  'º' => 8#33#,
   175.              ';' => 8#34#,  '+' => 8#35#,  '-' => 8#36#,  '.' => 8#37#,
   176.              '@' => 8#40#,  'A' => 8#41#,  'B' => 8#42#,  'C' => 8#43#,
   177.              'D' => 8#44#,  'E' => 8#45#,  'F' => 8#46#,  'G' => 8#47#,
   178.              'H' => 8#50#,  'I' => 8#51#,  'J' => 8#52#,  'K' => 8#53#,
   179.              'L' => 8#54#,  'M' => 8#55#,  'N' => 8#56#,  'O' => 8#57#,
   180.              'P' => 8#60#,  'Q' => 8#61#,  'R' => 8#62#,  'S' => 8#63#,
   181.              'T' => 8#64#,  'U' => 8#65#,  'V' => 8#66#,  'W' => 8#67#,
   182.              'X' => 8#70#,  'Y' => 8#71#,  'Z' => 8#72#,  '{' => 8#73#,
   183.              '}' => 8#74#,  E_M => 8#75#,  '\' => 8#76#,  W_F => 8#77#,
   184.              others => 0  -- This must be zero.
   185.             );
   186.
   187.    -- The Case Shift paper tape code:
   188.    TP_CS : constant output_code_table
   189.          := (' ',  '"',   LF,   FF,   HT,  '#',  C_S,  C_N,
   190.              '&',  '?',  '!',  '%',  ''',  '$',  '~',  ':',
   191.              '^',  '[',  ']',  '<',  '>',  '=',  '×',  '÷',
   192.              '(',  ')',  '_',  '£',  ';',  '±',  '*',  ',',
   193.              '@',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
   194.              'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
   195.              'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
   196.              'x',  'y',  'z',  '{',  '}',  E_M,  '\',  W_F
   197.             );
   198.
   199.    CS_TR : constant input_code_table
   200.          := (' ' => 8#00#,  '"' => 8#01#,   LF => 8#02#,   FF => 8#03#,
   201.               HT => 8#04#,  '#' => 8#05#,  C_S => 8#06#,  C_N => 8#07#,
   202.              '&' => 8#10#,  '?' => 8#11#,  '!' => 8#12#,  '%' => 8#13#,
   203.              ''' => 8#14#,  '$' => 8#15#,  '~' => 8#16#,  ':' => 8#17#,
   204.              '^' => 8#20#,  '[' => 8#21#,  ']' => 8#22#,  '<' => 8#23#,
   205.              '>' => 8#24#,  '=' => 8#25#,  '×' => 8#26#,  '÷' => 8#27#,
   206.              '(' => 8#30#,  ')' => 8#31#,  '_' => 8#32#,  '£' => 8#33#,
   207.              ';' => 8#34#,  '±' => 8#35#,  '*' => 8#36#,  ',' => 8#37#,
   208.              '@' => 8#40#,  'a' => 8#41#,  'b' => 8#42#,  'c' => 8#43#,
   209.              'd' => 8#44#,  'e' => 8#45#,  'f' => 8#46#,  'g' => 8#47#,
   210.              'h' => 8#50#,  'i' => 8#51#,  'j' => 8#52#,  'k' => 8#53#,
   211.              'l' => 8#54#,  'm' => 8#55#,  'n' => 8#56#,  'o' => 8#57#,
   212.              'p' => 8#60#,  'q' => 8#61#,  'r' => 8#62#,  's' => 8#63#,
   213.              't' => 8#64#,  'u' => 8#65#,  'v' => 8#66#,  'w' => 8#67#,
   214.              'x' => 8#70#,  'y' => 8#71#,  'z' => 8#72#,  '{' => 8#73#,
   215.              '}' => 8#74#,  E_M => 8#75#,  '\' => 8#76#,  W_F => 8#77#,
   216.              others => 0  -- This must be zero.
   217.             );
   218.
   219.    function glyph_for (char : Character)
   220.    return Character
   221.    is (case char is
   222.            when LF     => '®',
   223.            when FF     => '©',
   224.            when HT     => '¬',
   225.            when others => char
   226.       );
   227.
   228.    --
   229.    -- Used when (un)packing 8-bit bytes for raw data I/O.
   230.    --
   231.
   232.    type octet_index is mod 6;
   233.
   234.    type octet is mod 2**8;
   235.
   236. end KDF9_char_sets;

 236 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/posix.adb
Source file time stamp: 2020-08-06 23:06:43
Compiled at: 2020-11-12 18:12:12

     1. -- posix.adb
     2. --
     3. -- Provide a binding to a small subset of POSIX I/O operations.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with System;
    20. --
    21. with Ada.Characters.Latin_1;
    22.
    23. use  Ada.Characters.Latin_1;
    24.
    25. package body POSIX is
    26.
    27.    -- N.B. in POSIX the term 'buffer' is used conventionally.
    28.    --      It does NOT refer to a KDF9 DMA channel.
    29.
    30.    use type C.int;
    31.    use type C.long;
    32.
    33.    procedure verify (IO_status : in Integer; what : in String := "") is
    34.    begin
    35.       if IO_status < 0 then
    36.          put_error_message("POSIX operation failed in " & what);
    37.          raise POSIX_IO_error with what;
    38.       end if;
    39.    end verify;
    40.
    41.    procedure verify (IO_status : in C.long; what : in String := "") is
    42.    begin
    43.       if IO_status < 0 then
    44.          put_error_message("POSIX operation failed in " & what);
    45.          raise POSIX_IO_error with what;
    46.       end if;
    47.    end verify;
    48.
    49.    procedure verify (IO_status : in C.int; what : in String := "") is
    50.    begin
    51.       if IO_status < 0 then
    52.          put_error_message("POSIX operation failed in " & what);
    53.          raise POSIX_IO_error with what;
    54.       end if;
    55.    end verify;
    56.
    57.    function verify (IO_status : C.long; what : String := "")
    58.    return POSIX.file_position
    59.    is (
    60.        if IO_status < 0 then
    61.           raise POSIX_IO_error with what
    62.        else
    63.           POSIX.file_position(IO_status)
    64.       );
    65.
    66.    function verify (IO_status : C.int; what : String := "")
    67.    return Integer
    68.    is (
    69.        if IO_status < 0 then
    70.           raise POSIX_IO_error with what
    71.        else
    72.           Integer(IO_status)
    73.       );
    74.
    75.    function creat (name : C.char_array;  permissions : C.int)
    76.    return C.int
    77.       with Import, Convention => C;
    78.
    79.    function create (name : String;  permissions : POSIX.permission_set)
    80.    return Integer is
    81.       fd : constant C.int := creat(C.To_C(name, Append_Nul => True), C.int(permissions));
    82.    begin
    83.        verify(fd, "create: " & name);
    84.        return Integer(fd);
    85.    end create;
    86.
    87.    function open (name : C.char_array;  mode : C.int)
    88.    return C.int
    89.       with Import, Convention => C;
    90.
    91.    function open (name : String;  mode : POSIX.access_mode)
    92.    return Integer
    93.    is (
    94.        verify(open(C.To_C(name, Append_Nul => True), C.int(mode)), "open file: " & name)
    95.       );
    96.
    97.    function ftruncate (fd : C.int;  to_length : C.long)
    98.    return C.long
    99.       with Import, Convention => C;
   100.
   101.    procedure truncate (fd : Natural;  to_length : POSIX.file_position := 0) is
   102.    begin
   103.       verify(ftruncate(C.int(fd), C.long(to_length)), "truncate fd: " & fd'Image);
   104.    end truncate;
   105.
   106.    function lseek (fd : C.int;  to_offset : C.long;  whence : C.int)
   107.    return C.long
   108.       with Import, Convention => C;
   109.
   110.    function seek (fd        : Natural;
   111.                   to_offset : POSIX.file_position;
   112.                   whence    : POSIX.seek_origin := from_start)
   113.    return POSIX.file_position
   114.    is (
   115.        verify(lseek(C.int(fd), C.long(to_offset), C.int(whence)),
   116.               "seek fd: " & fd'Image)
   117.       );
   118.
   119.    function read (fd : C.int;  buffer : System.Address;  count : C.int)
   120.    return C.int
   121.       with Import, Convention => C;
   122.
   123.    function read (fd : Natural;  buffer : out String;  count : Natural)
   124.    return Integer is
   125.       size   : constant C.int := C.int(Integer'Min(count, buffer'Length));
   126.       status : C.int;
   127.    begin
   128.        status := read(C.int(fd), buffer'Address, size);
   129.        verify(status, "read fd: " & fd'Image);
   130.        return Integer(status);
   131.    end read;
   132.
   133.    function write (fd : C.int;  buffer : System.Address;  count : C.int)
   134.    return C.int
   135.       with Import, Convention => C;
   136.
   137.    function write (fd : Natural;  buffer : in String;  count : Natural)
   138.    return Integer is
   139.       size   : constant C.int := C.int(Integer'Min(count, buffer'Length));
   140.       status : C.int;
   141.    begin
   142.        status := write(C.int(fd), buffer'Address, size);
   143.        verify(status, "write fd: " & fd'Image);
   144.        return Integer(status);
   145.    end write;
   146.
   147.    function close (fd : C.int)
   148.    return C.int
   149.       with Import, Convention => C;
   150.
   151.    function close (fd : Natural)
   152.    return Integer
   153.    is (
   154.        verify(close(C.int(fd)), "close fd: " & fd'Image)
   155.       );
   156.
   157.    function get_errno
   158.    return C.int
   159.       with Import, Convention => C, External_Name => "__get_errno";
   160.
   161.    function error_number
   162.    return Integer
   163.    is (Integer(get_errno));
   164.
   165.    procedure set_errno (error_number : in C.int)
   166.       with Import, Convention => C, External_Name => "__set_errno";
   167.
   168.    procedure set_error_number (error_number : in Integer) is
   169.    begin
   170.       set_errno(C.int(error_number));
   171.    end set_error_number;
   172.
   173.    procedure perror (error_message : in C.char_array)
   174.       with Import, Convention => C;
   175.
   176.    procedure put_error_message (error_message : in String) is
   177.       message : constant C.char_array := C.To_C(error_message, Append_Nul => True);
   178.    begin
   179.       perror(message);
   180.       set_errno(get_errno);
   181.    end put_error_message;
   182.
   183.    procedure open_UI is
   184.    begin
   185.       UI_in_FD := open(UI_in_name, read_mode);
   186.       verify(UI_in_FD, UI_in_name);
   187.       UI_out_FD := open(UI_out_name, write_mode);
   188.       verify(UI_out_FD, UI_out_name);
   189.       UI_is_open := True;
   190.    end open_UI;
   191.
   192.    procedure ensure_UI_is_open is
   193.    begin
   194.       if not UI_is_open then
   195.          open_UI;
   196.       end if;
   197.    end ensure_UI_is_open;
   198.
   199.    C_reply_string : C.char_array(1 .. 256);
   200.
   201.    function next_file_name (prompt : String)
   202.    return String is
   203.       C_prompt        : constant C.char_array
   204.                       := C.To_C(NL & "ee9: " & prompt & ": ", Append_Nul => False);
   205.       C_reply_length : C.Int;
   206.    begin
   207.       ensure_UI_is_open;
   208.       verify(write(C.int(UI_out_FD), C_prompt'Address, C_prompt'Length),
   209.              "prompt: for file name");
   210.       C_reply_string := (256 => Interfaces.C.char(NUL), others => '?');
   211.       C_reply_length := read(C.int(UI_in_FD), C_reply_string'Address, C_reply_string'Length);
   212.       if C_reply_length < 2 then
   213.          -- ^C typed, probably.
   214.          return "";
   215.       else
   216.          return C.To_Ada(C_reply_string)(1..Natural(C_reply_length-1));
   217.       end if;
   218.    end next_file_name;
   219.
   220.    procedure data_prompt (offline   : in  Boolean;
   221.                           prompt    : in String;
   222.                           response  : out response_kind) is
   223.       message  : constant String := "ee9: " & prompt & ": ";
   224.       C_prompt : constant C.char_array := C.To_C(NL & message, Append_Nul => True);
   225.       C_reply_length : C.Int;
   226.    begin
   227.       if offline then
   228.          output_line("ee9: Running in the non-interactive mode: EOF signalled.");
   229.          response := EOF_response;
   230.          return;
   231.       end if;
   232.       ensure_UI_is_open;
   233.       verify(write(C.int(UI_out_FD), C_prompt'Address, C_prompt'Length-1),
   234.              "prompt: " & message);
   235.       C_reply_string := (others => '?');
   236.       C_reply_string(256) := C.char(NUL);
   237.       C_reply_length := read(C.int(UI_in_FD), C_reply_string'Address, 2);
   238.       verify(C_reply_length, "prompt: reply");
   239.
   240.       response := wrong_response;
   241.
   242.       if C_reply_length = 0                 or else
   243.             C.To_Ada(C_reply_string(1)) = LF   then
   244.          response := EOF_response;
   245.       elsif C_reply_length = 2                  and then
   246.                C.To_Ada(C_reply_string(2)) = LF and then
   247.                   C.To_Ada(C_reply_string(1)) = '/' then
   248.          response := name_response;
   249.       elsif C_reply_length = 2                  and then
   250.                C.To_Ada(C_reply_string(2)) = LF and then
   251.                   C.To_Ada(C_reply_string(1)) = '@' then
   252.          response := at_response;
   253.       elsif C_reply_length = 2                                                    and then
   254.                C.To_Ada(C_reply_string(Interfaces.C.size_t(C_reply_length))) = LF and then
   255.                    C.To_Ada(C_reply_string(1)) = '='                                  then
   256.          response := here_response;
   257.       elsif C_reply_length > 0 then
   258.          while C.To_Ada(C_reply_string(Interfaces.C.size_t(C_reply_length))) /= LF loop
   259.             C_reply_length := read(C.int(UI_in_FD), C_reply_string'Address, 1);
   260.             verify(C_reply_length, "LF");
   261.          end loop;
   262.       end if;
   263.    end data_prompt;
   264.
   265.    procedure debug_prompt (offline   : in  Boolean;
   266.                            reason    : in String;
   267.                            response  : out response_kind;
   268.                            letter    : out Character) is
   269.       prompt       : constant String
   270.                    := "ee9: " & reason & ": (d:ebug | f:ast | t:race | p:ause or q:uit)? ";
   271.       UNIX_prompt  : constant C.char_array := C.To_C(NL & prompt, Append_Nul => True);
   272.       C_reply_length : C.Int;
   273.    begin
   274.       if offline then
   275.          output_line("ee9: Running in the non-interactive mode: EOF signalled.");
   276.          response := EOF_response;
   277.          letter   := '?';
   278.          return;
   279.       end if;
   280.       ensure_UI_is_open;
   281.       verify(write(C.int(UI_out_FD), UNIX_prompt'Address, UNIX_prompt'Length-1),
   282.              "prompt: " & prompt);
   283.       C_reply_string := (others => '?');
   284.       C_reply_string(256) := C.char(NUL);
   285.       C_reply_length := read(C.int(UI_in_FD), C_reply_string'Address, 2);
   286.       verify(C_reply_length, "prompt: reply");
   287.
   288.       response := wrong_response;
   289.
   290.       if C_reply_length = 0               or else
   291.             C.To_Ada(C_reply_string(1)) = LF then
   292.          response := EOF_response;
   293.       elsif C_reply_length = 2 then
   294.          letter := C.To_Ada(C_reply_string(1));
   295.          if C.To_Ada(C_reply_string(2)) = LF                                  and then
   296.                letter in 'd' | 'f' | 'p' | 'q' | 't'| 'D' | 'F' | 'P' | 'Q' | 'T' then
   297.          response := name_response;
   298.          end if;
   299.       elsif C_reply_length > 0 then
   300.          while C.To_Ada(C_reply_string(Interfaces.C.size_t(C_reply_length))) /= LF loop
   301.             C_reply_length := read(C.int(UI_in_FD), C_reply_string'Address, 1);
   302.             verify(C_reply_length, "LF");
   303.          end loop;
   304.       end if;
   305.    end debug_prompt;
   306.
   307.    procedure output (message : in String) is
   308.       UNIX_message : constant C.char_array := C.To_C(message, Append_Nul => False);
   309.    begin
   310.       if message = "" then
   311.          return;
   312.       end if;
   313.       ensure_UI_is_open;
   314.       verify(write(C.int(UI_out_FD), UNIX_message'Address, UNIX_message'Length),
   315.              "output: " & message);
   316.    end output;
   317.
   318.    procedure output_line (message : in String) is
   319.       message_line : constant String := message & NL;
   320.    begin
   321.       output(message_line);
   322.    end output_line;
   323.
   324.    procedure output (message  : in Character) is
   325.    begin
   326.       ensure_UI_is_open;
   327.       verify(write(C.int(UI_out_FD), message'Address, 1), "output: " & message);
   328.    end output;
   329.
   330.    procedure output_line is
   331.    begin
   332.       output(NL);
   333.    end output_line;
   334.
   335.    procedure input  (message : out Character) is
   336.    begin
   337.       ensure_UI_is_open;
   338.       verify(read(C.int(UI_in_FD), message'Address, 1), "input");
   339.    end input;
   340.
   341.    procedure POSIX_exit (status : in C.int)
   342.       with Import, Convention => C, External_Name => "exit";
   343.
   344.    procedure exit_program (status : in Natural) is
   345.    begin
   346.       POSIX_exit(C.int(status));
   347.    end exit_program;
   348.
   349. end POSIX;
   350.

Compiling: ../Source/posix.ads
Source file time stamp: 2020-08-07 00:37:32
Compiled at: 2020-11-12 18:12:12

     1. -- posix.ads
     2. --
     3. -- Provide a binding to a small subset of POSIX I/O operations.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Interfaces.C;
    20. --
    21. with OS_specifics;
    22.
    23. use  OS_specifics;
    24.
    25. package POSIX is
    26.
    27.    NL : constant String := OS_specifics.EOL;
    28.
    29.    package C renames Interfaces.C;
    30.
    31.    -- N.B. within POSIX the term 'buffer' is used conventionally.
    32.    -- It does NOT refer to a KDF9 DMA channel.
    33.
    34.    POSIX_IO_error   : exception;
    35.
    36.    type permission_set is mod 2**9;
    37.
    38.    world_read_permission  : constant permission_set := 4;
    39.    world_write_permission : constant permission_set := 2;
    40.    world_exec_permission  : constant permission_set := 1;
    41.
    42.    group_read_permission  : constant permission_set := 8 * world_read_permission;
    43.    group_write_permission : constant permission_set := 8 * world_write_permission;
    44.    group_exec_permission  : constant permission_set := 8 * world_exec_permission;
    45.
    46.    owner_read_permission  : constant permission_set := 8 * group_read_permission;
    47.    owner_write_permission : constant permission_set := 8 * group_write_permission;
    48.    owner_exec_permission  : constant permission_set := 8 * group_exec_permission;
    49.
    50.    function create (name        : String;
    51.                     permissions : POSIX.permission_set)
    52.    return Integer;
    53.
    54.    type access_mode is mod 3;
    55.
    56.    read_mode  : constant POSIX.access_mode := 0;
    57.    write_mode : constant POSIX.access_mode := 1;
    58.    rd_wr_mode : constant POSIX.access_mode := 2;
    59.
    60.    function open (name : String;
    61.                   mode : POSIX.access_mode)
    62.    return Integer;
    63.
    64.    type file_position is new C.long;
    65.
    66.    procedure truncate (fd        : Natural;
    67.                        to_length : POSIX.file_position := 0);
    68.
    69.    type seek_origin is mod 3;
    70.
    71.    from_start : constant POSIX.seek_origin := 0;
    72.    from_here  : constant POSIX.seek_origin := 1;
    73.    from_end   : constant POSIX.seek_origin := 2;
    74.
    75.    function seek (fd        : Natural;
    76.                   to_offset : POSIX.file_position;
    77.                   whence    : POSIX.seek_origin := from_start)
    78.    return POSIX.file_position;
    79.
    80.    function read (fd : Natural;  buffer : out String;  count : Natural)
    81.    return Integer;
    82.
    83.    function write (fd : Natural;  buffer : in String;  count : Natural)
    84.    return Integer;
    85.
    86.    function close (fd : Natural)
    87.    return Integer;
    88.
    89.    --  get the task-safe error number
    90.    function error_number
    91.    return Integer;
    92.
    93.    --  set the task-safe error number
    94.    procedure set_error_number (error_number : in Integer);
    95.
    96.    procedure put_error_message (error_message : in String); -- and set the errno error number
    97.
    98.    procedure exit_program (status : in Natural);
    99.
   100.    procedure verify (IO_status : in Integer; what : in String := "");
   101.
   102.    -- The following all act on the interactive UI.
   103.
   104.    procedure ensure_UI_is_open;
   105.
   106.    UI_in_FD, UI_out_FD : Natural;
   107.    UI_is_open          : Boolean := False;
   108.
   109.    procedure output (message : in String);
   110.
   111.    procedure output (message : in Character);
   112.
   113.    procedure output_line (message : in String);  -- output(message & EOL)
   114.
   115.    procedure output_line;  -- output(EOL)
   116.
   117.    procedure input  (message  : out Character);
   118.
   119.    type response_kind is (EOF_response, name_response, at_response, here_response, wrong_response);
   120.
   121.    -- If we are in non-interactive mode, log an error and set response to wrong_response.
   122.    -- Display a message and read a reply, letter.
   123.    -- If it is null (EOF signalled) or EOL, set response to EOF_response.
   124.    -- If it is in 'd' | 'f' | 'p' | 't' | 'D' | 'F' | 'P' | 'T', set response to name_response.
   125.    -- If it is anything else, set response to wrong_response.
   126.    procedure debug_prompt (offline   : in  Boolean;
   127.                            reason    : in String;
   128.                            response  : out response_kind;
   129.                            letter    : out Character);
   130.
   131.    -- If we are in non-interactive mode, log an error and set response to wrong_response.
   132.    -- Display a prompt message and read a reply.
   133.    -- If it is null or EOL:   set response to EOF_response.
   134.    -- If it is /:             set response to name_response.
   135.    -- If it is @:             set response to at_response.
   136.    -- If it is =:             set response to here_response.
   137.    -- If it is anything else: set response to wrong_response.
   138.    procedure data_prompt (offline   : in  Boolean;
   139.                           prompt    : in String;
   140.                           response  : out response_kind);
   141.
   142.    -- Get the name of the file.
   143.    function next_file_name (prompt : String)
   144.    return String;
   145.
   146. end POSIX;

 350 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/ioc-fast-mt.adb
Source file time stamp: 2020-11-12 18:12:08
Compiled at: 2020-11-12 18:12:12

     1. -- ioc-fast-MT.adb
     2. --
     3. -- Emulation of magnetic tape decks and buffers.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Exceptions;
    20. with Ada.IO_Exceptions;
    21. --
    22. with HCI;
    23. with tracing;
    24.
    25. use  HCI;
    26. use  tracing;
    27.
    28. package body IOC.fast.MT is
    29.
    30.    -- Ada direct-access file management.
    31.
    32.    procedure open_RO (the_tape : in out MT.file; name : in String) is
    33.    begin
    34.       MT_slice_IO.Open(the_tape.reel, In_File, name);
    35.       the_tape.has_a_WP_ring := False;
    36.    exception
    37.       when others =>
    38.          trap_operator_error(name & " cannot be opened for reading or writing");
    39.    end open_RO;
    40.
    41.    procedure open_RW (the_tape : in out MT.file; name : in String) is
    42.    begin
    43.       MT_slice_IO.Open(the_tape.reel, Inout_File, name);
    44.       the_tape.has_a_WP_ring := True;
    45.    exception
    46.       when Ada.IO_Exceptions.Use_Error =>
    47.          the_tape.has_a_WP_ring := False;
    48.          open_RO(the_tape, name);
    49.       when Ada.IO_Exceptions.Name_Error =>
    50.          trap_operator_error(name & " cannot be found");
    51.       when error : others =>
    52.          trap_operator_error(
    53.                              name
    54.                            & " failed to open because of "
    55.                            &  Ada.Exceptions.Exception_Message(error)
    56.                             );
    57.    end open_RW;
    58.
    59.    procedure close (the_tape : in out MT.file) is
    60.    begin
    61.       if the_tape.has_a_WP_ring then
    62.          MT_slice_IO.Flush(the_tape.reel);
    63.       end if;
    64.       MT_slice_IO.Close(the_tape.reel);
    65.    end close;
    66.
    67.    function is_open (the_tape : in MT.file)
    68.    return Boolean
    69.    is (Is_Open(the_tape.reel));
    70.
    71.    -- Slice management.
    72.
    73.    end_of_tape : exception;
    74.
    75.    procedure write_slice (the_tape : in out MT.file;
    76.                           slice    : in MT.slice) is
    77.    begin
    78.       the_tape.position := the_tape.position + 1;
    79.       MT_slice_IO.Write(the_tape.reel, slice, to => the_tape.position);
    80.       if slice.kind not in tape_gap_kind then
    81.          the_tape.last_data_index := Count'Max(the_tape.last_data_index, the_tape.position);
    82.       end if;
    83.    exception
    84.       when End_Error =>
    85.          raise end_of_tape with "End_Error writing MT slice number" & the_tape.position'Image;
    86.    end write_slice;
    87.
    88.    procedure read_next_slice (the_tape : in out MT.file;
    89.                               slice    : out MT.slice) is
    90.    begin
    91.       if the_tape.last_data_index > 0 then
    92.          the_tape.position := the_tape.position + 1;
    93.          MT_slice_IO.Read(the_tape.reel, slice, from => the_tape.position);
    94.       else
    95.          raise end_of_tape with "read_next_slice";
    96.       end if;
    97.     exception
    98.        when End_Error =>
    99.           raise end_of_tape with "End_Error reading MT slice number" & the_tape.position'Image;
   100.    end read_next_slice;
   101.
   102.    procedure read_prev_slice (the_tape : in out MT.file;
   103.                               slice    : out MT.slice) is
   104.    begin
   105.       if the_tape.position > 0 then
   106.          MT_slice_IO.Read(the_tape.reel, slice, from => the_tape.position);
   107.          the_tape.position := the_tape.position - 1;
   108.       else
   109.          raise end_of_tape with "read_prev_slice";
   110.       end if;
   111.     exception
   112.        when End_Error =>
   113.           raise end_of_tape with "End_Error reading MT slice number" & the_tape.position'Image;
   114.    end read_prev_slice;
   115.
   116.    procedure bound_the_written_data (the_tape : in out MT.file) is
   117.       the_slice : MT.slice;
   118.    begin
   119.       the_tape.position := Size(the_tape.reel);
   120.       if the_tape.position = 0 then
   121.          -- There is no data in the file.
   122.          the_tape.last_data_index := 0;
   123.          return;
   124.       end if;
   125.       -- Locate the last data slice (if any).
   126.       while the_tape.position > 0 loop
   127.          read_prev_slice(the_tape, the_slice);
   128.       exit when the_slice.kind not in MT.tape_gap_kind;
   129.       end loop;
   130.       if the_slice.kind in MT.tape_gap_kind then
   131.          the_tape.last_data_index := 0;
   132.       else
   133.          the_tape.last_data_index := the_tape.position + 1;
   134.       end if;
   135.       the_tape.position := 0;
   136.    end bound_the_written_data;
   137.
   138.    procedure reset (the_deck : in out MT.deck) is
   139.     begin
   140.       bound_the_written_data(the_deck.tape);
   141.       the_deck.is_LBM_flagged := False;
   142.       the_deck.is_abnormal := False;
   143.       the_deck.unwound_frames := 0;
   144.    exception
   145.       when end_of_tape =>
   146.          the_deck.is_abnormal := True;
   147.          the_deck.is_LBM_flagged := False;
   148.          the_deck.unwound_frames := 0;
   149.    end reset;
   150.
   151.    -- Tape physical characteristics.
   152.
   153.    -- The physical end of tape (PET) is signalled one maximum block length before the tape runs out.
   154.    --  So PET is signalled at max_block_size before the absolute maximum position
   155.    --    to avoid running past the end of the tape when a very large block is written.
   156.
   157.    -- There could be as little as 60 inches of tape between the End of Tape Warning (ETW) and PET.
   158.    -- See the Manual, §22.1.3, p.182.
   159.
   160.    overriding
   161.    procedure Initialize (the_deck : in out MT.deck) is
   162.    begin
   163.       the_deck.device_name := device_name_of(the_deck);
   164.       open_RW(the_deck.tape, the_deck.device_name);
   165.       Initialize(IOC.device(the_deck));
   166.       if the_deck.kind = MT_kind then
   167.          the_deck.terminator        := End_Message;
   168.          the_deck.recording_density := max_bits_per_inch;  -- bits / inch
   169.          the_deck.max_reel_length   := max_reel_length;    -- inches
   170.       else
   171.          the_deck.terminator        := Group_Mark;
   172.          the_deck.recording_density := max_bits_per_inch/2;  -- bits / inch
   173.          the_deck.max_reel_length   := max_reel_length;      -- inches
   174.       end if;
   175.       the_deck.inter_block_gap := the_deck.recording_density / 3;
   176.       the_deck.tape_capacity   := the_deck.max_reel_length * the_deck.recording_density;
   177.       the_deck.PET_position    := the_deck.tape_capacity - max_block_size;
   178.       the_deck.ETW_position    := the_deck.PET_position - 60 * the_deck.recording_density;
   179.       reset(the_deck);
   180.    exception
   181.       when end_of_tape =>
   182.          the_deck.is_abnormal := True;
   183.    end Initialize;
   184.
   185.    function is_at_BTW (the_deck : MT.deck)
   186.    return Boolean
   187.    is (the_deck.is_open and then the_deck.tape.position = 0);
   188.
   189.    function is_loaded (the_deck : MT.deck)
   190.    return Boolean
   191.    is (the_deck.is_open and then the_deck.tape.last_data_index > 0);
   192.
   193.    function is_at_ETW (the_deck : MT.deck)
   194.    return Boolean
   195.    is (the_deck.is_open and then the_deck.unwound_frames >= the_deck.ETW_position);
   196.
   197.    function is_at_PET (the_deck : MT.deck)
   198.    return Boolean
   199.    is (the_deck.is_open and then the_deck.unwound_frames >= the_deck.PET_position);
   200.
   201.    procedure check_for_writing_past_PET (the_deck : in out MT.deck;
   202.                                          do_this  : String) is
   203.    begin
   204.       if is_at_PET (the_deck) then
   205.          the_deck.is_abnormal := True;
   206.          trap_invalid_operand("attempt to " & do_this & " on " & the_deck.device_name);
   207.       end if;
   208.    end check_for_writing_past_PET;
   209.
   210.    -- There are cases that are invalid iff the tape is positioned beyond the last written block.
   211.    function is_at_EOD (the_deck : MT.deck)
   212.    return Boolean
   213.    is (the_deck.is_open and then the_deck.tape.position > the_deck.tape.last_data_index);
   214.
   215.    function tape_traversal_time (the_deck : MT.deck; tape_crossed : KDF9.word)
   216.    return KDF9.us
   217.    is (the_deck.quantum * KDF9.us(tape_crossed));
   218.
   219.    function data_transfer_time (the_deck   : MT.deck;
   220.                                 byte_count : KDF9.word)
   221.    return KDF9.us
   222.    is (the_deck.quantum * KDF9.us(byte_count));
   223.
   224.    -- This is the time the MT deck is busy traversing the interblock gap and the data block.
   225.    function MT_IO_time (the_deck  : MT.deck;
   226.                         Q_operand : in KDF9.Q_register)
   227.    return KDF9.us
   228.    is (KDF9.us(the_deck.inter_block_gap) + 8*KDF9.us(Q_operand.M-Q_operand.I+1) * the_deck.quantum);
   229.
   230.    overriding
   231.    function is_open (the_deck : MT.deck)
   232.    return Boolean
   233.    is (the_deck.tape.is_open);
   234.
   235.    overriding
   236.    function usage (the_deck : MT.deck)
   237.    return KDF9.word
   238.    is (the_deck.bytes_moved);
   239.
   240.    overriding
   241.    procedure close (the_deck : in out MT.deck) is
   242.    begin
   243.       the_deck.tape.close;
   244.    end close;
   245.
   246.    procedure update_statistics (the_deck    : in out MT.deck;
   247.                                 tape_crossed,
   248.                                 bytes_moved : in length_in_frames) is
   249.       real_time : KDF9.us;
   250.    begin
   251.       the_deck.bytes_moved := the_deck.bytes_moved + KDF9.word(bytes_moved);
   252.       real_time := tape_traversal_time(the_deck, KDF9.word(tape_crossed))
   253.                  + data_transfer_time (the_deck, KDF9.word(bytes_moved));
   254.       the_deck.elapsed_time := the_deck.elapsed_time + real_time;
   255.       add_in_the_IO_CPU_time(the_deck, KDF9.word(bytes_moved));
   256.       correct_transfer_time(the_deck, real_time);
   257.    end update_statistics;
   258.
   259.    type movement is (forwards, backwards);
   260.
   261.    procedure note_tape_position (the_deck    : in out MT.deck;
   262.                                  direction   : in MT.movement;
   263.                                  tape_crossed,
   264.                                  bytes_moved : in length_in_frames) is
   265.    begin
   266.       if direction = forwards then
   267.          the_deck.unwound_frames := the_deck.unwound_frames
   268.                                   + MT.length_in_frames(tape_crossed + bytes_moved);
   269.       elsif MT.length_in_frames(tape_crossed + bytes_moved) > the_deck.unwound_frames then
   270.          the_deck.unwound_frames := 0;
   271.       else
   272.          the_deck.unwound_frames := the_deck.unwound_frames
   273.                                   - MT.length_in_frames(tape_crossed + bytes_moved);
   274.       end if;
   275.    end note_tape_position;
   276.
   277.    -- KDF9 MT operations.
   278.
   279.    -- Skip back over erased tape, leaving the_slice containing the next preceding data.
   280.    -- Postcondition: the_deck.is_at_BTW or else the_slice.kind not in tape_gap_kind
   281.    procedure skip_back_over_erasure (the_deck  : in out MT.deck;
   282.                                      the_slice : in out MT.slice;
   283.                                      crossed   : in out length_in_frames) is
   284.    begin
   285.       if the_deck.is_at_BTW then
   286.          return; -- We are as far back as we can go;
   287.       end if;
   288.       if the_slice.kind in data_kind then
   289.          return;  -- We have already found the preceding data block.
   290.       end if;
   291.       loop
   292.          read_prev_slice(the_deck.tape, the_slice);
   293.       exit when the_deck.is_at_BTW or else the_slice.kind not in tape_gap_kind;
   294.          crossed := crossed + the_slice.size;
   295.       end loop;
   296.    exception
   297.       when end_of_tape =>
   298.          the_deck.is_abnormal := True;
   299.          raise end_of_tape with "skip_back_over_erasure";
   300.    end skip_back_over_erasure;
   301.
   302.    -- Skip forward over erased tape, leaving the_slice containing the next following data.
   303.    -- Postcondition: the_deck.is_at_EOD or else the_slice.kind not in tape_gap_kind
   304.    procedure skip_forward_over_erasure (the_deck  : in out MT.deck;
   305.                                         the_slice : in out MT.slice;
   306.                                         crossed   : in out length_in_frames;
   307.                                         caller    : in String := "") is
   308.    begin
   309.       if the_slice.kind in data_slice then
   310.          return;
   311.       end if;
   312.       loop
   313.          read_next_slice(the_deck.tape, the_slice);
   314.       exit when the_deck.is_at_EOD or else the_slice.kind not in MT.tape_gap_kind;
   315.          crossed := crossed + the_slice.size;
   316.       end loop;
   317.    exception
   318.       when end_of_tape =>
   319.          the_deck.is_abnormal := True;
   320.          raise end_of_tape with "in skip_forward_over_erasure for " & caller;
   321.    end skip_forward_over_erasure;
   322.
   323.    -- Deal with blocks of invalid sizes.
   324.    -- 1081 buffers always write and read a whole number of words;
   325.    --    see Manual §22.1.5, p184, ¶2; and Appendix 7 ¶3, p318.
   326.    -- §3.4.7 of the EGDON 3 manual says that the 7-track tape buffer, due to a hardware
   327.    --    limitation, rejects blocks (other than tape marks) of less than 6 characters.
   328.    procedure handle_any_abnormality (the_deck : in out MT.deck;
   329.                                      the_size : in length_in_frames) is
   330.    begin
   331.       case the_deck.kind is
   332.          when MT_kind =>
   333.             the_deck.is_abnormal := the_deck.is_abnormal or (the_size mod 8 /= 0);
   334.          when ST_kind =>
   335.             the_deck.is_abnormal := the_deck.is_abnormal or (the_size < 6);
   336.          when others  =>
   337.             raise emulation_failure
   338.                with "handle_any_abnormality with a deck kind given as "
   339.                   & the_deck.kind'Image
   340.                   & " by "
   341.                   & the_deck.device_name
   342.                   & " with block size"
   343.                   & the_size'Image;
   344.       end case;
   345.    end handle_any_abnormality;
   346.
   347.    procedure read_block (the_deck  : in out MT.deck;
   348.                          the_data  : out MT.block_storage;
   349.                          the_size  : out length_in_frames;
   350.                          direction : in movement := forwards) is
   351.
   352.       left,
   353.       right      : length_in_frames := 1;
   354.       block_size,
   355.       crossed    : length_in_frames := 0;
   356.       is_last,
   357.       is_flagged : Boolean := False;
   358.       the_slice  : MT.slice := a_NULL_slice;
   359.    begin
   360.       the_deck.is_LBM_flagged := False;
   361.
   362.       skip_forward_over_erasure(the_deck, the_slice, crossed, caller => "read_block");
   363.
   364.       -- Ensure that we are not beyond the end of valid data.
   365.       if the_deck.is_at_EOD then
   366.          trap_invalid_operand("there is no data to be read past slice"
   367.                             & the_deck.tape.position'Image
   368.                             & " of "
   369.                             & the_deck.device_name
   370.                              );
   371.       end if;
   372.
   373.       if the_slice.kind in MT.tape_mark_kind then
   374.          -- Deal with a tape mark block; according to the Maual, Appendix 7, §2, p.317,
   375.          --    it reads as a single character with value #17.
   376.          block_size := 8;
   377.          the_data(1)    := KDF9_char_sets.TP_CN(KDF9_char_sets.Tape_Mark);
   378.          the_data(2..8) := (others => KDF9_char_sets.TP_CN(KDF9_char_sets.Blank_Space));
   379.          the_deck.is_LBM_flagged := True;
   380.       else
   381.          -- We have a bona fide data block.
   382.          the_size := 0;
   383.          -- Accumulate a series of slicefuls.
   384.          loop
   385.             right := left + the_slice.size - 1;
   386.             the_data(left .. right) := the_slice.data(1..the_slice.size);
   387.             block_size := block_size + the_slice.size;
   388.             left := left + the_slice.size;
   389.             is_flagged := is_flagged or the_slice.is_LBM_flagged;
   390.             is_last  := the_slice.is_last;
   391.          exit when is_last or block_size = max_block_size;
   392.             read_next_slice(the_deck.tape, the_slice);
   393.          end loop;
   394.          the_deck.is_LBM_flagged := is_flagged;
   395.       end if;
   396.       the_size := block_size;
   397.
   398.       note_tape_position(the_deck, direction,
   399.                         crossed + the_deck.inter_block_gap, bytes_moved => the_size);
   400.       update_statistics(the_deck,
   401.                         crossed + the_deck.inter_block_gap, bytes_moved => the_size);
   402.
   403.      if not is_last and block_size = max_block_size then
   404.          raise emulation_failure
   405.             with "size =" & block_size'Image & " > max_block_size in MT read_block";
   406.       end if;
   407.       handle_any_abnormality(the_deck, block_size);
   408.    exception
   409.       when end_of_tape =>
   410.          the_deck.is_abnormal := True;
   411.          raise end_of_tape with "read_block";
   412.    end read_block;
   413.
   414.    procedure increment (word_address : in out KDF9.address;
   415.                         symbol_nr    : in out KDF9_char_sets.symbol_index) is
   416.    begin
   417.       if symbol_nr < 7 then
   418.          symbol_nr := symbol_nr + 1;
   419.       else
   420.          symbol_nr := 0;
   421.          word_address := word_address + 1;
   422.       end if;
   423.    end increment;
   424.
   425.    tape_mark_data_word : constant KDF9.word := 8#17_00_00_00_00_00_00_00#;
   426.
   427.    procedure read (the_deck       : in out MT.deck;
   428.                    Q_operand      : in KDF9.Q_register;
   429.                    to_terminator  : in Boolean := False) is
   430.       start_address : constant KDF9.address := Q_operand.I;
   431.       end_address   : constant KDF9.address := Q_operand.M;
   432.       the_data : MT.block_storage;
   433.       s        : KDF9_char_sets.symbol_index;
   434.       w        : KDF9.address;
   435.       stored   : KDF9.word := 0;
   436.       the_size : length_in_frames;
   437.    begin
   438.       validate_device(the_deck, Q_operand);
   439.       check_addresses_and_lockouts(start_address, end_address);
   440.
   441.       read_block(the_deck, the_data, the_size);
   442.
   443.       if the_size mod 8 /= 0 and the_deck.kind = MT_kind then
   444.          -- Disregard an incomplete final word; see Manual, §22.1.5, p184, ¶2.
   445.          the_deck.is_abnormal := True;
   446.          the_size := the_size - the_size mod 8;
   447.       end if;
   448.
   449.       -- Store the relevant words.
   450.       w := start_address;
   451.       s := 0;
   452.       for i in 1 .. the_size loop
   453.          if s = 0 then
   454.             store_word(0, w);
   455.          end if;
   456.          store_symbol(CN_TR(the_data(i)), w, s);
   457.          stored := stored + 1;
   458.       exit when (w = end_address) and (s = 7);
   459.       exit when to_terminator and CN_TR(the_data(i)) = the_deck.terminator;
   460.          increment(w, s);
   461.       end loop;
   462.       if to_terminator then
   463.          correct_transfer_time(the_deck, stored);
   464.       end if;
   465.    exception
   466.       when end_of_tape =>
   467.          the_deck.is_abnormal := True;
   468.    end read;
   469.
   470.    procedure find_start_of_earlier_block (the_deck : in out MT.deck;
   471.                                           crossed  : in out length_in_frames) is
   472.       the_slice  : MT.slice := a_NULL_slice;
   473.       block_size : length_in_frames;
   474.    begin
   475.       if the_deck.is_at_BTW then
   476.          return; -- We have already gone as far back as possible.
   477.       end if;
   478.
   479.       -- Skip back over any erasures or tape marks.
   480.       skip_back_over_erasure(the_deck, the_slice, crossed);
   481.       crossed := crossed + the_deck.inter_block_gap;
   482.
   483.       if the_deck.is_at_BTW and the_slice.kind in tape_gap_kind then
   484.          the_deck.is_abnormal := True;
   485.          crossed := 0;
   486.          -- This cannot happen if the tape has (at least) a label.
   487.          trap_invalid_instruction("a backward skip was attempted at BTW");
   488.       end if;
   489.
   490.       if not the_slice.is_last then
   491.          raise emulation_failure
   492.             with "find_start_of_earlier_block at slice "
   493.                & Positive_Count'Image(the_deck.tape.position)
   494.                & " of "
   495.                & the_deck.device_name
   496.                & " failed to locate the last slice of a block";
   497.       end if;
   498.
   499.       -- We have reached the last slice of the block.
   500.       if the_deck.kind = ST_kind and the_slice.kind in tape_mark_kind then
   501.          block_size := 1;
   502.       else
   503.          block_size := the_slice.size;
   504.          -- Jump backwards over data slices until we reach the first of the block.
   505.          while not the_slice.is_first and then the_deck.tape.position > 0 loop
   506.             read_prev_slice(the_deck.tape, the_slice);
   507.             block_size := block_size + the_slice.size;
   508.          end loop;
   509.          handle_any_abnormality(the_deck, block_size);
   510.       end if;
   511.
   512.       crossed := crossed + block_size;
   513.    exception
   514.       when end_of_tape =>
   515.          the_deck.is_abnormal := True;
   516.          raise end_of_tape with "find_start_of_earlier_block";
   517.    end find_start_of_earlier_block;
   518.
   519.    procedure decrement (word_address : in out KDF9.address;
   520.                         symbol_nr    : in out KDF9_char_sets.symbol_index) is
   521.    begin
   522.       if symbol_nr > 0 then
   523.          symbol_nr := symbol_nr - 1;
   524.       else
   525.          symbol_nr := 7;
   526.          word_address := word_address + 1;
   527.       end if;
   528.    end decrement;
   529.
   530.    procedure read_backwards (the_deck       : in out MT.deck;
   531.                              Q_operand      : in KDF9.Q_register;
   532.                              to_terminator  : in Boolean := False) is
   533.       start_address : constant KDF9.address := Q_operand.I;
   534.       end_address   : constant KDF9.address := Q_operand.M;
   535.       terminator    : constant KDF9_char_sets.symbol := the_deck.terminator;
   536.       the_data  : MT.block_storage;
   537.       s         : KDF9_char_sets.symbol_index;
   538.       w         : KDF9.address;
   539.       the_first,
   540.       the_last  : length_in_frames;
   541.       crossed   : length_in_frames := 0 with Warnings => Off;  -- Because its value is never used.
   542.    begin
   543.       validate_device(the_deck, Q_operand);
   544.       check_addresses_and_lockouts(start_address, end_address);
   545.
   546.       -- Locate the start of the previous block.
   547.       find_start_of_earlier_block(the_deck, crossed);
   548.
   549.       -- Read it normally, i.e. forwards.
   550.       read_block(the_deck, the_data, the_last, backwards);
   551.
   552.       -- And retrace our steps, to position the tape as if the block had been read backwards.
   553.       find_start_of_earlier_block(the_deck, crossed);
   554.
   555.       -- Disregard an incomplete first word; see Manual §22.1.5, p184, ¶2; and Appendix 7 ¶3, p318.
   556.       if the_last mod 8 = 0 or the_deck.kind = ST_kind then
   557.          the_first := the_data'First;
   558.       elsif the_last = 1 and the_deck.kind = ST_kind then
   559.          -- See Manual, Appendix 7 ¶2, p317.
   560.          store_word(tape_mark_data_word, start_address);
   561.       else
   562.          the_deck.is_abnormal := True;
   563.          the_first := the_data'First + the_last mod 8;
   564.       end if;
   565.
   566.       -- Store the relevant words.
   567.       w := start_address;
   568.       s := 7;
   569.       for i in reverse the_first .. the_last loop
   570.          if s = 7 then
   571.             store_word(0, w);
   572.          end if;
   573.          store_symbol(CN_TR(the_data(i)), w, s);
   574.       exit when to_terminator and CN_TR(the_data(i)) = terminator;
   575.          decrement(w, s);
   576.       end loop;
   577.    end read_backwards;
   578.
   579.    -- MFRQq
   580.    overriding
   581.    procedure PIA (the_deck    : in out MT.deck;
   582.                   Q_operand   : in KDF9.Q_register;
   583.                   set_offline : in Boolean) is
   584.    begin
   585.       start_data_transfer(the_deck, Q_operand, set_offline, 22 + MT_IO_time(the_deck, Q_operand));
   586.       read(the_deck, Q_operand, to_terminator => False);
   587.       lock_out_relative_addresses(Q_operand);
   588.    end PIA;
   589.
   590.    -- MFREQq
   591.    overriding
   592.    procedure PIB (the_deck    : in out MT.deck;
   593.                   Q_operand   : in KDF9.Q_register;
   594.                   set_offline : in Boolean) is
   595.    begin
   596.       start_data_transfer(the_deck, Q_operand, set_offline, 22 + MT_IO_time(the_deck, Q_operand));
   597.       read(the_deck, Q_operand, to_terminator => True);
   598.       lock_out_relative_addresses(Q_operand);
   599.    end PIB;
   600.
   601.    -- as PIA
   602.    overriding
   603.    procedure PIC (the_deck    : in out MT.deck;
   604.                   Q_operand   : in KDF9.Q_register;
   605.                   set_offline : in Boolean) is
   606.    begin
   607.       the_deck.PIA(Q_operand, set_offline);
   608.    end PIC;
   609.
   610.    -- as PIB
   611.    overriding
   612.    procedure PID (the_deck    : in out MT.deck;
   613.                   Q_operand   : in KDF9.Q_register;
   614.                   set_offline : in Boolean) is
   615.    begin
   616.       the_deck.PIB(Q_operand, set_offline);
   617.    end PID;
   618.
   619.    -- MBRQq
   620.    overriding
   621.    procedure PIE (the_deck    : in out MT.deck;
   622.                   Q_operand   : in KDF9.Q_register;
   623.                   set_offline : in Boolean) is
   624.    begin
   625.       if the_deck.is_at_BTW then
   626.          trap_invalid_instruction("tried MBRQq at BTW");
   627.       end if;
   628.       start_data_transfer(the_deck, Q_operand, set_offline, 22 + MT_IO_time(the_deck, Q_operand));
   629.       read_backwards(the_deck, Q_operand, to_terminator => False);
   630.       if the_deck.kind = ST_kind then
   631.          the_deck.is_LBM_flagged := False;
   632.       end if;
   633.       lock_out_relative_addresses(Q_operand);
   634.    end PIE;
   635.
   636.    -- MBREQq
   637.    overriding
   638.    procedure PIF (the_deck    : in out MT.deck;
   639.                   Q_operand   : in KDF9.Q_register;
   640.                   set_offline : in Boolean) is
   641.    begin
   642.       if the_deck.is_at_BTW then
   643.          trap_invalid_instruction("attempt to read MT backwards to End_Message at BTW");
   644.       end if;
   645.       start_data_transfer(the_deck, Q_operand, set_offline, 22 + MT_IO_time(the_deck, Q_operand));
   646.       read_backwards(the_deck, Q_operand, to_terminator => True);
   647.       if the_deck.kind = ST_kind then
   648.          the_deck.is_LBM_flagged := False;
   649.       end if;
   650.       lock_out_relative_addresses(Q_operand);
   651.    end PIF;
   652.
   653.    -- as PIE
   654.    overriding
   655.    procedure PIG (the_deck    : in out MT.deck;
   656.                   Q_operand   : in KDF9.Q_register;
   657.                   set_offline : in Boolean) is
   658.    begin
   659.       the_deck.PIE(Q_operand, set_offline);
   660.    end PIG;
   661.
   662.    -- as PIF
   663.    overriding
   664.    procedure PIH (the_deck    : in out MT.deck;
   665.                   Q_operand   : in KDF9.Q_register;
   666.                   set_offline : in Boolean) is
   667.    begin
   668.       the_deck.PIF(Q_operand, set_offline);
   669.    end PIH;
   670.
   671.    procedure find_start_of_later_block (the_deck : in out MT.deck;
   672.                                         crossed  : in out length_in_frames) is
   673.       the_slice  : MT.slice := a_NULL_slice;
   674.       block_size : length_in_frames := 0;
   675.    begin
   676.       -- Skip over any erasures or tape marks.
   677.       skip_forward_over_erasure(the_deck, the_slice, crossed, caller => "find_start_of_later_block");
   678.       crossed := crossed + the_deck.inter_block_gap;
   679.
   680.       if not the_slice.is_first then
   681.          raise emulation_failure
   682.             with "find_start_of_later_block at slice"
   683.                & Positive_Count'Image(the_deck.tape.position)
   684.                & " of "
   685.                & the_deck.device_name
   686.                & " failed to locate the first slice of a block";
   687.       end if;
   688.
   689.       -- We have reached the first slice of the block.
   690.       if the_deck.kind = ST_kind and the_slice.kind in tape_mark_kind then
   691.          block_size := 1;
   692.       else
   693.          block_size := the_slice.size;
   694.          -- Ignore data slices until we get to the last slice of the block.
   695.          while not the_slice.is_last loop
   696.             read_next_slice(the_deck.tape, the_slice);
   697.             block_size := block_size + the_slice.size;
   698.          end loop;
   699.          handle_any_abnormality(the_deck, block_size);
   700.       end if;
   701.
   702.       the_deck.is_LBM_flagged := the_slice.is_LBM_flagged;
   703.       crossed := crossed + block_size;
   704.    exception
   705.       when end_of_tape =>
   706.          the_deck.is_abnormal := True;
   707.          raise end_of_tape with "find_start_of_later_block";
   708.    end find_start_of_later_block;
   709.
   710.    procedure skip_forwards (the_deck       : in out MT.deck;
   711.                             blocks_skipped : in KDF9.word) is
   712.       crossed : length_in_frames := 0;
   713.    begin
   714.       for i in 1 .. blocks_skipped loop
   715.          find_start_of_later_block(the_deck, crossed);
   716.       -- MFSKQq stops at an LBM-flagged block, or on count expiry.
   717.       -- Unlike MBSKQq it does record having seen an LBM-flagged block during the skipping.
   718.       -- See the Manual, §22.1.3, p.183, ¶1 and §22.1.9, p.188, ¶-2.
   719.       exit when the_deck.is_LBM_flagged;
   720.       end loop;
   721.       note_tape_position(the_deck, forwards, crossed, bytes_moved => 0);
   722.       update_statistics(the_deck, crossed, bytes_moved => 0);
   723.    end skip_forwards;
   724.
   725.    -- MFSKQq
   726.    overriding
   727.    procedure PMA (the_deck    : in out MT.deck;
   728.                   Q_operand   : in KDF9.Q_register;
   729.                   set_offline : in Boolean) is
   730.    begin
   731.       start_data_transfer(the_deck, Q_operand, set_offline, 19, is_DMAing => False);
   732.       if Q_operand.M = 0 then
   733.          skip_forwards(the_deck, 32768);  -- See Manual §22.1.9, p188, ¶1.
   734.       else
   735.          require_positive_count(Q_operand.M);
   736.          skip_forwards(the_deck, KDF9.word(Q_operand.M));
   737.       end if;
   738.    end PMA;
   739.
   740.    -- MBTQq
   741.    overriding
   742.    procedure PMB (the_deck    : in out MT.deck;
   743.                   Q_operand   : in KDF9.Q_register;
   744.                   set_offline : in Boolean) is
   745.    begin
   746.       validate_device(the_deck, Q_operand);
   747.       validate_parity(the_deck);
   748.       deal_with_a_busy_device(the_deck, 14, set_offline);
   749.       the_T_bit_is_set := the_deck.is_at_BTW and the_deck.is_loaded;
   750.       take_note_of_test(the_deck.device_name, Q_operand, the_T_bit_is_set);
   751.    end PMB;
   752.
   753.    -- MLBQq
   754.    overriding
   755.    procedure PMC (the_deck    : in out MT.deck;
   756.                   Q_operand   : in KDF9.Q_register;
   757.                   set_offline : in Boolean) is
   758.    begin
   759.       validate_device(the_deck, Q_operand);
   760.       validate_parity(the_deck);
   761.       deal_with_a_busy_device(the_deck, 14, set_offline);
   762.       the_T_bit_is_set := the_deck.is_LBM_flagged;
   763.       the_deck.is_LBM_flagged := False;
   764.       take_note_of_test(the_deck.device_name, Q_operand, the_T_bit_is_set);
   765.    end PMC;
   766.
   767.    procedure skip_backwards (the_deck       : in out MT.deck;
   768.                              blocks_skipped : in KDF9.word) is
   769.       crossed : length_in_frames := 0;
   770.    begin
   771.       for i in 1 .. blocks_skipped loop
   772.       exit when the_deck.is_at_BTW;  -- I.e., the tape is fully rewound.
   773.          find_start_of_earlier_block(the_deck, crossed);
   774.       -- MBSKQq does not stop at an LBM-flagged block, only at BTW or count expiry.
   775.       -- It ignores LBM flags encountered during the skipping.
   776.       -- See the Manual, §22.1.3, p.183, ¶1 and §22.1.9, p.188, ¶-2.
   777.       end loop;
   778.       note_tape_position(the_deck, backwards, crossed, bytes_moved => 0);
   779.       update_statistics(the_deck, crossed, bytes_moved => 0);
   780.    end skip_backwards;
   781.
   782.    -- MRWDQq
   783.    overriding
   784.    procedure PMD (the_deck    : in out MT.deck;
   785.                   Q_operand   : in KDF9.Q_register;
   786.                   set_offline : in Boolean) is
   787.       byte_count,
   788.       tape_length : length_in_frames := 0;
   789.       the_slice   : MT.slice;
   790.    begin  -- PMD
   791.       the_deck.is_abnormal := False;  -- See Manual §22.1.9, p.189, ¶-2.
   792.       start_data_transfer(the_deck, Q_operand, set_offline, 19, is_DMAing => False);
   793.       take_note_of_test(the_deck.device_name, Q_operand, the_deck.is_at_BTW);
   794.       -- No motion takes place if the tape is at BTW; see Manual §22.1.9, p.190, ¶1.
   795.       if the_deck.tape.position > 0 then
   796.          -- Make sure we dont try to read past the end of data.
   797.          -- Spool back to the BTW, accumulating distances.
   798.          while the_deck.tape.position > 0 loop
   799.             read_prev_slice(the_deck.tape, the_slice);
   800.             case the_slice.kind is
   801.                when data_slice =>
   802.                   byte_count := byte_count + the_slice.size;
   803.                   if the_slice.is_first then
   804.                      tape_length := tape_length + the_deck.inter_block_gap;
   805.                   end if;
   806.                when GAP_slice
   807.                   | WIPE_slice =>
   808.                   tape_length := tape_length + the_slice.size;
   809.                when others =>
   810.                   null;
   811.             end case;
   812.          end loop;
   813.       else
   814.          -- No motion takes place; see Manual §22.1.9, p.190, ¶1.
   815.          null;
   816.       end if;
   817.
   818.       update_statistics(the_deck, tape_length + byte_count, bytes_moved => 0);
   819.
   820.       if not the_deck.is_at_BTW then
   821.          raise emulation_failure
   822.             with "not at BTW after rewinding "
   823.                & the_deck.device_name
   824.                & " at slice"
   825.                & the_deck.tape.position'Image;
   826.       end if;
   827.       reset(the_deck);
   828.    end PMD;
   829.
   830.    -- MBSKQq
   831.    overriding
   832.    procedure PME (the_deck    : in out MT.deck;
   833.                   Q_operand   : in KDF9.Q_register;
   834.                   set_offline : in Boolean) is
   835.    begin
   836.       if the_deck.is_at_BTW then
   837.          trap_invalid_instruction("tried MBSKQq at BTW");
   838.       end if;
   839.       start_data_transfer(the_deck, Q_operand, set_offline, 19, is_DMAing => False);
   840.       if Q_operand.M = 0 then
   841.          skip_backwards(the_deck, 32768);  -- See Manual §22.1.9, p188, ¶1.
   842.       else
   843.          require_positive_count(Q_operand.M);
   844.          skip_backwards(the_deck, KDF9.word(Q_operand.M));
   845.       end if;
   846.    end PME;
   847.
   848.    -- METQq
   849.    overriding
   850.    procedure PMF (the_deck    : in out MT.deck;
   851.                   Q_operand   : in KDF9.Q_register;
   852.                   set_offline : in Boolean) is
   853.    begin
   854.       validate_device(the_deck, Q_operand);
   855.       validate_parity(the_deck);
   856.       deal_with_a_busy_device(the_deck, 13, set_offline);
   857.       the_T_bit_is_set := the_deck.is_at_ETW;
   858.       take_note_of_test(the_deck.device_name, Q_operand, the_T_bit_is_set);
   859.    end PMF;
   860.
   861.    -- PMKQq, forward skip, even parity, for character data with "group mark" (8#77#)
   862.    overriding
   863.    procedure PMK (the_deck    : in out MT.deck;
   864.                   Q_operand   : in KDF9.Q_register;
   865.                   set_offline : in Boolean) is
   866.    begin
   867.       if the_deck.kind = MT_kind then
   868.          trap_invalid_instruction("a 7-track forward skip was attempted on a 1081 deck");
   869.       else
   870.          the_deck.PMA(Q_operand, set_offline);
   871.       end if;
   872.    end PMK;
   873.
   874.    -- PMLQq, backward skip, even parity, for character data with "group mark" (8#77#)
   875.    overriding
   876.    procedure PML (the_deck    : in out MT.deck;
   877.                   Q_operand   : in KDF9.Q_register;
   878.                   set_offline : in Boolean) is
   879.    begin
   880.       if the_deck.kind = MT_kind then
   881.          trap_invalid_instruction("a 7-track backward skip was attempted on a 1081 deck");
   882.       else
   883.          the_deck.PMB(Q_operand, set_offline);
   884.       end if;
   885.    end PML;
   886.
   887.    procedure put_data_slice (the_deck   : in out MT.deck;
   888.                              data       : in MT.data_storage;
   889.                              size       : in length_in_frames;
   890.                              is_first,
   891.                              is_last,
   892.                              is_flagged : in Boolean) is
   893.       the_slice : MT.slice;
   894.    begin
   895.       the_slice := (
   896.                     data_slice,
   897.                     is_LBM_flagged => is_flagged,
   898.                     is_first => put_data_slice.is_first,
   899.                     is_last  => put_data_slice.is_last,
   900.                     size     => put_data_slice.size,
   901.                     data     => erased_gap_data
   902.                    );
   903.       the_slice.data(1 .. put_data_slice.size) := put_data_slice.data;
   904.       write_slice(the_deck.tape, the_slice);
   905.    exception
   906.       when end_of_tape =>
   907.          trap_invalid_operand("attempt to write past the PET on " & the_deck.device_name);
   908.    end put_data_slice;
   909.
   910.    procedure write_block (the_deck       : in out MT.deck;
   911.                           the_data       : in MT.data_storage;
   912.                           is_LBM_flagged : in Boolean) is
   913.       remnant  : length_in_frames := the_data'Length;
   914.       from     : length_in_frames;
   915.       the_size : length_in_frames;
   916.    begin
   917.       if not the_deck.tape.has_a_WP_ring then
   918.          trap_operator_error("attempt to write to "
   919.                            & the_deck.device_name
   920.                            & " without a Write Permit Ring");
   921.       end if;
   922.
   923.       check_for_writing_past_PET(the_deck, "write");
   924.
   925.       the_deck.is_LBM_flagged := False;
   926.
   927.       -- Write the first (and possibly final) slice of the block.
   928.       the_size := (if remnant > slice_size_limit then slice_size_limit else remnant);
   929.       remnant := remnant - the_size;
   930.       from := the_data'First;
   931.       put_data_slice (
   932.                       the_deck,
   933.                       the_data(from .. the_size),
   934.                       the_size,
   935.                       is_first   => True,
   936.                       is_last    => remnant = 0,
   937.                       is_flagged => write_block.is_LBM_flagged
   938.                      );
   939.
   940.       -- Write any full slices, the last of which may be final.
   941.       while remnant >= slice_size_limit loop
   942.          check_for_writing_past_PET(the_deck, "write");
   943.          remnant := remnant - slice_size_limit;
   944.          from := from + slice_size_limit;
   945.          put_data_slice (
   946.                          the_deck,
   947.                          the_data(from .. from+slice_size_limit-1),
   948.                          slice_size_limit,
   949.                          is_first   => False,
   950.                          is_last    => remnant = 0,
   951.                          is_flagged => write_block.is_LBM_flagged
   952.                         );
   953.       end loop;
   954.
   955.       -- Write the residue as a final slice of the block.
   956.       if remnant > 0 then
   957.          put_data_slice (
   958.                          the_deck,
   959.                          the_data(from+slice_size_limit .. the_data'Last),
   960.                          remnant,
   961.                          is_first   => False,
   962.                          is_last    => True,
   963.                          is_flagged => write_block.is_LBM_flagged
   964.                         );
   965.       end if;
   966.
   967.       note_tape_position(the_deck, forwards,
   968.                         the_deck.inter_block_gap, bytes_moved => the_data'Length);
   969.       update_statistics(the_deck,
   970.                         the_deck.inter_block_gap, bytes_moved => the_data'Length);
   971.
   972.    exception
   973.       when end_of_tape =>
   974.          trap_invalid_operand("attempt to write past the PET on " & the_deck.device_name);
   975.    end write_block;
   976.
   977.    procedure write (the_deck       : in out MT.deck;
   978.                     Q_operand      : in KDF9.Q_register;
   979.                     is_LBM_flagged : in Boolean := False) is
   980.       start_address : constant KDF9.address := Q_operand.I;
   981.       end_address   : constant KDF9.address := Q_operand.M;
   982.    begin
   983.       validate_device(the_deck, Q_operand);
   984.       check_addresses_and_lockouts(start_address, end_address);
   985.       declare
   986.          next_byte : length_in_frames := 1;
   987.          the_data  : MT.data_storage(1 .. length_in_frames(end_address-start_address+1)*8);
   988.       begin
   989.       word_loop:
   990.          for w in start_address .. end_address loop
   991.             for c in KDF9_char_sets.symbol_index'Range loop
   992.                the_data(next_byte) := TP_CN(fetch_symbol(w, c));
   993.                next_byte := next_byte + 1;
   994.             end loop;
   995.          end loop word_loop;
   996.          write_block(the_deck, the_data, is_LBM_flagged);
   997.       end;
   998.    end write;
   999.
  1000.    procedure write_to_terminator (the_deck       : in out MT.deck;
  1001.                                   Q_operand      : in KDF9.Q_register;
  1002.                                   is_LBM_flagged : in Boolean := False) is
  1003.       start_address : constant KDF9.address := Q_operand.I;
  1004.       end_address   : constant KDF9.address := Q_operand.M;
  1005.    begin
  1006.       validate_device(the_deck, Q_operand);
  1007.       check_addresses_and_lockouts(start_address, end_address);
  1008.       declare
  1009.          next_byte : length_in_frames := 1;
  1010.          the_data  : MT.data_storage(1 .. length_in_frames(end_address-start_address+1)*8);
  1011.          symbol    : KDF9_char_sets.symbol;
  1012.       begin
  1013.       word_loop:
  1014.          for w in start_address .. end_address loop
  1015.             for c in KDF9_char_sets.symbol_index'Range loop
  1016.                symbol := fetch_symbol(w, c);
  1017.                the_data(next_byte) := TP_CN(symbol);
  1018.                next_byte := next_byte + 1;
  1019.          exit word_loop when symbol = the_deck.terminator;
  1020.             end loop;
  1021.          end loop word_loop;
  1022.          if the_deck.kind = MT_kind then
  1023.             -- Pad out the last word to a full 8 symbols; 7-track decks do not do this.
  1024.             while next_byte mod 8 /= 1 loop
  1025.                the_data(next_byte) := TP_CN(0);
  1026.                next_byte := next_byte + 1;
  1027.             end loop;
  1028.          end if;
  1029.          write_block(the_deck, the_data(1 .. next_byte-1), is_LBM_flagged);
  1030.          correct_transfer_time(the_deck, KDF9.word(next_byte-1));
  1031.       end;
  1032.    end write_to_terminator;
  1033.
  1034.    -- MWQq
  1035.    overriding
  1036.    procedure POA (the_deck    : in out MT.deck;
  1037.                   Q_operand   : in KDF9.Q_register;
  1038.                   set_offline : in Boolean) is
  1039.    begin
  1040.       start_data_transfer(the_deck, Q_operand, set_offline, 22 + MT_IO_time(the_deck, Q_operand));
  1041.       write(the_deck, Q_operand);
  1042.       lock_out_relative_addresses(Q_operand);
  1043.    end POA;
  1044.
  1045.    -- MWEQq
  1046.    overriding
  1047.    procedure POB (the_deck    : in out MT.deck;
  1048.                   Q_operand   : in KDF9.Q_register;
  1049.                   set_offline : in Boolean) is
  1050.    begin
  1051.       start_data_transfer(the_deck, Q_operand, set_offline, 22 + MT_IO_time(the_deck, Q_operand));
  1052.       write_to_terminator(the_deck, Q_operand);
  1053.       lock_out_relative_addresses(Q_operand);
  1054.    end POB;
  1055.
  1056.    procedure put_ST_tapemark_slice (the_deck    : in out MT.deck;
  1057.                                     Q_operand   : in KDF9.Q_register;
  1058.                                     set_offline : in Boolean;
  1059.                                     the_slice   : in MT.slice) is
  1060.       timing_Q : constant KDF9.Q_register := (Q_operand.C, 0, 0);
  1061.    begin
  1062.       start_data_transfer(the_deck, Q_operand, set_offline, 22 + MT_IO_time(the_deck, timing_Q));
  1063.       write_slice(the_deck.tape, the_slice);
  1064.    exception
  1065.       when end_of_tape =>
  1066.          trap_invalid_operand("attempt to write past the PET on " & the_deck.device_name);
  1067.    end put_ST_tapemark_slice;
  1068.
  1069.    -- MLWQq
  1070.    overriding
  1071.    procedure POC (the_deck    : in out MT.deck;
  1072.                   Q_operand   : in KDF9.Q_register;
  1073.                   set_offline : in Boolean) is
  1074.    begin
  1075.       if the_deck.kind = MT_kind then
  1076.          start_data_transfer(the_deck, Q_operand, set_offline, 22 + MT_IO_time(the_deck, Q_operand));
  1077.          write(the_deck, Q_operand, is_LBM_flagged => True);
  1078.          lock_out_relative_addresses(Q_operand);
  1079.       else
  1080.          put_ST_tapemark_slice(the_deck, Q_operand, set_offline, odd_parity_tape_mark);
  1081.       end if;
  1082.    end POC;
  1083.
  1084.    -- MLWEQq
  1085.    overriding
  1086.    procedure POD (the_deck    : in out MT.deck;
  1087.                   Q_operand   : in KDF9.Q_register;
  1088.                   set_offline : in Boolean) is
  1089.    begin
  1090.       if the_deck.kind = MT_kind then
  1091.          start_data_transfer(the_deck, Q_operand, set_offline, 22 + MT_IO_time(the_deck, Q_operand));
  1092.          write_to_terminator(the_deck, Q_operand, is_LBM_flagged => True);
  1093.          lock_out_relative_addresses(Q_operand);
  1094.       else
  1095.          put_ST_tapemark_slice(the_deck, Q_operand, set_offline, even_parity_tape_mark);
  1096.       end if;
  1097.    end POD;
  1098.
  1099.    procedure erase_tape_gap (the_deck   : in out MT.deck;
  1100.                              the_length : in KDF9.Q_part; -- the_length is a number of words.
  1101.                              gap_kind   : in tape_gap_kind) is
  1102.       crossing  : constant length_in_frames := length_in_frames(the_length) * 8;
  1103.       the_slice : MT.slice := (if gap_kind = GAP_slice then a_GAP_slice else a_WIPE_slice);
  1104.       remnant   : length_in_frames := crossing;
  1105.       old_slice : MT.slice;
  1106.       the_size  : length_in_frames;
  1107.    begin
  1108.       loop
  1109.          check_for_writing_past_PET(the_deck, "erase");
  1110.          the_size := length_in_frames'Min(remnant, slice_size_limit);
  1111.          remnant  := remnant - the_size;
  1112.
  1113.          the_slice.size := the_size;
  1114.
  1115.          if gap_kind = GAP_slice  and then
  1116.                not the_deck.is_at_EOD then
  1117.             -- Safety rules apply to erasing gaps; see the Manual, Appendix 6.8, p.314.
  1118.             read_next_slice(the_deck.tape, old_slice);
  1119.             if old_slice.kind /= WIPE_slice then
  1120.                trap_invalid_operand("a GAP of length"
  1121.                                   & the_length'Image
  1122.                                   & " words would overwrite data at slice"
  1123.                                   & the_deck.tape.position'Image
  1124.                                   & " of "
  1125.                                   & the_deck.device_name
  1126.                                    );
  1127.             end if;
  1128.             -- Restore the writing position.
  1129.             read_prev_slice(the_deck.tape, old_slice);
  1130.          end if;
  1131.
  1132.          write_slice(the_deck.tape, the_slice);
  1133.       exit when remnant = 0;
  1134.       end loop;
  1135.
  1136.       the_deck.is_LBM_flagged := False;
  1137.       note_tape_position(the_deck, forwards, crossing, bytes_moved => 0);
  1138.       update_statistics(the_deck, crossing, bytes_moved => 0);
  1139.    exception
  1140.       when end_of_tape =>
  1141.          trap_invalid_operand("attempt to WIPE/GAP past the PET on" & the_deck.device_name);
  1142.    end erase_tape_gap;
  1143.
  1144.    -- MGAPQq
  1145.    overriding
  1146.    procedure POE (the_deck    : in out MT.deck;
  1147.                   Q_operand   : in KDF9.Q_register;
  1148.                   set_offline : in Boolean) is
  1149.    begin
  1150.       if not the_deck.tape.has_a_WP_ring then
  1151.          trap_operator_error("attempt to GAP "
  1152.                            & the_deck.device_name
  1153.                            & " without a Write Permit Ring");
  1154.       end if;
  1155.       require_positive_count(Q_operand.M);
  1156.       start_data_transfer(the_deck, Q_operand, set_offline,
  1157.                           19+IO_elapsed_time(the_deck, KDF9.word(Q_operand.M)),
  1158.                           is_DMAing => False);
  1159.       erase_tape_gap(the_deck, Q_operand.M, gap_kind => GAP_slice);
  1160.    end POE;
  1161.
  1162.    -- MWIPEQq
  1163.    overriding
  1164.    procedure POF (the_deck    : in out MT.deck;
  1165.                   Q_operand   : in KDF9.Q_register;
  1166.                   set_offline : in Boolean) is
  1167.    begin
  1168.       if not the_deck.tape.has_a_WP_ring then
  1169.          trap_operator_error("attempt to WIPE "
  1170.                            & the_deck.device_name
  1171.                            & " without a Write Permit Ring");
  1172.       end if;
  1173.       require_positive_count(Q_operand.M);
  1174.       start_data_transfer(the_deck, Q_operand, set_offline,
  1175.                           19+IO_elapsed_time(the_deck, KDF9.word(Q_operand.M)),
  1176.                           is_DMAing => False);
  1177.       erase_tape_gap(the_deck, Q_operand.M, gap_kind => WIPE_slice);
  1178.    end POF;
  1179.
  1180.    -- The following procedures support the TSD Director OUT4/OUT10 API emulations.
  1181.
  1182.    procedure find_tape (the_label  : in  MT.data_storage;
  1183.                         its_number : out KDF9.buffer_number;
  1184.                         its_serial : out KDF9.word;
  1185.                         requestor  : in  String) is
  1186.
  1187.       function as_word (the_serial : MT.data_storage)
  1188.       return KDF9.word is
  1189.          word : KDF9.word := 0;
  1190.       begin
  1191.          for b in the_serial'Range loop
  1192.             word := (word * 2**6) or KDF9.word(CN_TR(the_serial(b)));
  1193.          end loop;
  1194.          return word;
  1195.       end as_word;
  1196.
  1197.       the_block : MT.data_storage(1 .. max_block_size);
  1198.       the_size  : length_in_frames;
  1199.
  1200.    begin -- find_tape
  1201.       if the_label'Length < 1 then
  1202.          trap_invalid_operand("given a null label by " & requestor);
  1203.       end if;
  1204.       for t in KDF9.buffer_number loop
  1205.          if buffer(t) /= null                      and then
  1206.                buffer(t).kind in MT_kind | ST_kind and then
  1207.                    is_unallocated(buffer(t))           then
  1208.             declare
  1209.                the_deck : MT.deck renames MT.deck(buffer(t).all);
  1210.             begin
  1211.                if the_deck.is_loaded and then
  1212.                      the_deck.is_at_BTW    then
  1213.                   -- Read the label.
  1214.                   -- After reading the label the tape must be set back to BTW,
  1215.                   -- as is required to emulate Director; see the Manual, §22.1, Ex. 1.
  1216.                   read_block(the_deck, the_block, the_size);
  1217.                   reset(the_deck);
  1218.                   if the_size >= 8+the_label'Length                and then
  1219.                         the_block(9 .. 8+the_label'Length) = the_label then
  1220.                      its_number := t;
  1221.                      its_serial := as_word(the_block(1 .. 8));
  1222.                      return;
  1223.                   end if;
  1224.                end if;
  1225.             end;
  1226.          end if;
  1227.       end loop;
  1228.       trap_operator_error("the MT labelled '" & String(the_label) & "' has not been mounted");
  1229.    end find_tape;
  1230.
  1231.    procedure find_tape_labelled (the_label  : in  MT.long_label;
  1232.                                  its_number : out KDF9.buffer_number;
  1233.                                  its_serial : out KDF9.word) is
  1234.    begin
  1235.       find_tape(MT.data_storage(the_label), its_number, its_serial, "OUT 10");
  1236.    end find_tape_labelled;
  1237.
  1238.    procedure find_tape_labelled (the_label  : in  MT.short_label;
  1239.                                  its_number : out KDF9.buffer_number;
  1240.                                  its_serial : out KDF9.word) is
  1241.    begin
  1242.       find_tape(MT.data_storage(the_label), its_number, its_serial, "OUT 4");
  1243.    end find_tape_labelled;
  1244.
  1245.    -- Rewind decks, on problem program termination, as would happen under Director.
  1246.    procedure dispose_all_allocated_tapes is
  1247.    begin
  1248.       for b in KDF9.buffer_number loop
  1249.          if buffer(b) /= null                      and then
  1250.                buffer(b).kind in MT_kind | ST_kind and then
  1251.                    not is_unallocated(buffer(b))       then
  1252.             declare
  1253.                the_deck : MT.deck renames MT.deck(buffer(b).all);
  1254.             begin
  1255.                if the_deck.is_loaded then
  1256.                   PMD(the_deck, (b, 0, 0), set_offline => False);
  1257.                end if;
  1258.             end;
  1259.          end if;
  1260.       end loop;
  1261.    end dispose_all_allocated_tapes;
  1262.
  1263.    overriding
  1264.    procedure Finalize (the_deck : in out MT.deck) is
  1265.       the_deck_was_used : constant Boolean   := the_deck.bytes_moved /= 0 or not the_deck.is_at_BTW;
  1266.    begin
  1267.       if the_deck.is_open then
  1268.          if (the_final_state_is_wanted and the_log_is_wanted) and then
  1269.                the_deck_was_used                                  then
  1270.             log_line(
  1271.                      the_deck.device_name
  1272.                    & " on buffer #"
  1273.                    & oct_of(KDF9.Q_part(the_deck.number), 2)
  1274.                    & " transferred"
  1275.                    & the_deck.bytes_moved'Image
  1276.                    & " characters"
  1277.                    & (
  1278.                       if    the_deck.is_at_PET then ", and is now at PET."
  1279.                       elsif the_deck.is_at_ETW then ", and is now at ETW."
  1280.                       else                          "."
  1281.                      )
  1282.                    );
  1283.          end if;
  1284.          close(the_deck.tape);
  1285.       end if;
  1286.    exception
  1287.       when error : others =>
  1288.          raise emulation_failure
  1289.             with "Finalize error for MT buffer #"
  1290.                & oct_of(KDF9.Q_part(the_deck.number), 2)
  1291.                & Ada.Exceptions.Exception_Message(error);
  1292.    end Finalize;
  1293.
  1294.
  1295.    MT_quantum : constant := 1E6 / 40E3;  -- for 40_000 characters per second.
  1296.    ST_quantum : constant := 1E6 / 15E3;  -- for 15_000 characters per second.
  1297.
  1298.    type MT_access is access MT.deck;
  1299.    MT_deck         : array (IOC.unit_number range 0..7) of MT_access with Warnings => Off;
  1300.
  1301.    MT_units : IOC.unit_number := 0;
  1302.    ST_units : IOC.unit_number := 0;
  1303.
  1304.    procedure enable_MT_deck (b : in KDF9.buffer_number) is
  1305.    begin
  1306.       if MT_units+ST_units > MT_deck'Last then
  1307.          trap_operator_error("too many tape decks specified");
  1308.       end if;
  1309.       MT_deck(MT_units) := new deck (number  => b,
  1310.                                      kind    => MT_kind,
  1311.                                      unit    => MT_units,
  1312.                                      quantum => MT_quantum);
  1313.       MT_units := MT_units + 1;
  1314.    end enable_MT_deck;
  1315.
  1316.    procedure enable_ST_deck (b : in KDF9.buffer_number) is
  1317.    begin
  1318.       if ST_units >= 2 then
  1319.          trap_operator_error("more than 2 ST decks specified");
  1320.       end if;
  1321.       if MT_units+ST_units > MT_deck'Last then
  1322.          trap_operator_error("too many tape decks specified");
  1323.       end if;
  1324.       MT_deck(MT_units) := new deck (number  => b,
  1325.                                      kind    => ST_kind,
  1326.                                      unit    => ST_units,
  1327.                                      quantum => ST_quantum);
  1328.       ST_units := ST_units + 1;
  1329.       MT_units := MT_units + 1;
  1330.    end enable_ST_deck;
  1331.
  1332. end IOC.fast.MT;

Compiling: ../Source/ioc-fast-mt.ads
Source file time stamp: 2020-11-03 02:20:51
Compiled at: 2020-11-12 18:12:12

     1. -- ioc-fast-MT.ads
     2. --
     3. -- Emulation of magnetic tape decks and buffers.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with KDF9_char_sets;
    20.
    21. private with Ada.Direct_IO;
    22. --
    23. private with magtape_data;
    24.
    25. use  KDF9_char_sets;
    26.
    27. package IOC.fast.MT is
    28.
    29.    -- Both the EE 1081 and the Ampex TM-4 decks are emulated.
    30.    --
    31.    -- EE 1081, 16-track tape deck.
    32.       -- The physical characteristics of the deck are taken from the Manual, §22.1.2, i.e.:
    33.       -- 0.3 inch interblock gap, 400 ch/inch density, 100 inch/s tape speed,
    34.       --    full-reel rewind time ~3 minutes.
    35.    --
    36.    -- Ampex TM-4, 7-track IBM-compatible, tape deck.
    37.       -- The physical characteristics of the deck are taken from the Ampex document:
    38.       --    TECHNICAL MANUAL FOR SDSTM-4 TAPE TRANSPORT of 1963/2/15.
    39.       -- Where alternative characteristics are described, this code uses those considered to be of
    40.       --    greatest data interchange compatibility, as this is how the deck was used on KDF9, i.e.:
    41.       -- 0.3 inch interblock gap, 200 ch/inch density, 75 inch/s tape speed,
    42.       --    full-reel rewind time ~3 minutes.
    43.    --
    44.
    45.    type deck is new IOC.fast.device with private;
    46.
    47.    -- MRFQq
    48.    overriding
    49.    procedure PIA (the_deck    : in out MT.deck;
    50.                   Q_operand   : in KDF9.Q_register;
    51.                   set_offline : in Boolean);
    52.
    53.    -- MFREQq
    54.    overriding
    55.    procedure PIB (the_deck    : in out MT.deck;
    56.                   Q_operand   : in KDF9.Q_register;
    57.                   set_offline : in Boolean);
    58.
    59.    -- as PIA
    60.    overriding
    61.    procedure PIC (the_deck    : in out MT.deck;
    62.                   Q_operand   : in KDF9.Q_register;
    63.                   set_offline : in Boolean);
    64.
    65.    -- as PID
    66.    overriding
    67.    procedure PID (the_deck    : in out MT.deck;
    68.                   Q_operand   : in KDF9.Q_register;
    69.                   set_offline : in Boolean);
    70.
    71.    -- MBRQq
    72.    overriding
    73.    procedure PIE (the_deck    : in out MT.deck;
    74.                   Q_operand   : in KDF9.Q_register;
    75.                   set_offline : in Boolean);
    76.
    77.    -- MBREQq
    78.    overriding
    79.    procedure PIF (the_deck    : in out MT.deck;
    80.                   Q_operand   : in KDF9.Q_register;
    81.                   set_offline : in Boolean);
    82.
    83.    -- as PIE
    84.    overriding
    85.    procedure PIG (the_deck    : in out MT.deck;
    86.                   Q_operand   : in KDF9.Q_register;
    87.                   set_offline : in Boolean);
    88.
    89.    -- as PIF
    90.    overriding
    91.    procedure PIH (the_deck    : in out MT.deck;
    92.                   Q_operand   : in KDF9.Q_register;
    93.                   set_offline : in Boolean);
    94.
    95.    -- MFSKQq, for odd parity on 7-track deck
    96.    overriding
    97.    procedure PMA (the_deck    : in out MT.deck;
    98.                   Q_operand   : in KDF9.Q_register;
    99.                   set_offline : in Boolean);
   100.
   101.    -- MBTQq
   102.    overriding
   103.    procedure PMB (the_deck    : in out MT.deck;
   104.                   Q_operand   : in KDF9.Q_register;
   105.                   set_offline : in Boolean);
   106.
   107.    -- MLBQq
   108.    overriding
   109.    procedure PMC (the_deck    : in out MT.deck;
   110.                   Q_operand   : in KDF9.Q_register;
   111.                   set_offline : in Boolean);
   112.    -- MRWDQq
   113.    overriding
   114.    procedure PMD (the_deck    : in out MT.deck;
   115.                   Q_operand   : in KDF9.Q_register;
   116.                   set_offline : in Boolean);
   117.
   118.    -- MBSKQqMFSKQq, for odd parity on 7-track deck
   119.    overriding
   120.    procedure PME (the_deck    : in out MT.deck;
   121.                   Q_operand   : in KDF9.Q_register;
   122.                   set_offline : in Boolean);
   123.
   124.    -- METQq
   125.    overriding
   126.    procedure PMF (the_deck    : in out MT.deck;
   127.                   Q_operand   : in KDF9.Q_register;
   128.                   set_offline : in Boolean);
   129.
   130.    -- PMKQq, forward skip, even parity, for 7-track deck only
   131.    overriding
   132.    procedure PMK (the_deck    : in out MT.deck;
   133.                   Q_operand   : in KDF9.Q_register;
   134.                   set_offline : in Boolean);
   135.
   136.    -- PMLQq, backward skip, even parity, for 7-track deck only
   137.    overriding
   138.    procedure PML (the_deck    : in out MT.deck;
   139.                   Q_operand   : in KDF9.Q_register;
   140.                   set_offline : in Boolean);
   141.
   142.    -- MWQq
   143.    overriding
   144.    procedure POA (the_deck    : in out MT.deck;
   145.                   Q_operand   : in KDF9.Q_register;
   146.                   set_offline : in Boolean);
   147.
   148.    -- MWEQq
   149.    overriding
   150.    procedure POB (the_deck    : in out MT.deck;
   151.                   Q_operand   : in KDF9.Q_register;
   152.                   set_offline : in Boolean);
   153.
   154.    -- MLWQq
   155.    overriding
   156.    procedure POC (the_deck    : in out MT.deck;
   157.                   Q_operand   : in KDF9.Q_register;
   158.                   set_offline : in Boolean);
   159.
   160.    -- MLWEQq
   161.    overriding
   162.    procedure POD (the_deck    : in out MT.deck;
   163.                   Q_operand   : in KDF9.Q_register;
   164.                   set_offline : in Boolean);
   165.
   166.    -- MGAPQq
   167.    overriding
   168.    procedure POE (the_deck    : in out MT.deck;
   169.                   Q_operand   : in KDF9.Q_register;
   170.                   set_offline : in Boolean);
   171.
   172.    -- MWIPEQq
   173.    overriding
   174.    procedure POF (the_deck    : in out MT.deck;
   175.                   Q_operand   : in KDF9.Q_register;
   176.                   set_offline : in Boolean);
   177.
   178.    procedure enable_MT_deck (b : in KDF9.buffer_number);
   179.
   180.    procedure enable_ST_deck (b : in KDF9.buffer_number);
   181.
   182.    -- The following support the emulation of OUTs 4 and 10.
   183.
   184.    type short_label is new String(1 .. 8);
   185.    type long_label  is new String(1 .. 16);
   186.
   187.    procedure find_tape_labelled (the_label  : in MT.short_label;
   188.                                  its_number : out KDF9.buffer_number;
   189.                                  its_serial : out KDF9.word);
   190.
   191.    procedure find_tape_labelled (the_label  : in MT.long_label;
   192.                                  its_number : out KDF9.buffer_number;
   193.                                  its_serial : out KDF9.word);
   194.
   195.    -- Rewind decks, on problem program termination, as would happen under Director.
   196.    procedure dispose_all_allocated_tapes;
   197.
   198. private
   199.
   200.    use magtape_data;
   201.
   202.    -- slice_size_limit must be set so that the slice size field fits into 1 byte,
   203.    --    thus avoiding endian-ness and portability issues.
   204.    pragma Compile_Time_Error (slice_size_limit > 255, "magtape_data.slice_size_limit > 255");
   205.
   206.    -- I think that both types of tape for the KDF9 had a maximum reel length of 2400 feet.
   207.    -- I assume that the recording density of the 7-track deck was no greater than that of the 1081.
   208.
   209.    max_bits_per_inch      : constant := 400;
   210.    max_reel_length        : constant := 12 * 2400;
   211.    type length_in_frames is range 0 .. max_reel_length * max_bits_per_inch;
   212.    type data_storage     is array (MT.length_in_frames range <>) of Character;
   213.
   214.    -- Attempts to write a block of more than max_block_size/8 words will be rejected.
   215.    -- The largest recommended size, as stated in the Manual, §22.1.3, is 3000 words.
   216.    -- The present value cannot logically be exceeded, and so allows all possible usages.
   217.
   218.    max_block_size         : constant := 32768 * 8;
   219.    subtype block_range   is MT.length_in_frames range 0 .. max_block_size;
   220.    subtype block_storage is data_storage (MT.block_range range 1 .. max_block_size);
   221.
   222.    -- A data block consists of one or more slices:
   223.    --
   224.    -- 1. a block of data length <= slice_size_limit has 1 slice, with (is_last and is_first) = True;
   225.    --
   226.    -- 2. a longer block has 1 or more prior slices, which all have data length = slice_size_limit,
   227.    --       all of them having is_last = False, and the first of them having is_first = True;
   228.    --    and 1 final slice of data length <= slice_size_limit, with is_last = True.
   229.    --
   230.    -- The total data length of all the slices in a block is <= max_block_size.
   231.    --
   232.    -- GAP and WIPE slices represent erased lengths of tape.
   233.    -- They are implemented, in effect, as data slices with non-significant data.
   234.    --
   235.    -- Parity mark slices represent tape marks on IBM-compatible Ampex TM4 decks.
   236.    -- See Manual, Appendix 7, p.317.
   237.
   238.    type basis_kind is (data_slice,
   239.                        GAP_slice,
   240.                        NULL_slice,
   241.                        WIPE_slice,
   242.                        even_parity_mark,
   243.                        odd_parity_mark);
   244.
   245.    -- These representations make for easy inspection of a MT file (e.g. using the UNIX od command).
   246.    for basis_kind use (data_slice       => Character'Pos('D'),
   247.                        GAP_slice        => Character'Pos('G'),
   248.                        NULL_slice       => Character'Pos('N'),
   249.                        WIPE_slice       => Character'Pos('W'),
   250.                        even_parity_mark => Character'Pos('e'),
   251.                        odd_parity_mark  => Character'Pos('o'));
   252.
   253.    subtype tape_gap_kind is MT.basis_kind
   254.       with Static_Predicate => tape_gap_kind in GAP_slice | WIPE_slice;
   255.
   256.    subtype tape_mark_kind is MT.basis_kind
   257.       with Static_Predicate => tape_mark_kind in odd_parity_mark | even_parity_mark;
   258.
   259.    subtype data_kind is MT.basis_kind
   260.       with Static_Predicate => data_kind = data_slice;
   261.
   262.    subtype slice_range   is MT.block_range range 0 .. magtape_data.slice_size_limit;
   263.    subtype slice_storage is data_storage (1 .. slice_range'Last);
   264.
   265.    tape_mark_data  : constant MT.slice_storage := (1 => tape_mark_sign, others => block_padding);
   266.    erased_gap_data : constant MT.slice_storage := (others => block_padding);
   267.
   268.    type slice is
   269.       record
   270.          kind              : MT.basis_kind;
   271.          is_first, is_last : Boolean;
   272.          is_LBM_flagged    : Boolean;
   273.          size              : MT.slice_range;
   274.          data              : MT.slice_storage; -- Only data(1 .. size) are valid.
   275.       end record
   276.    with Size => 8 * MT_record_length;
   277.
   278.    -- These two representation specifications put the kind and is_* fields at convenient positions
   279.    --    for easy inspection in a legible print of a MT file (e.g. using the UNIX od command).
   280.
   281.    -- The first byte contains the initial letter of the slice type (see basis_kind).
   282.
   283.    -- The second byte takes the following octal/ASCII values for non-tape mark slices:
   284.    --    000 = NUL  => no flags
   285.    --    001 = SOH  => first slice of block
   286.    --    010 = BEL  => last slice of block
   287.    --    011 = HT   => only slice of block (first and last)
   288.    --    100 = @    => LBM flag
   289.    --    101 = A    => first slice of block with LBM flag
   290.    --    110 = H    => last slice of block with LBM flag
   291.    --    111 = I    => only slice of block with LBM flag
   292.
   293.    for slice use
   294.       record
   295.          kind           at 0 range  0..7;
   296.          is_first       at 1 range  0..2;
   297.          is_last        at 1 range  3..5;
   298.          is_LBM_flagged at 1 range  6..7;
   299.          size           at 2 range  0..7;
   300.          data           at 3 range  0..8*slice_size_limit - 1;
   301.       end record;
   302.
   303.    even_parity_tape_mark : constant MT.slice := (even_parity_mark,
   304.                                                  is_first       => True,
   305.                                                  is_last        => True,
   306.                                                  is_LBM_flagged => True,
   307.                                                  size           => 1,
   308.                                                  data           => tape_mark_data);
   309.
   310.    odd_parity_tape_mark  : constant MT.slice := (odd_parity_mark,
   311.                                                  is_first       => True,
   312.                                                  is_last        => True,
   313.                                                  is_LBM_flagged => True,
   314.                                                  size           => 1,
   315.                                                  data           => tape_mark_data);
   316.
   317.    a_NULL_slice          : constant MT.slice := (NULL_slice,
   318.                                                  is_first       => False,
   319.                                                  is_last        => False,
   320.                                                  is_LBM_flagged => False,
   321.                                                  size           => 0,
   322.                                                  data           => erased_gap_data);
   323.
   324.    a_WIPE_slice          : constant MT.slice := (WIPE_slice,
   325.                                                  is_first       => True,
   326.                                                  is_last        => True,
   327.                                                  is_LBM_flagged => False,
   328.                                                  size           => 0,
   329.                                                  data           => erased_gap_data);
   330.
   331.    a_GAP_slice           : constant MT.slice := (GAP_slice,
   332.                                                  is_first       => True,
   333.                                                  is_last        => True,
   334.                                                  is_LBM_flagged => False,
   335.                                                  size           => 0,
   336.                                                  data           => erased_gap_data);
   337.
   338.    package MT_slice_IO is new Ada.Direct_IO(MT.slice);
   339.    use MT_slice_IO;
   340.
   341.    type file is tagged limited
   342.       record
   343.          has_a_WP_ring   : Boolean := True;
   344.          last_data_index : Count   := 0;
   345.          position        : Count   := 0;
   346.          reel            : File_Type;
   347.       end record;
   348.
   349.    -- The complete deck type with its primitive operations.
   350.
   351.    type deck is new IOC.fast.device with
   352.       record
   353.          -- unwound_frames tallies the amount of tape wound from its spool to the takeup spool;
   354.          --    i.e. how much has to be wound back before being able to unload the tape.
   355.          unwound_frames    : MT.length_in_frames := 0;
   356.          bytes_moved       : KDF9.word := 0;
   357.          is_LBM_flagged    : Boolean   := False;
   358.          terminator        : KDF9_char_sets.symbol;
   359.          recording_density : MT.length_in_frames;
   360.          max_reel_length   : MT.length_in_frames;
   361.          inter_block_gap   : MT.length_in_frames;
   362.          tape_capacity     : MT.length_in_frames;
   363.          PET_position      : MT.length_in_frames;
   364.          ETW_position      : MT.length_in_frames;
   365.          tape              : MT.file;
   366.       end record;
   367.
   368.    overriding
   369.    function IO_elapsed_time_total (the_deck : MT.deck)
   370.    return KDF9.us
   371.    is (the_deck.elapsed_time);
   372.
   373.    overriding
   374.    procedure Initialize (the_deck : in out MT.deck);
   375.
   376.    overriding
   377.    procedure Finalize (the_deck : in out MT.deck);
   378.
   379.    procedure open (the_deck : in out MT.deck;
   380.                    the_mode : in POSIX.access_mode)
   381.    is null;
   382.
   383.    overriding
   384.    function is_open (the_deck : MT.deck)
   385.    return Boolean;
   386.
   387.    overriding
   388.    function usage (the_deck : MT.deck)
   389.    return KDF9.word;
   390.
   391.    overriding
   392.    procedure close (the_deck : in out MT.deck);
   393.
   394.    overriding
   395.    procedure flush(the_deck : in out MT.deck) is null;
   396.
   397. end IOC.fast.MT;

 1332 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/ioc-slow-shift-fw.adb
Source file time stamp: 2020-10-27 19:57:59
Compiled at: 2020-11-12 18:12:12

     1. -- ioc-slow-shift-fw.adb
     2. --
     3. -- Emulation of the FlexoWriter buffer: monitor typewriter functionality.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received the copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Exceptions;
    20. with Ada.Text_IO;
    21. --
    22. with HCI;
    23. with host_IO;
    24.
    25. use  Ada.Text_IO;
    26. --
    27. use  HCI;
    28. use  host_IO;
    29.
    30. package body IOC.slow.shift.FW is
    31.
    32.    use  KDF9_char_sets;
    33.    use  Ada.Characters.Latin_1;
    34.
    35.    function a_LF_was_just_read (the_FW : FW.device)
    36.    return Boolean
    37.    is (the_FW.mode = the_flexowriter_is_reading and then a_LF_was_just_read(the_FW.stream));
    38.
    39.    function a_LF_was_just_written (the_FW : FW.device)
    40.    return Boolean
    41.    is (the_FW.mode = the_flexowriter_is_writing and then a_LF_was_just_written(the_FW.stream));
    42.
    43.    max_text_length : constant Positive := 64;  -- This is arbitrary, but seems reasonable.
    44.    type interaction is
    45.       record
    46.          text           : String(1 .. max_text_length);
    47.          prompt_length,
    48.          total_length   : Positive range 1 .. max_text_length;
    49.       end record;
    50.
    51.    max_interactions : constant Positive := 16; -- This is arbitrary, but seems reasonable.
    52.    interactions     : array (1 .. max_interactions) of IOC.slow.shift.FW.interaction;
    53.    next_interaction : Positive := 1;
    54.    last_interaction : Natural  := 0;
    55.
    56.     -- A '®' denotes LF, and the '©' denotes FF in an interaction text input.
    57.    LF_surrogate     : constant Character := '®';
    58.    FF_surrogate     : constant Character := '©';
    59.
    60.    -- These are the ANSI SGR terminal escape codes for styling FW output.
    61.    red_font_code   : constant String := ESC & "[0m" & ESC & "[31m";
    62.    black_font_code : constant String := ESC & "[0m" & ESC & "[30m";
    63.    underline_code  : constant String := ESC & "[4m";
    64.    plain_font_code : constant String := ESC & "[0m";
    65.
    66.    procedure set_text_colour_to_red (the_flexowriter_output : in out host_IO.stream) is
    67.    begin
    68.       if the_terminal_is_ANSI_compatible and realistic_FW_output_is_wanted then
    69.          put_escape_code(red_font_code, the_flexowriter_output);
    70.       end if;
    71.    end set_text_colour_to_red;
    72.
    73.    procedure set_text_colour_to_black (the_flexowriter_output : in out host_IO.stream) is
    74.    begin
    75.       if the_terminal_is_ANSI_compatible then
    76.          put_escape_code(black_font_code, the_flexowriter_output);
    77.       end if;
    78.    end set_text_colour_to_black;
    79.
    80.    procedure set_text_style_to_underline (the_flexowriter_output : in out host_IO.stream) is
    81.    begin
    82.       if the_terminal_is_ANSI_compatible then
    83.          put_escape_code(underline_code, the_flexowriter_output);
    84.       end if;
    85.    end set_text_style_to_underline;
    86.
    87.    procedure set_text_style_to_plain (the_flexowriter_output : in out host_IO.stream) is
    88.    begin
    89.       if the_terminal_is_ANSI_compatible then
    90.          put_escape_code(plain_font_code, the_flexowriter_output);
    91.       end if;
    92.    end set_text_style_to_plain;
    93.
    94.    overriding
    95.    procedure Initialize (the_FW : in out FW.device) is
    96.       interaction_file : Ada.Text_IO.File_Type;
    97.    begin
    98.       ensure_UI_is_open;
    99.       the_FW.mode := the_flexowriter_is_writing;
   100.       the_FW.device_name := device_name_of(the_FW);
   101.       if the_FW.device_name = "FW0" then
   102.          -- Attempt to open the command file for the console the_FW.
   103.          begin
   104.             Open(interaction_file, In_File, "FW0");
   105.          response_list_loop:
   106.             while not End_of_file(interaction_file) loop
   107.                if last_interaction = max_interactions then
   108.                   log_line("The file FW0 contains too many interactions!");
   109.                   raise Ada.Text_IO.Data_Error;
   110.                end if;
   111.                last_interaction := last_interaction + 1;
   112.                declare
   113.                   interaction       : String  := Get_Line(interaction_file);
   114.                   the_prompt_length : Natural := 0;
   115.                begin
   116.                   if interaction'Length > max_text_length then
   117.                      log_line(
   118.                               "The file FW0 contains an overlong string: '"
   119.                             & interaction
   120.                             & "'!"
   121.                              );
   122.                      raise Ada.Text_IO.Data_Error;
   123.                   end if;
   124.
   125.                   exit response_list_loop when interaction'Length = 0;
   126.
   127.                   for p in 1 .. interaction'Length loop
   128.                      if interaction(p) = ';' then
   129.                         the_prompt_length := p;
   130.                      elsif interaction(p) = LF_surrogate then
   131.                         -- Convert '®' to LF to allow for multi-line prompts.
   132.                         interaction(p) := LF;
   133.                      elsif interaction(p) = FF_surrogate then
   134.                         -- Convert '©' to FF to allow for multi-line prompts.
   135.                         interaction(p) := FF;
   136.                      end if;
   137.                   end loop;
   138.
   139.                   if the_prompt_length = 0 then
   140.                      log_line(
   141.                               "The file FW0 contains the string: '"
   142.                             & interaction
   143.                             & "' without the semicolon!"
   144.                              );
   145.                      raise Ada.Text_IO.Data_Error;
   146.                   end if;
   147.
   148.                   interactions(last_interaction).text(1 .. interaction'Length) := interaction;
   149.                   interactions(last_interaction).prompt_length := the_prompt_length;
   150.                   interactions(last_interaction).total_length := interaction'Length;
   151.                end;
   152.             end loop response_list_loop;
   153.          exception
   154.             when Name_Error =>
   155.                null;
   156.             when Use_Error =>
   157.                log_line("The file FW0 exists, but cannot be read!");
   158.          end;
   159.       end if;
   160.       open(the_FW.stream, the_FW.device_name, read_mode, UI_in_FD);
   161.       open(the_FW.output, the_FW.device_name, write_mode, UI_out_FD);
   162.       IOC.device(the_FW).Initialize;
   163.       the_FW.current_case := KDF9_char_sets.Case_Normal;
   164.    end Initialize;
   165.
   166.    -- If authentic timing, the delay of length the_pause is inserted between characters output
   167.    --    to the Flexowriter, with the aim of approximating the actual speed of its typing.
   168.    the_pause  : KDF9.us := 0;
   169.
   170.    procedure set_the_duration_of_the_pause (the_FW : in FW.device) is
   171.    begin
   172.       if authentic_timing_is_enabled then
   173.          the_pause := the_FW.quantum;
   174.       else
   175.          the_pause := 0;
   176.       end if;
   177.    end set_the_duration_of_the_pause;
   178.
   179.    call_for_manual_input    : constant String (1..2) := (others => BEL);
   180.
   181.    procedure inject_a_response (the_FW     : in out FW.device;
   182.                                 the_prompt : in String;
   183.                                 the_size   : in out KDF9.word) is
   184.    begin
   185.       set_the_duration_of_the_pause(the_FW);
   186.       for t in next_interaction .. last_interaction loop
   187.          declare
   188.             the : interaction renames interactions(t);
   189.          begin
   190.             if the.prompt_length = the.total_length then
   191.                -- A null response, so terminate the program.
   192.                raise exceptions.quit_request
   193.                      with "at the prompt: '" & the_prompt & "'";
   194.             end if;
   195.             next_interaction := next_interaction + 1;
   196.             if the.text(1..the.prompt_length-1) = the_prompt and then
   197.                   the.text(the.prompt_length-0) = ';'            then
   198.                inject(the.text(the.prompt_length+1..the.total_length) & LF, the_FW.stream);
   199.                the_size := the_size + KDF9.word(the.total_length-the.prompt_length);
   200.                put_chars(the.text(the.prompt_length+1..the.total_length) & LF, the_FW.output);
   201.                -- Human operators type much more slowly than KDF9 buffers!
   202.                flush(the_FW.output, the_pause*10);
   203.                the_FW.mode := the_flexowriter_is_reading;
   204.                return;
   205.             end if;
   206.          end;
   207.       end loop;
   208.       -- No canned response is available, so control reverts to the terminal.
   209.       -- Output an audible signal to notify the operator.
   210.       if noninteractive_usage_is_enabled then
   211.          raise input_is_impossible;
   212.       end if;
   213.       put_bytes(call_for_manual_input, the_FW.output);
   214.       flush(the_FW.output, the_pause);
   215.       the_FW.mode := the_flexowriter_is_reading;
   216.    end inject_a_response;
   217.
   218.    -- TRQq
   219.    overriding
   220.    procedure PIA (the_FW      : in out FW.device;
   221.                   Q_operand   : in KDF9.Q_register;
   222.                   set_offline : in Boolean) is
   223.    begin
   224.       if noninteractive_usage_is_enabled then
   225.          raise input_is_impossible;
   226.       end if;
   227.       put_bytes(call_for_manual_input, the_FW.output);
   228.       flush(the_FW.output);
   229.       the_FW.mode := the_flexowriter_is_reading;
   230.       start_slow_transfer(the_FW, Q_operand, set_offline);
   231.       read(the_FW, Q_operand);
   232.       lock_out_relative_addresses(Q_operand);
   233.       reset(the_FW.stream);
   234.    end PIA;
   235.
   236.    -- TREQq
   237.    overriding
   238.    procedure PIB (the_FW      : in out FW.device;
   239.                   Q_operand   : in KDF9.Q_register;
   240.                   set_offline : in Boolean) is
   241.    begin
   242.       if noninteractive_usage_is_enabled then
   243.          raise input_is_impossible;
   244.       end if;
   245.       put_bytes(call_for_manual_input, the_FW.output);
   246.       flush(the_FW.output);
   247.       the_FW.mode := the_flexowriter_is_reading;
   248.       start_slow_transfer(the_FW, Q_operand, set_offline);
   249.       read_to_EM(the_FW, Q_operand);
   250.       lock_out_relative_addresses(Q_operand);
   251.       reset(the_FW.stream);
   252.    end PIB;
   253.
   254.    overriding
   255.    procedure PIC (the_FW      : in out FW.device;
   256.                   Q_operand   : in KDF9.Q_register;
   257.                   set_offline : in Boolean) is
   258.    begin
   259.       if noninteractive_usage_is_enabled then
   260.          raise input_is_impossible;
   261.       end if;
   262.       put_bytes(call_for_manual_input, the_FW.output);
   263.       flush(the_FW.output);
   264.       the_FW.mode := the_flexowriter_is_reading;
   265.       start_slow_transfer(the_FW, Q_operand, set_offline);
   266.       words_read(the_FW, Q_operand);
   267.       lock_out_relative_addresses(Q_operand);
   268.       reset(the_FW.stream);
   269.    end PIC;
   270.
   271.    overriding
   272.    procedure PID (the_FW      : in out FW.device;
   273.                   Q_operand   : in KDF9.Q_register;
   274.                   set_offline : in Boolean) is
   275.    begin
   276.       if noninteractive_usage_is_enabled then
   277.          raise input_is_impossible;
   278.       end if;
   279.       put_bytes(call_for_manual_input, the_FW.output);
   280.       flush(the_FW.output);
   281.       the_FW.mode := the_flexowriter_is_reading;
   282.       start_slow_transfer(the_FW, Q_operand, set_offline);
   283.       words_read_to_EM(the_FW, Q_operand);
   284.       lock_out_relative_addresses(Q_operand);
   285.       reset(the_FW.stream);
   286.    end PID;
   287.
   288.    overriding
   289.    procedure PIE (the_FW      : in out FW.device;
   290.                   Q_operand   : in KDF9.Q_register;
   291.                   set_offline : in Boolean) is
   292.    begin
   293.       PIA(the_FW, Q_operand, set_offline);
   294.    end PIE;
   295.
   296.    overriding
   297.    procedure PIF (the_FW      : in out FW.device;
   298.                   Q_operand   : in KDF9.Q_register;
   299.                   set_offline : in Boolean) is
   300.    begin
   301.       PIB(the_FW, Q_operand, set_offline);
   302.    end PIF;
   303.
   304.    overriding
   305.    procedure PIG (the_FW      : in out FW.device;
   306.                   Q_operand   : in KDF9.Q_register;
   307.                   set_offline : in Boolean) is
   308.    begin
   309.       PIC(the_FW, Q_operand, set_offline);
   310.    end PIG;
   311.
   312.    overriding
   313.    procedure PIH (the_FW      : in out FW.device;
   314.                   Q_operand   : in KDF9.Q_register;
   315.                   set_offline : in Boolean) is
   316.    begin
   317.       PID(the_FW, Q_operand, set_offline);
   318.    end PIH;
   319.
   320.    -- neat strips off any enclosing non-graphic characters from s.
   321.    function neat (s : String)
   322.    return String is
   323.       l : Positive := 1;
   324.       r : Natural  := 0;
   325.    begin
   326.       for i in s'Range loop
   327.          l := i;
   328.       exit when s(i) > ' ' and s(i) /= DEL;
   329.       end loop;
   330.       for i in reverse s'Range loop
   331.          r := i;
   332.       exit when s(i) > ' ' and s(i) /= DEL;
   333.       end loop;
   334.       return s(l..r);  -- s(1..0) yields the null string when s is the null string.
   335.    end neat;
   336.
   337.    overriding
   338.    procedure do_output_housekeeping (the_FW   : in out FW.device;
   339.                                      written,
   340.                                      fetched  : in KDF9.word) is
   341.    begin
   342.       flush(the_FW.stream);
   343.       add_in_the_IO_CPU_time(the_FW, fetched);
   344.       correct_transfer_time(the_FW, written);
   345.       the_FW.byte_count := the_FW.byte_count + fetched;
   346.    end do_output_housekeeping;
   347.
   348.    underlined : Boolean := False;
   349.
   350.    procedure put_symbols (the_FW         : in out FW.device;
   351.                           Q_operand      : in KDF9.Q_register;
   352.                           transfer_to_EM : in Boolean) is
   353.       start_address : constant KDF9.address := Q_operand.I;
   354.       end_address   : constant KDF9.address := Q_operand.M;
   355.       fill   : KDF9.word := 0;
   356.       size   : KDF9.word := 0;
   357.       symbol : KDF9_char_sets.symbol;
   358.       char   : Character;
   359.    begin
   360.       check_addresses_and_lockouts(start_address, end_address);
   361.       set_the_duration_of_the_pause(the_FW);
   362.       the_FW.mode := the_flexowriter_is_writing;
   363.       set_text_style_to_plain(the_FW.output);
   364.       set_text_colour_to_red(the_FW.output);
   365.
   366.       -- Ensure that any prompt occupies the buffer alone.
   367.       flush(the_FW.output);
   368.
   369.    word_loop:
   370.       for w in start_address .. end_address loop
   371.          for c in KDF9_char_sets.symbol_index'Range loop
   372.             case the_FW.mode is
   373.
   374.                when the_flexowriter_is_writing =>
   375.                   symbol := fetch_symbol(w, c);
   376.                   size := size + 1;
   377.
   378.                   if symbol = KDF9_char_sets.Word_Filler then
   379.                      fill := fill + 1;
   380.
   381.                   elsif symbol = KDF9_char_sets.Case_Shift then
   382.                      the_FW.current_case := KDF9_char_sets.Case_Shift;
   383.                      the_FW.shifts := the_FW.shifts + 1;
   384.
   385.                   elsif  symbol = KDF9_char_sets.Case_Normal then
   386.                      the_FW.current_case := KDF9_char_sets.Case_Normal;
   387.                      the_FW.shifts := the_FW.shifts + 1;
   388.
   389.                   else
   390.
   391.                      if the_FW.current_case = KDF9_char_sets.Case_Normal then
   392.                         char := TP_CN(symbol);
   393.                      else
   394.                         char := TP_CS(symbol);
   395.                      end if;
   396.
   397.                      if char = ';' then
   398.
   399.                         declare
   400.                            the_prompt : constant String := contents(the_FW.output);
   401.                         begin
   402.                            -- Must flush AFTER saving the prompt and BEFORE going black.
   403.                            flush(the_FW.output, the_pause);
   404.                            set_text_colour_to_black(the_FW.output);
   405.                            set_text_style_to_plain(the_FW.output);
   406.                            put_byte(';', the_FW.output);
   407.                            flush(the_FW.output, the_pause);
   408.
   409.                            inject_a_response(the_FW, neat(the_prompt), size);
   410.
   411.                            the_FW.mode := the_flexowriter_is_reading;
   412.                            set_text_style_to_plain(the_FW.output);
   413.                         end;
   414.
   415.                      elsif flexowriter_output_is_wanted then
   416.
   417.                         if char = '_' then
   418.                            underlined := True;
   419.                            do_not_put_byte(char, the_FW.output);
   420.                            flush(the_FW.output, the_pause);
   421.                         else
   422.                            if underlined then
   423.                               set_text_style_to_underline(the_FW.output);
   424.                            end if;
   425.                            put_char(char, the_FW.output);
   426.                            if underlined then
   427.                               flush(the_FW.output, the_pause);
   428.                               set_text_style_to_plain(the_FW.output);
   429.                               set_text_colour_to_red(the_FW.output);
   430.                               underlined := False;
   431.                            end if;
   432.                         end if;
   433.
   434.                      else
   435.                         do_not_put_byte(char, the_FW.output);
   436.                      end if;
   437.
   438.                      exit word_loop when transfer_to_EM and symbol = KDF9_char_sets.End_Message;
   439.                   end if;
   440.
   441.                when the_flexowriter_is_reading =>
   442.                   get_char(char, the_FW.stream);
   443.                   if case_of(char) /= both and case_of(char) /= the_FW.current_case then
   444.                      store_symbol(CN_TR(next_case(the_FW.current_case)), w, c);
   445.                      size := size + 1;
   446.                      the_FW.current_case := the_FW.current_case xor 1;
   447.                      back_off(the_FW.stream);
   448.                   else
   449.                      if the_FW.current_case = KDF9_char_sets.Case_Normal then
   450.                         symbol := CN_TR(char);
   451.                      else
   452.                         symbol := CS_TR(char);
   453.                      end if;
   454.                      store_symbol(symbol, w, c);
   455.                      size := size + 1;
   456.                      if transfer_to_EM and symbol = KDF9_char_sets.End_Message then
   457.                         for d in 1 .. 7-c loop
   458.                            store_symbol(KDF9_char_sets.Blank_Space, w, c+d);
   459.                         end loop;
   460.                         exit word_loop;
   461.                      end if;
   462.                   end if;
   463.
   464.             end case;
   465.          end loop;
   466.       end loop word_loop;
   467.
   468.       flush(the_FW.output, the_pause);
   469.       set_text_style_to_plain(the_FW.output);
   470.       set_text_colour_to_black(the_FW.output);
   471.       do_output_housekeeping(the_FW, written => size-fill, fetched => size);
   472.
   473.    exception
   474.
   475.       when end_of_stream =>
   476.          flush(the_FW.output);
   477.          set_text_colour_to_black(the_FW.output);
   478.          set_text_style_to_plain(the_FW.output);
   479.          do_output_housekeeping(the_FW, written => size-fill, fetched => size);
   480.    end put_symbols;
   481.
   482.    overriding
   483.    procedure write (the_FW    : in out FW.device;
   484.                     Q_operand : in KDF9.Q_register) is
   485.    begin
   486.       put_symbols(the_FW, Q_operand, transfer_to_EM => False);
   487.    end write;
   488.
   489.    overriding
   490.    procedure write_to_EM (the_FW    : in out FW.device;
   491.                           Q_operand : in KDF9.Q_register) is
   492.    begin
   493.       put_symbols(the_FW, Q_operand, transfer_to_EM => True);
   494.    end write_to_EM;
   495.
   496.    -- TWQq
   497.    overriding
   498.    procedure POA (the_FW      : in out FW.device;
   499.                   Q_operand   : in KDF9.Q_register;
   500.                   set_offline : in Boolean) is
   501.    begin
   502.       start_slow_transfer(the_FW, Q_operand, set_offline);
   503.       write(the_FW, Q_operand);
   504.       lock_out_relative_addresses(Q_operand);
   505.       reset(the_FW.stream);
   506.    end POA;
   507.
   508.    -- TWEQq
   509.    overriding
   510.    procedure POB (the_FW      : in out FW.device;
   511.                   Q_operand   : in KDF9.Q_register;
   512.                   set_offline : in Boolean) is
   513.    begin
   514.       start_slow_transfer(the_FW, Q_operand, set_offline);
   515.       write_to_EM(the_FW, Q_operand);
   516.       lock_out_relative_addresses(Q_operand);
   517.       reset(the_FW.stream);
   518.    end POB;
   519.
   520.    procedure put_words (the_FW         : in out FW.device;
   521.                         Q_operand      : in KDF9.Q_register;
   522.                         transfer_to_EM : in Boolean := False) is
   523.       start_address : constant KDF9.address := Q_operand.I;
   524.       end_address   : constant KDF9.address := Q_operand.M;
   525.       size : KDF9.word := 0;
   526.       word : KDF9.word;
   527.       char : Character;
   528.    begin
   529.       check_addresses_and_lockouts(start_address, end_address);
   530.       set_the_duration_of_the_pause(the_FW);
   531.       the_FW.mode := the_flexowriter_is_writing;
   532.       set_text_style_to_plain(the_FW.output);
   533.       set_text_colour_to_red(the_FW.output);
   534.    word_loop:
   535.       for w in start_address .. end_address loop
   536.          case the_FW.mode is
   537.
   538.             when the_flexowriter_is_writing =>
   539.                word := fetch_word(w) and 8#377#;
   540.                size := size + 1;
   541.                char := Character'Val(word);
   542.
   543.                if word = KDF9_char_sets.Semi_Colon_tape_bits then
   544.                   -- Hypothesis: POC and POD act like POA and POB with respect to prompting;
   545.                   --    and change from writing to reading after the output of any word that has
   546.                   --       the KDF9 FW tape code for a semicolon in its least significant 8 bits.
   547.                   declare
   548.                      the_prompt : constant String := contents(the_FW.output);
   549.                   begin
   550.                      -- Must flush AFTER saving the prompt and BEFORE going black.
   551.                      flush(the_FW.output, the_pause);
   552.                      set_text_colour_to_black(the_FW.output);
   553.                      set_text_style_to_plain(the_FW.output);
   554.                      put_byte(';', the_FW.output);
   555.                      flush(the_FW.output, the_pause);
   556.
   557.                      inject_a_response(the_FW, neat(the_prompt), size);
   558.
   559.                      the_FW.mode := the_flexowriter_is_reading;
   560.                      set_text_style_to_plain(the_FW.output);
   561.                   end;
   562.                elsif flexowriter_output_is_wanted then
   563.
   564.                         if char = '_' then
   565.                            underlined := True;
   566.                            do_not_put_byte(char, the_FW.output);
   567.                            flush(the_FW.output, the_pause);
   568.                         else
   569.                            if underlined then
   570.                               set_text_style_to_underline(the_FW.output);
   571.                            end if;
   572.                            put_char(char, the_FW.output);
   573.                            if underlined then
   574.                               flush(the_FW.output, the_pause);
   575.                               set_text_style_to_plain(the_FW.output);
   576.                               set_text_colour_to_red(the_FW.output);
   577.                               underlined := False;
   578.                            end if;
   579.                         end if;
   580.       exit word_loop when transfer_to_EM and then word = KDF9_char_sets.End_Message_tape_bits;
   581.                end if;
   582.
   583.             when the_flexowriter_is_reading =>
   584.                get_char(char, the_FW.stream);
   585.                size := size + 1;
   586.                word := KDF9.word(Character'Pos(char));
   587.                store_word(word, w);
   588.       exit word_loop when transfer_to_EM and then word = KDF9_char_sets.End_Message_tape_bits;
   589.
   590.          end case;
   591.       end loop word_loop;
   592.
   593.       flush(the_FW.output);
   594.       set_text_colour_to_black(the_FW.output);
   595.       set_text_style_to_plain(the_FW.output);
   596.       do_output_housekeeping(the_FW, written => size, fetched => size);
   597.
   598.    exception
   599.
   600.       when end_of_stream =>
   601.          flush(the_FW.output);
   602.          set_text_colour_to_black(the_FW.output);
   603.          set_text_style_to_plain(the_FW.output);
   604.          do_output_housekeeping(the_FW, written => size, fetched => size);
   605.    end put_words;
   606.
   607.    overriding
   608.    procedure words_write (the_FW    : in out FW.device;
   609.                           Q_operand : in KDF9.Q_register) is
   610.    begin
   611.       put_words(the_FW, Q_operand, transfer_to_EM => False);
   612.    end words_write;
   613.
   614.    overriding
   615.    procedure words_write_to_EM (the_FW    : in out FW.device;
   616.                                 Q_operand : in KDF9.Q_register) is
   617.    begin
   618.       put_words(the_FW, Q_operand, transfer_to_EM => True);
   619.    end words_write_to_EM;
   620.
   621.    -- TWCQq
   622.    overriding
   623.    procedure POC (the_FW      : in out FW.device;
   624.                   Q_operand   : in KDF9.Q_register;
   625.                   set_offline : in Boolean) is
   626.    begin
   627.       start_slow_transfer(the_FW, Q_operand, set_offline);
   628.       words_write(the_FW, Q_operand);
   629.       lock_out_relative_addresses(Q_operand);
   630.    end POC;
   631.
   632.    -- TWECQq
   633.    overriding
   634.    procedure POD (the_FW      : in out FW.device;
   635.                   Q_operand   : in KDF9.Q_register;
   636.                   set_offline : in Boolean) is
   637.    begin
   638.       start_slow_transfer(the_FW, Q_operand, set_offline);
   639.       words_write_to_EM(the_FW, Q_operand);
   640.       lock_out_relative_addresses(Q_operand);
   641.    end POD;
   642.
   643.    overriding
   644.    procedure Finalize (the_FW : in out FW.device) is
   645.       total : constant KDF9.word := the_FW.output.bytes_moved+the_FW.stream.bytes_moved + the_FW.shifts;
   646.    begin
   647.       close(
   648.            the_FW,
   649.            "transferred",
   650.            total,
   651.            "character" & plurality(total)
   652.           );
   653.    end Finalize;
   654.
   655.    -- This is the monitor console Flexowriter.
   656.
   657.    FW_quantum : constant := 1E6 / 10;  -- 10 characters per second.
   658.
   659.    type FW_access is access FW.device;
   660.
   661.    FW0 : FW_access with Warnings => Off;
   662.
   663.    already_enabled : Boolean := False;
   664.
   665.    procedure enable (b : in KDF9.buffer_number) is
   666.    begin
   667.       if already_enabled then trap_operator_error("more than 1 FW unit specified"); end if;
   668.       if b /= 0 then trap_operator_error("FW0 must be on buffer 0"); end if;
   669.       FW0 := new FW.device (number  => b,
   670.                             kind    => FW_kind,
   671.                             unit    => 0,
   672.                             quantum => FW_quantum);
   673.       already_enabled := True;
   674.    end enable;
   675.
   676. end IOC.slow.shift.FW;
   677.

Compiling: ../Source/ioc-slow-shift-fw.ads
Source file time stamp: 2020-09-27 23:56:35
Compiled at: 2020-11-12 18:12:12

     1. -- ioc-slow-shift-fw.ads
     2. --
     3. -- Emulation of a FlexoWriter buffer: monitor typewriter functionality.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package IOC.slow.shift.FW is
    20.
    21.    type device is new IOC.slow.shift.device with private;
    22.
    23.    function a_LF_was_just_read (the_FW : FW.device)
    24.    return Boolean;
    25.
    26.    function a_LF_was_just_written (the_FW : FW.device)
    27.    return Boolean;
    28.
    29.    -- TRQq
    30.    overriding
    31.    procedure PIA (the_FW      : in out FW.device;
    32.                   Q_operand   : in KDF9.Q_register;
    33.                   set_offline : in Boolean);
    34.
    35.    -- TREQq
    36.    overriding
    37.    procedure PIB (the_FW      : in out FW.device;
    38.                   Q_operand   : in KDF9.Q_register;
    39.                   set_offline : in Boolean);
    40.
    41.    -- TRCQq character read
    42.    overriding
    43.    procedure PIC (the_FW      : in out FW.device;
    44.                   Q_operand   : in KDF9.Q_register;
    45.                   set_offline : in Boolean);
    46.
    47.    -- TRECQq character read to End_Message
    48.    overriding
    49.    procedure PID (the_FW      : in out FW.device;
    50.                   Q_operand   : in KDF9.Q_register;
    51.                   set_offline : in Boolean);
    52.
    53.    -- as PIA
    54.    overriding
    55.    procedure PIE (the_FW      : in out FW.device;
    56.                   Q_operand   : in KDF9.Q_register;
    57.                   set_offline : in Boolean);
    58.
    59.    -- as PIB
    60.    overriding
    61.    procedure PIF (the_FW      : in out FW.device;
    62.                   Q_operand   : in KDF9.Q_register;
    63.                   set_offline : in Boolean);
    64.
    65.    -- as PIC
    66.    overriding
    67.    procedure PIG (the_FW      : in out FW.device;
    68.                   Q_operand   : in KDF9.Q_register;
    69.                   set_offline : in Boolean);
    70.
    71.    -- as PID
    72.    overriding
    73.    procedure PIH (the_FW      : in out FW.device;
    74.                   Q_operand   : in KDF9.Q_register;
    75.                   set_offline : in Boolean);
    76.
    77.    -- TWQq
    78.    overriding
    79.    procedure POA (the_FW      : in out FW.device;
    80.                   Q_operand   : in KDF9.Q_register;
    81.                   set_offline : in Boolean);
    82.
    83.    -- TWEQq
    84.    overriding
    85.    procedure POB (the_FW      : in out FW.device;
    86.                   Q_operand   : in KDF9.Q_register;
    87.                   set_offline : in Boolean);
    88.
    89.    -- NB the following assumes that page 285 of the Manual is erroneous,
    90.    -- and that POC and POD for the Flexowriter are analogous to the tape punch,
    91.    -- as other sources, such as the "Usecode Digest", do in fact indicate.
    92.
    93.    -- TWCQq character write
    94.    overriding
    95.    procedure POC (the_FW      : in out FW.device;
    96.                   Q_operand   : in KDF9.Q_register;
    97.                   set_offline : in Boolean);
    98.
    99.    -- TWECQq character write to End_Message
   100.    overriding
   101.    procedure POD (the_FW      : in out FW.device;
   102.                   Q_operand   : in KDF9.Q_register;
   103.                   set_offline : in Boolean);
   104.
   105.    procedure enable (b : in KDF9.buffer_number);
   106.
   107. private
   108.
   109.    type flexowriter_mode is
   110.       (the_flexowriter_is_reading, the_flexowriter_is_writing);
   111.
   112.    -- The Flexowriter has separate input and output streams, to accommodate the console I/O API
   113.    --    of MS Windows, which requires separate pseudo-devices for input and output.
   114.    type device is new IOC.slow.shift.device with
   115.       record
   116.          output : host_IO.stream;
   117.          mode   : FW.flexowriter_mode;
   118.          shifts : KDF9.word := 0;
   119.       end record;
   120.
   121.    overriding
   122.    procedure Initialize (the_FW : in out FW.device);
   123.
   124.    overriding
   125.    procedure Finalize (the_FW : in out FW.device);
   126.
   127.    overriding
   128.    procedure write (the_FW    : in out FW.device;
   129.                     Q_operand : in KDF9.Q_register);
   130.
   131.    overriding
   132.    procedure write_to_EM (the_FW    : in out FW.device;
   133.                           Q_operand : in KDF9.Q_register);
   134.    overriding
   135.    procedure words_write (the_FW    : in out FW.device;
   136.                           Q_operand : in KDF9.Q_register);
   137.
   138.    overriding
   139.    procedure words_write_to_EM (the_FW    : in out FW.device;
   140.                                 Q_operand : in KDF9.Q_register);
   141.
   142.    overriding
   143.    procedure do_output_housekeeping (the_FW   : in out FW.device;
   144.                                      written,
   145.                                      fetched  : in KDF9.word);
   146.
   147. end IOC.slow.shift.FW;

 677 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/ioc-slow-shift-gp.adb
Source file time stamp: 2020-11-07 00:32:50
Compiled at: 2020-11-12 18:12:12

     1. -- ioc-slow-shift-gp.ads
     2. --
     3. -- Emulation of a Calcomp 564 graph plotter, switched to a tape punch buffer.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Exceptions;
    20. --
    21. with HCI;
    22. with IOC.equipment;
    23. with plotter;
    24. with postscript;
    25. with tracing;
    26.
    27. use  HCI;
    28. use  IOC.equipment;
    29. use  plotter;
    30. use  postscript;
    31. use  tracing;
    32.
    33. package body IOC.slow.shift.GP is
    34.
    35.    overriding
    36.    procedure Initialize (the_GP : in out GP.device) is
    37.    begin
    38.       -- Ready the graph plotter driver and PostScript output file.
    39.       the_GP.device_name := device_name_of(the_GP);
    40.       open(the_GP.stream, the_GP.device_name, write_mode);
    41.       IOC.device(the_GP).Initialize;
    42.       if the_GP.is_open then
    43.          truncate(the_GP.stream, to_length => 0);
    44.          initialize_PS_output(the_GP.stream);
    45.          open_the_plot_file(the_GP.stream);
    46.       end if;
    47.    end Initialize;
    48.
    49.    overriding
    50.    procedure Finalize (the_GP : in out GP.device) is
    51.    begin
    52.       if the_graph_plotter_is_enabled then
    53.          if the_GP.is_open           and then
    54.                the_GP.byte_count /= 0    then
    55.             if the_final_state_is_wanted then
    56.                log_line(
    57.                         the_GP.device_name
    58.                       & " on buffer #"
    59.                       & oct_of(KDF9.Q_part(the_GP.number), 2)
    60.                       & " made"
    61.                       & the_GP.byte_count'Image
    62.                       & " plotting steps."
    63.                        );
    64.             end if;
    65.             the_GP.byte_count := 0;
    66.             close_the_plot_file(the_GP.stream);
    67.             finalize_PS_output(the_GP.stream);
    68.          end if;
    69.       end if;
    70.    end Finalize;
    71.
    72.    -- the_T_bit_is_set (the buffer has been switched from a tape punch to a graph plotter)
    73.    overriding
    74.    procedure PMB (the_GP      : in out GP.device;
    75.                   Q_operand   : in KDF9.Q_register;
    76.                   set_offline : in Boolean) is
    77.    begin
    78.       validate_device(the_GP, Q_operand);
    79.       validate_parity(the_GP);
    80.       deal_with_a_busy_device(the_GP, 13, set_offline);
    81.       the_T_bit_is_set := True;
    82.       take_note_of_test(the_GP.device_name, Q_operand, the_T_bit_is_set);
    83.    end PMB;
    84.
    85.    GP_quantum   : constant := 1E6 / 200;  -- 200 plotting movements per second.
    86.    GP_lift_time : constant := 1E6 /  10;  -- 10 pen up/down movements per second.
    87.    lift_ratio   : constant := GP_lift_time / GP_quantum;
    88.
    89.    overriding
    90.    procedure do_output_housekeeping (the_GP      : in out GP.device;
    91.                                      size, lifts : in     KDF9.word) is
    92.    begin
    93.       add_in_the_IO_CPU_time(the_GP, size);
    94.       correct_transfer_time(the_GP, size - lifts + lifts * lift_ratio);
    95.    end do_output_housekeeping;
    96.
    97.    procedure put_symbols (the_GP    : in out GP.device;
    98.                           Q_operand : in KDF9.Q_register) is
    99.       start_address : constant KDF9.address := Q_operand.I;
   100.       end_address   : constant KDF9.address := Q_operand.M;
   101.       size    : KDF9.word := 0;
   102.       lifts   : KDF9.word := 0;
   103.       command : plotter.command;
   104.    begin
   105.       check_addresses_and_lockouts(start_address, end_address);
   106.    word_loop:
   107.       for w in start_address .. end_address loop
   108.          for c in KDF9_char_sets.symbol_index'Range loop
   109.             command := plotter.command(fetch_symbol(w, c));
   110.             perform(command, the_GP.stream);
   111.             size := size + 1;
   112.             the_GP.byte_count := the_GP.byte_count + 1;
   113.             if command in pen_up | pen_down then
   114.                -- These actions are much slower than plotting movements.
   115.                lifts := lifts + 1;
   116.             end if;
   117.          end loop;
   118.       end loop word_loop;
   119.       do_output_housekeeping (the_GP, size, lifts);
   120.    end put_symbols;
   121.
   122.    overriding
   123.    procedure POA (the_GP      : in out GP.device;
   124.                   Q_operand   : in KDF9.Q_register;
   125.                   set_offline : in Boolean) is
   126.    begin
   127.       start_slow_transfer(the_GP, Q_operand, set_offline);
   128.       put_symbols(the_GP, Q_operand);
   129.       lock_out_relative_addresses(Q_operand);
   130.    end POA;
   131.
   132.    overriding
   133.    procedure POB (the_GP      : in out GP.device;
   134.                   Q_operand   : in KDF9.Q_register;
   135.                   set_offline : in Boolean) is
   136.    begin
   137.       -- See the Manual Appendix 6, §5.2, p.303.
   138.       POA(the_GP, Q_operand, set_offline);
   139.    end POB;
   140.
   141.    procedure put_words (the_GP    : in out GP.device;
   142.                         Q_operand : in KDF9.Q_register) is
   143.       start_address : constant KDF9.address := Q_operand.I;
   144.       end_address   : constant KDF9.address := Q_operand.M;
   145.       size    : KDF9.word := 0;
   146.       lifts   : KDF9.word := 0;
   147.       command : plotter.command;
   148.    begin
   149.       check_addresses_and_lockouts(start_address, end_address);
   150.       for w in start_address .. end_address loop
   151.          -- Ony the last 6 bits (character 7) of each word are used.
   152.          command := plotter.command(fetch_symbol(w, 7));
   153.          perform(command, the_GP.stream);
   154.          size := size + 1;
   155.          the_GP.byte_count := the_GP.byte_count + 1;
   156.          if command in pen_up | pen_down then
   157.             -- These actions are much slower than plotting movements.
   158.             lifts := lifts + 1;
   159.          end if;
   160.       end loop;
   161.       do_output_housekeeping (the_GP, size, lifts);
   162.    end put_words;
   163.
   164.    overriding
   165.    procedure POC (the_GP      : in out GP.device;
   166.                   Q_operand   : in KDF9.Q_register;
   167.                   set_offline : in Boolean) is
   168.    begin
   169.       start_slow_transfer(the_GP, Q_operand, set_offline);
   170.       put_words(the_GP, Q_operand);
   171.       lock_out_relative_addresses(Q_operand);
   172.    end POC;
   173.
   174.    overriding
   175.    procedure POD (the_GP      : in out GP.device;
   176.                   Q_operand   : in KDF9.Q_register;
   177.                   set_offline : in Boolean) is
   178.    begin
   179.       -- See the Manual Appendix 6, §5.2, p.303.
   180.       POC(the_GP, Q_operand, set_offline);
   181.    end POD;
   182.
   183.
   184.    type GP_access is access GP.device;
   185.
   186.    GP0 : GP_access with Warnings => Off;
   187.
   188.    procedure enable (b : in KDF9.buffer_number) is
   189.    begin
   190.       GP0 := new GP.device (number  => b,
   191.                             kind    => GP_kind,
   192.                             unit    => 0,
   193.                             quantum => GP_quantum);
   194.       GP0_number := b;
   195.    end enable;
   196.
   197. end IOC.slow.shift.GP;

Compiling: ../Source/ioc-slow-shift-gp.ads
Source file time stamp: 2020-10-28 18:15:27
Compiled at: 2020-11-12 18:12:12

     1. -- ioc-slow-shift-gp.ads
     2. --
     3. -- Emulation of a Calcomp 564 graph plotter, switched to a tape punch buffer.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package IOC.slow.shift.GP is
    20.
    21.    type device is new IOC.slow.shift.device with private;
    22.
    23.    overriding
    24.    procedure POA (the_GP      : in out GP.device;
    25.                   Q_operand   : in KDF9.Q_register;
    26.                   set_offline : in Boolean);
    27.
    28.    overriding
    29.    procedure POB (the_GP      : in out GP.device;
    30.                   Q_operand   : in KDF9.Q_register;
    31.                   set_offline : in Boolean);
    32.
    33.    overriding
    34.    procedure POC (the_GP      : in out GP.device;
    35.                   Q_operand   : in KDF9.Q_register;
    36.                   set_offline : in Boolean);
    37.
    38.    overriding
    39.    procedure POD (the_GP      : in out GP.device;
    40.                   Q_operand   : in KDF9.Q_register;
    41.                   set_offline : in Boolean);
    42.
    43.    -- the_T_bit_is_set (buffer is switched to graph plotter)
    44.    overriding
    45.    procedure PMB (the_GP      : in out GP.device;
    46.                   Q_operand   : in KDF9.Q_register;
    47.                   set_offline : in Boolean);
    48.
    49.    procedure enable (b : in KDF9.buffer_number);
    50.
    51. private
    52.
    53.    type device is new IOC.slow.shift.device with null record;
    54.
    55.    overriding
    56.    procedure Initialize (the_GP : in out GP.device);
    57.
    58.    overriding
    59.    procedure Finalize (the_GP : in out GP.device);
    60.
    61.    overriding
    62.    procedure do_output_housekeeping (the_GP      : in out GP.device;
    63.                                      size, lifts : in     KDF9.word);
    64.
    65. end IOC.slow.shift.GP;

 197 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/ioc-slow-shift-si.adb
Source file time stamp: 2020-10-29 01:03:50
Compiled at: 2020-11-12 18:12:12

     1. -- ioc-slow-shift-si.adb
     2. --
     3. -- Emulation of a standard interface buffer.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Exceptions;
    20. --
    21. with IOC.equipment;
    22. with tracing;
    23.
    24. use  IOC.equipment;
    25. use  tracing;
    26.
    27. package body IOC.slow.shift.SI is
    28.
    29.    use KDF9_char_sets;
    30.
    31.    overriding
    32.    procedure Initialize (the_SI : in out SI.device) is
    33.    begin
    34.       -- Open the associated file.
    35.       open(IOC.device(the_SI), rd_wr_mode);
    36.    end Initialize;
    37.
    38.    overriding
    39.    procedure PIA (the_SI      : in out SI.device;
    40.                   Q_operand   : in KDF9.Q_register;
    41.                   set_offline : in Boolean) is
    42.    begin
    43.       start_slow_transfer(the_SI, Q_operand, set_offline);
    44.       read(the_SI, Q_operand);
    45.       lock_out_relative_addresses(Q_operand);
    46.    end PIA;
    47.
    48.    overriding
    49.    procedure PIB (the_SI      : in out SI.device;
    50.                   Q_operand   : in KDF9.Q_register;
    51.                   set_offline : in Boolean) is
    52.    begin
    53.       start_slow_transfer(the_SI, Q_operand, set_offline);
    54.       read_to_EM(the_SI, Q_operand);
    55.       lock_out_relative_addresses(Q_operand);
    56.    end PIB;
    57.
    58.    overriding
    59.    procedure PIC (the_SI      : in out SI.device;
    60.                   Q_operand   : in KDF9.Q_register;
    61.                   set_offline : in Boolean) is
    62.    begin
    63.       start_slow_transfer(the_SI, Q_operand, set_offline);
    64.       words_read(the_SI, Q_operand);
    65.       lock_out_relative_addresses(Q_operand);
    66.    end PIC;
    67.
    68.    overriding
    69.    procedure PID (the_SI      : in out SI.device;
    70.                   Q_operand   : in KDF9.Q_register;
    71.                   set_offline : in Boolean) is
    72.    begin
    73.       start_slow_transfer(the_SI, Q_operand, set_offline);
    74.       words_read_to_EM(the_SI, Q_operand);
    75.       lock_out_relative_addresses(Q_operand);
    76.    end PID;
    77.
    78.    overriding
    79.    procedure PIE (the_SI      : in out SI.device;
    80.                   Q_operand   : in KDF9.Q_register;
    81.                   set_offline : in Boolean) is
    82.    begin
    83.       -- as PIA: "parity off" has no effect on the data read in
    84.       PIA(the_SI, Q_operand, set_offline);
    85.    end PIE;
    86.
    87.    overriding
    88.    procedure PIF (the_SI      : in out SI.device;
    89.                   Q_operand   : in KDF9.Q_register;
    90.                   set_offline : in Boolean) is
    91.    begin
    92.       -- as PIB: "parity off" has no effect on the data read in
    93.       PIB(the_SI, Q_operand, set_offline);
    94.    end PIF;
    95.
    96.    overriding
    97.    procedure PIG (the_SI      : in out SI.device;
    98.                   Q_operand   : in KDF9.Q_register;
    99.                   set_offline : in Boolean) is
   100.    begin
   101.       PIC(the_SI, Q_operand, set_offline);
   102.    end PIG;
   103.
   104.    overriding
   105.    procedure PIH (the_SI      : in out SI.device;
   106.                   Q_operand   : in KDF9.Q_register;
   107.                   set_offline : in Boolean) is
   108.    begin
   109.       PID(the_SI, Q_operand, set_offline);
   110.    end PIH;
   111.
   112.    overriding
   113.    procedure PMB (the_SI      : in out SI.device;
   114.                   Q_operand   : in KDF9.Q_register;
   115.                   set_offline : in Boolean) is
   116.    begin
   117.       -- ee9's SI0 always asserts 8 channel mode.
   118.       validate_device(the_SI, Q_operand);
   119.       validate_parity(the_SI);
   120.       deal_with_a_busy_device(the_SI, 13, set_offline);
   121.       the_T_bit_is_set := True;
   122.       take_note_of_test(the_SI.device_name, Q_operand, the_T_bit_is_set);
   123.    end PMB;
   124.
   125.    overriding
   126.    procedure PMC (the_SI      : in out SI.device;
   127.                   Q_operand   : in KDF9.Q_register;
   128.                   set_offline : in Boolean) is
   129.    begin
   130.       PMB(the_SI, Q_operand, set_offline);
   131.    end PMC;
   132.
   133.    overriding
   134.    procedure POA (the_SI      : in out SI.device;
   135.                   Q_operand   : in KDF9.Q_register;
   136.                   set_offline : in Boolean) is
   137.    begin
   138.       start_slow_transfer(the_SI, Q_operand, set_offline);
   139.       write(the_SI, Q_operand);
   140.       lock_out_relative_addresses(Q_operand);
   141.    end POA;
   142.
   143.    overriding
   144.    procedure POB (the_SI      : in out SI.device;
   145.                   Q_operand   : in KDF9.Q_register;
   146.                   set_offline : in Boolean) is
   147.    begin
   148.       start_slow_transfer(the_SI, Q_operand, set_offline);
   149.       write_to_EM(the_SI, Q_operand);
   150.       lock_out_relative_addresses(Q_operand);
   151.    end POB;
   152.
   153.    overriding
   154.    procedure POC (the_SI      : in out SI.device;
   155.                   Q_operand   : in KDF9.Q_register;
   156.                   set_offline : in Boolean) is
   157.    begin
   158.       start_slow_transfer(the_SI, Q_operand, set_offline);
   159.       words_write(the_SI, Q_operand);
   160.       lock_out_relative_addresses(Q_operand);
   161.    end POC;
   162.
   163.    overriding
   164.    procedure POD (the_SI      : in out SI.device;
   165.                   Q_operand   : in KDF9.Q_register;
   166.                   set_offline : in Boolean) is
   167.    begin
   168.       start_slow_transfer(the_SI, Q_operand, set_offline);
   169.       words_write_to_EM(the_SI, Q_operand);
   170.       lock_out_relative_addresses(Q_operand);
   171.    end POD;
   172.
   173.    overriding
   174.    procedure POE (the_SI      : in out SI.device;
   175.                   Q_operand   : in KDF9.Q_register;
   176.                   set_offline : in Boolean) is
   177.    begin
   178.       require_nonnegative_count(Q_operand.M);
   179.       output_a_gap(the_SI, Q_operand, set_offline, word_mode => False, text_mode => False);
   180.    end POE;
   181.
   182.    overriding
   183.    procedure POF (the_SI      : in out SI.device;
   184.                   Q_operand   : in KDF9.Q_register;
   185.                   set_offline : in Boolean) is
   186.    begin
   187.       require_nonnegative_count(Q_operand.M);
   188.       output_a_gap(the_SI, Q_operand, set_offline, word_mode => True, text_mode => False);
   189.    end POF;
   190.
   191.    overriding
   192.    procedure Finalize (the_SI : in out SI.device) is
   193.    begin
   194.       close(
   195.             the_SI,
   196.             "transferred",
   197.             the_SI.byte_count,
   198.             "character" & plurality(the_SI.byte_count)
   199.            );
   200.    end Finalize;
   201.
   202.    type SI_access is access SI.device;
   203.
   204.    SI0 : SI_access with Warnings => Off;
   205.    SI1 : SI_access with Warnings => Off;
   206.
   207.    unit : IOC.unit_number := 0;
   208.
   209.    procedure enable (b : in KDF9.buffer_number) is
   210.       SI_quantum : constant := 1E6 / 50E3;  -- for 50_000 characters per second (a guess) !!
   211.    begin
   212.       case unit is
   213.          when 0 =>
   214.             SI0 := new SI.device (number  => b,
   215.                                   kind    => SI_kind,
   216.                                   unit    => 0,
   217.                                   quantum => SI_quantum);
   218.             SI0_number := b;
   219.          when 1 =>
   220.             SI1 := new SI.device (number  => b,
   221.                                   kind    => SI_kind,
   222.                                   unit    => 1,
   223.                                   quantum => SI_quantum);
   224.             SI1_number := b;
   225.          when others =>
   226.             trap_operator_error("more than 2 SI units specified");
   227.       end case;
   228.       unit := unit + 1;
   229.    end enable;
   230.
   231.    function SI0_is_enabled
   232.    return Boolean
   233.    is (SI0 /= null or SI1 /= null);
   234.
   235. end IOC.slow.shift.SI;

Compiling: ../Source/ioc-slow-shift-si.ads
Source file time stamp: 2020-10-28 16:05:56
Compiled at: 2020-11-12 18:12:12

     1. -- ioc-slow-shift-si.ads
     2. --
     3. -- Emulation of a standard interface buffer.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package IOC.slow.shift.SI is
    20.
    21.    -- The Standard Interface Buffer is the KDF9 device about which least is presently known.
    22.    -- Anecdotal evidence suggests it is an implementation of the British Standard Interface (BSIF).
    23.    -- ee9 implements a best guess as to its functionality, based on the following considerations.
    24.
    25.    -- According to the Manual, Appendix 6.1, p.296,
    26.    --    the Standard Interface Buffer has orders that look very like the union of a TR and a TP.
    27.
    28.    -- However:
    29.
    30.    -- (a) PIE and PIF do a read with "parity off".
    31.    --     I think this relates to a feature of the BSIF,
    32.    --        whereby a source device can omit parity if it de-asserts its "parity valid" signal.
    33.    --     With such a device the KDF9 would need a way of ignoring spurious parity errors.
    34.    -- PIE/PIF act in exactly the same way as PIA/PIB as there will be no such error under ee9.
    35.
    36.    -- (b) PMB and PMC set the Test Register "if 8 channel set".  The BSIF is 8 data bits wide.
    37.    --     I think this signals that the KDF9 should use "character" orders to access all 8 bits,
    38.    --        and I think that the other orders access only the low-order 6 bits of the interface.
    39.    -- ee9 always asserts "8 channel set", as it is always capable of providing 8-bit bytes.
    40.
    41.    -- In any case, 6-bit transfers work in the same way as for paper tape readers and punches.
    42.
    43.    type device is new IOC.slow.shift.device with private;
    44.
    45.    overriding
    46.    procedure PIA (the_SI      : in out SI.device;
    47.                   Q_operand   : in KDF9.Q_register;
    48.                   set_offline : in Boolean);
    49.
    50.    overriding
    51.    procedure PIB (the_SI      : in out SI.device;
    52.                   Q_operand   : in KDF9.Q_register;
    53.                   set_offline : in Boolean);
    54.
    55.    overriding
    56.    procedure PIC (the_SI      : in out SI.device;
    57.                   Q_operand   : in KDF9.Q_register;
    58.                   set_offline : in Boolean);
    59.
    60.    overriding
    61.    procedure PID (the_SI      : in out SI.device;
    62.                   Q_operand   : in KDF9.Q_register;
    63.                   set_offline : in Boolean);
    64.
    65.    overriding
    66.    procedure PIE (the_SI      : in out SI.device;
    67.                   Q_operand   : in KDF9.Q_register;
    68.                   set_offline : in Boolean);
    69.
    70.    overriding
    71.    procedure PIF (the_SI      : in out SI.device;
    72.                   Q_operand   : in KDF9.Q_register;
    73.                   set_offline : in Boolean);
    74.
    75.    overriding
    76.    procedure PIG (the_SI      : in out SI.device;
    77.                   Q_operand   : in KDF9.Q_register;
    78.                   set_offline : in Boolean);
    79.
    80.    overriding
    81.    procedure PIH (the_SI      : in out SI.device;
    82.                   Q_operand   : in KDF9.Q_register;
    83.                   set_offline : in Boolean);
    84.
    85.    overriding
    86.    procedure PMB (the_SI      : in out SI.device;
    87.                   Q_operand   : in KDF9.Q_register;
    88.                   set_offline : in Boolean);
    89.
    90.    overriding
    91.    procedure PMC (the_SI      : in out SI.device;
    92.                   Q_operand   : in KDF9.Q_register;
    93.                   set_offline : in Boolean);
    94.
    95.    overriding
    96.    procedure POA (the_SI      : in out SI.device;
    97.                   Q_operand   : in KDF9.Q_register;
    98.                   set_offline : in Boolean);
    99.
   100.    overriding
   101.    procedure POB (the_SI      : in out SI.device;
   102.                   Q_operand   : in KDF9.Q_register;
   103.                   set_offline : in Boolean);
   104.
   105.    overriding
   106.    procedure POC (the_SI      : in out SI.device;
   107.                   Q_operand   : in KDF9.Q_register;
   108.                   set_offline : in Boolean);
   109.
   110.    overriding
   111.    procedure POD (the_SI      : in out SI.device;
   112.                   Q_operand   : in KDF9.Q_register;
   113.                   set_offline : in Boolean);
   114.
   115.    overriding
   116.    procedure POE (the_SI      : in out SI.device;
   117.                   Q_operand   : in KDF9.Q_register;
   118.                   set_offline : in Boolean);
   119.
   120.    overriding
   121.    procedure POF (the_SI      : in out SI.device;
   122.                   Q_operand   : in KDF9.Q_register;
   123.                   set_offline : in Boolean);
   124.
   125.    procedure enable (b : in KDF9.buffer_number);
   126.
   127.    function SI0_is_enabled
   128.    return Boolean;
   129.
   130. private
   131.
   132.    type device is new IOC.slow.shift.device with null record;
   133.    overriding
   134.    procedure Initialize (the_SI : in out SI.device);
   135.
   136.    overriding
   137.    procedure Finalize (the_SI : in out SI.device);
   138.
   139. end IOC.slow.shift.SI;

 235 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/ioc-slow-unit.adb
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- ioc-slow-unit.adb
     2. --
     3. -- Emulation of the common functionality of "unit record" (i.e. LP, CP or CR) devices.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package body IOC.slow.unit is
    20.
    21.    overriding
    22.    function IO_elapsed_time_total (the_buffer : unit.device)
    23.    return KDF9.us is
    24.    begin
    25.       return IO_elapsed_time(the_buffer, the_buffer.unit_count);
    26.    end IO_elapsed_time_total;
    27.
    28.    overriding
    29.    function atomic_item_count (the_buffer : unit.device;
    30.                                Q_operand  : KDF9.Q_register)
    31.    return KDF9.word is
    32.       pragma Unreferenced(the_buffer);
    33.       pragma Unreferenced(Q_operand);
    34.    begin
    35.       return 1;
    36.    end atomic_item_count;
    37.
    38. end IOC.slow.unit;

Compiling: ../Source/ioc-slow-unit.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- ioc-slow-unit.ads
     2. --
     3. -- Emulation of the common functionality of "unit record" (i.e. LP, CP or CR) devices.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package IOC.slow.unit is
    20.
    21.    --
    22.    -- This is the root type for all unit-record I/O device types.
    23.    --
    24.
    25.    type device is abstract new IOC.slow.device with private;
    26.
    27. private
    28.
    29.    type device is abstract new IOC.slow.device with
    30.       record
    31.          unit_count : KDF9.word := 0;
    32.       end record;
    33.
    34.    overriding
    35.    function IO_elapsed_time_total (the_buffer : unit.device)
    36.    return KDF9.us;
    37.
    38.    overriding
    39.    function atomic_item_count (the_buffer : unit.device;
    40.                                Q_operand  : KDF9.Q_register)
    41.    return KDF9.word;
    42.
    43. end IOC.slow.unit;

 38 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/ioc-slow-unit-cp.adb
Source file time stamp: 2020-10-28 16:29:54
Compiled at: 2020-11-12 18:12:13

     1. -- ioc-unit-cp.adb
     2. --
     3. -- Emulation of a card punch buffer.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with IOC.equipment;
    20.
    21. use  IOC.equipment;
    22.
    23. package body IOC.slow.unit.CP is
    24.
    25.    use KDF9_char_sets;
    26.
    27.    overriding
    28.    procedure Initialize (the_CP : in out CP.device) is
    29.    begin
    30.       open(the_CP, write_mode);
    31.    end Initialize;
    32.
    33.    procedure do_output_housekeeping (the_CP     : in out CP.device;
    34.                                      fetched    : in KDF9.word) is
    35.    begin
    36.       flush(the_CP.stream);
    37.       correct_transfer_time(the_CP, actual_length => 1);
    38.       add_in_the_IO_CPU_time(the_CP, fetched);
    39.    end do_output_housekeeping;
    40.
    41.    procedure write_card (the_CP        : in out CP.device;
    42.                          Q_operand     : in KDF9.Q_register;
    43.                          max_words     : in KDF9.address;
    44.                          writing_to_EM : in Boolean := False) is
    45.       start_address : constant KDF9.address := Q_operand.I;
    46.       end_address   : constant KDF9.address := Q_operand.M;
    47.       size : KDF9.word := 0;
    48.       char : Character;
    49.       byte : KDF9_char_sets.symbol;
    50.    begin
    51.       check_addresses_and_lockouts(start_address, end_address);
    52.    word_loop:
    53.       for w in start_address .. KDF9.address'Min(end_address, start_address+max_words-1) loop
    54.          for c in KDF9_char_sets.symbol_index'Range loop
    55.             byte := fetch_symbol(w, c);
    56.             size := size + 1;
    57.             char := to_CP(byte);
    58.             put_byte(char, the_CP.stream);
    59.             exit word_loop when writing_to_EM and char = KDF9_char_sets.E_M;
    60.          end loop;
    61.       end loop word_loop;
    62.       put_EOL(the_CP.stream);
    63.       the_CP.unit_count := the_CP.unit_count + 1;
    64.       do_output_housekeeping(the_CP, fetched => size);
    65.    end write_card;
    66.
    67.    procedure words_write_card (the_CP        : in out CP.device;
    68.                                Q_operand     : in KDF9.Q_register;
    69.                                max_words     : in KDF9.address;
    70.                                writing_to_EM : in Boolean := False) is
    71.
    72.       start_address : constant KDF9.address := Q_operand.I;
    73.       end_address   : constant KDF9.address := Q_operand.M;
    74.       size : KDF9.word := 0;
    75.       char : Character;
    76.       byte : KDF9_char_sets.symbol;
    77.    begin
    78.       check_addresses_and_lockouts(start_address, end_address);
    79.       for w in start_address .. KDF9.address'Min(end_address, start_address+max_words-1) loop
    80.          byte := KDF9_char_sets.symbol(fetch_word(w) and 8#77#);
    81.          size := size + 1;
    82.          char := to_CP(byte);
    83.          put_byte(char, the_CP.stream);
    84.       exit when writing_to_EM and char = KDF9_char_sets.E_M;
    85.       end loop;
    86.       put_EOL(the_CP.stream);
    87.       the_CP.unit_count := the_CP.unit_count + 1;
    88.       do_output_housekeeping(the_CP, fetched => size);
    89.    end words_write_card;
    90.
    91.    overriding
    92.    procedure POA (the_CP      : in out CP.device;
    93.                   Q_operand   : in KDF9.Q_register;
    94.                   set_offline : in Boolean) is
    95.    begin
    96.       start_slow_transfer(the_CP, Q_operand, set_offline);
    97.       write_card(the_CP, Q_operand, max_words => 20);
    98.       lock_out_relative_addresses(Q_operand);
    99.    end POA;
   100.
   101.    overriding
   102.    procedure POB (the_CP      : in out CP.device;
   103.                   Q_operand   : in KDF9.Q_register;
   104.                   set_offline : in Boolean) is
   105.    begin
   106.       start_slow_transfer(the_CP, Q_operand, set_offline);
   107.       write_card(the_CP, Q_operand, max_words => 20, writing_to_EM => True);
   108.       lock_out_relative_addresses(Q_operand);
   109.    end POB;
   110.
   111.    overriding
   112.    procedure POC (the_CP      : in out CP.device;
   113.                   Q_operand   : in KDF9.Q_register;
   114.                   set_offline : in Boolean) is
   115.    begin
   116.       start_slow_transfer(the_CP, Q_operand, set_offline);
   117.       words_write_card(the_CP, Q_operand, max_words => 160);
   118.       lock_out_relative_addresses(Q_operand);
   119.    end POC;
   120.
   121.    overriding
   122.    procedure POD (the_CP      : in out CP.device;
   123.                   Q_operand   : in KDF9.Q_register;
   124.                   set_offline : in Boolean) is
   125.    begin
   126.       start_slow_transfer(the_CP, Q_operand, set_offline);
   127.       words_write_card(the_CP, Q_operand, max_words => 160, writing_to_EM => True);
   128.       lock_out_relative_addresses(Q_operand);
   129.    end POD;
   130.
   131.    overriding
   132.    procedure POE (the_CP      : in out CP.device;
   133.                   Q_operand   : in KDF9.Q_register;
   134.                   set_offline : in Boolean) is
   135.    begin
   136.       POC(the_CP, Q_operand, set_offline);
   137.    end POE;
   138.
   139.    overriding
   140.    procedure POF (the_CP      : in out CP.device;
   141.                   Q_operand   : in KDF9.Q_register;
   142.                   set_offline : in Boolean) is
   143.    begin
   144.       POA(the_CP, Q_operand, set_offline);
   145.    end POF;
   146.
   147.    overriding
   148.    procedure POG (the_CP      : in out CP.device;
   149.                   Q_operand   : in KDF9.Q_register;
   150.                   set_offline : in Boolean) is
   151.    begin
   152.       start_slow_transfer(the_CP, Q_operand, set_offline);
   153.       write_card(the_CP, Q_operand, max_words => 10, writing_to_EM => False);
   154.       lock_out_relative_addresses(Q_operand);
   155.    end POG;
   156.
   157.    overriding
   158.    procedure POH (the_CP      : in out CP.device;
   159.                   Q_operand   : in KDF9.Q_register;
   160.                   set_offline : in Boolean) is
   161.    begin
   162.       start_slow_transfer(the_CP, Q_operand, set_offline);
   163.       write_card(the_CP, Q_operand, max_words => 10, writing_to_EM => True);
   164.       lock_out_relative_addresses(Q_operand);
   165.    end POH;
   166.
   167.    overriding
   168.    procedure POK (the_CP      : in out CP.device;
   169.                   Q_operand   : in KDF9.Q_register;
   170.                   set_offline : in Boolean) is
   171.    begin
   172.       start_slow_transfer(the_CP, Q_operand, set_offline);
   173.       -- See the Manual, p289.
   174.       words_write_card(the_CP, Q_operand, max_words => 80, writing_to_EM => True);
   175.       lock_out_relative_addresses(Q_operand);
   176.    end POK;
   177.
   178.    overriding
   179.    procedure POL (the_CP      : in out CP.device;
   180.                   Q_operand   : in KDF9.Q_register;
   181.                   set_offline : in Boolean) is
   182.    begin
   183.       start_slow_transfer(the_CP, Q_operand, set_offline);
   184.       -- See the Manual, p289.
   185.       words_write_card(the_CP, Q_operand, max_words => 80, writing_to_EM => False);
   186.       lock_out_relative_addresses(Q_operand);
   187.    end POL;
   188.
   189.    overriding
   190.    procedure Finalize (the_CP : in out CP.device) is
   191.    begin
   192.       close(
   193.             the_CP,
   194.             "punched",
   195.             the_CP.unit_count,
   196.             "card" & plurality(the_CP.unit_count)
   197.            );
   198.    end Finalize;
   199.
   200.    CP_quantum : constant := 1E6 / (300 / 60); -- 300 cards per minute.
   201.
   202.    type CP_access is access CP.device;
   203.
   204.    CP0 : CP_access with Warnings => Off;
   205.    CP1 : CP_access with Warnings => Off;
   206.
   207.    unit : IOC.unit_number := 0;
   208.
   209.    procedure enable (b : in KDF9.buffer_number) is
   210.    begin
   211.       case unit is
   212.          when 0 =>
   213.             CP0 := new CP.device (number  => b,
   214.                                   kind    => CP_kind,
   215.                                   unit    => 0,
   216.                                   quantum => CP_quantum);
   217.             CP0_number := b;
   218.          when 1 =>
   219.             CP1 := new CP.device (number  => b,
   220.                                   kind    => CP_kind,
   221.                                   unit    => 1,
   222.                                   quantum => CP_quantum);
   223.             CP1_number := b;
   224.          when others =>
   225.             trap_operator_error("more than 2 CP units specified");
   226.       end case;
   227.       unit := unit + 1;
   228.    end enable;
   229.
   230. end IOC.slow.unit.CP;

Compiling: ../Source/ioc-slow-unit-cp.ads
Source file time stamp: 2020-09-27 23:55:01
Compiled at: 2020-11-12 18:12:13

     1. -- ioc-unit-cp.ads
     2. --
     3. -- Emulation of a card punch buffer.
     4. -- Card punches are "unit record" devices: they cannot transfer less than a whole card.
     5. --
     6. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     7. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     8. --
     9. -- The ee9 program is free software; you can redistribute it and/or
    10. -- modify it under terms of the GNU General Public License as published
    11. -- by the Free Software Foundation; either version 3, or (at your option)
    12. -- any later version. This program is distributed in the hope that it
    13. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    14. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    15. -- See the GNU General Public License for more details. You should have
    16. -- received a copy of the GNU General Public License distributed with
    17. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    18. --
    19.
    20. package IOC.slow.unit.CP is
    21.
    22.    type device is new IOC.slow.unit.device with private;
    23.
    24.    -- Punch binary mode.
    25.    overriding
    26.    procedure POA (the_CP      : in out CP.device;
    27.                   Q_operand   : in KDF9.Q_register;
    28.                   set_offline : in Boolean);
    29.
    30.    -- Punch binary mode to End Message.
    31.    overriding
    32.    procedure POB (the_CP      : in out CP.device;
    33.                   Q_operand   : in KDF9.Q_register;
    34.                   set_offline : in Boolean);
    35.
    36.    -- Punch binary character mode.
    37.    overriding
    38.    procedure POC (the_CP      : in out CP.device;
    39.                   Q_operand   : in KDF9.Q_register;
    40.                   set_offline : in Boolean);
    41.
    42.    -- Punch binary character mode to End Message.
    43.    overriding
    44.    procedure POD (the_CP      : in out CP.device;
    45.                   Q_operand   : in KDF9.Q_register;
    46.                   set_offline : in Boolean);
    47.
    48.    -- As POC.
    49.    overriding
    50.    procedure POE (the_CP      : in out CP.device;
    51.                   Q_operand   : in KDF9.Q_register;
    52.                   set_offline : in Boolean);
    53.
    54.    -- As POA.
    55.    overriding
    56.    procedure POF (the_CP      : in out CP.device;
    57.                   Q_operand   : in KDF9.Q_register;
    58.                   set_offline : in Boolean);
    59.
    60.    -- Punch alphanumeric mode.
    61.    overriding
    62.    procedure POG (the_CP      : in out CP.device;
    63.                   Q_operand   : in KDF9.Q_register;
    64.                   set_offline : in Boolean);
    65.
    66.    -- Punch alphanumeric mode to End Message.
    67.    overriding
    68.    procedure POH (the_CP      : in out CP.device;
    69.                   Q_operand   : in KDF9.Q_register;
    70.                   set_offline : in Boolean);
    71.
    72.    -- Punch alphanumeric character mode to End Message.
    73.    overriding
    74.    procedure POK (the_CP      : in out CP.device;
    75.                   Q_operand   : in KDF9.Q_register;
    76.                   set_offline : in Boolean);
    77.
    78.    -- Punch alphanumeric character mode.
    79.    overriding
    80.    procedure POL (the_CP      : in out CP.device;
    81.                   Q_operand   : in KDF9.Q_register;
    82.                   set_offline : in Boolean);
    83.
    84.    procedure enable (b : in KDF9.buffer_number);
    85.
    86. private
    87.
    88.    type device is new IOC.slow.unit.device with null record;
    89.
    90.    overriding
    91.    procedure Initialize (the_CP : in out CP.device);
    92.
    93.    overriding
    94.    procedure Finalize (the_CP : in out CP.device);
    95.
    96. end IOC.slow.unit.CP;

 230 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/ioc-slow-unit-cr.adb
Source file time stamp: 2020-10-28 16:30:03
Compiled at: 2020-11-12 18:12:13

     1. -- ioc-unit-cr.adb
     2. --
     3. -- Emulation of a card reader buffer.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with IOC.equipment;
    20. with tracing;
    21.
    22. use  IOC.equipment;
    23. use  tracing;
    24.
    25. package body IOC.slow.unit.CR is
    26.
    27.    use KDF9_char_sets;
    28.
    29.    overriding
    30.    procedure Initialize (the_CR : in out CR.device) is
    31.    begin
    32.       open(the_CR, read_mode);
    33.    end Initialize;
    34.
    35.    blank_card : constant String(max_card_columns) := (others => SP);
    36.
    37.    procedure get_card_image (the_CR         : in out CR.device;
    38.                               size          : in out KDF9.word;
    39.                               max_columns   : in KDF9.address;
    40.                               reading_to_EM : in Boolean := False) is
    41.       max  : constant Positive := Positive(max_columns);
    42.       char : Character;
    43.    begin
    44.       -- Clear out the card image field.
    45.       the_CR.card_image(1..max) := blank_card(1..max);
    46.       -- Fill as much of the card image as possible with the next data line, padded out with
    47.       --    blanks, so that it is unnecessary to type all 80 or 160 characters.
    48.       -- For transfers to End Message, a line terminator must follow the E_M.
    49.       for i in 1 .. max loop
    50.          get_char_from_stream (char, the_CR);
    51.          size := size + 1;
    52.       exit when char = LF;
    53.          the_CR.card_image(i) := char;
    54.       exit when reading_to_EM and char = KDF9_char_sets.E_M;
    55.       end loop;
    56.       if char /= KDF9_char_sets.E_M then  -- The whole card was read.
    57.          size := KDF9.word(max);
    58.       end if;
    59.       the_CR.unit_count := the_CR.unit_count + 1;
    60.       -- Discard excess characters in the current data line.
    61.       while char /= LF loop
    62.          get_char_from_stream (char, the_CR);  -- N.B. do not update size for discards.
    63.       end loop;
    64.    exception
    65.       when end_of_stream =>
    66.          flush(the_CR.stream);
    67.          the_CR.is_abnormal := True;
    68.          raise;
    69.    end get_card_image;
    70.
    71.    procedure do_input_housekeeping (the_CR  : in out CR.device;
    72.                                     fetched : in KDF9.word) is
    73.    begin
    74.       add_in_the_IO_CPU_time(the_CR, fetched);
    75.       correct_transfer_time(the_CR, actual_length => 1);
    76.    end do_input_housekeeping;
    77.
    78.    procedure read_card (the_CR        : in out CR.device;
    79.                         Q_operand     : in KDF9.Q_register;
    80.                         max_words     : in KDF9.address;
    81.                         reading_to_EM : in Boolean := False) is
    82.       start_address : constant KDF9.address := Q_operand.I;
    83.       end_address   : constant KDF9.address := Q_operand.M;
    84.       size : KDF9.word := 0;
    85.       next : Natural := 0;
    86.       char : Character;
    87.       byte : KDF9_char_sets.symbol;
    88.    begin
    89.       check_addresses_and_lockouts(start_address, end_address);
    90.       get_card_image(the_CR, size, max_columns => max_words*8);
    91.    word_loop:
    92.       for w in start_address .. KDF9.address'Min(end_address, start_address+max_words-1) loop
    93.          for c in KDF9_char_sets.symbol_index'Range loop
    94.             next := next + 1;
    95.             char := the_CR.card_image(next);
    96.             byte := CR_in(char);
    97.             store_symbol(byte, w, c);
    98.             if reading_to_EM and byte = KDF9_char_sets.End_Message then
    99.                for d in 1 .. 7-c loop
   100.                   store_symbol(KDF9_char_sets.Blank_Space, w, c+d);
   101.                end loop;
   102.                exit word_loop;
   103.             end if;
   104.          end loop;
   105.       end loop word_loop;
   106.       do_input_housekeeping(the_CR, size);
   107.    exception
   108.       when end_of_stream =>
   109.          do_input_housekeeping(the_CR, size);
   110.    end read_card;
   111.
   112.    procedure words_read_card (the_CR        : in out CR.device;
   113.                               Q_operand     : in KDF9.Q_register;
   114.                               max_words     : in KDF9.address;
   115.                               reading_to_EM : in Boolean := False) is
   116.       start_address : constant KDF9.address := Q_operand.I;
   117.       end_address   : constant KDF9.address := Q_operand.M;
   118.       size : KDF9.word := 0;
   119.       next : Natural := 0;
   120.       char : Character;
   121.       word : KDF9.word;
   122.    begin
   123.       check_addresses_and_lockouts(start_address, end_address);
   124.       get_card_image(the_CR, size, max_columns => max_words);
   125.       if the_CR.is_abnormal then return; end if;
   126.       for w in start_address .. KDF9.address'Min(end_address, start_address+max_words-1) loop
   127.          next := next + 1;
   128.          char := the_CR.card_image(next);
   129.          word := KDF9.word(CR_in(char));
   130.          store_word(word, w);
   131.       exit when reading_to_EM and char = KDF9_char_sets.E_M;
   132.       end loop;
   133.       add_in_the_IO_CPU_time(the_CR, size);
   134.       correct_transfer_time(the_CR, actual_length => 1);
   135.    exception
   136.       when end_of_stream =>
   137.          flush(the_CR.stream);
   138.          add_in_the_IO_CPU_time(the_CR, size);
   139.          correct_transfer_time(the_CR, actual_length => 1);
   140.    end words_read_card;
   141.
   142.    overriding
   143.    procedure PIA (the_CR      : in out CR.device;
   144.                   Q_operand   : in KDF9.Q_register;
   145.                   set_offline : in Boolean) is
   146.    begin
   147.       start_slow_transfer(the_CR, Q_operand, set_offline);
   148.       read_card(the_CR, Q_operand, max_words => 20);
   149.       lock_out_relative_addresses(Q_operand);
   150.    end PIA;
   151.
   152.    overriding
   153.    procedure PIB (the_CR      : in out CR.device;
   154.                   Q_operand   : in KDF9.Q_register;
   155.                   set_offline : in Boolean) is
   156.    begin
   157.       start_slow_transfer(the_CR, Q_operand, set_offline);
   158.       read_card(the_CR, Q_operand, max_words => 20, reading_to_EM => True);
   159.       lock_out_relative_addresses(Q_operand);
   160.    end PIB;
   161.
   162.    overriding
   163.    procedure PIC (the_CR      : in out CR.device;
   164.                   Q_operand   : in KDF9.Q_register;
   165.                   set_offline : in Boolean) is
   166.    begin
   167.       start_slow_transfer(the_CR, Q_operand, set_offline);
   168.       words_read_card(the_CR, Q_operand, max_words => 160);
   169.       lock_out_relative_addresses(Q_operand);
   170.    end PIC;
   171.
   172.    overriding
   173.    procedure PID (the_CR      : in out CR.device;
   174.                   Q_operand   : in KDF9.Q_register;
   175.                   set_offline : in Boolean) is
   176.    begin
   177.       start_slow_transfer(the_CR, Q_operand, set_offline);
   178.       words_read_card(the_CR, Q_operand, max_words => 160, reading_to_EM => True);
   179.       lock_out_relative_addresses(Q_operand);
   180.    end PID;
   181.
   182.    overriding
   183.    procedure PIE (the_CR      : in out CR.device;
   184.                   Q_operand   : in KDF9.Q_register;
   185.                   set_offline : in Boolean) is
   186.    begin
   187.       start_slow_transfer(the_CR, Q_operand, set_offline);
   188.       read_card(the_CR, Q_operand, max_words => 10);
   189.       lock_out_relative_addresses(Q_operand);
   190.    end PIE;
   191.
   192.    overriding
   193.    procedure PIF (the_CR      : in out CR.device;
   194.                   Q_operand   : in KDF9.Q_register;
   195.                   set_offline : in Boolean) is
   196.    begin
   197.       start_slow_transfer(the_CR, Q_operand, set_offline);
   198.       read_card(the_CR, Q_operand, max_words => 10, reading_to_EM => True);
   199.       lock_out_relative_addresses(Q_operand);
   200.    end PIF;
   201.
   202.    overriding
   203.    procedure PIG (the_CR      : in out CR.device;
   204.                   Q_operand   : in KDF9.Q_register;
   205.                   set_offline : in Boolean) is
   206.    begin
   207.       start_slow_transfer(the_CR, Q_operand, set_offline);
   208.       words_read_card(the_CR, Q_operand, max_words => 80);
   209.       lock_out_relative_addresses(Q_operand);
   210.    end PIG;
   211.
   212.    overriding
   213.    procedure PIH (the_CR      : in out CR.device;
   214.                   Q_operand   : in KDF9.Q_register;
   215.                   set_offline : in Boolean) is
   216.    begin
   217.       start_slow_transfer(the_CR, Q_operand, set_offline);
   218.       words_read_card(the_CR, Q_operand, max_words => 80, reading_to_EM => True);
   219.       lock_out_relative_addresses(Q_operand);
   220.    end PIH;
   221.
   222.    -- the_T_bit_is_set := (RECHECK switch is OFF). {It always is nowadays!}
   223.    overriding
   224.    procedure PMB (the_CR      : in out CR.device;
   225.                   Q_operand   : in KDF9.Q_register;
   226.                   set_offline : in Boolean) is
   227.    begin
   228.       validate_device(the_CR, Q_operand);
   229.       validate_parity(the_CR);
   230.       deal_with_a_busy_device(the_CR, 13, set_offline);
   231.       the_T_bit_is_set := True;
   232.       take_note_of_test(the_CR.device_name, Q_operand, the_T_bit_is_set);
   233.    end PMB;
   234.
   235.    overriding
   236.    procedure Finalize (the_CR : in out CR.device) is
   237.    begin
   238.       close(
   239.             the_CR,
   240.             "read",
   241.             the_CR.unit_count,
   242.             "card" & plurality(the_CR.unit_count)
   243.            );
   244.    end Finalize;
   245.
   246.    CR_quantum : constant := 1E6 / (600 / 60); -- 600 cards per minute.
   247.
   248.    type CR_access is access CR.device;
   249.
   250.    CR0 : CR_access with Warnings => Off;
   251.    CR1 : CR_access with Warnings => Off;
   252.
   253.    unit : IOC.unit_number := 0;
   254.
   255.    procedure enable (b : in KDF9.buffer_number) is
   256.    begin
   257.       case unit is
   258.          when 0 =>
   259.             CR0 := new CR.device (number  => b,
   260.                                   kind    => CR_kind,
   261.                                   unit    => 0,
   262.                                   quantum => CR_quantum);
   263.             CR0_number := b;
   264.          when 1 =>
   265.             CR1 := new CR.device (number  => b,
   266.                                   kind    => CR_kind,
   267.                                   unit    => 1,
   268.                                   quantum => CR_quantum);
   269.             CR1_number := b;
   270.          when others =>
   271.             trap_operator_error("more than 2 CR units specified");
   272.       end case;
   273.       unit := unit + 1;
   274.    end enable;
   275.
   276. end IOC.slow.unit.CR;

Compiling: ../Source/ioc-slow-unit-cr.ads
Source file time stamp: 2020-09-27 23:54:51
Compiled at: 2020-11-12 18:12:13

     1. -- ioc-slow-unit-cr.ads
     2. --
     3. -- Emulation of a card reader buffer.
     4. -- Card readers are "unit record" devices: they cannot transfer less than a whole card.
     5. --
     6. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     7. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     8. --
     9. -- The ee9 program is free software; you can redistribute it and/or
    10. -- modify it under terms of the GNU General Public License as published
    11. -- by the Free Software Foundation; either version 3, or (at your option)
    12. -- any later version. This program is distributed in the hope that it
    13. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    14. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    15. -- See the GNU General Public License for more details. You should have
    16. -- received a copy of the GNU General Public License distributed with
    17. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    18. --
    19.
    20. package IOC.slow.unit.CR is
    21.
    22.    type device is new IOC.slow.unit.device with private;
    23.
    24.    -- Binary (undecoded) read
    25.    overriding
    26.    procedure PIA (the_CR      : in out CR.device;
    27.                   Q_operand   : in KDF9.Q_register;
    28.                   set_offline : in Boolean);
    29.
    30.    -- Binary (undecoded) read to End_Message
    31.    overriding
    32.    procedure PIB (the_CR      : in out CR.device;
    33.                   Q_operand   : in KDF9.Q_register;
    34.                   set_offline : in Boolean);
    35.
    36.    -- Binary (undecoded) character read
    37.    overriding
    38.    procedure PIC (the_CR      : in out CR.device;
    39.                   Q_operand   : in KDF9.Q_register;
    40.                   set_offline : in Boolean);
    41.
    42.    -- Binary (undecoded) character read to End_Message
    43.    overriding
    44.    procedure PID (the_CR      : in out CR.device;
    45.                   Q_operand   : in KDF9.Q_register;
    46.                   set_offline : in Boolean);
    47.
    48.    -- Alphanumeric (decoded) read
    49.    overriding
    50.    procedure PIE (the_CR      : in out CR.device;
    51.                   Q_operand   : in KDF9.Q_register;
    52.                   set_offline : in Boolean);
    53.
    54.    -- Alphanumeric (decoded) read to End_Message
    55.    overriding
    56.    procedure PIF (the_CR      : in out CR.device;
    57.                   Q_operand   : in KDF9.Q_register;
    58.                   set_offline : in Boolean);
    59.
    60.    -- Alphanumeric (decoded) character read
    61.    overriding
    62.    procedure PIG (the_CR      : in out CR.device;
    63.                   Q_operand   : in KDF9.Q_register;
    64.                   set_offline : in Boolean);
    65.
    66.    -- Alphanumeric (decoded) character read to End_Message
    67.    overriding
    68.    procedure PIH (the_CR      : in out CR.device;
    69.                   Q_operand   : in KDF9.Q_register;
    70.                   set_offline : in Boolean);
    71.
    72.    -- the_T_bit_is_set := (RECHECK switch is OFF)
    73.    overriding
    74.    procedure PMB (the_CR      : in out CR.device;
    75.                   Q_operand   : in KDF9.Q_register;
    76.                   set_offline : in Boolean);
    77.
    78.    procedure enable (b : in KDF9.buffer_number);
    79.
    80. private
    81.
    82.    subtype max_card_columns is Positive range 1 .. 160;
    83.
    84.    type device is new IOC.slow.unit.device with
    85.       record
    86.          card_image : String(max_card_columns);
    87.       end record;
    88.
    89.    overriding
    90.    procedure Initialize (the_CR : in out CR.device);
    91.
    92.    overriding
    93.    procedure Finalize (the_CR : in out CR.device);
    94.
    95. end IOC.slow.unit.CR;

 276 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/ioc-slow-unit-lp.adb
Source file time stamp: 2020-10-28 16:30:09
Compiled at: 2020-11-12 18:12:13

     1. -- ioc-slow-unit-lp.adb
     2. --
     3. -- Emulation of a lineprinter buffer.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with IOC.equipment;
    20.
    21. use  IOC.equipment;
    22.
    23. package body IOC.slow.unit.LP is
    24.
    25.    use KDF9_char_sets;
    26.
    27.    overriding
    28.    procedure Initialize (the_LP : in out LP.device) is
    29.    begin
    30.       open(the_LP, write_mode);
    31.    end Initialize;
    32.
    33.    max_LP_line_length : constant := 160;  -- This is a hardware limit.
    34.    max_LP_page_length : constant :=  66;  -- This is the length of a page of standard stationery.
    35.
    36.    -- The number of lines traversed by paper motion with a standard control loop.
    37.    function skip_length (the_LP : LP.device; symbol : KDF9_char_sets.symbol)
    38.    return KDF9.word
    39.    is (
    40.        if symbol = KDF9_char_sets.Page_Change
    41.        then max_LP_page_length - the_LP.unit_count mod max_LP_page_length
    42.        else 1
    43.       );
    44.
    45.    procedure do_output_housekeeping (the_LP   : in out LP.device;
    46.                                      old_count,
    47.                                      fetched  : in KDF9.word) is
    48.    begin
    49.       flush(the_LP.stream);
    50.       correct_transfer_time(the_LP, IO_elapsed_time(the_LP, the_LP.unit_count-old_count));
    51.       add_in_the_IO_CPU_time(the_LP, fetched);
    52.    end do_output_housekeeping;
    53.
    54.    next_column : Natural := 0;
    55.
    56.    procedure print (symbol : in KDF9_char_sets.symbol; the_LP : in out LP.device) is
    57.       char : constant Character := to_LP(symbol);
    58.    begin
    59.       if char /= KDF9_char_sets.W_F then
    60.          if symbol in KDF9_char_sets.Line_Shift | KDF9_char_sets.Page_Change then
    61.             the_LP.unit_count := the_LP.unit_count + skip_length(the_LP, symbol);
    62.             put_char(char, the_LP.stream);
    63.             next_column := 0;
    64.          elsif next_column < max_LP_line_length then
    65.             next_column := next_column + 1;
    66.             put_char(char, the_LP.stream);
    67.          end if;
    68.       end if;
    69.    end print;
    70.
    71.    -- It is unclear what should happen if more than max_LP_line_length printable characters
    72.    --    are sent to the printer before a LS or PC character, which empties the print matrix.
    73.    -- ee9 simply ignores the excess.
    74.
    75.    procedure put_symbols (the_LP        : in out LP.device;
    76.                           Q_operand     : in KDF9.Q_register;
    77.                           writing_to_EM : in Boolean) is
    78.       start_address : constant KDF9.address := Q_operand.I;
    79.       end_address   : constant KDF9.address := Q_operand.M;
    80.       count         : constant KDF9.word := the_LP.unit_count;
    81.       size   : KDF9.word := 0;
    82.       symbol : KDF9_char_sets.symbol;
    83.    begin
    84.       check_addresses_and_lockouts(start_address, end_address);
    85.    word_loop:
    86.       for w in start_address .. end_address loop
    87.          for c in KDF9_char_sets.symbol_index'Range loop
    88.             symbol := fetch_symbol(w, c);
    89.             size := size + 1;
    90.       -- Is this what should happen transfers on EM, leaving the print matrix ready for more data ??
    91.       exit word_loop when writing_to_EM and symbol = KDF9_char_sets.End_Message;
    92.             print(symbol, the_LP);
    93.          end loop;
    94.       end loop word_loop;
    95.       do_output_housekeeping(the_LP, old_count => count, fetched => size);
    96.    end put_symbols;
    97.
    98.    -- LPQq
    99.    overriding
   100.    procedure POA (the_LP      : in out LP.device;
   101.                   Q_operand   : in KDF9.Q_register;
   102.                   set_offline : in Boolean) is
   103.    begin
   104.       start_slow_transfer(the_LP, Q_operand, set_offline);
   105.       put_symbols(the_LP, Q_operand, writing_to_EM => False);
   106.       lock_out_relative_addresses(Q_operand);
   107.    end POA;
   108.
   109.    -- LPEQq
   110.    overriding
   111.    procedure POB (the_LP      : in out LP.device;
   112.                   Q_operand   : in KDF9.Q_register;
   113.                   set_offline : in Boolean) is
   114.    begin
   115.       start_slow_transfer(the_LP, Q_operand, set_offline);
   116.       put_symbols(the_LP, Q_operand, writing_to_EM => True);
   117.       lock_out_relative_addresses(Q_operand);
   118.    end POB;
   119.
   120.    procedure put_words (the_LP        : in out LP.device;
   121.                         Q_operand     : in KDF9.Q_register;
   122.                         writing_to_EM : in Boolean) is
   123.       start_address : constant KDF9.address := Q_operand.I;
   124.       end_address   : constant KDF9.address := Q_operand.M;
   125.       count         : constant KDF9.word := the_LP.unit_count;
   126.       size   : KDF9.word := 0;
   127.       symbol : KDF9_char_sets.symbol;
   128.    begin
   129.       check_addresses_and_lockouts(start_address, end_address);
   130.       for w in start_address .. end_address loop
   131.          symbol := KDF9_char_sets.symbol(fetch_word(w) and 8#77#);
   132.          size := size + 1;
   133.       -- Is this what should happen transfers on EM, leaving the print matrix ready for more data ??
   134.       exit when writing_to_EM and symbol = KDF9_char_sets.End_Message;
   135.          print(symbol, the_LP);
   136.       end loop;
   137.       do_output_housekeeping(the_LP, old_count => count, fetched => size);
   138.    end put_words;
   139.
   140.    -- Character write ?? Usercode Digest and Manual conflict!
   141.    overriding
   142.    procedure POC (the_LP      : in out LP.device;
   143.                   Q_operand   : in KDF9.Q_register;
   144.                   set_offline : in Boolean) is
   145.    begin
   146.       start_slow_transfer(the_LP, Q_operand, set_offline);
   147.       put_words(the_LP, Q_operand, writing_to_EM => False);
   148.       lock_out_relative_addresses(Q_operand);
   149.    end POC;
   150.
   151.    -- Character write to End_Message ?? Usercode Digest and Manual conflict!
   152.    overriding
   153.    procedure POD (the_LP      : in out LP.device;
   154.                   Q_operand   : in KDF9.Q_register;
   155.                   set_offline : in Boolean) is
   156.    begin
   157.       start_slow_transfer(the_LP, Q_operand, set_offline);
   158.       put_words(the_LP, Q_operand, writing_to_EM => True);
   159.       lock_out_relative_addresses(Q_operand);
   160.    end POD;
   161.
   162.    overriding
   163.    procedure Finalize (the_LP : in out LP.device) is
   164.    begin
   165.       close(
   166.             the_LP,
   167.             "printed",
   168.             the_LP.unit_count,
   169.             "line" & plurality(the_LP.unit_count)
   170.            );
   171.    end Finalize;
   172.
   173.    LP_quantum : constant := 1E6 / (900 / 60);  -- 900 lines per minute.
   174.
   175.    type LP_access is access LP.device;
   176.
   177.    LP0 : LP_access with Warnings => Off;
   178.    LP1 : LP_access with Warnings => Off;
   179.
   180.    unit : IOC.unit_number := 0;
   181.
   182.    procedure enable (b : in KDF9.buffer_number) is
   183.    begin
   184.       case unit is
   185.          when 0 =>
   186.             LP0 := new LP.device (number  => b,
   187.                                   kind    => LP_kind,
   188.                                   unit    => 0,
   189.                                   quantum => LP_quantum);
   190.             LP0_number := b;
   191.          when 1 =>
   192.             LP1 := new LP.device (number  => b,
   193.                                   kind    => LP_kind,
   194.                                   unit    => 1,
   195.                                   quantum => LP_quantum);
   196.             LP1_number := b;
   197.          when others =>
   198.             trap_operator_error("more than 2 LP units specified");
   199.       end case;
   200.       unit := unit + 1;
   201.    end enable;
   202.
   203. end IOC.slow.unit.LP;

Compiling: ../Source/ioc-slow-unit-lp.ads
Source file time stamp: 2020-09-27 23:54:28
Compiled at: 2020-11-12 18:12:13

     1. -- ioc-slow-unit-lp.ads
     2. --
     3. -- Emulation of a lineprinter buffer.
     4. -- Lineprinters are "unit record" devices: they cannot transfer less than a whole line.
     5. --
     6. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     7. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     8. --
     9. -- The ee9 program is free software; you can redistribute it and/or
    10. -- modify it under terms of the GNU General Public License as published
    11. -- by the Free Software Foundation; either version 3, or (at your option)
    12. -- any later version. This program is distributed in the hope that it
    13. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    14. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    15. -- See the GNU General Public License for more details. You should have
    16. -- received a copy of the GNU General Public License distributed with
    17. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    18. --
    19.
    20. package IOC.slow.unit.LP is
    21.
    22.    type device is new IOC.slow.unit.device with private;
    23.
    24.    -- LPQq
    25.    overriding
    26.    procedure POA (the_LP      : in out LP.device;
    27.                   Q_operand   : in KDF9.Q_register;
    28.                   set_offline : in Boolean);
    29.
    30.    -- LPEQq
    31.    overriding
    32.    procedure POB (the_LP      : in out LP.device;
    33.                   Q_operand   : in KDF9.Q_register;
    34.                   set_offline : in Boolean);
    35.
    36.    -- Character write ??
    37.    overriding
    38.    procedure POC (the_LP      : in out LP.device;
    39.                   Q_operand   : in KDF9.Q_register;
    40.                   set_offline : in Boolean);
    41.
    42.    -- Character write to End_Message ??
    43.    overriding
    44.    procedure POD (the_LP      : in out LP.device;
    45.                   Q_operand   : in KDF9.Q_register;
    46.                   set_offline : in Boolean);
    47.
    48.    procedure enable (b : in KDF9.buffer_number);
    49.
    50. private
    51.
    52.    type device is new IOC.slow.unit.device with null record;
    53.
    54.    overriding
    55.    procedure Initialize (the_LP : in out LP.device);
    56.
    57.    overriding
    58.    procedure Finalize (the_LP : in out LP.device);
    59.
    60. end IOC.slow.unit.LP;

 203 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/os_specifics.adb
Source file time stamp: 2020-11-12 18:12:11
Compiled at: 2020-11-12 18:12:13

     1. -- OS_specifics.adb
     2. --
     3. -- Specific feature values and operation for the console terminal streams.
     4. -- This is the Linux, macOS and UNIX version.
     5. --
     6. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     7. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     8. --
     9. -- The ee9 program is free software; you can redistribute it and/or
    10. -- modify it under terms of the GNU General Public License as published
    11. -- by the Free Software Foundation; either version 3, or (at your option)
    12. -- any later version. This program is distributed in the hope that it
    13. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    14. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    15. -- See the GNU General Public License for more details. You should have
    16. -- received a copy of the GNU General Public License distributed with
    17. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    18. --
    19.
    20. package body OS_specifics is
    21.
    22.    procedure make_transparent (fd : in Integer) is null;
    23.
    24.    function EOL
    25.    return String
    26.    is (1 => Character'Val(16#0A#));
    27.
    28.    function UI_in_name
    29.    return String
    30.    is ("/dev/tty");
    31.
    32.    function UI_out_name
    33.    return String
    34.    is ("/dev/tty");
    35.
    36. end OS_specifics;

Compiling: ../Source/os_specifics.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- OS_specifics.ads
     2. --
     3. -- Specific feature values and operation for the console terminal streams.
     4. -- This specification is the same for Windows, Linux, macOS and UNIX versions of ee9.
     5. --
     6. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     7. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     8. --
     9. -- The ee9 program is free software; you can redistribute it and/or
    10. -- modify it under terms of the GNU General Public License as published
    11. -- by the Free Software Foundation; either version 3, or (at your option)
    12. -- any later version. This program is distributed in the hope that it
    13. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    14. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    15. -- See the GNU General Public License for more details. You should have
    16. -- received a copy of the GNU General Public License distributed with
    17. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    18. --
    19.
    20. package OS_specifics is
    21.
    22.    -- make_transparent sets the "binary" mode of I/O on Windows/Cygwin.
    23.    -- It does nothing on UNIX-family systems, where no such precaution is necessary.
    24.    procedure make_transparent (fd : in Integer);
    25.
    26.    -- UI_in_name returns the interactive input device name appropriate to the host OS, e.g.:
    27.    -- "/dev/tty"  for macOS/UNIX/Linux,
    28.    -- "CONIN$" for Windows.
    29.    function UI_in_name
    30.    return String;
    31.
    32.    -- UI_out_name returns the interactive output device name appropriate to the host OS, e.g.:
    33.    -- "/dev/tty"  for macOS/UNIX/Linux,
    34.    -- "CONOUT$" for Windows.
    35.    function UI_out_name
    36.    return String;
    37.
    38.    -- EOL returns the appropriate line terminator for the selected host OS, e.g.:
    39.    -- LF for macOS/UNIX/Linux,
    40.    -- CRLF for Windows.
    41.    function EOL
    42.    return String;
    43.
    44. end OS_specifics;

 36 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/value_of.adb
Source file time stamp: 2020-08-06 23:14:23
Compiled at: 2020-11-12 18:12:13

     1. -- value_of.adb
     2. --
     3. -- Get the value of an environment variable.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Environment_Variables;
    20.
    21. function value_of (name, default : String)
    22. return String is
    23.    value : constant String := Ada.Environment_Variables.Value(name, "");
    24. begin
    25.    return (if value = "" then default else value);
    26. end value_of;

 26 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/kdf9-tod_clock.adb
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- kdf9-tod_clock.adb
     2. --
     3. -- functions that implement timing for Director emulation.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Calendar;
    20. with Ada.Calendar.Time_Zones;
    21. with Ada.Calendar.Formatting;
    22.
    23. use  Ada.Calendar;
    24. use  Ada.Calendar.Time_Zones;
    25. use  Ada.Calendar.Formatting;
    26.
    27. package body KDF9.TOD_clock is
    28.
    29.    function todays_date_28n_years_ago
    30.    return KDF9.word is
    31.
    32.       zero  : constant KDF9.word := 8#20#;
    33.       slash : constant KDF9.word := 8#17#;
    34.       today : constant Ada.Calendar.Time := Ada.Calendar.Clock;
    35.
    36.       year, month, day, hour, minute, second, sub_second : KDF9.word;
    37.
    38.       -- For values of i in 0..99, return two 6-bit KDF9 decimal digits.
    39.       function as_2_digits (i : KDF9.word)
    40.       return KDF9.word
    41.       is ((i/10 + zero)*64 or (i mod 10 + zero));
    42.
    43.    begin  -- todays_date_28n_years_ago
    44.       Split(today,
    45.             Year_Number(year),
    46.             Month_Number(month),
    47.             Day_Number(day),
    48.             Hour_Number(hour),
    49.             Minute_Number(minute),
    50.             Second_Number(second),
    51.             Second_Duration(sub_second),
    52.             Time_Zone => UTC_Time_Offset(today)
    53.            );
    54.       loop  -- Repeat n > 0 times, assuming no time travel into the past!
    55.          year := year - 28;
    56.       exit when year < 2000;
    57.       end loop;
    58.       return (as_2_digits(day)*64   or slash) * 64**5  -- DD/.....
    59.           or (as_2_digits(month)*64 or slash) * 64**2  --    MM/..
    60.           or (as_2_digits((year) mod 100));            --       YY
    61.    end todays_date_28n_years_ago;
    62.
    63.    function the_time_of_day
    64.    return KDF9.us is
    65.       today : constant Ada.Calendar.Time := Ada.Calendar.Clock;
    66.       year, month, day, hour, minute, second, sub_second : KDF9.word;
    67.    begin
    68.       Split(today,
    69.             Year_Number(year),
    70.             Month_Number(month),
    71.             Day_Number(day),
    72.             Hour_Number(hour),
    73.             Minute_Number(minute),
    74.             Second_Number(second),
    75.             Second_Duration(sub_second),
    76.             Time_Zone => UTC_Time_Offset(today)
    77.            );
    78.       return KDF9.us(hour*3600 + minute*60 + second) * 1_000_000;
    79.    end the_time_of_day;
    80.
    81. end KDF9.TOD_clock;

Compiling: ../Source/kdf9-tod_clock.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- kdf9-tod_clock.ads
     2. --
     3. -- functions that implement timing for Director emulation.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package KDF9.TOD_clock is
    20.
    21.    -- The date a multiple of 28 years ago has the same day/date correspondence as today.
    22.    -- To avoid exposing KDF9's lack of Y2K compliance, ee9 uses such a date before 2000.
    23.    -- 8-)
    24.    -- todays_date_28n_years_ago returns a word of 8 KDF9 characters in the format DD/MM/YY.
    25.
    26.    function todays_date_28n_years_ago
    27.    return KDF9.word;
    28.
    29.    -- The time in microseconds since midnight.
    30.    function the_time_of_day
    31.    return KDF9.us;
    32.
    33. end KDF9.TOD_clock;

 81 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/postscript.adb
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- postscript.adb
     2. --
     3. -- Elementary Encapsulated PostScript (EPS) line drawing.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with formatting;
    20. with KDF9;
    21.
    22. use  formatting;
    23. use  KDF9;
    24.
    25. package body postscript is
    26.
    27.    use host_IO;
    28.
    29.    function image (p : postscript.point)
    30.    return String
    31.    is ("<" & trimmed(p.x'Image) & ", " & trimmed(p.y'Image) & ">");
    32.
    33.
    34.    -- A path is a series of vectors v1, v2, ..., vn such that the last point
    35.    --    of vi is the same as the first point of v(i+1),
    36.    --       and v1, ..., vn are all drawn in the same colour.
    37.    -- A new path is started by a jump to a non-contiguous point or by a change of pen colour.
    38.
    39.    there_is_an_open_path      : Boolean := False;
    40.    the_last_point_in_the_path : postscript.point := (0, 0);
    41.
    42.    -- The bounding box limits are set from the value of maximum_offset at the end of the plot.
    43.    maximum_offset             : postscript.point := (0, 0);
    44.
    45.    procedure ensure_separation (stream : in out host_IO.stream) is
    46.    begin
    47.       if column(stream) > 0 then
    48.          put_byte(' ', stream);
    49.       end if;
    50.    end ensure_separation;
    51.
    52.    procedure put (stream : in out host_IO.stream; PS_text : String) is
    53.    begin
    54.       put_bytes(PS_text, stream);
    55.    end put;
    56.
    57.    procedure put_unit (stream : in out host_IO.stream; PS_text : String) is
    58.    begin
    59.       ensure_separation(stream);
    60.       put(stream, PS_text);
    61.    end put_unit;
    62.
    63.    procedure put_line (stream : in out host_IO.stream; PS_text : String) is
    64.    begin
    65.       put(stream, PS_text);
    66.       put_EOL(stream);
    67.    end put_line;
    68.
    69.    procedure put_unit_line (stream : in out host_IO.stream; PS_text : String) is
    70.    begin
    71.       put_unit(stream, PS_text);
    72.       put_EOL(stream);
    73.    end put_unit_line;
    74.
    75.    procedure put_integer (stream : in out host_IO.stream; i : Integer) is
    76.       image  : constant String := i'Image;
    77.    begin
    78.       ensure_separation(stream);
    79.       if image(image'First) /= ' ' then
    80.          put(stream, image);
    81.       else  -- Suppress the nuisance blank character.
    82.          put(stream, image(image'First+1..image'Last));
    83.       end if;
    84.    end put_integer;
    85.
    86.    procedure terminate_any_open_path (stream : in out host_IO.stream) is
    87.    begin
    88.       if there_is_an_open_path then
    89.          -- Draw the accumulated strokes.
    90.          put_unit_line(stream, "s");
    91.       end if;
    92.       there_is_an_open_path := False;
    93.    end terminate_any_open_path;
    94.
    95.    procedure draw_a_PS_vector (stream : in out host_IO.stream;
    96.                                initial,
    97.                                final  : in postscript.point) is
    98.
    99.       function largest_of (a, b, c : Natural)
   100.       return Natural
   101.       is (Natural'Max(a, Natural'Max(b, c)));
   102.
   103.    begin -- draw_a_PS_vector
   104.       if initial /= the_last_point_in_the_path then
   105.          -- This vector is not contiguous with the previous one.
   106.          terminate_any_open_path(stream);
   107.       end if;
   108.       if initial = final then
   109.          -- This vector is of length 0.
   110.          return;
   111.       end if;
   112.       maximum_offset.x := largest_of(maximum_offset.x, initial.x, final.x);
   113.       maximum_offset.y := largest_of(maximum_offset.y, initial.y, final.y);
   114.       if there_is_an_open_path then
   115.          -- Draw a line to the final point, extending the current path.
   116.          put_integer(stream, final.x);
   117.          put_integer(stream, final.y);
   118.          put_unit_line(stream, "l");
   119.       else
   120.          -- Move to the initial point, opening a fresh path, and draw a line to the final point.
   121.          put_integer(stream, final.x);
   122.          put_integer(stream, final.y);
   123.          put_integer(stream, initial.x);
   124.          put_integer(stream, initial.y);
   125.          put_unit_line(stream, "n");
   126.          there_is_an_open_path := True;
   127.       end if;
   128.       the_last_point_in_the_path := final;
   129.    exception
   130.       when others =>
   131.          close(stream);
   132.          raise;
   133.    end draw_a_PS_vector;
   134.
   135.    subtype RGB is String(1..11);
   136.    gamut : constant array (pen_colour) of RGB
   137.          := (
   138.                Black          => ".00 .00 .00",
   139.                Blue           => ".00 .00 1.0",
   140.                Brown          => ".60 .20 .00",
   141.                Cyan           => ".00 1.0 1.0",
   142.                Dark_Blue      => ".10 .10 .80",
   143.                Dark_Cyan      => ".20 .80 1.0",
   144.                Dark_Green     => ".00 .60 .40",
   145.                Dark_Grey      => ".50 .50 .50",
   146.                Dark_Magenta   => ".75 .25 .75",
   147.                Dark_Red       => ".75 .00 .00",
   148.                Green          => ".00 1.0 .00",
   149.                Grey           => ".80 .80 .80",
   150.                Magenta        => "1.0 .00 1.0",
   151.                Red            => "1.0 .00 .00",
   152.                White          => "1.0 1.0 1.0",
   153.                Yellow         => "1.0 1.0 .00"
   154.             );
   155.
   156.    subtype tip_breadth is String(1..4);
   157.    breadth : constant array (pen_tip_size) of tip_breadth
   158.            := (
   159.                Extra_Extra_Fine => "1.00",
   160.                Extra_Fine       => "2.00",
   161.                Fine             => "4.00",
   162.                Medium           => "6.00",
   163.                Medium_Broad     => "8.00",
   164.                Broad            => "10.0",
   165.                Extra_Broad      => "12.0"
   166.               );
   167.
   168.    the_colour   : pen_colour   := the_default_colour;
   169.    the_pen_size : pen_tip_size := the_default_tip_size;
   170.
   171.    procedure put_the_pen_settings (stream : in out host_IO.Stream) is
   172.    begin -- put_the_pen_settings
   173.       terminate_any_open_path(stream);
   174.       put_unit(stream, gamut(the_colour));
   175.       put_unit_line(stream, "setrgbcolor");
   176.       put_unit(stream, breadth(the_pen_size));
   177.       put_unit_line(stream, "setlinewidth");
   178.    end put_the_pen_settings;
   179.
   180.    procedure set_the_pen_properties (this_colour   : in pen_colour   := the_default_colour;
   181.                                      this_pen_size : in pen_tip_size := the_default_tip_size) is
   182.    begin -- set_the_pen_properties
   183.       the_colour := this_colour;
   184.       the_pen_size := this_pen_size;
   185.    end set_the_pen_properties;
   186.
   187.    -- We eventually seek back to the bounding box parametsrs using this, their file offset.
   188.    the_position_of_the_placeholders : Natural;
   189.
   190.    procedure initialize_PS_output (stream : in out host_IO.Stream) is
   191.
   192.    begin -- initialize_PS_output
   193.       put_line(stream, "%!PS-Adobe-3.0 EPSF-1.0");
   194.       put_unit(stream, "%%BoundingBox: ");
   195.
   196.       -- Note the file offset of the bounding box placeholders.
   197.       get_position(the_position_of_the_placeholders, stream);
   198.
   199.       -- Write the 10-column placeholders.
   200.       put_line(stream, "xxxxxxxxxx|yyyyyyyyyy");
   201.
   202.       put_line(stream, "% This graph was plotted by ee9, the GNU Ada KDF9 emulator.");
   203.       put_line(stream, "% For more information, see <http://www.findlayw.plus.com/KDF9>.");
   204.       put_line(stream, "save");
   205.
   206.       put_line(stream, "1 setlinecap");
   207.       put_line(stream, "1 setlinejoin");
   208.
   209.       put_the_pen_settings(stream);
   210.
   211.       put_line(stream, "0 792 translate");  -- Assumes a page of length 11"!
   212.
   213.       -- The plotter step was 0.005", which is the same as 0.36 PostScript points.
   214.       -- The scaling factor is set here to make the wabbit example fit an A4 page.
   215.       put_line(stream, "0.12 -0.12 scale");
   216.
   217.       put_line(stream, "/l { lineto } bind def");
   218.       put_line(stream, "/n { newpath moveto lineto } bind def");
   219.       put_line(stream, "/s { stroke } bind def");
   220.
   221.       put_line(stream, "save");
   222.    exception
   223.       when others =>
   224.          close(stream);
   225.          raise;
   226.    end initialize_PS_output;
   227.
   228.    procedure finalize_PS_output (stream : in out host_IO.Stream) is
   229.
   230.       subtype bound_string is String(1..10);
   231.
   232.       function bound_image (n : in Natural)
   233.       return bound_string is
   234.          n_image : constant String := n'Image;
   235.          b : bound_string := (others => ' ');
   236.       begin
   237.          if n_image'Length > bound_string'Length then
   238.             trap_invalid_operand("infeasible PostScript bounding box size");
   239.          else
   240.             b(b'Last-n_image'Length+b'First .. b'Last) := n_image;
   241.             return b;
   242.          end if;
   243.       end bound_image;
   244.
   245.    begin -- finalize_PS_output
   246.       terminate_any_open_path(stream);
   247.       put_line(stream, "showpage");
   248.       put_line(stream, "restore");
   249.       put_line(stream, "restore");
   250.       put_line(stream, "% End of plot");
   251.
   252.       -- Go back to the bounding box placeholders in the output file.
   253.       set_position(the_position_of_the_placeholders, stream);
   254.
   255.       -- Overwrite them with the actual x and y co-ordinate bounds.
   256.       put(stream, bound_image(maximum_offset.x));
   257.       put(stream, " ");
   258.       put(stream, bound_image(maximum_offset.y));
   259.
   260.       close(stream);
   261.    exception
   262.       when others =>
   263.          close(stream);
   264.          raise;
   265.    end finalize_PS_output;
   266.
   267. end postscript;
   268.

Compiling: ../Source/postscript.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- postscript.ads
     2. --
     3. -- Elementary Encapsulated PostScript (EPS) line drawing.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with host_IO;
    20.
    21. package postscript is
    22.
    23.    type pen_colour is (
    24.                        Black,
    25.                        Blue,
    26.                        Brown,
    27.                        Cyan,
    28.                        Dark_Blue,
    29.                        Dark_Cyan,
    30.                        Dark_Green,
    31.                        Dark_Grey,
    32.                        Dark_Magenta,
    33.                        Dark_Red,
    34.                        Green,
    35.                        Grey,
    36.                        Magenta,
    37.                        Red,
    38.                        White,
    39.                        Yellow
    40.                       );
    41.
    42.    the_default_colour : constant pen_colour := Black;
    43.
    44.    type pen_tip_size is (
    45.                          Extra_Extra_Fine,
    46.                          Extra_Fine,
    47.                          Fine,
    48.                          Medium,
    49.                          Medium_Broad,
    50.                          Broad,
    51.                          Extra_Broad
    52.                         );
    53.
    54.    the_default_tip_size : constant pen_tip_size := Extra_Extra_Fine;
    55.
    56.    -- Choose the pen's colour and tip size.
    57.
    58.    procedure set_the_pen_properties (this_colour   : in pen_colour   := the_default_colour;
    59.                                      this_pen_size : in pen_tip_size := the_default_tip_size);
    60.
    61.    -- Drawing is done in terms of the plotter's co-ordinate system.
    62.    -- (0, 0) is the top left point of the drawing,
    63.    -- The x axis increases down the plot (long axis, direction of paper movement),
    64.    --    and the y axis increases across the plot (short axis, direction of pen movement).
    65.
    66.    type point is
    67.      record
    68.         x, y : Natural;  -- All physically possible co-ordinates are non-negative.
    69.      end record;
    70.
    71.     function image (p : postscript.point)
    72.     return String;
    73.
    74.    -- Draw a straight line from initial to final.
    75.    procedure draw_a_PS_vector (stream : in out host_IO.stream;
    76.                                initial,
    77.                                final  : in postscript.point);
    78.
    79.    -- Open the PostScript file and write the prelude, with a placeholder for the bounds.
    80.    procedure initialize_PS_output (stream : in out host_IO.Stream);
    81.
    82.    -- Close the PostScript file, having gone back to overwrite the bounding box placeholders.
    83.    procedure finalize_PS_output (stream : in out host_IO.Stream);
    84.
    85. end postscript;

 268 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/finalize_ee9.adb
Source file time stamp: 2020-10-26 01:33:35
Compiled at: 2020-11-12 18:12:13

     1. -- finalize_ee9.adb
     2. --
     3. -- Shut down processing in preparation for a dignified exit.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Exceptions;
    20. --
    21. with HCI;
    22. with IOC;
    23. with state_display;
    24.
    25. use  HCI;
    26. use  IOC;
    27. use  state_display;
    28.
    29. procedure finalize_ee9 (because : in String) is
    30. begin
    31.    show_final_state(because);
    32.    finalize_all_KDF9_buffers;
    33. exception
    34.    when error : others =>
    35.       log_line("Failure in finalize_ee9: " & Ada.Exceptions.Exception_Information(error));
    36. end finalize_ee9;

 36 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/kdf9-directors.adb
Source file time stamp: 2020-11-02 19:19:39
Compiled at: 2020-11-12 18:12:13

     1. -- kdf9.directors.adb
     2. --
     3. -- Implement the system call API (OUTs) of the supported KDF9 Directors.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with dumping;
    20. with exceptions;
    21. with formatting;
    22. with HCI;
    23. with host_IO;
    24. with IOC;
    25. with IOC.dispatcher;
    26. with IOC.equipment;
    27. with IOC.fast.MT;
    28. with IOC.slow.shift.FW;
    29. with IOC.slow.shift.SI;
    30. with IOC.slow.shift.TR;
    31. with KDF9_char_sets;
    32. with KDF9.CPU;
    33. with KDF9.store;
    34. with logging.file;
    35. with settings;
    36. with state_display;
    37. with tracing;
    38.
    39. with value_of;
    40.
    41. use  dumping;
    42. use  exceptions;
    43. use  formatting;
    44. use  HCI;
    45. use  host_IO;
    46. use  IOC;
    47. use  IOC.dispatcher;
    48. use  IOC.equipment;
    49. use  IOC.fast.MT;
    50. use  IOC.slow.shift.FW;
    51. use  IOC.slow.shift.SI;
    52. use  IOC.slow.shift.TR;
    53. use  KDF9_char_sets;
    54. use  KDF9.store;
    55. use  logging.file;
    56. use  settings;
    57. use  state_display;
    58. use  tracing;
    59.
    60. package body KDF9.Directors is
    61.
    62.    procedure log_API_message (message  : in String;
    63.                               skip     : in Natural := 1) is
    64.    begin
    65.       if API_logging_is_wanted then
    66.          log_ee9_status(message, skip, True);
    67.       end if;
    68.    end log_API_message;
    69.
    70.    -- This is the actual wall clock time at which the program was loaded.
    71.    -- If signature hashing is enabled, it stays at zero to get a repeatable hash.
    72.    the_time_of_loading : KDF9.us := 0;
    73.
    74.    -- Set the base for virtual elapsed time reckoning.
    75.    procedure set_the_time_of_loading (the_time : in KDF9.us) is
    76.    begin
    77.       the_time_of_loading := the_time;
    78.    end set_the_time_of_loading;
    79.
    80.    -- Emulate a subset of the EGDON Director's OUT API.
    81.    procedure do_an_EGDON_OUT (OUT_number : in KDF9.word) is
    82.    begin
    83.       trap_unimplemented_feature("EGDON OUTs are not yet supported");
    84.    end do_an_EGDON_OUT;
    85.
    86.    -- Implement a subset of the Time Sharing Director's OUT 8 spooling API.
    87.    procedure do_an_OUT_8 is
    88.
    89.       function destination_device_for (the_stream : KDF9.word)
    90.       return IOC.device_number is
    91.       begin
    92.          case the_stream is
    93.             when 8#00# =>
    94.                return 0;
    95.             when 8#10# |8#12# |8#14# | 8#16# =>
    96.                return TP0_number;
    97.             when 8#11# |8#13# |8#15# | 8#17# =>
    98.                return TP1_number;
    99.             when 8#30#..8#37# =>
   100.                return LP0_number;
   101.             when 8#50#..8#57# =>
   102.                return TP1_number;
   103.             when 8#70#..8#77# =>
   104.                return LP0_number;
   105.             when others =>
   106.                trap_invalid_operand("OUT 8: invalid stream #" & oct_of(the_stream));
   107.          end case;
   108.       end destination_device_for;
   109.
   110.       the_stream : KDF9.word;
   111.       Q, G       : KDF9.Q_register;
   112.       FW_query   : Boolean;
   113.
   114.       procedure prepare_OUT8_to_FW0 is
   115.       begin
   116.          -- The logic of FW streams is rather complex, to preserve the layout of the typescript.
   117.          -- There are three significant aspects.
   118.
   119.          -- 1. The message is truncated if longer than 8 words.
   120.          if Q.M - Q.I > 8 then
   121.             Q.M := Q.I + 8;
   122.          end if;
   123.
   124.          -- 2. It must not contain LS or HT;
   125.          --       nor ';' in the last word;
   126.          --          nor ';' other than in character 7;
   127.          --    but anything after an End Message can safely be ignored.
   128.          word_loop: for w in Q.I+1 .. Q.M loop
   129.              for c in KDF9_char_sets.symbol_index'Range loop
   130.                 declare
   131.                    s : constant KDF9_char_sets.symbol := fetch_symbol(w, c);
   132.                 begin
   133.                    if s = KDF9_char_sets.Line_Shift                       or else
   134.                          s = KDF9_char_sets.Tabulation                    or else
   135.                             ((s = KDF9_char_sets.Semi_Colon) and
   136.                              (c /= 7 or  w = Q.M or not FW_query)) then
   137.                       trap_invalid_operand("OUT 8: failure 73, invalid data for OUT8 to FW");
   138.                    end if;
   139.          exit word_loop when s = KDF9_char_sets.Semi_Colon or s = KDF9_char_sets.End_Message;
   140.                 end;
   141.              end loop;
   142.          end loop word_loop;
   143.
   144.          -- 3. The Director takes a new line for each OUT 8 message to the FW.
   145.          -- It sets up the format effector(s) in the first word of the OUT 8 area.
   146.          declare
   147.             FW_prefix : constant KDF9.word := 8#77_77_77_77_77_77_07_02#;  -- CN LS
   148.             package FW renames IOC.slow.shift.FW;
   149.             the_FW : FW.device renames FW.device(buffer(0).all);
   150.          begin
   151.             if a_LF_was_just_read(the_FW) then
   152.                -- Replace the redundant Line Shift with a Word Filler character.
   153.                store_word(FW_prefix or 8#77#, Q.I);
   154.             else
   155.                -- The initial Line Shift is needed.
   156.                store_word(FW_prefix, Q.I);
   157.             end if;
   158.          end;
   159.       end prepare_OUT8_to_FW0;
   160.
   161.       page_change : constant := 8#77_77_77_77_77_77_77_03#;  --  LP Page Change character
   162.
   163.    begin  -- do_an_OUT_8
   164.       -- Spool output.
   165.       ensure_that_the_nest_holds_an_operand;
   166.       Q := as_Q(pop);  -- the N2 parameter
   167.       the_trace_operand := as_word(Q);
   168.
   169.       -- An OUT 8 to the FW for a query must have D0 of the control word set.
   170.       FW_query := (Q.C and 8#1_00000#) /= 0;
   171.       if FW_query then
   172.          Q.C := 0;
   173.       end if;
   174.
   175.       --
   176.          -- OUT 8 'output spooling'.
   177.       --
   178.
   179.       if Q.C = Q.I and Q.I = Q.M then
   180.          -- The N2 parameter specifies stream closure.
   181.          flush(buffer(destination_device_for(KDF9.word(Q.C))).all);
   182.          return;
   183.       end if;
   184.
   185.       -- The Q = N2 parameter specifies a block starting with the stream number.
   186.       check_address_and_lockout(Q.I);
   187.       the_stream := fetch_word(Q.I);
   188.       Q.C := destination_device_for(the_stream);
   189.       set_state_of(buffer(Q.C), allocated => True);
   190.       check_address_and_lockout(Q.I+1);
   191.       G := as_Q(fetch_word(Q.I+1));
   192.
   193.       -- See the Manual, §12.6.1.
   194.       if G.C = 4095 and then G.I = 8#177777# then
   195.          -- The G parameter specifies output of a 'gap' suitable for the device.
   196.          the_trace_operand := as_word(G);
   197.          if G.M = 0 then
   198.             return;
   199.          end if;
   200.          G.M := (if G.M in 1 .. 511 then G.M else 120);
   201.          if destination_device_for(the_stream) = 0 then
   202.             null;  -- What else could be done; fail?
   203.          elsif destination_device_for(the_stream) in TP0_number | TP1_number then
   204.             POE((Q.C, 0, G.M), False);   -- Write gap.
   205.          elsif destination_device_for(the_stream) = LP0_number then
   206.             store_word(page_change, Q.I);
   207.             POA((Q.C, Q.I, Q.I), False); -- Write PC.
   208.          else
   209.             trap_invalid_operand("OUT 8: invalid device for gapping #"
   210.                                & oct_of(destination_device_for(the_stream)));
   211.          end if;
   212.          return;
   213.       end if;
   214.
   215.       if Q.M <= Q.I then
   216.          trap_invalid_operand("OUT 8: invalid M-part #" & oct_of(Q.M));
   217.       end if;
   218.
   219.       -- Perform the transfer at once (no spooling is implemented).
   220.
   221.       if Q.C /= 0 then
   222.          -- For non-FW streams, the first word of the OUT 8 area is not transferred.
   223.          Q.I := Q.I + 1;
   224.       else
   225.          -- The logic for FW streams is more complex, to preserve the layout of the typescript.
   226.          prepare_OUT8_to_FW0;
   227.       end if;
   228.
   229.       -- OUT 8 transfers always go to End Message.
   230.       POB(Q, False);
   231.
   232.    exception
   233.       when host_IO.end_of_stream =>
   234.          trap_invalid_operand("OUT 8: no device file for stream #"
   235.                             & oct_of(KDF9.Q_part(the_stream)));
   236.    end do_an_OUT_8;
   237.
   238.    -- This is used to keep a note of explicitly requested allocations by OUT 5.
   239.    -- Allocations by OUT 8 emulation prevent LIVs on the the pseudo-spooled devices.
   240.    -- These internal allocations by OUT 8 must not cause a reservation by OUT 5 to fail.
   241.    is_free_for_explicit_allocation : array(KDF9.buffer_number) of Boolean := (others => True);
   242.
   243.    -- Emulate a subset of the Time Sharing Director's OUT API.
   244.    procedure do_a_TSD_OUT (OUT_number : in KDF9.word) is
   245.
   246.       B : KDF9.Q_part;
   247.       W : KDF9.word;
   248.       P : KDF9.pair;
   249.
   250.       procedure set_ancestor_to_rest is
   251.       begin
   252.          if the_log_is_wanted and pre_overlay_state_is_enabled then
   253.             show_final_state("pre-overlay");
   254.          end if;
   255.
   256.          if the_log_is_wanted and nr_of_post_dumping_areas /= 0 then
   257.             log_new_line;
   258.             log_rule;
   259.             log_title("Post-run Dump:");
   260.             print_postrun_dump_areas;
   261.          end if;
   262.
   263.          remove_prerun_dump_areas;
   264.          remove_postrun_dump_areas;
   265.
   266.          clear_retro_FIFO;
   267.          clear_IOC_FIFO;
   268.          clear_interrupt_FIFO;
   269.          clear_the_histogram;
   270.          clear_the_profile;
   271.          the_profile_is_wanted := False;
   272.          the_INS_plot_is_wanted := False;
   273.       end set_ancestor_to_rest;
   274.
   275.       procedure prepare_successor is
   276.       begin
   277.          poke_all_amendments;
   278.          save_the_initial_jump;
   279.
   280.          the_program_has_been_analysed := False;
   281.          show_all_prerun_dump_areas;
   282.
   283.          -- Setting NIA must follow loading, as it fetches E0 into the IWBs.
   284.          set_NIA_to((0, 0));
   285.          the_V_bit_is_set := False;
   286.          the_T_bit_is_set := False;
   287.       end prepare_successor;
   288.
   289.       procedure overlay_a_new_program (program_name : in String) is
   290.          overlay : constant String := value_of("KDF9_BINARY", default => "Binary")
   291.                                     & "/"
   292.                                     & program_name;
   293.          W : KDF9.word;
   294.       begin
   295.          --
   296.          -- Handle exceptional cases first.
   297.          --
   298.
   299.          if program_name = "" then
   300.             trap_invalid_operand("OUT 1: program name is an empty string");
   301.          end if;
   302.
   303.          if program_name = "KMW0201--UPU" then
   304.             -- The Whetstone Controller is trying to overlay itself with the Translator.
   305.             -- This is so inconvenient in practice that I simply prevent it.
   306.             log_API_message("OUT 1: ee9 will not return to the Whetstone Translator",
   307.                             skip => 2
   308.                            );
   309.             raise program_exit;
   310.          end if;
   311.
   312.          --
   313.          -- Do all the common housekeeping for an effected overlay.
   314.          --
   315.
   316.          -- Clear up the calling program.
   317.
   318.          complete_all_extant_transfers;  -- To get an accurate elapsed time.
   319.          is_free_for_explicit_allocation := (others => True);
   320.          set_ancestor_to_rest;
   321.
   322.          log_API_message("OUT 1: ICR ="
   323.                        & ICR'Image
   324.                        & "; RAN/EL ="
   325.                        & the_CPU_time'Image
   326.                        & " /"
   327.                        & KDF9.us'Image(the_clock_time)
   328.                        & " KDF9 us"
   329.                         );
   330.
   331.          -- Set up the called program.
   332.
   333.          if the_external_trace_is_enabled then
   334.             log_new_line(the_external_trace_file);
   335.             log(the_external_trace_file, "ee9: Running overlay " & overlay);
   336.             log_new_line(the_external_trace_file);
   337.             log_an_external_trace_header;
   338.          end if;
   339.
   340.          get_settings_from_file("2");
   341.
   342.          install_GP0;
   343.
   344.          log_new_line;
   345.          display_execution_modes(overlay);
   346.
   347.          -- Word 1 is preserved across overlays.
   348.          W := fetch_word(1);
   349.          load_a_program(program_file_name => overlay);
   350.          store_word(W, 1);
   351.
   352.          prepare_successor;
   353.
   354.          raise mode_change_request;  -- signal a new program run.
   355.
   356.       end overlay_a_new_program;
   357.
   358.       procedure restart_this_program_with_new_time_limit (W : in KDF9.word) is
   359.       begin
   360.          set_ancestor_to_rest;
   361.
   362.          log_API_message("OUT 2: ICR ="
   363.                        & ICR'Image
   364.                        & "; RAN/EL ="
   365.                        & the_CPU_time'Image
   366.                        & " /"
   367.                        & KDF9.us'Image(the_clock_time)
   368.                        & " KDF9 us"
   369.                         );
   370.          log_API_message(
   371.                          "OUT 2: restarts with time limit = "
   372.                        & KDF9.word'Image(W/2**24) &"s",
   373.                          skip => 0
   374.                         );
   375.
   376.          get_settings_from_file("2");
   377.
   378.          install_GP0;
   379.
   380.          prepare_successor;
   381.
   382.          ICR := 0;
   383.
   384.           -- Set the new time limit in E1U.
   385.          store_halfword(W, 1, 0);
   386.
   387.           -- Set the new store limit in E1L.
   388.          store_halfword((KDF9.word'(max_address)) * 2**24, 1, 1);
   389.
   390.          log_new_line;
   391.          display_execution_modes;
   392.
   393.          reset_the_program_state;
   394.
   395.       end restart_this_program_with_new_time_limit;
   396.
   397.       -- These are the device-type codes to be given when requesting
   398.       --    the allocation of a peripheral with TSD OUT 5,
   399.       --       according to the Manual and the document:
   400.       --          "Service Routine Library Manual" §22.13, p22-28-0.
   401.
   402.       FW_OUT5_code : constant := 0;
   403.       TP_OUT5_code : constant := 1;
   404.       TR_OUT5_code : constant := 2;
   405.       LP_OUT5_code : constant := 3;
   406.       CR_OUT5_code : constant := 4;
   407.       FP_OUT5_code : constant := 5;      -- Ferranti 5-channel Tape punch
   408.       CP_OUT5_code : constant := 7;
   409.       GP_OUT5_code : constant := 8#20#;
   410.       SI_OUT5_code : constant := 8#21#;  -- Standard Interface, "Data Link, N.P.L. Special Buffer"
   411.       FE_OUT5_code : constant := 8#65#;  -- Tape buffer link for PDP-8 on Eldon2, and perhaps COTAN
   412.       UT_OUT5_code : constant := 8#67#;  -- Unlabelled Tape
   413.
   414.       procedure select_the_next_device_from_among
   415.          (device_A, device_B : in  KDF9.buffer_number;
   416.           wanted_device_type : in  KDF9.word;
   417.           chosen_device      : out KDF9.buffer_number) is
   418.       begin
   419.          if device_A /= 0                            and then
   420.                is_free_for_explicit_allocation(device_A) then
   421.             chosen_device := device_A;
   422.             is_free_for_explicit_allocation(device_A) := False;
   423.          elsif device_B /= 0                         and then
   424.                is_free_for_explicit_allocation(device_B) then
   425.             chosen_device := device_B;
   426.             is_free_for_explicit_allocation(device_B) := False;
   427.          else
   428.             trap_invalid_operand("OUT 5: no device of type #"
   429.                                & oct_of(wanted_device_type)
   430.                                & " is available"
   431.                                 );
   432.          end if;
   433.       end select_the_next_device_from_among;
   434.
   435.       procedure allocate_a_device is
   436.       begin
   437.          ensure_that_the_nest_holds_an_operand;
   438.          W := read_top;
   439.
   440.          case W is
   441.             -- 8 was added to the code to pre-allocate a device.
   442.             -- I treat pre-allocating and allocating the same way here.
   443.             when FW_OUT5_code
   444.                | FW_OUT5_code+8 =>
   445.                B := 0;  -- Always allowed, no checking performed.
   446.             when TP_OUT5_code
   447.                | TP_OUT5_code+8
   448.                | FP_OUT5_code
   449.                | FP_OUT5_code+8 =>
   450.                select_the_next_device_from_among(TP0_number, TP1_number, W, B);
   451.             when TR_OUT5_code
   452.                | TR_OUT5_code+8 =>
   453.                -- N.B. the TR devices must appear in this order.
   454.                -- TR0 is used for reading the bootstrap/problem program in KDF9 code.
   455.                -- When there is Latin-1 data it therefore needs to go in via TR1.
   456.                select_the_next_device_from_among(TR1_number, TR0_number, W, B);
   457.                set_case(IOC.slow.shift.TR.device(buffer(B).all));
   458.             when LP_OUT5_code
   459.                | LP_OUT5_code+8 =>
   460.                select_the_next_device_from_among(LP0_number, LP1_number, W, B);
   461.             when CR_OUT5_code
   462.                | CR_OUT5_code+8 =>
   463.                select_the_next_device_from_among(CR0_number, CR1_number, W, B);
   464.             when CP_OUT5_code
   465.                | CP_OUT5_code+8 =>
   466.                select_the_next_device_from_among(CP0_number, CP1_number, W, B);
   467.             when GP_OUT5_code
   468.                | GP_OUT5_code+8 =>
   469.                -- There is only 1 graph plotter.
   470.                the_graph_plotter_is_enabled := True;
   471.                install_GP0;
   472.                select_the_next_device_from_among(GP0_number, GP0_number, W, B);
   473.             when SI_OUT5_code =>
   474.                if SI0_is_enabled then
   475.                   select_the_next_device_from_among(SI0_number, SI1_number, W, B);
   476.                else
   477.                   trap_invalid_operand("OUT 5: SI0 has not been enabled");
   478.                end if;
   479.             when FE_OUT5_code =>
   480.                trap_unimplemented_feature("PDP-8 Front End Tape buffers");
   481.             when UT_OUT5_code =>
   482.                trap_unimplemented_feature("Unlabelled Tape buffers");
   483.             when others =>
   484.                trap_invalid_operand("OUT 5: invalid device type" & W'Image);
   485.          end case;
   486.
   487.          pop;
   488.          push(KDF9.word(B));
   489.          the_trace_operand := KDF9.word(B);
   490.          set_state_of(buffer(B), allocated => True);
   491.
   492.          if buffer(B).all in IOC.slow.shift.device'Class and then
   493.                buffer(B).kind /= GP_kind                     then
   494.             log_API_message("OUT 5: requested a device of type #"
   495.                           & oct_of(KDF9.Q_part(W), 2)
   496.                           & " and got "
   497.                           & device_name_of(buffer(B).all)
   498.                           & " using "
   499.                           & (
   500.                              if IOC.slow.shift.device(buffer(B).all).uses_Latin_1 then
   501.                                 "Latin-1"
   502.                              else
   503.                                 "KDF9"
   504.                             )
   505.                           & " code"
   506.                            );
   507.          else
   508.             log_API_message("OUT 5: requested a device of type #"
   509.                           & oct_of(KDF9.Q_part(W), 2)
   510.                           & " and got "
   511.                           & device_name_of(buffer(B).all)
   512.                            );
   513.          end if;
   514.       end allocate_a_device;
   515.
   516.       procedure validate_buffer_number (parameter : in KDF9.word) is
   517.       begin
   518.          if parameter > 15 then
   519.             raise emulation_failure
   520.                with "invalid buffer #"
   521.                   & oct_of(parameter)
   522.                   & " found in OUT "
   523.                   & OUT_number'Image;
   524.          end if;
   525.       end validate_buffer_number;
   526.
   527.       procedure allocate_a_tape_with_1_word_label is
   528.       begin
   529.          ensure_that_the_nest_holds_an_operand;
   530.          W := pop;
   531.          declare
   532.             label : constant short_label := short_label(to_string(W));
   533.          begin
   534.             B := -1;
   535.             find_tape_labelled(label, B, W);  -- W is not actually used in OUT 4
   536.             validate_buffer_number(KDF9.word(B));
   537.             push(KDF9.word(B));
   538.             the_trace_operand := KDF9.word(B);
   539.             if W = 0 then
   540.                log_API_message("OUT 4: requested a scratch tape and got '"
   541.                              & device_name_of(buffer(B).all)
   542.                              & "' with TSN '"
   543.                              & to_string(W)
   544.                              & "'"
   545.                               );
   546.             else
   547.                log_API_message("OUT 4: requested a tape labelled '"
   548.                              & String(label)
   549.                              & "' and got "
   550.                              & device_name_of(buffer(B).all)
   551.                              & " with TSN '"
   552.                              & to_string(W)
   553.                              & "'"
   554.                               );
   555.             end if;
   556.          end;
   557.          set_state_of(buffer(B), allocated => True);
   558.          is_free_for_explicit_allocation(B) := False;
   559.       end allocate_a_tape_with_1_word_label;
   560.
   561.       procedure deallocate_a_device_and_unload_a_tape is
   562.       begin
   563.          ensure_that_the_nest_holds_an_operand;
   564.          W := pop;
   565.          the_trace_operand := W;
   566.          validate_buffer_number(W);
   567.          B := KDF9.Q_part(W);
   568.          if is_free_for_explicit_allocation(B) then
   569.             trap_invalid_operand("OUT 6: device #"
   570.                                & oct_of(B, 2)
   571.                                & ", i.e. "
   572.                                & device_name_of(buffer(B).all)
   573.                                & ", is not allocated to this program"
   574.                                 );
   575.          elsif buffer(B).kind in MT_kind | ST_kind then
   576.             -- Rewind the tape and unload it.
   577.             PMD(buffer(B).all, KDF9.Q_register'(B, 0, 0), set_offline => True);
   578.          end if;
   579.          set_state_of(buffer(B), allocated => False);
   580.          is_free_for_explicit_allocation(B) := True;
   581.          log_API_message("OUT 6: released " & device_name_of(buffer(B).all));
   582.       end deallocate_a_device_and_unload_a_tape;
   583.
   584.       procedure deallocate_a_deck_leaving_the_tape_in_situ is
   585.       begin
   586.          ensure_that_the_nest_holds_an_operand;
   587.          W := pop;
   588.          the_trace_operand := W;
   589.          validate_buffer_number(W);
   590.          B := KDF9.Q_part(W);
   591.          if is_unallocated(buffer(B)) then
   592.             trap_invalid_operand("OUT 7: device #"
   593.                                & oct_of(B, 2)
   594.                                & ", i.e. "
   595.                                & device_name_of(buffer(B).all)
   596.                                & ", is not allocated to this program"
   597.                                 );
   598.          elsif buffer(B).kind in MT_kind | ST_kind then
   599.             -- Rewind the tape, but do not unload it.
   600.             PMD(buffer(B).all, KDF9.Q_register'(B, 0, 0), set_offline => False);
   601.             set_state_of(buffer(B), allocated => False);
   602.             is_free_for_explicit_allocation(B) := True;
   603.             log_API_message("OUT 7: released " & device_name_of(buffer(B).all));
   604.          else
   605.             trap_invalid_operand("OUT 7: device #"
   606.                                & oct_of(B, 2)
   607.                                & ", i.e. "
   608.                                & device_name_of(buffer(B).all)
   609.                                & ", is not a MT"
   610.                                 );
   611.          end if;
   612.       end deallocate_a_deck_leaving_the_tape_in_situ;
   613.
   614.       procedure allocate_a_tape_with_2_word_label is
   615.       begin
   616.          ensure_that_the_nest_holds_2_operands;
   617.          P := pop;
   618.          declare
   619.             label : constant long_label := long_label(to_string(P));
   620.          begin
   621.             find_tape_labelled(label, B, W);
   622.             push(W);
   623.             push(KDF9.word(B));
   624.             the_trace_operand := KDF9.word(B);
   625.             validate_buffer_number(KDF9.word(B));
   626.             log_API_message("OUT 10: requested a tape labelled '"
   627.                           & String(label)
   628.                           & "' and got "
   629.                           & device_name_of(buffer(B).all)
   630.                           & " with TSN '"
   631.                           & to_string(W)
   632.                           & "'"
   633.                            );
   634.          end;
   635.          set_state_of(buffer(B), allocated => True);
   636.          is_free_for_explicit_allocation(B) := False;
   637.       end allocate_a_tape_with_2_word_label;
   638.
   639.       -- Return a time in µs as 48-bit seconds to 23 integral places.
   640.       function OUT_time (microseconds : KDF9.us)
   641.       return KDF9.word is
   642.          -- The time was recorded by the hardware in units of 32 us, not 1 us.
   643.          truncated_time : constant KDF9.us := microseconds and not 31;
   644.       begin
   645.          if truncated_time < 2**23 * 1E6 then
   646.             -- 2**18 / 15625 = 2**24 / 1E6, with no risk of overflow in 64 bits.
   647.             return KDF9.word(truncated_time * 2**18 / 15625);
   648.          else
   649.             -- The virtual elapsed time overflows the 23-bit seconds field;
   650.             --    return a nonce and indicate overflow.
   651.             -- This would never have happened to a real KDF9,
   652.             --    as 2**23 seconds is over three months.
   653.             -- No KDF9 could stay up that long!
   654.             -- However 2**23 KDF9 seconds pass in about 5 hours of ee9 real time,
   655.             --    so precautions have to be taken.
   656.             trap_invalid_operand("the time for OUT 3/9/17 exceeds the allowed range");
   657.          end if;
   658.       end OUT_time;
   659.
   660.    begin -- do_a_TSD_OUT
   661.       -- Dismiss the OUT number in N1, allowing for an empty NEST, treated as OUT 0.
   662.       if the_nest_depth > 0 then
   663.          pop;
   664.       end if;
   665.
   666.       case OUT_number is
   667.
   668.          when 0 =>
   669.             -- Terminate program.
   670.             log_API_message("OUT 0: end of run");
   671.             notify_termination;
   672.             dispose_all_allocated_tapes;
   673.             raise program_exit;
   674.
   675.          when 1 =>
   676.             ensure_that_the_nest_holds_2_operands;
   677.             P := pop;
   678.             P := CPU.shift_logical(P, 24);
   679.             overlay_a_new_program(program_name => trimmed(to_string(P)));
   680.
   681.          when 2 =>
   682.             -- Restart a newly self-overwritten program.
   683.             ensure_that_the_nest_holds_an_operand;
   684.             W := pop;
   685.             the_trace_operand := W;
   686.             restart_this_program_with_new_time_limit(W);
   687.             notify_termination;
   688.             dispose_all_allocated_tapes;
   689.
   690.          when 3 =>
   691.             -- Get the virtual CPU time used, allowing for previous overlays.
   692.             W := OUT_time(the_CPU_time);
   693.             push(W);
   694.             the_trace_operand := W;
   695.
   696.          when 4 =>
   697.             allocate_a_tape_with_1_word_label;
   698.
   699.          when 5 =>
   700.             allocate_a_device;
   701.
   702.          when 6 =>
   703.             deallocate_a_device_and_unload_a_tape;
   704.
   705.          when 7 =>
   706.             deallocate_a_deck_leaving_the_tape_in_situ;
   707.
   708.          when 8 =>
   709.             do_an_OUT_8;
   710.
   711.          when 9 =>
   712.             -- Get the time of day, in seconds since midnight to 23 integral places.
   713.             -- A TOD clock is simulated using the real TOD at which the program was
   714.             --    loaded, and the virtual time that has elapsed since.
   715.             W := OUT_time(the_time_of_loading + the_clock_time);
   716.             push(W);
   717.             the_trace_operand := W;
   718.
   719.          when 10 =>
   720.             allocate_a_tape_with_2_word_label;
   721.
   722.          when 17 =>
   723.             -- Get the CPU Time and the Notional Elapsed Time in seconds to 23 integral places.
   724.             -- In program mode, the Notional Elapsed Time is the same thing as the_clock_time.
   725.             ensure_that_the_nest_has_room_for_2_results;
   726.             W := OUT_time(the_CPU_time);
   727.             push(OUT_time(the_clock_time));
   728.             push(W);
   729.             the_trace_operand := W;
   730.
   731.          when 70 =>
   732.             -- This is not a genuine TSD OUT, it is an expedient for debugging KAlgol,
   733.             --   so ee9 simply erases its parameters from N1 and N2.
   734.             ensure_that_the_nest_holds_2_operands;
   735.             W := pop;
   736.             the_trace_operand := W;
   737.             W := pop;
   738.
   739.          when 98 =>
   740.             -- This is not a genuine TSD OUT, it is an ee9 'OUT' for setting FW output format.
   741.             ensure_that_the_nest_holds_an_operand;
   742.             W := pop;
   743.             the_trace_operand := W;
   744.             realistic_FW_output_is_wanted := the_trace_operand /= 0;
   745.
   746.          when 99 =>
   747.             -- This is not a genuine TSD OUT, it is an ee9 'OUT' for program instrumentation.
   748.             -- Get present value of the Instruction Count Register (ICR) from within ee9.
   749.             W := KDF9.word(ICR);
   750.             push(W);
   751.             the_trace_operand := W;
   752.
   753.          when others =>
   754.             trap_unimplemented_feature("OUT" & OUT_number'Image);
   755.
   756.       end case;
   757.
   758.    end do_a_TSD_OUT;
   759.
   760. end KDF9.Directors;

Compiling: ../Source/kdf9-directors.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- kdf9.directors.ads
     2. --
     3. -- Implement the APIs  (OUTs) of the supported KDF9 Directors.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package KDF9.Directors is
    20.
    21.    -- Emulate a subset of the EGDON Director's OUT API.
    22.    procedure do_an_EGDON_OUT (OUT_number : in KDF9.word);
    23.
    24.    -- Implement a subset of the Time Sharing Director's OUT 8 API.
    25.    procedure do_an_OUT_8;
    26.
    27.    -- Emulate a subset of the Time Sharing Director's OUT API.
    28.    procedure do_a_TSD_OUT (OUT_number : in KDF9.word);
    29.
    30.    -- Set the base for virtual elapsed time reckoning.
    31.    procedure set_the_time_of_loading (the_time : in KDF9.us);
    32.
    33. end KDF9.Directors;

 760 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/generic_sets.adb
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- generic_sets.adb
     2. --
     3. -- Powersets of a discrete member type.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. -- generic
    20. --    type member is (<>);
    21. package body generic_sets is
    22.
    23.    function "abs" (set : generic_sets.set)
    24.    return Natural is
    25.       result : Natural := 0;
    26.    begin
    27.       for member in generic_sets.member loop
    28.          if set/member then
    29.             result := result + 1;
    30.          end if;
    31.       end loop;
    32.       return result;
    33.    end "abs";
    34.
    35.    function "/" (member : generic_sets.member; set : generic_sets.set)
    36.    return Boolean
    37.    is (set(member));
    38.
    39.    function "/" (set : generic_sets.set; member : generic_sets.member)
    40.    return Boolean
    41.    is (set(member));
    42.
    43.    function "or" (member : generic_sets.member; set : generic_sets.set)
    44.    return generic_sets.set is
    45.    begin
    46.       return result : generic_sets.set := set do
    47.          result(member) := True;
    48.       end return;
    49.    end "or";
    50.
    51.    function "or" (set : generic_sets.set; member : generic_sets.member)
    52.    return generic_sets.set is
    53.    begin
    54.       return result : generic_sets.set := set do
    55.          result(member) := True;
    56.       end return;
    57.    end "or";
    58.
    59.    function "-" (set : generic_sets.set; member : generic_sets.member)
    60.    return generic_sets.set is
    61.    begin
    62.       return result : generic_sets.set := set do
    63.          result(member) := False;
    64.       end return;
    65.    end "-";
    66.
    67.    function "-" (set1, set2 : generic_sets.set)
    68.    return generic_sets.set is
    69.    begin -- (set1 and not set2), avoiding need for large statically allocated workspace
    70.       return result : generic_sets.set := set1 do
    71.          for m in generic_sets.member loop
    72.             if set2(m) then
    73.                result(m) := False;
    74.             end if;
    75.          end loop;
    76.       end return;
    77.    end "-";
    78.
    79. end generic_sets;

Compiling: ../Source/generic_sets.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- generic_sets.ads
     2. --
     3. -- Powersets of a discrete member type.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. generic
    20.    type member is (<>);
    21. package generic_sets is
    22.
    23.    pragma Preelaborate;
    24.
    25. --
    26. -- This package implements only those set operations that are needed by ee9.
    27. --
    28.
    29.    type set is array (generic_sets.member) of Boolean
    30.       with Component_Size => 1, Convention => C;
    31.
    32.    universe  : constant generic_sets.set := (others => True);
    33.
    34.    empty_set : constant generic_sets.set := (others => False);
    35.
    36.    function "abs" (set : generic_sets.set)
    37.    return Natural;
    38.
    39.    -- Test for membership of the set.
    40.    function "/" (set : generic_sets.set; member : generic_sets.member)
    41.    return Boolean;
    42.
    43.    function "/" (member : generic_sets.member; set : generic_sets.set)
    44.    return Boolean;
    45.
    46.     function "or"  (member : generic_sets.member; set : generic_sets.set)
    47.     return generic_sets.set;
    48.
    49.     function "or"  (set : generic_sets.set; member : generic_sets.member)
    50.     return generic_sets.set;
    51.
    52. -- "or"  (set1, set2 : generic_sets.set) is predefined
    53.
    54.    function "-" (set : generic_sets.set; member : generic_sets.member)
    55.    return generic_sets.set;
    56.
    57.    function "-" (set1, set2 : generic_sets.set)
    58.    return generic_sets.set;
    59.
    60. end generic_sets;

 79 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/settings-io.adb
Source file time stamp: 2020-07-02 23:18:28
Compiled at: 2020-11-12 18:12:13

     1. -- settings-IO.ads
     2. --
     3. -- Settings-reader I/O support.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Characters.Latin_1;
    20. --
    21. with file_interfacing;
    22. with KDF9;
    23.
    24. use  Ada.Characters.Latin_1;
    25.
    26. package body settings.IO is
    27.
    28.    procedure open_options_file (file : in out File_Type; name : in String) is
    29.    begin
    30.       file_interfacing.initialize(file, in_file, name);
    31.       line_number := 1;
    32.    exception
    33.       when others =>
    34.          raise Status_Error with name;
    35.    end open_options_file;
    36.
    37.    procedure close_options_file (file : in out File_Type; name : in String) is
    38.    begin
    39.       file_interfacing.finalize(file, name);
    40.    end close_options_file;
    41.
    42.    comment_flag_character : constant Character := '|';
    43.
    44.    procedure skip_to_next_non_blank (file : File_Type) is
    45.       next_char : Character := ' ';
    46.       end_line  : Boolean;
    47.    begin
    48.       loop
    49.         look_ahead(file, next_char, end_line);
    50.       exit when end_line or else
    51.                   (next_char /= ' ' and next_char /= HT);
    52.          get(file, next_char);
    53.       end loop;
    54.       if next_char = comment_flag_character then
    55.          while not end_of_line(file) loop
    56.             get(file, next_char);
    57.          end loop;
    58.       end if;
    59.    end skip_to_next_non_blank;
    60.
    61.    procedure ensure_not_at_end_of_line (file : File_Type) is
    62.    begin
    63.       skip_to_next_non_blank (file);
    64.       if end_of_line(file) then
    65.          raise Data_Error;
    66.       end if;
    67.    end ensure_not_at_end_of_line;
    68.
    69.    procedure skip_to_next_nonempty_line (file : in File_Type) is
    70.       flag     : Character;
    71.       end_line : Boolean;
    72.    begin
    73.       loop
    74.          look_ahead(file, flag, end_line);
    75.          if end_line                      or else
    76.                flag = comment_flag_character then
    77.             Skip_Line(file);
    78.             line_number := line_number + 1;
    79.          else
    80.             exit;
    81.          end if;
    82.       end loop;
    83.       if flag = comment_flag_character then
    84.          raise Data_Error;
    85.       end if;
    86.    end skip_to_next_nonempty_line;
    87.
    88.    digit_offset : constant := Character'Pos('0');
    89.
    90.    procedure get_octal (file : in File_Type; value : out KDF9.word) is
    91.       next_char : Character;
    92.       last_char : Character := '_';
    93.       place     : Natural   := 0;
    94.       end_line  : Boolean   := False;
    95.    begin
    96.       value := 0;
    97.       ensure_not_at_end_of_line(file);
    98.       get(file, next_char);
    99.       if next_char = '#' then
   100.          get(file, next_char);
   101.       else
   102.          raise Data_Error;
   103.       end if;
   104.       loop
   105.          if next_char in '0' .. '7' then
   106.             value := value*8 + KDF9.word(Character'Pos(next_char)-digit_offset);
   107.             place := place + 1;
   108.             if place > 16 then
   109.                raise Data_Error;
   110.             end if;
   111.          elsif next_char = '_' then
   112.             if place = 0 then
   113.                raise Data_Error;
   114.             end if;
   115.          else
   116.             if last_char = '_' or place = 0 then
   117.                raise Data_Error;
   118.             end if;
   119.             exit;
   120.          end if;
   121.          last_char := next_char;
   122.          look_ahead(file, next_char, end_line);
   123.       exit when end_line;
   124.          if next_char in '0' .. '7' or next_char = '_' then
   125.             get(file, next_char);
   126.          else
   127.             if last_char = '_' or place = 0 then
   128.                raise Data_Error;
   129.             end if;
   130.             exit;
   131.          end if;
   132.       end loop;
   133.    end get_octal;
   134.
   135.    procedure get_decimal (file : in File_Type; value : out KDF9.word) is
   136.       next_char : Character;
   137.       last_char : Character := '_';
   138.       place     : Natural   := 0;
   139.       end_line  : Boolean   := False;
   140.    begin
   141.       value := 0;
   142.       ensure_not_at_end_of_line(file);
   143.       get(file, next_char);
   144.       if next_char not in '0' .. '9' then
   145.          raise Program_Error with "get_decimal " & next_char;
   146.       end if;
   147.       loop
   148.          if next_char in '0' .. '9' then
   149.             value := value*10 + KDF9.word(Character'Pos(next_char)-digit_offset);
   150.             place := place + 1;
   151.             if place > 15 then
   152.                raise Data_Error;
   153.             end if;
   154.          elsif next_char = '_' then
   155.             if place = 0 then
   156.                raise Data_Error;
   157.             end if;
   158.          else
   159.             if last_char = '_' or place = 0 then
   160.                raise Data_Error;
   161.             end if;
   162.       exit;
   163.          end if;
   164.          last_char := next_char;
   165.          look_ahead(file, next_char, end_line);
   166.       exit when end_line;
   167.          if next_char in '0' .. '9' or next_char = '_' then
   168.             get(file, next_char);
   169.          else
   170.             if last_char = '_' or place = 0 then
   171.                raise Data_Error;
   172.             end if;
   173.       exit;
   174.          end if;
   175.       end loop;
   176.    end get_decimal;
   177.
   178.    procedure get_word (file : in File_Type; value : out KDF9.word) is
   179.       next_char : Character;
   180.       end_line  : Boolean;
   181.    begin
   182.       ensure_not_at_end_of_line(file);
   183.       look_ahead(file, next_char, end_line);
   184.       pragma Unreferenced(end_line);
   185.       if next_char = '#' then
   186.          get_octal(file, value);
   187.       else
   188.          get_decimal(file, value);
   189.       end if;
   190.    end get_word;
   191.
   192.    procedure get_char (file : in File_Type; value : out Character) is
   193.       end_line : Boolean;
   194.       char     : Character;
   195.    begin
   196.       ensure_not_at_end_of_line(file);
   197.       look_ahead(file, char, end_line);
   198.       if end_line then
   199.          raise Data_Error;
   200.       end if;
   201.       if char /= ' ' then
   202.          get(file, value);
   203.       end if;
   204.    end get_char;
   205.
   206. end settings.IO;
   207.

Compiling: ../Source/settings-io.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- settings.IO.ads
     2. --
     3. -- Settings-reader I/O support.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Text_IO;
    20. --
    21. with postscript;
    22.
    23. use  Ada.Text_IO;
    24.
    25. package settings.IO is
    26.
    27.    line_number : Natural := 0;
    28.
    29.    procedure open_options_file (file : in out File_Type; name : in String);
    30.
    31.    procedure close_options_file (file : in out File_Type; name : in String);
    32.
    33.    -- Check that the end of the line has not yet been reached, else raise Data_Error.
    34.    procedure ensure_not_at_end_of_line (file : in File_Type);
    35.
    36.    -- Move the reading position to the next non-blank or EOL, skipping comment.
    37.    procedure skip_to_next_non_blank (file : in File_Type);
    38.
    39.    -- Discard input until a non-empty line is reached,
    40.    --    leaving the reading position at the start of that line,
    41.    --    and incrementing line_number for each line terminator passed.
    42.    procedure skip_to_next_nonempty_line (file : in File_Type);
    43.
    44.    -- Read octal digits string as KDF9.word,
    45.    --    raising Data_Error on overflow or bad syntax.
    46.    procedure get_octal (file : in File_Type; value : out KDF9.word);
    47.
    48.    -- Read decimal digits string as KDF9.word,
    49.    --    raising Data_Error on overflow or bad syntax.
    50.    procedure get_decimal (file  : in File_Type; value : out KDF9.word);
    51.
    52.    -- Read an address as a KDF9.word in either octal or decimal,
    53.    --    using get_octal or get_decimal as indicated by the syntax.
    54.    procedure get_word (file : in File_Type; value : out KDF9.word);
    55.
    56.    -- Read the character value immediately following an octal or decimal number,
    57.    --    if it is not a space character; if it is a space, leave value unchanged.
    58.    procedure get_char (file : in File_Type; value : out Character);
    59.
    60.    package colour_IO is new Ada.Text_IO.Enumeration_IO(postscript.pen_colour);
    61.    package  width_IO is new Ada.Text_IO.Enumeration_IO(postscript.pen_tip_size);
    62.
    63. end settings.IO;

 207 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/file_interfacing.adb
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- file_IO_interface.adb
     2. --
     3. -- Provide an Ada.Text_IO interface to the file system of the real OS.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package body file_interfacing is
    20.
    21.    procedure initialize (some_file : in out File_Type;
    22.                          mode      : in File_Mode;
    23.                          file_name : in String) is
    24.    begin
    25.       Open(some_file, mode, file_name);
    26.    exception
    27.       when others =>
    28.          if mode = Out_File then
    29.             Create(some_file, Out_File, file_name);
    30.          else
    31.             raise;
    32.          end if;
    33.    end initialize;
    34.
    35.    procedure finalize (some_file : in out File_Type;
    36.                        file_name : in String) is
    37.       pragma Unreferenced(file_name);
    38.    begin
    39.       Close(some_file);
    40.    end finalize;
    41.
    42. end file_interfacing;

Compiling: ../Source/file_interfacing.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- file_interfacing.ads
     2. --
     3. -- Provide an Ada.Text_IO interface to the file system of the real OS.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with Ada.Text_IO;
    20.
    21. use  Ada.Text_IO;
    22.
    23. package file_interfacing is
    24.
    25.    procedure initialize (some_file : in out File_Type;
    26.                          mode      : in File_Mode;
    27.                          file_name : in String);
    28.
    29.    procedure finalize (some_file : in out File_Type;
    30.                        file_name : in String);
    31.
    32. end file_interfacing;

 42 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/kdf9-decoding.ads
Source file time stamp: 2020-11-02 19:40:41
Compiled at: 2020-11-12 18:12:13

     1. -- kdf9-decoding.ads
     2. --
     3. -- The "compressed_opcode" values are effective opcodes, partially decoded from the first syllable,
     4. --   and combined with opcode bits of the second syllable, where appropriate (e.g. in jumps).
     5. --
     6. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     7. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     8. --
     9. -- The ee9 program is free software; you can redistribute it and/or
    10. -- modify it under terms of the GNU General Public License as published
    11. -- by the Free Software Foundation; either version 3, or (at your option)
    12. -- any later version. This program is distributed in the hope that it
    13. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    14. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    15. -- See the GNU General Public License for more details. You should have
    16. -- received a copy of the GNU General Public License distributed with
    17. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    18. --
    19.
    20. package KDF9.decoding is
    21.
    22.    -- The compressed_opcode values for 1-syllable orders are equal to their full codes.
    23.
    24.    ALL_0    : constant KDF9.compressed_opcode := 2#000_000#;
    25.    VR       : constant KDF9.compressed_opcode := 2#000_001#;
    26.    TO_TR    : constant KDF9.compressed_opcode := 2#000_010#;
    27.    BITS     : constant KDF9.compressed_opcode := 2#000_011#;
    28.    XF       : constant KDF9.compressed_opcode := 2#000_100#;
    29.    XDF      : constant KDF9.compressed_opcode := 2#000_101#;
    30.    INV006   : constant KDF9.compressed_opcode := 2#000_110#;
    31.    XPLUSF   : constant KDF9.compressed_opcode := 2#000_111#;
    32.    NEGD     : constant KDF9.compressed_opcode := 2#001_000#;
    33.    OR_9     : constant KDF9.compressed_opcode := 2#001_001#;
    34.    PERM     : constant KDF9.compressed_opcode := 2#001_010#;
    35.    TOB      : constant KDF9.compressed_opcode := 2#001_011#;
    36.    ROUNDH   : constant KDF9.compressed_opcode := 2#001_100#;
    37.    NEV      : constant KDF9.compressed_opcode := 2#001_101#;
    38.    ROUND    : constant KDF9.compressed_opcode := 2#001_110#;
    39.    DUMMY    : constant KDF9.compressed_opcode := 2#001_111#;
    40.    ROUNDF   : constant KDF9.compressed_opcode := 2#010_000#;
    41.    ROUNDHF  : constant KDF9.compressed_opcode := 2#010_001#;
    42.    MINUSDF  : constant KDF9.compressed_opcode := 2#010_010#;
    43.    PLUSDF   : constant KDF9.compressed_opcode := 2#010_011#;
    44.    FLOAT_9  : constant KDF9.compressed_opcode := 2#010_100#;
    45.    FLOATD   : constant KDF9.compressed_opcode := 2#010_101#;
    46.    ABS_9    : constant KDF9.compressed_opcode := 2#010_110#;
    47.    NEG      : constant KDF9.compressed_opcode := 2#010_111#;
    48.    ABSF     : constant KDF9.compressed_opcode := 2#011_000#;
    49.    NEGF     : constant KDF9.compressed_opcode := 2#011_001#;
    50.    MAX      : constant KDF9.compressed_opcode := 2#011_010#;
    51.    NOT_9    : constant KDF9.compressed_opcode := 2#011_011#;
    52.    XD       : constant KDF9.compressed_opcode := 2#011_100#;
    53.    X_frac   : constant KDF9.compressed_opcode := 2#011_101#;
    54.    MINUS    : constant KDF9.compressed_opcode := 2#011_110#;
    55.    SIGN     : constant KDF9.compressed_opcode := 2#011_111#;
    56.    INV040   : constant KDF9.compressed_opcode := 2#100_000#;
    57.    ZERO     : constant KDF9.compressed_opcode := 2#100_001#;
    58.    DUP      : constant KDF9.compressed_opcode := 2#100_010#;
    59.    DUPD     : constant KDF9.compressed_opcode := 2#100_011#;
    60.    DIVI     : constant KDF9.compressed_opcode := 2#100_100#;
    61.    FIX      : constant KDF9.compressed_opcode := 2#100_101#;
    62.    INV046   : constant KDF9.compressed_opcode := 2#100_110#;
    63.    STR      : constant KDF9.compressed_opcode := 2#100_111#;
    64.    CONT     : constant KDF9.compressed_opcode := 2#101_000#;
    65.    REVD     : constant KDF9.compressed_opcode := 2#101_001#;
    66.    ERASE    : constant KDF9.compressed_opcode := 2#101_010#;
    67.    MINUSD   : constant KDF9.compressed_opcode := 2#101_011#;
    68.    AND_9    : constant KDF9.compressed_opcode := 2#101_100#;
    69.    INV055   : constant KDF9.compressed_opcode := 2#101_101#;
    70.    PLUS     : constant KDF9.compressed_opcode := 2#101_110#;
    71.    PLUSD    : constant KDF9.compressed_opcode := 2#101_111#;
    72.    DIV      : constant KDF9.compressed_opcode := 2#110_000#;
    73.    DIVD     : constant KDF9.compressed_opcode := 2#110_001#;
    74.    DIVF     : constant KDF9.compressed_opcode := 2#110_010#;
    75.    DIVDF    : constant KDF9.compressed_opcode := 2#110_011#;
    76.    DIVR     : constant KDF9.compressed_opcode := 2#110_100#;
    77.    REV      : constant KDF9.compressed_opcode := 2#110_101#;
    78.    CAB      : constant KDF9.compressed_opcode := 2#110_110#;
    79.    FRB      : constant KDF9.compressed_opcode := 2#110_111#;
    80.    STAND    : constant KDF9.compressed_opcode := 2#111_000#;
    81.    NEGDF    : constant KDF9.compressed_opcode := 2#111_001#;
    82.    MAXF     : constant KDF9.compressed_opcode := 2#111_010#;
    83.    INV073   : constant KDF9.compressed_opcode := 2#111_011#;
    84.    PLUSF    : constant KDF9.compressed_opcode := 2#111_100#;
    85.    MINUSF   : constant KDF9.compressed_opcode := 2#111_101#;
    86.    INV076   : constant KDF9.compressed_opcode := 2#111_110#;
    87.    SIGNF    : constant KDF9.compressed_opcode := 2#111_111#;
    88.
    89.
    90.    -- compressed_opcode values for 2-syllable indirect fetch and store orders
    91.
    92.    MkMq       : constant KDF9.compressed_opcode := 2#000_000#;
    93.    MkMqQ      : constant KDF9.compressed_opcode := 2#000_010#;
    94.    MkMqH      : constant KDF9.compressed_opcode := 2#000_100#;
    95.    MkMqQH     : constant KDF9.compressed_opcode := 2#000_110#;
    96.    MkMqN      : constant KDF9.compressed_opcode := 2#001_000#;
    97.    MkMqQN     : constant KDF9.compressed_opcode := 2#001_010#;
    98.    MkMqHN     : constant KDF9.compressed_opcode := 2#001_100#;
    99.    MkMqQHN    : constant KDF9.compressed_opcode := 2#001_110#;
   100.
   101.    TO_MkMq    : constant KDF9.compressed_opcode := 2#000_001#;
   102.    TO_MkMqQ   : constant KDF9.compressed_opcode := 2#000_011#;
   103.    TO_MkMqH   : constant KDF9.compressed_opcode := 2#000_101#;
   104.    TO_MkMqQH  : constant KDF9.compressed_opcode := 2#000_111#;
   105.    TO_MkMqN   : constant KDF9.compressed_opcode := 2#001_001#;
   106.    TO_MkMqQN  : constant KDF9.compressed_opcode := 2#001_011#;
   107.    TO_MkMqHN  : constant KDF9.compressed_opcode := 2#001_101#;
   108.    TO_MkMqQHN : constant KDF9.compressed_opcode := 2#001_111#;
   109.
   110.
   111.    -- compressed_opcode values for 2-syllable Q store orders
   112.
   113.    M_PLUS_Iq    : constant KDF9.compressed_opcode := 2#100_000#;
   114.    M_MINUS_Iq   : constant KDF9.compressed_opcode := 2#100_001#;
   115.    NCq          : constant KDF9.compressed_opcode := 2#100_010#;
   116.    DCq          : constant KDF9.compressed_opcode := 2#100_011#;
   117.    POS1_TO_Iq   : constant KDF9.compressed_opcode := 2#100_100#;
   118.    NEG1_TO_Iq   : constant KDF9.compressed_opcode := 2#100_101#;
   119.    POS2_TO_Iq   : constant KDF9.compressed_opcode := 2#100_110#;
   120.    NEG2_TO_Iq   : constant KDF9.compressed_opcode := 2#100_111#;
   121.
   122.    MqTOQk       : constant KDF9.compressed_opcode := 2#101_001#;
   123.    IqTOQk       : constant KDF9.compressed_opcode := 2#101_010#;
   124.    IMqTOQk      : constant KDF9.compressed_opcode := 2#101_011#;
   125.    CqTOQk       : constant KDF9.compressed_opcode := 2#101_100#;
   126.    CMqTOQk      : constant KDF9.compressed_opcode := 2#101_101#;
   127.    CIqTOQk      : constant KDF9.compressed_opcode := 2#101_110#;
   128.    QqTOQk       : constant KDF9.compressed_opcode := 2#101_111#;
   129.
   130.    SHA          : constant KDF9.compressed_opcode := 2#110_001#;
   131.    SHAD         : constant KDF9.compressed_opcode := 2#110_010#;
   132.    MACC         : constant KDF9.compressed_opcode := 2#110_011#;
   133.    SHL          : constant KDF9.compressed_opcode := 2#110_100#;
   134.    SHLD         : constant KDF9.compressed_opcode := 2#110_110#;
   135.    SHC          : constant KDF9.compressed_opcode := 2#110_111#;
   136.    constant_bit : constant := 1;
   137.
   138.    TO_RCIMq     : constant KDF9.compressed_opcode := 2#111_000#;
   139.    QCIMq        : constant KDF9.compressed_opcode := 2#111_001#;
   140.    ADD_TO_QCIMq : constant KDF9.compressed_opcode := 2#111_010#;
   141.
   142.    -- masks for Q store Qk bits
   143.
   144.    reset_choice  : constant := 2#0001#;
   145.    C_part_choice : constant := 2#1000#;
   146.    I_part_choice : constant := 2#0100#;
   147.    M_part_choice : constant := 2#0010#;
   148.    all_Q_choice  : constant := C_part_choice + I_part_choice + M_part_choice;
   149.
   150.
   151.    -- compressed_opcode values for 2-syllable SJNS orders
   152.
   153.    LINK    : constant KDF9.compressed_opcode := 2#111_011#;
   154.    TO_LINK : constant KDF9.compressed_opcode := 2#111_100#;
   155.
   156.
   157.    -- compressed_opcode values for 2-syllable Director-only orders
   158.
   159.    TO_Kq : constant KDF9.compressed_opcode := 2#111_101#;
   160.    K0    : constant := 2#1000#;
   161.    K1    : constant := 2#0100#;
   162.    K2    : constant := 2#0010#;
   163.    K3    : constant := 2#0001#;
   164.    Kk    : constant KDF9.compressed_opcode := 2#111_110#;
   165.    K4    : constant := 2#1000#;
   166.    K5    : constant := 2#0100#;
   167.    K7    : constant := 2#0001#;
   168.
   169.
   170.    -- compressed_opcode value for 2-syllable short-loop jump order
   171.
   172.    JCqNZS : constant KDF9.compressed_opcode := 2#111_111#;
   173.
   174.
   175.    -- compressed_opcode values for 2-syllable I/O orders
   176.
   177.    CT_PMB_PMC_BUSY_Qq     : constant KDF9.compressed_opcode := 2#010_000#;
   178.    PAR_Qq                 : constant KDF9.compressed_opcode := 2#010_001#;
   179.    PMF_PMG_Qq             : constant KDF9.compressed_opcode := 2#010_010#;
   180.    PIA_PIC_CLO_TLO_Qq     : constant KDF9.compressed_opcode := 2#010_100#;
   181.    PIB_PID_Qq             : constant KDF9.compressed_opcode := 2#010_101#;
   182.    PIE_PIG_Qq             : constant KDF9.compressed_opcode := 2#010_110#;
   183.    PIF_PIH_Qq             : constant KDF9.compressed_opcode := 2#010_111#;
   184.
   185.    POA_POC_POE_POF_PMH_Qq : constant KDF9.compressed_opcode := 2#011_000#;
   186.    POB_POD_Qq             : constant KDF9.compressed_opcode := 2#011_001#;
   187.    POG_POL_Qq             : constant KDF9.compressed_opcode := 2#011_010#;
   188.    POH_POK_Qq             : constant KDF9.compressed_opcode := 2#011_011#;
   189.    PMA_PMK_INT_Qq         : constant KDF9.compressed_opcode := 2#011_100#;
   190.    PMD_PME_PML_Qq         : constant KDF9.compressed_opcode := 2#011_110#;
   191.
   192.    -- masks for I/O opcode extension bits (Qk field)
   193.
   194.    PAR_bits  : constant := 2#0000#;
   195.
   196.    -- PIA_PIC_CLO_TLO_Qq:
   197.    PIA_bits  : constant := 2#0000#;
   198.    PIC_bits  : constant := 2#1000#;
   199.    CLO_bits  : constant := 2#0010#;
   200.    TLO_bits  : constant := 2#0100#;
   201.
   202.    -- PIB_PID_Qq:
   203.    PIB_bits  : constant := 2#0000#;
   204.    PID_bits  : constant := 2#1000#;
   205.
   206.    -- PIE_PIG_Qq:
   207.    PIE_bits  : constant := 2#0000#;
   208.    PIG_bits  : constant := 2#1000#;
   209.
   210.    -- PIF_PIH_Qq:
   211.    PIF_bits  : constant := 2#0000#;
   212.    PIH_bits  : constant := 2#1000#;
   213.
   214.    -- PMA_PMK_INT_Qq:
   215.    PMA_bits  : constant := 2#0000#;
   216.    PMK_bits  : constant := 2#0100#;
   217.    INT_bits  : constant := 2#0010#;
   218.
   219.    -- CT_PMB_PMC_BUSY_Qq:
   220.    CTQ_bits   : constant := 2#0000#;
   221.    PMB_bits   : constant := 2#1000#;
   222.    PMC_bits   : constant := 2#0100#;
   223.    BUSY_bits  : constant := 2#0010#;
   224.    manual_bit : constant := 2#0001#;
   225.
   226.    -- PMD_PME_PML_Qq:
   227.    PME_bits  : constant := 2#0000#;
   228.    PMD_bits  : constant := 2#1000#;
   229.    PML_bits  : constant := 2#0100#;
   230.
   231.    -- PMF_PMG_Qq:
   232.    PMF_bits  : constant := 2#0000#;
   233.    PMG_bits  : constant := 2#0100#;
   234.
   235.    -- POA_POC_POE_POF_PMH_Qq:
   236.    POA_bits  : constant := 2#0000#;
   237.    POC_bits  : constant := 2#1000#;
   238.    POE_bits  : constant := 2#1100#;
   239.    POF_bits  : constant := 2#0100#;
   240.    PMH_bits  : constant := 2#0010#;
   241.
   242.    -- POB_POD_Qq:
   243.    POB_bits  : constant := 2#0000#;
   244.    POD_bits  : constant := 2#1000#;
   245.
   246.    -- POG_POL_Qq:
   247.    POG_bits  : constant := 2#0000#;
   248.    POL_bits  : constant := 2#1000#;
   249.
   250.    -- POH_POK_Qq:
   251.    POH_bits  : constant := 2#0000#;
   252.    POK_bits  : constant := 2#1000#;
   253.
   254.
   255.    -- compressed_opcode values for normal jump orders
   256.
   257.    JrNE   : constant KDF9.compressed_opcode := 2#000_001#;
   258.    JrGEZ  : constant KDF9.compressed_opcode := 2#000_010#;
   259.    JrLEZ  : constant KDF9.compressed_opcode := 2#000_100#;
   260.    JrNEZ  : constant KDF9.compressed_opcode := 2#000_110#;
   261.    JrNV   : constant KDF9.compressed_opcode := 2#001_000#;
   262.    OS_OUT : constant KDF9.compressed_opcode := 2#001_001#;
   263.    JrNEN  : constant KDF9.compressed_opcode := 2#001_010#;
   264.    Jr     : constant KDF9.compressed_opcode := 2#001_011#;
   265.    JrNEJ  : constant KDF9.compressed_opcode := 2#001_100#;
   266.    JSr    : constant KDF9.compressed_opcode := 2#001_101#;
   267.    JrNTR  : constant KDF9.compressed_opcode := 2#001_110#;
   268.    EXIT_n : constant KDF9.compressed_opcode := 2#001_111#;  -- 0h0 in bits 5-7
   269.    JrEQ   : constant KDF9.compressed_opcode := 2#010_001#;
   270.    JrLTZ  : constant KDF9.compressed_opcode := 2#010_010#;
   271.    JrGTZ  : constant KDF9.compressed_opcode := 2#010_100#;
   272.    JrEQZ  : constant KDF9.compressed_opcode := 2#010_110#;
   273.    JrV    : constant KDF9.compressed_opcode := 2#011_000#;
   274.    JrEN   : constant KDF9.compressed_opcode := 2#011_010#;
   275.    JrEJ   : constant KDF9.compressed_opcode := 2#011_100#;
   276.    JrTR   : constant KDF9.compressed_opcode := 2#011_110#;
   277.    EXITD  : constant KDF9.compressed_opcode := 2#011_111#;  -- 010 in bits 5-7
   278.    JrCqZ  : constant KDF9.compressed_opcode := 2#100_000#;
   279.    JrCqNZ : constant KDF9.compressed_opcode := 2#110_000#;
   280.
   281.    EXIT_1_bit : constant := 2#010#;  -- 0h0 in bits 5-7 of EXIT syllable_0
   282.
   283.
   284.    -- compressed_opcode values for directly-addressed data access orders
   285.
   286.    EaMq     : constant KDF9.compressed_opcode := 2#000_000#;
   287.    TO_EaMq  : constant KDF9.compressed_opcode := 2#000_001#;
   288.    EaMqQ    : constant KDF9.compressed_opcode := 2#000_010#;
   289.    TO_EaMqQ : constant KDF9.compressed_opcode := 2#000_011#;
   290.    SET      : constant KDF9.compressed_opcode := 2#000_100#;
   291.
   292.
   293. end KDF9.decoding;

 293 lines: No errors

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.

GNAT Community 2020 (20200429-84)
Copyright 1992-2020, Free Software Foundation, Inc.


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/magtape_data.ads
Source file time stamp: 2020-08-18 10:42:20
Compiled at: 2020-11-12 18:12:13

     1. -- magtape_data.ads
     2. --
     3. -- This contains information used by magnetic tape emulation.
     4. -- It is separated out to allow use in programs other than ee9.
     5. --
     6. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     7. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     8. --
     9. -- The ee9 program is free software; you can redistribute it and/or
    10. -- modify it under terms of the GNU General Public License as published
    11. -- by the Free Software Foundation; either version 3, or (at your option)
    12. -- any later version. This program is distributed in the hope that it
    13. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    14. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    15. -- See the GNU General Public License for more details. You should have
    16. -- received a copy of the GNU General Public License distributed with
    17. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    18. --
    19.
    20. with Ada.Characters.Latin_1;
    21.
    22. use  Ada.Characters.Latin_1;
    23.
    24. package magtape_data is
    25.
    26.    -- 81 is efficient for both card images and full 160-column print lines with final LS or PC.
    27.    slice_size_limit : constant := 81;
    28.
    29.    MT_record_length : constant := slice_size_limit + 3;
    30.
    31.    subtype valid_slice_kinds is Character
    32.       with Static_Predicate => valid_slice_kinds in 'D' | 'G' | 'W' | 'e' | 'o';
    33.
    34.    subtype erasure_kinds is Character
    35.       with Static_Predicate => erasure_kinds in 'G' | 'W';
    36.
    37.    subtype tape_mark_kinds is Character
    38.       with Static_Predicate => tape_mark_kinds in 'e' | 'o';
    39.
    40.    subtype valid_slice_flags is Character
    41.       with Static_Predicate => valid_slice_flags in NUL | SOH | BS | HT | '@' | 'A' | 'H' | 'I';
    42.
    43.    subtype final_slice_flags is Character
    44.       with Static_Predicate => final_slice_flags in BS | HT | 'H' | 'I';
    45.
    46.    subtype last_block_flags is Character
    47.       with Static_Predicate => last_block_flags in '@' | 'A' | 'H' | 'I';
    48.
    49.    -- This is the (only) character written to a 7-track tape mark block.
    50.    -- It is the § character on file, but is read back as #17 by the PI?Qq orders.
    51.    -- Section_Sign is appropriate because tape marks are used to delimit sections of a tape file.
    52.    -- See Manual, Appendix 7 ¶2, p317.
    53.    tape_mark_sign  : constant Character := Section_Sign;
    54.
    55.    block_padding   : constant Character := Middle_Dot;
    56.
    57.    -- This subtype is used in the post-processing of OUT 8 spool tapes.
    58.    subtype OUT8_selection_characters is Character
    59.       with Static_Predicate => OUT8_selection_characters in '#' | '_' | '@' | '"';
    60.
    61. end magtape_data;

 61 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/ioc-dispatcher.adb
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- ioc-dispatcher.adb
     2. --
     3. -- CPU I/O orders are dispatched here to device-specific handlers within the IOC type hierarchy.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with KDF9.PHU_store;
    20. with tracing;
    21.
    22. use  tracing;
    23.
    24. package body IOC.dispatcher is
    25.
    26.    --
    27.    --
    28.    -- CLO, SLO and TLO do not operate on a buffer, and so can be fully implemented here.
    29.    --
    30.    --
    31.
    32.    procedure CLO (Q_operand   : in KDF9.Q_register;
    33.                   set_offline : in Boolean) is
    34.       pragma Unreferenced(set_offline);
    35.       use  KDF9.PHU_store;
    36.    begin
    37.       -- This is a Director-only instruction.
    38.       take_note_of_test("   ", Q_operand, False);
    39.       unlock_absolute_addresses(Q_operand);
    40.       -- CLO also clears PHU[CPL].
    41.       PHU(CPL) := idle_PHU;
    42.       add_in_the_IO_lockout_CPU_time(Q_operand);
    43.    end CLO;
    44.
    45.    procedure SLO (Q_operand   : in KDF9.Q_register;
    46.                   set_offline : in Boolean) is
    47.       pragma Unreferenced(set_offline);
    48.    begin
    49.       -- This is a Director-only instruction.
    50.       take_note_of_test("   ", Q_operand, False);
    51.       lock_out_absolute_addresses(Q_operand);
    52.       add_in_the_IO_lockout_CPU_time(Q_operand);
    53.    end SLO;
    54.
    55.    procedure TLO (Q_operand   : in KDF9.Q_register;
    56.                   result      : out Boolean) is
    57.    begin
    58.       -- This is NOT Director-only.
    59.       result := there_are_locks_in_relative_addresses(Q_operand);
    60.       take_note_of_test("   ", Q_operand, result);
    61.       add_in_the_IO_lockout_CPU_time(Q_operand);
    62.    end TLO;
    63.
    64.    --
    65.    --
    66.    -- All other I/O orders do access a buffer, and so dispatch to the relevant device driver.
    67.    --
    68.    --
    69.
    70.    procedure BUSY (Q_operand   : in KDF9.Q_register;
    71.                    set_offline : in Boolean;
    72.                    result      : out Boolean) is
    73.    begin
    74.       buffer(Q_operand.C and buffer_number_mask).BUSY(Q_operand, set_offline, result);
    75.    end BUSY;
    76.
    77.    procedure PAR (Q_operand   : in KDF9.Q_register;
    78.                   set_offline : in Boolean;
    79.                   result      : out Boolean) is
    80.    begin
    81.       buffer(Q_operand.C and buffer_number_mask).PAR(Q_operand, set_offline, result);
    82.    end PAR;
    83.
    84.    procedure MANUAL_CT (Q_operand   : in KDF9.Q_register;
    85.                         set_offline : in Boolean) is
    86.    begin
    87.       buffer(Q_operand.C and buffer_number_mask).MANUAL_CT(Q_operand, set_offline);
    88.    end MANUAL_CT;
    89.
    90.    procedure INT (Q_operand   : in KDF9.Q_register;
    91.                   set_offline : in Boolean) is
    92.    begin
    93.       buffer(Q_operand.C and buffer_number_mask).INT(Q_operand, set_offline);
    94.    end INT;
    95.
    96.    procedure PIA (Q_operand   : in KDF9.Q_register;
    97.                   set_offline : in Boolean) is
    98.    begin
    99.       buffer(Q_operand.C and buffer_number_mask).PIA(Q_operand, set_offline);
   100.       add_in_the_IO_lockout_CPU_time(Q_operand);
   101.    end PIA;
   102.
   103.    procedure PIB (Q_operand   : in KDF9.Q_register;
   104.                   set_offline : in Boolean) is
   105.    begin
   106.       buffer(Q_operand.C and buffer_number_mask).PIB(Q_operand, set_offline);
   107.       add_in_the_IO_lockout_CPU_time(Q_operand);
   108.    end PIB;
   109.
   110.    procedure PIC (Q_operand   : in KDF9.Q_register;
   111.                   set_offline : in Boolean) is
   112.    begin
   113.       buffer(Q_operand.C and buffer_number_mask).PIC(Q_operand, set_offline);
   114.       add_in_the_IO_lockout_CPU_time(Q_operand);
   115.    end PIC;
   116.
   117.    procedure PID (Q_operand   : in KDF9.Q_register;
   118.                   set_offline : in Boolean) is
   119.    begin
   120.       buffer(Q_operand.C and buffer_number_mask).PID(Q_operand, set_offline);
   121.       add_in_the_IO_lockout_CPU_time(Q_operand);
   122.    end PID;
   123.
   124.    procedure PIE (Q_operand   : in KDF9.Q_register;
   125.                   set_offline : in Boolean) is
   126.    begin
   127.       buffer(Q_operand.C and buffer_number_mask).PIE(Q_operand, set_offline);
   128.       add_in_the_IO_lockout_CPU_time(Q_operand);
   129.    end PIE;
   130.
   131.    procedure PIF (Q_operand   : in KDF9.Q_register;
   132.                   set_offline : in Boolean) is
   133.    begin
   134.       buffer(Q_operand.C and buffer_number_mask).PIF(Q_operand, set_offline);
   135.       add_in_the_IO_lockout_CPU_time(Q_operand);
   136.    end PIF;
   137.
   138.    procedure PIG (Q_operand   : in KDF9.Q_register;
   139.                   set_offline : in Boolean) is
   140.    begin
   141.       buffer(Q_operand.C and buffer_number_mask).PIG(Q_operand, set_offline);
   142.       add_in_the_IO_lockout_CPU_time(Q_operand);
   143.    end PIG;
   144.
   145.    procedure PIH (Q_operand   : in KDF9.Q_register;
   146.                   set_offline : in Boolean) is
   147.    begin
   148.       buffer(Q_operand.C and buffer_number_mask).PIH(Q_operand, set_offline);
   149.       add_in_the_IO_lockout_CPU_time(Q_operand);
   150.    end PIH;
   151.
   152.    procedure PMA (Q_operand   : in KDF9.Q_register;
   153.                   set_offline : in Boolean) is
   154.    begin
   155.       buffer(Q_operand.C and buffer_number_mask).PMA(Q_operand, set_offline);
   156.    end PMA;
   157.
   158.    procedure PMB (Q_operand   : in KDF9.Q_register;
   159.                   set_offline : in Boolean) is
   160.    begin
   161.       buffer(Q_operand.C and buffer_number_mask).PMB(Q_operand, set_offline);
   162.    end PMB;
   163.
   164.    procedure PMC (Q_operand   : in KDF9.Q_register;
   165.                   set_offline : in Boolean) is
   166.    begin
   167.       buffer(Q_operand.C and buffer_number_mask).PMC(Q_operand, set_offline);
   168.    end PMC;
   169.
   170.    procedure PMD (Q_operand   : in KDF9.Q_register;
   171.                   set_offline : in Boolean) is
   172.    begin
   173.       buffer(Q_operand.C and buffer_number_mask).PMD(Q_operand, set_offline);
   174.    end PMD;
   175.
   176.    procedure PME (Q_operand   : in KDF9.Q_register;
   177.                   set_offline : in Boolean) is
   178.    begin
   179.       buffer(Q_operand.C and buffer_number_mask).PME(Q_operand, set_offline);
   180.    end PME;
   181.
   182.    procedure PMF (Q_operand   : in KDF9.Q_register;
   183.                   set_offline : in Boolean) is
   184.    begin
   185.       buffer(Q_operand.C and buffer_number_mask).PMF(Q_operand, set_offline);
   186.    end PMF;
   187.
   188.    procedure PMG (Q_operand   : in KDF9.Q_register;
   189.                   set_offline : in Boolean) is
   190.    begin
   191.       buffer(Q_operand.C and buffer_number_mask).PMG(Q_operand, set_offline);
   192.    end PMG;
   193.
   194.    procedure PMK (Q_operand   : in KDF9.Q_register;
   195.                   set_offline : in Boolean) is
   196.    begin
   197.       buffer(Q_operand.C and buffer_number_mask).PMK(Q_operand, set_offline);
   198.    end PMK;
   199.
   200.    procedure PML (Q_operand   : in KDF9.Q_register;
   201.                   set_offline : in Boolean) is
   202.    begin
   203.       buffer(Q_operand.C and buffer_number_mask).PML(Q_operand, set_offline);
   204.    end PML;
   205.
   206.    procedure POA (Q_operand   : in KDF9.Q_register;
   207.                   set_offline : in Boolean) is
   208.    begin
   209.       buffer(Q_operand.C and buffer_number_mask).POA(Q_operand, set_offline);
   210.       add_in_the_IO_lockout_CPU_time(Q_operand);
   211.    end POA;
   212.
   213.    procedure POB (Q_operand   : in KDF9.Q_register;
   214.                   set_offline : in Boolean) is
   215.    begin
   216.       buffer(Q_operand.C and buffer_number_mask).POB(Q_operand, set_offline);
   217.       add_in_the_IO_lockout_CPU_time(Q_operand);
   218.    end POB;
   219.
   220.    procedure POC (Q_operand   : in KDF9.Q_register;
   221.                   set_offline : in Boolean) is
   222.    begin
   223.       buffer(Q_operand.C and buffer_number_mask).POC(Q_operand, set_offline);
   224.       add_in_the_IO_lockout_CPU_time(Q_operand);
   225.    end POC;
   226.
   227.    procedure POD (Q_operand   : in KDF9.Q_register;
   228.                   set_offline : in Boolean) is
   229.    begin
   230.       buffer(Q_operand.C and buffer_number_mask).POD(Q_operand, set_offline);
   231.       add_in_the_IO_lockout_CPU_time(Q_operand);
   232.    end POD;
   233.
   234.    procedure POE (Q_operand   : in KDF9.Q_register;
   235.                   set_offline : in Boolean) is
   236.    begin
   237.       buffer(Q_operand.C and buffer_number_mask).POE(Q_operand, set_offline);
   238.    end POE;
   239.
   240.    procedure POF (Q_operand   : in KDF9.Q_register;
   241.                   set_offline : in Boolean) is
   242.    begin
   243.       buffer(Q_operand.C and buffer_number_mask).POF(Q_operand, set_offline);
   244.    end POF;
   245.
   246.    procedure POG (Q_operand   : in KDF9.Q_register;
   247.                   set_offline : in Boolean) is
   248.    begin
   249.       buffer(Q_operand.C and buffer_number_mask).POG(Q_operand, set_offline);
   250.    end POG;
   251.
   252.    procedure POH (Q_operand   : in KDF9.Q_register;
   253.                   set_offline : in Boolean) is
   254.    begin
   255.       buffer(Q_operand.C and buffer_number_mask).POH(Q_operand, set_offline);
   256.    end POH;
   257.
   258.    procedure POK (Q_operand   : in KDF9.Q_register;
   259.                   set_offline : in Boolean) is
   260.    begin
   261.       buffer(Q_operand.C and buffer_number_mask).POK(Q_operand, set_offline);
   262.    end POK;
   263.
   264.    procedure POL (Q_operand   : in KDF9.Q_register;
   265.                   set_offline : in Boolean) is
   266.    begin
   267.       buffer(Q_operand.C and buffer_number_mask).POL(Q_operand, set_offline);
   268.    end POL;
   269.
   270. end IOC.dispatcher;

Compiling: ../Source/ioc-dispatcher.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- ioc-dispatcher.ads
     2. --
     3. -- CPU I/O orders are dispatched here to device-specific handlers within the IOC type hierarchy.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. package IOC.dispatcher is
    20.
    21.    procedure MANUAL_CT (Q_operand   : in KDF9.Q_register;
    22.                         set_offline : in Boolean);
    23.
    24.    procedure BUSY (Q_operand   : in KDF9.Q_register;
    25.                    set_offline : in Boolean;
    26.                    result      : out Boolean);
    27.
    28.    procedure PAR (Q_operand   : in KDF9.Q_register;
    29.                   set_offline : in Boolean;
    30.                   result      : out Boolean);
    31.
    32.    procedure TLO (Q_operand   : in KDF9.Q_register;
    33.                   result      : out Boolean);
    34.
    35.    procedure CLO (Q_operand   : in KDF9.Q_register;
    36.                   set_offline : in Boolean);
    37.
    38.    procedure SLO (Q_operand   : in KDF9.Q_register;
    39.                   set_offline : in Boolean);
    40.
    41.    procedure INT (Q_operand   : in KDF9.Q_register;
    42.                   set_offline : in Boolean);
    43.
    44.    procedure PIA (Q_operand   : in KDF9.Q_register;
    45.                   set_offline : in Boolean);
    46.
    47.    procedure PIB (Q_operand   : in KDF9.Q_register;
    48.                   set_offline : in Boolean);
    49.
    50.    procedure PIC (Q_operand   : in KDF9.Q_register;
    51.                   set_offline : in Boolean);
    52.
    53.    procedure PID (Q_operand   : in KDF9.Q_register;
    54.                   set_offline : in Boolean);
    55.
    56.    procedure PIE (Q_operand   : in KDF9.Q_register;
    57.                   set_offline : in Boolean);
    58.
    59.    procedure PIF (Q_operand   : in KDF9.Q_register;
    60.                   set_offline : in Boolean);
    61.
    62.    procedure PIG (Q_operand   : in KDF9.Q_register;
    63.                   set_offline : in Boolean);
    64.
    65.    procedure PIH (Q_operand   : in KDF9.Q_register;
    66.                   set_offline : in Boolean);
    67.
    68.    procedure PMA (Q_operand   : in KDF9.Q_register;
    69.                   set_offline : in Boolean);
    70.
    71.    procedure PMB (Q_operand   : in KDF9.Q_register;
    72.                   set_offline : in Boolean);
    73.
    74.    procedure PMC (Q_operand   : in KDF9.Q_register;
    75.                   set_offline : in Boolean);
    76.
    77.    procedure PMD (Q_operand   : in KDF9.Q_register;
    78.                   set_offline : in Boolean);
    79.
    80.    procedure PME (Q_operand   : in KDF9.Q_register;
    81.                   set_offline : in Boolean);
    82.
    83.    procedure PMF (Q_operand   : in KDF9.Q_register;
    84.                   set_offline : in Boolean);
    85.
    86.    procedure PMG (Q_operand   : in KDF9.Q_register;
    87.                   set_offline : in Boolean);
    88.
    89.    procedure PMK (Q_operand   : in KDF9.Q_register;
    90.                   set_offline : in Boolean);
    91.
    92.    procedure PML (Q_operand   : in KDF9.Q_register;
    93.                   set_offline : in Boolean);
    94.
    95.    procedure POA (Q_operand   : in KDF9.Q_register;
    96.                   set_offline : in Boolean);
    97.
    98.    procedure POB (Q_operand   : in KDF9.Q_register;
    99.                   set_offline : in Boolean);
   100.
   101.    procedure POC (Q_operand   : in KDF9.Q_register;
   102.                   set_offline : in Boolean);
   103.
   104.    procedure POD (Q_operand   : in KDF9.Q_register;
   105.                   set_offline : in Boolean);
   106.
   107.    procedure POE (Q_operand   : in KDF9.Q_register;
   108.                   set_offline : in Boolean);
   109.
   110.    procedure POF (Q_operand   : in KDF9.Q_register;
   111.                   set_offline : in Boolean);
   112.
   113.    procedure POG (Q_operand   : in KDF9.Q_register;
   114.                   set_offline : in Boolean);
   115.
   116.    procedure POH (Q_operand   : in KDF9.Q_register;
   117.                   set_offline : in Boolean);
   118.
   119.    procedure POK (Q_operand   : in KDF9.Q_register;
   120.                   set_offline : in Boolean);
   121.
   122.    procedure POL (Q_operand   : in KDF9.Q_register;
   123.                   set_offline : in Boolean);
   124.
   125. end IOC.dispatcher;
   126.

 270 lines: No errors


Compiling: /Users/wf/Documents/KDF9 Material/ee9_archive/V5/emulation/Source/plotter.adb
Source file time stamp: 2020-11-07 00:34:37
Compiled at: 2020-11-12 18:12:13

     1. -- plotter.adb
     2. --
     3. -- Emulation of the plotting commands of the Calcomp 564 graph plotter.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with formatting;
    20. with KDF9;
    21. with postscript;
    22.
    23. use  formatting;
    24. use  KDF9;
    25. use  postscript;
    26.
    27. package body plotter is
    28.
    29.    -- The plotter made equal movements in the x and y directions, in steps of 0.005 inches.
    30.    -- Each command moves the plotting position by at most 1 step,
    31.    --   in either the positive or negative direction of each axis.
    32.
    33.    type step is
    34.       record
    35.          dx, dy : Integer range -1 .. +1;
    36.       end record;
    37.
    38.    null_step : constant plotter.step := (0, 0);
    39.
    40.    function image (s : plotter.step)
    41.    return String
    42.    is ("<" & trimmed(s.dx'Image) & ", " & trimmed(s.dy'Image) & ">");
    43.
    44.    function "+" (p : postscript.point; s : plotter.step)
    45.    return postscript.point
    46.    is ((p.x + s.dx, p.y + s.dy));
    47.
    48.    function "-" (p, q : postscript.point)
    49.    return plotter.step
    50.    is ((p.x - q.x, p.y - q.y));
    51.
    52.    -- The plotter drew on a roll of paper 29.5 inches wide and 120 feet long.
    53.    -- 29.5" is   5900 steps at 200 steps per inch = 59 * 100
    54.    -- 120'  is 288000 steps  = 200 per inch * 12 inches per foot * 120 feet.
    55.    -- This sets the boundaries of the plot.
    56.    -- It was physically impossible to move to a point beyond these limits.
    57.
    58.    plot_limit : constant postscript.point := (120*12*200, 59*100);
    59.
    60.    -- A vector is represented by a series of consecutive colinear plotter movements.
    61.    -- For better efficiency, the steps of a vector are accumulated until there is
    62.    --    a change of direction, a pen lift, or the need to close the plotter file.
    63.    -- On these events, any vector thus defined is drawn via a single PostScript command.
    64.
    65.    the_origin      : constant postscript.point := (0, 0);
    66.
    67.    plot_position,
    68.    start_position  : postscript.point := the_origin;
    69.
    70.    -- last_step retains the direction of the previous plotter step.
    71.    last_step       : plotter.step := null_step;
    72.
    73.    the_pen_is_down : Boolean := False;
    74.
    75.    procedure notify_invalid_movement (this  : in postscript.point;
    76.                                       after : in plotter.step)
    77.       with Inline => False, No_Return;
    78.
    79.    procedure notify_invalid_movement (this  : in postscript.point;
    80.                                       after : in plotter.step) is
    81.    begin
    82.       trap_invalid_operand("impossible movement for plotter from "
    83.                        & image(this)
    84.                        & " by "
    85.                        & image(after));
    86.    end notify_invalid_movement;
    87.
    88.    procedure ensure_the_validity_of (this  : in postscript.point;
    89.                                      after : in plotter.step) is
    90.    begin
    91.       if this.x + after.dx < 0                     or else
    92.             this.y + after.dy < 0                  or else
    93.                this.x + after.dx > plot_limit.x    or else
    94.                   this.y + after.dy > plot_limit.y    then
    95.          notify_invalid_movement(this, after);
    96.       end if;
    97.    end ensure_the_validity_of;
    98.
    99.    procedure jump_to (p : in postscript.point)
   100.       with Inline;
   101.
   102.    procedure jump_to (p : in postscript.point) is
   103.    begin
   104.       -- Posit a new vector starting at p.
   105.       last_step := null_step;
   106.       plot_position := p;
   107.       start_position := p;
   108.    end;
   109.
   110.    procedure jump_by (this_step : in plotter.step)
   111.       with Inline;
   112.
   113.    procedure jump_by (this_step : in plotter.step) is
   114.    begin
   115.       ensure_the_validity_of(this => plot_position, after => this_step);
   116.       jump_to(plot_position + this_step);
   117.    end jump_by;
   118.
   119.    procedure close_any_open_vector (stream : in out host_IO.stream) is
   120.    begin
   121.       if the_pen_is_down                and then
   122.             start_position /= plot_position then
   123.          draw_a_PS_vector(stream, start_position, plot_position);
   124.          start_position := plot_position;
   125.       end if;
   126.    end close_any_open_vector;
   127.
   128.    procedure perform (action : in plotter.command; stream : in out host_IO.stream) is
   129.
   130.       procedure draw_to (p : in postscript.point)
   131.          with Inline;
   132.
   133.       procedure draw_to (p : in postscript.point) is
   134.       begin
   135.          if (plot_position - p) = last_step then
   136.             -- p is colinear with the previous step, so merely extend the vector to p.
   137.             plot_position := p;
   138.          else
   139.             -- Draw the whole vector and start a new one.
   140.             draw_a_PS_vector(stream, start_position, plot_position);
   141.             last_step := plot_position - p;
   142.             start_position := plot_position;
   143.             plot_position := p;
   144.          end if;
   145.       end draw_to;
   146.
   147.       procedure draw_by (this_step : in plotter.step)
   148.          with Inline;
   149.
   150.       procedure draw_by (this_step : in plotter.step) is
   151.       begin
   152.          ensure_the_validity_of(this => plot_position, after => this_step);
   153.          draw_to(plot_position + this_step);
   154.       end draw_by;
   155.
   156.       procedure move_by (this_step : in plotter.step)
   157.          with Inline;
   158.
   159.       procedure move_by (this_step : in plotter.step) is
   160.       begin
   161.          -- Convert from natural orientation of X axis to PostScript direction.
   162.          if the_pen_is_down then
   163.             draw_by((-this_step.dx, +this_step.dy));
   164.          else
   165.             jump_by((-this_step.dx, +this_step.dy));
   166.          end if;
   167.       end move_by;
   168.
   169.    begin -- perform
   170.       case action is
   171.          when dummy =>
   172.             null;
   173.          when pen_up =>
   174.             close_any_open_vector(stream);
   175.             the_pen_is_down := False;
   176.          when pen_down =>
   177.             the_pen_is_down := True;
   178.          when go_pY =>
   179.             move_by((+0, +1));
   180.          when go_nY =>
   181.             move_by((+0, -1));
   182.          when go_pX =>
   183.             move_by((+1, +0));
   184.          when go_nX =>
   185.             move_by((-1, +0));
   186.          when go_pXpY =>
   187.             move_by((+1, +1));
   188.          when go_nXnY =>
   189.             move_by((-1, -1));
   190.          when go_pXnY =>
   191.             move_by((+1, -1));
   192.          when go_nXpY =>
   193.             move_by((-1, +1));
   194.          when others =>
   195.             -- EM causes a 'peculiar' motion, according to the Manual, Appendix 5.2, p.303,
   196.             --    and other codes cause 'unpredictable' effects.
   197.             -- ee9 therefore performs an arbitrary, but safe, operation: moving to the origin.
   198.             close_any_open_vector(stream);
   199.             plot_position := the_origin;
   200.       end case;
   201.    end perform;
   202.
   203.    a_plot_is_open : Boolean := False;
   204.
   205.    procedure open_the_plot_file (stream : in out host_IO.stream) is
   206.    begin
   207.       if a_plot_is_open then
   208.          close_the_plot_file(stream);
   209.       end if;
   210.       plot_position := (0,0);
   211.       a_plot_is_open := True;
   212.    end open_the_plot_file;
   213.
   214.    procedure close_the_plot_file (stream : in out host_IO.stream) is
   215.    begin
   216.       if not a_plot_is_open then
   217.          return;
   218.       end if;
   219.       close_any_open_vector(stream);
   220.       a_plot_is_open := False;
   221.    end close_the_plot_file;
   222.
   223. end plotter;

Compiling: ../Source/plotter.ads
Source file time stamp: 2020-06-22 15:57:38
Compiled at: 2020-11-12 18:12:13

     1. -- plotter.ads
     2. --
     3. -- Emulation of the plotting commands of the Calcomp 564 graph plotter.
     4. --
     5. -- This file is part of ee9 (V5.1a), the GNU Ada emulator of the English Electric KDF9.
     6. -- Copyright (C) 2020, W. Findlay; all rights reserved.
     7. --
     8. -- The ee9 program is free software; you can redistribute it and/or
     9. -- modify it under terms of the GNU General Public License as published
    10. -- by the Free Software Foundation; either version 3, or (at your option)
    11. -- any later version. This program is distributed in the hope that it
    12. -- will be useful, but WITHOUT ANY WARRANTY; without even the implied
    13. -- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    14. -- See the GNU General Public License for more details. You should have
    15. -- received a copy of the GNU General Public License distributed with
    16. -- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
    17. --
    18.
    19. with host_IO;
    20. with KDF9_char_sets;
    21.
    22. use  host_IO;
    23.
    24. package plotter is
    25.
    26.    procedure open_the_plot_file (stream : in out host_IO.stream);
    27.
    28.    procedure close_the_plot_file (stream : in out host_IO.stream);
    29.
    30.    type command is new KDF9_char_sets.symbol;
    31.
    32.    -- The KDF9 plotting commands are defined in the Manual, Appendix 6, §5.3, pp. 303-304.
    33.
    34.    -- BUT there is obviously an error in the Manual, as only 9 of the claimed 11 command
    35.    --    codes are listed, and the last, go_nXnY, is coded inconsistently with the others.
    36.
    37.    -- Hypothesis: the table should read as follows:
    38.
    39.    dummy    : constant plotter.command := 2#000_000#;
    40.
    41.    pen_up   : constant plotter.command := 2#100_000#;
    42.    pen_down : constant plotter.command := 2#010_000#;
    43.
    44.    go_pY    : constant plotter.command := 2#001_000#;
    45.    go_nY    : constant plotter.command := 2#000_100#;
    46.    go_pX    : constant plotter.command := 2#000_010#;
    47.    go_nX    : constant plotter.command := 2#000_001#;
    48.
    49.    go_nXnY  : constant plotter.command := go_nX + go_nY;
    50.    go_pXnY  : constant plotter.command := go_pX + go_nY;
    51.    go_nXpY  : constant plotter.command := go_nX + go_pY;
    52.    go_pXpY  : constant plotter.command := go_pX + go_pY;
    53.
    54.    -- These encodings are consistent with the Calcomp plotter command codes used here:
    55.    --     ub.fnwi.uva.nl/computermuseum//calcomp565.html
    56.    -- which defines a full set of 11 commands, two of which are missing from the KDF9 list.
    57.
    58.    is_valid : constant array (plotter.command) of Boolean
    59.             := (dummy    |
    60.                 pen_up   |
    61.                 pen_down |
    62.                 go_pY    |
    63.                 go_nY    |
    64.                 go_pX    |
    65.                 go_nX    |
    66.                 go_pXnY  |
    67.                 go_nXpY  |
    68.                 go_pXpY  |
    69.                 go_nXnY  => True,
    70.                 others   => False
    71.                );
    72.
    73.    procedure perform (action : in plotter.command; stream : in out host_IO.stream);
    74.
    75. end plotter;

 223 lines: No errors
