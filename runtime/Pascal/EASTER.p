%checks = no
program EASTER;

type
   natural = 0 .. maxint;

var
   Easter_day, Easter_month, data_year : natural;

%listing off
%include PASKAL_char_IO
%include PASKAL_int_IO
%include PASKAL_timing
%listing on

procedure Tom_O_Beirne (var Easter_day, Easter_month : natural; given_year : natural);
{ This algorithm, an improvement of a method of Gauss, is due to T.H. O'Beirne. }
{ See: "The New Scientist", Nr. 228, 30 March 1961, pp 828-829.                 }
   var
      a, b, c, d, e, g, h, i, j, k, l, m, n, p : natural;
      r, s, t : natural;
   begin {Tom_O_Beirne}
   a := given_year mod 19;
   b := given_year div 100;
   c := given_year mod 100;
   d := b div 4;
   e := b mod 4;
   g := 8*b + 13;
   r := (19*a + b - d - g div 25 + 15);
   h := r mod 30;
   i := c div 4;
   k := c mod 4;
   s := (2*e + 2*i - h - k + 32);
   l :=  s mod 7;
   m := (a + 11*h + 19*l) div 433;
   t := (h + l - 7*m + 90);
   n :=  t div 25;
   p := (h + l - 7*m + 33*n + 19) mod 32;
   Easter_day := p;
   Easter_month := n;
   end {Tom_O_Beirne};

procedure show_a_range_of_dates (first_year, final_year : natural);
   var
      this_year : integer;
   begin
   for this_year := first_year to final_year do
      begin
      Tom_O_Beirne(Easter_day, Easter_month, this_year);
      put_small_int(1, this_year);
      put_small_int(1, Easter_month);
      put_small_int(1, Easter_day);
      new_line(1, 1);
      end;
   end {show_a_range_of_dates};

begin
open(1, 3);

data_year := 2022;

put_alfa16(1, 'THESE ARE THE 21');
put_alfa16(1, ' YEARS OF KDF9  ');
new_line(1, 1);
show_a_range_of_dates (1960, 1980);
new_line(1, 2);

put_alfa16(1, 'THESE ARE THE YE');
put_alfa16(1, 'ARS OF EE9      ');
new_line(1, 1);
show_a_range_of_dates (2009, data_year);
new_line(1, 2);

put_alfa16(1, 'THESE ARE THE NE');
put_alfa16(1, 'XT 21 YEARS--MAK');
put_alfa16(1, 'ING OPTIMISTIC A');
put_alfa16(1, 'SSUMPTIONS      ');
new_line(1, 1);
show_a_range_of_dates (data_year + 1, data_year + 21);
end .