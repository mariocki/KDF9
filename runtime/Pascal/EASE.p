%checks = no

program EASTER;

   type
      natural = 0 .. maxint;

   var
      Easter_day, Easter_month, data_year : natural;

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
      end;
   end {show_a_range_of_dates};

begin
show_a_range_of_dates (1960, 2960);
end .
