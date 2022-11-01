%checks = yes
program wirth_10_11;
{ procedures and functions; dynamic variables and pointers }
type
   ref  = ^node;
   node = record l, r : ref end;
var
   global_a    : integer;
   p, q : ref;

   procedure p0;
   begin end;                    { 2 ins overhead: leaf with no params or locals}

   procedure p1 (x : integer);
   begin end;                    { 4 ins overhead: leaf with params but no locals}

   procedure p2 (x, y : integer);
      var
         nonlocal_b : integer;

      procedure p4 (x, y, z : integer);
         var
            local_c : integer;
         begin {p4}
         global_a := local_c;    { 2 ins}
         end; {p4}               { 4 ins overhead: leaf with params and locals}

      procedure p3 (x, y, z : integer);
         var
            local_c : integer;
         begin {p3}
         global_a := nonlocal_b; { 2 ins}
         global_a := local_c;    { 2 ins}
         end; {p3}               { 8 ins overhead: leaf with params, locals and nonlocals}

      begin {p2}
      global_a := nonlocal_b;       { 2 ins}
      p3(global_a, nonlocal_b, x);  { 7 ins, 19 obeyed}
      p4(global_a, nonlocal_b, x);  { 7 ins, 13 obeyed}
      end; {p2}                     {20 ins overhead: nonleaf with params, locals and nonlocals}

   procedure s2 (x, y : integer);
      var
         s2_local_b : integer;

      procedure s3 (x, y, z : integer);
         var
            local_c : integer;
         begin {s3}
         global_a := x;          { 2 ins}
         global_a := local_c;    { 2 ins}
         end; {s3}               { 4 ins overhead: leaf with params and locals}

      begin {s2}
      global_a := s2_local_b;       { 2 ins}
      s3(global_a, s2_local_b, x);  { 7 ins, 15 obeyed}
      end; {s2}                     {16 ins overhead: nonleaf with params and locals}

%checks off
   function f (x : integer) : integer;
      begin
      f := 1                     { 4 ins}
      end;                       { 5 ins overhead}

%checks on
   function g (x : integer) : integer;
      begin
      g := 1                     { 4 ins}
      end;                       {11 ins overhead, inc. checking subroutine}
%checks off

   procedure q1 (var x : integer);
      begin
      x := 1;                    { 6 ins}
      global_a := x;             { 4 ins}
      end; {q1}                  { 4 ins overhead: as p1}

   procedure q2 (procedure r);
      begin
      r                          { NYI }
      end;

   function a (m, n : integer) : integer;
      begin
      if m = 0 then a := n+1        { 6 ins}
      else                          { 1 ins}
      if n = 0 then a := a(m-1, 1)  {14 ins}
      else                          { 1 ins}
         a := a(m-1, a(m, n-1));    {18 ins}
      end;                          {14 ins overhead; 21 or 32 obeyed, average 26.5}

begin

{T10: procedures and functions}
p0;                              { 1 ins,  3 obeyed}
p1(global_a);                    { 3 ins,  7 obeyed}

p2(global_a, global_a);          { 5 ins, 25 obeyed, excl. those in p2 body}
s2(global_a, global_a);          { 5 ins, 21 obeyed, excl. those in s2 body}

q1(global_a);                    { 4 ins, 18 obeyed}

q2(p0);                          { NYI }

global_a := f(global_a);         { 4 ins, 13 obeyed}
global_a := g(global_a);         { 4 ins, 19 obeyed, incl. subroutine check on fn result}
global_a := a (3, 7);            { 6 ins, 27 or 38 obeyed, average 32.5}

{T11: dynamic variables and pointers}
new(p);                          { NYI }
q := nil;                        { 3 ins}
q := p;                          { 2 ins}
q := p^.l;                       { NYI }
q := p^.r^.l;                    { NYI }
q := p^.l^.r^.l;                 { NYI }
end.
