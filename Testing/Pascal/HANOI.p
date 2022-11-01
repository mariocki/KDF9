program HANOI;

var disc_count : integer;

%listing off
%include PASKAL_char_IO
%listing on

procedure disc (d : integer);
   begin
   if d = 1 then put_char(1, 'A') else
   if d = 2 then put_char(1, 'B') else
   if d = 3 then put_char(1, 'C');
   end {disc};

procedure print_move (from, next  : integer);
   begin
    put_alfa16(1, 'Move a disc from');
    space(1, 3);
    disc(from);
    put_alfa8(1, '   to   ');
    disc(next);
    new_line(1, 1);
   end {print_move};

procedure solve (number,  source,  target,  spare : integer);
   begin
   if number > 1 then
      begin
      solve(number-1, source, spare, target);
      print_move (source, target);
      solve(number-1,  spare,  target,  source);
      end
   else
      print_move (source, target);
   end {solve};


begin
open(1, 3);

disc_count := 5;

put_alfa16(1, 'Towers of Hanoi.');
new_line(1, 1);
put_alfa16(1, 'Move 5 discs fro');
put_alfa16(1, 'm tower a to tow');
put_alfa16(1, 'er c via tower b');
new_line(1, 1);
solve(disc_count, 1, 3, 2);
end .
