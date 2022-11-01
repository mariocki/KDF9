program sett;
type
   address = integer;
   posrange = 1..5;
   charrange = ' ' .. ':';
   bigrange = 0..65;
   colour = (red, yellow, green, blue, tartan);
var
   colourset : set of colour;
   emptyset, undefinedset, posset : set of posrange;
   charrangeset : set of charrange;
   bigset : set of bigrange;
   big : 0..63;
   hue : colour;
   ch  : charrange;
   pos : posrange;

begin (* test4 *)
for pos := 1 to 5 do
   if odd(pos) then
      begin
      posset := posset + [ pos ];
      end;
for ch := '|' downto '*' do
   if odd(ord(ch)) then
      begin
      charrangeset := charrangeset + [ ch ];
      end;
for big := 0 to 63 do
   if big mod 3 = 0 then
      begin
      bigset := bigset +[ big ];
      end;
for hue := red to tartan do
   if not odd(ord(hue)) then
      begin
      colourset := colourset + [ hue ];
      end;
emptyset := [];
end (* test4 *) .;
