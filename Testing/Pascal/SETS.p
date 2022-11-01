%checks = yes
program SETS;
type
   word_set_type = set of 0..47;

   pair_set_type = set of 0..95;

   char_set_type = set of char;

   decimal = '0'..'9';
   digit_set_type = set of decimal;
   digit_set_type2 = set of '0'..'9';

   choices = (yes, no, maybe);
   choice_set_type = set of choices;
   no_yes_set_type = set of no..maybe;

var
  digit : decimal;
  i : 0..47;
  j : 0..95;
  char_set_v, char_set_w, char_set_x : char_set_type;
  digit_set_v, digit_set_w, digit_set_x : digit_set_type;
  choice_set_v, choice_set_w, choice_set_x : choice_set_type;
  word_set_v, word_set_w, word_set_x : word_set_type;
  pair_set_v, pair_set_w, pair_set_x : pair_set_type;
  choice_bool, pair_bool, word_bool : Boolean;

begin
digit_set_v := ['1'..'9'];

word_set_v := [1..45];
word_set_v := [0..47];
word_set_v := [0..47];
word_set_v := [1..i];
word_set_v := [i..23];
word_set_v := [i..i];


pair_set_v := [1..45];
pair_set_v := [0..95];
pair_set_v := [1..j];
pair_set_v := [j..23];
pair_set_v := [j..j];

word_set_v := word_set_w * [32..43];
pair_set_v := pair_set_w * [42..53];

choice_bool := yes   in choice_set_v;
choice_bool := no    in choice_set_v;
choice_bool := maybe in choice_set_v;

word_bool := 0  in word_set_v;
word_bool := 1  in word_set_v;

pair_bool := 0  in pair_set_v;
pair_bool := 1  in pair_set_v;
pair_bool := 95 in pair_set_v;

word_bool := digit in digit_set_v;
word_bool := digit in digit_set_v;
word_bool := succ(digit) in digit_set_v;
{word_bool := 95 in word_set_v; fails as it should}

digit := digit;
digit := '5';
digit_set_v := [];
digit_set_v := ['1'];
digit_set_v := [digit];

word_set_v := [];
word_set_v := [47];
word_set_v := word_set_w + [0];
word_set_v := word_set_w * ([42] + [44] + [45]);

pair_set_v := [];
pair_set_x := [1];
pair_set_x := [47];
pair_set_x := [48];
pair_set_x := [95];
pair_set_x := [96];

pair_set_v := pair_set_w + [0];
pair_set_v := pair_set_w - [95];

pair_set_v := pair_set_w - pair_set_x;
pair_set_v := pair_set_w + pair_set_x;
pair_set_v := pair_set_w * pair_set_x;

pair_set_v := pair_set_v - (pair_set_w * pair_set_x);
pair_set_v := pair_set_v - (pair_set_w - pair_set_x);
pair_set_v := pair_set_v + (pair_set_w - pair_set_x);
pair_set_v := pair_set_v * (pair_set_w + pair_set_x);

pair_set_v := (pair_set_w * pair_set_x) - pair_set_v;
pair_set_v := (pair_set_w - pair_set_x) + pair_set_v;
pair_set_v := (pair_set_w - pair_set_x) - pair_set_v;

choice_set_v := [yes] * choice_set_x;
choice_set_v := [no] - choice_set_x;
choice_set_v := [maybe] + choice_set_x;

choice_set_v := choice_set_x * [yes];
choice_set_v := choice_set_x - [no];
choice_set_v := choice_set_x + [maybe];

digit_set_v := digit_set_w - digit_set_x;
digit_set_v := digit_set_w + digit_set_x;
digit_set_v := digit_set_w * digit_set_x;

char_set_v := char_set_w - char_set_x;
char_set_v := char_set_w + char_set_x;
char_set_v := char_set_w * char_set_x;

choice_set_v := choice_set_w - choice_set_x;
choice_set_v := choice_set_w + choice_set_x;
choice_set_v := choice_set_w * choice_set_x;


choice_set_v := choice_set_v - (choice_set_w * choice_set_x);
choice_set_v := choice_set_v - (choice_set_w - choice_set_x);
choice_set_v := choice_set_v + (choice_set_w - choice_set_x);
choice_set_v := choice_set_v * (choice_set_w + choice_set_x);

choice_set_v := (choice_set_w * choice_set_x) - choice_set_v;
choice_set_v := (choice_set_w - choice_set_x) + choice_set_v;
choice_set_v := (choice_set_w - choice_set_x) - choice_set_v;

word_set_v := word_set_w - word_set_x;
word_set_v := word_set_w + word_set_x;
word_set_v := word_set_w * word_set_x;

end.
