// Author David Holdsworth <ecldh@leeds.ac.uk>
// yacc grammar for KDF9 Usercode
// The most recent version of this program is on-line at:
//        http://sw.ccs.bcs.org/KDF9/kal3.y
// KAL3 is an attempt to accept KDF9 Usercode both the original
// paper tape version and EGDON's UCA3 which was normally input on cards.
// Listings copy-typed from lineprinter POST listings should also be
// OK, except that pound must be replaced by dollar (or asterisk), and *DIV
// will be seen as a separate asterisk.  DIV is valid UCA3 for integer divide.
%{
#include <ctype.h>
/* kal3.y KDF9 assembler for USERCODE */
#include <stdio.h>

#define MAXP  4000
         // this is needed for fudging L77
extern int yylex();
extern void yyerror(char *s);
extern int tree;

extern void endBblock();
/* called at end of B-block to create first 8 words of store
   and set pointers for main code generation
 */
extern void reserveStore(int p1, int p2);
/* reserve store of type p1 for p2 words */

extern void Bblock(int p1, int p2);
/* accept B-block data, either St nnn;  or TL nnn; */

extern void storesyl(int p1);
/* store single syllable instruction p1 */

extern void store2syl(int p1);
/* store 2-syllable instruction p1 */

extern void qnum(int q);
/* confirms that q is a valid Q-store number */

extern int twosylqx(int instr, int q1, int q2);
/* evaluate two syllable Q-store instruction with 2 Q-store numbers in it or other lower byte */

extern int twosylshetc(int i0, int n);
/* evaluate two syllable Q-store instruction with 1 Q-store number in it */

extern void storejump(int s1, int s2, int loc);
/* store a jump to specified location */

extern void setlabel(int labno);
/* record integer label */

extern int codeloc(int pno, int labno);
/* evaluate location in the code -- result is syllable address */

extern int wordform(int loc);
/* convert a syllable address to word address in SJNS format */

extern void store3syl(int instr, int addr);
/* takes the output from dataloc and embeds in a SET or EXIT instruction */

extern void storememinstr(int instr, int addr, int mod);
/* takes the output from data loc and embeds in a emmory accessing instruction */

extern int dataloc(int pno, int vno);
/* get data location as word address for a V-store */

extern int ystoreloc(int yset, int yno);
/* get ystore location
   yset will be the letter for YA YB etc
 */

extern void newproutine(int pno, int v);
/* entering a new P-routine */

extern int keepMarker();
/* uses srcmarker to deliver a pointer
   to the first char of a sequence of digits
 */

extern int octalval(int p);
/* delivers a 16-bit octal value from the pointer p
 */

extern void vfraction(int p, int sig);
/* evalues integer or fraction with sig integral places */

extern void vfloat(int p, int exp);
/* evaluates floating point V-store value */

extern void vfppos();
/* stores positive floating point V-store value */

extern void vfpneg();
/* stores negative floating point V-store value */

extern void voctal(int p, int sig);
/* ditto octal */

extern void vqformat(int p1, int p2, int p3);
/* constructs a Q-store format V-store value */

extern int negatevstore();
/* negates the vstore value deposited by vfraction, etc */
/* returns 0 if V-store vale was zero -- used for F-0.0 */

void setvstore(int vno, int lo, int hi);
/* sets give V-store from value constructed in vstoreval */
/* for a full word lo = 0 and hi = 6, for top halp lo = 0 and hi = 3, for bottom half lo = 3 and hi = 6 */

extern void setprinterconst(int v1, int v2);
/* sets a printer constant in a range of V-stores -- involves lexical cheating */

extern void startnewword();
/* inserts dummy instructions up to word boundary */

extern void fixY0(int zero, int zero2, int loc);
/* locates Y0 at specified absolute address, e.g. Y0=E 4896 */

extern int progstart;     // location at which code is to commence -- reset by START directive
extern int ptform;        // set to 8 if using the rounding rules found in the PT Usercode compiler

%}

%token UNSIGNED_INTEGER
%token COMMENT

%%
/* comment !!! indicates incomplete implementation */

program       :   p0head  code          { printf("Reached end of file\n"); }
              ;

p0head        :   'R' 'E' 'S' 'T' 'A' 'R' 'T' ';' code 'P' 'R' 'O' 'G' 'R' 'A' 'M' ';'          { endBblock(); }
              |   'R' 'E' 'S' 'T' 'A' 'R' 'T' ';' code 'P' 'R' 'O' 'G' 'R' 'A' 'M' 'M' 'E' ';'  { endBblock(); }
              |   'P' 'R' 'O' 'G' 'R' 'A' 'M' ';'                                               { endBblock(); }
              |   'P' 'R' 'O' 'G' 'R' 'A' 'M' 'M' 'E' ';'                                       { endBblock(); }
              |   storespec  p0head
              ;

storespec     :   'V' UNSIGNED_INTEGER ';'           { reserveStore('V', $2); }
              |   'V' ';'                          /* believe that this specifies the absence of any V-stores in P0 */
              |   'W' UNSIGNED_INTEGER ';'           { reserveStore('W', $2); }
              |   'W' ';'                          /* believe that this specifies the absence of any W-stores in P0 */
              |   ystref UNSIGNED_INTEGER ';'        { reserveStore('Y' + $1 * 256, $2); }
              |   ystref ';'                                         /* ignore -- default is no storage reserved */
              |   ystref UNSIGNED_INTEGER '=' 'E' UNSIGNED_INTEGER ';'
                                                     { fixY0($1, $2, $5); }
              |   'H' UNSIGNED_INTEGER '=' 'E' UNSIGNED_INTEGER ';'
                                                     { fixY0(-3, $2, $5); }

              |   ystref UNSIGNED_INTEGER '=' 'E' ';'{ fprintf(stderr,"Y0=E; not understood\n"); }

              |   'S' 'T' 'A' 'R' 'T' UNSIGNED_INTEGER ';'
                                                     { progstart = $6 + ptform; }
              |   'S' 'T' ';'                      /* no need to do anything - default is 0 */
              |   'T' 'L' ';'                      /* no need to do anything - default is 0 */
              |   'M' 'T' 'P' 'R' ';'              /* no ides what this meant - do nothing */
              |   'S' 'T' UNSIGNED_INTEGER ';'       { Bblock('S', $3); }
              |   'T' 'L' UNSIGNED_INTEGER ';'       { Bblock('L', $3); }
              |   COMMENT
              ;

integer       : UNSIGNED_INTEGER
              | '-' UNSIGNED_INTEGER              { $$ = -$2; }
              ;

codeloc       : UNSIGNED_INTEGER                  { $$ = codeloc(-1, $1); }   /* -1 indicates current routine */
              | 'P' UNSIGNED_INTEGER              { $$ = codeloc($2, 0); }
              | UNSIGNED_INTEGER 'P' UNSIGNED_INTEGER { $$ = codeloc($3, $1); }
              | 'E' UNSIGNED_INTEGER              { $$ = $2 * 6; }
              | 'L' UNSIGNED_INTEGER              { $$ = codeloc(MAXP-1, 0); printf("L%d not available\n", $2); }
              ;

jumpinstr     :  'J' codeloc ';'                  { storejump(0200, 0260, $2); }
              |  'J' 'S' codeloc ';'              { storejump(0200, 0320, $3); }
              |  'J' codeloc '=' ';'              { storejump(0220, 0020, $2); }
              |  'J' codeloc '!' '=' ';'          { storejump(0200, 0020, $2); }
              |  'J' codeloc '/' '=' ';'          { storejump(0200, 0020, $2); }
              |  'J' codeloc '#' ';'              { storejump(0200, 0020, $2); }
              |  'J' codeloc '±' ';'              { storejump(0200, 0020, $2); }
              |  'J' codeloc 'N' 'E' ';'          { storejump(0200, 0020, $2); }
              |  'J' codeloc '*' 'N' 'E' ';'      { storejump(0200, 0020, $2); }
              |  'J' codeloc '<' 'Z' ';'          { storejump(0220, 0040, $2); }
              |  'J' codeloc 'L' 'T' 'Z' ';'      { storejump(0220, 0040, $2); }
              |  'J' codeloc '*' 'L' 'T' 'Z' ';'  { storejump(0220, 0040, $2); }
              |  'J' codeloc '>' '=' 'Z' ';'      { storejump(0200, 0040, $2); }
              |  'J' codeloc 'G' 'E' 'Z' ';'      { storejump(0200, 0040, $2); }
              |  'J' codeloc '_' '>' 'Z' ';'      { storejump(0200, 0040, $2); }   /* for paper tape input */
              |  'J' codeloc '*' 'G' 'E' 'Z' ';'  { storejump(0200, 0040, $2); }
              |  'J' codeloc '>' 'Z' ';'          { storejump(0220, 0100, $2); }
              |  'J' codeloc '*' 'G' 'T' 'Z' ';'  { storejump(0220, 0100, $2); }
              |  'J' codeloc 'G' 'T' 'Z' ';'      { storejump(0220, 0100, $2); }
              |  'J' codeloc '<' '=' 'Z' ';'      { storejump(0200, 0100, $2); }
              |  'J' codeloc '*' 'L' 'E' 'Z' ';'  { storejump(0200, 0100, $2); }
              |  'J' codeloc 'L' 'E' 'Z' ';'      { storejump(0200, 0100, $2); }
              |  'J' codeloc '_' '<' 'Z' ';'      { storejump(0200, 0100, $2); }   /* for paper tape input */
              |  'J' codeloc '=' 'Z' ';'          { storejump(0220, 0140, $2); }
              |  'J' codeloc '!' '=' 'Z' ';'      { storejump(0200, 0140, $2); }
              |  'J' codeloc '/' '=' 'Z' ';'      { storejump(0200, 0140, $2); }
              |  'J' codeloc '#' 'Z' ';'          { storejump(0200, 0140, $2); }
              |  'J' codeloc '±' 'Z' ';'          { storejump(0200, 0140, $2); }
              |  'J' codeloc '*' 'N' 'E' 'Z' ';'  { storejump(0200, 0140, $2); }
              |  'J' codeloc 'N' 'E' 'Z' ';'      { storejump(0200, 0140, $2); }
              |  'J' codeloc 'V' ';'              { storejump(0220, 0200, $2); }
              |  'J' codeloc 'N' 'V' ';'          { storejump(0200, 0200, $2); }
              |  'J' codeloc 'E' 'N' ';'          { storejump(0220, 0240, $2); }
              |  'J' codeloc 'N' 'E' 'N' ';'      { storejump(0200, 0240, $2); }
              |  'J' codeloc 'E' 'J' ';'          { storejump(0220, 0300, $2); }
              |  'J' codeloc 'N' 'E' 'J' ';'      { storejump(0200, 0300, $2); }
              |  'J' codeloc 'T' 'R' ';'          { storejump(0220, 0340, $2); }
              |  'J' codeloc 'N' 'T' 'R' ';'      { storejump(0200, 0340, $2); }
              |  'J' codeloc 'C' UNSIGNED_INTEGER 'Z' ';'  { qnum($4); storejump(0240, $4<<4, $2); }
              |  'J' codeloc 'C' UNSIGNED_INTEGER 'N' 'Z' ';' { qnum($4); storejump(0260, $4<<4, $2); }
              |  'J' codeloc 'C' UNSIGNED_INTEGER 'N' 'Z' 'S'   { store2syl(twosylqx(0177, $4, 0)); }
              ;

vchars        :  'V' UNSIGNED_INTEGER '='                        { setprinterconst($2, $2); }
              |  'V' UNSIGNED_INTEGER '/' UNSIGNED_INTEGER '='   { setprinterconst($2, $4); }
              ;

vspec         :  'V' UNSIGNED_INTEGER '=' vstoreval ';'           { setvstore($2, 0, 6); }
              |  'V' UNSIGNED_INTEGER 'U' '=' vstoreval ';'       { setvstore($2, 0, 3); }
              |  'V' UNSIGNED_INTEGER 'L' '=' vstoreval ';'       { setvstore($2, 3, 6); }
              |  vchars 'P' ';'                              /* V-store printer characters */
              ;

digitsequence :  UNSIGNED_INTEGER     { $$ = keepMarker(); }      /* result is a pointer to the start of the digit string */
              ;

bit16         :  integer
              |  '+' integer           { $$ = $2; }
              |  'A' memref            { $$ = $2; }
              |  'B' digitsequence     { $$ = octalval($2); }
              ;

vstoreval     :  digitsequence                             { vfraction($1, 47); }
              |  'B' digitsequence                         { voctal($2, 47); }
              |  'B' digitsequence '/' UNSIGNED_INTEGER    { voctal($2, $4); }
              |  digitsequence '.' UNSIGNED_INTEGER        { vfraction($1, 47); }
              |  digitsequence '.' UNSIGNED_INTEGER '/' UNSIGNED_INTEGER  { vfraction($1, $5); }
              |  digitsequence '/' UNSIGNED_INTEGER        { vfraction($1, $3); }
              |  '-' digitsequence                         { vfraction($2, 47); negatevstore(); }
              |  '-' digitsequence '.' UNSIGNED_INTEGER    { vfraction($2, 47); negatevstore(); }
              |  '-' digitsequence '.' UNSIGNED_INTEGER '/' UNSIGNED_INTEGER  { vfraction($2, $6); negatevstore(); }
              |  '-' digitsequence '/' UNSIGNED_INTEGER    { vfraction($2, $4); negatevstore(); }
              |  '+' digitsequence                         { vfraction($2, 47); }
              |  '+' digitsequence '.' UNSIGNED_INTEGER    { vfraction($2, 47); }
              |  '+' digitsequence '.' UNSIGNED_INTEGER '/' UNSIGNED_INTEGER  { vfraction($2, $6); }
              |  '+' digitsequence '/' UNSIGNED_INTEGER    { vfraction($2, $4); }
              |  'Q' bit16 '/' bit16 '/' bit16             { vqformat($2, $4, $6); }
              |  'A' memref                                 { vqformat(0, 0, $2); }
              |  'F' digitsequence '.' UNSIGNED_INTEGER     { vfloat($2, 0); vfppos(); }
              |  'F' digitsequence                          { vfloat($2, 0); vfppos(); }
              |  'F' digitsequence '.' UNSIGNED_INTEGER '~' UNSIGNED_INTEGER   { vfloat($2, $6); vfppos(); }
              |  'F' digitsequence '.' UNSIGNED_INTEGER '~' '-' UNSIGNED_INTEGER   { vfloat($2, -$7); vfppos(); }
              |  'F' digitsequence '.' UNSIGNED_INTEGER '~' '+' UNSIGNED_INTEGER   { vfloat($2, $7); vfppos(); }
              |  'F' digitsequence '~' UNSIGNED_INTEGER     { vfloat($2, $4); vfppos(); }
              |  'F' digitsequence '~' '-' UNSIGNED_INTEGER     { vfloat($2, -$5); vfppos(); }
              |  'F' '-' digitsequence '.' UNSIGNED_INTEGER { vfloat($3, 0); vfpneg(); }
              |  'F' '-' digitsequence                      { vfloat($3, 0); vfpneg(); }
              |  'F' '-' digitsequence '.' UNSIGNED_INTEGER '~' UNSIGNED_INTEGER   { vfloat($3, $7); vfpneg(); }
              |  'F' '-' digitsequence '.' UNSIGNED_INTEGER '~' '-' UNSIGNED_INTEGER   { vfloat($3, -$8); vfpneg(); }
              |  'F' '-' digitsequence '.' UNSIGNED_INTEGER '~' '+' UNSIGNED_INTEGER   { vfloat($3, $8); vfpneg(); }
              |  'F' '-' digitsequence '~' UNSIGNED_INTEGER { vfloat($3, $5); vfpneg(); }
              |  'F' '-' digitsequence '~' '-' UNSIGNED_INTEGER { vfloat($3, -$6); vfpneg(); }
              |  'F' '+' digitsequence '.' UNSIGNED_INTEGER { vfloat($3, 0); vfppos(); }
              |  'F' '+' digitsequence                      { vfloat($3, 0); vfppos(); }
              |  'F' '+' digitsequence '.' UNSIGNED_INTEGER '~' UNSIGNED_INTEGER   { vfloat($3, $7); vfppos(); }
              |  'F' '+' digitsequence '.' UNSIGNED_INTEGER '~' '-' UNSIGNED_INTEGER   { vfloat($3, -$8); vfppos(); }
              |  'F' '+' digitsequence '.' UNSIGNED_INTEGER '~' '+' UNSIGNED_INTEGER   { vfloat($3, $8); vfppos(); }
              |  'F' '+' digitsequence '~' UNSIGNED_INTEGER { vfloat($3, $5); vfppos(); }
              |  'F' '+' digitsequence '~' '-' UNSIGNED_INTEGER { vfloat($3, -$6); vfppos(); }

              |  '+' digitsequence '.' UNSIGNED_INTEGER '/' '-' UNSIGNED_INTEGER  { vfraction($2, -$7); }
              ;

code          :  code  instruction
              |  instruction 
              ;

ystref        :  'Y'        {  $$ = 0; }
              |  'Y' 'A'    {  $$ = 1; }
              |  'Y' 'B'    {  $$ = 2; }
              |  'Y' 'C'    {  $$ = 3; }
              |  'Y' 'D'    {  $$ = 4; }
              |  'Y' 'E'    {  $$ = 5; }
              |  'Y' 'F'    {  $$ = 6; }
              |  'Y' 'G'    {  $$ = 7; }
              |  'Y' 'H'    {  $$ = 8; }
              |  'Y' 'I'    {  $$ = 9; }
              |  'Y' 'J'    {  $$ = 10; }
              |  'Y' 'K'    {  $$ = 11; }
              |  'Y' 'L'    {  $$ = 12; }
              |  'Y' 'M'    {  $$ = 13; }
              |  'Y' 'N'    {  $$ = 14; }
              |  'Y' 'O'    {  $$ = 15; }
              |  'Y' 'P'    {  $$ = 16; }
              |  'Y' 'Q'    {  $$ = 17; }
              |  'Y' 'R'    {  $$ = 18; }
              |  'Y' 'S'    {  $$ = 19; }
              |  'Y' 'T'    {  $$ = 20; }
              |  'Y' 'U'    {  $$ = 21; }
              |  'Y' 'V'    {  $$ = 22; }
              |  'Y' 'W'    {  $$ = 23; }
              |  'Y' 'X'    {  $$ = 24; }
              |  'Y' 'Y'    {  $$ = 25; }
              |  'Y' 'Z'    {  $$ = 26; }
              ;

memref        :  'V' integer                        {  $$ = dataloc(-1, $2); }
              |  'V' integer 'U'                    {  $$ = dataloc(-1, $2) << 1; }
              |  'V' integer 'L'                    {  $$ = (dataloc(-1, $2) << 1) | 1; }
              |  'V' integer 'P' UNSIGNED_INTEGER   {  $$ = dataloc($4, $2); }
              |  'R' integer                        {  $$ = wordform(codeloc(-1, $2)); }
              |  'R' integer 'P' UNSIGNED_INTEGER   {  $$ = wordform(codeloc($4, $2)); }
              |  'P' UNSIGNED_INTEGER               {  $$ = codeloc($2, 0)/6; }      /* P-routine always starts on word boundary */
              |  'E' integer                        {  $$ = $2; }
              |  ystref integer                     {  $$ = ystoreloc($1, $2); }
              |  'W' integer                        {  $$ = ystoreloc(-1, $2); }     /* W stores indicated as -1 */
              |  'Z' integer                        {  $$ = ystoreloc(-2, $2); }     /* Z stores indicated as -2 */
              |  'H' integer                        {  $$ = ystoreloc(-3, $2); }     /* H stores are a mystery */
              ;

mqmq          :  'M' UNSIGNED_INTEGER 'M' UNSIGNED_INTEGER ';'              {$$ = twosylqx(0100, $4, $2); }
              |  'M' UNSIGNED_INTEGER 'M' UNSIGNED_INTEGER 'Q' ';'          {$$ = twosylqx(0102, $4, $2); }
              |  'M' UNSIGNED_INTEGER 'M' UNSIGNED_INTEGER 'N' ';'          {$$ = twosylqx(0110, $4, $2); }
              |  'M' UNSIGNED_INTEGER 'M' UNSIGNED_INTEGER 'Q' 'N' ';'      {$$ = twosylqx(0112, $4, $2); }
              |  'M' UNSIGNED_INTEGER 'M' UNSIGNED_INTEGER 'H' ';'          {$$ = twosylqx(0104, $4, $2); }
              |  'M' UNSIGNED_INTEGER 'M' UNSIGNED_INTEGER 'Q' 'H' ';'      {$$ = twosylqx(0106, $4, $2); }
              |  'M' UNSIGNED_INTEGER 'M' UNSIGNED_INTEGER 'H' 'N' ';'      {$$ = twosylqx(0114, $4, $2); }
              |  'M' UNSIGNED_INTEGER 'M' UNSIGNED_INTEGER 'Q' 'H' 'N' ';'  {$$ = twosylqx(0116, $4, $2); }
              ;

shiftval      :  '+' UNSIGNED_INTEGER    {  $$ = $2; }
              |  '-' UNSIGNED_INTEGER    {  $$ = (-$2) & 0177; }
              |  UNSIGNED_INTEGER
              ;

instruction   :  ';'
              |  COMMENT
              |  UNSIGNED_INTEGER ';'      { setlabel($1); }
              |  vspec
              |  'P' UNSIGNED_INTEGER ';'  { newproutine($2, -1); }
              |  'P' UNSIGNED_INTEGER 'V' UNSIGNED_INTEGER ';'  { newproutine($2, $4); }
              |  'L' UNSIGNED_INTEGER ';'  { newproutine(-$2, -1); }

              |  memref ';'     { storememinstr(0300, $1, 0); }                  /* Vn;  VnPn; Yn; YAn;  etc */
              |  '=' memref ';'     { storememinstr(0301, $2, 0); }

              |  memref 'M' UNSIGNED_INTEGER ';'         { qnum($3); storememinstr(0300, $1, $3); }    /* VnMm;  etc */
              |  '=' memref 'M' UNSIGNED_INTEGER ';'     { qnum($4); storememinstr(0301, $2, $4); }
              |  memref 'M' UNSIGNED_INTEGER 'Q' ';'     { qnum($3); storememinstr(0302, $1, $3); }    /* VnMmQ;  etc */
              |  '=' memref 'M' UNSIGNED_INTEGER 'Q' ';' { qnum($4); storememinstr(0303, $2, $4); }

              |  'S' 'E' 'T' UNSIGNED_INTEGER ';'         {  store3syl(0304, $4); }
              |  'S' 'E' 'T' 'B' digitsequence ';'        {  store3syl(0304, octalval($5)); }
              |  'S' 'E' 'T' '-' UNSIGNED_INTEGER ';'     {  store3syl(0304, (-$5)&0177777); }
              |  'S' 'E' 'T' '+' UNSIGNED_INTEGER ';'     {  store3syl(0304, $5); }
              |  'S' 'E' 'T' 'A' memref ';'               {  store3syl(0304, $5); }
              |  'S' 'E' 'T' 'A' memref 'U' ';'           {  store3syl(0304, $5<<1); }
              |  'S' 'E' 'T' 'A' memref 'L' ';'           {  store3syl(0304, ($5<<1) + 1); }
              |  'S' 'E' 'T' 'A' 'P' UNSIGNED_INTEGER ';' {  store3syl(0304, codeloc($6, 0)/6); }

              |  mqmq                 {  store2syl($1); }                /* MnMn;    MnMnQ;   etc */
              |  '=' mqmq                {  store2syl(256 + $2); }

              |  'M' UNSIGNED_INTEGER 'T' 'O' 'Q' UNSIGNED_INTEGER ';'      { store2syl(twosylqx(0151, $2, $6)); }
              |  'I' UNSIGNED_INTEGER 'T' 'O' 'Q' UNSIGNED_INTEGER ';'      { store2syl(twosylqx(0152, $2, $6)); }
              |  'I' 'M' UNSIGNED_INTEGER 'T' 'O' 'Q' UNSIGNED_INTEGER ';'  { store2syl(twosylqx(0153, $3, $7)); }
              |  'C' UNSIGNED_INTEGER 'T' 'O' 'Q' UNSIGNED_INTEGER ';'      { store2syl(twosylqx(0154, $2, $6)); }
              |  'C' 'M' UNSIGNED_INTEGER 'T' 'O' 'Q' UNSIGNED_INTEGER ';'  { store2syl(twosylqx(0155, $3, $7)); }
              |  'C' 'I' UNSIGNED_INTEGER 'T' 'O' 'Q' UNSIGNED_INTEGER ';'  { store2syl(twosylqx(0156, $3, $7)); }
              |  'Q' UNSIGNED_INTEGER 'T' 'O' 'Q' UNSIGNED_INTEGER ';'      { store2syl(twosylqx(0157, $2, $6)); }
              |  'V' 'R'  ';'  { storesyl(001); }
              |  '=' 'T' 'R'  ';'  { storesyl(002); }
              |  'B' 'I' 'T' 'S'  ';'  { storesyl(003); }
         /*   |  '*' 'F'  ';'  { storesyl(004); }   */
         /*   |  '*' 'D' 'F'  ';'  { storesyl(005); }   */
         /*   |  '*' '+' 'F'  ';'  { storesyl(007); }   */
         /*   |  '*' 'D'  ';'  { storesyl(034); }   */
         /*   |  '*'  ';'  { storesyl(035); }   */
         /*   |  '*' '+' shiftval ';'        { store2syl(twosylshetc(0163, 2*$3 + 1)); }   */
         /*   |  '*' '+' 'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0163, $4, 0)); }   */
              |  'x' 'F'  ';'  { storesyl(004); }
              |  'x' 'D' 'F'  ';'  { storesyl(005); }
              |  'x' '+' 'F'  ';'  { storesyl(007); }
              |  'x' 'D'  ';'  { storesyl(034); }
              |  'x'  ';'  { storesyl(035); }
              |  'x' '+' shiftval ';'        { store2syl(twosylshetc(0163, 2*$3 + 1)); }
              |  'x' '+' 'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0163, $4, 0)); }
              |  '×' 'F'  ';'  { storesyl(004); }
              |  '×' 'D' 'F'  ';'  { storesyl(005); }
              |  '×' '+' 'F'  ';'  { storesyl(007); }
              |  '×' 'D'  ';'  { storesyl(034); }
              |  '×'  ';'  { storesyl(035); }
              |  '×' '+' shiftval ';'        { store2syl(twosylshetc(0163, 2*$3 + 1)); }
              |  '×' '+' 'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0163, $4, 0)); }
              |  'X' 'F'  ';'  { storesyl(004); }
              |  'X' 'D' 'F'  ';'  { storesyl(005); }
              |  'X' '+' 'F'  ';'  { storesyl(007); }
              |  'X' 'D'  ';'  { storesyl(034); }
              |  'X'  ';'  { storesyl(035); }
              |  'X' '+' shiftval ';'        { store2syl(twosylshetc(0163, 2*$3 + 1)); }
              |  'X' '+' 'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0163, $4, 0)); }
              |  'N' 'E' 'G' 'D'  ';'  { storesyl(010); }
              |  'O' 'R'  ';'  { storesyl(011); }
              |  'P' 'E' 'R' 'M'  ';'  { storesyl(012); }
              |  'T' 'O' 'B'  ';'  { storesyl(013); }
              |  'R' 'O' 'U' 'N' 'D' 'H'  ';'  { storesyl(014); }
              |  'N' 'E' 'V'  ';'  { storesyl(015); }
              |  'R' 'O' 'U' 'N' 'D'  ';'  { storesyl(016); }
              |  'D' 'U' 'M' 'M' 'Y'  ';'  { storesyl(017); }
              |  'R' 'O' 'U' 'N' 'D' 'F'  ';'  { storesyl(020); }
              |  'R' 'O' 'U' 'N' 'D' 'H' 'F'  ';'  { storesyl(021); }
              |  '-' 'D' 'F'  ';'  { storesyl(022); }
              |  '+' 'D' 'F'  ';'  { storesyl(023); }
              |  'F' 'L' 'O' 'A' 'T'  ';'  { storesyl(024); }
              |  'F' 'L' 'O' 'A' 'T' 'D'  ';'  { storesyl(025); }
              |  'A' 'B' 'S'  ';'  { storesyl(026); }
              |  'N' 'E' 'G'  ';'  { storesyl(027); }
              |  'A' 'B' 'S' 'F'  ';'  { storesyl(030); }
              |  'N' 'E' 'G' 'F'  ';'  { storesyl(031); }
              |  'M' 'A' 'X'  ';'  { storesyl(032); }
              |  'N' 'O' 'T'  ';'  { storesyl(033); }
              |  '-'  ';'  { storesyl(036); }
              |  'S' 'I' 'G' 'N'  ';'  { storesyl(037); }
              |  'Z' 'E' 'R' 'O'  ';'  { storesyl(041); }
              |  'D' 'U' 'P'  ';'  { storesyl(042); }
              |  'D' 'U' 'P' 'D'  ';'  { storesyl(043); }
              |  'F' 'I' 'X'  ';'  { storesyl(045); }
              |  'S' 'T' 'R'  ';'  { storesyl(047); }
              |  'C' 'O' 'N' 'T'  ';'  { storesyl(050); }
              |  'R' 'E' 'V' 'D'  ';'  { storesyl(051); }
              |  'E' 'R' 'A' 'S' 'E'  ';'  { storesyl(052); }
              |  '-' 'D'  ';'  { storesyl(053); }
              |  'A' 'N' 'D'  ';'  { storesyl(054); }
              |  '+'  ';'  { storesyl(056); }
              |  '+' 'D'  ';'  { storesyl(057); }
              |  '÷' 'I'  ';'  { storesyl(044); }
              |  '÷'  ';'  { storesyl(060); }
              |  '÷' 'D'  ';'  { storesyl(061); }
              |  '÷' 'F'  ';'  { storesyl(062); }
              |  '÷' 'D' 'F'  ';'  { storesyl(063); }
              |  '÷' 'R'  ';'  { storesyl(064); }
              |  '%' 'I'  ';'  { storesyl(044); }
              |  '%'  ';'  { storesyl(060); }
              |  '%' 'D'  ';'  { storesyl(061); }
              |  '%' 'F'  ';'  { storesyl(062); }
              |  '%' 'D' 'F'  ';'  { storesyl(063); }
              |  '%' 'R'  ';'  { storesyl(064); }
              |  '/' 'I'  ';'  { storesyl(044); }
              |  '/'  ';'  { storesyl(060); }
              |  '/' 'D'  ';'  { storesyl(061); }
              |  '/' 'F'  ';'  { storesyl(062); }
              |  '/' 'D' 'F'  ';'  { storesyl(063); }
              |  '/' 'R'  ';'  { storesyl(064); }

              |  'D' 'I' 'V' 'I'  ';'  { storesyl(044); }
              |  'D' 'I' 'V'  ';'  { storesyl(060); }
              |  'D' 'I' 'V' 'D'  ';'  { storesyl(061); }
              |  'D' 'I' 'V' 'F'  ';'  { storesyl(062); }
              |  'D' 'I' 'V' 'D' 'F'  ';'  { storesyl(063); }
              |  'D' 'I' 'V' 'R'  ';'  { storesyl(064); }

              |  'R' 'E' 'V'  ';'  { storesyl(065); }
              |  'C' 'A' 'B'  ';'  { storesyl(066); }
              |  'F' 'R' 'B'  ';'  { storesyl(067); }
              |  'S' 'T' 'A' 'N' 'D'  ';'  { storesyl(070); }
              |  'N' 'E' 'G' 'D' 'F'  ';'  { storesyl(071); }
              |  'M' 'A' 'X' 'F'  ';'  { storesyl(072); }
              |  '+' 'F'  ';'  { storesyl(074); }
              |  '-' 'F'  ';'  { storesyl(075); }
              |  'S' 'I' 'G' 'N' 'F'  ';'  { storesyl(077); }

              |  'K' UNSIGNED_INTEGER  ';'   { store2syl(twosylshetc(0176, 128>>$2));  }
              |  '=' 'K' UNSIGNED_INTEGER  ';'   { store2syl(twosylshetc(0175, 128>>$3));  }

              |  'F' 'I' 'N' 'I' 'S' 'H' ';'   {  newproutine(-1, -1); }

              |  'M' '+' 'I' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0140, $4, 0)); }
              |  'M' '-' 'I' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0141, $4, 0)); }
              |  'N' 'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0142, $3, 0)); }
              |  'D' 'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0143, $3, 0)); }
              |  'I' UNSIGNED_INTEGER '=' '+' UNSIGNED_INTEGER ';'  { store2syl(twosylqx(0144 + (($5-1)&1)*2, $2, 0)); }
              |  'I' UNSIGNED_INTEGER '=' UNSIGNED_INTEGER ';'  { store2syl(twosylqx(0144 + (($4-1)&1)*2, $2, 0)); }
              |  'I' UNSIGNED_INTEGER '=' '-' UNSIGNED_INTEGER ';'  { store2syl(twosylqx(0145 + (($5-1)&1)*2, $2, 0)); }

              |  '=' 'M' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0170, $3, 2)); }
              |  '=' 'R' 'M' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0170, $4, 3)); }
              |  '=' 'I' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0170, $3, 4)); }
              |  '=' 'R' 'I' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0170, $4, 5)); }
              |  '=' 'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0170, $3, 010)); }
              |  '=' 'R' 'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0170, $4, 011)); }
              |  '=' 'Q' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0170, $3, 016)); }

              |  'M' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0171, $2, 2)); }
              |  'I' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0171, $2, 4)); }
              |  'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0171, $2, 010)); }
              |  'Q' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0171, $2, 016)); }

              |  '=' '+' 'M' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0172, $4, 2)); }
              |  '=' '+' 'I' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0172, $4, 4)); }
              |  '=' '+' 'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0172, $4, 010)); }
              |  '=' '+' 'Q' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0172, $4, 016)); }

              |  'L' 'I' 'N' 'K'  ';'            { store2syl(twosylshetc(0173, 0)); }
              |  '=' 'L' 'I' 'N' 'K' ';'         { store2syl(twosylshetc(0174, 0)); }

              |  'S' 'H' 'A' shiftval ';'        { store2syl(twosylshetc(0161, 2*$4 + 1)); }
              |  'S' 'H' 'A' 'D' shiftval ';'    { store2syl(twosylshetc(0162, 2*$5 + 1)); }
              |  'S' 'H' 'L' shiftval ';'        { store2syl(twosylshetc(0164, 2*$4 + 1)); }
              |  'S' 'H' 'L' 'D' shiftval ';'    { store2syl(twosylshetc(0166, 2*$5 + 1)); }
              |  'S' 'H' 'C' shiftval ';'        { store2syl(twosylshetc(0167, 2*$4 + 1)); }

              |  'S' 'H' 'A' 'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0161, $5, 0)); }
              |  'S' 'H' 'A' 'D' 'C' UNSIGNED_INTEGER ';'    { store2syl(twosylqx(0162, $6, 0)); }
              |  'S' 'H' 'L' 'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0164, $5, 0)); }
              |  'S' 'H' 'L' 'D' 'C' UNSIGNED_INTEGER ';'    { store2syl(twosylqx(0166, $6, 0)); }
              |  'S' 'H' 'C' 'C' UNSIGNED_INTEGER ';'        { store2syl(twosylqx(0167, $5, 0)); }

              |  jumpinstr
              |  'O' 'U' 'T' ';'                              { store3syl(0200, (0220<<8)); }
              |  'E' 'X' 'I' 'T' 'D' ';'                      { store3syl(0222, (0360<<8)); }
              |  'E' 'X' 'I' 'T' UNSIGNED_INTEGER ';'         { store3syl(0202 - (($5&1)<<1), (0360<<8)+($5>>1)); }
              |  'E' 'X' 'I' 'T' ';'                          { store3syl(0202, (0360<<8)); }
              |  'E' 'X' 'I' 'T' 'A' memref ';'               { store3syl(0202, (0360<<8) + $6); }
              |  'E' 'X' 'I' 'T' 'A' 'P' UNSIGNED_INTEGER ';' { store3syl(0202, (0360<<8) + codeloc($7, 0)); }
              |  'E' 'X' 'I' 'T' UNSIGNED_INTEGER 'A' 'P' UNSIGNED_INTEGER ';'
                                                              { store3syl(0202 - (($5&1)<<1), (0360<<8) + codeloc($8, 0)); }  /* !!! */

              |  'C' 'T' 'Q' UNSIGNED_INTEGER ';'                 { store2syl(twosylqx(0120, $4, 000)); }
              |  'P' 'R' 'Q' UNSIGNED_INTEGER ';'                 { store2syl(twosylqx(0124, $4, 000)); }
              |  'P' 'W' 'Q' UNSIGNED_INTEGER ';'                 { store2syl(twosylqx(0130, $4, 000)); }
              |  'T' 'W' 'Q' UNSIGNED_INTEGER ';'                 { store2syl(twosylqx(0130, $4, 000)); }
              |  'L' 'P' 'Q' UNSIGNED_INTEGER ';'                 { store2syl(twosylqx(0130, $4, 000)); }
              |  'M' 'W' 'Q' UNSIGNED_INTEGER ';'                 { store2syl(twosylqx(0130, $4, 000)); }

              |  'M' 'L' 'B' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0120, $5, 004)); }
              |  'M' 'B' 'T' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0120, $5, 010)); }
              |  'P' 'A' 'R' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0121, $5, 000)); }
              |  'M' 'E' 'T' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0122, $5, 000)); }
              |  'M' 'F' 'R' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0124, $5, 000)); }
              |  'C' 'L' 'O' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0124, $5, 002)); }
              |  'T' 'L' 'O' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0124, $5, 004)); }
              |  'P' 'R' 'C' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0124, $5, 010)); }
              |  'M' 'R' 'E' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0125, $5, 000)); }
              |  'P' 'R' 'E' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0125, $5, 000)); }
              |  'M' 'B' 'R' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0126, $5, 000)); }
              |  'P' 'W' 'C' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0130, $5, 010)); }
              |  'M' 'L' 'W' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0130, $5, 010)); }
              |  'I' 'N' 'T' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0134, $5, 002)); }
              |  'M' 'W' 'E' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0131, $5, 000)); }
              |  'P' 'W' 'E' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0131, $5, 000)); }
              |  'T' 'W' 'E' 'Q' UNSIGNED_INTEGER ';'             { store2syl(twosylqx(0131, $5, 000)); }

              |  'B' 'U' 'S' 'Y' 'Q' UNSIGNED_INTEGER ';'         { store2syl(twosylqx(0120, $6, 002)); }
              |  'M' 'G' 'A' 'P' 'Q' UNSIGNED_INTEGER ';'         { store2syl(twosylqx(0130, $6, 014)); }
              |  'P' 'G' 'A' 'P' 'Q' UNSIGNED_INTEGER ';'         { store2syl(twosylqx(0130, $6, 014)); }
              |  'M' 'L' 'W' 'E' 'Q' UNSIGNED_INTEGER ';'         { store2syl(twosylqx(0131, $6, 010)); }
              |  'M' 'F' 'S' 'K' 'Q' UNSIGNED_INTEGER ';'         { store2syl(twosylqx(0134, $6, 000)); }
              |  'M' 'B' 'S' 'K' 'Q' UNSIGNED_INTEGER ';'         { store2syl(twosylqx(0136, $6, 000)); }
              |  'M' 'R' 'W' 'D' 'Q' UNSIGNED_INTEGER ';'         { store2syl(twosylqx(0136, $6, 010)); }
              |  'M' 'B' 'R' 'E' 'Q' UNSIGNED_INTEGER ';'         { store2syl(twosylqx(0127, $6, 000)); }
              |  'M' 'F' 'R' 'E' 'Q' UNSIGNED_INTEGER ';'         { store2syl(twosylqx(0125, $6, 000)); }
              |  'P' 'R' 'C' 'E' 'Q' UNSIGNED_INTEGER ';'         { store2syl(twosylqx(0125, $6, 010)); }

              |  'M' 'W' 'I' 'P' 'E' 'Q' UNSIGNED_INTEGER ';'     { store2syl(twosylqx(0130, $7, 004)); }
              |  'M' 'A' 'N' 'U' 'A' 'L' 'Q' UNSIGNED_INTEGER ';' { store2syl(twosylqx(0120, $8, 001)); }


              |  'P' 'I' 'A' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0124, $5, 000)); }   /* ordinary read */
              |  'P' 'I' 'B' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0125, $5, 000)); }   /* read to end-message */
              |  'P' 'I' 'C' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0124, $5, 010)); }   /* PRCQq */
              |  'P' 'I' 'D' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0125, $5, 010)); }   /* PRCEQq */
              |  'P' 'I' 'E' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0126, $5, 000)); }   /* MBRQq */
              |  'P' 'I' 'F' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0127, $5, 000)); }   /* MBREQq */
              |  'P' 'I' 'G' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0126, $5, 010)); }   /* alpha-numeric char read on CR */
              |  'P' 'I' 'H' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0127, $5, 010)); }   /* alpha-numeric char read to EM on CR */

              |  'P' 'O' 'A' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0130, $5, 000)); }   /* ordinary write */
              |  'P' 'O' 'B' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0131, $5, 000)); }   /* write to end-message */
              |  'P' 'O' 'C' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0130, $5, 010)); }   /* PWCQq and MLWQq */
              |  'P' 'O' 'D' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0131, $5, 010)); }   /* PWCEQq and MLWEQq */
              |  'P' 'O' 'E' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0130, $5, 014)); }   /* PGAPQq and MGAPQq */
              |  'P' 'O' 'F' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0130, $5, 004)); }   /* MWIPEQq */

              |  'P' 'M' 'A' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0134, $5, 000)); }   /* seek on disc MFSK */
              |  'P' 'M' 'B' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0120, $5, 010)); }   /* test MBT */
              |  'P' 'M' 'C' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0120, $5, 004)); }   /* test MLB */
              |  'P' 'M' 'D' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0136, $5, 010)); }   /* MRWD */
              |  'P' 'M' 'E' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0136, $5, 000)); }   /* MBSK */
              |  'P' 'M' 'F' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0122, $5, 000)); }   /* MET */
              |  'P' 'M' 'G' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0122, $5, 004)); }   /* Read C-store - definitive from KAA01 */
              |  'P' 'M' 'H' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0130, $5, 002)); }   /* Set lockout - definitive from KAA01 */
              |  'P' 'M' 'K' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0134, $5, 004)); }   /* IBM Even parity skip forward - definitive from KAA01 */
              |  'P' 'M' 'L' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0136, $5, 004)); }   /* IBM Even parity skip back - definitive from KAA01 */

              |  'P' 'O' 'G' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0132, $5, 000)); }   /* CP A/N;       FD Next sector - Bill's confident guess */
              |  'P' 'O' 'H' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0133, $5, 000)); }   /* CP A/N, EM;   FD Next sector, EM - Bill's confident guess */
              |  'P' 'O' 'K' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0133, $5, 010)); }   /* CP A/N, EM, Character mode; FD Next sector, EM, fixed heads - Bill's confident guess */
              |  'P' 'O' 'L' 'Q' UNSIGNED_INTEGER ';'   { store2syl(twosylqx(0132, $5, 010)); }   /* CP A/N, Character mode;     FD Next sector, fixed heads - Bill's confident guess */


              |  '$'                        { startnewword(); }
              |  '*'                        { startnewword(); }
              ;
%%

