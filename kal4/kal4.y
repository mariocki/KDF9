%{
#include <ctype.h>
/* KDF9 assembler for KAL4 */
#include <stdio.h>

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

extern void setlabel(int labno, int loc);
/* record integer label - different in KAL4 */

extern int codeloc(int pno, int labno);
/* evaluate location in the code -- result is syllable address */

extern void store3syl(int instr, int addr);
/* takes the output from data loc and embeds in the instruction */

extern int dataloc(int pno, int vno);
/* get data location as word address for a V-store */

extern int ystoreloc(int yset, int yno);
/* get ystore location
   yset will be the letter for YA YB etc
 */

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

extern void startnewword(int n);
/* inserts dummy instructions up to word boundary */
/* n is the numbr of word boundary */
/* -- different from Usercode because of KAL4's ** which rounds to 32 word */

extern void fixY0(int zero, int zero2, int loc);
/* locates Y0 at specified absolute address, e.g. Y0=E 4896 */

/* KAL4 only */
extern int evaluate(int ident);
/* obtain the value of an identifier */

extern void promote(int ident);
/* increas the scope level of this identifier */

extern int makelabiden(int i1, int i2, int i3);
/* combines an identifier that was wrongly split becuase of the no digits rule after ; or : */

extern void nonums(int i1);
extern void onenum(int i1, int n1);
extern void twonums(int i1, int n1, int n2);
extern void justaddr(int i1, int n1);
extern void numaddr(int i1, int n1, int a1);

extern void openscope();
extern void closescope();

extern void onlyset(int i1, int n1);
/* just used for the SET instruction such as SET#55 */

extern int tablocate(char *s);
/* locate this character string as though it were an identifier */

extern int idtablocate(char *s, int id);
/* locate the character string formed by concatenating s and id as though it were an identifier */

extern void setlasttoken(char c);
/* sets the last token (always to ;??) to have mnemonic mode lexical analysis for x+Cn */

extern void preset48();
/* deposits a 48-bit preset value as per Usercode V-store */

extern void preset24();
/* deposits a 24-bit preset value in a half-word */

extern void preset96();
/* deposits a 96-bit preset value in a double-word */

extern void printerconst(char stype, int align);
/* sets a printer constant at current address -- involves lexical cheating */

extern void mpmi(int i1, int pm, int i2);
/* Only valid for instructions of the form M+Iq and M-Iq */

extern int addr16(int a);
/* take a byte address into KDF9 SJNS form if not a multiple of 6 */

extern void setvstore(int len);
/* copies value constructed in vstoreval into the next part of the program */

extern int jumptest(int idj, int idz, char *syn);
/* handles synonyms such as J>=Z -- JGEZ  */

extern void multplus(int mult, int id, char *syn);
/* handles MULT+, MULT+Cq, etc -- for possible future use */

extern void checkid(int id, char *pat);
/* checks tht the identifier is the specified pat */

extern void k4address(int memaddr, int constlen);
/* loads a memaddress object into vstoreval at the correct position for setvstore */

extern int endsrc;
extern int justz;
int constlen;        /* length of constant from header letter */
%}

/* Mostly IDENTIFIER is as in Algol 60, but at the start of an instruction (i.e. after ; or :)
   it is not allowed to contain digits, so as to recognise instruction such as SET8; FMMQ1/2;.
   To cope with lab2: there is are special productions whose actions invoke makelabiden()
   which reconstitutes the identifier by calling yylex().
 */
%token IDENTIFIER
%token UNSIGNED_INTEGER
%token COMMENT
%token BEGIN
%token END
%token LABEL
%token KDF9ADDRESS             /* address of the form 234s3 for syll 3 of word 234 */
%token STRING
%%
/* comment !!! indicates incomplete implementation */

program       : block                { printf("Reached end of file\n");  endsrc = 1; }
              ;

factor        : UNSIGNED_INTEGER                  { $$ = $1 * 6; }            /* addresses are in syllables */
              | factor '*' UNSIGNED_INTEGER       { $$ = $1 * $3 / 6; }
              | IDENTIFIER                        { $$ = evaluate($1); }
              | factor '*' IDENTIFIER             { $$ = $1 * evaluate($3) / 6; }
              | '(' expression ')'                { $$ = $2; }
              | KDF9ADDRESS
              ;

expression    : factor
              | expression '+' factor             { $$ = $1 + $3; }
              | expression '-' factor             { $$ = $1 - $3; }
              | '-' factor                        { $$ = -$2; }
              ;

codeloc       : expression
              ;

code          :  code  instruction
              |  instruction 
              ;

timesplus     : '×' '+'                                     { setlasttoken(';'); }    /* ensure ×+Cn is OK */
              ;

instruction   :  ';'
              |  '*'                                        { startnewword(1); }
              |  '$'                                        { startnewword(1); }
              |  '*' '*'                                    { startnewword(32); }
              |  '$' '$'                                    { startnewword(32); }
              |  COMMENT

              |  IDENTIFIER ':'                             { setlabel($1, -1); }
              |  IDENTIFIER '=' codeloc ':'                 { setlabel($1, $3); }
              |  IDENTIFIER '=' '#' digitsequence ':'       { setlabel($1, octalval($4)*6); }

              |  IDENTIFIER UNSIGNED_INTEGER ':'            { setlabel(makelabiden($1, $2, -1), -1); }
              |  IDENTIFIER UNSIGNED_INTEGER '=' codeloc ':'{ setlabel(makelabiden($1, $2, -1), $4); }
              |  IDENTIFIER UNSIGNED_INTEGER '=' '#' digitsequence ':'
                                                            { setlabel(makelabiden($1, $2, -1), octalval($5)*6); }

              |  IDENTIFIER UNSIGNED_INTEGER IDENTIFIER ':' { setlabel(makelabiden($1, $2, $3), -1); }
              |  IDENTIFIER UNSIGNED_INTEGER IDENTIFIER '=' codeloc ':'
                                                            { setlabel(makelabiden($1, $2, $3), $5); }

              |  IDENTIFIER ';'                             { nonums($1); }
              |  IDENTIFIER UNSIGNED_INTEGER ';'            { onenum($1, $2); }
              |  IDENTIFIER '-' UNSIGNED_INTEGER ';'        { onenum($1, -$3); }
              |  IDENTIFIER '+' UNSIGNED_INTEGER ';'        { onenum($1, $3); }
              |  IDENTIFIER '#' digitsequence ';'           { onlyset($1, $3); }    /* SETB */
              |  IDENTIFIER UNSIGNED_INTEGER '/' UNSIGNED_INTEGER ';'
                                                            { twonums($1, $2, $4); }
              |  IDENTIFIER '.' codeloc ';'                 { justaddr($1, $3); }
              |  IDENTIFIER UNSIGNED_INTEGER '.' codeloc';' { numaddr($1, $2, $4); }
              |  block


              |  '+' ';'                                   { nonums(tablocate("+")); }
              |  '%' ';'                                   { nonums(tablocate("%")); }
              |  '-' ';'                                   { nonums(tablocate("-")); }
              |  '×' ';'                                   { nonums(tablocate("×")); }
              |  '÷' ';'                                   { nonums(tablocate("÷")); }

              |  timesplus ';'                             { onenum(tablocate("×+"), 47); }
              |  timesplus UNSIGNED_INTEGER ';'            { onenum(tablocate("×+"), $3); }
              |  timesplus '+' UNSIGNED_INTEGER ';'        { onenum(tablocate("×+"), $4); }   /* shifts -- needs upgrade for MULT+n */
              |  timesplus '-' UNSIGNED_INTEGER ';'        { onenum(tablocate("×+"), -$4); }
              |  timesplus IDENTIFIER ';'                  { nonums(idtablocate("×+", $2)); }
              |  timesplus IDENTIFIER UNSIGNED_INTEGER ';' { onenum(idtablocate("×+", $2), $3); }

              |  IDENTIFIER '#' IDENTIFIER '.' codeloc ';' { justaddr(jumptest($1, $3, "JNEZ"), $5); }
              |  IDENTIFIER '#' '.' codeloc ';'            { justaddr(jumptest($1, justz, "JNE"), $4); }

              |  IDENTIFIER '=' IDENTIFIER '.' codeloc ';' { justaddr(jumptest($1, $3, "JEQZ"), $5); }
              |  IDENTIFIER '=' '.' codeloc ';'            { justaddr(jumptest($1, justz, "JEQ"), $4); }
              |  IDENTIFIER '<' IDENTIFIER '.' codeloc ';' { justaddr(jumptest($1, $3, "JLTZ"), $5); }
              |  IDENTIFIER '<' '.' codeloc ';'            { justaddr(jumptest($1, justz, "JLT"), $4); }
              |  IDENTIFIER '>' IDENTIFIER '.' codeloc ';' { justaddr(jumptest($1, $3, "JGTZ"), $5); }
              |  IDENTIFIER '>' '.' codeloc ';'            { justaddr(jumptest($1, justz, "JGT"), $4); }
              |  IDENTIFIER '<' '=' IDENTIFIER '.' codeloc ';' { justaddr(jumptest($1, $4, "JLEZ"), $6); }
              |  IDENTIFIER '<' '=' '.' codeloc ';'        { justaddr(jumptest($1, justz, "JLE"), $5); }
              |  IDENTIFIER '>' '=' IDENTIFIER '.' codeloc ';' { justaddr(jumptest($1, $4, "JGEZ"), $6); }
              |  IDENTIFIER '>' '=' '.' codeloc ';'        { justaddr(jumptest($1, justz, "JGE"), $5); }

              |  '%' IDENTIFIER ';'         { nonums(idtablocate("%", $2)); }
              |  '+' IDENTIFIER ';'         { nonums(idtablocate("+", $2)); }
              |  '-' IDENTIFIER ';'         { nonums(idtablocate("-", $2)); }
              |  '×' IDENTIFIER ';'         { nonums(idtablocate("×", $2)); }
              |  '÷' IDENTIFIER ';'         { nonums(idtablocate("÷", $2)); }

              |  IDENTIFIER '-' IDENTIFIER ';'        { mpmi($1, '-', $3); }    /* M-Iq */
              |  IDENTIFIER '+' IDENTIFIER ';'        { mpmi($1, '+', $3); }    /* M+Iq, S+Mq, S+Iq, S+Cq, S+Qq needs upgrate for MULT+Cq */

              |  IDENTIFIER '+' ';'                             { checkid($1, "MULT"); onenum(tablocate("×+"), 47); } /* MULT+  */
       /*     |  IDENTIFIER '+' UNSIGNED_INTEGER ';'            { checkid($1, "MULT"); onenum(tablocate("×+"), $4); }  */
              |  IDENTIFIER '+' '+' UNSIGNED_INTEGER ';'        { checkid($1, "MULT"); onenum(tablocate("×+"), $5); }
              |  IDENTIFIER '+' '-' UNSIGNED_INTEGER ';'        { checkid($1, "MULT"); onenum(tablocate("×+"), -$5); }
              |  IDENTIFIER '+' IDENTIFIER UNSIGNED_INTEGER ';' { checkid($1, "MULT"); onenum(idtablocate("×+", $3), $4); }

              |   preset

              |   LABEL idlist ';'
              ;

begin         :  BEGIN                              { openscope(); }

block         :  begin code END                     { closescope(); }
              ;

idlist        :  idlist ',' IDENTIFIER       { promote($3); }
              |  IDENTIFIER                  { promote($1); }
              ;

digitsequence :  UNSIGNED_INTEGER      { $$ = keepMarker(); }      /* result is a pointer to the start of the digit string */
              ;
              ;

memref        : codeloc                { $$ = addr16($1); }
              ;

bit16         :   memref
              |  '#' digitsequence     { $$ = octalval($2); }
              ;

vstoreval     :  digitsequence                             { vfraction($1, constlen-1); }    /* most of this is identical to Usercode */
              |  '#' digitsequence                         { voctal($2, constlen-1); }
              |  '#' digitsequence '/' UNSIGNED_INTEGER    { voctal($2, $4); }
              |  digitsequence '.' UNSIGNED_INTEGER        { vfraction($1, constlen-1); }
              |  digitsequence '.' UNSIGNED_INTEGER '/' UNSIGNED_INTEGER  { vfraction($1, $5); }
              |  digitsequence '/' UNSIGNED_INTEGER        { vfraction($1, $3); }
              |  '-' digitsequence                         { vfraction($2, constlen-1); negatevstore(); }
              |  '-' digitsequence '.' UNSIGNED_INTEGER    { vfraction($2, constlen-1); negatevstore(); }
              |  '-' digitsequence '.' UNSIGNED_INTEGER '/' UNSIGNED_INTEGER  { vfraction($2, $6); negatevstore(); }
              |  '-' digitsequence '/' UNSIGNED_INTEGER    { vfraction($2, $4); negatevstore(); }
              |  '+' digitsequence                         { vfraction($2, constlen-1); }
              |  '+' digitsequence '.' UNSIGNED_INTEGER    { vfraction($2, constlen-1); }
              |  '+' digitsequence '.' UNSIGNED_INTEGER '/' UNSIGNED_INTEGER  { vfraction($2, $6); }
              |  '+' digitsequence '/' UNSIGNED_INTEGER    { vfraction($2, $4); }
              |  '+' digitsequence '.' UNSIGNED_INTEGER '/' '-' UNSIGNED_INTEGER  { vfraction($2, -$7); }
              |  'Q' bit16 '/' bit16 '/' bit16             { vqformat($2, $4, $6); }
              |  'F' digitsequence '.' UNSIGNED_INTEGER     { vfloat($2, 0); vfppos(); }
              |  'F' digitsequence                          { vfloat($2, 0); vfppos(); }
              |  'F' digitsequence '.' UNSIGNED_INTEGER '@' UNSIGNED_INTEGER   { vfloat($2, $6); vfppos(); }
              |  'F' digitsequence '.' UNSIGNED_INTEGER '@' '-' UNSIGNED_INTEGER   { vfloat($2, -$7); vfppos(); }
              |  'F' digitsequence '.' UNSIGNED_INTEGER '@' '+' UNSIGNED_INTEGER   { vfloat($2, $7); vfppos(); }
              |  'F' digitsequence '@' UNSIGNED_INTEGER     { vfloat($2, $4); vfppos(); }
              |  'F' digitsequence '@' '-' UNSIGNED_INTEGER     { vfloat($2, -$5); vfppos(); }
              |  'F' '-' digitsequence '.' UNSIGNED_INTEGER { vfloat($3, 0); vfpneg(); }
              |  'F' '-' digitsequence                      { vfloat($3, 0); vfpneg(); }
              |  'F' '-' digitsequence '.' UNSIGNED_INTEGER '@' UNSIGNED_INTEGER   { vfloat($3, $7); vfpneg(); }
              |  'F' '-' digitsequence '.' UNSIGNED_INTEGER '@' '-' UNSIGNED_INTEGER   { vfloat($3, -$8); vfpneg(); }
              |  'F' '-' digitsequence '.' UNSIGNED_INTEGER '@' '+' UNSIGNED_INTEGER   { vfloat($3, $8); vfpneg(); }
              |  'F' '-' digitsequence '@' UNSIGNED_INTEGER { vfloat($3, $5); vfpneg(); }
              |  'F' '-' digitsequence '@' '-' UNSIGNED_INTEGER { vfloat($3, -$6); vfpneg(); }
              |  'F' '+' digitsequence '.' UNSIGNED_INTEGER { vfloat($3, 0); vfppos(); }
              |  'F' '+' digitsequence                      { vfloat($3, 0); vfppos(); }
              |  'F' '+' digitsequence '.' UNSIGNED_INTEGER '@' UNSIGNED_INTEGER   { vfloat($3, $7); vfppos(); }
              |  'F' '+' digitsequence '.' UNSIGNED_INTEGER '@' '-' UNSIGNED_INTEGER   { vfloat($3, -$8); vfppos(); }
              |  'F' '+' digitsequence '.' UNSIGNED_INTEGER '@' '+' UNSIGNED_INTEGER   { vfloat($3, $8); vfppos(); }
              |  'F' '+' digitsequence '@' UNSIGNED_INTEGER { vfloat($3, $5); vfppos(); }
              |  'F' '+' digitsequence '@' '-' UNSIGNED_INTEGER { vfloat($3, -$6); vfppos(); }

              |  'A' memref                                 { k4address($2, constlen); }   /* KAL4 specific */
              ;




eqlen         :  '='                 { constlen = 48; }
              |  '=' 'D'             { constlen = 95; }
              |  '=' 'H'             { constlen = 24; }
              |  '=' 'T'             { constlen = 16; }
              |  '=' 'S'             { constlen = 8; }
              ;

preset        :  eqlen vstoreval ';' { setvstore(constlen); }
              |  eqlen STRING ';'    { printerconst($2, constlen); }
              ;


%%


