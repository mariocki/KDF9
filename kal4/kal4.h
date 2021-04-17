
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

extern void setvstore(int vno);
/* sets give V-store from value constructed in vstoreval */

extern void setprinterconst(int v1, int v2);
/* sets a printer constant in a range of V-stores -- involves lexical cheating */

extern void startnewword();
/* inserts dummy instructions up to word boundary */

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

extern int endsrc;
