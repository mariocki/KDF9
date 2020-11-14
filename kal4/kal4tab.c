// Constructs the table if instructions seen as identifiers

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>

#define  IDHASHSZ  3000
extern char constype[];
extern char idenchar[];
extern char isletter[];
extern char abs8tab[];               // 8-bit Algol basic symbols indexed by ASCII values
extern char abs6tab[];               // 6-bit Algol basic symbols indexed by ASCII values
extern char lprttab[];               // 6-bit KDF9 printer code indexed by ASCII values

extern char *idtable[];              // allow up to 3000 names -- temp ????  -- add an extra one in case there is a duplicate at the top
extern int category[];               // Indicates what type of instruction for identifiers that are also valid instructions

extern int mplusi, mminusi, splusm, splusi, splusc, splusq, justj, justz;
                                      // values of pseudo identifiers M+I, M-I, S+M, S+I, S+C, S+Q, J and Z

// defining categories of instructions
#define  ONESYLL  1
#define  ONEQ  2
#define  TWOQ  3
#define  SET  4
#define  JUMP  5
#define  JUMPQ  6
#define  MEMREF  7
#define  MEMREFQ  8
#define  SHIFT  9
#define  LINK  10
#define  OUTETC  11
#define  EXIT  12
#define  LATER  99

    int
    getidindex(unsigned char *s)
/* fancy characters in instruction mnemonics need unsigned char */
{  int hh = (((*s)<<7) + s[1]) % IDHASHSZ;

   while  ( idtable[hh] != NULL  &&  strcmp(idtable[hh], s) != 0 )
      if  ( ++hh == IDHASHSZ )
         hh = 0;
   return hh;
}

int setcatok(char *ins, int cat, int s1, int s2)
/* fancy characters in instruction mnemonics need unsigned char */
{  int ix = getidindex((unsigned char *)ins);
   if  ( idtable[ix] != NULL )
   {  ins = idtable[ix];                          // ensure just one copy of the string, used in checks
      if  ( idtable[++ix] != NULL )
      {  printf("Instruction %s is duplicated - next slot %s\n", ins, idtable[ix]);
         return -1;
      }
   }

   idtable[ix] = ins;
   category[ix] = (s1<<16) + (s2<<8) + cat;
   return ix;
}

void setcatlater(char *ins, int cat, int s1, int s2)
/* fancy characters in instruction mnemonics need unsigned char */
{  int ix = getidindex((unsigned char *)ins);
   if  ( idtable[ix] != NULL )
      printf("Instruction %s is duplicated\n", ins);
}

char absinit[] = "!^<!(!!.![..!{*!_%?!)!!.;]..!} !?/=!!,?.!×?!.?!..:....?.!+>!..!......!?.!-#...!......!..!...............!......!";

void initidtable()
{  int i;

   for  ( i = 0; i<256; i++ )
   {  idenchar[i] = 0;
      isletter[i] = 0;
      constype[i] = 0;
      abs8tab[i] = 255;
      abs6tab[i] = 255;
      lprttab[i] = 255;
   }
   for  ( i = 'A'; i<='Z'; i++ )
   {  idenchar[i] = i;
      isletter[i] = i;
      abs8tab[i] = 014 - 'A' + i;
      abs6tab[i] = 014 - 'A' + i;   // not sure what these should be
      lprttab[i] = 041 - 'A' + i;
   }
   for  ( i = 'a'; i<='z'; i++ )
   {  idenchar[i] = i;
      isletter[i] = i;
      abs8tab[i] = 046 - 'a' + i;
      abs6tab[i] = 046 - 'a' + i;
      lprttab[i] = 041 - 'a' + i;
   }
   for  ( i = '0'; i<='9'; i++ )
   {  idenchar[i] = i;
      abs8tab[i] = i - '0';
      abs6tab[i] = i - '0';
      lprttab[i] = 020 - '0' + i;
   }

   abs6tab['+'] = 010;
   abs6tab['-'] = 011;

   lprttab['%'] = 06;
   lprttab['\''] = 07;                // 07 '
   lprttab[':'] = 010;
   lprttab['='] = 011;
   lprttab['('] = 012;
   lprttab[')'] = 013;

   lprttab['*'] = 015;
   lprttab[','] = 016;
   lprttab['/'] = 017;
// lprttab['#'] = 033;      // subscript 10 - what is this in Hans's code
   lprttab['+'] = 035;
   lprttab['-'] = 036;
   lprttab['.'] = 037;
   lprttab['$'] = 014;        // 14 dd &pound;
// lprttab['#'] = 000;        // space
   lprttab[' '] = -1;         // space is ignored in the printer constant

   for  ( i = 0; i<=0200; i++ )
      abs8tab[absinit[i]&255] = i + 0200;
   abs8tab['.'] = 013;
   abs8tab['!'] = 0377;        // inderlined ABS ???
   abs8tab['?'] = 0377;        // compound ABS ???
   abs8tab[' '] = 0377;        // ignore spaces in string constants
   abs8tab['\t'] = 0377;       // ignore tabs in string constants
   abs8tab['\n'] = 0377;       // ignore newlines in string constants

   constype['H'] = '=';        // leave lasttoken as = for the length codes D T S H
   constype['S'] = '=';
   constype['D'] = '=';
   constype['T'] = '=';

   constype['A'] = 'A';        // anything other than = ensures that ...
   constype['F'] = 'F';        // ...  we do not treat the next letter as a single token
   constype['P'] = 'P';
   constype['Q'] = 'Q';
   constype['X'] = 'X';

   for  ( i = 0; i<IDHASHSZ; i++ )
   {  idtable[i] = NULL;
      category[i] = 0;           // category 0 means not an instruction
   }

   setcatok("SM", MEMREFQ, 0301, 0);    // ensure that duplicates can live in adjacent cells
   i = setcatok("SM", ONEQ, 0170, 2);       // ONEQ version must be the second one
   if  ( i == 0 )                       // if we have been really unlucky
      printf("Hash table has wrapped round on duplicate instruction SM\n");
   setcatok("FM", MEMREFQ, 0300, 0);
   i = setcatok("FM", ONEQ, 0171, 2);
   if  ( i == 0 )                       // if we have been really unlucky
      printf("Hash table has wrapped round on duplicate instruction FM\n");
   setcatok("MFRQ", TWOQ, 0151, 0);
   i = setcatok("MFRQ", ONEQ, 0124, 000);
   if  ( i == 0 )                       // if we have been really unlucky
      printf("Hash table has wrapped round on duplicate instruction MFRQ\n");

   setcatok("FI", ONEQ, 0171, 4);
   setcatok("FC", ONEQ, 0171, 010);
   setcatok("FQ", ONEQ, 0171, 016);

   setcatok("S", MEMREF, 0301, 0);
   setcatok("F", MEMREF, 0300, 0);
   setcatok("SMQ", MEMREFQ, 0303, 0);
   setcatok("FMQ", MEMREFQ, 0302, 0);

   setcatok("IFRQ", TWOQ, 0152, 0);
   setcatok("IMFRQ", TWOQ, 0153, 0);
   setcatok("CFRQ", TWOQ, 0154, 0);
   setcatok("CMFRQ", TWOQ, 0155, 0);
   setcatok("CIFRQ", TWOQ, 0156, 0);
   setcatok("QFRQ", TWOQ, 0157, 0);

   setcatok("SRM", ONEQ, 0170, 3);
   setcatok("SI", ONEQ, 0170, 4);
   setcatok("SRI", ONEQ, 0170, 5);
   setcatok("SC", ONEQ, 0170, 010);
   setcatok("SRC", ONEQ, 0170, 011);
   setcatok("SQ", ONEQ, 0170, 016);

   setcatok("SET", SET, 0304, 0);

   justz = setcatok("Z", 0, 0, 0);         // just for checking things like J<Z
   justj = setcatok("J", JUMP, 0200, 0260);
   setcatok("JS", JUMP, 0200, 0320);
   setcatok("JEQ", JUMP, 0220, 0020);
   setcatlater("J=", JUMP, 0220, 0020);
   setcatlater("J!=", JUMP, 0200, 0020);
   setcatlater("J/=", JUMP, 0200, 0020);
   setcatlater("J#", JUMP, 0200, 0020);
   setcatok("JNE", JUMP, 0200, 0020);
   setcatlater("J*NE", JUMP, 0200, 0020);
   setcatlater("J<Z", JUMP, 0220, 0040);
   setcatok("JLTZ", JUMP, 0220, 0040);
   setcatlater("J*LTZ", JUMP, 0220, 0040);
   setcatlater("J>=Z", JUMP, 0200, 0040);
   setcatok("JGEZ", JUMP, 0200, 0040);
   setcatlater("J*GEZ", JUMP, 0200, 0040);
   setcatlater("J>Z", JUMP, 0220, 0100);
   setcatlater("J*GTZ", JUMP, 0220, 0100);
   setcatok("JGTZ", JUMP, 0220, 0100);
   setcatlater("J<=Z", JUMP, 0200, 0100);
   setcatlater("J*LEZ", JUMP, 0200, 0100);
   setcatok("JLEZ", JUMP, 0200, 0100);
   setcatok("JEQZ", JUMP, 0220, 0140);
   setcatlater("J=Z", JUMP, 0220, 0140);
   setcatlater("J!=Z", JUMP, 0200, 0140);
   setcatlater("J/=Z", JUMP, 0200, 0140);
   setcatlater("J#Z", JUMP, 0200, 0140);
   setcatlater("J*NEZ", JUMP, 0200, 0140);
   setcatok("JNEZ", JUMP, 0200, 0140);
   setcatok("JV", JUMP, 0220, 0200);
   setcatok("JNV", JUMP, 0200, 0200);
   setcatok("JEN", JUMP, 0220, 0240);
   setcatok("JNEN", JUMP, 0200, 0240);
   setcatok("JEJ", JUMP, 0220, 0300);
   setcatok("JNEJ", JUMP, 0200, 0300);
   setcatok("JTR", JUMP, 0220, 0340);
   setcatok("JNTR", JUMP, 0200, 0340);

   setcatok("JCZ", JUMPQ, 0240, 0);
   setcatok("JCNZ", JUMPQ, 0260, 0);

   setcatok("JCNZS", ONEQ, 0177, 0);

   setcatok("FMM", TWOQ, 0100, 0);
   setcatok("FMMQ", TWOQ, 0102, 0);
   setcatok("FMMN", TWOQ, 0110, 0);
   setcatok("FMMQN", TWOQ, 0112, 0);
   setcatok("FMMH", TWOQ, 0104, 0);
   setcatok("FMMQH", TWOQ, 0106, 0);
   setcatok("FMMHN", TWOQ, 0114, 0);
   setcatok("FMMQHN", TWOQ, 0116, 0);
   setcatok("SMM", TWOQ,0101, 0);
   setcatok("SMMQ", TWOQ, 0103, 0);
   setcatok("SMMN", TWOQ, 0111, 0);
   setcatok("SMMQN", TWOQ, 0113, 0);
   setcatok("SMMH", TWOQ, 0105, 0);
   setcatok("SMMQH", TWOQ, 0107, 0);
   setcatok("SMMHN", TWOQ, 0115, 0);
   setcatok("SMMQHN", TWOQ, 0117, 0);

   setcatok("VR",   ONESYLL, 001, 0);
   setcatok("SETTR",   ONESYLL, 002, 0);
   setcatok("BITS",   ONESYLL, 003, 0);
   setcatlater("xF",   ONESYLL, 004, 0);
   setcatlater("xDF",   ONESYLL, 005, 0);
   setcatlater("x+F",   ONESYLL, 007, 0);
   setcatlater("xD",   ONESYLL, 034, 0);
   setcatlater("x",   ONESYLL, 035, 0);
   setcatlater("x+", SHIFT, 0163, 0);
   setcatlater("x+C", ONEQ, 0163, 0);

   setcatok("MULTF",   ONESYLL, 004, 0);
   setcatok("MULTDF",   ONESYLL, 005, 0);
   setcatlater("MULT+F",   ONESYLL, 007, 0);
   setcatok("MULTD",   ONESYLL, 034, 0);
   setcatok("MULT",   ONESYLL, 035, 0);
   setcatok("MULT+", SHIFT, 0163, 0);
   setcatok("MULT+C", ONEQ, 0163, 0);

   setcatok("XF",   ONESYLL, 004, 0);
   setcatok("XDF",   ONESYLL, 005, 0);
   setcatlater("X+F",   ONESYLL, 007, 0);
   setcatok("XD",   ONESYLL, 034, 0);
   setcatok("X",   ONESYLL, 035, 0);
   setcatlater("X+", SHIFT, 0163, 0);
   setcatlater("X+C", ONEQ, 0163, 0);
   setcatok("NEGD",   ONESYLL, 010, 0);
   setcatok("OR",   ONESYLL, 011, 0);
   setcatok("PERM",   ONESYLL, 012, 0);
   setcatok("TOB",   ONESYLL, 013, 0);
   setcatok("ROUNDH",   ONESYLL, 014, 0);
   setcatok("NEV",   ONESYLL, 015, 0);
   setcatok("ROUND",   ONESYLL, 016, 0);
   setcatok("DUMMY",   ONESYLL, 017, 0);
   setcatok("ROUNDF",   ONESYLL, 020, 0);
   setcatok("ROUNDHF",   ONESYLL, 021, 0);
   setcatok("FLOAT",   ONESYLL, 024, 0);
   setcatok("FLOATD",   ONESYLL, 025, 0);
   setcatok("ABS",   ONESYLL, 026, 0);
   setcatok("NEG",   ONESYLL, 027, 0);
   setcatok("ABSF",   ONESYLL, 030, 0);
   setcatok("NEGF",   ONESYLL, 031, 0);
   setcatok("MAX",   ONESYLL, 032, 0);
   setcatok("NOT",   ONESYLL, 033, 0);
   setcatok("-",   ONESYLL, 036, 0);
   setcatok("SIGN",   ONESYLL, 037, 0);
   setcatok("ZERO",   ONESYLL, 041, 0);
   setcatok("DUP",   ONESYLL, 042, 0);
   setcatok("DUPD",   ONESYLL, 043, 0);
   setcatok("FIX",   ONESYLL, 045, 0);
   setcatok("STR",   ONESYLL, 047, 0);
   setcatok("CONT",   ONESYLL, 050, 0);
   setcatok("REVD",   ONESYLL, 051, 0);
   setcatok("ERASE",   ONESYLL, 052, 0);
   setcatok("-D",   ONESYLL, 053, 0);
   setcatok("AND",   ONESYLL, 054, 0);

   setcatok("DIVI",   ONESYLL, 044, 0);
   setcatok("DIV",   ONESYLL, 060, 0);
   setcatok("DIVD",   ONESYLL, 061, 0);
   setcatok("DIVF",   ONESYLL, 062, 0);
   setcatok("DIVDF",   ONESYLL, 063, 0);
   setcatok("DIVR",   ONESYLL, 064, 0);

   setcatok("REV",   ONESYLL, 065, 0);
   setcatok("CAB",   ONESYLL, 066, 0);
   setcatok("FRB",   ONESYLL, 067, 0);
   setcatok("STAND",   ONESYLL, 070, 0);
   setcatok("NEGDF",   ONESYLL, 071, 0);
   setcatok("MAXF",   ONESYLL, 072, 0);
   setcatok("SIGNF",   ONESYLL, 077, 0);

   setcatlater("K", LATER, 0176, 0);
   setcatlater("=K", LATER, 0175, 0);

   mplusi = setcatok("M+I", ONEQ, 0140, 0);
   mminusi = setcatok("M-I", ONEQ, 0141, 0);
   setcatok("NC", ONEQ, 0142, 0);
   setcatok("DC", ONEQ, 0143, 0);
   setcatok("IONEQ", ONEQ, 0144, 0);
   setcatok("IONE", ONEQ, 0144, 0);
   setcatok("ITWOQ", ONEQ, 0146, 0);
   setcatok("ITWO", ONEQ, 0146, 0);        /* seems to be valid without the Q */
   setcatok("IMONEQ", ONEQ, 0145, 0);
   setcatok("IMONE", ONEQ, 0145, 0);
   setcatok("IMTWOQ", ONEQ, 0147, 0);
   setcatok("IMTWO", ONEQ, 0147, 0);

   setcatlater("I=+1", LATER, 0144, 0);
   setcatlater("I=+2", LATER, 0146, 0);
   setcatlater("I=1", LATER, 0144, 0);
   setcatlater("I=2", LATER, 0146, 0);
   setcatlater("I=-1", LATER, 0145, 0);
   setcatlater("I=-2", LATER, 0147, 0);

   splusm = setcatok("S+M", ONEQ, 0172, 2);
   splusi = setcatok("S+I", ONEQ, 0172, 4);
   splusc = setcatok("S+C", ONEQ, 0172, 010);
   splusq = setcatok("S+Q", ONEQ, 0172, 016);

   setcatok("FLINK", LINK, 0173, 0);
   setcatok("SLINK", LINK, 0174, 0);

   setcatok("SHA", SHIFT, 0161, 0);
   setcatok("SHAD", SHIFT, 0162, 0);
   setcatok("SHL", SHIFT, 0164, 0);
   setcatok("SHLD", SHIFT, 0166, 0);
   setcatok("SHC", SHIFT, 0167, 0);

   setcatok("SHAC", ONEQ, 0161, 0);
   setcatok("SHADC", ONEQ, 0162, 0);
   setcatok("SHLC", ONEQ, 0164, 0);
   setcatok("SHLDC", ONEQ, 0166, 0);
   setcatok("SHCC", ONEQ, 0167, 0);

   setcatok("OUT", OUTETC, 0200, 0220);
   setcatok("EXITD", OUTETC, 0222, 0360);
   setcatok("EXIT", EXIT, 0202, 0360);
   setcatok("EXITH", EXIT, 0200, 0360);

   setcatok("CTQ", ONEQ, 0120, 000);
   setcatok("MANUALQ", ONEQ, 0120, 001);
   setcatok("BUSYQ", ONEQ, 0120, 002);
   setcatok("MLBQ", ONEQ, 0120, 004);
   setcatok("MBTQ", ONEQ, 0120, 010);
   setcatok("PARQ", ONEQ, 0121, 000);
   setcatok("METQ", ONEQ, 0122, 000);
// setcatok("MFRQ", ONEQ, 0124, 000);       Put in at the top because of duplication
   setcatok("PRQ", ONEQ, 0124, 000);
   setcatok("TRQ", ONEQ, 0124, 000);
   setcatok("CLOQ", ONEQ, 0124, 002);
   setcatok("TLOQ", ONEQ, 0124, 004);
   setcatok("PRCQ", ONEQ, 0124, 010);
   setcatok("MREQ", ONEQ, 0125, 000);
   setcatok("PREQ", ONEQ, 0125, 000);
   setcatok("TREQ", ONEQ, 0125, 000);
   setcatok("PRCEQ", ONEQ, 0125, 010);
   setcatok("MBRQ", ONEQ, 0126, 000);
   setcatok("MBREQ", ONEQ, 0127, 000);
   setcatok("PWQ", ONEQ, 0130, 000);
   setcatok("TWQ", ONEQ, 0130, 000);
   setcatok("LPQ", ONEQ, 0130, 000);
   setcatok("LPEQ", ONEQ, 0131, 000);
   setcatok("PWCQ", ONEQ, 0130, 010);
   setcatok("MWQ", ONEQ, 0130, 000);
   setcatok("MLWQ", ONEQ, 0130, 010);
   setcatok("MGAPQ", ONEQ, 0130, 014);
   setcatok("PGAPQ", ONEQ, 0130, 014);
   setcatok("MWIPEQ", ONEQ, 0130, 004);
   setcatok("MWEQ", ONEQ, 0131, 000);
   setcatok("PWEQ", ONEQ, 0131, 000);
   setcatok("TWEQ", ONEQ, 0131, 000);
   setcatok("MLWEQ", ONEQ, 0131, 010);
   setcatok("MFSKQ", ONEQ, 0134, 000);
   setcatok("INTQ", ONEQ, 0134, 002);

   setcatok("MBSKQ", ONEQ, 0136, 000);
   setcatok("MRWDQ", ONEQ, 0136, 010);

   setcatok("PWCEQ", ONEQ, 0131, 010);


   setcatok("PIAQ", ONEQ, 0124, 000);   /* ordinary read */
   setcatok("PIBQ", ONEQ, 0125, 000);   /* read to end-message */
   setcatok("PICQ", ONEQ, 0124, 010);   /* PRCQq */
   setcatok("PIDQ", ONEQ, 0125, 010);   /* PRCEQq */
   setcatok("PIEQ", ONEQ, 0126, 000);   /* MBRQq */
   setcatok("PIFQ", ONEQ, 0127, 000);   /* MBREQq */
   setcatok("PIGQ", ONEQ, 0126, 010);   /* alpha-numeric char read on CR */
   setcatok("PIHQ", ONEQ, 0127, 010);   /* alpha-numeric char read to EM on CR */

   setcatok("POAQ", ONEQ, 0130, 000);   /* ordinary write */
   setcatok("POBQ", ONEQ, 0131, 000);   /* write to end-message */
   setcatok("POCQ", ONEQ, 0130, 010);   /* PWCQq and MLWQq */
   setcatok("PODQ", ONEQ, 0131, 010);   /* PWCEQq and MLWEQq */
   setcatok("POEQ", ONEQ, 0130, 014);   /* PGAPQq and MGAPQq */
   setcatok("POFQ", ONEQ, 0130, 004);   /* MWIPEQq */

   setcatok("PMAQ", ONEQ, 0134, 000);   /* seek on disc MFSK */
   setcatok("PMBQ", ONEQ, 0120, 010);   /* test MBT */
   setcatok("PMCQ", ONEQ, 0120, 004);   /* test MLB */
   setcatok("PMDQ", ONEQ, 0136, 010);   /* MRWD */
   setcatok("PMEQ", ONEQ, 0136, 000);   /* MBSK */
   setcatok("PMFQ", ONEQ, 0122, 000);   /* MET */

   setcatok("PMGQ", ONEQ, 0122, 004);   /* Read C-store - definitive from KAA01 */
   setcatok("PMHQ", ONEQ, 0130, 002);   /* Set lockout - definitive from KAA01 */
   setcatok("PMKQ", ONEQ, 0134, 004);   /* IBM Even parity skip forward - definitive from KAA01 */
   setcatok("PMLQ", ONEQ, 0136, 004);   /* IBM Even parity skip back - definitive from KAA01 */

   setcatok("POGQ", ONEQ, 0132, 000);   /* CP A/N;       FD Next sector - Bill's confident guess */
   setcatok("POHQ", ONEQ, 0133, 000);   /* CP A/N, EM;   FD Next sector, EM - Bill's confident guess */
   setcatok("POKQ", ONEQ, 0133, 010);   /* CP A/N, EM, Character mode; FD Next sector, EM, fixed heads - Bill's confident guess */
   setcatok("POLQ", ONEQ, 0132, 010);   /* CP A/N, Character mode;     FD Next sector, fixed heads - Bill's confident guess */

   setcatok("POMQ", ONEQ, 0132, 010);   /* no idea */

// Entries to be located by tablocate and idtablocate
   setcatok("%I",   ONESYLL, 044, 0);
   setcatok("%",   ONESYLL, 060, 0);
   setcatok("%D",   ONESYLL, 061, 0);
   setcatok("%F",   ONESYLL, 062, 0);
   setcatok("%DF",   ONESYLL, 063, 0);
   setcatok("%R",   ONESYLL, 064, 0);

   setcatok("/I",   ONESYLL, 044, 0);
   setcatok("/",   ONESYLL, 060, 0);
   setcatok("/D",   ONESYLL, 061, 0);
   setcatok("/F",   ONESYLL, 062, 0);
   setcatok("/DF",   ONESYLL, 063, 0);
   setcatok("/R",   ONESYLL, 064, 0);

   setcatok("×F",   ONESYLL, 004, 0);
   setcatok("×DF",   ONESYLL, 005, 0);
   setcatok("×+F",   ONESYLL, 007, 0);
   setcatok("×D",   ONESYLL, 034, 0);
   setcatok("×",   ONESYLL, 035, 0);
   setcatok("×+", SHIFT, 0163, 0);
   setcatok("×+C", ONEQ, 0163, 0);

   setcatok("-DF",   ONESYLL, 022, 0);
   setcatok("+DF",   ONESYLL, 023, 0);
   setcatok("+",   ONESYLL, 056, 0);
   setcatok("+D",   ONESYLL, 057, 0);
   setcatok("+F",   ONESYLL, 074, 0);
   setcatok("-F",   ONESYLL, 075, 0);

   setcatok("÷I",   ONESYLL, 044, 0);
   setcatok("÷",   ONESYLL, 060, 0);
   setcatok("÷D",   ONESYLL, 061, 0);
   setcatok("÷F",   ONESYLL, 062, 0);
   setcatok("÷DF",   ONESYLL, 063, 0);
   setcatok("÷R",   ONESYLL, 064, 0);

   setcatok("FNSC", LINK, 0176, 001);   //   K7;
   setcatok("FPHU", LINK, 0176, 004);   //   K5;
   setcatok("FRFIS", LINK, 0176, 010);   //   K4;
   setcatok("SBANOL", LINK, 0175, 0100);   //   =K1;
   setcatok("SBUZZ", LINK, 0175, 0200);   //   =K0;
   setcatok("SCPDAR", LINK, 0175, 040);   //   =K2;
   setcatok("SNSC", LINK, 0175, 020);   //   =K3;

}

