/* lexical and code generation sections of a KDF9 assembler
   parsing is done by yacc in companion file with .y extension
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>

#include "y.tab.h"

#ifndef YYDEBUG
int yydebug;
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

#define MAXP 3000
#define MAXLAB 1224
#define GUTTER 70

int verbose = 0;
unsigned char vstoreval[7]; // newly evaluated V-store contents are put here -- plus a guard digit for rounding fractions

char prtline[1000]; // line for storing output of binary progam to listing
int prtpos = 0;     // position of RH end of printing line

int memaddr;

int numvstores[MAXP];    // number of V-stores in each routine
int *lablist[MAXP];      // addresses of labels in each routine
int maxlabno[MAXP];      // largest label number each routine
int ystorebase = 0;      // address of Y0 as calculated from size of code -- actually W0
int y0spec = -1;         // address of Y0 as specified by Y0=Edddd
int currentlabs[MAXLAB]; // accumulates label addresses in the current routine
int currentRoutine = 0;  // number of current P routine
int currentmaxlab = 0;   // maximum label in current routine
int p0nv = 0;            // number of V-stores in main program
int p0nw = 0;            // number of W-stores
int numystores[] =       // number of Y-stores, YA-stores, YB-stores, YC-stores, etc
    {32767, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
int baseystores[] = // addresses of base of above - calculated at end of first pass
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

extern int yydebug;

int listing = 1;
int error_seen = 0;
int lineCount = 0;
int firstpass = 1;
int endsrc = 0;
char *srcmarker; // set by yylex to mark the start of a sequence of digits
int nexch = 0;   // really local to yylex() but modified by setprinterconst
unsigned char *buffp, *startLine;

unsigned char progname[12];
unsigned char nonprtcode[128], prtcode[128];
unsigned char ptchar[64] // 8-bit paper tape chars for each 6-bit character
    = {0x90, 0x11, 0x12, 0x03, 0x14, 0x05, 0x06, 0x17,
       0x18, 0x09, 0x0A, 0x1B, 0x0C, 0x1D, 0x1E, 0x0F,
       0x30, 0x21, 0x22, 0x33, 0x24, 0x35, 0x36, 0x37,
       0x28, 0x39, 0x3A, 0x2B, 0x3C, 0x2D, 0x2E, 0x3F,
       0x50, 0x41, 0x42, 0x53, 0x44, 0x55, 0x56, 0x47,
       0x48, 0x59, 0x5A, 0x4B, 0x5C, 0x4D, 0x4E, 0x5F,
       0x60, 0x71, 0x72, 0x63, 0x74, 0x65, 0x66, 0x67,
       0x78, 0x69, 0x6A, 0x7B, 0x6C, 0x7D, 0x7E, 0x6F};

unsigned char *buff;               // buffer for input -- allocated by loadfile
int endbuff;                       // start of work area in buffer for dealing with subscript 10
unsigned char corestore[8193 * 6]; // The syllables of KDF9 object code + (6 spares for overflow detection ??)

/* code generator routines used in main() */
extern void storejump(int s1, int s2, int loc);
extern int codeloc(int pno, int labno);

void loadfile(char *fn)
{
   int fin = open(fn, O_RDONLY + O_BINARY);
   int file_size, n;
   struct stat file_gen;

   if (fstat(fin, &file_gen) < 0) /* this should be impossible */
      perror("fstat to get size");
   file_size = file_gen.st_size;
   buff = (char *)malloc(file_size + 123);

   n = read(fin, buff + 2, file_size);
   memset(buff + n, ')', 10);    // try to guarantee that we do not run off the end
   memset(buff + n + 10, 0, 10); // try to guarantee that we do not run off the end
   close(fin);
   printf("Loaded %d bytes for %s\n", n, fn);
   buffp = buff;
   *buffp++ = '\n'; // ensure that we do not run off the front wiht invalid input
   *buffp++ = '\n';
   endbuff = n + 23;
}

int yylex()
/* yacc's lexical analyser.
 */
{
   int res;

   // if  ( buff[0] != 'V' )
   // {  printf("Memory corruption %o\n", buff[0]);
   //    exit(1);
   // }

   if (listing)
   {
      while (nexch <= ' ')
      {
         if (nexch == '\n')
         {
            if (prtpos >= GUTTER) // too much code for side-by-side printing
            {
               printf("%s\n", prtline);
               prtpos = 0;
            }
            memset(prtline + prtpos, ' ', GUTTER - prtpos);
            prtline[GUTTER] = 0;
            res = *(buffp - 1);
            *(buffp - 1) = 0;
            printf("%s %s\n", prtline, startLine);
            *(buffp - 1) = res;
            startLine = buffp;
            lineCount++;
            prtpos = 0;
         }
         nexch = *(buffp++);
      }
   }
   else
      while (nexch <= ' ')
         nexch = *(buffp++);

   if (endsrc)
   {
      nexch = 0; // stop false first read
      return 0;  // end of file
   }

   if (isdigit(nexch))
   {
      srcmarker = buffp; // used in evaluation of constants of various sorts
      res = nexch - '0';
      while (isdigit(nexch = *(buffp++)) || nexch == ' ')
         if (nexch != ' ')
            res = res * 10 + nexch - '0';
      yylval = res;
      return UNSIGNED_INTEGER;
   }
   else if (nexch == '(') // comment
   {
      res = 0;
      while (res >= 0)
         if ((nexch = *(buffp++)) == ')')
            res--;
         else if (nexch == '(')
            res++;
      nexch = *(buffp++);
      return COMMENT;
   }
   else if (nexch == '[') // 21st century comment -- i.e. inserted in 21st century
   {
      if (!firstpass && memcmp(buffp, "????", 4) == 0) // facility for turning on diagnostics
      {
         printf("?????????????? setting verbose at line %d\n", lineCount);
         verbose = 1;
      }
      res = 0;
      while ((nexch = *(buffp++)) != '\n')
         ; // skip to end of line
      return COMMENT;
   }
   else
   {
      res = nexch;
      nexch = *(buffp++);
      if (res >= 'a' && res <= 'z')
         res -= 'a' - 'A';
      return res;
   }
}

yyerror(char *s)
{
   int w;
   char *p = buffp;
   int limit = 12;
   fprintf(stderr, "Error %s\n", s);
   error_seen++;

   while (*++p != '\n')
      ; // find end of current line
   *p = 0;

   p = buffp;
   while (*--p != '\n')
      ; // find start of current line
   while (*--p != '\n')
      ; // and the one before
   w = *buffp;
   *buffp = 0;
   printf("\nError %s\n------------------------%s!!!%c%s\n", s, p, w, buffp + 1);

   exit(1);

   w = yylex();
   if (w >= 256)
      printf("  %d  ->  %d\n", w, yylval);
   else
      printf("%c %d \n", w, w);
   w = yylex();
   if (w >= 256)
      printf("  %d  ->  %d\n", w, yylval);
   else
      printf("%c %d \n", w, w);
   w = yylex();
   if (w >= 256)
      printf("  %d  ->  %d\n", w, yylval);
   else
      printf("%c %d \n", w, w);
   w = yylex();
   if (w >= 256)
      printf("  %d  ->  %d\n", w, yylval);
   else
      printf("%c %d \n", w, w);
   exit(1);
}

void setprinterchars()
{
   int i;
   for (i = '0'; i <= '9'; i++)
   {
      prtcode[i] = i - '0' + 020;     // decimal digits
      nonprtcode[i] = i - '0' + 0140; // decimal digits as repeat counts
   }
   for (i = 'A'; i <= 'Z'; i++)
      prtcode[i] = i + 041 - 'A'; // uppercase letters

   prtcode['%'] = 06;
   prtcode['\''] = 07; // 07 '
   prtcode[':'] = 010;
   prtcode['='] = 011;
   prtcode['('] = 012;
   prtcode[')'] = 013;

   nonprtcode['['] = '['; // for switching between sets
   prtcode[']'] = ']';

   nonprtcode['*'] = 000;
   nonprtcode['C'] = 002;
   nonprtcode['P'] = 003;
   nonprtcode['T'] = 004;
   nonprtcode['N'] = 007;
   nonprtcode['S'] = 006;
   nonprtcode['D'] = 077;
   nonprtcode['Q'] = 034;
   nonprtcode['M'] = 075;
   prtcode['*'] = 015;
   prtcode[','] = 016;
   prtcode['/'] = 017;
   prtcode['#'] = 033; // subscript 10 - what is this in Hans's code
   prtcode['+'] = 035;
   prtcode['-'] = 036;
   prtcode['.'] = 037;
   prtcode['$'] = 014; // 14 dd &pound;
   prtcode['#'] = 000; // space
   prtcode[' '] = 65;  // space is ignored in the printer constant

   // 75 dd EM
   // 76 dd start message
   // 77 dd ignored
}

void writeprogname(int dv, int n)
/* writes out the program name in A-block and B-block format */
{
   int i;
   buff[0] = ptchar[7];
   buff[1] = ptchar[2];
   buff[2] = ptchar[060]; // letter P
   buff[3] = ptchar[2];
   for (i = 0; i < 12; i++)
      buff[i + 4] = progname[i];
   write(dv, buff, n);
}

void ptwrite(int dv, int start, int stop)
// write out a block of store upto but not including stop
{
   int i = start * 6;
   int j = stop * 6;
   int k = 0;
   while (i < j)
   {
      buff[k++] = ptchar[(corestore[i] >> 2) & 63]; // use the input buffer as output area
      buff[k++] = ptchar[((corestore[i] << 4) + (corestore[i + 1] >> 4)) & 63];
      buff[k++] = ptchar[((corestore[i + 1] << 2) + (corestore[i + 2] >> 6)) & 63];
      buff[k++] = ptchar[corestore[i + 2] & 63];
      i += 3;
   }
   write(dv, buff, k);
}

void ptbinout()
// output a KDF9 paper tape binary program
// not yet complete -- needs A-block and program identifier
{
   int fout = open("a.out", O_CREAT + O_TRUNC + O_WRONLY, 0640);
   if (fout < 0)
      perror("a.out");
   else
   {
      buff[16] = ptchar[075]; // end-message
      buff[17] = 0;           // blank tape
      buff[18] = 0;           // blank tape
      buff[19] = 0;           // blank tape
      writeprogname(fout, 20);
      corestore[45] = 8;                      // filler word in B-block
      corestore[46] = (ystorebase - 1) >> 8;  // filler word in B-block
      corestore[47] = (ystorebase - 1) & 255; // filler word in B-block
      ptwrite(fout, 0, 2);                    // B-block
      writeprogname(fout, 16);
      ptwrite(fout, 4, 8);          // B-block continued
      ptwrite(fout, 8, ystorebase); // C-block
      close(fout);
   }
}

void mtbinout()
// output a KDF9 mag tape B-block and C-block
// must be called after ptbinout()
{
   int fout = open("mt.out", O_CREAT + O_TRUNC + O_WRONLY + O_BINARY, 0640);
   int i;
   int prn[4]; // program name in KDF9 chars
   if (fout < 0)
      perror("mt.out");
   else
   {
      prn[0] = 07025502; // load up words 2 and 3
      prn[1] = (progname[0] << 18) + (progname[1] << 12) + (progname[2] << 6) + progname[3];
      prn[2] = (progname[4] << 18) + (progname[5] << 12) + (progname[6] << 6) + progname[7];
      prn[3] = (progname[8] << 18) + (progname[9] << 12) + (progname[10] << 6) + progname[11];
      for (i = 0; i < 4; i++)
      {
         corestore[3 * i + 12] = (prn[i] >> 16) & 255;
         corestore[3 * i + 13] = (prn[i] >> 8) & 255;
         corestore[3 * i + 14] = prn[i] & 255;
      }
      write(fout, corestore, 48);                  // 8 word B-block
      write(fout, corestore + 48, ystorebase * 6); // C-block + a bit ????
      close(fout);
   }
}

main(int argc, char **argv)
{
   int i;

   if (yydebug)
      printf("Started\n\n");

   loadfile(argv[1]);
   listing = argc - 2;

   for (i = 0; i < 128; i++)
   {
      nonprtcode[i] = 64; // invalid by default
      prtcode[i] = 64;    // invalid by default
   }
   setprinterchars();
   for (i = 0; i < 48; i++)
      corestore[i] = 0; // zeroise B-block

   startLine = buffp;
   currentRoutine = 0;
   for (i = 1; i < MAXLAB; i++)
      currentlabs[i] = -1;

   for (i = 1; i < MAXP; i++)
   {
      numvstores[i] = -1; // number of V-stores in each routine
      lablist[i] = NULL;  // addresses of labels in each routine
   }
   numvstores[0] = 0;        // number of V-stores in main program
   lablist[0] = currentlabs; // vector for collecting addresses
   memaddr = 24;             // ready for possible restart code
   yyparse();
   printf("\n\n=============== Second pass\n");
   // printf(buff);
   printf("===========================\n");
   prtpos = 0;
   firstpass = 0;
   startLine = buffp;
   currentRoutine = 0;
   listing = 1;

   printf("\n\n%7o W0 = E%d\n", ystorebase, ystorebase);
   printf("%7o Y0 = E%d\n", baseystores[0], baseystores[0]);
   for (i = 1; i < 26; i++)
      if (numystores[i] != 0)
         printf("%7o Y%c0 = E%d\n", baseystores[i], i + ('A' - 1), baseystores[i]);

   memaddr = 0; // ready to plant initial jump
   storejump(0200, 0260, codeloc(-1, 0));
   memaddr = 24; // ready for possible restart code
   buffp = buff;
   startLine = buffp;
   endsrc = 0;
   nexch = 0; // stop false first read
   yyparse(); // second pass
   srcmarker = argv[1];
   for (i = 0; i < 9; i++)
      progname[i] = prtcode[srcmarker[i]];
   progname[11] = (progname[9] = prtcode['U']);
   progname[10] = prtcode['P'];
   ptbinout();
   mtbinout();
}

char *lstpos(int memaddr, int nsylls)
// sets address for printing instructions, and updates prtpos
{
   char *res;
   if (prtpos == 0) // start of new line
   {
      sprintf(prtline, "%5o/%o: ", memaddr / 6, memaddr % 6);
      prtpos = 9;
   }
   else if (*prtline == 'V') // must be V-store spec and code on same line
   {
      printf("%s\n", prtline); // so print out V-store value
      sprintf(prtline, "%5o/%o: ", memaddr / 6, memaddr % 6);
      prtpos = 9;
   }
   else
      prtline[prtpos++] = '|';
   res = prtline + prtpos;
   prtpos += nsylls * 3 + nsylls - 1;
   return res;
}

void listvstore(int loc, unsigned char *vstoreval)
/* puts assembled value of V-store in programm listing */
{
   if (listing)
   {
      if (prtpos != 0) // must be multiple V-store spec and/or code on same line
         printf("%s\n", prtline);
      sprintf(prtline, "V-store at %05o = %03o %03o %03o %03o %03o %03o  ", loc / 6,
              vstoreval[0], vstoreval[1], vstoreval[2], vstoreval[3], vstoreval[4], vstoreval[5]);
      prtpos = 43;
   }
}

void storesyl(int p1)
/* sstore single syllable instruction p1 */
{
   if (listing)
      sprintf(lstpos(memaddr, 1), "%03o", p1);
   corestore[memaddr++] = p1;
}

void store2syl(int p1)
/* sstore 2-syllable instruction p1 */
{
   if (listing)
      sprintf(lstpos(memaddr, 2), "%03o %03o", (p1 >> 8) & 255, p1 & 255);
   corestore[memaddr++] = p1 >> 8;
   corestore[memaddr++] = p1;
}

void qnum(int q)
/* confirms that q is a valid Q-store number */
{
   if (q >= 16)
      yyerror("Q-store number > 15");
}

int twosylqx(int instr, int q1, int q2)
/* evaluate two syllable Q-store instruction with 2 Q-store numbers in it
   or one Q-store number and a specified lower byte  */
{
   if (q1 >= 16 || q2 >= 16)
      yyerror("Q-store number > 15");
   return (instr << 8) | (q1 << 4) | q2;
}

int twosylshetc(int i0, int n)
/* evaluate two syllable instruction such as a shift */
{
   return (i0 << 8) | n;
}

void storejump(int s1, int s2, int loc)
/* store a jump to specified location */
{
   int syl = loc % 6;
   int word = loc / 6;
   s1 += ((word >> 9) & 010) + syl;
   s2 += ((word >> 8) & 017);
   if (listing)
      sprintf(lstpos(memaddr, 3), "%03o %03o %03o", s1, s2, word & 255);
   corestore[memaddr++] = s1;
   corestore[memaddr++] = s2;
   corestore[memaddr++] = word;
}

void reserveStore(int p1, int p2)
/* reserve store of type p1 for p2 words */
{
   if (firstpass)
   {
      if (p1 == 'V')
         numvstores[0] = (p0nv = p2 + 1);
      else if (p1 == 'W')
         p0nw = p2 + 1;
      else if ((p1 & 255) == 'Y')
      {
         int s = (p1 >> 8) & 31; // letter A gives 1, etc
         numystores[s] = p2 + 1;
      }
   }
   // printf("reserved %c %d %d\n", p1, p1>>8, p2);
}

void setlabel(int labno)
/* record integer label */
{
   if (labno >= MAXLAB)
      printf("*** label %d out of range on line %d\n", labno, lineCount);
   else if (firstpass)
   {
      if (currentlabs[labno] >= 0) /* duplicate label */
      {
         printf("*** label %d at address %d on line %d is a duplicate\n", labno, memaddr, lineCount);
         yyerror("Duplicate label");
      }
      // printf("*** label %d at address %d on line %d\n", labno, memaddr, lineCount);
      currentlabs[labno] = memaddr;
      if (labno >= currentmaxlab)
         currentmaxlab = labno;
   }
   else if (lablist[currentRoutine][labno] != memaddr) // check on second pass
      printf("*** label %d at %d does not match %d\n", labno, memaddr, lablist[currentRoutine][labno]);
}

void store3syl(int instr, int addr)
/* takes the output from data loc and embeds in the instruction */
{
   int s1 = ((addr >> 13) & 070) + instr;
   int s2 = (addr >> 8) & 255;
   int s3 = addr & 255;
   if (instr == 0304 && addr >= 010000) // set instruction for big address TEMP !!! ???
   {
      s1 = 0304;                 // still need to deal with syllable addresses in SETARn
      s2 += (addr >> 12) & 0160; // put top address bits where they belong
   }
   // printf("Raw address is %X\n", addr);
   if (listing)
      sprintf(lstpos(memaddr, 3), "%03o %03o %03o", s1, s2, s3);
   corestore[memaddr++] = s1;
   corestore[memaddr++] = s2;
   corestore[memaddr++] = s3;
}

int dataloc(int pno, int vno)
/* get data location as word address for a V-store */
{
   if (!firstpass)
   {
      int word, top3;
      if (pno < 0)
         pno = currentRoutine;
      if (pno < MAXP && lablist[pno] != NULL)
      {
         word = lablist[pno][0] / 6 - numvstores[pno] + vno;
         if (vno >= numvstores[pno])
            printf("Warning: V%dP%d is out of range, only %d V-stores specified\n", vno, pno, numvstores[pno]);
      }
      else
      {
         word = 0;
         printf("*** There is no routine P%d\n", pno);
      }
      // printf("Calculated address %d for V%dP%d\n", word, vno, pno);
      top3 = word & 070000;
      // printf("Returning %X\n", (top3<<4) + (word&07777));
      return (top3 << 4) + (word & 07777);
   }
   else
      return 0;
}

int ystoreloc(int yset, int yno)
/* get ystore location
   yset will be the letter for YA YB etc
 */
{
   int res = 0;
   if (!firstpass)
   {
      int word, top3;
      if (yset < 0) /* W-stores come first in the data memory area */
         word = ystorebase + yno;
      else
         word = baseystores[yset] + yno;
      top3 = word & 070000;
      // printf("Calculated address %d for Y%d\n", word, yno);
      res = (top3 << 4) + (word & 07777);
   }
   if (yset < 0 && yno >= p0nw)
      printf("Warning: W%d is out of range, only %d W-stores specified\n", yno, p0nw);
   else if (yset > 0 && yno >= numystores[yset])
      printf("Warning: Y%c%d is out of range, only %d Y%c-stores specified\n", 'A' - 1 + yset, yno, numystores[yset], 'A' - 1 + yset);
   return res;
}

int codeloc(int pno, int labno)
/* evaluate location in the code -- result is syllable address */
{
   int addr = 0;

   if (pno < 0)
      pno = currentRoutine;
   // printf("accessing R%dP%d\n", labno, pno);
   if (pno >= MAXP)
      printf("*** P-routine P%d exceeds max allowed P%d\n", pno, MAXP - 1);
   else if (!firstpass) // sort out addresses on second pass
   {
      int *ll = lablist[pno];
      if (ll == NULL)
         printf("*** There is no routine P%d\n", pno);
      else if (labno >= maxlabno[pno] || (addr = ll[labno]) < 0)
         printf("*** There is no label %d in routine P%d\n", labno, pno);
   }
   return addr;
}

void newproutine(int pno, int v)
/* entering a new P-routine */
{
   int previous = currentRoutine;
   currentRoutine = pno;
   memaddr = ((memaddr + 5) / 6 + v + 1) * 6;
   if (pno >= MAXP)
      printf("*** Routine number greater than %d P%dV%d; at address %d\n", MAXP, pno, v, memaddr / 6);
   else
   {
      if (firstpass)
      {
         int *ll = (int *)malloc((++currentmaxlab) * sizeof(int));
         int i;
         for (i = 0; i < currentmaxlab; i++)
         {
            ll[i] = currentlabs[i];
            currentlabs[i] = -1;
         }
         lablist[previous] = ll;
         maxlabno[previous] = currentmaxlab;
         currentlabs[0] = memaddr; // set default routine entry point
      }
      else
         printf("=== Start P%dV%d; at address %d\n", pno, v, memaddr / 6);

      currentmaxlab = 0;
      if (pno < 0) // FINISH;
      {
         ystorebase = (memaddr + 5) / 6;
         endsrc = 1;
         if (firstpass) /* need to compute the base of the W- and Y-store arrays */
         {
            int yyy = ystorebase + p0nw;
            int i;
            baseystores[1] = yyy;
            for (i = 1; i < 26; i++)
               baseystores[i + 1] = (yyy += numystores[i]);
            baseystores[0] = yyy + numystores[26];
            if (y0spec >= 0) // absolute location of Y0 is specified
            {
               int reloc = y0spec - baseystores[0];
               if (reloc < 0)
                  printf("*** Code overlaps Y-stores by %d words\n", -reloc);
               else
                  printf("Specifying Y0 leaves gap of %d words\n", reloc);
               ystorebase += reloc;
               for (i = 0; i < 26; i++)
                  baseystores[i] += reloc;
            }
         }
      }
      else
         numvstores[pno] = v + 1;
   }
}

void endBblock()
/* called at end of B-block to create first 8 words of store
   and set pointers for main code generation
 */
{
   memaddr = (p0nv + 8) * 6; // syllable address of start of code
   currentlabs[0] = memaddr; // set default routine entry point
   // printf("Start main program\n");
}

int keepMarker()
/* uses srcmarker to deliver a pointer
   to the first char of a sequence of digits
 */
{
   return (((int)srcmarker) - ((int)buff)) - 2; // delivers value of the src pointer for the parser stack
   // return (srcmarker - buff) - 2;         // amazingly gcc will not compile this
}

int octalval(int p)
/* delivers an octal value from the pointer p
 */
{
   int res = 0;
   char *pp = buff + p;
   while (isdigit(*++pp))
      res = (res << 3) + *pp - '0';
   return res;
}

int addindigit(int d, int r)
/* adds in a digit to vstoreval with radix r */
/* returns last carry digit, which will be 0 unless the value has overflowed */
{
   int i = 6;
   int v;
   d -= '0';
   while (--i >= 0)
   {
      v = vstoreval[i] * r + d;
      vstoreval[i] = v & 255;
      d = v >> 8;
   }
   return d;
}

void addinfractiondigit(int d, unsigned char *f)
/* adds in a digit to vstoreval multiplied by the fraction f */
{
   int i = 7;
   int c = 0;
   int v;
   if (verbose)
      printf("Adding digit %c at weight %03o %03o %03o %03o %03o %03o %03o\n", d, f[0], f[1], f[2], f[3], f[4], f[5], f[6]);
   d -= '0';
   while (--i >= 0)
   {
      v = vstoreval[i] + d * f[i] + c;
      vstoreval[i] = v & 255;
      c = v >> 8;
   }
}

void vfraction(int p, int sig)
/* evaluates a decimal number and loads result into 56-bit vstoreval array */
/* result is shifted so that there are sig integral places */
{
   if (sig >= 48)
      printf("*** %d significant digits is too many\n", sig);
   else if (!firstpass)
   {
      unsigned char *pp = buff + p;
      int vr = 0; // 'overflow' register
      memset(vstoreval, 0, 7);
      while (isdigit(*++pp) || *pp == ' ')
         if (*pp != ' ')
            vr += addindigit(*pp, 10);
      if (sig != 47 || *pp == '.') // allow for V0 = 234.6; etc - silly but valid
      {
         int s = 47 - sig; // amount to shift left
         int x = s & 7;    // extra shift after byte move
         {
            int j = 0;
            s = s >> 3; // number of bytes to shift
            while (s < 6)
               vstoreval[j++] = vstoreval[s++];
            while (j < 6)
               vstoreval[j++] = 0;
         }
         if (x != 0)                             // not multiple of 8 -- need to shuffle up a bit
            vr += addindigit('0', 2 << (x - 1)); // lazy, but efficiency is not a concern here

         if (*pp == '.') // if there is a fraction part
         {
            unsigned char fpart[7]; // starts out as 1.0 correctly aligned
            memset(fpart, 0, 7);
            sig = 5 - ((47 - sig) >> 3); // position of units digit in result
            if (sig >= 0)
               fpart[sig] = 1 << x; // 1.0 aligned in fpart
            else                    // must be a small number
               fpart[0] = 1 << x;   // 1.0 aligned max left -- realigned later
                                    // printf("!!!! sig = %d %c%c%c%c%c\n", sig, pp[0], pp[1], pp[2], pp[3], pp[4]);
                                    // printf("fpart = %03o %03o %03o %03o %03o %03o %03o\n",
                                    //                      fpart[0], fpart[1], fpart[2], fpart[3], fpart[4], fpart[5], fpart[6]);
            while (isdigit(*++pp) || *pp == ' ')
               if (*pp != ' ')
               {
                  int j;
                  s = 0; // next divide fpart by 10
                  for (j = 0; j < 7; j++)
                  {
                     s = (fpart[j] + s * 256);
                     fpart[j] = s / 10;
                     s = s % 10;
                  }
                  if (sig < 0 && fpart[0] == 0) // if OK to move up significance
                  {
                     sig++;
                     for (j = 0; j < 6; j++)
                        fpart[j] = fpart[j + 1];
                     fpart[6] = 0;
                     // printf("char = %c  fpart = %03o %03o %03o %03o %03o %03o %03o\n", *pp,
                     //             fpart[0], fpart[1], fpart[2], fpart[3], fpart[4], fpart[5], fpart[6]);
                  }
                  if (*pp != '0')
                  {
                     if (sig < 0) // non-zero digit encountered while scale factor out of range
                        vr++;     // overflow
                     else
                        addinfractiondigit(*pp, fpart); // add in good digit
                  }
               }
            if ((vstoreval[6] & 128) != 0) // if we need round up
               vr += addindigit('1', 1);   // lazy, but efficiency is not a concern here
         }
      }
      if (vr != 0)
         printf("*** V-store value overflows\n");
   }
   if (verbose)
      printf("Evaluated %03o %03o %03o %03o %03o %03o %03o\n",
             vstoreval[0], vstoreval[1], vstoreval[2], vstoreval[3], vstoreval[4], vstoreval[5], vstoreval[6]);
}

int negatevstore()
/* negates the vstore value deposited by vfraction, etc */
/* returns 0 if V-store vale was zero -- used for F-0.0 */
/* N.B. this has already been rounded to 48 bits by vfraction */
{
   int i = 6;
   int v;
   int c = 0;  // carry digit
   int zz = 0; // tester for zero
   while (--i >= 0)
   {
      v = -vstoreval[i] - c;
      zz += (vstoreval[i] = v & 255);
      c = (v >> 8) & 1;
   }
   return zz;
}

int sig; // floating point exponent -- see below

void vfloat(int p, int exp)
/* evalues floating point V-store value */
/* p points at digit string, and exp is the value following subscript 10 */
/* The value is assembled into vstoreval to 39 integral places, ready for conversion to floating point by vfppos or vfpneg */
{
   if (!firstpass)
   {
      char *pp = buff + p;
      int ipart = 0;
      int dcount = 0;
      sig = 8;                             // number of significant bits needed to accommodate the integer part ...
      while (isdigit(*++pp) || *pp == ' ') // ... at the right point in the word for FP conversion
         if (*pp != ' ')
         {
            ipart = ipart * 10 + *pp - '0';
            dcount++;
         }
      if (exp != 0)
      {
         int q = endbuff;
         pp = buff + p;
         while (dcount + exp > 0) // there are significant digits to the left of the decimal point
            if (isdigit(*++pp))
            {
               dcount--;
               buff[q++] = *pp;
            }
         buff[q++] = '.';         // put in the decimal point
         while (dcount + exp < 0) // we need zeroes after the decimal point
         {
            dcount++;
            buff[q++] = '0';
         }
         while (isdigit(*++pp) || *pp == ' ' || *pp == '.')
            if (isdigit(*pp))
               buff[q++] = *pp; // copy the rest of the number
         buff[q++] = ';';       // terminate the number
         buff[q++] = 0;         // terminate the string
                                // printf("V-store equivalent number is %s\n", buff + endbuff);
         vfloat(endbuff - 1, 0);
         return;
      }
      if (ipart != 0)
      {
         while (ipart != 0) // find out what precision we need
         {
            sig++;
            ipart = ipart >> 1;
         }
      }
      else if (*pp == '.') // number is less than one
      {
         int zc = 0; // count of leading zeros
                     // printf("Small number 0.%c%c%c\n", pp[1],pp[2], pp[3]);
         while (*++pp == '0')
            zc++;
         sig -= zc * 3; // allow 3 bits for each decimal digit ...
         sig -= zc / 3; // ... but assume 1000 needs 10 bits
      }
      if (verbose)
         printf("Using %d significant digits\n", sig);
      vfraction(p, sig); // assemble number as fixed point to maximum possible significance
      if (verbose)
         printf("vstoreval = %03o %03o %03o %03o %03o %03o %03o\n",
                vstoreval[0], vstoreval[1], vstoreval[2], vstoreval[3], vstoreval[4], vstoreval[5], vstoreval[6]);
   }
}

void vfppos()
/* stores positive floating point V-store value that is already assembled in fixed point form -- see above */
{
   int w = sig + 120;
   int zz = 0;                     // will be zero if number was exactly zero
   if ((vstoreval[1] & 0100) == 0) // we need to shift 1 left to standardise - or it might be zero
   {
      int c = 0;
      int i = 6;
      w--; // also shrink the exponent
      while (--i > 0)
      {
         int v = (vstoreval[i] << 1) + c; // shifted up one place
         c = (v >> 8) & 1;
         vstoreval[i] = v & 255; // and carry suitably propagated
         zz += v;
      }
      if (zz == 0)
         return; // just leave the all zeroes in vstoreval
   }
   vstoreval[0] = w >> 1;
   if ((w & 1) != 0)
      vstoreval[1] |= 128; // bottom bit of exponent
}

void vfpneg()
/* stores negative floating point V-store value -- see above */
{
   int w = sig + 120;
   if (negatevstore() == 0)           // negate and test for zero
      return;                         // leaves zero result in vstoreval
   while ((vstoreval[1] & 0100) != 0) // we need to shift 1 left to standardise -- or maybe 2??
   {
      int c = 0;
      int i = 6;
      w--; // also shrink the exponent
      while (--i > 0)
      {
         int v = (vstoreval[i] << 1) + c; // shifted up one place
         c = (v >> 8) & 1;
         vstoreval[i] = v & 255; // and carry suitably propagated
      }
   }
   vstoreval[0] = (w >> 1) + 128; // force in the sign digit
   if ((w & 1) == 0)
      vstoreval[1] &= 127; // bottom bit of exponent cleared to 0
}

void voctal(int p, int sig)
/* ditto octal */
{
   if (!firstpass)
   {
      char *pp = buff + p;
      memset(vstoreval, 0, 6);
      if (verbose)
         printf("enter voctal(%d, %d)\n", p, sig);

      while (isdigit(*++pp) || *pp == ' ')
         if (*pp != ' ')
            addindigit(*pp, 8);
      if (sig != 47)
      {
         int s = 47 - sig; // amount to shift left
         int x = s & 7;    // extra shift after byte move
         {
            int j = 0;
            s = s >> 3; // number of bytes to shift
            while (s < 6)
               vstoreval[j++] = vstoreval[s++];
            while (j < 6)
               vstoreval[j++] = 0;
         }

         if (x != 0)                       // not multiple of 8 -- need to shuffle up a bit
            addindigit('0', 2 << (x - 1)); // lazy, but efficiency is not a concern here
      }
      if (verbose)
         printf("leave voctal(%d, %d)\n", p, sig);
   }
}

void vqformat(p1, p2, p3)
/* constructs a Q-store format V-store value into vstoreval */
{
   if (!firstpass)
   {
      vstoreval[0] = (p1 >> 8) & 255;
      vstoreval[1] = p1 & 255;
      vstoreval[2] = (p2 >> 8) & 255;
      vstoreval[3] = p2 & 255;
      vstoreval[4] = (p3 >> 8) & 255;
      vstoreval[5] = p3 & 255;
   }
}

void setvstore(int vno)
/* sets give V-store from value constructed in vstoreval */
{
   if (!firstpass)
   {
      int loc = dataloc(-1, vno);
      int i = 6;
      if (verbose)
         printf("Setting V%d in location %d\n", vno, loc);
      if (loc >= 010000) // top bit needs to be put back where it belongs
         loc = (loc & 07777) + 010000;
      loc *= 6;
      while (--i >= 0)
         corestore[loc + i] = vstoreval[i];
      listvstore(loc, vstoreval);
      // printf("V-store at %05o = %03o %03o %03o %03o %03o %03o\n", loc/6,
      //              vstoreval[0], vstoreval[1], vstoreval[2], vstoreval[3], vstoreval[4], vstoreval[5]);
   }
}

void setprinterconst(int v1, int v2)
/* sets a printer constant in a range of V-stores -- involves lexical cheating */
{
   char *pp = buffp; // position of the letter P or just after
                     // printf("Setting V%d/%d in P%d  -- nex3ch = [%c%c%c] (nexch=%d)\n", v1, v2, currentRoutine,
                     //              *(buffp-1), *(buffp), *(buffp+1), nexch);
   while (*++buffp != ';')
      ; // find end of printer constant
   if (nexch != 'P')
      nexch = 0; // forces skip to next character, i.e. ;

   if (!firstpass)
   {
      int loc1 = dataloc(-1, v1) * 6;
      int loc2 = loc1 + (v2 - v1) * 6;
      int n = (v2 - v1 + 1) * 8; // number of KDF9 characters in the range of V-stores
      char *vch = (char *)malloc(n + n);
      char *p, ch;
      char *code = prtcode;
      memset(vch, 0, n + n); // extra zeros cause zero right padding
      p = buffp;             // points at the terminating semi-colon
      while (n >= 0 && p > pp)
         if ((ch = code[*--p & 127]) == ']')
            code = nonprtcode;
         else if (ch == '[')
            code = prtcode;
         else if (ch == 64) // unrecognised
            yyerror("Bad printer constant - unrecognised character");
         else if (ch > 0140) // repeat count
         {
            char w = vch[n]; // last character deposited
            while (--ch >= 0141)
               vch[--n] = w;
         }
         else if (ch != 65) // not an actual space in the source code
         {
            vch[--n] = ch;      // right justify the chars
            if (ch == 075)      // end-message abbreviates to [EM] -- sees the M
               if (*--p != 'E') // skip the E
                  yyerror("Bad printer constant - unrecognised character");
         }

      // n = 0;                            // enable this if right-justification needed, but think it should be left
      while (loc1 <= loc2)
      {
         corestore[loc1] = (vch[n] << 2) + (vch[n + 1] >> 4);
         corestore[loc1 + 1] = (vch[n + 1] << 4) + (vch[n + 2] >> 2);
         corestore[loc1 + 2] = (vch[n + 2] << 6) + vch[n + 3];
         corestore[loc1 + 3] = (vch[n + 4] << 2) + (vch[n + 5] >> 4);
         corestore[loc1 + 4] = (vch[n + 5] << 4) + (vch[n + 6] >> 2);
         corestore[loc1 + 5] = (vch[n + 6] << 6) + vch[n + 7];

         listvstore(loc1, corestore + loc1);
         // printf("V-store at %05o = %03o %03o %03o %03o %03o %03o\n", loc1/6,
         //              corestore[loc1], corestore[loc1+1], corestore[loc1+2], corestore[loc1+3],
         //                            corestore[loc1+4], corestore[loc1+5]);
         loc1 += 6;
         n += 8;
      }
   }
}

void startnewword()
/* inserts dummy instructions up to word boundary */
{
   while (memaddr % 6 != 0)
      storesyl(017);
}

void fixY0(int zero, int zero2, int loc)
/* locates Y0 at specified absolute address, e.g. Y0=E 4896 */
{
   if (firstpass)
   {
      if (zero != 0 || zero2 != 0)
      {
         if (zero == 0)
            zero = ' ';
         else
            zero += 'A' - 1;
         printf("*** Specifying Y%c%d not supported\n", zero, zero2);
      }
      y0spec = loc;
   }
}

void Bblock(int p1, int p2)
/* inserts store or time limit into B-block */
{
   int loc = 6;
   if (p1 == 'S')
      loc = 9;
   corestore[loc] = p2 >> 16;
   corestore[loc + 1] = (p2 >> 8) & 255;
   corestore[loc + 1] = p2 & 255;
}
