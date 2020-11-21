// × character included to make gedit see this as ISO not UTF-8
/* lexical and code generation sections of a KDF9 assembler
   parsing is done by yacc in companion file with .y extension
 */

// Usage kal4 {switches} filename
// Switches are
//    -t produice tables of identifiers
//    -v produce a listing on first pass as well as second
//    -a supress A-block at start of binary paper tape

// Listng is on stdout
// MT format binary in mt.out, suitable for input to kdf9.c
// PT format binary in a.out -- KDF9 chars suitable for input to ee9

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <sys/stat.h>
#include <unistd.h>

#include "kal4.tab.h"

#include "kal4.h"

extern int yylval;

#ifndef YYDEBUG
int yydebug = 0;
#endif

#ifndef O_BINARY
#define O_BINARY  0
#endif

#define GUTTER  70
             // GUTTER is where the source listing is to the right of the code listing

int verbose = 0;
unsigned char vstoreval[13]; // newly evaluated V-store contents are put here -- plus a guard digit for rounding fractions

char prtline[1000];          // line for storing output of binary progam to listing
int prtpos = 0;              // position of RH end of printing line

int memaddr;
int ablk = 1;                // can be set to 0 by -a switch to remove A-block from binary paper tape

int ystorebase = 0;          // temp to pacify compiler

extern int yydebug;
extern int yyparse();

int listing = 0;
int tables = 0;              // produce KAL4 tables of identifier values
int error_seen = 0;
int lineCount = 0;
int firstpass = 1;
int endsrc = 0;
unsigned char *srcmarker;         // set by yylex to mark the start of a sequence of digits
int nexch = 0;           // really local to yylex() but modified by printerconst
unsigned char *buffp, *startLine;

unsigned char progname[12];
unsigned char nonprtcode[128], prtcode[128];
unsigned char ptchar[64]          // 8-bit paper tape chars for each 6-bit character
    = { 0x90, 0x11, 0x12, 0x03, 0x14, 0x05, 0x06, 0x17,
        0x18, 0x09, 0x0A, 0x1B, 0x0C, 0x1D, 0x1E, 0x0F,
        0x30, 0x21, 0x22, 0x33, 0x24, 0x35, 0x36, 0x27,
        0x28, 0x39, 0x3A, 0x2B, 0x3C, 0x2D, 0x2E, 0x3F,
        0x50, 0x41, 0x42, 0x53, 0x44, 0x55, 0x56, 0x47,
        0x48, 0x59, 0x5A, 0x4B, 0x5C, 0x4D, 0x4E, 0x5F,
        0x60, 0x71, 0x72, 0x63, 0x74, 0x65, 0x66, 0x77,
        0x78, 0x69, 0x6A, 0x7B, 0x6C, 0x7D, 0x7E, 0x6F
      };
   
unsigned char *buff  ;            // buffer for input -- allocated by loadfile
int endbuff;                      // start of work area in buffer for dealing with subscript 10
unsigned char corestore[8193*6];  // The syllables of KDF9 object code + (6 spares for overflow detection ??)

#define  IDHASHSZ  3000               // allow up to 3000 names -- temp ????
char idenchar[256];
char isletter[256];
char constype[256];
char abs8tab[256];                    // 8-bit Algol basic symbols indexed by ASCII values
char abs6tab[256];                    // 6-bit Algol basic symbols indexed by ASCII values
char lprttab[256];                    // 6-bit KDF9 printer code indexed by ASCII values

char *idtable[IDHASHSZ];              // Symbol table, value of identifier is the integer index in this table
int category[IDHASHSZ];               // Indicates what type of instruction for identifiers that are also valid instructions
int mplusi, mminusi, splusm, splusi, splusc, splusq, justj, justz;
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

/* separately compiled table generator */
extern void initidtable();

/* code generator routines used in main() */
void storejump(int s1, int s2, int loc);
int codeloc(int pno, int labno);

/* prototype to deal withforward reference and keep kal3 copies at the end */
char *lstpos(int memaddr, int nsylls);

void loadfile(char *fn)
{  int fin = open(fn, O_RDONLY + O_BINARY);
   int file_size, n;
   struct stat file_gen;

   if  ( fin < 0 )
   {  perror(fn);
      exit(1);
   }

   if  ( fstat(fin, &file_gen) < 0 )  /* this should be impossible */
      perror( "fstat to get size" );
   file_size = file_gen.st_size;
   buff = (unsigned char *)malloc(file_size + 123);

   n = read(fin, buff+2, file_size);
   memset(buff+n, 0, 10);            // try to guarantee that we do not run off the end

   close(fin);
   printf("Loaded %d bytes for %s\n", n, fn);
   buffp = buff;
   *buffp++ = '\n';                  // ensure that we do not run off the front wiht invalid input
   *buffp++ = '\n';
   endbuff = n+23;
}

int lasttoken = ';';                    // holds the last token delivered by yylex
char *keepbuffp;                        // holds start of identifier after ; etc
void yyerror(char *s);

int yylex()
/* yacc's lexical analyser.
   Note the comment about IDENTIFIER on routine makelabiden(int i1, int i2, int i3)
 */
{  int res;
   char *ws;
   char *idch = idenchar;

   if  ( listing )
   {  while  ( nexch <= ' ' )
      {  if  ( nexch == '\n' )
         {  if  ( prtpos >= GUTTER )    // too much code for side-by-side printing
            {  printf("%s\n", prtline);
               prtpos = 0;
            }
            memset(prtline+prtpos, ' ', GUTTER-prtpos);
            prtline[GUTTER] = 0;
            res = *(buffp-1);
            *(buffp-1) = 0;
            printf("%s %s\n", prtline, startLine);
            *(buffp-1) = res;
            startLine = buffp;
            lineCount ++;
            prtpos = 0;
         }
         nexch = *(buffp++);
      }
   }
   else
      while  ( nexch <= ' ' )
         nexch = *(buffp++);

   if  ( endsrc )
   {  nexch = 0;             // stop false first read
      return lasttoken = 0;              // end of file
   }

   if  ( isdigit(nexch) )
   {  srcmarker = buffp;    // used in evaluation of numberical constants of various sorts
      res = nexch - '0';
      while  ( isdigit(nexch = *(buffp++))  ||  nexch == ' ' )
         if  ( nexch != ' ' )
            res = res*10 + nexch - '0';
      yylval = res;
      if  ( nexch != 's' )                   // if not a KDF9 address with syllable specification
         return lasttoken = UNSIGNED_INTEGER;
      nexch = *(buffp++);                    // should be KDF9 syllable number
      if  ( nexch < '0'  ||  nexch >= '6' )  // obviously not a syllable number
      {  buffp --;
         nexch = 's';
         return lasttoken = UNSIGNED_INTEGER;
      }
      yylval = res*6 + nexch - '0';           // convert to KDF9 memory location
      nexch = *(buffp++);                     // move on
      return lasttoken = KDF9ADDRESS;
   }
   else if  ( nexch == '/' && ( lasttoken == ';'  ||  lasttoken == ':' )    // comment
           || nexch == '[' )                                                // 21st century comment -- i.e. inserted in 21st century
   {  if  ( memcmp(buffp, "????", 4) == 0 )          // facility for turning on diagnostics
      {  printf("?????????????? setting verbose at line %d\n", lineCount);
         verbose = 1;
      }
      res = 0;
      while  ( (nexch = *(buffp++)) != '\n' ) ;      // skip to end of line
      lasttoken = ';';                               // looks like end of instruction
      return COMMENT;
   }
   else if  ( isletter[nexch] != 0 )                 // identifier
   {  ws = buffp - 1;
      if  ( lasttoken == ';'  ||  lasttoken == ':'  ||  lasttoken == '*' )
                                                     // must be start of new instruction ...
      {  idch = isletter;                            // ... so don't allow digits in first identifier
         keepbuffp = buffp;                          // remember where we are so that we can go back to start of identifier
      }
      else if  ( lasttoken == '='                    // if start of a constant ...
                 &&  ( constype[nexch] != 0 )        // ... and this letter is a type of constant ...
                )                                    // ... then we return the letter as a token
      {  srcmarker = buffp;                          // used in evaluation of constants of various sorts
         res = nexch;
         lasttoken = constype[nexch];                // leave lasttoken as = for the length codes D T S H
         if  ( res == 'P'  ||  res == 'X' )          // printer constant or 6-bit Algol basic symbols
         {  while  ( *buffp != ';' )                 // skip to ;
               buffp ++;
            yylval = res;
            res = STRING;
         // printf("Printer string   type = %c  firstch = %c\n", yylval, *srcmarker);
         }
         nexch = *(buffp++);
         if  ( verbose)   printf("Letter token %c %c\n", yylval, res);
         return res;
      }
      while  ( idch[*buffp] != 0 )
         buffp ++;
      res = *buffp;   *buffp = 0;
      
      yylval = (((*ws)<<7) + ws[1]) % IDHASHSZ;
      while  ( idtable[yylval] != NULL  &&  strcmp(idtable[yylval], ws) != 0 )
         if  ( ++yylval == IDHASHSZ )
            yylval = 0;
      if  ( idtable[yylval] == NULL )
         idtable[yylval] = strdup(ws);
      ws = idtable[yylval];

      if  ( strcmp(ws, "SET3") == 0 )
         printf("Last token = %d %c\n", lasttoken, lasttoken);

      
      *buffp = res;
      nexch = *(buffp++);
      if  ( verbose )  printf("Identifier %s %d\n", ws, yylval);
#ifdef NEVER
      if  ( strcmp(ws, "begin") == 0 )          // can re-enable this to get reserved words begin etc
      {  lasttoken = ';';                       // Makes yylex() parse next bit as if start of instruction
         return BEGIN;
      }
      if  ( strcmp(ws, "end") == 0 )
      {  lasttoken = ';';                       // Makes yylex() parse next bit as if start of instruction
         return END;
      }
      if  ( strcmp(ws, "label") == 0 )
         return lasttoken = LABEL;
#endif
      return lasttoken = IDENTIFIER;
   }
   else if  ( nexch == '{' )                    // Algol basic symbol string
   {  srcmarker = buffp;                        // used in evaluation of constants of various sorts
      while  ( *(buffp++) != '}' ) ;            // skip to }
      nexch = *(buffp++);
      yylval = '{';
   // printf("ABS string   type = %c  firstch = %c\n", yylval, *srcmarker);
      return lasttoken = STRING;
   }
   else if  ( nexch == 215 && lasttoken == '=' ) // packed ABSs  215 is '×'
   {  srcmarker = buffp;
      while  ( *buffp != ';' )                 // skip to ;
         buffp ++;
      yylval = nexch;
      nexch = *(buffp++);
      return lasttoken = STRING;
      printf("Packed ABS string   type = %c  firstch = %c\n", yylval, *srcmarker);
   }
   else if  ( nexch == '!'  ||  nexch == '\'' )         // compound Algol basic symbol
   {  lasttoken = ';';                                  // Makes yylex() parse next bit as if start of instruction
      if  ( memcmp(buffp, "begin", nexch = 5) == 0 )    // !begin Eldon2 version of ABS begin
         res = BEGIN;
      else if  ( memcmp(buffp, "label", 5) == 0 )       // nexch holds length of symbol
         res = (lasttoken = LABEL);                     // don't want next ID parsing as though start of instruction
      else if  ( memcmp(buffp, "end", nexch = 3) == 0 )
         res = END;
      else
         yyerror("Invalid compound Algol basic symbol");
      buffp += nexch;
      if  ( (nexch = *(buffp++)) == '\'' )              // if 'begin' style stropping
         nexch = *(buffp++);
//    printf("Returning token %d   nexch = %c\n", res, nexch);
      return res;
   }
   else
   {  res = nexch;
      nexch = *(buffp++);
      return lasttoken = res;
   }
}

char *errsub(char *f, char *s)
/* substitute a character string in an error message */
{  static char buff[200];
   if  ( strlen(f) + strlen(s) >= 200 )
      return f;
   else
      sprintf(buff, f, s);
   return buff;
}

void yyerror(char *s)
{  int w;
   char *p = buffp;
   int limit = 12;
   fprintf(stderr, "Error %s\n", s);
   error_seen ++;

   while  ( *++p != '\n' ) ;     // find end of current line
   *p = 0;

   p = buffp;   
   while  ( *--p != '\n' ) ;     // find start of current line
   while  ( *--p != '\n' ) ;     // and the one before
   w = *buffp;
   *buffp = 0;
   printf("\nError %s\n------------------------%s!!!%c%s\n", s, p, w, buffp+1);

   if  ( lasttoken < 256 )
      printf("lasttoken =  %c %d\n", lasttoken, lasttoken);
   else if  ( lasttoken == IDENTIFIER )
      printf("lasttoken = IDENTIFIER %d\n", lasttoken);
   else if  ( lasttoken == UNSIGNED_INTEGER )
      printf("lasttoken = UNSIGNED_INTEGER %d\n", lasttoken);
   else if  ( lasttoken == COMMENT )
      printf("lasttoken = COMMENT %d\n", lasttoken);
   else if  ( lasttoken == BEGIN )
      printf("lasttoken = BEGIN %d\n", lasttoken);
   else if  ( lasttoken == END )
      printf("lasttoken = END %d\n", lasttoken);
   else if  ( lasttoken == LABEL )
      printf("lasttoken = LABEL %d\n", lasttoken);
   else if  ( lasttoken == KDF9ADDRESS )
      printf("lasttoken = KDF9ADDRESS %d\n", lasttoken);
   else
      printf("lasttoken = ??????? %d\n", lasttoken);

   if  ( yylval < 3000 )
      printf("yylval = %d %s\n", yylval, idtable[yylval]);
   else
      printf("yylval = %d\n", yylval);

   printf("nexch = %c %d\n", nexch, nexch);

   exit(1);

   printf("\nError\n%s\n", buffp-1);

   printf("\n======\n%s\n==============\n", buff);


   w = yylex();
   if  ( w >= 256 )
      printf("  %d  ->  %d\n", w, yylval );
   else
      printf("%c %d \n", w, w );
   w = yylex();
   if  ( w >= 256 )
      printf("  %d  ->  %d\n", w, yylval );
   else
      printf("%c %d \n", w, w );
   w = yylex();
   if  ( w >= 256 )
      printf("  %d  ->  %d\n", w, yylval );
   else
      printf("%c %d \n", w, w );
   w = yylex();
   if  ( w >= 256 )
      printf("  %d  ->  %d\n", w, yylval );
   else
      printf("%c %d \n", w, w );

   exit(1);

}

void setprinterchars()
{  int i;
   for  ( i = '0'; i<='9'; i++ )
   {  prtcode[i] = i - '0' + 020;    // decimal digits
      nonprtcode[i] = i - '0' + 0140;    // decimal digits as repeat counts
   }
   for  ( i = 'A'; i<='Z'; i++ )
      prtcode[i] = i + 041 - 'A';        // uppercase letters

      prtcode['%'] = 06;
      prtcode['\''] = 07;                // 07 '
      prtcode[':'] = 010;
      prtcode['='] = 011;
      prtcode['('] = 012;
      prtcode[')'] = 013;

      nonprtcode['['] = '[';            // for switching between sets
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
      prtcode['#'] = 033;      // subscript 10 - what is this in Hans's code
      prtcode['+'] = 035;
      prtcode['-'] = 036;
      prtcode['.'] = 037;
      prtcode['$'] = 014;        // 14 dd &pound;
      prtcode['#'] = 000;        // space
      prtcode[' '] = 65;         // space is ignored in the printer constant

                // 75 dd EM
                // 76 dd start message
                // 77 dd ignored
}

void writeprogname(int dv, int n)
/* writes out the program name in A-block and B-block format */
{  int i;
   buff[0] = ptchar[7];
   buff[1] = ptchar[2];
   buff[2] = ptchar[060];        // letter P
   buff[3] = ptchar[2];
   for  ( i = 0; i<12; i++ )
      buff[i+4] = ptchar[progname[i]&63];
   write(dv, buff, n);
}

void ptwrite(int dv, int start, int stop)
// write out a block of store upto but not including stop
{  int i = start * 6;
   int j = stop * 6;
   int k = 0;
   while  ( i < j )
   {  buff[k++] = ptchar[(corestore[i] >> 2)&63];          // use the input buffer as output area
      buff[k++] = ptchar[((corestore[i]<<4) + (corestore[i+1]>>4))&63];
      buff[k++] = ptchar[((corestore[i+1]<<2) + (corestore[i+2]>>6))&63];
      buff[k++] = ptchar[corestore[i+2]&63];
      i += 3;
   }
   write(dv, buff, k);
}

void ptbinout()
// output a KDF9 paper tape binary program
// not yet complete -- needs A-block and program identifier
{  int fout = open("/tmp/a.out", O_CREAT + O_BINARY + O_TRUNC + O_WRONLY, 0640);
   int i, j;
   if  ( fout < 0 )
      perror("/tmp/a.out");
   else
   {  if  ( ablk != 0 )        // A-block generation not suppressed to satisfy ee9 -- temp ??
      {  buff[16] = ptchar[075];                  // end-message
         buff[17] = 0;                            // blank tape
         buff[18] = 0;                            // blank tape
         buff[19] = 0;                            // blank tape
         writeprogname(fout, 20);
      }
      if  ( (corestore[47]|corestore[46]|corestore[45]|corestore[44]) == 0 )   // no filler word set
      {  corestore[45] = 8;                       // filler word in B-block
         corestore[46] = (ystorebase - 1) >> 8;   // filler word in B-block
         corestore[47] = (ystorebase - 1) & 255;  // filler word in B-block
      }
      ptwrite(fout, 0, 2);                     // B-block
      writeprogname(fout, 16);
      ptwrite(fout, 4, 8);                     // B-block continued
      ptwrite(fout,                // C-block addr taken from E7
           ((corestore[44]<<8) | corestore[45]),
           ((corestore[46]<<8) | corestore[47]) + 1
             );
      close(fout);
   }
}

void mtbinout()
// output a KDF9 mag tape B-block and C-block
// must be called after ptbinout()
{  int fout = open("/tmp/mt.out", O_CREAT + O_TRUNC + O_WRONLY + O_BINARY, 0640);
   int i, j;
   if  ( fout < 0 )
      perror("/tmp/mt.out");
   else
   {  if  ( corestore[12] == 0 )    // if program name not set in source text
      {  int prn[4];         // program name in KDF9 chars
         prn[0] = 07024402;        // load up words 2 and 3
         prn[1] = (progname[0]<<18) + (progname[1]<<12) + (progname[2]<<6) + progname[3];
         prn[2] = (progname[4]<<18) + (progname[5]<<12) + (progname[6]<<6) + progname[7];
         prn[3] = (progname[8]<<18) + (progname[9]<<12) + (progname[10]<<6) + progname[11];
         for  ( i = 0; i<4; i++ )
         {  corestore[3*i + 12] = (prn[i]>>16)&255;
            corestore[3*i + 13] = (prn[i]>>8)&255;
            corestore[3*i + 14] = prn[i]&255;
         }
      }
      i = ((corestore[44]<<8) | corestore[45])*6;
      j = ((corestore[46]<<8) | corestore[47])*6;
      write(fout, corestore, 48);                     // 8 word B-block 
      write(fout, corestore + i, j - i + 6);          // C-block
      close(fout);
      printf("C-block from %d to %d\n", i/6, j/6);
   // printf("C-block from %o to %o\n", i/6, j/6);
   }
}

int main(int argc, char **argv)
{  int i;

   yydebug = 1;
   if  ( yydebug )
      printf("Started\n\n");

   for  ( i = 0; i<128; i++ )
   {  nonprtcode[i] = 64;    // invalid by default
      prtcode[i] = 64;    // invalid by default
   }
   setprinterchars();
   for  ( i = 0; i<48; i++ )
      corestore[i] = 0;    // zeroise B-block

   i = 0;
   while  ( *(srcmarker = (unsigned char *)argv[++i]) == '-' )
      if  ( srcmarker[1] == 't' )      // -t produce tables
         tables = 1;
      else if  ( srcmarker[1] == 'v' ) // -v verbose lists on first pass as well as second
         listing = 1;
      else if  ( srcmarker[1] == 'a' ) // suppress A-block output on paper tape for ee9
         ablk = 0;

   loadfile((char *)srcmarker);     // deal with opening source text etc

   for  ( i = 0; i<9; i++ )
      progname[i] = prtcode[srcmarker[i]];
   progname[11] = (progname[9] = prtcode['U']);
   progname[10] = prtcode['P'];

   initidtable();

   startLine = buffp;

   yyparse();                    // first pass

   printf("\n\n=============== Second pass\n");
// printf(buff);
   printf("===========================\n");
   prtpos = 0;
   firstpass = 0;
   startLine = buffp;
   listing = 1;

   memset(corestore, 0, 48);     // zeroise B-block, esp filler word

   memaddr = 0;                  // ready to plant initial jump
// storejump(0200, 0260, codeloc(-1, 0));
   buffp = buff;
   startLine = buffp;
   endsrc = 0;
   nexch = 0;              // stop false first read

   yyparse();              // second pass

   ystorebase = (memaddr+5)/6;
   ptbinout();
   mtbinout();

   yylval = 0;          // just used for counting
   for  ( i = 0; i<=IDHASHSZ; i++ )
      if  ( idtable[i] != NULL )
         yylval ++;
   printf("No of ID table entries = %d (%d%%)\n", yylval, (yylval*100)/IDHASHSZ);
}


// KAL4 specific routines, mostly invoked from kal4.y


// Scope management routines

struct LABREC { struct LABREC *next; int id; int val; };         // chain of labels within a block
struct SCOPEREC { struct SCOPEREC *outer;         // pointer to containing scope
                  struct SCOPEREC *next;          // pointer to next in sequence during 2nd pass -- to previous in first pass
                  struct LABREC *labs;            // identifiers with scope in this block
                  unsigned char *buffp;           // remember where this block starts as a check on 2nd pass
                } *currentblock = NULL,           // currently active block
                  *recentblock = NULL;            // most recently seen block in first pass -- next new block in 2nd pass

int scopelevel = 0;

char *showlab(struct LABREC *lr)
{  if  ( lr == NULL )
      return "(null)";
   else
      return idtable[lr->id];
}

extern void openscope()
{  struct LABREC *lr;
   scopelevel ++;
   if  ( tables )
      printf("Start scope level %d\n", scopelevel);
   if  ( firstpass )
   {  struct SCOPEREC *s = currentblock;
      currentblock = (struct SCOPEREC *)malloc(sizeof(struct SCOPEREC));
      currentblock->outer = s;
      currentblock->next = recentblock;           // this list is constructed backwards on the 1st pass (see below)
      recentblock = currentblock;
      currentblock->labs = NULL;
      currentblock->buffp = buffp;
   }
   else if  ( currentblock == NULL )              // entry to first block on second pass
   {  struct SCOPEREC *s;
      while  ( (s = recentblock->next) != NULL )  // reversing the order of this chain to get it in the true order
      {  recentblock->next = currentblock;
         currentblock = recentblock;
         recentblock = s;
      }
      recentblock->next = currentblock;
      currentblock = recentblock;
      if  ( verbose )
         printf("Starting in block starting with label %s\n", showlab(currentblock->labs));
   }
   else                                         // entry to inner block on second pass
   {  if  ( verbose )
         printf("Leaving block starting with label %s\n", showlab(currentblock->labs));
      recentblock = (currentblock = recentblock->next);
      if  ( verbose )
         printf("Entering block starting with label %s\n", showlab(currentblock->labs));
      if  ( currentblock->buffp != buffp )
         printf("!!! Out of sync by %d chars\n", (int)(currentblock->buffp - buffp));
      lr = currentblock->labs;
      if  ( 0 && verbose )                      // do we ever want this output
         while  ( lr != NULL )
         {  printf("      %08o/%o %6d %s\n", lr->val/6, lr->val%6, lr->val, idtable[lr->id]);
            lr = lr->next;
         }
   }
}

extern void closescope()
// On the firstpass, deals with promoted labels by going down the current scope,
// and either retaining them or promoting them to outer
{  struct LABREC *lst = currentblock->labs;       // list to scan
   struct SCOPEREC *s;

   currentblock->labs = NULL;                 // take them all away and put thme back with the next loop
   if  ( firstpass )                          // need to promote any labels that were in a label list
   {  while  ( lst != NULL )
      {  struct LABREC *wlr;                  // workspace for a LABREC
         int addr = (lst->val) & 07777777;    // mask out the promote bit
         s = currentblock;
         if  ( ((lst->val) >= 010000000) )    // if this label is to be promoted
         {  s = s->outer;                     // change the target block to the next scope outwards
            if  ( tables )
               printf("Promoted %8o/%o %s\n", addr/6, addr%6, idtable[lst->id]);
            if  ( s == NULL )                 // trying to promote out of the root block
            {  printf("trying to promote %s out of the root block\n", idtable[lst->id]);
               s = currentblock;
            }
            else                              // see if this label already exists in the outer block
            {  wlr = s->labs;
               while  ( wlr != NULL  &&  wlr->id != lst->id )
                  wlr = wlr->next;
               if  ( wlr != NULL )            // already exists in outer block
                  if  ( wlr->val >= 0 )       // and is defined
                     yyerror(errsub("Label %s is defined in outer block", idtable[lst->id]));
                  else
                  {  s = NULL;                // stop chaining into outer list
                     wlr->val = lst->val;     // pass value to multiply promoted label ...
                  }                           // ... still marked as promoted
            }
            lst->val -= 010000000;            // remove the promote marker
         }
         else if  ( ((lst->val) < 0) )        // if this label is to be promoted but was not defined
         {  printf("Promoted label %s was not defined in this block\n", idtable[lst->id]);
            s = NULL;
         }
         else                                 // healthy local label
            if  ( tables )
               printf("         %8o/%o %s\n", addr/6, addr%6, idtable[lst->id]);
         wlr = lst->next;
         if  ( s != NULL )                   // if we are retaining this entry
         {  lst->next = s->labs;
            s->labs = lst;
         }
         lst = wlr;
      }
   }
   currentblock = currentblock->outer;

   if  ( tables )
      printf("Leaving scope level %d\n", scopelevel);
   scopelevel --;

   if  ( verbose )
      if  ( currentblock == NULL )
         printf("Now at end of program\n");
      else if  ( currentblock->labs == NULL )
         printf("Returning to block not yet containing any labels\n");
      else
         printf("Returning to block starting with label %s\n", idtable[currentblock->labs->id]);
}

void setlabel(int labid, int loc)
/* record integer label - different in KAL4 */
{  if  ( loc == -6 )
      yyerror("Illegal forward reference");
   if  ( strcmp(idtable[labid], "prog") == 0 )
      memaddr = loc;
   else
   {  struct LABREC *lab;
      if  ( loc < 0 )
      {  loc = memaddr;
         if  ( category[labid] != 0 )      // if this is a valid instruction mnemonic
            printf("Label %s may be an instruction teminated by : instead of ;\n", idtable[labid]);
      }
      if  ( currentblock == NULL )
         yyerror("Current block is null");
      lab = currentblock->labs;
      while  ( lab != NULL  &&  lab->id != labid )
         lab = lab->next;

      if  ( verbose )
         if  ( lab == NULL )
            printf("Defining %s = %d = %o/%o\n", idtable[labid], loc, loc/6, loc%6);
         else
            printf("Defining %s = %d = %o/%o [%d]\n", idtable[labid], loc, loc/6, loc%6, lab->val);

      if  ( firstpass )
      {  if  ( lab == NULL )                       // no previous label with the same scope
         {  lab = (struct LABREC *)malloc(sizeof(struct LABREC));
            lab->next = currentblock->labs;        // make a new LABREC
            currentblock->labs = lab;
            lab->id = labid;
            lab->val = loc;
         }
         else if  ( lab->val < 0 )                 // only previously seen in label list
            lab->val = 010000000 + loc;
         else                                      // error already set in this scope
         // yyerror(errsub("Label %s declared twice", idtable[labid]));
            printf("Label %s declared twice at %o/%o and %o/%o\n", idtable[labid],
                         lab->val/6, lab->val%6, loc/6, loc%6);
      }
      else if  ( lab != NULL                       // may have been promoted - don't bother to check
                 &&  lab->val != loc )             // check value is the same second time around
         printf("Failure to match label -- out of sync -- %d != %d %s\n", lab->val, loc, idtable[labid]);
   }
}

int evaluate(int ident)
/* obtain the value of an identifier */
{  if  ( strcmp(idtable[ident], "prog") == 0 )
      return memaddr;
   else
   {  struct SCOPEREC *blk = currentblock;
      struct LABREC *lab = NULL;
      int cs = scopelevel;
      while  ( lab == NULL  &&  blk != NULL )    // follow all the scopes outwards
      {  lab = blk->labs;                        // search one scope
         while  ( lab != NULL  &&  lab->id != ident )
            lab = lab->next;
         blk = blk->outer;
         if  ( verbose )
            printf("Searching level %d for %s\n", --cs, idtable[ident]);
      }
      if  ( lab == NULL  ||  lab->val < 0 )      // label does not exist, or not yet defined
      {  if  ( ! firstpass )
            if  ( lab == NULL )
               printf("Label %s is not defined\n", idtable[ident]);
            else
               printf("Label %s is not defined but appears in a !label clause\n", idtable[ident]);
         return -6;
      }
      else                                       // we must evaluate if we can to deal with explicit labels
      {  if  ( lab->val >= 010000000  &&  ! firstpass )
            printf("Label %s is over promoted\n", idtable[ident]);
         if  ( verbose )  printf("evaluated %s as %d\n", idtable[ident], lab->val);
         return lab->val & 07777777;
      }
   }
}

void promote(int ident)
/* increas the scope level of this identifier */
{  struct LABREC *lab;
   if  ( firstpass )
   {  lab = currentblock->labs;
      while  ( lab != NULL  &&  lab->id != ident )
         lab = lab->next;
      if  ( lab != NULL )            // label value aleady set
         lab->val |= 010000000;      // promotion marker is added into value at position 21
      else                           // no previous label with the same scope
      {  lab = (struct LABREC *)malloc(sizeof(struct LABREC));
         lab->next = currentblock->labs;
         currentblock->labs = lab;   // make a new LABREC
         lab->val = -1;               // special value to indicate a label due for promotion
         lab->id = ident;
      }
      if  ( verbose )
         printf("promote %s [%o]\n", idtable[ident], lab->val);
   }
}

// end of scope menagement routines

int makelabiden(int i1, int i2, int i3)
/* combines an identifier that was wrongly split because of the no digits rule after ; or : */
{  char knexch = nexch;
   char *kbuffp = buffp;
   buffp = keepbuffp - 1;
   nexch = *(buffp++);
   if  ( verbose )  printf("Combining %s%d -- first char of ID = %c\n", idtable[i1], i2, nexch);
   lasttoken = 'z';                    // anything not ';' etc
   if  ( yylex() != IDENTIFIER )
      printf("error!!\n");
   nexch = knexch;
   buffp = kbuffp;
   lasttoken = ':';
   return yylval;
}


int tablocate(unsigned char *s)
/* locate this character string as though it were an identifier */
/* fancy characters in instruction mnemonics need unsigned char */
{  int i = (((*s)<<7) + s[1]) % IDHASHSZ;
   while  ( idtable[i] != NULL  &&  strcmp(idtable[i], s) != 0 )
      if  ( ++i == IDHASHSZ )
         i = 0;
   if  ( idtable[i] == NULL )
      yyerror("Invalid instruction mnemonic");
   return i;
}

int idtablocate(char *s, int id)
/* locate the character string formed by concatenating s and id as though it were an identifier */
{  char pat[7];
   strcpy(pat, s);
   strcat(pat, idtable[id]);
   return tablocate(pat);
}

int jumptest(int idj, int idz, char *syn)
// handles synonyms such as J>=Z -- JGEZ
// idj must be "J" and idz "Z" for the instruction to be valid
// syn is the all-letters mnemonic that will be used for table lookup
{  if  ( idj != justj )
      yyerror("Invalid mnemonic");
   if  ( idz != justz )   // e.g.J=?.addr (for J=.addr, idz is set to justz in the yacc file
      yyerror("Invalid jump instruction");
   return tablocate(syn);
}

void setlasttoken(char c)
/* sets the last token (always to ;??) to have mnemonic mode lexical analysis for x+Cn */
{  lasttoken = c;
}

void nonums(int i1)
// process instructions with no parameters
{  int cat = category[i1];
   if  ( verbose )  printf("%s => %d\n", idtable[i1], cat&255);
   if  ( (cat&255) == ONESYLL )
      storesyl((cat>>16)&255);
   else if  ( (cat&255) == LINK )
      store2syl(twosylshetc((cat>>16)&255, 0));
   else if  ( (cat&255) == OUTETC  ||  (cat&255) == EXIT )  // does EXITD, OUT, EXIT, EXITH
      store3syl((cat>>16)&255, cat&0374000);
   else if  ( (cat&255) != LATER )
      yyerror(errsub("Not a one-syllable instruction: %s (nonums)", idtable[i1]));
}

void onenum(int i1, int n1)
// process instructions with a single number parameter
// 3 of these instructions have duplicate mnemonics of another format
{  int cat = category[i1];
   if  ( verbose )  printf("%s => %d  n=%d\n", idtable[i1], cat&255, n1);
   if  ( (cat&255) == SET )
      store3syl(0304, n1&0177777);
   else if  ( (cat&255) == ONEQ )
      store2syl(twosylqx((cat>>16)&255, n1, (cat>>8)&255));
   else if  ( idtable[i1] == idtable[i1+1] )         // must be a duplicate instruction e.g. MFRRQ
      onenum(i1+1, n1);
   else if  ( (cat&255) == SHIFT )
   {  if  ( n1 < -64 || n1 >= 64 )
         yyerror("Shift out of range");
      store2syl(twosylshetc((cat>>16)&255, 2*(n1&0177) + 1));
   }
   else
      yyerror(errsub("Not a valid instruction %s (onenum)", idtable[i1]));
}

void onlyset(int i1, int n1)
/* just used for the SET instruction such as SET#55 */
{  int cat = category[i1];
   char *p = buff + n1;
   n1 = 0;
   while  ( isdigit(*++p) )
      n1 = (n1<<3) + (*p&7);
   if  ( verbose )  printf("%s => %d  n=%d\n", idtable[i1], cat&255, n1);
   if  ( (cat&255) == SET )
      store3syl(0304, n1&0177777);
   else
      yyerror(errsub("Not a valid instruction %s (onlyset)", idtable[i1]));
}

void mpmi(int i1, int pm, int i2)
/* Only valid for instructions of the form M+Iq, M-Iq, S+Cq, S+Iq, S+Mq, S+Qq */
{  char *i1str = idtable[i1];
   char *i2str = idtable[i2];
   int n = 0;
   int ins = 0;
   if  ( strcmp(i1str, "M") == 0  &&  *i2str == 'I' )
   {  if  ( pm == '+' )
         ins = mplusi;
      else
         ins = mminusi;
   }
   else if  ( strcmp(i1str, "S") == 0  &&  pm == '+' )
   {  if  ( *i2str == 'C' )
         ins = splusc;
      else if  ( *i2str == 'I' )
         ins = splusi;
      else if  ( *i2str == 'M' )
         ins = splusm;
      else if  ( *i2str == 'Q' )
         ins = splusq;
   }
   while  ( isdigit(*++i2str) )
      n = n*10 + *i2str - '0';
   if  ( ins == 0  ||  n >= 16 )
   {  printf("invalid instruction %s%c%s %d\n", i1str, pm, i2str, n);
      yyerror("Not a valid instruction (new mpmi)");
   }
   onenum(ins, n);
}

void multplus(int mult, int id, char *syn)
/* handles MULT+, MULT+Cq, etc */
{
}

void checkid(int id, char *pat)
/* checks tht the identifier is the specified pat */
{  if  ( strcmp(idtable[id], pat) != 0 )
      yyerror("Invalid instruction (not MULT)");
}


void twonums(int i1, int n1, int n2)
{  int cat = category[i1];
   if  ( verbose )  printf("%s => %d\n", idtable[i1], cat&255);
   if  ( (cat&255) != TWOQ )
      yyerror("Not a valid 2 Q-store instruction");
   store2syl(twosylqx((cat>>16)&255, n1, n2));
}

void memaccess(int instr, int qst, int addr)
/* access to memory possibly with Q-store modifications */
/* addr is in bytes */
{  int a = addr/6;
   int s1 = ((a>>9)&070) + instr;   // put top address bits in 1st syllable
   if  ( addr%6 != 0 )
      yyerror("Not a whole word address");
   else
      store3syl(s1, (a&07777) + (qst<<12));
}

void justaddr(int i1, int a1)
{  int cat = category[i1];
   int c0;
   if  ( verbose )  printf("%s => %d  address = %d\n", idtable[i1], cat&255, a1);
   if  ( (c0 = cat&255) == JUMP )
      storejump((cat>>16)&255, (cat>>8)&255, a1);
   else if  ( c0 == SET )
   {  int n1 = a1/6;
      if  ( (cat = a1%6) != 0 )
      {  printf("Warning: SET instruction to non-whole word address\n");
         n1 += cat<<13;
      }
      store3syl(0304, n1&0177777);
   }
   else if  ( c0 == MEMREF )
      memaccess((cat>>16)&255, 0, a1);
   else if  ( c0 == EXIT )
      store3syl((cat>>16)&255, a1/6 + (cat & 0xFF00));
   else
      yyerror("Not a valid instruction (justaddr)");
}

void numaddr(int i1, int n1, int a1)
{  int cat = category[i1];
   if  ( verbose )  printf("%s => %d  Q=%d  address = %d\n", idtable[i1], cat&255, n1, a1);
   if  ( (n1>>4) != 0 )
      yyerror("Invalid Q-store number");
   if  ( (cat&255) == JUMPQ )
      storejump((cat>>16)&255, n1<<4, a1);
   else if  ( (cat&255) == MEMREFQ )
      memaccess((cat>>16)&255, n1, a1);
   else
      yyerror("Not a valid instruction (numaddr)");
}

int addr16(int a)
/* take a byte address into KDF9 SJNS form if not a multiple of 6 */
{  if  ( a%6 == 0 )
      return a/6;
   if  ( a >= 8192*6 )
      printf("Address is not a whole word and exceeds 8191\n");
   return (a/6) | ((a%6)<<13);
}

void preset96()
/* deposits a 96-bit preset value in a double-word */
/* not currently used */
{  int i;
   if  ( memaddr%6 != 0 )
      yyerror("Not a whole word address for 48-bit constant");
   if  ( listing )
      sprintf(lstpos(memaddr, 12), "%03o %03o %03o %03o %03o %03o !!! !!! !!! !!! !!! !!!",
              vstoreval[0], vstoreval[1], vstoreval[2], vstoreval[3], vstoreval[4], vstoreval[5]);
  
   for  ( i = 0; i<6; i++ )
      corestore[memaddr++] = vstoreval[i];
   for  ( i = 0; i<6; i++ )
      corestore[memaddr++] = 0;                // temp !!! ???
}


void preset48(int val)
/* deposits a 48-bit preset value */
{  int i;
   if  ( memaddr%6 != 0 )
      yyerror("Not a whole word address for 48-bit constant");
   if  ( listing )
      sprintf(lstpos(memaddr, 6), "%03o %03o %03o %03o %03o %03o",
              vstoreval[0], vstoreval[1], vstoreval[2], vstoreval[3], vstoreval[4], vstoreval[5]);
  
   for  ( i = 0; i<6; i++ )
      corestore[memaddr++] = vstoreval[i];
}

void preset24(int val)
/* deposits a 24-bit preset value */
{  int i;
   if  ( memaddr%3 != 0 )
      yyerror("Not a half word address for 24-bit constant");
   if  ( listing )
      sprintf(lstpos(memaddr, 3), "%03o %03o %03o",
              vstoreval[0], vstoreval[1], vstoreval[2]);  // or shour it have been: vstoreval[3], vstoreval[4], vstoreval[5]);
  
   for  ( i = 0; i<3; i++ )
      corestore[memaddr++] = vstoreval[i];
}

char *cfmt[] = {  "%03o",                         // single syllable alignment
                  "%03o %03o",                    // 2 syllable alignment
                  "%03o %03o %03o",               // 3 syllable alignment
                  NULL, NULL,
                  "%03o %03o %03o %03o %03o %03o",// 6 syllable alignment
                  NULL, NULL, NULL, NULL, NULL,
                  "%03o %03o %03o %03o %03o %03o %03o %03o %03o %03o %03o %03o"
               };                                 // 12 syllable alignment

char fcharsprt[] = { 014,64,002,077,075,64,64,64,64,64,64,64,64,007,64,003,64,64,000,004,032,64,64,006,64,46};
char fcharsabs[] = { 0216,64,0240,0377,0276,64,64,64,64,64,64,0220,64,64,64,0003,0215,64,0236,0256,0235,64,64,63,64,0316};

void printerconst(char stype, int align)
/* sets a printer constant -- involves lexical cheating */
/* not yet implemented -- except for parsing */
{  char *p = srcmarker - 1;              // position of the P, {, X or multiply sign
   char term = ';';
   char wc;
   char *tab, *ftab;
   int i = memaddr;                      // assemble chars at current prog address
   char *fmt;
   int chalign;                          // alignment when still in char mode before syllable packing of printer const
   int pad;                              // char or byte to pad with at end of alignment

// printf("printerconst( %c, %d)  %c%c%c -- %d\n", stype, align, p[0], p[1], p[2], memaddr);

   if  ( align >= 40 )
      align = 6;
   else
      align = (align+1)/8;

   if  ( memaddr%align != 0 )
      yyerror("Mis-aligned constant");

   if  ( stype == '{' )                  // ABS string
   {  term = '}';
      tab = abs8tab;
      ftab = fcharsabs;
      chalign = align;
      pad = 0377;
   }
   else
   {  if  ( align < 3 )
         yyerror("Printer constant must align on half word");
      if  ( stype == 'P' )                // printer constant
      {  tab = lprttab;
         ftab = fcharsprt;
         pad = 077;                       // or should it be space
      }
      else                                // ABS 6 string ???
      {  tab = abs6tab;
         ftab = fcharsabs;
         pad = 0;
      }
      chalign = (align&4) + 4;            // 4 or 8 for half or full word
   }

// printf("Start the first loop  -- %d  align = %d\n", memaddr, align);

   while  ( (wc = *++p) != term )        // find end of printer constant
      if  ( wc == '*'  ||  wc == '$' )   // formal symbol
      {  int n = 1;
         if  ( isdigit(*++p) )           // repeat count on formal symbol
         {  n = 0;   p --;
            while  ( isdigit(*++p) )
               n = n*10 + *p - '0';
         }
      // printf("Formal char %c %d\n", *p, *p);
         if  ( (wc = *p) < 'a'  ||  wc > 'z'         // letter out of range
                  ||  (wc = ftab[wc - 'a']) == 64 )  // 64 is marker for bad formal char
            yyerror("Bad formal char");
         while  ( --n >= 0 )
            corestore[i++] = wc;
      }
      else if  ( ((wc = tab[wc]) & 0200) == 0 )      // skip whitespace etc
         corestore[i++] = wc;                        // assemble sequence of chars, and compress later if needed

// printf("Start the padding loop  -- %d\n", memaddr);

   while  ( (i - memaddr)%chalign != 0 )               // pad 
      corestore[i++] = pad;


// printf("Start the printer packing loop i = %d   memaddr = %d\n", i, memaddr);

   if  ( stype != '{' )                  // if it is a 6-bit string
   {  int k = i;                         // the end of the 6-bit chars
      int j = memaddr;
      char s1, s2;
      i = memaddr;                       // back to the start of the string and ...
      while  ( j < k )                   // ... pick up groups of 4 and pack-em-in
      {  s1 = corestore[j+1];
         s2 = corestore[j+2];
      // printf("%d %02o %02o %02o %02o\n", j, corestore[j], corestore[j+1], corestore[j+2], corestore[j+3]);
         corestore[i] = ((corestore[j]<<2) | (s1>>4)) & 255;
         corestore[i+1] = ((s1<<4) | (s2>>2)) & 255;
         corestore[i+2] = ((s2<<6) | corestore[j+3] ) & 255;
      // printf(" -> %d %03o %03o %03o\n", i, corestore[i], corestore[i+1], corestore[i+2]);
         j += 4; i += 3;
      }
   }

// printf("Start the printing loop\n");

   if  ( listing )                       // print in align-size chunks
   {  fmt = cfmt[align-1];
      while  ( memaddr < i )
      {  sprintf(lstpos(memaddr, align), fmt,
                     corestore[memaddr], corestore[memaddr+1], corestore[memaddr+2],
                     corestore[memaddr+3], corestore[memaddr+4], corestore[memaddr+5] );
         memaddr += align;
      }
   }
   else
      memaddr = i;                       // no listing so update memaddress to end of string

// printf("Exit the printer const\n");
}

void setvstore(int len)
/* copies vstoreval into the next part of the program */
{  int i;
   len = (len+1)/8;             // convert from bits to syllables - add 1  in case of 95
   if  ( len >= 7 )             // must be a double constant
   {  printf("Setvstore %d -- implemented as 48 bits STR;\n", len);
      if  ( vstoreval[0] >= 128 ) // -ve double length
      {  vstoreval[0] -= 128;     // clear l.s. sign digit
         for  ( i = 0; i<6; i++ )
            corestore[memaddr+i] = 255;
      }
      else
         for  ( i = 0; i<6; i++ )
            corestore[memaddr+i] = 0;
      for  ( i = 6; i<len; i++ )
         corestore[memaddr+i] = vstoreval[i-6];
   }
   else
      for  ( i = 0; i<len; i++ )
         corestore[memaddr+i] = vstoreval[i];
   if  ( listing )
      sprintf(lstpos(memaddr, len), cfmt[len-1],
                     corestore[memaddr], corestore[memaddr+1], corestore[memaddr+2],
                     corestore[memaddr+3], corestore[memaddr+4], corestore[memaddr+5],
                     corestore[memaddr+6], corestore[memaddr+7], corestore[memaddr+8],
                     corestore[memaddr+9], corestore[memaddr+10], corestore[memaddr+11] );
   memaddr += len;
}

extern int endsrc;

void startnewword(int n)
/* inserts dummy instructions up to word boundary */
/* n is the numbr of word boundary */
/* -- different from Usercode because of KAL4's ** which rounds to 32 word */
{  n *= 6;          // convert to bytes/syllables
   while  ( memaddr%n != 0 )
      storesyl(017);
}


void k4address(int memaddr, int constlen)
/* loads a memref object into vstoreval at the correct position for setvstore */
{  constlen /= 8;          // convert to syllables
   if  ( constlen >=7 )    // if double length accommodate our 48-bit kludge
      constlen = 6;
  memset(vstoreval, 0, 7); // fill with zeros to prevent unwanted rounding (when rounding eventually gets implemented)
  vstoreval[--constlen] = memaddr & 0377;
  vstoreval[--constlen] = (memaddr>>8) & 0377;   // two syllables now put in their rightful place
}

// routines copied from kal3.c

char *lstpos(int memaddr, int nsylls)
// sets address for printing instructions, and updates prtpos
{  char *res;
   if  ( prtpos == 0 )                // start of new line
   {  sprintf(prtline, "%5o/%o: ", memaddr/6,  memaddr%6);
      prtpos = 9;
   }
   else
      prtline[prtpos++] = '|';
   res = prtline + prtpos;
   prtpos += nsylls*3 + nsylls - 1;
   return res;
}

void storesyl(int p1)
/* sstore single syllable instruction p1 */
{  if  ( listing )
      sprintf(lstpos(memaddr, 1), "%03o",  p1);
   corestore[memaddr++] = p1;
}

void store2syl(int p1)
/* sstore 2-syllable instruction p1 */
{  if  ( listing )
      sprintf(lstpos(memaddr, 2), "%03o %03o",  (p1>>8)&255, p1&255);
   corestore[memaddr++] = p1>>8;
   corestore[memaddr++] = p1;
}

void qnum(int q)
/* confirms that q is a valid Q-store number */
{  if  ( q >= 16 )
      yyerror("Q-store number > 15");
}

int twosylqx(int instr, int q1, int q2)
/* evaluate two syllable Q-store instruction with 2 Q-store numbers in it
   or one Q-store number and a specified lower byte  */
{  if  ( q1 >= 16  ||  q2 >= 16 )
      yyerror("Q-store number > 15");
   return (instr<<8) | (q1<<4) | q2;
}


void store3syl(int instr, int addr)
/* takes the output from evaluate and embeds in the instruction */
/* for memory references the top address bits must already be embedded in instr */
/* any Q-store modifier must already be <<12 and added to addr */
{  int s2 = (addr>>8)&255;
   int s3 = addr&255;
// printf("Raw address is %X\n", addr);
   if  ( listing )
      sprintf(lstpos(memaddr, 3), "%03o %03o %03o",  instr, s2, s3);
   corestore[memaddr++] = instr;
   corestore[memaddr++] = s2;
   corestore[memaddr++] = s3;
}

int twosylshetc(int i0, int n)
/* evaluate two syllable instruction such as a shift */
{  return (i0<<8) | n;
}

void storejump(int s1, int s2, int loc)
/* store a jump to specified location */
{  int syl = loc % 6;
   int word = loc / 6;
   s1 += ((word>>9)&010) + syl;
   s2 += ((word>>8)&017);
   if  ( listing )
      sprintf(lstpos(memaddr, 3), "%03o %03o %03o",  s1, s2, word&255);
   corestore[memaddr++] = s1;
   corestore[memaddr++] = s2;
   corestore[memaddr++] = word;
}


int keepMarker()
/* uses srcmarker to deliver a pointer
   to the first char of a sequence of digits
 */
{  return (srcmarker - buff) - 2;   // delivers value of the src pointer for the parser stack
}

int octalval(int p)
/* delivers an octal value from the pointer p
 */
{  int res = 0;
   char *pp = buff + p;
   while  ( isdigit(*++pp) )
      res = (res<<3) + *pp - '0';
   return res;
}

int addindigit(int d, int r)
/* adds in a digit to vstoreval with radix r */
/* returns last carry digit, which will be 0 unless the value has overflowed */
{  int i = 6;
   int v;
   d -= '0';
   while  ( --i >= 0 )
   {  v = vstoreval[i]*r + d;
      vstoreval[i] = v & 255;
      d = v>>8;
   }
   return d;
}

void addinfractiondigit(int d, unsigned char *f)
/* adds in a digit to vstoreval multiplied by the fraction f */
{  int i = 7;
   int c = 0;
   int v;
   if  ( verbose )
      printf("Adding digit %c at weight %03o %03o %03o %03o %03o %03o %03o\n", d, f[0], f[1], f[2], f[3], f[4], f[5], f[6]);
   d -= '0';
   while  ( --i >= 0 )
   {  v = vstoreval[i] + d*f[i] + c;
      vstoreval[i] = v & 255;
      c = v>>8;
   }
}

void vfraction(int p, int sig)
/* evaluates a decimal number and loads result into 56-bit vstoreval array */
/* result is shifted so that there are sig integral places */
{  if  ( sig >= 48 )
      sig -= 47;     // printf("*** %d significant digits is too many\n", sig); just assemble single const
   if  ( ! firstpass )
   {  unsigned char *pp = buff + p;
      int vr = 0;                 // 'overflow' register
      memset(vstoreval, 0, 7);
      while  ( isdigit(*++pp)  ||  *pp == ' ' )
         if  ( *pp != ' ' )
            vr += addindigit(*pp, 10);
      if  ( sig != 47  ||  *pp == '.' )   // allow for V0 = 234.6; etc - silly but valid
      {  int s = 47 - sig;        // amount to shift left
         int x = s&7;             // extra shift after byte move
         {  int j = 0;
            s = s>>3;             // number of bytes to shift
            while  ( s < 6 )
               vstoreval[j++] = vstoreval[s++];
            while  ( j < 6 )
               vstoreval[j++] = 0;
         }
         if  ( x != 0 )           // not multiple of 8 -- need to shuffle up a bit
            vr += addindigit('0', 2<<(x-1)); // lazy, but efficiency is not a concern here

         if  ( *pp == '.' )           // if there is a fraction part
         {  unsigned char fpart[7];   // starts out as 1.0 correctly aligned
            memset(fpart, 0, 7);
            sig = 5 - ((47 - sig)>>3);// position of units digit in result
            if  ( sig >= 0)
               fpart[sig] = 1<<x;     // 1.0 aligned in fpart
            else                      // must be a small number
               fpart[0] = 1<<x;       // 1.0 aligned max left -- realigned later
         // printf("!!!! sig = %d %c%c%c%c%c\n", sig, pp[0], pp[1], pp[2], pp[3], pp[4]);
         // printf("fpart = %03o %03o %03o %03o %03o %03o %03o\n",
         //                      fpart[0], fpart[1], fpart[2], fpart[3], fpart[4], fpart[5], fpart[6]);
            while  ( isdigit(*++pp)  ||  *pp == ' ' )
               if  ( *pp != ' ' )
               {  int j;
                  s = 0;              // next divide fpart by 10
                  for  ( j = 0; j<7; j++ )
                  {  s = (fpart[j] + s*256);
                     fpart[j] = s/10;
                     s = s%10;
                  }
                  if  ( sig < 0  &&  fpart[0] == 0 )   // if OK to move up significance
                  {  sig ++;
                     for  ( j = 0; j<6; j++ )
                        fpart[j] = fpart[j+1];
                     fpart[6] = 0;
                  // printf("char = %c  fpart = %03o %03o %03o %03o %03o %03o %03o\n", *pp,
                  //             fpart[0], fpart[1], fpart[2], fpart[3], fpart[4], fpart[5], fpart[6]);
                  }
                  if  ( *pp != '0' )
                  {  if  ( sig < 0 )                   // non-zero digit encountered while scale factor out of range
                        vr ++;                         // overflow
                     else
                        addinfractiondigit(*pp, fpart);// add in good digit
                  }
               }
            if  ( (vstoreval[6]&128) != 0 )    // if we need round up
               vr += addindigit('1', 1); // lazy, but efficiency is not a concern here
         }
      }
      if  ( vr != 0 )
         printf("*** V-store value overflows\n");
   }
   if  ( verbose )
      printf("Evaluated %03o %03o %03o %03o %03o %03o %03o\n",
                   vstoreval[0], vstoreval[1], vstoreval[2], vstoreval[3], vstoreval[4], vstoreval[5], vstoreval[6]);
}


int negatevstore()
/* negates the vstore value deposited by vfraction, etc */
/* returns 0 if V-store vale was zero -- used for F-0.0 */
/* N.B. this has already been rounded to 48 bits by vfraction */
{  int i = 6;
   int v;
   int c = 0;             // carry digit
   int zz = 0;            // tester for zero
   while  ( --i >= 0 )
   {  v = -vstoreval[i] - c;
      zz += (vstoreval[i] = v & 255);
      c = (v>>8) & 1;
   }
   return zz;
}

int sig;           // floating point exponent -- see below

void vfloat(int p, int exp)
/* evalues floating point V-store value */
/* p points at digit string, and exp is the value following subscript 10 */
/* The value is assembled into vstoreval to 39 integral places, ready for conversion to floating point by vfppos or vfpneg */
{  if  ( ! firstpass )
   {  char *pp = buff + p;
      int ipart = 0;
      int dcount = 0;
      sig = 8;                // number of significant bits needed to accommodate the integer part ...
      while  ( isdigit(*++pp)  ||  *pp == ' ' )     // ... at the right point in the word for FP conversion
         if  ( *pp != ' ' )
         {  ipart = ipart*10 + *pp - '0';
            dcount ++;
         }
      if  ( exp != 0 )
      {  int q = endbuff;
         pp = buff + p;
         while  ( dcount + exp > 0 )    // there are significant digits to the left of the decimal point
            if  ( isdigit(*++pp) )
            {  dcount --;
               buff[q++] = *pp;
            }
         buff[q++] = '.';               // put in the decimal point
         while  ( dcount + exp < 0 )    // we need zeroes after the decimal point
         {  dcount ++;
            buff[q++] = '0';
         }
         while  ( isdigit(*++pp)  ||  *pp == ' '  ||  *pp == '.' )
            if  ( isdigit(*pp) )
               buff[q++] = *pp;         // copy the rest of the number
         buff[q++] = ';';               // terminate the number
         buff[q++] = 0;                 // terminate the string
      // printf("V-store equivalent number is %s\n", buff + endbuff);
         vfloat(endbuff-1, 0);
         return;
      }
      if  ( ipart != 0 )
      {  while  ( ipart != 0 )   // find out what precision we need
         {  sig ++;
            ipart = ipart >> 1;
         }
      }
      else if  ( *pp == '.' )    // number is less than one
      {  int zc = 0;             // count of leading zeros
      // printf("Small number 0.%c%c%c\n", pp[1],pp[2], pp[3]);
         while  ( *++pp == '0' )
            zc ++;
         sig -= zc * 3;          // allow 3 bits for each decimal digit ...
         sig -= zc/3;            // ... but assume 1000 needs 10 bits
      }
      if  ( verbose )
         printf("Using %d significant digits\n", sig);
      vfraction(p, sig);          // assemble number as fixed point to maximum possible significance
      if  ( verbose )
         printf("vstoreval = %03o %03o %03o %03o %03o %03o %03o\n",
                     vstoreval[0], vstoreval[1], vstoreval[2], vstoreval[3], vstoreval[4], vstoreval[5], vstoreval[6]);
   }
}

void vfppos()
/* stores positive floating point V-store value that is already assembled in fixed point form -- see above */
{  int w = sig + 120;
   int zz = 0;                           // will be zero if number was exactly zero
   if  ( (vstoreval[1] & 0100) == 0 )    // we need to shift 1 left to standardise - or it might be zero
   {  int c = 0;
      int i = 6;
      w --;                              // also shrink the exponent
      while  ( --i > 0 )
      {  int v = (vstoreval[i]<<1) + c;  // shifted up one place
         c = (v>>8) & 1;
         vstoreval[i] = v & 255;         // and carry suitably propagated
         zz += v;
      }
      if  ( zz == 0 )
         return;                         // just leave the all zeroes in vstoreval
   }
   vstoreval[0] = w >> 1;
   if  ( (w&1) != 0 )
      vstoreval[1] |= 128;               // bottom bit of exponent
}

void vfpneg()
/* stores negative floating point V-store value -- see above */
{  int w = sig + 120;
   if  ( negatevstore() == 0 )           // negate and test for zero
      return;                            // leaves zero result in vstoreval
   while  ( (vstoreval[1] & 0100) != 0 ) // we need to shift 1 left to standardise -- or maybe 2??
   {  int c = 0;
      int i = 6;
      w --;                              // also shrink the exponent
      while  ( --i > 0 )
      {  int v = (vstoreval[i]<<1) + c;  // shifted up one place
         c = (v>>8) & 1;
         vstoreval[i] = v & 255;         // and carry suitably propagated
      }
   }
   vstoreval[0] = (w >> 1) + 128;        // force in the sign digit
   if  ( (w&1) == 0 )
      vstoreval[1] &= 127;               // bottom bit of exponent cleared to 0
}

void voctal(int p, int sig)
/* ditto octal */
{  if  ( ! firstpass )
   {  char *pp = buff + p;
      memset(vstoreval, 0, 6);
      if  ( verbose )
         printf("enter voctal(%d, %d)\n", p, sig);

      while  ( isdigit(*++pp)  ||  *pp == ' ' )
         if  ( *pp != ' ' )
            addindigit(*pp, 8);
      if  ( sig != 47 )
      {  int s = 47 - sig;        // amount to shift left
         int x = s&7;             // extra shift after byte move
         {  int j = 0;
            s = s>>3;             // number of bytes to shift
            while  ( s < 6 )
               vstoreval[j++] = vstoreval[s++];
            while  ( j < 6 )
               vstoreval[j++] = 0;
         }

         if  ( x != 0 )           // not multiple of 8 -- need to shuffle up a bit
            addindigit('0', 2<<(x-1)); // lazy, but efficiency is not a concern here
      }
      if  ( verbose )
         printf("leave voctal(%d, %d)\n", p, sig);
   }
}

void vqformat(int p1, int p2, int p3)
/* constructs a Q-store format V-store value into vstoreval */
{  if  ( ! firstpass )
   {  vstoreval[0] = (p1>>8)&255;
      vstoreval[1] = p1&255;
      vstoreval[2] = (p2>>8)&255;
      vstoreval[3] = p2&255;
      vstoreval[4] = (p3>>8)&255;
      vstoreval[5] = p3&255;
   }
}


