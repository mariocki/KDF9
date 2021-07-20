// Under development -- always

// June 2020 -- new facility for producing standard from of ABSes on paper tape

// Usage: mkchan [switches] input_file output_file

// The type of output is defined by the output_file extension (i.e. bit after the dot)
//     .mt       // POST format data in MT form for DH's emulator
//     .pt       // POST format data in paper tape that can be uploaded to MT by KAB81
//     .dsk      // PROMPT format data in emulated disk blocks -- mainly for KAL4
//     .txt      // Paper tape Latin-1 representation of KDF9 input chars 1 for 1

// The input file is Algol Basic Symbols represented as per http://settle.ddns.net/KDF9/chars.htm
// In addition lots of other synonyms are accepted.
// The design aim is that almost any valid Algol program will be converted into input suitable for KDF9

// Switches:
//     -v           // increment verbosity
//     -b           // specify block size -- probably obsolete but can generate intermediate tapes - maybe
//     -s           // star specified as asterisk - special for KAL4 bootstrap
//     -k           // KDF9 program ID specified for inclusion in PROMPT blocks
//     -l           // this is a library file for incorporation in the system tape

// new facility May 2019, -l parameter to deal with library material

// rewrite of mkchan 30 Jan 2017 -- replaces mkchan.c which is renamed mkchan0.c

// Prog to manufacture text tapes for Kalgol
// copies Algol 60 source to a POST format channel 6 tape
// so that it can be read by brick 01 (KAB01) of the Kidsgrove compiler
// Takes a liberal view and accepts multiple stropping conventions
// Asterisk is multiply, and underline is KDF9 Algol's asterisk
// $ is also acceptable as asterisk.

// Library facility introduced 28 Jan 2017 -- rather crude
// but now obsolete as we have fixed the bug in the KAB01 library inclusion facility
// If the file starts !begin !library A0; or  'begin' 'library' A0; or _b_e_g_i_n _l_i_b_r_a_r_y A0;
// as its first line, a file A0.a60 in the current directory will be incorporated
// There must be no more characters after the semi-colon, and the spaces must be as above. (no initial spaces)
// This was originally a temporary program

// However, a near-complete library insertion mechanism was found in brick 01, despite
// the accurate recollection that library insertion was done in the editing part of POST.
// The original version had a slight bug which has been corrected, so regular library insertion now works
// However the above mechanism has been left in place in case it should
// prove useful in some test situations.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <ctype.h>

#ifndef O_BINARY
#define O_BINARY  0
#endif

#define NSYM  2000

unsigned char symval[NSYM];        // the value of the ABS
char *symchars[NSYM];              // the same symbol in characters

unsigned char *pttab[256];         // character form of each basic symbol

int ulform = 1;          // compound symbols have embedded underlines
int excform = 1;         // compound symbols have initial exclamation mark
int strform = 1;         // compound symbols have single quote strop marks at each end

int nsyms = 0;
int verbose = 0;
int libmarker = 0;
int endspace = 2;        // space at end of block for check sum -- also prog id in PROMPT

char *heading = NULL;    // heading needed for library files which are done in 2 passes

FILE *diag;

char buff[3846];
char opbuff[20];

unsigned char store[640*6 + 6 + 900];        // 640 words in case of PROMPT output + 1 word for MT header

void notyetimplemented(int n, char *s)
// temp
{  printf("Not implemented: %s\n", s);
   exit(1);
}

void ulsym(int s, char *chs)
// create entry for an underlined word
// introduced by ! in ASCII
{  strcpy(buff+1, chs);
   if  ( excform != 0 )        // exclamation mark in front of compound symbol (Eldon2 TTY style)
   {  symval[nsyms] = s;
      buff[0] = '!';
      symchars[nsyms++] = strdup(buff);
   }
   if  ( strform != 0 )        // single quote as strop mark
   {  symval[nsyms] = s;
      buff[0] = '\'';
      strcat(buff+3, "'");     // buff contains stropped form of ABS
      symchars[nsyms++] = strdup(buff);
   }
   if  ( ulform != 0 )         // underline chars before every character
   {  int i = 0;
      symval[nsyms] = s;
      chs --;
      while  ( *++chs != 0 )
      {  buff[i++] = '_';
         buff[i++] = *chs;
      }
      buff[i] = 0;     // terminate string
      pttab[s] = symchars[nsyms++] = strdup(buff);
   }
}

void abs2char(char *chs, int abs)
// makes entry for ABS that is 2 characters
{  symval[nsyms] = abs;
   pttab[abs] = symchars[nsyms++] = chs;
}

void abs8bit(unsigned char ch, int abs)
// makes entries for abs that is a single character
{  symval[nsyms] = abs;
   buff[0] = ch;
   buff[1] = 0;
   pttab[abs] = symchars[nsyms++] = strdup(buff);
}

int convline(char *dataline, int loc)
// convert the line of ASCII characters in dataline
// into a POST format line in memory, starting at loc
// result is the next free location in store
{  int j = loc*6;                      // counter along the output line
   int p = 0;                          // counter along the input line
   int i;
   unsigned char wc, wwc;

   while  ( (wc = dataline[p]) != 0 )
   {  i = nsyms;
      while  ( --i > 0  &&  memcmp(dataline + p, symchars[i], strlen(symchars[i])) != 0 ) ;

      if  ( (wc = symval[i]) < 255 )
         store[++j] = wc;
      else
         store[++j] = 0216;       // * - not a true ABS and should cause syntax erros for invalied symbol

//    fprintf(stdout, "i = %d  symval = %03o symchars = %s\n", i, symval[i], symchars[i]);
//    fprintf(stdout, "recognised %03o %s\n", wc&255, dataline+p);
      p += strlen(symchars[i]);

   }

// fprintf(diag, "Data: %s\n", dataline);
   if  ( store[j] != 0240 )       // last char not \n
      notyetimplemented(store[j], "over length line");
   while  ( j%6 != 5 )
      store[j++] = 0377;          // pad last word with dummies
   store[j++] = 0240;
   j /= 6;                        // convert from bytes to words
   store[loc*6] = j - loc         // number of words in the line
               + libmarker;       // flag to indicate a ibrary line
   return j;
}

void settables(int starval)
// create the conversion tables
{  int i, j;
   char *ws, wc;

   abs8bit('?', 0377);          // unrcognised chars deliver a dummy
                                // location 0 holds to query to match anything

   for  ( i = 'A'; i<='Z'; i++ )
      abs8bit(i, 014 - 'A' + i);
   for  ( i = 'a'; i<='z'; i++ )
      abs8bit(i, 046 - 'a' + i);
   for  ( i = '0'; i<='9'; i++ )
      abs8bit(i, i - '0');

   abs8bit('*', starval);        // * is multiply by default
   abs8bit('_', 0216);           // _ is star
   abs8bit('$', 0216);           // $ is star

   abs8bit('?', 0377);        // compound ABS ???
// abs8bit(' ', 0377);        // ignore spaces in string constants -- what was this for
   abs8bit(' ', 0236);        // space
   abs8bit('\t', 0256);       // tab
   abs8bit('\n', 0240);       // newline

   abs2char(":=", 0265);      // :=
   abs2char(">=", 0262);      // >= for greater than or equals
   abs2char("<=", 0222);      // <= for less than or equals

   ulsym(0300, "ALGOL");
   ulsym(0360, "EXIT");
   ulsym(0260, "KDF9");

   ulsym(0223, "and");
   ulsym(0110, "array");
   ulsym(0214, "begin");
   ulsym(0103, "boolean");
   ulsym(0103, "Boolean");
   ulsym(0200, "comment");
   ulsym(0221, "div");
   ulsym(0326, "do");
   ulsym(0245, "else");
   ulsym(0234, "end");
   ulsym(0303, "eqv");
   ulsym(0315, "false");
   ulsym(0206, "for");
   ulsym(0210, "goto");
   ulsym(0302, "gt");
   ulsym(0205, "if");
   ulsym(0263, "imp");
   ulsym(0102, "integer");
   ulsym(0171, "label");
   ulsym(0320, "library");
   ulsym(0202, "lt");
   ulsym(0322, "ne");
   ulsym(0203, "not");
   ulsym(0243, "or");
   ulsym(0217, "own");
   ulsym(0120, "procedure");
// ulsym(0215, "q");             bad idea
   ulsym(0101, "real");
   ulsym(0340, "segment");
   ulsym(0266, "step");
   ulsym(0172, "string");
   ulsym(0130, "switch");
   ulsym(0225, "then");
   ulsym(0335, "true");
// ulsym(0235, "u");
   ulsym(0306, "until");
   ulsym(0237, "value");
   ulsym(0226, "while");

// Many (all?) of the following symbols have synonyms
// For output only the last one counts

   abs2char("!=", 0322);      // != for not equals
   abs8bit('#', 0322);        // not equals
   abs8bit(0xB1, 0322);       // not equals as plus-minus sign

   ulsym(0201, "up");         // up arrow (c.f. updater listing)
   abs8bit('^', 0201);        // was comment -- why?
   abs8bit(0x5E, 0201);       // ^ up arrow

   abs8bit('%', 0221);
   abs8bit(0xF7, 0221);        // '÷' upsets Mac cc

   abs8bit(0xD7, 0261);        // '×' upsets Mac cc

   abs8bit('.', 013);
   abs8bit('<', 0202);
   abs8bit('(', 0204);
   abs8bit('[', 0211);
   abs8bit('{', 0215);

   abs8bit(')', 0224);
   abs8bit(';', 0230);
   abs8bit(']', 0231);
   abs8bit('}', 0235);
   abs8bit(' ', 0236);
   abs8bit('/', 0241);
   abs8bit('=', 0242);
   abs8bit(',', 0246);
   abs8bit(':', 0271);
   abs8bit('+', 0301);
   abs8bit('>', 0302);
   abs8bit('-', 0321);

   ulsym(0262, "ge");
   ulsym(0222, "le");
   if  ( ulform != 0 )        // underline chars form needs _[ etc
   {  abs2char("_[", 0215);   // string quotes
      abs2char("_]", 0235);
      abs2char("_>", 0262);   // _> for greater than or equals
      abs2char("_<", 0222);   // _< for less than or equals
   }
   pttab[0216] = "*";         // spaces in Algol strings, etc

// chars with no obvious ASCII representation
   abs8bit('@', 012);      // subscript 10 - deprecated or obsolete
   abs8bit('~', 012);      // subscript 10 - 5-hole PT is underlined tilde
   abs8bit('\\', 012);     // subscript 10 - as in Algol 68
   abs8bit(0xBA, 012);     // subscript 10 - masculine ordinal indicator
   abs8bit('|', 0276);     // end message

// now to sort so that we search the longest first
// simple exchange sort
   i = nsyms;
   while  ( --i >= 2 )              // leave location 0 alone
   {  j = i;
      while  ( --j >= 1 )
         if  ( strlen(symchars[i]) < strlen(symchars[j]) )
         {  ws = symchars[i];
            symchars[i] = symchars[j];
            symchars[j] = ws;
            wc = symval[i];
            symval[i] = symval[j];
            symval[j] = wc;
         }
   }
}

unsigned char emword[] = { 1,0377,0377,0377,0276,0240 };     // end message word at end of program
unsigned char blank[] = { 0201,0377,0377,0377,0377,0240 };   // blank line
unsigned char begin[] = { 0001,0214,0377,0377,0377,0240 };   // just begin newline -- before library inclusion

void mtwrite(int dvout, int sz, int bsz, int blknum)
// write out the current block after padding with dummies
// still needs change for PROMPT
{  int i, w, ww;
   w = bsz - sz - endspace;             // number words of unused space - 1
   if  ( verbose != 0 )
   {  printf("%d words generated\n", sz-1);
      printf("Number of spare words = %d\n", w+1);
   }
   sz *= 6;                      // convert to byte address
   i = w%63 + 1;                 // if i > 63 we first get a short dummy then 63 word dummies
   w = (bsz-endspace+1) * 6;
   while  ( sz < w )
   {  store[sz++] = i | 0300;
      i = 077;
   }
   i = 6*(bsz - 1 );             // address of double-length sumcheck words in bytes
   store[i] = blknum>>8;
   store[i+1] = blknum&255;//block number in Bits 0-15 of penultimate word
   if  ( verbose >= 2 )
      for  ( i = 1; i<bsz+1; i++ )
         printf("  %o %o %o %o %o %o\n", store[i*6], store[i*6+1], store[i*6+2], store[i*6+3], store[i*6+4], store[i*6+5]);
   sz = bsz*6;
   store[2] = 0;   store[3] = 0;   store[4] = sz>>8;   store[5] = sz&255; // MT header
   if  ( blknum >= 0 )
      write(dvout, store+2, bsz*6 + 4);
   else    //   write(dvout, store+6, bsz*6);
   {  for  ( i = 0; i<bsz; i ++ )
      {  w = (((store[i*6 + 6] << 8) + store[i*6 + 7]) << 8) + store[i*6 + 8];
         ww = (((store[i*6 + 9] << 8) + store[i*6 + 10]) << 8) + store[i*6 + 11];
         sprintf(opbuff, "%08o %08o\n", w, ww);
         write(dvout, opbuff, strlen(opbuff));
      }
      write(dvout, "\n", 1);
   }
}

int prepBlock(int bsz, int hwm, int blknum, char *progid, int noc)
{  int i, sz;

   if  ( bsz != 640 )      // not PROMPT output
      return 1;            // start a new MT block at this address

   i = 6;                  // byte address of 2-word PROMPT header
   if  ( blknum == 1 )     // alpha-block
      i = 81 * 6;          // PROMPT header is 80 words
   memset(store+i, 0, 12);
   store[i+1] = store[i+5] = noc;       // N.O.C.- assume < 256 blocks
   sz = hwm - i/6 - 2;     // number of useful words in the block
   store[i+10] = sz >> 8;  // cannot be more than 2 bytes
   store[i+11] = sz & 0377;
   memcpy(store + (bsz - 3) * 6, progid, 12);    // program ID at end of every block
   return 3;               // need 2 extra words for NOC (next on chain) and filler word
}


int main(int argc, char **argv)
{  int dvout;
   int i, sz, sz0, wc;
   int bsz = 32;          // block size in KDF9 words
   char *eof;             // become NULL whan at end of file
   int blknum = 1;        // block number to be inserted in each block or -ve for paper tape binary output
   char *fnout, *fnin;
   int noc = 2;           // next on chain - for chaining "disk" blocks together
   char progid[] = "ZZZZZDH004PU";       // program identifier in KDF9 chars
   int starabs = 0261;
   char *libid = NULL;

   i = 0;
   while  ( ++i < argc  &&  *(fnin = argv[i]) == '-' )  // process switches
      if  ( fnin[1] == 'v' )                     // increment verbosity
         verbose ++;
      else if  ( fnin[1] == 'b' )                // block size specified
         bsz = atoi(fnin+2);
      else if  ( fnin[1] == 's' )                // star specified as asterisk - special for KAL4 bootstrap
         starabs = 0216;
      else if  ( fnin[1] == 'k' )                // KDF9 program ID specified
         memcpy(progid, fnin+2, 12);
      else if  ( fnin[1] == 'l' )                // this is a library file for the system tape
         libid = fnin+2;                         // may put the name here later

   if  ( argc < i + 2 )   // not enough parameters
      fprintf(stderr, "Usage: %s [-b block_size] [-v] source_file output_file\n", *argv);
   else
   {  FILE *fin = fopen(fnin, "r");
      FILE *fin0 = NULL;               // keeps proper file during library extraction

      if  ( fin == NULL )
      {  perror(fnin);
         exit(1);
      }

      fnout = argv[i+1];                // open output file here, to generate an empty file if input duff

      dvout = open(fnout, O_WRONLY + O_BINARY + O_CREAT + O_TRUNC, 0640);
      if  ( dvout < 0 )
      {  perror(fnout);
         exit(1);
      }

      settables(starabs);           // organise conversion tables

      if  ( heading != NULL )           // if this is the second pass for a library file
         write(dvout, heading, strlen(heading));

      if  ( strcmp(fnout + strlen(fnout) - 3, ".pt") == 0 )    // simulated MT output on paper tape
      {  printf("Paper tape output -  MT image\n");
         blknum = -1000000;  // likely always to stay -ve
         if  ( libid != NULL )         // making a library file
            libmarker = 040;
      }
      else if  ( strcmp(fnout + strlen(fnout) - 4, ".txt") == 0 )    // Latin-1 version of KDF9 PT chars e.g. for walgol
      {  printf("Paper tape output - ISO Latin-1\n");
         blknum = -2100000;  // very unlikely ever to occur by accident
      }
      else if  ( strcmp(fnout + strlen(fnout) - 4, ".dsk") == 0 )    // simulated output as PROMPT file(s)
      {  printf("PROMPT output\n");
         bsz = 640;                    // 640 word PROMPT blocks
         endspace = 4;                 // need 2 extra words for prog identifier

         for  ( i = 0; i<12; i++ )
            if  ( (wc = progid[i]) <= '9' )
               progid[i] = wc&017 | 020;
            else if  ( (wc = progid[i]) >= 'A' )
               progid[i] = wc&037 | 040;

         i = sz = 12;
         while  ( i >= 0 )
         {  i -= 4;
            wc = (progid[i]<<18) + (progid[i+1]<<12) + (progid[i+2]<<6) + progid[i+3];
            progid[--sz] = wc;
            progid[--sz] = wc>>8;
            progid[--sz] = wc>>16;
         }

         sz = 07026402;   progid[0] = sz>>16;  progid[1] = sz>>8;  progid[2] = sz;
         memset(store, 0, sizeof(store));    // clear memory for easier diagnostics and clear ASL

      // Simulate an index with just one file
         store[6] = 0;             // Q-store to scan free entries in this block -- from memory !!!
         store[7] = 211;           // counter part = 211
         store[8] = 0;
         store[9] = 3;             // increment part = 3
         store[10] = 0;
         store[11] = 1;             // modifier part = 1
         memcpy(store+12, progid, 12);          // program identifier in start of alpha-block
         store[24] = 0;
         store[25] = 1;             // fictitious disc address of 1 / 1 / 1
         store[26] = 0;
         store[27] = 1;             // fictitious disc address of 1 / 1 / 1
         store[28] = 0;
         store[29] = 1;             // fictitious disc address of 1 / 1 / 1

         store[5] = (6*640) & 255;      // block size for tape header
         store[4] = (6*640) >> 8;

         write(dvout, store+2, bsz*6 + 4);     // writes a bogud index block

         memset(store, 0, 30);                 // clear memory for easier diagnostics and clear ASL
         memcpy(store+6, progid, 12);          // program identifier in start of alpha-block
      }

      diag = stdout;

      while  ( (eof = fgets(buff, 6*256, fin)) != NULL  &&  strlen(buff) < 3 ) ;
                                       // skip blank lines

      if  ( eof == NULL )
      {  printf("No program input\n");
         exit(1);
      }

      if  ( bsz == 640 )
         sz = 83;                              // leave space for ASL in alpha-block (not implemented)
      else
         sz = 1;

      if  ( blknum >= 0 )              // only allow library incorporation with MT format output
      {  if  ( memcmp(buff, "!begin !library", sz0 = 15) == 0        // library incorporation needed
           ||  memcmp(buff, "'begin' 'library'", sz0 = 17) == 0      // library incorporation needed
           ||  memcmp(buff, "_b_e_g_i_n _l_i_b_r_a_r_y", sz0 = 25) == 0 )
         {  fin0 = fin;
            libmarker = 040;              // sets library bit in each line copied
            i = strlen(buff);
            while  ( buff[--i] <= ' ' ) ;
            strcpy(buff+i, ".a60");       // should overwrite semi-colon
            fin = fopen(buff+sz0+1, "r");
            if  ( fin == NULL )
            {  perror(buff+sz0+1);
               exit(1);
            }
            printf("Opened library file %s\n", buff+16);
            eof = fgets(buff, 6*256, fin);
            memcpy(store + sz*6, begin, 6);       // first line just has begin
            sz ++;
         }
      }

      while  ( eof != NULL  && memcmp(buff, "****", 4) != 0 )
      {  i = strlen(buff);
         while  ( --i >= 0  &&  buff[i] <= ' ' ) ;
         buff[++i] = '\n';             // chop off trailing white space
         buff[i+1] = 0;                // chop off trailing white space
         if  ( verbose != 0 )
            printf("HWM = %2d  Input: %s", sz, buff);
         sz0 = sz;

         if  ( i == 0 )                // non-printing line
            memcpy(store + 6*(sz++), blank, 6);
         else
            sz = convline(buff, sz0);

         if  ( blknum < -1000000 )          // Latin-1 paper tape output
         {  i = 0;                          // pointer along output
       // printf("Line  in: %s\n", buff);
       // printf("Line ABS: %o %o %o %o %o %o %o %o %o %o\n",
       //         store[0], store[1], store[2], store[3], store[4], store[5], store[6], store[7], store[8], store[9], store[10]);
            sz *= 6;
            for  ( sz0 = 7; sz0<sz; sz0++ )
               if  ( (wc = store[sz0]) != 255 )      // ignore padding dummies
               {  eof = pttab[wc];                   // canonical character form of symbol
                  strcpy(buff+i, eof);
                  i += strlen(eof);
               }
       // buff[i] = 0; printf("Line out: %s\n", buff);
            write(dvout, buff, i);          // one line at a time -- crlf is handled in pttab[0240]
            eof = fgets(buff, 6*256, fin);  // read next line
            sz = 1;
         }
         else if  ( sz > bsz - endspace )   // no room for this record -- should this be bsz - 2?
         {  sz = prepBlock(bsz, sz0, blknum, progid, noc++);
            mtwrite(dvout, sz0, bsz, blknum++);  // so write an output block
         // printf("Written block %d\n", blknum-1);
         }
         else                          // read more input
         {  eof = fgets(buff, 6*256, fin);
            if  ( memcmp(buff, "****", 4 ) == 0 )   // chop of end of library during development
               eof = NULL;
            if  ( eof == NULL  &&  fin0 != NULL )   // end of library
            {  fclose(fin);
               fin = fin0;  fin0 = NULL;
               eof = fgets(buff, 6*256, fin);
               libmarker = 0;

            }
         }
// printf("sample: %03o %03o %03o %03o %03o %03o\n", store[24], store[25], store[26], store[27], store[28], store[29]);
      }
      if  ( sz >= bsz - 3 )            // no room for the end message line
      {  sz0 = sz;
         sz = prepBlock(bsz, sz0, blknum, progid, noc++);
         mtwrite(dvout, sz0, bsz, blknum++);     // so write an output block
      }
      memcpy(store + sz*6, emword, 6);
      prepBlock(bsz, ++sz, blknum, progid, 0);   // noc = 0 marks end of chain of file blocks
      mtwrite(dvout, sz, bsz, blknum);           // write final output block, adding in word for em
      if  ( blknum >= 0 )                        // writing MT format
         close(dvout);
      else if  ( libid != NULL  &&  heading == NULL )
      {  close(dvout);                           // end of first pass -- file will be overwritten
         if  ( strlen(libid) != 5 )              // not specified in -l switch
            sprintf((libid = buff), "%c%04d", *fnout, atoi(fnout+1));
         sprintf(buff+10, "R %s %d blocks %s\n\n", libid, blknum + 1000001, fnin);
         heading = strdup(buff+10);
      // printf("Second pas for %s\n", heading);
         main(argc, argv);                       // second pass
      }
      else
      {  write(dvout, ";;;;@|@|\n", 9);          // terminator on paper tape
         printf("%d blocks written\n", blknum + 1000001);
         close(dvout);
      }

      exit(0);
   }
}
