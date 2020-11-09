/* A Bison parser, made by GNU Bison 1.875.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     UNSIGNED_INTEGER = 258,
     COMMENT = 259
   };
#endif
#define UNSIGNED_INTEGER 258
#define COMMENT 259




/* Copy the first part of user declarations.  */
#line 1 "kal3.y"

#include <ctype.h>
/* KDF9 assembler for USERCODE */
#include <stdio.h>

extern yylex();
extern yyerror(char *s);
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

extern void store3syl(int instr, int addr);
/* takes the output from data loc and embeds in the instruction */

extern int dataloc(int pno, int vno);
/* get data location as word address for a V-store */

extern int ystoreloc(int yset, int yno);
/* get ystore location
   yset will be the letter for YA YB etc
 */

extern newproutine(int pno, int v);
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

extern void setvstore(int vno);
/* sets give V-store from value constructed in vstoreval */

extern void setprinterconst(int v1, int v2);
/* sets a printer constant in a range of V-stores -- involves lexical cheating */

extern void startnewword();
/* inserts dummy instructions up to word boundary */

extern void fixY0(int zero, int zero2, int loc);
/* locates Y0 at specified absolute address, e.g. Y0=E 4896 */



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 196 "y.tab.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  46
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1211

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  47
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  18
/* YYNRULES -- Number of rules. */
#define YYNRULES  378
/* YYNRULES -- Number of states. */
#define YYNSTATES  1074

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   259

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    21,     2,    23,     2,    46,     2,     2,
       2,     2,    25,    30,     2,    19,    32,    22,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    10,
      26,    17,    28,     2,    35,     9,    31,    29,    37,     6,
      34,    13,    38,    39,    20,    40,    18,    14,    24,    12,
      11,    33,     5,     7,     8,    41,    15,    16,    42,    36,
      27,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      43,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,    44,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    45,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short yyprhs[] =
{
       0,     0,     3,     6,    24,    33,    36,    40,    43,    47,
      51,    58,    62,    67,    72,    74,    76,    79,    81,    84,
      88,    91,    95,   100,   105,   111,   117,   122,   128,   135,
     141,   148,   156,   163,   170,   178,   184,   192,   199,   206,
     214,   221,   227,   234,   241,   247,   255,   262,   267,   273,
     279,   286,   292,   299,   305,   312,   319,   327,   335,   339,
     345,   351,   355,   357,   359,   362,   365,   368,   370,   373,
     378,   382,   388,   392,   395,   400,   407,   412,   415,   420,
     427,   432,   439,   442,   447,   450,   457,   465,   473,   478,
     484,   490,   494,   502,   511,   520,   526,   533,   539,   543,
     551,   560,   569,   575,   582,   590,   593,   595,   597,   600,
     603,   606,   609,   612,   615,   618,   621,   624,   627,   630,
     633,   636,   639,   642,   645,   648,   651,   654,   657,   660,
     663,   666,   669,   672,   675,   678,   683,   686,   691,   694,
     697,   700,   706,   713,   720,   728,   735,   743,   751,   760,
     762,   765,   768,   770,   772,   775,   777,   781,   787,   790,
     794,   799,   805,   811,   818,   824,   831,   838,   845,   852,
     860,   868,   876,   878,   881,   889,   897,   906,   914,   923,
     932,   940,   944,   949,   955,   959,   964,   969,   973,   976,
     981,   987,   991,   996,  1001,  1005,  1008,  1013,  1019,  1023,
    1028,  1033,  1037,  1040,  1045,  1051,  1057,  1061,  1067,  1072,
    1080,  1085,  1092,  1099,  1107,  1116,  1121,  1126,  1133,  1141,
    1146,  1151,  1157,  1163,  1168,  1173,  1176,  1182,  1188,  1193,
    1199,  1204,  1209,  1215,  1221,  1228,  1232,  1237,  1240,  1244,
    1248,  1251,  1255,  1259,  1264,  1268,  1272,  1275,  1279,  1283,
    1288,  1292,  1296,  1299,  1303,  1307,  1312,  1316,  1322,  1327,
    1333,  1339,  1346,  1352,  1357,  1362,  1367,  1374,  1381,  1387,
    1391,  1395,  1402,  1406,  1411,  1419,  1425,  1431,  1436,  1441,
    1448,  1454,  1461,  1466,  1472,  1477,  1483,  1488,  1494,  1499,
    1503,  1507,  1511,  1515,  1521,  1527,  1533,  1539,  1545,  1552,
    1558,  1565,  1571,  1578,  1584,  1591,  1599,  1606,  1614,  1621,
    1623,  1628,  1635,  1642,  1648,  1656,  1665,  1675,  1681,  1691,
    1699,  1706,  1713,  1720,  1727,  1734,  1740,  1747,  1754,  1761,
    1768,  1775,  1783,  1790,  1798,  1804,  1810,  1817,  1823,  1830,
    1838,  1846,  1855,  1862,  1869,  1876,  1884,  1892,  1899,  1907,
    1915,  1922,  1929,  1936,  1943,  1950,  1957,  1964,  1971,  1978,
    1985,  1992,  1999,  2006,  2013,  2020,  2027,  2034,  2041,  2048,
    2055,  2062,  2069,  2076,  2083,  2090,  2097,  2104,  2111
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      48,     0,    -1,    49,    59,    -1,     5,     6,     7,     8,
       9,     5,     8,    10,    59,    11,     5,    12,    13,     5,
       9,    14,    10,    -1,    11,     5,    12,    13,     5,     9,
      14,    10,    -1,    50,    49,    -1,    15,     3,    10,    -1,
      15,    10,    -1,    16,     3,    10,    -1,    60,     3,    10,
      -1,    60,     3,    17,     6,     3,    10,    -1,     7,     8,
      10,    -1,     7,     8,     3,    10,    -1,     8,    18,     3,
      10,    -1,     4,    -1,     3,    -1,    19,     3,    -1,     3,
      -1,    11,     3,    -1,     3,    11,     3,    -1,     6,     3,
      -1,    20,    52,    10,    -1,    20,     7,    52,    10,    -1,
      20,    52,    17,    10,    -1,    20,    52,    21,    17,    10,
      -1,    20,    52,    22,    17,    10,    -1,    20,    52,    23,
      10,    -1,    20,    52,    24,     6,    10,    -1,    20,    52,
      25,    24,     6,    10,    -1,    20,    52,    26,    27,    10,
      -1,    20,    52,    18,     8,    27,    10,    -1,    20,    52,
      25,    18,     8,    27,    10,    -1,    20,    52,    28,    17,
      27,    10,    -1,    20,    52,    13,     6,    27,    10,    -1,
      20,    52,    25,    13,     6,    27,    10,    -1,    20,    52,
      28,    27,    10,    -1,    20,    52,    25,    13,     8,    27,
      10,    -1,    20,    52,    13,     8,    27,    10,    -1,    20,
      52,    26,    17,    27,    10,    -1,    20,    52,    25,    18,
       6,    27,    10,    -1,    20,    52,    18,     6,    27,    10,
      -1,    20,    52,    17,    27,    10,    -1,    20,    52,    21,
      17,    27,    10,    -1,    20,    52,    22,    17,    27,    10,
      -1,    20,    52,    23,    27,    10,    -1,    20,    52,    25,
      24,     6,    27,    10,    -1,    20,    52,    24,     6,    27,
      10,    -1,    20,    52,    15,    10,    -1,    20,    52,    24,
      15,    10,    -1,    20,    52,     6,    24,    10,    -1,    20,
      52,    24,     6,    24,    10,    -1,    20,    52,     6,    20,
      10,    -1,    20,    52,    24,     6,    20,    10,    -1,    20,
      52,     8,     5,    10,    -1,    20,    52,    24,     8,     5,
      10,    -1,    20,    52,    29,     3,    27,    10,    -1,    20,
      52,    29,     3,    24,    27,    10,    -1,    20,    52,    29,
       3,    24,    27,     7,    -1,    15,     3,    17,    -1,    15,
       3,    22,     3,    17,    -1,    15,     3,    17,    58,    10,
      -1,    54,    11,    10,    -1,     3,    -1,    51,    -1,    30,
      51,    -1,     9,    61,    -1,    31,    56,    -1,    56,    -1,
      31,    56,    -1,    31,    56,    22,     3,    -1,    56,    32,
       3,    -1,    56,    32,     3,    22,     3,    -1,    56,    22,
       3,    -1,    19,    56,    -1,    19,    56,    32,     3,    -1,
      19,    56,    32,     3,    22,     3,    -1,    19,    56,    22,
       3,    -1,    30,    56,    -1,    30,    56,    32,     3,    -1,
      30,    56,    32,     3,    22,     3,    -1,    30,    56,    22,
       3,    -1,    33,    57,    22,    57,    22,    57,    -1,     9,
      61,    -1,    34,    56,    32,     3,    -1,    34,    56,    -1,
      34,    56,    32,     3,    35,     3,    -1,    34,    56,    32,
       3,    35,    19,     3,    -1,    34,    56,    32,     3,    35,
      30,     3,    -1,    34,    56,    35,     3,    -1,    34,    56,
      35,    19,     3,    -1,    34,    19,    56,    32,     3,    -1,
      34,    19,    56,    -1,    34,    19,    56,    32,     3,    35,
       3,    -1,    34,    19,    56,    32,     3,    35,    19,     3,
      -1,    34,    19,    56,    32,     3,    35,    30,     3,    -1,
      34,    19,    56,    35,     3,    -1,    34,    19,    56,    35,
      19,     3,    -1,    34,    30,    56,    32,     3,    -1,    34,
      30,    56,    -1,    34,    30,    56,    32,     3,    35,     3,
      -1,    34,    30,    56,    32,     3,    35,    19,     3,    -1,
      34,    30,    56,    32,     3,    35,    30,     3,    -1,    34,
      30,    56,    35,     3,    -1,    34,    30,    56,    35,    19,
       3,    -1,    30,    56,    32,     3,    22,    19,     3,    -1,
      59,    64,    -1,    64,    -1,    36,    -1,    36,     9,    -1,
      36,    31,    -1,    36,    29,    -1,    36,    37,    -1,    36,
       6,    -1,    36,    34,    -1,    36,    13,    -1,    36,    38,
      -1,    36,    39,    -1,    36,    20,    -1,    36,    40,    -1,
      36,    18,    -1,    36,    14,    -1,    36,    24,    -1,    36,
      12,    -1,    36,    11,    -1,    36,    33,    -1,    36,     5,
      -1,    36,     7,    -1,    36,     8,    -1,    36,    41,    -1,
      36,    15,    -1,    36,    16,    -1,    36,    42,    -1,    36,
      36,    -1,    36,    27,    -1,    15,    51,    -1,    15,    51,
      11,     3,    -1,     5,    51,    -1,     5,    51,    11,     3,
      -1,     6,    51,    -1,    60,    51,    -1,    16,    51,    -1,
      14,     3,    14,     3,    10,    -1,    14,     3,    14,     3,
      33,    10,    -1,    14,     3,    14,     3,    24,    10,    -1,
      14,     3,    14,     3,    33,    24,    10,    -1,    14,     3,
      14,     3,    38,    10,    -1,    14,     3,    14,     3,    33,
      38,    10,    -1,    14,     3,    14,     3,    38,    24,    10,
      -1,    14,     3,    14,     3,    33,    38,    24,    10,    -1,
       3,    -1,    30,     3,    -1,    19,     3,    -1,    10,    -1,
       4,    -1,     3,    10,    -1,    55,    -1,    11,     3,    10,
      -1,    11,     3,    15,     3,    10,    -1,    61,    10,    -1,
      17,    61,    10,    -1,    61,    14,     3,    10,    -1,    17,
      61,    14,     3,    10,    -1,    61,    14,     3,    33,    10,
      -1,    17,    61,    14,     3,    33,    10,    -1,     7,     6,
       8,     3,    10,    -1,     7,     6,     8,    31,    56,    10,
      -1,     7,     6,     8,    19,     3,    10,    -1,     7,     6,
       8,    30,     3,    10,    -1,     7,     6,     8,     9,    61,
      10,    -1,     7,     6,     8,     9,    61,    41,    10,    -1,
       7,     6,     8,     9,    61,    18,    10,    -1,     7,     6,
       8,     9,    11,     3,    10,    -1,    62,    -1,    17,    62,
      -1,    14,     3,     8,    12,    33,     3,    10,    -1,    39,
       3,     8,    12,    33,     3,    10,    -1,    39,    14,     3,
       8,    12,    33,     3,    10,    -1,    29,     3,     8,    12,
      33,     3,    10,    -1,    29,    14,     3,     8,    12,    33,
       3,    10,    -1,    29,    39,     3,     8,    12,    33,     3,
      10,    -1,    33,     3,     8,    12,    33,     3,    10,    -1,
      15,     5,    10,    -1,    17,     8,     5,    10,    -1,    31,
      39,     8,     7,    10,    -1,    43,    34,    10,    -1,    43,
      37,    34,    10,    -1,    43,    30,    34,    10,    -1,    43,
      37,    10,    -1,    43,    10,    -1,    43,    30,    63,    10,
      -1,    43,    30,    29,     3,    10,    -1,    44,    34,    10,
      -1,    44,    37,    34,    10,    -1,    44,    30,    34,    10,
      -1,    44,    37,    10,    -1,    44,    10,    -1,    44,    30,
      63,    10,    -1,    44,    30,    29,     3,    10,    -1,    42,
      34,    10,    -1,    42,    37,    34,    10,    -1,    42,    30,
      34,    10,    -1,    42,    37,    10,    -1,    42,    10,    -1,
      42,    30,    63,    10,    -1,    42,    30,    29,     3,    10,
      -1,    24,     6,    13,    37,    10,    -1,    12,     5,    10,
      -1,    11,     6,     5,    14,    10,    -1,     8,    12,    31,
      10,    -1,     5,    12,    41,    24,    37,    38,    10,    -1,
      24,     6,    15,    10,    -1,     5,    12,    41,    24,    37,
      10,    -1,    37,    41,    14,    14,    36,    10,    -1,     5,
      12,    41,    24,    37,    34,    10,    -1,     5,    12,    41,
      24,    37,    38,    34,    10,    -1,    19,    37,    34,    10,
      -1,    30,    37,    34,    10,    -1,    34,    18,    12,     9,
       8,    10,    -1,    34,    18,    12,     9,     8,    37,    10,
      -1,     9,    31,     7,    10,    -1,    24,     6,    13,    10,
      -1,     9,    31,     7,    34,    10,    -1,    24,     6,    13,
      34,    10,    -1,    14,     9,    42,    10,    -1,    24,    12,
       8,    10,    -1,    19,    10,    -1,     7,    39,    13,    24,
      10,    -1,    27,     6,     5,    12,    10,    -1,    37,    41,
      11,    10,    -1,    37,    41,    11,    37,    10,    -1,    34,
      39,    42,    10,    -1,     7,     8,     5,    10,    -1,    29,
      12,    24,     8,    10,    -1,     5,     6,    15,    37,    10,
      -1,     6,     5,     9,     7,     6,    10,    -1,    19,    37,
      10,    -1,     9,    24,    37,    10,    -1,    30,    10,    -1,
      30,    37,    10,    -1,    45,    39,    10,    -1,    45,    10,
      -1,    45,    37,    10,    -1,    45,    34,    10,    -1,    45,
      37,    34,    10,    -1,    45,     5,    10,    -1,    46,    39,
      10,    -1,    46,    10,    -1,    46,    37,    10,    -1,    46,
      34,    10,    -1,    46,    37,    34,    10,    -1,    46,     5,
      10,    -1,    22,    39,    10,    -1,    22,    10,    -1,    22,
      37,    10,    -1,    22,    34,    10,    -1,    22,    37,    34,
      10,    -1,    22,     5,    10,    -1,    37,    39,    15,    39,
      10,    -1,    37,    39,    15,    10,    -1,    37,    39,    15,
      37,    10,    -1,    37,    39,    15,    34,    10,    -1,    37,
      39,    15,    37,    34,    10,    -1,    37,    39,    15,     5,
      10,    -1,     5,     6,    15,    10,    -1,    29,     9,    31,
      10,    -1,    34,     5,    31,    10,    -1,     7,     8,     9,
      24,    37,    10,    -1,    24,     6,    13,    37,    34,    10,
      -1,    14,     9,    42,    34,    10,    -1,    30,    34,    10,
      -1,    19,    34,    10,    -1,     7,    39,    13,    24,    34,
      10,    -1,    40,     3,    10,    -1,    17,    40,     3,    10,
      -1,    34,    39,    24,    39,     7,    38,    10,    -1,    14,
      30,    39,     3,    10,    -1,    14,    19,    39,     3,    10,
      -1,    24,    29,     3,    10,    -1,    37,    29,     3,    10,
      -1,    39,     3,    17,    30,     3,    10,    -1,    39,     3,
      17,     3,    10,    -1,    39,     3,    17,    19,     3,    10,
      -1,    17,    14,     3,    10,    -1,    17,     5,    14,     3,
      10,    -1,    17,    39,     3,    10,    -1,    17,     5,    39,
       3,    10,    -1,    17,    29,     3,    10,    -1,    17,     5,
      29,     3,    10,    -1,    17,    33,     3,    10,    -1,    14,
       3,    10,    -1,    39,     3,    10,    -1,    29,     3,    10,
      -1,    33,     3,    10,    -1,    17,    30,    14,     3,    10,
      -1,    17,    30,    39,     3,    10,    -1,    17,    30,    29,
       3,    10,    -1,    17,    30,    33,     3,    10,    -1,    18,
      39,    24,    40,    10,    -1,    17,    18,    39,    24,    40,
      10,    -1,     7,    38,     9,    63,    10,    -1,     7,    38,
       9,    37,    63,    10,    -1,     7,    38,    18,    63,    10,
      -1,     7,    38,    18,    37,    63,    10,    -1,     7,    38,
      29,    63,    10,    -1,     7,    38,     9,    29,     3,    10,
      -1,     7,    38,     9,    37,    29,     3,    10,    -1,     7,
      38,    18,    29,     3,    10,    -1,     7,    38,    18,    37,
      29,     3,    10,    -1,     7,    38,    29,    29,     3,    10,
      -1,    53,    -1,    12,    41,     8,    10,    -1,     6,    42,
      39,     8,    37,    10,    -1,     6,    42,    39,     8,     3,
      10,    -1,     6,    42,    39,     8,    10,    -1,     6,    42,
      39,     8,     9,    61,    10,    -1,     6,    42,    39,     8,
       9,    11,     3,    10,    -1,     6,    42,    39,     8,     3,
       9,    11,     3,    10,    -1,    29,     8,    33,     3,    10,
      -1,    14,     9,    24,    41,     9,    18,    33,     3,    10,
      -1,    31,    41,     7,    36,    33,     3,    10,    -1,    14,
      18,    31,    33,     3,    10,    -1,    14,    31,     8,    33,
       3,    10,    -1,    11,     9,     5,    33,     3,    10,    -1,
      14,     6,     8,    33,     3,    10,    -1,    14,    34,     5,
      33,     3,    10,    -1,    11,     5,    33,     3,    10,    -1,
      29,    18,    12,    33,     3,    10,    -1,     8,    18,    12,
      33,     3,    10,    -1,    11,     5,    29,    33,     3,    10,
      -1,    14,     5,     6,    33,     3,    10,    -1,    11,     5,
       6,    33,     3,    10,    -1,    11,     5,    29,     6,    33,
       3,    10,    -1,    14,    31,     5,    33,     3,    10,    -1,
      14,    31,     5,     6,    33,     3,    10,    -1,    11,    16,
      33,     3,    10,    -1,     8,    16,    33,     3,    10,    -1,
      11,    16,    29,    33,     3,    10,    -1,    14,    16,    33,
       3,    10,    -1,    14,    18,    16,    33,     3,    10,    -1,
      14,    13,     9,    11,    33,     3,    10,    -1,    11,    13,
       9,    11,    33,     3,    10,    -1,    14,    16,    39,    11,
       6,    33,     3,    10,    -1,    14,    16,     6,    33,     3,
      10,    -1,    11,    16,     6,    33,     3,    10,    -1,     8,
      16,     6,    33,     3,    10,    -1,    14,    18,    16,     6,
      33,     3,    10,    -1,    14,    34,     7,    40,    33,     3,
      10,    -1,    39,    24,     8,    33,     3,    10,    -1,    14,
      31,     7,    40,    33,     3,    10,    -1,    14,     5,    16,
      37,    33,     3,    10,    -1,    11,    39,     9,    33,     3,
      10,    -1,    11,    39,    31,    33,     3,    10,    -1,    11,
      39,    29,    33,     3,    10,    -1,    11,    39,    37,    33,
       3,    10,    -1,    11,    39,     6,    33,     3,    10,    -1,
      11,    39,    34,    33,     3,    10,    -1,    11,    39,    13,
      33,     3,    10,    -1,    11,    39,    38,    33,     3,    10,
      -1,    11,    12,     9,    33,     3,    10,    -1,    11,    12,
      31,    33,     3,    10,    -1,    11,    12,    29,    33,     3,
      10,    -1,    11,    12,    37,    33,     3,    10,    -1,    11,
      12,     6,    33,     3,    10,    -1,    11,    12,    34,    33,
       3,    10,    -1,    11,    14,     9,    33,     3,    10,    -1,
      11,    14,    31,    33,     3,    10,    -1,    11,    14,    29,
      33,     3,    10,    -1,    11,    14,    37,    33,     3,    10,
      -1,    11,    14,     6,    33,     3,    10,    -1,    11,    14,
      34,    33,     3,    10,    -1,    11,    14,    13,    33,     3,
      10,    -1,    11,    14,    38,    33,     3,    10,    -1,    11,
      14,    40,    33,     3,    10,    -1,    11,    14,    18,    33,
       3,    10,    -1,    11,    12,    13,    33,     3,    10,    -1,
      11,    12,    38,    33,     3,    10,    -1,    11,    12,    40,
      33,     3,    10,    -1,    11,    12,    18,    33,     3,    10,
      -1,    25,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   109,   109,   112,   113,   114,   117,   118,   119,   120,
     121,   123,   124,   125,   126,   129,   130,   133,   134,   135,
     136,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   156,   157,
     158,   159,   160,   161,   162,   163,   164,   165,   166,   167,
     168,   169,   170,   171,   172,   173,   174,   175,   178,   179,
     182,   183,   186,   189,   190,   191,   192,   195,   196,   197,
     198,   199,   200,   201,   202,   203,   204,   205,   206,   207,
     208,   209,   210,   211,   212,   213,   214,   215,   216,   217,
     218,   219,   220,   221,   222,   223,   224,   225,   226,   227,
     228,   229,   230,   231,   233,   236,   237,   240,   241,   242,
     243,   244,   245,   246,   247,   248,   249,   250,   251,   252,
     253,   254,   255,   256,   257,   258,   259,   260,   261,   262,
     263,   264,   265,   266,   269,   270,   271,   272,   273,   274,
     275,   278,   279,   280,   281,   282,   283,   284,   285,   288,
     289,   290,   293,   294,   295,   296,   297,   298,   300,   301,
     303,   304,   305,   306,   308,   309,   310,   311,   312,   313,
     314,   315,   317,   318,   320,   321,   322,   323,   324,   325,
     326,   327,   328,   329,   337,   338,   339,   340,   341,   342,
     343,   344,   345,   346,   347,   348,   349,   350,   351,   352,
     353,   354,   355,   356,   357,   358,   359,   360,   361,   362,
     363,   364,   365,   366,   367,   368,   369,   370,   371,   372,
     373,   374,   375,   376,   377,   378,   379,   380,   381,   382,
     383,   384,   385,   386,   387,   388,   389,   390,   391,   392,
     393,   394,   395,   396,   397,   398,   399,   400,   401,   402,
     403,   404,   405,   406,   407,   408,   409,   411,   412,   413,
     414,   415,   416,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   428,   429,   431,   433,   434,   435,   436,   437,
     438,   439,   441,   442,   443,   444,   445,   446,   447,   449,
     450,   451,   452,   454,   455,   456,   457,   459,   460,   462,
     463,   464,   465,   466,   468,   469,   470,   471,   472,   474,
     475,   476,   477,   478,   479,   480,   481,   484,   485,   486,
     487,   488,   489,   490,   491,   492,   493,   494,   495,   496,
     497,   498,   499,   500,   501,   502,   503,   504,   505,   506,
     507,   508,   509,   510,   511,   512,   513,   514,   515,   516,
     519,   520,   521,   522,   523,   524,   525,   526,   528,   529,
     530,   531,   532,   533,   535,   536,   537,   538,   539,   540,
     541,   542,   543,   544,   546,   547,   548,   549,   552
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "UNSIGNED_INTEGER", "COMMENT", "'R'", 
  "'E'", "'S'", "'T'", "'A'", "';'", "'P'", "'O'", "'G'", "'M'", "'V'", 
  "'W'", "'='", "'L'", "'-'", "'J'", "'!'", "'/'", "'#'", "'N'", "'*'", 
  "'<'", "'Z'", "'>'", "'C'", "'+'", "'B'", "'.'", "'Q'", "'F'", "'@'", 
  "'Y'", "'D'", "'H'", "'I'", "'K'", "'U'", "'X'", "'x'", "'×'", "'÷'", 
  "'%'", "$accept", "program", "p0head", "storespec", "integer", 
  "codeloc", "jumpinstr", "vchars", "vspec", "digitsequence", "bit16", 
  "vstoreval", "code", "ystref", "memref", "mqmq", "shiftval", 
  "instruction", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,    82,    69,    83,    84,    65,
      59,    80,    79,    71,    77,    86,    87,    61,    76,    45,
      74,    33,    47,    35,    78,    42,    60,    90,    62,    67,
      43,    66,    46,    81,    70,    64,    89,    68,    72,    73,
      75,    85,    88,   120,   215,   247,    37
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    47,    48,    49,    49,    49,    50,    50,    50,    50,
      50,    50,    50,    50,    50,    51,    51,    52,    52,    52,
      52,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    54,    54,
      55,    55,    56,    57,    57,    57,    57,    58,    58,    58,
      58,    58,    58,    58,    58,    58,    58,    58,    58,    58,
      58,    58,    58,    58,    58,    58,    58,    58,    58,    58,
      58,    58,    58,    58,    58,    58,    58,    58,    58,    58,
      58,    58,    58,    58,    58,    59,    59,    60,    60,    60,
      60,    60,    60,    60,    60,    60,    60,    60,    60,    60,
      60,    60,    60,    60,    60,    60,    60,    60,    60,    60,
      60,    60,    60,    60,    61,    61,    61,    61,    61,    61,
      61,    62,    62,    62,    62,    62,    62,    62,    62,    63,
      63,    63,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     2,    17,     8,     2,     3,     2,     3,     3,
       6,     3,     4,     4,     1,     1,     2,     1,     2,     3,
       2,     3,     4,     4,     5,     5,     4,     5,     6,     5,
       6,     7,     6,     6,     7,     5,     7,     6,     6,     7,
       6,     5,     6,     6,     5,     7,     6,     4,     5,     5,
       6,     5,     6,     5,     6,     6,     7,     7,     3,     5,
       5,     3,     1,     1,     2,     2,     2,     1,     2,     4,
       3,     5,     3,     2,     4,     6,     4,     2,     4,     6,
       4,     6,     2,     4,     2,     6,     7,     7,     4,     5,
       5,     3,     7,     8,     8,     5,     6,     5,     3,     7,
       8,     8,     5,     6,     7,     2,     1,     1,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     4,     2,     4,     2,     2,
       2,     5,     6,     6,     7,     6,     7,     7,     8,     1,
       2,     2,     1,     1,     2,     1,     3,     5,     2,     3,
       4,     5,     5,     6,     5,     6,     6,     6,     6,     7,
       7,     7,     1,     2,     7,     7,     8,     7,     8,     8,
       7,     3,     4,     5,     3,     4,     4,     3,     2,     4,
       5,     3,     4,     4,     3,     2,     4,     5,     3,     4,
       4,     3,     2,     4,     5,     5,     3,     5,     4,     7,
       4,     6,     6,     7,     8,     4,     4,     6,     7,     4,
       4,     5,     5,     4,     4,     2,     5,     5,     4,     5,
       4,     4,     5,     5,     6,     3,     4,     2,     3,     3,
       2,     3,     3,     4,     3,     3,     2,     3,     3,     4,
       3,     3,     2,     3,     3,     4,     3,     5,     4,     5,
       5,     6,     5,     4,     4,     4,     6,     6,     5,     3,
       3,     6,     3,     4,     7,     5,     5,     4,     4,     6,
       5,     6,     4,     5,     4,     5,     4,     5,     4,     3,
       3,     3,     3,     5,     5,     5,     5,     5,     6,     5,
       6,     5,     6,     5,     6,     7,     6,     7,     6,     1,
       4,     6,     6,     5,     7,     8,     9,     5,     9,     7,
       6,     6,     6,     6,     6,     5,     6,     6,     6,     6,
       6,     7,     6,     7,     5,     5,     6,     5,     6,     7,
       7,     8,     6,     6,     6,     7,     7,     6,     7,     7,
       6,     6,     6,     6,     6,     6,     6,     6,     6,     6,
       6,     6,     6,     6,     6,     6,     6,     6,     6,     6,
       6,     6,     6,     6,     6,     6,     6,     6,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned short yydefact[] =
{
       0,    14,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     7,     0,
     125,   112,   126,   127,   108,   123,   122,   114,   120,   129,
     130,   119,   117,   121,   133,   110,   109,   124,   113,   132,
     111,   115,   116,   118,   128,   131,     1,     0,   153,     0,
       0,     0,     0,     0,   152,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   378,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   309,     0,   155,     2,     0,     0,   172,   106,     5,
       0,     0,     0,    11,     0,     0,     6,     8,   154,    15,
       0,     0,     0,   136,     0,     0,   138,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    15,     0,
     134,   140,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   173,     0,   225,     0,     0,    17,
       0,     0,     0,     0,     0,   252,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     237,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   202,     0,     0,     0,
     188,     0,     0,     0,   195,     0,     0,     0,     0,   240,
       0,     0,     0,     0,   246,     0,     0,     0,     0,   105,
     139,   158,     0,     9,     0,     0,    12,    13,     0,     0,
       0,    16,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   156,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   206,     0,     0,
     289,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,   181,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   159,     0,
       0,   270,   235,     0,     0,    20,     0,    18,     0,     0,
      21,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   256,   254,   253,     0,   251,     0,     0,
       0,     0,     0,     0,   291,     0,     0,     0,     0,     0,
       0,   269,   238,     0,     0,     0,     0,   292,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   290,     0,     0,
       0,   272,   149,     0,     0,     0,     0,     0,   198,   201,
       0,     0,     0,     0,   184,   187,     0,     0,     0,     0,
     191,   194,     0,   244,   242,   241,     0,   239,   250,   248,
     247,     0,   245,    61,     0,     0,     0,     0,   263,     0,
       0,   137,     0,     0,     0,     0,     0,     0,     0,   231,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     208,     0,     0,     0,   236,   219,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   310,     0,     0,     0,
       0,     0,     0,   223,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      62,     0,     0,     0,     0,     0,     0,    67,     0,     0,
     135,     0,     0,     0,   182,   282,     0,   286,     0,     0,
       0,     0,   288,   284,   273,     0,     0,   215,    19,    22,
       0,     0,     0,     0,     0,    47,    23,     0,     0,     0,
       0,     0,    26,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   255,   220,     0,     0,   210,
     224,   277,     0,     0,     0,   264,     0,     0,     0,     0,
     216,     0,     0,     0,   265,     0,     0,   230,   278,     0,
     258,     0,     0,     0,   228,     0,     0,     0,     0,     0,
       0,     0,     0,   151,     0,   150,   200,   203,   199,     0,
     186,   189,   185,     0,   193,   196,   192,   243,   249,   160,
       0,     0,     0,     0,   233,     0,     0,     0,     0,   313,
       0,   164,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   299,     0,     0,     0,   301,     0,   303,   226,
       0,     0,   335,     0,   221,   157,     0,     0,     0,   325,
     207,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   334,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   141,     0,     0,     0,     0,
       0,     0,     0,   268,     0,     0,   337,     0,     0,     0,
       0,   276,   275,     0,     0,     0,     0,     0,     0,    82,
      73,    77,    68,     0,     0,     0,    63,     0,     0,     0,
      84,     0,     0,    60,    59,   283,   287,   285,     0,   293,
     295,   296,   294,   161,     0,   297,    51,    49,    53,     0,
       0,    41,     0,     0,    24,     0,    25,     0,    44,    27,
       0,     0,     0,     0,    48,     0,     0,     0,     0,     0,
       0,    29,     0,    35,     0,     0,   222,   205,     0,   227,
       0,   317,   232,     0,     0,     0,   183,     0,     0,     0,
       0,   262,   260,   259,     0,   257,   229,     0,     0,   280,
       0,     0,     0,     0,   204,   190,   197,   162,    10,     0,
       0,   211,     0,     0,   234,     0,   312,     0,     0,   311,
       0,   168,     0,     0,   166,   167,   165,   266,   304,     0,
     300,   306,     0,   302,   308,   271,   344,   327,   330,     0,
     328,   322,   362,   358,   374,   377,   360,   359,   363,   361,
     375,   376,     0,   368,   364,   370,   373,   366,   365,   369,
     367,   371,   372,   343,   336,   354,   350,   356,   352,   351,
     355,   353,   357,     0,   143,   142,     0,     0,   145,     0,
     329,     0,   323,     0,     0,   342,     0,     0,   338,   320,
       0,   332,     0,   321,   324,     0,     0,     0,     0,     0,
       0,    65,    64,    66,     0,    91,    98,     0,     0,    72,
      70,   298,   163,    33,    37,    40,    30,    42,    43,    52,
      50,    46,    54,     0,     0,     0,     0,    28,     0,    38,
      32,     0,    55,   267,     0,     0,   326,     0,     0,     0,
     217,     0,     0,   261,   212,     0,   281,   279,     0,   347,
       0,     4,   213,   209,     0,     0,     0,   314,   171,   170,
     169,   305,   307,   331,   340,   174,   144,   146,     0,   147,
     349,     0,   339,     0,   345,   333,   348,   346,    76,    74,
      80,    78,    69,     0,     0,     0,     0,     0,    83,    88,
       0,     0,    34,    36,    39,    31,    45,    57,    56,   177,
       0,     0,   319,   180,   218,   274,   175,     0,     0,   214,
       0,   315,   148,     0,   341,     0,     0,     0,    90,    95,
       0,    97,   102,     0,     0,    89,    71,   178,   179,   176,
       0,   316,   318,    75,    79,     0,    81,     0,    96,     0,
     103,    85,     0,     0,     0,   104,    92,     0,     0,    99,
       0,     0,    86,    87,     0,    93,    94,   100,   101,     0,
       0,     0,     0,     3
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short yydefgoto[] =
{
      -1,     9,    10,    11,   103,   163,    81,    82,    83,   527,
     737,   528,    84,    85,    86,    87,   397,    88
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -911
static const short yypact[] =
{
     316,  -911,    11,    28,   108,   296,    67,   311,   180,   334,
      94,   316,   323,   467,   560,   494,   520,   509,  -911,   564,
    -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,
    -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,
    -911,  -911,  -911,  -911,  -911,  -911,  -911,   578,  -911,   518,
       8,   280,   553,   544,  -911,     9,    15,   335,   403,   132,
     219,   570,    -5,   555,   192,   477,  -911,   604,    43,   220,
     513,   608,   205,   504,   499,   609,   312,   394,   402,   235,
     256,  -911,   602,  -911,    94,   132,   563,  -911,  -911,  -911,
     538,   606,   605,  -911,   607,   603,  -911,  -911,  -911,  -911,
     610,   577,   616,   611,   612,   585,  -911,   615,   574,   464,
     613,   597,    59,   617,   583,   620,    61,   170,   625,   626,
     233,   623,   247,   310,   299,   624,   627,   568,    63,   628,
     376,   629,   173,   469,   594,   598,   584,   588,   221,   630,
     631,  -911,    39,   132,   634,   638,   132,   614,   640,   448,
     641,   642,   643,   571,  -911,   632,  -911,   637,    52,   639,
     645,   561,   646,   486,   644,  -911,   647,   156,   648,   581,
     651,   649,   650,   589,   618,   633,   636,   658,   653,   659,
    -911,   656,   279,   655,   660,   590,   652,   657,   474,   665,
     661,     5,   112,   667,   663,   662,  -911,   344,   664,   345,
    -911,   361,   668,   377,  -911,   367,   669,   442,   670,  -911,
     671,   444,   672,   675,  -911,   676,   445,   677,   678,  -911,
    -911,  -911,   674,  -911,   683,   681,  -911,  -911,   686,    70,
     673,  -911,   689,   666,   685,   379,   684,   679,    30,    44,
     404,   680,   688,   682,   692,   687,   690,   446,  -911,   693,
     691,   126,   696,   694,   695,   697,   698,   699,   700,   701,
     702,   703,   704,   705,   706,   707,   708,   709,   710,   711,
     712,   713,   714,   715,   716,   717,   718,   719,   720,   721,
     722,   723,   724,   725,   726,   727,   728,  -911,   730,   741,
    -911,   759,   731,   729,   732,   733,   450,   752,   734,   765,
     758,   195,   737,   768,   769,   370,   735,   740,   743,   738,
     326,   774,  -911,   776,   777,   778,   779,   773,   576,   760,
     775,   783,   784,   785,   786,   780,   781,   782,  -911,   790,
     754,  -911,  -911,   787,   792,  -911,   788,  -911,   429,   791,
    -911,   595,   789,    -3,   596,   793,   794,    56,   545,   526,
     188,   511,   797,  -911,  -911,  -911,   795,  -911,   265,   796,
     798,   799,   800,   801,  -911,   804,   805,   806,   808,   770,
     809,  -911,  -911,   810,   811,   766,   807,  -911,   812,   814,
     762,   815,   816,   272,   231,   813,   817,  -911,   277,   820,
     771,  -911,  -911,   818,   821,   827,   822,   823,  -911,  -911,
     824,   828,   825,   826,  -911,  -911,   829,   834,   830,   831,
    -911,  -911,   832,  -911,  -911,  -911,   833,  -911,  -911,  -911,
    -911,   835,  -911,  -911,   282,   841,   842,   837,  -911,   838,
     819,  -911,   843,   288,   840,    79,   848,   849,   850,  -911,
     836,   851,   416,   845,   854,   418,   852,   855,   853,   454,
    -911,   856,   857,   858,  -911,  -911,   859,   860,   862,   839,
     863,   861,   864,   865,   872,   873,   874,   875,   876,   877,
     878,   879,   880,   881,   866,   882,   883,   884,   885,   886,
     887,   888,   889,   890,   891,   892,   893,   894,   895,   897,
     898,   899,   900,   902,   903,   904,  -911,   896,   392,   905,
     901,   906,   907,  -911,   908,   909,   910,   911,   913,   912,
     914,   917,   915,   916,   918,   919,   920,   921,   924,   922,
    -911,   378,   850,   850,   850,   396,   410,   524,   923,   926,
    -911,   927,   928,   929,  -911,  -911,   870,  -911,   930,   931,
     934,   936,  -911,  -911,  -911,   284,   937,  -911,  -911,  -911,
     938,   939,   940,   925,   932,  -911,  -911,   944,   933,   935,
     358,   495,  -911,   946,   507,   952,   948,   599,   600,   955,
     941,   953,   942,   954,    48,  -911,  -911,   956,   456,  -911,
    -911,  -911,   957,   943,   960,  -911,   961,   962,   969,   963,
    -911,   968,   947,   949,  -911,   965,   958,  -911,  -911,   971,
    -911,   973,   457,   974,  -911,   975,   802,   959,   976,   984,
     985,   967,   986,  -911,   980,  -911,  -911,  -911,  -911,   981,
    -911,  -911,  -911,   983,  -911,  -911,  -911,  -911,  -911,  -911,
     987,   988,   991,   846,  -911,   198,   990,   441,   356,  -911,
     992,  -911,   132,   993,   340,   994,   995,   996,   997,   998,
    1000,   999,  -911,  1001,  1007,  1002,  -911,  1003,  -911,  -911,
    1004,  1005,  -911,  1006,  -911,  -911,  1008,  1014,  1009,  -911,
    -911,  1010,  1011,  1012,  1013,  1015,  1016,  1017,  1018,  1019,
    1020,  1021,  1029,  1023,  1024,  1025,  1026,  1027,  1028,  1030,
    1031,  1032,  1033,  1034,  1035,  -911,  1036,  1037,  1038,  1039,
    1040,  1041,  1042,  1043,  1051,  -911,  1045,    16,   365,  1046,
    1054,  1048,   977,  -911,  1056,  1050,  -911,  1044,  1058,  1052,
    1053,  -911,  -911,  1061,  1055,  1063,  1057,  1059,  1065,  -911,
     525,   527,   972,   378,   132,   850,  -911,   979,   850,   850,
     129,  1067,  1068,  -911,  -911,  -911,  -911,  -911,  1062,  -911,
    -911,  -911,  -911,  -911,  1064,  -911,  -911,  -911,  -911,  1066,
    1069,  -911,  1070,  1071,  -911,  1072,  -911,  1073,  -911,  -911,
    1074,  1075,  1076,  1077,  -911,  1078,  1079,  1080,  1081,   508,
    1082,  -911,  1083,  -911,  1084,  1085,  -911,  -911,  1086,  -911,
    1087,  -911,  -911,  1088,  1089,  1090,  -911,  1091,  1094,   273,
    1060,  -911,  -911,  -911,  1092,  -911,  -911,  1093,  1097,  -911,
    1099,  1100,  1095,  1102,  -911,  -911,  -911,  -911,  -911,  1103,
    1104,  -911,  1105,   461,  -911,  1106,  -911,  1098,  1108,  -911,
    1109,  -911,  1110,  1112,  -911,  -911,  -911,  -911,  -911,  1114,
    -911,  -911,  1115,  -911,  -911,  -911,  -911,  -911,  -911,  1116,
    -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,
    -911,  -911,  1117,  -911,  -911,  -911,  -911,  -911,  -911,  -911,
    -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,
    -911,  -911,  -911,  1119,  -911,  -911,  1120,   462,  -911,  1121,
    -911,  1122,  -911,  1101,  1123,  -911,  1113,  1125,  -911,  -911,
    1126,  -911,  1127,  -911,  -911,  1128,  1136,  1137,  1138,  1139,
    1140,  -911,  -911,  -911,   396,   385,   552,  1141,   260,  -911,
    1124,  -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,
    -911,  -911,  -911,  1135,  1142,  1143,  1144,  -911,  1145,  -911,
    -911,   163,  -911,  -911,  1146,  1147,  -911,  1148,  1149,  1150,
    -911,  1151,  1152,  -911,  -911,  1153,  -911,  -911,  1154,  -911,
      94,  -911,  -911,  -911,  1155,  1161,  1156,  -911,  -911,  -911,
    -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,  1157,  -911,
    -911,  1165,  -911,  1159,  -911,  -911,  -911,  -911,  -911,  1158,
    -911,  1160,  -911,  1162,  1167,   517,  1168,   522,   989,  -911,
    1169,  1170,  -911,  -911,  -911,  -911,  -911,  -911,  -911,  -911,
    1164,  1166,  -911,  -911,  -911,  -911,  -911,  1171,   138,  -911,
    1173,  -911,  -911,  1175,  -911,  1172,   523,   396,  1163,  -911,
    1174,  1176,  -911,  1183,   438,  -911,  -911,  -911,  -911,  -911,
      25,  -911,  -911,  -911,  -911,  1184,  -911,   439,  -911,   440,
    -911,  -911,  1185,  1186,   432,  -911,  -911,  1187,  1188,  -911,
    1189,  1190,  -911,  -911,  1134,  -911,  -911,  -911,  -911,  1191,
    1192,  1180,  1193,  -911
};

/* YYPGOTO[NTERM-NUM].  */
static const short yypgoto[] =
{
    -911,  -911,  1194,  -911,   -50,   540,  -911,  -911,  -911,  -435,
    -910,  -911,  -258,   529,   -58,   654,  -195,   -83
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const unsigned short yytable[] =
{
     106,   219,   153,   647,   993,   156,   403,   556,   140,   141,
     409,    99,   116,   104,   117,   118,   384,    13,   119,   385,
     125,   120,   121,   122,   557,   123,   885,   102,   116,   157,
    1054,   118,   158,   392,   119,   220,    14,   120,   121,   122,
     886,   123,    99,   443,   446,   448,   173,   392,   124,   393,
     105,   174,   175,   314,   887,   176,   126,   177,   102,   441,
     395,   178,   332,   393,   124,   243,   562,   442,   315,   292,
      17,   248,   784,   444,   395,   785,   249,    18,   316,   293,
     428,   445,   179,   563,   642,   143,   333,   730,   731,   732,
     643,   740,   244,   106,   146,    59,   140,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,   429,    57,    58,
      59,    60,    61,    62,    63,     8,    64,  1046,    65,    66,
     386,    67,   387,    68,    69,    70,    15,    71,    72,   388,
       8,    73,   459,    74,    75,    99,    76,    77,    78,    79,
      80,    47,    48,    49,    50,    51,    52,    53,    54,  1040,
      56,   102,    57,    58,    59,    60,    61,    62,    63,   460,
      64,   917,    65,    66,   918,    67,   355,    68,    69,    70,
    1007,    71,    72,  1008,     8,    73,   250,    74,    75,   298,
      76,    77,    78,    79,    80,    20,    21,    22,    23,    24,
     356,    25,    26,    27,    28,    29,    30,   164,    31,   251,
      32,   509,   165,   252,    33,   570,   299,    34,   821,    35,
     186,    36,   300,    37,    38,   571,    39,    40,    41,    42,
      43,    44,    45,   187,   142,   143,   166,   144,   510,   167,
     180,   168,   822,   145,   146,    59,   823,   147,   310,   255,
     208,   604,   256,   311,   188,   209,   257,   651,   148,   149,
     655,   258,   150,   266,   181,     8,   267,   182,   151,   152,
     268,   213,   259,   999,   260,   269,   214,   261,   605,   210,
     262,   263,   211,   264,   212,   576,   270,   599,   271,  1000,
     608,   272,   600,   950,   273,   274,   107,   275,   108,   372,
     215,   637,   629,   216,   753,   217,   609,   638,   639,   577,
     913,    16,   578,   915,   916,   279,   601,   610,   280,   602,
     951,   603,   281,   373,    19,   630,   276,   754,   109,   110,
       1,     2,   196,     3,     4,   640,    90,     5,   282,   520,
     283,     6,     7,   284,    46,   521,   285,   286,   127,   277,
     128,   129,   197,   278,   130,   522,   198,   392,   131,   199,
     831,   132,     8,   133,   134,   399,   523,   524,   832,   525,
     526,   642,   143,   393,   392,   135,   136,   827,   764,   137,
     392,   146,    59,   394,   395,   888,   514,   644,   396,   400,
     393,   833,   434,   642,   143,   765,   393,   405,   435,   889,
     401,   395,     8,   146,    59,   402,   407,   395,   436,    99,
     295,   408,   705,   515,   200,   733,   138,   392,   139,   437,
     438,   406,   204,   520,     8,   102,   706,   994,   296,   392,
     995,   392,   102,   393,   201,   707,   734,   735,   202,   738,
     708,   203,   205,   447,   395,   393,   206,   393,   250,   207,
     739,  1051,  1056,  1059,  1064,   650,   395,   654,   395,   550,
     825,   826,   411,   551,   415,   420,   455,  1052,  1057,  1060,
     503,   251,   321,   729,   659,   252,   787,   803,  1053,  1058,
    1061,   963,   977,   238,    91,   736,   412,   322,   416,   421,
     456,   323,   239,   169,   504,   301,   978,   324,   660,   170,
     788,   804,   338,   240,   339,   964,   340,    94,   380,   341,
     302,   342,   192,   343,   344,   766,   171,   345,   346,   347,
     348,   349,   350,   193,   351,   352,   381,   769,   937,    96,
    1029,    99,   767,   194,   100,  1032,  1044,   770,   572,    12,
     101,   771,    95,   189,   772,   938,  1030,   102,   573,   567,
      12,  1033,  1045,   190,   568,   191,   741,   906,   223,   908,
     569,   564,   183,   565,   184,   224,   742,   907,   159,   909,
     566,   160,   161,    92,   159,   111,   162,   160,   114,   112,
      93,   113,   162,   221,    97,   115,   289,   222,   290,   236,
     828,   328,   291,   237,   996,   329,   535,   997,    98,   305,
     291,   306,   307,   308,   358,   309,   359,   363,   376,   364,
     377,   553,   558,   554,   559,   775,   777,   776,   778,   155,
     172,   185,   195,   218,   225,   226,   228,   227,   230,   231,
     246,   233,   232,   235,   234,   229,   241,   247,   242,   245,
     253,   254,   265,   303,   287,   288,   294,   304,   297,   317,
     312,   318,   313,   320,   325,   326,   327,   331,   335,   337,
     334,   365,   361,   319,   353,   362,   330,   354,   357,   360,
     367,   368,   370,   374,   366,   369,   371,   375,   382,   379,
     389,   390,   391,   432,   398,   911,   383,   424,   404,   410,
     413,   414,   417,   378,   912,   418,   419,   422,   423,   425,
     426,   427,   431,   433,   439,   452,   457,   430,   450,   461,
     454,   336,  1018,   440,   449,     0,     0,     0,   462,     0,
       0,     0,     0,     0,   154,   451,     0,     0,   474,     0,
     453,     0,     0,   487,   458,     0,     0,     0,   463,     0,
     464,   465,   466,   467,   468,   469,   470,   471,   472,   473,
     496,   475,   476,   477,   478,   479,   480,   481,   482,   483,
     484,   485,   486,   497,   488,   489,   490,   491,   492,   493,
     494,   495,   498,   505,   499,   501,   500,   506,   507,   508,
     511,   512,   513,   517,   502,   516,   518,   529,   519,   530,
     531,   532,   533,   534,   536,   537,   538,   539,   540,   541,
     542,   543,   544,   545,   546,   548,   552,   547,   549,   555,
     574,   596,   592,   588,   612,   575,   579,   584,   580,   581,
     560,   561,   582,   583,   586,   585,   587,   589,   591,   593,
     590,   613,   594,   595,   614,   597,   598,   606,   611,   607,
     615,   619,   616,   617,   618,   620,   621,   623,   807,   622,
     624,   625,   626,   627,   631,   628,   633,   632,   634,   636,
     641,   645,   646,   520,   649,   652,   635,   653,   657,   661,
     820,   663,   656,   658,   736,   666,   668,   662,   671,   664,
     665,   669,   667,   648,   670,   672,   673,   674,   675,   676,
     677,   678,   679,   680,   681,   683,   684,   685,   686,   687,
     688,   689,   690,   691,   692,   693,   694,     0,   696,   682,
     697,   698,   699,   700,   695,   701,   702,   703,   709,   711,
     748,     0,     0,   715,     0,     0,   712,   719,   713,   717,
     720,   716,   724,     0,   726,   721,   722,   727,     0,   704,
       0,     0,     0,   743,   710,   219,     0,   745,   746,   747,
     749,   750,   714,   744,   751,   718,   752,   755,   756,   757,
     758,   723,   759,   725,   761,   728,   768,   773,   774,   760,
     762,   779,   763,   781,   783,   800,   786,   789,   780,   782,
     791,   792,   794,   799,   793,   795,   790,   736,   796,   812,
     797,   801,   798,   802,   805,   806,   809,   810,   811,   813,
     814,   815,   808,   816,   910,   893,   830,   817,   818,   819,
     824,   914,   829,   839,   834,   835,   836,   837,   838,   840,
     842,   841,   843,   844,   845,   846,   847,   849,   848,   850,
     851,   852,   853,   854,  1034,   855,   856,   857,   858,   859,
     860,   861,   862,   863,   864,   865,   866,   867,   868,     0,
     869,   870,   871,   872,   873,   874,   875,   876,   877,   878,
     879,   880,   881,   882,   883,   884,   890,   891,   892,   894,
     895,   897,   898,   899,   900,   901,   902,   903,   905,   904,
     919,   920,   921,     0,   922,     0,   923,   896,     0,   924,
     925,   926,   927,   928,   929,   930,   931,   932,     0,     0,
     944,     0,   939,   940,   948,   942,   943,   949,   952,   946,
     955,   966,   953,   954,     0,   933,   934,   935,   936,   956,
     957,   941,   959,   960,   961,   962,   983,   965,   967,   968,
     969,   945,   970,   947,   971,   972,   973,   974,   958,   975,
     976,   979,   980,   982,   981,   984,   985,   986,   987,   988,
     989,   990,   991,   992,   998,  1002,  1001,  1069,     0,     0,
    1010,  1011,  1003,  1004,  1005,  1006,  1009,  1017,     0,  1012,
    1013,  1014,  1015,  1016,  1020,  1019,  1021,  1022,  1023,  1024,
    1028,  1031,  1035,  1036,  1037,  1043,  1038,  1048,     0,     0,
    1025,  1039,  1026,  1041,  1027,  1042,  1050,  1055,  1062,  1063,
    1065,  1066,  1067,  1068,  1072,     0,  1070,     0,  1047,     0,
       0,  1071,     0,  1073,     0,    89,     0,     0,     0,     0,
       0,  1049
};

static const short yycheck[] =
{
      50,    84,    60,   438,   914,    10,   201,    10,    58,    59,
     205,     3,     3,     5,     5,     6,    11,     6,     9,    14,
       5,    12,    13,    14,    27,    16,    10,    19,     3,    34,
       5,     6,    37,     3,     9,    85,     8,    12,    13,    14,
      24,    16,     3,   238,   239,   240,     3,     3,    39,    19,
      42,     8,     9,    14,    38,    12,    41,    14,    19,    29,
      30,    18,    10,    19,    39,     6,    10,    37,    29,     6,
       3,    10,    24,    29,    30,    27,    15,    10,    39,    16,
      10,    37,    39,    27,     5,     6,    34,   522,   523,   524,
      11,   526,    33,   143,    15,    16,   146,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    37,    14,    15,
      16,    17,    18,    19,    20,    36,    22,  1027,    24,    25,
       8,    27,    10,    29,    30,    31,    18,    33,    34,    17,
      36,    37,     6,    39,    40,     3,    42,    43,    44,    45,
      46,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    19,    14,    15,    16,    17,    18,    19,    20,    33,
      22,    32,    24,    25,    35,    27,    10,    29,    30,    31,
       7,    33,    34,    10,    36,    37,     6,    39,    40,     6,
      42,    43,    44,    45,    46,     5,     6,     7,     8,     9,
      34,    11,    12,    13,    14,    15,    16,     5,    18,    29,
      20,     6,    10,    33,    24,    17,    33,    27,    10,    29,
       5,    31,    39,    33,    34,    27,    36,    37,    38,    39,
      40,    41,    42,    18,     5,     6,    34,     8,    33,    37,
      10,    39,    34,    14,    15,    16,    38,    18,    17,     6,
       5,    10,     9,    22,    39,    10,    13,   442,    29,    30,
     445,    18,    33,     6,    34,    36,     9,    37,    39,    40,
      13,     5,    29,     3,    31,    18,    10,    34,    37,    34,
      37,    38,    37,    40,    39,    10,    29,     5,    31,    19,
       3,    34,    10,    10,    37,    38,     6,    40,     8,    10,
      34,     3,    10,    37,    10,    39,    19,     9,    10,    34,
     735,     5,    37,   738,   739,     6,    34,    30,     9,    37,
      37,    39,    13,    34,     3,    33,     6,    33,    38,    39,
       4,     5,    10,     7,     8,    37,     3,    11,    29,     3,
      31,    15,    16,    34,     0,     9,    37,    38,     3,    29,
       5,     6,    30,    33,     9,    19,    34,     3,    13,    37,
      10,    16,    36,    18,    19,    10,    30,    31,    18,    33,
      34,     5,     6,    19,     3,    30,    31,    11,    10,    34,
       3,    15,    16,    29,    30,    10,     6,   435,    34,    34,
      19,    41,     3,     5,     6,    27,    19,    10,     9,    24,
      29,    30,    36,    15,    16,    34,    29,    30,    19,     3,
      24,    34,    10,    33,    10,     9,     3,     3,     5,    30,
      31,    34,    10,     3,    36,    19,    24,    32,    42,     3,
      35,     3,    19,    19,    30,    33,    30,    31,    34,    19,
      38,    37,    30,    29,    30,    19,    34,    19,     6,    37,
      30,     3,     3,     3,    12,    29,    30,    29,    30,    20,
       9,    10,    10,    24,    10,    10,    10,    19,    19,    19,
      10,    29,    14,   521,    10,    33,    10,    10,    30,    30,
      30,    10,    10,     9,     7,   525,    34,    29,    34,    34,
      34,    33,    18,     6,    34,    16,    24,    39,    34,    12,
      34,    34,     6,    29,     8,    34,    10,     3,    24,    13,
      31,    15,     3,    17,    18,    10,    29,    21,    22,    23,
      24,    25,    26,    14,    28,    29,    42,    10,    10,    10,
       3,     3,    27,    24,     6,     3,     3,    20,    17,     0,
      12,    24,    12,    29,    27,    27,    19,    19,    27,    13,
      11,    19,    19,    39,    18,    41,    22,    22,    10,    22,
      24,     6,    39,     8,    41,    17,    32,    32,     3,    32,
      15,     6,     7,     3,     3,    12,    11,     6,    24,    16,
      10,    18,    11,    10,    10,    31,     8,    14,    10,     5,
     638,    10,    14,     9,    32,    14,    10,    35,    10,     5,
      14,     7,     8,     5,    13,     7,    15,     8,     8,    10,
      10,     6,     6,     8,     8,     6,     6,     8,     8,    39,
       6,     3,     3,    11,     8,    10,    13,    10,    41,     3,
      37,     9,    11,     8,    39,    15,    13,     7,    31,    12,
       5,     5,     9,    39,    10,     8,     8,    39,     9,     5,
      10,     3,    11,     3,     3,     3,     3,    10,     3,     3,
      11,    33,     3,    39,    10,     5,    24,    10,    10,     8,
      24,     3,     3,     8,    31,    12,    10,     7,     3,    12,
       3,     8,    10,     7,    10,   733,    15,     3,    10,    10,
      10,    10,    10,    31,   734,    10,    10,    10,    10,     6,
       9,     5,     3,     8,    10,     3,     3,    24,    10,     3,
      10,   161,   960,    24,    24,    -1,    -1,    -1,    14,    -1,
      -1,    -1,    -1,    -1,    60,    33,    -1,    -1,    11,    -1,
      33,    -1,    -1,     3,    33,    -1,    -1,    -1,    33,    -1,
      33,    33,    33,    33,    33,    33,    33,    33,    33,    33,
      10,    33,    33,    33,    33,    33,    33,    33,    33,    33,
      33,    33,    33,    12,    33,    33,    33,    33,    33,    33,
      33,    33,     3,    11,    33,    33,    37,    33,     3,    11,
      33,     3,     3,    33,    41,    40,    33,     3,    40,     3,
       3,     3,     3,    10,    24,    10,     3,     3,     3,     3,
      10,    10,    10,     3,    40,     3,     5,    10,    10,    10,
       3,    39,    36,    33,    33,    10,    10,     3,    10,    10,
      17,    17,    12,    12,     8,    10,     8,     8,     7,    12,
      10,     3,    10,     9,     3,    10,    10,    14,     8,    12,
       3,     3,    10,    10,    10,    10,    10,     3,    36,    10,
      10,    10,    10,    10,     3,    10,     9,     5,    10,     6,
      10,     3,     3,     3,     3,    10,    37,     3,     3,     3,
      14,     3,    10,    10,   914,     3,     3,    10,     3,    10,
      10,    10,    33,    37,    10,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,    -1,     3,    33,
       3,     3,     3,     3,    10,     3,     3,     3,     3,     3,
      40,    -1,    -1,     3,    -1,    -1,     9,     3,    10,     6,
       3,    10,     3,    -1,     3,    10,    10,     3,    -1,    33,
      -1,    -1,    -1,    10,    33,  1018,    -1,    10,    10,    10,
      10,    10,    33,    17,    10,    33,    10,    10,    10,    10,
      10,    33,    27,    33,    10,    33,    10,     5,    10,    27,
      27,     6,    27,    10,    10,     7,    10,    10,    27,    27,
      10,    10,     3,     8,    12,    12,    33,  1027,    10,    12,
      33,    10,    33,    10,    10,    10,    10,     3,     3,     3,
      10,    10,    33,    10,    22,    18,     3,    10,    10,     8,
      10,    22,    10,     3,    10,    10,    10,    10,    10,    10,
       3,    10,    10,    10,    10,    10,    10,     3,    10,    10,
      10,    10,    10,    10,    35,    10,    10,    10,    10,    10,
      10,    10,     3,    10,    10,    10,    10,    10,    10,    -1,
      10,    10,    10,    10,    10,    10,    10,    10,    10,    10,
      10,    10,    10,    10,     3,    10,    10,     3,    10,     3,
      10,     3,    10,    10,     3,    10,     3,    10,     3,    10,
       3,     3,    10,    -1,    10,    -1,    10,    33,    -1,    10,
      10,    10,    10,    10,    10,    10,    10,    10,    -1,    -1,
       3,    -1,    10,    10,     3,    10,    10,     3,    38,    10,
       3,     3,    10,    10,    -1,    27,    27,    27,    27,    10,
      10,    27,    10,    10,    10,    10,     3,    11,    10,    10,
      10,    33,    10,    33,    10,    10,    10,    10,    33,    10,
      10,    10,    10,    10,    33,    10,    10,    10,    10,     3,
       3,     3,     3,     3,     3,    10,    22,    13,    -1,    -1,
       3,     3,    10,    10,    10,    10,    10,     3,    -1,    10,
      10,    10,    10,    10,     3,    10,    10,    10,     3,    10,
       3,     3,     3,     3,    10,     3,    10,     3,    -1,    -1,
      22,    10,    22,    10,    22,    10,     3,     3,     3,     3,
       3,     3,     3,     3,    14,    -1,     5,    -1,    35,    -1,
      -1,     9,    -1,    10,    -1,    11,    -1,    -1,    -1,    -1,
      -1,    35
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,     4,     5,     7,     8,    11,    15,    16,    36,    48,
      49,    50,    60,     6,     8,    18,     5,     3,    10,     3,
       5,     6,     7,     8,     9,    11,    12,    13,    14,    15,
      16,    18,    20,    24,    27,    29,    31,    33,    34,    36,
      37,    38,    39,    40,    41,    42,     0,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    14,    15,    16,
      17,    18,    19,    20,    22,    24,    25,    27,    29,    30,
      31,    33,    34,    37,    39,    40,    42,    43,    44,    45,
      46,    53,    54,    55,    59,    60,    61,    62,    64,    49,
       3,     7,     3,    10,     3,    12,    10,    10,    10,     3,
       6,    12,    19,    51,     5,    42,    51,     6,     8,    38,
      39,    12,    16,    18,    24,    31,     3,     5,     6,     9,
      12,    13,    14,    16,    39,     5,    41,     3,     5,     6,
       9,    13,    16,    18,    19,    30,    31,    34,     3,     5,
      51,    51,     5,     6,     8,    14,    15,    18,    29,    30,
      33,    39,    40,    61,    62,    39,    10,    34,    37,     3,
       6,     7,    11,    52,     5,    10,    34,    37,    39,     6,
      12,    29,     6,     3,     8,     9,    12,    14,    18,    39,
      10,    34,    37,    39,    41,     3,     5,    18,    39,    29,
      39,    41,     3,    14,    24,     3,    10,    30,    34,    37,
      10,    30,    34,    37,    10,    30,    34,    37,     5,    10,
      34,    37,    39,     5,    10,    34,    37,    39,    11,    64,
      51,    10,    14,    10,    17,     8,    10,    10,    13,    15,
      41,     3,    11,     9,    39,     8,     5,     9,     9,    18,
      29,    13,    31,     6,    33,    12,    37,     7,    10,    15,
       6,    29,    33,     5,     5,     6,     9,    13,    18,    29,
      31,    34,    37,    38,    40,     9,     6,     9,    13,    18,
      29,    31,    34,    37,    38,    40,     6,    29,    33,     6,
       9,    13,    29,    31,    34,    37,    38,    10,     8,     8,
      10,    14,     6,    16,     8,    24,    42,     9,     6,    33,
      39,    16,    31,    39,    39,     5,     7,     8,     5,     7,
      17,    22,    10,    11,    14,    29,    39,     5,     3,    39,
       3,    14,    29,    33,    39,     3,     3,     3,    10,    14,
      24,    10,    10,    34,    11,     3,    52,     3,     6,     8,
      10,    13,    15,    17,    18,    21,    22,    23,    24,    25,
      26,    28,    29,    10,    10,    10,    34,    10,    13,    15,
       8,     3,     5,     8,    10,    33,    31,    24,     3,    12,
       3,    10,    10,    34,     8,     7,     8,    10,    31,    12,
      24,    42,     3,    15,    11,    14,     8,    10,    17,     3,
       8,    10,     3,    19,    29,    30,    34,    63,    10,    10,
      34,    29,    34,    63,    10,    10,    34,    29,    34,    63,
      10,    10,    34,    10,    10,    10,    34,    10,    10,    10,
      10,    34,    10,    10,     3,     6,     9,     5,    10,    37,
      24,     3,     7,     8,     3,     9,    19,    30,    31,    10,
      24,    29,    37,    63,    29,    37,    63,    29,    63,    24,
      10,    33,     3,    33,    10,    10,    34,     3,    33,     6,
      33,     3,    14,    33,    33,    33,    33,    33,    33,    33,
      33,    33,    33,    33,    11,    33,    33,    33,    33,    33,
      33,    33,    33,    33,    33,    33,    33,     3,    33,    33,
      33,    33,    33,    33,    33,    33,    10,    12,     3,    33,
      37,    33,    41,    10,    34,    11,    33,     3,    11,     6,
      33,    33,     3,     3,     6,    33,    40,    33,    33,    40,
       3,     9,    19,    30,    31,    33,    34,    56,    58,     3,
       3,     3,     3,     3,    10,    10,    24,    10,     3,     3,
       3,     3,    10,    10,    10,     3,    40,    10,     3,    10,
      20,    24,     5,     6,     8,    10,    10,    27,     6,     8,
      17,    17,    10,    27,     6,     8,    15,    13,    18,    24,
      17,    27,    17,    27,     3,    10,    10,    34,    37,    10,
      10,    10,    12,    12,     3,    10,     8,     8,    33,     8,
      10,     7,    36,    12,    10,     9,    39,    10,    10,     5,
      10,    34,    37,    39,    10,    37,    14,    12,     3,    19,
      30,     8,    33,     3,     3,     3,    10,    10,    10,     3,
      10,    10,    10,     3,    10,    10,    10,    10,    10,    10,
      33,     3,     5,     9,    10,    37,     6,     3,     9,    10,
      37,    10,     5,    11,    61,     3,     3,    56,    37,     3,
      29,    63,    10,     3,    29,    63,    10,     3,    10,    10,
      34,     3,    10,     3,    10,    10,     3,    33,     3,    10,
      10,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,    33,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,    10,     3,     3,     3,     3,
       3,     3,     3,     3,    33,    10,    24,    33,    38,     3,
      33,     3,     9,    10,    33,     3,    10,     6,    33,     3,
       3,    10,    10,    33,     3,    33,     3,     3,    33,    61,
      56,    56,    56,     9,    30,    31,    51,    57,    19,    30,
      56,    22,    32,    10,    17,    10,    10,    10,    40,    10,
      10,    10,    10,    10,    33,    10,    10,    10,    10,    27,
      27,    10,    27,    27,    10,    27,    10,    27,    10,    10,
      20,    24,    27,     5,    10,     6,     8,     6,     8,     6,
      27,    10,    27,    10,    24,    27,    10,    10,    34,    10,
      33,    10,    10,    12,     3,    12,    10,    33,    33,     8,
       7,    10,    10,    10,    34,    10,    10,    36,    33,    10,
       3,     3,    12,     3,    10,    10,    10,    10,    10,     8,
      14,    10,    34,    38,    10,     9,    10,    11,    61,    10,
       3,    10,    18,    41,    10,    10,    10,    10,    10,     3,
      10,    10,     3,    10,    10,    10,    10,    10,    10,     3,
      10,    10,    10,    10,    10,    10,    10,    10,    10,    10,
      10,    10,     3,    10,    10,    10,    10,    10,    10,    10,
      10,    10,    10,    10,    10,    10,    10,    10,    10,    10,
      10,    10,    10,     3,    10,    10,    24,    38,    10,    24,
      10,     3,    10,    18,     3,    10,    33,     3,    10,    10,
       3,    10,     3,    10,    10,     3,    22,    32,    22,    32,
      22,    61,    51,    56,    22,    56,    56,    32,    35,     3,
       3,    10,    10,    10,    10,    10,    10,    10,    10,    10,
      10,    10,    10,    27,    27,    27,    27,    10,    27,    10,
      10,    27,    10,    10,     3,    33,    10,    33,     3,     3,
      10,    37,    38,    10,    10,     3,    10,    10,    33,    10,
      10,    10,    10,    10,    34,    11,     3,    10,    10,    10,
      10,    10,    10,    10,    10,    10,    10,    10,    24,    10,
      10,    33,    10,     3,    10,    10,    10,    10,     3,     3,
       3,     3,     3,    57,    32,    35,    32,    35,     3,     3,
      19,    22,    10,    10,    10,    10,    10,     7,    10,    10,
       3,     3,    10,    10,    10,    10,    10,     3,    59,    10,
       3,    10,    10,     3,    10,    22,    22,    22,     3,     3,
      19,     3,     3,    19,    35,     3,     3,    10,    10,    10,
      11,    10,    10,     3,     3,    19,    57,    35,     3,    35,
       3,     3,    19,    30,     5,     3,     3,    19,    30,     3,
      19,    30,     3,     3,    12,     3,     3,     3,     3,    13,
       5,     9,    14,    10
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrlab1

/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)         \
  Current.first_line   = Rhs[1].first_line;      \
  Current.first_column = Rhs[1].first_column;    \
  Current.last_line    = Rhs[N].last_line;       \
  Current.last_column  = Rhs[N].last_column;
#endif

/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (cinluded).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylineno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylineno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 109 "kal3.y"
    { printf("Reached end of file\n"); }
    break;

  case 3:
#line 112 "kal3.y"
    { endBblock(); }
    break;

  case 4:
#line 113 "kal3.y"
    { endBblock(); }
    break;

  case 6:
#line 117 "kal3.y"
    { reserveStore('V', yyvsp[-1]); }
    break;

  case 8:
#line 119 "kal3.y"
    { reserveStore('W', yyvsp[-1]); }
    break;

  case 9:
#line 120 "kal3.y"
    { reserveStore('Y' + yyvsp[-2] * 256, yyvsp[-1]); }
    break;

  case 10:
#line 122 "kal3.y"
    { fixY0(yyvsp[-5], yyvsp[-4], yyvsp[-1]); }
    break;

  case 12:
#line 124 "kal3.y"
    { Bblock('S', yyvsp[-1]); }
    break;

  case 13:
#line 125 "kal3.y"
    { Bblock('L', yyvsp[-1]); }
    break;

  case 16:
#line 130 "kal3.y"
    { yyval = -yyvsp[0]; }
    break;

  case 17:
#line 133 "kal3.y"
    { yyval = codeloc(-1, yyvsp[0]); }
    break;

  case 18:
#line 134 "kal3.y"
    { yyval = codeloc(yyvsp[0], 0); }
    break;

  case 19:
#line 135 "kal3.y"
    { yyval = codeloc(yyvsp[0], yyvsp[-2]); }
    break;

  case 20:
#line 136 "kal3.y"
    { yyval = yyvsp[0] * 6; }
    break;

  case 21:
#line 139 "kal3.y"
    { storejump(0200, 0260, yyvsp[-1]); }
    break;

  case 22:
#line 140 "kal3.y"
    { storejump(0200, 0320, yyvsp[-1]); }
    break;

  case 23:
#line 141 "kal3.y"
    { storejump(0220, 0020, yyvsp[-2]); }
    break;

  case 24:
#line 142 "kal3.y"
    { storejump(0200, 0020, yyvsp[-3]); }
    break;

  case 25:
#line 143 "kal3.y"
    { storejump(0200, 0020, yyvsp[-3]); }
    break;

  case 26:
#line 144 "kal3.y"
    { storejump(0200, 0020, yyvsp[-2]); }
    break;

  case 27:
#line 145 "kal3.y"
    { storejump(0200, 0020, yyvsp[-3]); }
    break;

  case 28:
#line 146 "kal3.y"
    { storejump(0200, 0020, yyvsp[-4]); }
    break;

  case 29:
#line 147 "kal3.y"
    { storejump(0220, 0040, yyvsp[-3]); }
    break;

  case 30:
#line 148 "kal3.y"
    { storejump(0220, 0040, yyvsp[-4]); }
    break;

  case 31:
#line 149 "kal3.y"
    { storejump(0220, 0040, yyvsp[-5]); }
    break;

  case 32:
#line 150 "kal3.y"
    { storejump(0200, 0040, yyvsp[-4]); }
    break;

  case 33:
#line 151 "kal3.y"
    { storejump(0200, 0040, yyvsp[-4]); }
    break;

  case 34:
#line 152 "kal3.y"
    { storejump(0200, 0040, yyvsp[-5]); }
    break;

  case 35:
#line 153 "kal3.y"
    { storejump(0220, 0100, yyvsp[-3]); }
    break;

  case 36:
#line 154 "kal3.y"
    { storejump(0220, 0100, yyvsp[-5]); }
    break;

  case 37:
#line 155 "kal3.y"
    { storejump(0220, 0100, yyvsp[-4]); }
    break;

  case 38:
#line 156 "kal3.y"
    { storejump(0200, 0100, yyvsp[-4]); }
    break;

  case 39:
#line 157 "kal3.y"
    { storejump(0200, 0100, yyvsp[-5]); }
    break;

  case 40:
#line 158 "kal3.y"
    { storejump(0200, 0100, yyvsp[-4]); }
    break;

  case 41:
#line 159 "kal3.y"
    { storejump(0220, 0140, yyvsp[-3]); }
    break;

  case 42:
#line 160 "kal3.y"
    { storejump(0200, 0140, yyvsp[-4]); }
    break;

  case 43:
#line 161 "kal3.y"
    { storejump(0200, 0140, yyvsp[-4]); }
    break;

  case 44:
#line 162 "kal3.y"
    { storejump(0200, 0140, yyvsp[-3]); }
    break;

  case 45:
#line 163 "kal3.y"
    { storejump(0200, 0140, yyvsp[-5]); }
    break;

  case 46:
#line 164 "kal3.y"
    { storejump(0200, 0140, yyvsp[-4]); }
    break;

  case 47:
#line 165 "kal3.y"
    { storejump(0220, 0200, yyvsp[-2]); }
    break;

  case 48:
#line 166 "kal3.y"
    { storejump(0200, 0200, yyvsp[-3]); }
    break;

  case 49:
#line 167 "kal3.y"
    { storejump(0220, 0240, yyvsp[-3]); }
    break;

  case 50:
#line 168 "kal3.y"
    { storejump(0200, 0240, yyvsp[-4]); }
    break;

  case 51:
#line 169 "kal3.y"
    { storejump(0220, 0300, yyvsp[-3]); }
    break;

  case 52:
#line 170 "kal3.y"
    { storejump(0200, 0300, yyvsp[-4]); }
    break;

  case 53:
#line 171 "kal3.y"
    { storejump(0220, 0340, yyvsp[-3]); }
    break;

  case 54:
#line 172 "kal3.y"
    { storejump(0200, 0340, yyvsp[-4]); }
    break;

  case 55:
#line 173 "kal3.y"
    { qnum(yyvsp[-2]); storejump(0240, yyvsp[-2]<<4, yyvsp[-4]); }
    break;

  case 56:
#line 174 "kal3.y"
    { qnum(yyvsp[-3]); storejump(0260, yyvsp[-3]<<4, yyvsp[-5]); }
    break;

  case 57:
#line 175 "kal3.y"
    { store2syl(twosylqx(0177, yyvsp[-3], 0)); }
    break;

  case 58:
#line 178 "kal3.y"
    { setprinterconst(yyvsp[-1], yyvsp[-1]); }
    break;

  case 59:
#line 179 "kal3.y"
    { setprinterconst(yyvsp[-3], yyvsp[-1]); }
    break;

  case 60:
#line 182 "kal3.y"
    { setvstore(yyvsp[-3]); }
    break;

  case 62:
#line 186 "kal3.y"
    { yyval = keepMarker(); }
    break;

  case 64:
#line 190 "kal3.y"
    { yyval = yyvsp[0]; }
    break;

  case 65:
#line 191 "kal3.y"
    { yyval = yyvsp[0]; }
    break;

  case 66:
#line 192 "kal3.y"
    { yyval = octalval(yyvsp[0]); }
    break;

  case 67:
#line 195 "kal3.y"
    { vfraction(yyvsp[0], 47); }
    break;

  case 68:
#line 196 "kal3.y"
    { voctal(yyvsp[0], 47); }
    break;

  case 69:
#line 197 "kal3.y"
    { voctal(yyvsp[-2], yyvsp[0]); }
    break;

  case 70:
#line 198 "kal3.y"
    { vfraction(yyvsp[-2], 47); }
    break;

  case 71:
#line 199 "kal3.y"
    { vfraction(yyvsp[-4], yyvsp[0]); }
    break;

  case 72:
#line 200 "kal3.y"
    { vfraction(yyvsp[-2], yyvsp[0]); }
    break;

  case 73:
#line 201 "kal3.y"
    { vfraction(yyvsp[0], 47); negatevstore(); }
    break;

  case 74:
#line 202 "kal3.y"
    { vfraction(yyvsp[-2], 47); negatevstore(); }
    break;

  case 75:
#line 203 "kal3.y"
    { vfraction(yyvsp[-4], yyvsp[0]); negatevstore(); }
    break;

  case 76:
#line 204 "kal3.y"
    { vfraction(yyvsp[-2], yyvsp[0]); negatevstore(); }
    break;

  case 77:
#line 205 "kal3.y"
    { vfraction(yyvsp[0], 47); }
    break;

  case 78:
#line 206 "kal3.y"
    { vfraction(yyvsp[-2], 47); }
    break;

  case 79:
#line 207 "kal3.y"
    { vfraction(yyvsp[-4], yyvsp[0]); }
    break;

  case 80:
#line 208 "kal3.y"
    { vfraction(yyvsp[-2], yyvsp[0]); }
    break;

  case 81:
#line 209 "kal3.y"
    { vqformat(yyvsp[-4], yyvsp[-2], yyvsp[0]); }
    break;

  case 82:
#line 210 "kal3.y"
    { vqformat(0, 0, yyvsp[0]); }
    break;

  case 83:
#line 211 "kal3.y"
    { vfloat(yyvsp[-2], 0); vfppos(); }
    break;

  case 84:
#line 212 "kal3.y"
    { vfloat(yyvsp[0], 0); vfppos(); }
    break;

  case 85:
#line 213 "kal3.y"
    { vfloat(yyvsp[-4], yyvsp[0]); vfppos(); }
    break;

  case 86:
#line 214 "kal3.y"
    { vfloat(yyvsp[-5], -yyvsp[0]); vfppos(); }
    break;

  case 87:
#line 215 "kal3.y"
    { vfloat(yyvsp[-5], yyvsp[0]); vfppos(); }
    break;

  case 88:
#line 216 "kal3.y"
    { vfloat(yyvsp[-2], yyvsp[0]); vfppos(); }
    break;

  case 89:
#line 217 "kal3.y"
    { vfloat(yyvsp[-3], -yyvsp[0]); vfppos(); }
    break;

  case 90:
#line 218 "kal3.y"
    { vfloat(yyvsp[-2], 0); vfpneg(); }
    break;

  case 91:
#line 219 "kal3.y"
    { vfloat(yyvsp[0], 0); vfpneg(); }
    break;

  case 92:
#line 220 "kal3.y"
    { vfloat(yyvsp[-4], yyvsp[0]); vfpneg(); }
    break;

  case 93:
#line 221 "kal3.y"
    { vfloat(yyvsp[-5], -yyvsp[0]); vfpneg(); }
    break;

  case 94:
#line 222 "kal3.y"
    { vfloat(yyvsp[-5], yyvsp[0]); vfpneg(); }
    break;

  case 95:
#line 223 "kal3.y"
    { vfloat(yyvsp[-2], yyvsp[0]); vfpneg(); }
    break;

  case 96:
#line 224 "kal3.y"
    { vfloat(yyvsp[-3], -yyvsp[0]); vfpneg(); }
    break;

  case 97:
#line 225 "kal3.y"
    { vfloat(yyvsp[-2], 0); vfppos(); }
    break;

  case 98:
#line 226 "kal3.y"
    { vfloat(yyvsp[0], 0); vfppos(); }
    break;

  case 99:
#line 227 "kal3.y"
    { vfloat(yyvsp[-4], yyvsp[0]); vfppos(); }
    break;

  case 100:
#line 228 "kal3.y"
    { vfloat(yyvsp[-5], -yyvsp[0]); vfppos(); }
    break;

  case 101:
#line 229 "kal3.y"
    { vfloat(yyvsp[-5], yyvsp[0]); vfppos(); }
    break;

  case 102:
#line 230 "kal3.y"
    { vfloat(yyvsp[-2], yyvsp[0]); vfppos(); }
    break;

  case 103:
#line 231 "kal3.y"
    { vfloat(yyvsp[-3], -yyvsp[0]); vfppos(); }
    break;

  case 104:
#line 233 "kal3.y"
    { vfraction(yyvsp[-5], -yyvsp[0]); }
    break;

  case 107:
#line 240 "kal3.y"
    {  yyval = 0; }
    break;

  case 108:
#line 241 "kal3.y"
    {  yyval = 1; }
    break;

  case 109:
#line 242 "kal3.y"
    {  yyval = 2; }
    break;

  case 110:
#line 243 "kal3.y"
    {  yyval = 3; }
    break;

  case 111:
#line 244 "kal3.y"
    {  yyval = 4; }
    break;

  case 112:
#line 245 "kal3.y"
    {  yyval = 5; }
    break;

  case 113:
#line 246 "kal3.y"
    {  yyval = 6; }
    break;

  case 114:
#line 247 "kal3.y"
    {  yyval = 7; }
    break;

  case 115:
#line 248 "kal3.y"
    {  yyval = 8; }
    break;

  case 116:
#line 249 "kal3.y"
    {  yyval = 9; }
    break;

  case 117:
#line 250 "kal3.y"
    {  yyval = 10; }
    break;

  case 118:
#line 251 "kal3.y"
    {  yyval = 11; }
    break;

  case 119:
#line 252 "kal3.y"
    {  yyval = 12; }
    break;

  case 120:
#line 253 "kal3.y"
    {  yyval = 13; }
    break;

  case 121:
#line 254 "kal3.y"
    {  yyval = 14; }
    break;

  case 122:
#line 255 "kal3.y"
    {  yyval = 15; }
    break;

  case 123:
#line 256 "kal3.y"
    {  yyval = 16; }
    break;

  case 124:
#line 257 "kal3.y"
    {  yyval = 17; }
    break;

  case 125:
#line 258 "kal3.y"
    {  yyval = 18; }
    break;

  case 126:
#line 259 "kal3.y"
    {  yyval = 19; }
    break;

  case 127:
#line 260 "kal3.y"
    {  yyval = 20; }
    break;

  case 128:
#line 261 "kal3.y"
    {  yyval = 21; }
    break;

  case 129:
#line 262 "kal3.y"
    {  yyval = 22; }
    break;

  case 130:
#line 263 "kal3.y"
    {  yyval = 23; }
    break;

  case 131:
#line 264 "kal3.y"
    {  yyval = 24; }
    break;

  case 132:
#line 265 "kal3.y"
    {  yyval = 25; }
    break;

  case 133:
#line 266 "kal3.y"
    {  yyval = 26; }
    break;

  case 134:
#line 269 "kal3.y"
    {  yyval = dataloc(-1, yyvsp[0]); }
    break;

  case 135:
#line 270 "kal3.y"
    {  yyval = dataloc(yyvsp[0], yyvsp[-2]); }
    break;

  case 136:
#line 271 "kal3.y"
    {  yyval = codeloc(-1, yyvsp[0])/6; }
    break;

  case 137:
#line 272 "kal3.y"
    {  yyval = codeloc(yyvsp[0], yyvsp[-2])/6; }
    break;

  case 138:
#line 273 "kal3.y"
    {  yyval = yyvsp[0] & 07777; }
    break;

  case 139:
#line 274 "kal3.y"
    {  yyval = ystoreloc(yyvsp[-1], yyvsp[0]); }
    break;

  case 140:
#line 275 "kal3.y"
    {  yyval = ystoreloc(-1, yyvsp[0]); }
    break;

  case 141:
#line 278 "kal3.y"
    {yyval = twosylqx(0100, yyvsp[-1], yyvsp[-3]); }
    break;

  case 142:
#line 279 "kal3.y"
    {yyval = twosylqx(0102, yyvsp[-2], yyvsp[-4]); }
    break;

  case 143:
#line 280 "kal3.y"
    {yyval = twosylqx(0110, yyvsp[-2], yyvsp[-4]); }
    break;

  case 144:
#line 281 "kal3.y"
    {yyval = twosylqx(0112, yyvsp[-3], yyvsp[-5]); }
    break;

  case 145:
#line 282 "kal3.y"
    {yyval = twosylqx(0104, yyvsp[-2], yyvsp[-4]); }
    break;

  case 146:
#line 283 "kal3.y"
    {yyval = twosylqx(0106, yyvsp[-3], yyvsp[-5]); }
    break;

  case 147:
#line 284 "kal3.y"
    {yyval = twosylqx(0114, yyvsp[-3], yyvsp[-5]); }
    break;

  case 148:
#line 285 "kal3.y"
    {yyval = twosylqx(0116, yyvsp[-4], yyvsp[-6]); }
    break;

  case 150:
#line 289 "kal3.y"
    {  yyval = yyvsp[0]; }
    break;

  case 151:
#line 290 "kal3.y"
    {  yyval = (-yyvsp[0]) & 0177; }
    break;

  case 154:
#line 295 "kal3.y"
    { setlabel(yyvsp[-1]); }
    break;

  case 156:
#line 297 "kal3.y"
    { newproutine(yyvsp[-1], -1); }
    break;

  case 157:
#line 298 "kal3.y"
    { newproutine(yyvsp[-3], yyvsp[-1]); }
    break;

  case 158:
#line 300 "kal3.y"
    { store3syl(0300, yyvsp[-1]); }
    break;

  case 159:
#line 301 "kal3.y"
    { store3syl(0301, yyvsp[-1]); }
    break;

  case 160:
#line 303 "kal3.y"
    { qnum(yyvsp[-1]); store3syl(0300, yyvsp[-3] + (yyvsp[-1]<<12)); }
    break;

  case 161:
#line 304 "kal3.y"
    { qnum(yyvsp[-1]); store3syl(0301, yyvsp[-3] + (yyvsp[-1]<<12)); }
    break;

  case 162:
#line 305 "kal3.y"
    { qnum(yyvsp[-2]); store3syl(0302, yyvsp[-4] + (yyvsp[-2]<<12)); }
    break;

  case 163:
#line 306 "kal3.y"
    { qnum(yyvsp[-2]); store3syl(0303, yyvsp[-4] + (yyvsp[-2]<<12)); }
    break;

  case 164:
#line 308 "kal3.y"
    {  store3syl(0304, yyvsp[-1]); }
    break;

  case 165:
#line 309 "kal3.y"
    {  store3syl(0304, octalval(yyvsp[-1])); }
    break;

  case 166:
#line 310 "kal3.y"
    {  store3syl(0304, (-yyvsp[-1])&0177777); }
    break;

  case 167:
#line 311 "kal3.y"
    {  store3syl(0304, yyvsp[-1]); }
    break;

  case 168:
#line 312 "kal3.y"
    {  store3syl(0304, yyvsp[-1]); }
    break;

  case 169:
#line 313 "kal3.y"
    {  store3syl(0304, yyvsp[-2]<<1); }
    break;

  case 170:
#line 314 "kal3.y"
    {  store3syl(0304, yyvsp[-2]<<1 + 1); }
    break;

  case 171:
#line 315 "kal3.y"
    {  store3syl(0304, codeloc(yyvsp[-1], 0)/6); }
    break;

  case 172:
#line 317 "kal3.y"
    {  store2syl(yyvsp[0]); }
    break;

  case 173:
#line 318 "kal3.y"
    {  store2syl(256 + yyvsp[0]); }
    break;

  case 174:
#line 320 "kal3.y"
    { store2syl(twosylqx(0151, yyvsp[-5], yyvsp[-1])); }
    break;

  case 175:
#line 321 "kal3.y"
    { store2syl(twosylqx(0152, yyvsp[-5], yyvsp[-1])); }
    break;

  case 176:
#line 322 "kal3.y"
    { store2syl(twosylqx(0153, yyvsp[-5], yyvsp[-1])); }
    break;

  case 177:
#line 323 "kal3.y"
    { store2syl(twosylqx(0154, yyvsp[-5], yyvsp[-1])); }
    break;

  case 178:
#line 324 "kal3.y"
    { store2syl(twosylqx(0155, yyvsp[-5], yyvsp[-1])); }
    break;

  case 179:
#line 325 "kal3.y"
    { store2syl(twosylqx(0156, yyvsp[-5], yyvsp[-1])); }
    break;

  case 180:
#line 326 "kal3.y"
    { store2syl(twosylqx(0157, yyvsp[-5], yyvsp[-1])); }
    break;

  case 181:
#line 327 "kal3.y"
    { storesyl(001); }
    break;

  case 182:
#line 328 "kal3.y"
    { storesyl(002); }
    break;

  case 183:
#line 329 "kal3.y"
    { storesyl(003); }
    break;

  case 184:
#line 337 "kal3.y"
    { storesyl(004); }
    break;

  case 185:
#line 338 "kal3.y"
    { storesyl(005); }
    break;

  case 186:
#line 339 "kal3.y"
    { storesyl(007); }
    break;

  case 187:
#line 340 "kal3.y"
    { storesyl(034); }
    break;

  case 188:
#line 341 "kal3.y"
    { storesyl(035); }
    break;

  case 189:
#line 342 "kal3.y"
    { store2syl(twosylshetc(0163, 2*yyvsp[-1] + 1)); }
    break;

  case 190:
#line 343 "kal3.y"
    { store2syl(twosylqx(0163, yyvsp[-1], 0)); }
    break;

  case 191:
#line 344 "kal3.y"
    { storesyl(004); }
    break;

  case 192:
#line 345 "kal3.y"
    { storesyl(005); }
    break;

  case 193:
#line 346 "kal3.y"
    { storesyl(007); }
    break;

  case 194:
#line 347 "kal3.y"
    { storesyl(034); }
    break;

  case 195:
#line 348 "kal3.y"
    { storesyl(035); }
    break;

  case 196:
#line 349 "kal3.y"
    { store2syl(twosylshetc(0163, 2*yyvsp[-1] + 1)); }
    break;

  case 197:
#line 350 "kal3.y"
    { store2syl(twosylqx(0163, yyvsp[-1], 0)); }
    break;

  case 198:
#line 351 "kal3.y"
    { storesyl(004); }
    break;

  case 199:
#line 352 "kal3.y"
    { storesyl(005); }
    break;

  case 200:
#line 353 "kal3.y"
    { storesyl(007); }
    break;

  case 201:
#line 354 "kal3.y"
    { storesyl(034); }
    break;

  case 202:
#line 355 "kal3.y"
    { storesyl(035); }
    break;

  case 203:
#line 356 "kal3.y"
    { store2syl(twosylshetc(0163, 2*yyvsp[-1] + 1)); }
    break;

  case 204:
#line 357 "kal3.y"
    { store2syl(twosylqx(0163, yyvsp[-1], 0)); }
    break;

  case 205:
#line 358 "kal3.y"
    { storesyl(010); }
    break;

  case 206:
#line 359 "kal3.y"
    { storesyl(011); }
    break;

  case 207:
#line 360 "kal3.y"
    { storesyl(012); }
    break;

  case 208:
#line 361 "kal3.y"
    { storesyl(013); }
    break;

  case 209:
#line 362 "kal3.y"
    { storesyl(014); }
    break;

  case 210:
#line 363 "kal3.y"
    { storesyl(015); }
    break;

  case 211:
#line 364 "kal3.y"
    { storesyl(016); }
    break;

  case 212:
#line 365 "kal3.y"
    { storesyl(017); }
    break;

  case 213:
#line 366 "kal3.y"
    { storesyl(020); }
    break;

  case 214:
#line 367 "kal3.y"
    { storesyl(021); }
    break;

  case 215:
#line 368 "kal3.y"
    { storesyl(022); }
    break;

  case 216:
#line 369 "kal3.y"
    { storesyl(023); }
    break;

  case 217:
#line 370 "kal3.y"
    { storesyl(024); }
    break;

  case 218:
#line 371 "kal3.y"
    { storesyl(025); }
    break;

  case 219:
#line 372 "kal3.y"
    { storesyl(026); }
    break;

  case 220:
#line 373 "kal3.y"
    { storesyl(027); }
    break;

  case 221:
#line 374 "kal3.y"
    { storesyl(030); }
    break;

  case 222:
#line 375 "kal3.y"
    { storesyl(031); }
    break;

  case 223:
#line 376 "kal3.y"
    { storesyl(032); }
    break;

  case 224:
#line 377 "kal3.y"
    { storesyl(033); }
    break;

  case 225:
#line 378 "kal3.y"
    { storesyl(036); }
    break;

  case 226:
#line 379 "kal3.y"
    { storesyl(037); }
    break;

  case 227:
#line 380 "kal3.y"
    { storesyl(041); }
    break;

  case 228:
#line 381 "kal3.y"
    { storesyl(042); }
    break;

  case 229:
#line 382 "kal3.y"
    { storesyl(043); }
    break;

  case 230:
#line 383 "kal3.y"
    { storesyl(045); }
    break;

  case 231:
#line 384 "kal3.y"
    { storesyl(047); }
    break;

  case 232:
#line 385 "kal3.y"
    { storesyl(050); }
    break;

  case 233:
#line 386 "kal3.y"
    { storesyl(051); }
    break;

  case 234:
#line 387 "kal3.y"
    { storesyl(052); }
    break;

  case 235:
#line 388 "kal3.y"
    { storesyl(053); }
    break;

  case 236:
#line 389 "kal3.y"
    { storesyl(054); }
    break;

  case 237:
#line 390 "kal3.y"
    { storesyl(056); }
    break;

  case 238:
#line 391 "kal3.y"
    { storesyl(057); }
    break;

  case 239:
#line 392 "kal3.y"
    { storesyl(044); }
    break;

  case 240:
#line 393 "kal3.y"
    { storesyl(060); }
    break;

  case 241:
#line 394 "kal3.y"
    { storesyl(061); }
    break;

  case 242:
#line 395 "kal3.y"
    { storesyl(062); }
    break;

  case 243:
#line 396 "kal3.y"
    { storesyl(063); }
    break;

  case 244:
#line 397 "kal3.y"
    { storesyl(064); }
    break;

  case 245:
#line 398 "kal3.y"
    { storesyl(044); }
    break;

  case 246:
#line 399 "kal3.y"
    { storesyl(060); }
    break;

  case 247:
#line 400 "kal3.y"
    { storesyl(061); }
    break;

  case 248:
#line 401 "kal3.y"
    { storesyl(062); }
    break;

  case 249:
#line 402 "kal3.y"
    { storesyl(063); }
    break;

  case 250:
#line 403 "kal3.y"
    { storesyl(064); }
    break;

  case 251:
#line 404 "kal3.y"
    { storesyl(044); }
    break;

  case 252:
#line 405 "kal3.y"
    { storesyl(060); }
    break;

  case 253:
#line 406 "kal3.y"
    { storesyl(061); }
    break;

  case 254:
#line 407 "kal3.y"
    { storesyl(062); }
    break;

  case 255:
#line 408 "kal3.y"
    { storesyl(063); }
    break;

  case 256:
#line 409 "kal3.y"
    { storesyl(064); }
    break;

  case 257:
#line 411 "kal3.y"
    { storesyl(044); }
    break;

  case 258:
#line 412 "kal3.y"
    { storesyl(060); }
    break;

  case 259:
#line 413 "kal3.y"
    { storesyl(061); }
    break;

  case 260:
#line 414 "kal3.y"
    { storesyl(062); }
    break;

  case 261:
#line 415 "kal3.y"
    { storesyl(063); }
    break;

  case 262:
#line 416 "kal3.y"
    { storesyl(064); }
    break;

  case 263:
#line 418 "kal3.y"
    { storesyl(065); }
    break;

  case 264:
#line 419 "kal3.y"
    { storesyl(066); }
    break;

  case 265:
#line 420 "kal3.y"
    { storesyl(067); }
    break;

  case 266:
#line 421 "kal3.y"
    { storesyl(070); }
    break;

  case 267:
#line 422 "kal3.y"
    { storesyl(071); }
    break;

  case 268:
#line 423 "kal3.y"
    { storesyl(072); }
    break;

  case 269:
#line 424 "kal3.y"
    { storesyl(074); }
    break;

  case 270:
#line 425 "kal3.y"
    { storesyl(075); }
    break;

  case 271:
#line 426 "kal3.y"
    { storesyl(077); }
    break;

  case 272:
#line 428 "kal3.y"
    { store2syl(twosylshetc(0176, 128>>yyvsp[-1]));  }
    break;

  case 273:
#line 429 "kal3.y"
    { store2syl(twosylshetc(0175, 128>>yyvsp[-1]));  }
    break;

  case 274:
#line 431 "kal3.y"
    {  newproutine(-1, -1); }
    break;

  case 275:
#line 433 "kal3.y"
    { store2syl(twosylqx(0140, yyvsp[-1], 0)); }
    break;

  case 276:
#line 434 "kal3.y"
    { store2syl(twosylqx(0141, yyvsp[-1], 0)); }
    break;

  case 277:
#line 435 "kal3.y"
    { store2syl(twosylqx(0142, yyvsp[-1], 0)); }
    break;

  case 278:
#line 436 "kal3.y"
    { store2syl(twosylqx(0143, yyvsp[-1], 0)); }
    break;

  case 279:
#line 437 "kal3.y"
    { store2syl(twosylqx(0144 + ((yyvsp[-1]-1)&1)*2, yyvsp[-4], 0)); }
    break;

  case 280:
#line 438 "kal3.y"
    { store2syl(twosylqx(0144 + ((yyvsp[-1]-1)&1)*2, yyvsp[-3], 0)); }
    break;

  case 281:
#line 439 "kal3.y"
    { store2syl(twosylqx(0145 + ((yyvsp[-1]-1)&1)*2, yyvsp[-4], 0)); }
    break;

  case 282:
#line 441 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 2)); }
    break;

  case 283:
#line 442 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 3)); }
    break;

  case 284:
#line 443 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 4)); }
    break;

  case 285:
#line 444 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 5)); }
    break;

  case 286:
#line 445 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 010)); }
    break;

  case 287:
#line 446 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 011)); }
    break;

  case 288:
#line 447 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 016)); }
    break;

  case 289:
#line 449 "kal3.y"
    { store2syl(twosylqx(0171, yyvsp[-1], 2)); }
    break;

  case 290:
#line 450 "kal3.y"
    { store2syl(twosylqx(0171, yyvsp[-1], 4)); }
    break;

  case 291:
#line 451 "kal3.y"
    { store2syl(twosylqx(0171, yyvsp[-1], 010)); }
    break;

  case 292:
#line 452 "kal3.y"
    { store2syl(twosylqx(0171, yyvsp[-1], 016)); }
    break;

  case 293:
#line 454 "kal3.y"
    { store2syl(twosylqx(0172, yyvsp[-1], 2)); }
    break;

  case 294:
#line 455 "kal3.y"
    { store2syl(twosylqx(0172, yyvsp[-1], 4)); }
    break;

  case 295:
#line 456 "kal3.y"
    { store2syl(twosylqx(0172, yyvsp[-1], 010)); }
    break;

  case 296:
#line 457 "kal3.y"
    { store2syl(twosylqx(0172, yyvsp[-1], 016)); }
    break;

  case 297:
#line 459 "kal3.y"
    { store2syl(twosylshetc(0173, 0)); }
    break;

  case 298:
#line 460 "kal3.y"
    { store2syl(twosylshetc(0174, 0)); }
    break;

  case 299:
#line 462 "kal3.y"
    { store2syl(twosylshetc(0161, 2*yyvsp[-1] + 1)); }
    break;

  case 300:
#line 463 "kal3.y"
    { store2syl(twosylshetc(0162, 2*yyvsp[-1] + 1)); }
    break;

  case 301:
#line 464 "kal3.y"
    { store2syl(twosylshetc(0164, 2*yyvsp[-1] + 1)); }
    break;

  case 302:
#line 465 "kal3.y"
    { store2syl(twosylshetc(0166, 2*yyvsp[-1] + 1)); }
    break;

  case 303:
#line 466 "kal3.y"
    { store2syl(twosylshetc(0167, 2*yyvsp[-1] + 1)); }
    break;

  case 304:
#line 468 "kal3.y"
    { store2syl(twosylqx(0161, yyvsp[-1], 0)); }
    break;

  case 305:
#line 469 "kal3.y"
    { store2syl(twosylqx(0162, yyvsp[-1], 0)); }
    break;

  case 306:
#line 470 "kal3.y"
    { store2syl(twosylqx(0164, yyvsp[-1], 0)); }
    break;

  case 307:
#line 471 "kal3.y"
    { store2syl(twosylqx(0166, yyvsp[-1], 0)); }
    break;

  case 308:
#line 472 "kal3.y"
    { store2syl(twosylqx(0167, yyvsp[-1], 0)); }
    break;

  case 310:
#line 475 "kal3.y"
    { store3syl(0200, (0220<<8)); }
    break;

  case 311:
#line 476 "kal3.y"
    { store3syl(0222, (0360<<8)); }
    break;

  case 312:
#line 477 "kal3.y"
    { store3syl(0202 - ((yyvsp[-1]&1)<<1), (0360<<8)+(yyvsp[-1]>>1)); }
    break;

  case 313:
#line 478 "kal3.y"
    { store3syl(0202, (0360<<8)); }
    break;

  case 314:
#line 479 "kal3.y"
    { store3syl(0202, (0360<<8) + yyvsp[-1]); }
    break;

  case 315:
#line 480 "kal3.y"
    { store3syl(0202, (0360<<8) + codeloc(yyvsp[-1], 0)); }
    break;

  case 316:
#line 482 "kal3.y"
    { store3syl(0202 - ((yyvsp[-4]&1)<<1), (0360<<8) + codeloc(yyvsp[-1], 0)); }
    break;

  case 317:
#line 484 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 000)); }
    break;

  case 318:
#line 485 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 001)); }
    break;

  case 319:
#line 486 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 002)); }
    break;

  case 320:
#line 487 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 004)); }
    break;

  case 321:
#line 488 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 010)); }
    break;

  case 322:
#line 489 "kal3.y"
    { store2syl(twosylqx(0121, yyvsp[-1], 000)); }
    break;

  case 323:
#line 490 "kal3.y"
    { store2syl(twosylqx(0122, yyvsp[-1], 000)); }
    break;

  case 324:
#line 491 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 000)); }
    break;

  case 325:
#line 492 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 000)); }
    break;

  case 326:
#line 493 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 002)); }
    break;

  case 327:
#line 494 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 004)); }
    break;

  case 328:
#line 495 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 010)); }
    break;

  case 329:
#line 496 "kal3.y"
    { store2syl(twosylqx(0125, yyvsp[-1], 000)); }
    break;

  case 330:
#line 497 "kal3.y"
    { store2syl(twosylqx(0125, yyvsp[-1], 000)); }
    break;

  case 331:
#line 498 "kal3.y"
    { store2syl(twosylqx(0125, yyvsp[-1], 010)); }
    break;

  case 332:
#line 499 "kal3.y"
    { store2syl(twosylqx(0126, yyvsp[-1], 000)); }
    break;

  case 333:
#line 500 "kal3.y"
    { store2syl(twosylqx(0127, yyvsp[-1], 000)); }
    break;

  case 334:
#line 501 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 000)); }
    break;

  case 335:
#line 502 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 000)); }
    break;

  case 336:
#line 503 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 010)); }
    break;

  case 337:
#line 504 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 000)); }
    break;

  case 338:
#line 505 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 010)); }
    break;

  case 339:
#line 506 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 014)); }
    break;

  case 340:
#line 507 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 014)); }
    break;

  case 341:
#line 508 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 004)); }
    break;

  case 342:
#line 509 "kal3.y"
    { store2syl(twosylqx(0131, yyvsp[-1], 000)); }
    break;

  case 343:
#line 510 "kal3.y"
    { store2syl(twosylqx(0131, yyvsp[-1], 000)); }
    break;

  case 344:
#line 511 "kal3.y"
    { store2syl(twosylqx(0131, yyvsp[-1], 000)); }
    break;

  case 345:
#line 512 "kal3.y"
    { store2syl(twosylqx(0131, yyvsp[-1], 010)); }
    break;

  case 346:
#line 513 "kal3.y"
    { store2syl(twosylqx(0134, yyvsp[-1], 000)); }
    break;

  case 347:
#line 514 "kal3.y"
    { store2syl(twosylqx(0134, yyvsp[-1], 002)); }
    break;

  case 348:
#line 515 "kal3.y"
    { store2syl(twosylqx(0136, yyvsp[-1], 000)); }
    break;

  case 349:
#line 516 "kal3.y"
    { store2syl(twosylqx(0136, yyvsp[-1], 010)); }
    break;

  case 350:
#line 519 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 000)); }
    break;

  case 351:
#line 520 "kal3.y"
    { store2syl(twosylqx(0125, yyvsp[-1], 000)); }
    break;

  case 352:
#line 521 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 010)); }
    break;

  case 353:
#line 522 "kal3.y"
    { store2syl(twosylqx(0125, yyvsp[-1], 010)); }
    break;

  case 354:
#line 523 "kal3.y"
    { store2syl(twosylqx(0126, yyvsp[-1], 000)); }
    break;

  case 355:
#line 524 "kal3.y"
    { store2syl(twosylqx(0127, yyvsp[-1], 000)); }
    break;

  case 356:
#line 525 "kal3.y"
    { store2syl(twosylqx(0126, yyvsp[-1], 010)); }
    break;

  case 357:
#line 526 "kal3.y"
    { store2syl(twosylqx(0127, yyvsp[-1], 010)); }
    break;

  case 358:
#line 528 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 000)); }
    break;

  case 359:
#line 529 "kal3.y"
    { store2syl(twosylqx(0131, yyvsp[-1], 000)); }
    break;

  case 360:
#line 530 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 010)); }
    break;

  case 361:
#line 531 "kal3.y"
    { store2syl(twosylqx(0131, yyvsp[-1], 010)); }
    break;

  case 362:
#line 532 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 014)); }
    break;

  case 363:
#line 533 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 004)); }
    break;

  case 364:
#line 535 "kal3.y"
    { store2syl(twosylqx(0134, yyvsp[-1], 000)); }
    break;

  case 365:
#line 536 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 010)); }
    break;

  case 366:
#line 537 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 004)); }
    break;

  case 367:
#line 538 "kal3.y"
    { store2syl(twosylqx(0136, yyvsp[-1], 010)); }
    break;

  case 368:
#line 539 "kal3.y"
    { store2syl(twosylqx(0136, yyvsp[-1], 000)); }
    break;

  case 369:
#line 540 "kal3.y"
    { store2syl(twosylqx(0122, yyvsp[-1], 000)); }
    break;

  case 370:
#line 541 "kal3.y"
    { store2syl(twosylqx(0134, yyvsp[-1], 010)); }
    break;

  case 371:
#line 542 "kal3.y"
    { store2syl(twosylqx(0134, yyvsp[-1], 004)); }
    break;

  case 372:
#line 543 "kal3.y"
    { store2syl(twosylqx(0135, yyvsp[-1], 000)); }
    break;

  case 373:
#line 544 "kal3.y"
    { store2syl(twosylqx(0137, yyvsp[-1], 000)); }
    break;

  case 374:
#line 546 "kal3.y"
    { store2syl(twosylqx(0132, yyvsp[-1], 000)); }
    break;

  case 375:
#line 547 "kal3.y"
    { store2syl(twosylqx(0133, yyvsp[-1], 000)); }
    break;

  case 376:
#line 548 "kal3.y"
    { store2syl(twosylqx(0133, yyvsp[-1], 010)); }
    break;

  case 377:
#line 549 "kal3.y"
    { store2syl(twosylqx(0132, yyvsp[-1], 010)); }
    break;

  case 378:
#line 552 "kal3.y"
    { startnewword(); }
    break;


    }

/* Line 991 of yacc.c.  */
#line 3806 "y.tab.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("syntax error, unexpected ") + 1;
	  yysize += yystrlen (yytname[yytype]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* Return failure if at end of input.  */
      if (yychar == YYEOF)
        {
	  /* Pop the error token.  */
          YYPOPSTACK;
	  /* Pop the rest of the stack.  */
	  while (yyss < yyssp)
	    {
	      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
	      yydestruct (yystos[*yyssp], yyvsp);
	      YYPOPSTACK;
	    }
	  YYABORT;
        }

      YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
      yydestruct (yytoken, &yylval);
      yychar = YYEMPTY;

    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab2;


/*----------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action.  |
`----------------------------------------------------*/
yyerrlab1:

  /* Suppress GCC warning that yyerrlab1 is unused when no action
     invokes YYERROR.  */
#if defined (__GNUC_MINOR__) && 2093 <= (__GNUC__ * 1000 + __GNUC_MINOR__)
  __attribute__ ((__unused__))
#endif


  goto yyerrlab2;


/*---------------------------------------------------------------.
| yyerrlab2 -- pop states until the error token can be shifted.  |
`---------------------------------------------------------------*/
yyerrlab2:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      yyvsp--;
      yystate = *--yyssp;

      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 555 "kal3.y"




