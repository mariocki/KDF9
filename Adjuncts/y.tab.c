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
#line 10 "kal3.y"

#include <ctype.h>
/* KDF9 assembler for USERCODE */
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
#line 206 "y.tab.c"

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
#define YYFINAL  51
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1291

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  50
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  18
/* YYNRULES -- Number of rules. */
#define YYNRULES  403
/* YYNRULES -- Number of states. */
#define YYNSTATES  1140

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
       2,     2,     2,    22,     2,    24,    49,    48,     2,     2,
       2,     2,    27,    34,     2,    20,    36,    23,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    10,
      28,    17,    30,     2,     2,     9,    35,    32,    41,     6,
      38,    13,    18,    42,    21,    43,    19,    14,    26,    12,
      11,    37,     5,     7,     8,    33,    15,    16,    44,    40,
      29,     2,     2,     2,     2,    31,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      45,     2,     2,     2,     2,     2,    39,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    25,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,    46,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    47,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short yyprhs[] =
{
       0,     0,     3,     6,    24,    44,    53,    64,    67,    71,
      74,    78,    81,    85,    88,    95,   102,   108,   116,   120,
     124,   130,   135,   140,   142,   144,   147,   149,   152,   156,
     159,   162,   166,   171,   176,   182,   188,   193,   198,   204,
     211,   217,   224,   232,   239,   246,   253,   261,   267,   275,
     282,   289,   297,   304,   311,   317,   324,   331,   337,   343,
     351,   358,   363,   369,   375,   382,   388,   395,   401,   408,
     415,   423,   431,   435,   441,   447,   454,   461,   465,   467,
     469,   472,   475,   478,   480,   483,   488,   492,   498,   502,
     505,   510,   517,   522,   525,   530,   537,   542,   549,   552,
     557,   560,   567,   575,   583,   588,   594,   600,   604,   612,
     621,   630,   636,   643,   649,   653,   661,   670,   679,   685,
     692,   700,   703,   705,   707,   710,   713,   716,   719,   722,
     725,   728,   731,   734,   737,   740,   743,   746,   749,   752,
     755,   758,   761,   764,   767,   770,   773,   776,   779,   782,
     785,   788,   792,   796,   801,   804,   809,   812,   815,   818,
     821,   824,   827,   833,   840,   847,   855,   862,   870,   878,
     887,   890,   893,   895,   897,   899,   902,   904,   908,   914,
     918,   921,   925,   930,   936,   942,   949,   955,   962,   969,
     976,   983,   991,   999,  1007,  1009,  1012,  1020,  1028,  1037,
    1045,  1054,  1063,  1071,  1075,  1080,  1086,  1090,  1095,  1100,
    1104,  1107,  1112,  1118,  1122,  1127,  1132,  1136,  1139,  1144,
    1150,  1154,  1159,  1164,  1168,  1171,  1176,  1182,  1188,  1192,
    1198,  1203,  1211,  1216,  1223,  1230,  1238,  1247,  1252,  1257,
    1264,  1272,  1277,  1282,  1288,  1294,  1299,  1304,  1307,  1313,
    1319,  1324,  1330,  1335,  1340,  1346,  1352,  1359,  1363,  1368,
    1371,  1375,  1379,  1382,  1386,  1390,  1395,  1399,  1403,  1406,
    1410,  1414,  1419,  1423,  1427,  1430,  1434,  1438,  1443,  1447,
    1453,  1458,  1464,  1470,  1477,  1483,  1488,  1493,  1498,  1505,
    1512,  1518,  1522,  1526,  1533,  1537,  1542,  1550,  1556,  1562,
    1567,  1572,  1579,  1585,  1592,  1597,  1603,  1608,  1614,  1619,
    1625,  1630,  1634,  1638,  1642,  1646,  1652,  1658,  1664,  1670,
    1676,  1683,  1689,  1696,  1702,  1709,  1715,  1722,  1730,  1737,
    1745,  1752,  1754,  1759,  1766,  1773,  1779,  1787,  1796,  1806,
    1812,  1818,  1824,  1830,  1836,  1842,  1849,  1856,  1863,  1870,
    1877,  1884,  1891,  1898,  1905,  1912,  1919,  1926,  1933,  1940,
    1947,  1954,  1961,  1969,  1977,  1985,  1993,  2001,  2009,  2017,
    2025,  2033,  2041,  2050,  2060,  2067,  2074,  2081,  2088,  2095,
    2102,  2109,  2116,  2123,  2130,  2137,  2144,  2151,  2158,  2165,
    2172,  2179,  2186,  2193,  2200,  2207,  2214,  2221,  2228,  2235,
    2242,  2249,  2256,  2258
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      51,     0,    -1,    52,    62,    -1,     5,     6,     7,     8,
       9,     5,     8,    10,    62,    11,     5,    12,    13,     5,
       9,    14,    10,    -1,     5,     6,     7,     8,     9,     5,
       8,    10,    62,    11,     5,    12,    13,     5,     9,    14,
      14,     6,    10,    -1,    11,     5,    12,    13,     5,     9,
      14,    10,    -1,    11,     5,    12,    13,     5,     9,    14,
      14,     6,    10,    -1,    53,    52,    -1,    15,     3,    10,
      -1,    15,    10,    -1,    16,     3,    10,    -1,    16,    10,
      -1,    63,     3,    10,    -1,    63,    10,    -1,    63,     3,
      17,     6,     3,    10,    -1,    18,     3,    17,     6,     3,
      10,    -1,    63,     3,    17,     6,    10,    -1,     7,     8,
       9,     5,     8,     3,    10,    -1,     7,     8,    10,    -1,
       8,    19,    10,    -1,    14,     8,    11,     5,    10,    -1,
       7,     8,     3,    10,    -1,     8,    19,     3,    10,    -1,
       4,    -1,     3,    -1,    20,     3,    -1,     3,    -1,    11,
       3,    -1,     3,    11,     3,    -1,     6,     3,    -1,    19,
       3,    -1,    21,    55,    10,    -1,    21,     7,    55,    10,
      -1,    21,    55,    17,    10,    -1,    21,    55,    22,    17,
      10,    -1,    21,    55,    23,    17,    10,    -1,    21,    55,
      24,    10,    -1,    21,    55,    25,    10,    -1,    21,    55,
      26,     6,    10,    -1,    21,    55,    27,    26,     6,    10,
      -1,    21,    55,    28,    29,    10,    -1,    21,    55,    19,
       8,    29,    10,    -1,    21,    55,    27,    19,     8,    29,
      10,    -1,    21,    55,    30,    17,    29,    10,    -1,    21,
      55,    13,     6,    29,    10,    -1,    21,    55,    31,    30,
      29,    10,    -1,    21,    55,    27,    13,     6,    29,    10,
      -1,    21,    55,    30,    29,    10,    -1,    21,    55,    27,
      13,     8,    29,    10,    -1,    21,    55,    13,     8,    29,
      10,    -1,    21,    55,    28,    17,    29,    10,    -1,    21,
      55,    27,    19,     6,    29,    10,    -1,    21,    55,    19,
       6,    29,    10,    -1,    21,    55,    31,    28,    29,    10,
      -1,    21,    55,    17,    29,    10,    -1,    21,    55,    22,
      17,    29,    10,    -1,    21,    55,    23,    17,    29,    10,
      -1,    21,    55,    24,    29,    10,    -1,    21,    55,    25,
      29,    10,    -1,    21,    55,    27,    26,     6,    29,    10,
      -1,    21,    55,    26,     6,    29,    10,    -1,    21,    55,
      15,    10,    -1,    21,    55,    26,    15,    10,    -1,    21,
      55,     6,    26,    10,    -1,    21,    55,    26,     6,    26,
      10,    -1,    21,    55,     6,    21,    10,    -1,    21,    55,
      26,     6,    21,    10,    -1,    21,    55,     8,     5,    10,
      -1,    21,    55,    26,     8,     5,    10,    -1,    21,    55,
      32,     3,    29,    10,    -1,    21,    55,    32,     3,    26,
      29,    10,    -1,    21,    55,    32,     3,    26,    29,     7,
      -1,    15,     3,    17,    -1,    15,     3,    23,     3,    17,
      -1,    15,     3,    17,    61,    10,    -1,    15,     3,    33,
      17,    61,    10,    -1,    15,     3,    19,    17,    61,    10,
      -1,    57,    11,    10,    -1,     3,    -1,    54,    -1,    34,
      54,    -1,     9,    64,    -1,    35,    59,    -1,    59,    -1,
      35,    59,    -1,    35,    59,    23,     3,    -1,    59,    36,
       3,    -1,    59,    36,     3,    23,     3,    -1,    59,    23,
       3,    -1,    20,    59,    -1,    20,    59,    36,     3,    -1,
      20,    59,    36,     3,    23,     3,    -1,    20,    59,    23,
       3,    -1,    34,    59,    -1,    34,    59,    36,     3,    -1,
      34,    59,    36,     3,    23,     3,    -1,    34,    59,    23,
       3,    -1,    37,    60,    23,    60,    23,    60,    -1,     9,
      64,    -1,    38,    59,    36,     3,    -1,    38,    59,    -1,
      38,    59,    36,     3,    39,     3,    -1,    38,    59,    36,
       3,    39,    20,     3,    -1,    38,    59,    36,     3,    39,
      34,     3,    -1,    38,    59,    39,     3,    -1,    38,    59,
      39,    20,     3,    -1,    38,    20,    59,    36,     3,    -1,
      38,    20,    59,    -1,    38,    20,    59,    36,     3,    39,
       3,    -1,    38,    20,    59,    36,     3,    39,    20,     3,
      -1,    38,    20,    59,    36,     3,    39,    34,     3,    -1,
      38,    20,    59,    39,     3,    -1,    38,    20,    59,    39,
      20,     3,    -1,    38,    34,    59,    36,     3,    -1,    38,
      34,    59,    -1,    38,    34,    59,    36,     3,    39,     3,
      -1,    38,    34,    59,    36,     3,    39,    20,     3,    -1,
      38,    34,    59,    36,     3,    39,    34,     3,    -1,    38,
      34,    59,    39,     3,    -1,    38,    34,    59,    39,    20,
       3,    -1,    34,    59,    36,     3,    23,    20,     3,    -1,
      62,    67,    -1,    67,    -1,    40,    -1,    40,     9,    -1,
      40,    35,    -1,    40,    32,    -1,    40,    41,    -1,    40,
       6,    -1,    40,    38,    -1,    40,    13,    -1,    40,    18,
      -1,    40,    42,    -1,    40,    21,    -1,    40,    43,    -1,
      40,    19,    -1,    40,    14,    -1,    40,    26,    -1,    40,
      12,    -1,    40,    11,    -1,    40,    37,    -1,    40,     5,
      -1,    40,     7,    -1,    40,     8,    -1,    40,    33,    -1,
      40,    15,    -1,    40,    16,    -1,    40,    44,    -1,    40,
      40,    -1,    40,    29,    -1,    15,    54,    -1,    15,    54,
      33,    -1,    15,    54,    19,    -1,    15,    54,    11,     3,
      -1,     5,    54,    -1,     5,    54,    11,     3,    -1,    11,
       3,    -1,     6,    54,    -1,    63,    54,    -1,    16,    54,
      -1,    29,    54,    -1,    18,    54,    -1,    14,     3,    14,
       3,    10,    -1,    14,     3,    14,     3,    37,    10,    -1,
      14,     3,    14,     3,    26,    10,    -1,    14,     3,    14,
       3,    37,    26,    10,    -1,    14,     3,    14,     3,    18,
      10,    -1,    14,     3,    14,     3,    37,    18,    10,    -1,
      14,     3,    14,     3,    18,    26,    10,    -1,    14,     3,
      14,     3,    37,    18,    26,    10,    -1,    34,     3,    -1,
      20,     3,    -1,     3,    -1,    10,    -1,     4,    -1,     3,
      10,    -1,    58,    -1,    11,     3,    10,    -1,    11,     3,
      15,     3,    10,    -1,    19,     3,    10,    -1,    64,    10,
      -1,    17,    64,    10,    -1,    64,    14,     3,    10,    -1,
      17,    64,    14,     3,    10,    -1,    64,    14,     3,    37,
      10,    -1,    17,    64,    14,     3,    37,    10,    -1,     7,
       6,     8,     3,    10,    -1,     7,     6,     8,    35,    59,
      10,    -1,     7,     6,     8,    20,     3,    10,    -1,     7,
       6,     8,    34,     3,    10,    -1,     7,     6,     8,     9,
      64,    10,    -1,     7,     6,     8,     9,    64,    33,    10,
      -1,     7,     6,     8,     9,    64,    19,    10,    -1,     7,
       6,     8,     9,    11,     3,    10,    -1,    65,    -1,    17,
      65,    -1,    14,     3,     8,    12,    37,     3,    10,    -1,
      42,     3,     8,    12,    37,     3,    10,    -1,    42,    14,
       3,     8,    12,    37,     3,    10,    -1,    32,     3,     8,
      12,    37,     3,    10,    -1,    32,    14,     3,     8,    12,
      37,     3,    10,    -1,    32,    42,     3,     8,    12,    37,
       3,    10,    -1,    37,     3,     8,    12,    37,     3,    10,
      -1,    15,     5,    10,    -1,    17,     8,     5,    10,    -1,
      35,    42,     8,     7,    10,    -1,    45,    38,    10,    -1,
      45,    41,    38,    10,    -1,    45,    34,    38,    10,    -1,
      45,    41,    10,    -1,    45,    10,    -1,    45,    34,    66,
      10,    -1,    45,    34,    32,     3,    10,    -1,    46,    38,
      10,    -1,    46,    41,    38,    10,    -1,    46,    34,    38,
      10,    -1,    46,    41,    10,    -1,    46,    10,    -1,    46,
      34,    66,    10,    -1,    46,    34,    32,     3,    10,    -1,
      44,    38,    10,    -1,    44,    41,    38,    10,    -1,    44,
      34,    38,    10,    -1,    44,    41,    10,    -1,    44,    10,
      -1,    44,    34,    66,    10,    -1,    44,    34,    32,     3,
      10,    -1,    26,     6,    13,    41,    10,    -1,    12,     5,
      10,    -1,    11,     6,     5,    14,    10,    -1,     8,    12,
      35,    10,    -1,     5,    12,    33,    26,    41,    18,    10,
      -1,    26,     6,    15,    10,    -1,     5,    12,    33,    26,
      41,    10,    -1,    41,    33,    14,    14,    40,    10,    -1,
       5,    12,    33,    26,    41,    38,    10,    -1,     5,    12,
      33,    26,    41,    18,    38,    10,    -1,    20,    41,    38,
      10,    -1,    34,    41,    38,    10,    -1,    38,    19,    12,
       9,     8,    10,    -1,    38,    19,    12,     9,     8,    41,
      10,    -1,     9,    35,     7,    10,    -1,    26,     6,    13,
      10,    -1,     9,    35,     7,    38,    10,    -1,    26,     6,
      13,    38,    10,    -1,    14,     9,    44,    10,    -1,    26,
      12,     8,    10,    -1,    20,    10,    -1,     7,    42,    13,
      26,    10,    -1,    29,     6,     5,    12,    10,    -1,    41,
      33,    11,    10,    -1,    41,    33,    11,    41,    10,    -1,
      38,    42,    44,    10,    -1,     7,     8,     5,    10,    -1,
      32,    12,    26,     8,    10,    -1,     5,     6,    15,    41,
      10,    -1,     6,     5,     9,     7,     6,    10,    -1,    20,
      41,    10,    -1,     9,    26,    41,    10,    -1,    34,    10,
      -1,    34,    41,    10,    -1,    47,    42,    10,    -1,    47,
      10,    -1,    47,    41,    10,    -1,    47,    38,    10,    -1,
      47,    41,    38,    10,    -1,    47,     5,    10,    -1,    48,
      42,    10,    -1,    48,    10,    -1,    48,    41,    10,    -1,
      48,    38,    10,    -1,    48,    41,    38,    10,    -1,    48,
       5,    10,    -1,    23,    42,    10,    -1,    23,    10,    -1,
      23,    41,    10,    -1,    23,    38,    10,    -1,    23,    41,
      38,    10,    -1,    23,     5,    10,    -1,    41,    42,    15,
      42,    10,    -1,    41,    42,    15,    10,    -1,    41,    42,
      15,    41,    10,    -1,    41,    42,    15,    38,    10,    -1,
      41,    42,    15,    41,    38,    10,    -1,    41,    42,    15,
       5,    10,    -1,     5,     6,    15,    10,    -1,    32,     9,
      35,    10,    -1,    38,     5,    35,    10,    -1,     7,     8,
       9,    26,    41,    10,    -1,    26,     6,    13,    41,    38,
      10,    -1,    14,     9,    44,    38,    10,    -1,    34,    38,
      10,    -1,    20,    38,    10,    -1,     7,    42,    13,    26,
      38,    10,    -1,    43,     3,    10,    -1,    17,    43,     3,
      10,    -1,    38,    42,    26,    42,     7,    18,    10,    -1,
      14,    34,    42,     3,    10,    -1,    14,    20,    42,     3,
      10,    -1,    26,    32,     3,    10,    -1,    41,    32,     3,
      10,    -1,    42,     3,    17,    34,     3,    10,    -1,    42,
       3,    17,     3,    10,    -1,    42,     3,    17,    20,     3,
      10,    -1,    17,    14,     3,    10,    -1,    17,     5,    14,
       3,    10,    -1,    17,    42,     3,    10,    -1,    17,     5,
      42,     3,    10,    -1,    17,    32,     3,    10,    -1,    17,
       5,    32,     3,    10,    -1,    17,    37,     3,    10,    -1,
      14,     3,    10,    -1,    42,     3,    10,    -1,    32,     3,
      10,    -1,    37,     3,    10,    -1,    17,    34,    14,     3,
      10,    -1,    17,    34,    42,     3,    10,    -1,    17,    34,
      32,     3,    10,    -1,    17,    34,    37,     3,    10,    -1,
      19,    42,    26,    43,    10,    -1,    17,    19,    42,    26,
      43,    10,    -1,     7,    18,     9,    66,    10,    -1,     7,
      18,     9,    41,    66,    10,    -1,     7,    18,    19,    66,
      10,    -1,     7,    18,    19,    41,    66,    10,    -1,     7,
      18,    32,    66,    10,    -1,     7,    18,     9,    32,     3,
      10,    -1,     7,    18,     9,    41,    32,     3,    10,    -1,
       7,    18,    19,    32,     3,    10,    -1,     7,    18,    19,
      41,    32,     3,    10,    -1,     7,    18,    32,    32,     3,
      10,    -1,    56,    -1,    12,    33,     8,    10,    -1,     6,
      44,    42,     8,    41,    10,    -1,     6,    44,    42,     8,
       3,    10,    -1,     6,    44,    42,     8,    10,    -1,     6,
      44,    42,     8,     9,    64,    10,    -1,     6,    44,    42,
       8,     9,    11,     3,    10,    -1,     6,    44,    42,     8,
       3,     9,    11,     3,    10,    -1,    32,     8,    37,     3,
      10,    -1,    11,     5,    37,     3,    10,    -1,    11,    16,
      37,     3,    10,    -1,     8,    16,    37,     3,    10,    -1,
      19,    11,    37,     3,    10,    -1,    14,    16,    37,     3,
      10,    -1,    14,    19,    35,    37,     3,    10,    -1,    14,
      35,     8,    37,     3,    10,    -1,    11,     9,     5,    37,
       3,    10,    -1,    14,     6,     8,    37,     3,    10,    -1,
      14,    38,     5,    37,     3,    10,    -1,    32,    19,    12,
      37,     3,    10,    -1,     8,    19,    12,    37,     3,    10,
      -1,    11,     5,    32,    37,     3,    10,    -1,    14,     5,
       6,    37,     3,    10,    -1,    11,     5,     6,    37,     3,
      10,    -1,    14,    35,     5,    37,     3,    10,    -1,    11,
      16,    32,    37,     3,    10,    -1,    14,    19,    16,    37,
       3,    10,    -1,    42,    26,     8,    37,     3,    10,    -1,
      14,    16,     6,    37,     3,    10,    -1,    11,    16,     6,
      37,     3,    10,    -1,     8,    16,     6,    37,     3,    10,
      -1,    35,    33,     7,    40,    37,     3,    10,    -1,    14,
      13,     9,    11,    37,     3,    10,    -1,    11,    13,     9,
      11,    37,     3,    10,    -1,    14,    19,    16,     6,    37,
       3,    10,    -1,    14,    38,     7,    43,    37,     3,    10,
      -1,    14,    35,     7,    43,    37,     3,    10,    -1,    14,
       5,    16,    41,    37,     3,    10,    -1,    14,    35,     5,
       6,    37,     3,    10,    -1,    14,    38,     5,     6,    37,
       3,    10,    -1,    11,     5,    32,     6,    37,     3,    10,
      -1,    14,    16,    42,    11,     6,    37,     3,    10,    -1,
      14,     9,    26,    33,     9,    19,    37,     3,    10,    -1,
      11,    42,     9,    37,     3,    10,    -1,    11,    42,    35,
      37,     3,    10,    -1,    11,    42,    32,    37,     3,    10,
      -1,    11,    42,    41,    37,     3,    10,    -1,    11,    42,
       6,    37,     3,    10,    -1,    11,    42,    38,    37,     3,
      10,    -1,    11,    42,    13,    37,     3,    10,    -1,    11,
      42,    18,    37,     3,    10,    -1,    11,    12,     9,    37,
       3,    10,    -1,    11,    12,    35,    37,     3,    10,    -1,
      11,    12,    32,    37,     3,    10,    -1,    11,    12,    41,
      37,     3,    10,    -1,    11,    12,     6,    37,     3,    10,
      -1,    11,    12,    38,    37,     3,    10,    -1,    11,    14,
       9,    37,     3,    10,    -1,    11,    14,    35,    37,     3,
      10,    -1,    11,    14,    32,    37,     3,    10,    -1,    11,
      14,    41,    37,     3,    10,    -1,    11,    14,     6,    37,
       3,    10,    -1,    11,    14,    38,    37,     3,    10,    -1,
      11,    14,    13,    37,     3,    10,    -1,    11,    14,    18,
      37,     3,    10,    -1,    11,    14,    43,    37,     3,    10,
      -1,    11,    14,    19,    37,     3,    10,    -1,    11,    12,
      13,    37,     3,    10,    -1,    11,    12,    18,    37,     3,
      10,    -1,    11,    12,    43,    37,     3,    10,    -1,    11,
      12,    19,    37,     3,    10,    -1,    49,    -1,    27,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   128,   128,   131,   132,   133,   134,   135,   138,   139,
     140,   141,   142,   143,   144,   146,   149,   151,   153,   154,
     155,   156,   157,   158,   161,   162,   165,   166,   167,   168,
     169,   172,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
     201,   202,   203,   204,   205,   206,   207,   208,   209,   210,
     211,   212,   215,   216,   219,   220,   221,   222,   225,   228,
     229,   230,   231,   234,   235,   236,   237,   238,   239,   240,
     241,   242,   243,   244,   245,   246,   247,   248,   249,   250,
     251,   252,   253,   254,   255,   256,   257,   258,   259,   260,
     261,   262,   263,   264,   265,   266,   267,   268,   269,   270,
     272,   275,   276,   279,   280,   281,   282,   283,   284,   285,
     286,   287,   288,   289,   290,   291,   292,   293,   294,   295,
     296,   297,   298,   299,   300,   301,   302,   303,   304,   305,
     308,   309,   310,   311,   312,   313,   314,   315,   316,   317,
     318,   319,   322,   323,   324,   325,   326,   327,   328,   329,
     332,   333,   334,   337,   338,   339,   340,   341,   342,   343,
     345,   346,   348,   349,   350,   351,   353,   354,   355,   356,
     357,   358,   359,   360,   362,   363,   365,   366,   367,   368,
     369,   370,   371,   372,   373,   374,   382,   383,   384,   385,
     386,   387,   388,   389,   390,   391,   392,   393,   394,   395,
     396,   397,   398,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,   412,   413,   414,   415,
     416,   417,   418,   419,   420,   421,   422,   423,   424,   425,
     426,   427,   428,   429,   430,   431,   432,   433,   434,   435,
     436,   437,   438,   439,   440,   441,   442,   443,   444,   445,
     446,   447,   448,   449,   450,   451,   452,   453,   454,   456,
     457,   458,   459,   460,   461,   463,   464,   465,   466,   467,
     468,   469,   470,   471,   473,   474,   476,   478,   479,   480,
     481,   482,   483,   484,   486,   487,   488,   489,   490,   491,
     492,   494,   495,   496,   497,   499,   500,   501,   502,   504,
     505,   507,   508,   509,   510,   511,   513,   514,   515,   516,
     517,   519,   520,   521,   522,   523,   524,   525,   526,   529,
     530,   531,   532,   533,   534,   536,   537,   538,   539,   540,
     541,   542,   543,   544,   545,   546,   547,   548,   549,   550,
     551,   552,   554,   555,   556,   557,   558,   559,   560,   561,
     562,   563,   565,   566,   569,   570,   571,   572,   573,   574,
     575,   576,   578,   579,   580,   581,   582,   583,   585,   586,
     587,   588,   589,   590,   591,   592,   593,   594,   596,   597,
     598,   599,   602,   603
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "UNSIGNED_INTEGER", "COMMENT", "'R'", 
  "'E'", "'S'", "'T'", "'A'", "';'", "'P'", "'O'", "'G'", "'M'", "'V'", 
  "'W'", "'='", "'H'", "'L'", "'-'", "'J'", "'!'", "'/'", "'#'", "'±'", 
  "'N'", "'*'", "'<'", "'Z'", "'>'", "'_'", "'C'", "'U'", "'+'", "'B'", 
  "'.'", "'Q'", "'F'", "'~'", "'Y'", "'D'", "'I'", "'K'", "'X'", "'x'", 
  "'×'", "'÷'", "'%'", "'$'", "$accept", "program", "p0head", "storespec", 
  "integer", "codeloc", "jumpinstr", "vchars", "vspec", "digitsequence", 
  "bit16", "vstoreval", "code", "ystref", "memref", "mqmq", "shiftval", 
  "instruction", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,    82,    69,    83,    84,    65,
      59,    80,    79,    71,    77,    86,    87,    61,    72,    76,
      45,    74,    33,    47,    35,   177,    78,    42,    60,    90,
      62,    95,    67,    85,    43,    66,    46,    81,    70,   126,
      89,    68,    73,    75,    88,   120,   215,   247,    37,    36
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    50,    51,    52,    52,    52,    52,    52,    53,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    53,    53,    53,    54,    54,    55,    55,    55,    55,
      55,    56,    56,    56,    56,    56,    56,    56,    56,    56,
      56,    56,    56,    56,    56,    56,    56,    56,    56,    56,
      56,    56,    56,    56,    56,    56,    56,    56,    56,    56,
      56,    56,    56,    56,    56,    56,    56,    56,    56,    56,
      56,    56,    57,    57,    58,    58,    58,    58,    59,    60,
      60,    60,    60,    61,    61,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    62,    62,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    65,    65,    65,    65,    65,    65,    65,    65,
      66,    66,    66,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     2,    17,    19,     8,    10,     2,     3,     2,
       3,     2,     3,     2,     6,     6,     5,     7,     3,     3,
       5,     4,     4,     1,     1,     2,     1,     2,     3,     2,
       2,     3,     4,     4,     5,     5,     4,     4,     5,     6,
       5,     6,     7,     6,     6,     6,     7,     5,     7,     6,
       6,     7,     6,     6,     5,     6,     6,     5,     5,     7,
       6,     4,     5,     5,     6,     5,     6,     5,     6,     6,
       7,     7,     3,     5,     5,     6,     6,     3,     1,     1,
       2,     2,     2,     1,     2,     4,     3,     5,     3,     2,
       4,     6,     4,     2,     4,     6,     4,     6,     2,     4,
       2,     6,     7,     7,     4,     5,     5,     3,     7,     8,
       8,     5,     6,     5,     3,     7,     8,     8,     5,     6,
       7,     2,     1,     1,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     3,     3,     4,     2,     4,     2,     2,     2,     2,
       2,     2,     5,     6,     6,     7,     6,     7,     7,     8,
       2,     2,     1,     1,     1,     2,     1,     3,     5,     3,
       2,     3,     4,     5,     5,     6,     5,     6,     6,     6,
       6,     7,     7,     7,     1,     2,     7,     7,     8,     7,
       8,     8,     7,     3,     4,     5,     3,     4,     4,     3,
       2,     4,     5,     3,     4,     4,     3,     2,     4,     5,
       3,     4,     4,     3,     2,     4,     5,     5,     3,     5,
       4,     7,     4,     6,     6,     7,     8,     4,     4,     6,
       7,     4,     4,     5,     5,     4,     4,     2,     5,     5,
       4,     5,     4,     4,     5,     5,     6,     3,     4,     2,
       3,     3,     2,     3,     3,     4,     3,     3,     2,     3,
       3,     4,     3,     3,     2,     3,     3,     4,     3,     5,
       4,     5,     5,     6,     5,     4,     4,     4,     6,     6,
       5,     3,     3,     6,     3,     4,     7,     5,     5,     4,
       4,     6,     5,     6,     4,     5,     4,     5,     4,     5,
       4,     3,     3,     3,     3,     5,     5,     5,     5,     5,
       6,     5,     6,     5,     6,     5,     6,     7,     6,     7,
       6,     1,     4,     6,     6,     5,     7,     8,     9,     5,
       5,     5,     5,     5,     5,     6,     6,     6,     6,     6,
       6,     6,     6,     6,     6,     6,     6,     6,     6,     6,
       6,     6,     7,     7,     7,     7,     7,     7,     7,     7,
       7,     7,     8,     9,     6,     6,     6,     6,     6,     6,
       6,     6,     6,     6,     6,     6,     6,     6,     6,     6,
       6,     6,     6,     6,     6,     6,     6,     6,     6,     6,
       6,     6,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned short yydefact[] =
{
       0,    23,     0,     0,     0,     0,     0,     0,     0,     0,
     123,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     9,     0,    11,     0,   141,   128,   142,   143,   124,
     139,   138,   130,   136,   145,   146,   131,   135,   133,   137,
     149,   126,   144,   125,   140,   129,   148,   127,   132,   134,
     147,     1,     0,   174,     0,     0,     0,     0,     0,   173,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   403,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   402,   331,     0,
     176,     2,     0,     0,   194,   122,     7,     0,    13,     0,
       0,     0,    18,     0,    19,     0,     0,     8,    10,     0,
     175,    24,     0,     0,     0,   154,     0,     0,   157,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   156,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      24,     0,   150,   159,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   195,   161,
       0,     0,     0,   247,     0,     0,    26,     0,     0,     0,
       0,     0,     0,   274,     0,     0,     0,     0,     0,     0,
       0,   160,     0,     0,     0,     0,     0,     0,     0,   259,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   224,     0,     0,     0,   210,
       0,     0,     0,   217,     0,     0,     0,     0,   262,     0,
       0,     0,     0,   268,     0,     0,     0,     0,   121,   158,
     180,     0,    12,     0,     0,    21,     0,    22,     0,     0,
       0,     0,     0,    25,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     177,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   228,
       0,     0,   311,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,   203,     0,   152,   151,
       0,     0,     0,     0,   156,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,     0,   179,     0,     0,
     292,   257,     0,     0,    29,     0,    27,    30,     0,     0,
      31,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   278,   276,   275,     0,   273,
       0,     0,     0,     0,     0,     0,   313,     0,     0,     0,
       0,     0,     0,   291,   260,     0,     0,     0,     0,   314,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   312,
       0,     0,     0,   294,   172,     0,     0,     0,     0,     0,
     220,   223,     0,     0,     0,     0,   206,   209,     0,     0,
       0,     0,   213,   216,     0,   266,   264,   263,     0,   261,
     272,   270,   269,     0,   267,    77,     0,     0,     0,     0,
       0,    20,     0,   285,     0,     0,   155,     0,     0,     0,
       0,     0,     0,     0,   253,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   230,     0,     0,     0,   258,
     241,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   332,     0,     0,     0,     0,     0,     0,   245,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,     0,     0,     0,
       0,     0,     0,    83,     0,     0,     0,     0,   153,     0,
       0,     0,   204,   304,     0,   308,     0,     0,     0,     0,
     310,   306,   295,     0,     0,     0,   237,    28,    32,     0,
       0,     0,     0,     0,    61,    33,     0,     0,     0,     0,
       0,    36,     0,    37,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   277,   242,
       0,     0,   232,   246,   299,     0,     0,     0,   286,     0,
       0,     0,     0,   238,     0,     0,     0,   287,     0,     0,
     252,   300,   250,     0,     0,     0,   280,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   171,     0,   170,   222,
     225,   221,     0,   208,   211,   207,     0,   215,   218,   214,
     265,   271,   182,     0,     0,    16,     0,     0,     0,    15,
     255,     0,     0,     0,     0,   335,     0,   186,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   321,     0,
       0,     0,   323,     0,   325,   248,     0,     0,   342,     0,
     243,   178,     0,     0,     0,   340,   229,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   341,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   162,     0,     0,     0,     0,     0,     0,     0,   290,
       0,     0,   344,     0,     0,     0,     0,   298,   297,     0,
       0,     0,     0,     0,     0,     0,    98,    89,    93,    84,
       0,     0,     0,    79,     0,     0,     0,   100,     0,     0,
      74,     0,    73,     0,   305,   309,   307,     0,   315,   317,
     318,   316,   183,     0,   343,   319,    65,    63,    67,     0,
       0,    54,     0,     0,    34,     0,    35,     0,    57,    58,
      38,     0,     0,     0,     0,    62,     0,     0,     0,     0,
       0,     0,    40,     0,    47,     0,     0,     0,     0,   244,
     227,     0,   249,     0,   339,   254,     0,     0,     0,     0,
     205,     0,     0,     0,   251,     0,   284,   282,   281,     0,
     279,     0,   302,     0,     0,     0,     0,   226,   212,   219,
     184,    14,     0,    17,     0,   233,     0,     0,   256,     0,
     334,     0,     0,   333,   156,   190,     0,     0,   188,   189,
     187,   288,   326,     0,   322,   328,     0,   324,   330,   293,
     361,   351,   354,     0,   352,   347,   386,   382,   398,   399,
     401,   384,   383,   387,   385,   400,     0,   392,   388,   394,
     395,   397,   390,   389,   393,   391,   396,   360,   356,   378,
     374,   380,   381,   376,   375,   379,   377,     0,   166,     0,
     164,   163,     0,     0,   353,     0,   348,     0,     0,   359,
       0,     0,   357,   345,     0,   355,     0,   346,     0,   349,
       0,     0,     0,     0,     0,     0,    81,    80,    82,     0,
     107,   114,     0,     0,    88,    86,    76,    75,   320,   185,
      44,    49,    52,    41,    55,    56,    66,    64,    60,    68,
       0,     0,     0,     0,    39,     0,    50,    43,    53,    45,
       0,    69,   289,     0,     0,   350,     0,     0,     0,   239,
       0,     0,   234,   283,     0,   303,   301,     0,   358,     0,
       5,     0,   231,     0,   235,     0,     0,   336,   193,   192,
     191,   327,   329,   371,   364,   196,   168,   167,     0,   165,
     368,     0,   363,     0,   365,   369,   367,   370,   366,    92,
      90,    96,    94,    85,     0,     0,     0,     0,     0,    99,
     104,     0,     0,    46,    48,    51,    42,    59,    71,    70,
     199,     0,     0,   362,   202,   240,   296,   197,     0,     0,
       0,   236,     0,   337,   169,     0,   372,     0,     0,     0,
     106,   111,     0,   113,   118,     0,     0,   105,    87,   200,
     201,   198,     0,     6,   338,   373,    91,    95,     0,    97,
       0,   112,     0,   119,   101,     0,     0,     0,   120,   108,
       0,     0,   115,     0,     0,   102,   103,     0,   109,   110,
     116,   117,     0,     0,     0,     0,     3,     0,     0,     4
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short yydefgoto[] =
{
      -1,    11,    12,    13,   115,   181,    88,    89,    90,   563,
     784,   564,    91,    92,    93,    94,   429,    95
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -966
static const short yypact[] =
{
     340,  -966,    33,    52,    -2,    96,   178,   599,   609,   228,
     203,   253,   111,   340,   610,   184,   607,   620,   316,   421,
     582,  -966,   601,  -966,   426,  -966,  -966,  -966,  -966,  -966,
    -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,
    -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,
    -966,  -966,   618,  -966,   543,     9,   284,   606,   154,  -966,
      10,     0,   354,   547,   249,   243,   249,    29,    53,   562,
      57,   272,  -966,   551,    42,    95,   459,   569,     2,   572,
     522,   641,   101,   352,   437,   229,   255,  -966,  -966,   465,
    -966,   111,   249,   216,  -966,  -966,  -966,   614,  -966,   635,
     650,   645,  -966,   663,  -966,   638,   667,  -966,  -966,   668,
    -966,  -966,   660,   643,   674,   669,   670,   639,  -966,   675,
     337,    11,   665,   647,    75,   672,   644,   679,    87,   188,
     682,   683,   282,   680,   300,   191,   359,   681,   684,   624,
     487,   685,    59,   686,    71,   350,   648,   654,   640,   371,
     557,   687,   342,  -966,    54,   249,   689,   695,   696,   249,
     658,   249,   698,   447,   699,   700,   701,   625,  -966,  -966,
     697,   671,   688,  -966,   702,    45,   694,   703,   576,   706,
     707,   507,   705,  -966,   708,    55,   709,   274,   712,   710,
     711,  -966,   489,   690,   676,   704,   718,   713,   719,  -966,
     714,   217,   716,   720,   556,   715,   717,   358,   728,   136,
     721,   590,   729,   725,   724,  -966,   302,   727,   293,  -966,
     349,   730,   311,  -966,   425,   731,   372,   732,  -966,   733,
     464,   734,   735,  -966,   736,   467,   737,   738,  -966,  -966,
    -966,   746,  -966,   745,   726,  -966,   744,  -966,   748,   747,
     751,   100,   739,  -966,   752,   749,   750,   430,   753,   740,
      38,    46,   428,   741,   754,   722,   757,   742,   758,   471,
    -966,   759,   743,   176,   766,   756,   755,   760,   761,   762,
     763,   764,   765,   767,   768,   769,   770,   771,   772,   773,
     774,   775,   776,   777,   778,   779,   780,   781,   782,   783,
     784,   785,   786,   787,   788,   789,   790,   791,   792,  -966,
     793,   796,  -966,   818,   794,   795,   797,   799,   472,   819,
     798,   830,   826,   227,   801,   836,   837,   231,   800,   804,
     236,   802,   417,   825,   841,   829,  -966,   844,  -966,  -966,
     845,   846,   847,   842,  -966,   626,   827,   848,   851,   852,
     853,   854,   849,   850,   855,  -966,   858,  -966,   859,   808,
    -966,  -966,   856,   860,  -966,   857,  -966,  -966,   162,   863,
    -966,   621,   861,   415,   655,   862,   864,   485,   498,   600,
     570,   265,   574,   636,   866,  -966,  -966,  -966,   865,  -966,
     289,   867,   868,   870,   871,   872,  -966,   869,   875,   874,
     878,   833,   879,  -966,  -966,   880,   824,   881,   877,  -966,
     882,   884,   831,   885,   886,   288,   883,   269,   887,  -966,
      39,   890,   839,  -966,  -966,   888,   891,   897,   892,   893,
    -966,  -966,   894,   898,   895,   896,  -966,  -966,   899,   904,
     900,   901,  -966,  -966,   902,  -966,  -966,  -966,   903,  -966,
    -966,  -966,  -966,   905,  -966,  -966,    72,   623,   911,   914,
     909,  -966,   910,  -966,   912,   889,  -966,   913,    66,   915,
     390,   918,   920,   921,  -966,   906,   923,   438,   917,   925,
     453,   919,   928,   922,   473,  -966,   930,   924,   932,  -966,
    -966,   926,   927,   935,   907,   936,   931,   933,   937,   939,
     942,   943,   945,   946,   947,   948,   949,   950,   951,   929,
     952,   953,   954,   955,   956,   957,   958,   959,   960,   961,
     962,   964,   963,   965,   966,   967,   968,   969,   971,   972,
     973,  -966,   940,   486,   975,   944,   976,   974,  -966,   970,
     977,   979,   978,   980,   981,   982,   984,   983,   985,   986,
     987,   988,   989,   990,   991,   992,  -966,   398,   921,   921,
     921,   433,   330,   301,   993,   417,   994,   417,  -966,   995,
     996,   997,  -966,  -966,   941,  -966,   998,   999,  1000,  1002,
    -966,  -966,  -966,   134,  1003,  1005,  -966,  -966,  -966,  1006,
    1007,  1009,  1001,  1004,  -966,  -966,  1010,  1008,  1011,   518,
     531,  -966,  1012,  -966,  1014,   530,  1016,  1018,   659,   662,
    1020,  1013,  1021,  1015,  1022,  1017,  1019,   628,  -966,  -966,
    1025,   478,  -966,  -966,  -966,  1026,  1023,  1028,  -966,  1029,
    1031,  1038,  1033,  -966,  1024,  1037,  1027,  -966,  1041,  1043,
    -966,  -966,  -966,  1042,   834,  1044,  -966,  1045,   480,  1046,
    1030,  1047,  1048,  1050,  1051,  1055,  -966,  1049,  -966,  -966,
    -966,  -966,  1052,  -966,  -966,  -966,  1056,  -966,  -966,  -966,
    -966,  -966,  -966,  1058,  1059,  -966,  1057,  1060,  1061,  -966,
    -966,   361,  1062,   534,   406,  -966,  1063,  -966,   249,  1068,
     488,  1064,  1066,  1067,  1069,  1070,  1075,  1071,  -966,  1072,
    1080,  1074,  -966,  1076,  -966,  -966,  1077,  1078,  -966,  1079,
    -966,  -966,  1081,  1082,  1083,  -966,  -966,  1084,  1085,  1086,
    1087,  1088,  1089,  1090,  1091,  1092,  1093,  1094,  1102,  1096,
    1097,  1098,  1099,  1100,  1101,  1103,  1104,  1105,  1106,  1107,
    1108,  -966,  1109,  1110,  1111,  1112,  1113,  1114,  1115,  1116,
    1124,  -966,   408,  1118,   567,  1119,  1127,  1121,  1073,  -966,
    1129,  1123,  -966,  1053,  1131,  1125,  1126,  -966,  -966,  1134,
    1128,  1136,  1130,  1138,  1132,  1140,  -966,   561,   563,  1122,
     398,   249,   921,  -966,  1133,   921,   921,   613,  1141,  1143,
    -966,  1137,  -966,  1139,  -966,  -966,  -966,  1142,  -966,  -966,
    -966,  -966,  -966,  1144,  -966,  -966,  -966,  -966,  -966,  1145,
    1147,  -966,  1148,  1149,  -966,  1150,  -966,  1151,  -966,  -966,
    -966,  1152,  1153,  1154,  1155,  -966,  1146,  1156,  1157,  1158,
     532,  1159,  -966,  1160,  -966,  1161,  1162,  1163,  1164,  -966,
    -966,  1166,  -966,  1165,  -966,  -966,  1167,  1168,  1169,  1170,
    -966,  1174,   298,  1135,  -966,  1171,  -966,  -966,  -966,  1172,
    -966,  1176,  -966,  1173,  1178,  1175,  1179,  -966,  -966,  -966,
    -966,  -966,  1180,  -966,   627,  -966,   481,  1181,  -966,  1182,
    -966,  1177,  1184,  -966,  1185,  -966,  1186,  1187,  -966,  -966,
    -966,  -966,  -966,  1188,  -966,  -966,  1189,  -966,  -966,  -966,
    -966,  -966,  -966,  1190,  -966,  -966,  -966,  -966,  -966,  -966,
    -966,  -966,  -966,  -966,  -966,  -966,  1191,  -966,  -966,  -966,
    -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,
    -966,  -966,  -966,  -966,  -966,  -966,  -966,  1192,  -966,  1193,
    -966,  -966,   568,  1195,  -966,  1197,  -966,  1183,  1198,  -966,
    1206,  1200,  -966,  -966,  1201,  -966,  1203,  -966,  1204,  -966,
    1205,  1213,  1214,  1215,  1216,  1218,  -966,  -966,  -966,   433,
     617,   619,  1219,   420,  -966,  1202,  -966,  -966,  -966,  -966,
    -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,
    1217,  1220,  1221,  1222,  -966,  1223,  -966,  -966,  -966,  -966,
     652,  -966,  -966,  1224,  1225,  -966,  1226,  1227,  1228,  -966,
    1229,  1230,  -966,  -966,  1231,  -966,  -966,  1232,  -966,   111,
    -966,  1236,  -966,  1233,  -966,  1241,  1235,  -966,  -966,  -966,
    -966,  -966,  -966,  -966,  -966,  -966,  -966,  -966,  1237,  -966,
    -966,  1243,  -966,  1238,  -966,  -966,  -966,  -966,  -966,  -966,
    1234,  -966,  1239,  -966,  1240,  1246,   542,  1247,   550,  1212,
    -966,  1249,  1250,  -966,  -966,  -966,  -966,  -966,  -966,  -966,
    -966,  1244,  1245,  -966,  -966,  -966,  -966,  -966,  1248,   158,
    1251,  -966,  1254,  -966,  -966,  1255,  -966,  1253,   555,   433,
    1242,  -966,  1256,  1252,  -966,  1257,   395,  -966,  -966,  -966,
    -966,  -966,    22,  -966,  -966,  -966,  -966,  -966,  1263,  -966,
     446,  -966,   466,  -966,  -966,  1264,  1265,   244,  -966,  -966,
    1266,  1267,  -966,  1268,  1269,  -966,  -966,  1210,  -966,  -966,
    -966,  -966,  1270,  1271,  1259,   632,  -966,  1272,  1273,  -966
};

/* YYPGOTO[NTERM-NUM].  */
static const short yypgoto[] =
{
    -966,  -966,  1211,  -966,   -55,   560,  -966,  -966,  -966,  -470,
    -965,   104,  -280,   588,   -63,   723,  -214,   -90
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const unsigned short yytable[] =
{
     118,   238,   167,   693,  1054,   137,   435,   205,   152,   153,
     441,   169,   111,   128,   116,   129,   130,    17,   191,   131,
     260,   206,   132,   133,   134,   128,   135,  1117,   130,   114,
     261,   131,   170,   138,   132,   133,   134,   239,   135,    15,
     171,   424,   651,   262,   207,   192,   478,   481,   483,   424,
     193,   194,   136,   117,   195,   361,   196,   111,   425,   652,
      16,   197,   182,   173,   136,   387,   425,   183,   340,   683,
     476,   172,   427,   653,   114,   684,   685,   320,   479,   477,
     427,   265,   672,   362,   198,   317,   341,   480,   777,   778,
     779,   174,   787,   388,   175,   184,   342,   270,   185,   186,
     118,    18,   271,   318,   152,   199,   191,   686,   321,   673,
     463,   215,   266,   322,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,  1109,    62,    63,    64,    65,    66,
      67,    68,    69,   200,    70,   216,   201,    71,    72,   217,
      73,   464,   218,    74,   802,    75,    76,   415,    77,    78,
     416,    10,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    52,    53,    54,    55,    56,    57,    58,    59,  1102,
      61,   803,    62,    63,    64,    65,    66,    67,    68,    69,
     126,    70,   494,   589,    71,    72,    19,    73,   590,   127,
      74,    99,    75,    76,   272,    77,    78,   298,    10,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    25,    26,
      27,    28,    29,   495,    30,    31,    32,    33,    34,    35,
     273,    36,    37,   299,    38,   274,   240,   404,   300,    39,
     241,    24,    40,   544,   227,    41,    42,   549,    43,   228,
      44,    45,   553,    46,    47,    48,    49,    50,   154,   155,
     272,   156,   111,    51,   157,   405,  1127,   158,   159,    64,
     232,    66,   160,   697,   545,   233,   701,   229,   550,   114,
     230,   231,   161,   554,   645,   162,   273,   163,   187,   646,
     164,   274,   611,    10,   188,   165,   166,   390,   277,   391,
     119,   278,   120,   234,   612,   279,   235,   236,   642,   619,
     280,   281,   121,   431,   189,   424,   288,   647,  1009,   289,
     648,   649,   968,   290,   282,   970,   971,   283,   291,   292,
     284,   437,   425,   285,   788,   286,   122,   620,   105,   643,
     621,   432,   293,   556,   426,   294,   427,   789,   295,  1010,
     428,   296,   258,   297,     1,     2,   259,     3,     4,   438,
     785,     5,   424,   337,     6,     7,     8,   139,     9,   140,
     141,   338,   219,   142,   786,   301,   323,   143,   302,   425,
     144,   875,   303,   145,   146,   339,   330,   304,   331,   876,
      10,   433,   443,   427,   412,   324,   220,   434,   147,   148,
     221,   305,   149,   222,   306,   688,   155,   307,  1114,   877,
     308,   689,   413,   688,   155,   159,    64,   690,    66,   157,
     444,   688,   155,   159,    64,  1115,    66,   881,   938,   161,
     556,   159,    64,  1060,    66,   595,   557,   161,   424,  1116,
      10,   424,   106,   469,   939,   161,   111,   558,    10,   470,
    1061,   424,   780,   109,   596,   425,    10,   223,   425,  1119,
     471,   559,   560,   114,   561,   562,   424,   439,   425,   427,
     482,   348,   427,   440,   472,   473,  1120,   781,   782,  1122,
     696,   224,   427,   425,   447,   225,   237,   452,   226,   349,
    1121,   490,   538,   705,   350,   700,  1123,   427,   840,   351,
     858,  1022,   202,   314,   776,   601,   751,   395,   885,   396,
    1124,   203,   448,   315,   752,   453,   783,   886,   603,   491,
     539,   706,   753,   368,   602,   369,   841,   370,   859,  1023,
     371,   887,   372,   754,   373,   211,   374,   604,   814,   375,
     376,   377,   378,   379,   380,   381,   212,   382,   383,   384,
     820,   816,   994,   879,   880,  1091,   111,   815,   213,   112,
     150,   821,   151,  1094,   111,   113,   822,   190,  1107,   823,
     817,   995,  1092,   114,   408,   176,   409,   114,   177,   178,
    1095,   114,   204,   179,   332,  1108,   333,   941,  1037,   176,
     334,   180,   177,   608,   961,   942,   963,   179,    14,   609,
     335,   613,   107,   943,  1038,   180,   610,   962,   418,   964,
     419,    14,    20,   614,   208,   209,   605,   420,   606,    21,
     100,   108,    22,    97,   210,   607,   101,   102,   123,    23,
      98,   882,   124,   103,   242,   125,   674,   592,   110,   593,
     104,   243,   311,   675,   312,   355,   573,  1020,   313,   356,
     313,  1021,  1136,   244,   214,   327,  1137,   328,   329,   972,
     246,   248,   973,  1055,   837,  1057,  1056,   838,  1058,  1068,
     245,   597,  1069,   598,   615,   826,   616,   827,   828,   791,
     829,   793,   249,   247,   250,   251,   252,   253,   263,   255,
     254,   256,   264,   257,   267,   268,   269,   275,   276,   287,
     325,   309,   310,   316,   343,   319,   326,   336,   344,   345,
     346,   347,   352,   353,   354,   363,   364,   357,   358,   366,
     367,   398,   360,   393,   359,   385,   394,   966,   386,   389,
     392,   400,   402,   406,   403,   401,   967,   397,   407,   411,
     399,   414,   421,   422,   423,   458,   417,   430,   365,  1079,
     436,   442,   445,   446,   449,   450,   451,   454,   455,   456,
     410,   457,   459,   460,   462,   466,   467,   461,   468,   486,
     487,     0,   492,   474,   485,   465,   475,   484,   489,   496,
     497,     0,     0,     0,     0,     0,     0,     0,     0,   488,
     493,     0,   509,     0,     0,     0,     0,   522,   168,     0,
       0,     0,   498,     0,     0,     0,     0,   499,   500,   501,
     502,   503,   504,   531,   505,   506,   507,   508,   532,   510,
     511,   512,   513,   514,   515,   516,   517,   518,   519,   520,
     521,   533,   523,   524,   525,   526,   527,   528,   529,   530,
     540,   534,   537,   542,   536,   541,   535,   543,   546,   547,
     548,   552,   565,   551,   566,   555,   567,   568,   569,   570,
     571,   585,   572,   574,   576,   577,   578,   579,   575,   580,
     581,   583,   584,   587,   634,   582,   586,   588,   591,   617,
     631,   594,   627,   639,   855,   618,   655,   622,   623,   599,
     624,   600,   629,   625,   626,   628,   630,   632,   635,   636,
     633,   656,   637,   638,   657,   640,   641,   644,   654,   650,
     658,   662,   659,   660,   661,   663,   664,   666,     0,   665,
     667,   668,   669,   670,   783,   671,   676,   677,   678,   682,
     679,   691,   680,   692,   556,   687,   695,   698,   699,   702,
     681,   703,   704,   707,   708,   709,   710,   711,   712,   714,
     717,   715,   718,   716,   713,   719,   720,   694,   721,   722,
     723,   724,   725,   726,   727,   729,   730,   731,   732,   733,
     734,   735,   736,   737,   738,   739,   728,   740,   742,   743,
     744,   745,   746,   741,   747,   748,   749,   750,   755,   757,
     759,   756,   761,   758,   797,   765,   763,   766,   762,   238,
     770,     0,   772,   767,   774,   768,     0,     0,     0,     0,
       0,     0,     0,   790,     0,   794,   795,   796,   798,   799,
     800,   792,   801,   804,   760,   805,   806,   807,   764,   808,
     811,   824,   818,   769,   819,   771,   830,   773,   825,   775,
     809,   832,   834,   810,   783,   839,   842,   812,   844,   845,
     813,   847,   831,   846,   833,   848,   835,   850,   836,   852,
     853,   863,   854,   864,   856,   857,   860,   862,   866,   867,
     843,   849,   868,   865,   851,   872,   869,   861,   870,   871,
     873,   884,   878,   883,   888,   874,   889,   890,   893,   891,
     892,   894,   895,   896,   897,   903,   898,   899,   900,   901,
     950,   902,   947,   904,   905,   906,   907,   908,   909,   910,
     911,   912,   913,   914,   915,   916,   917,   918,   919,   920,
     921,   922,     0,   923,   924,   925,   926,   927,   928,   929,
     930,   931,   932,   933,   934,   935,   936,   937,   940,   944,
     945,   946,   948,   949,   951,   952,   953,   954,   955,   956,
     957,   958,   959,   960,   974,   965,   975,   976,     0,   977,
       0,     0,   978,  1011,   979,   980,   969,   981,   982,   983,
     984,   985,   986,   987,   988,   989,     0,     0,  1003,   996,
     997,   998,   999,  1007,  1001,   990,  1002,  1008,  1005,  1014,
    1026,  1012,  1013,  1015,     0,   991,   992,   993,  1016,  1018,
    1019,  1024,  1000,  1025,  1027,  1028,  1029,  1030,  1031,  1032,
    1033,  1034,  1035,  1036,  1004,  1039,  1006,  1040,  1042,  1043,
    1044,  1045,  1017,  1046,  1047,  1048,  1049,  1050,  1051,  1052,
    1041,  1053,  1059,  1132,    96,  1062,     0,  1063,  1071,  1072,
    1064,  1065,  1066,  1067,  1070,  1078,     0,  1073,  1074,  1075,
    1076,  1077,  1080,  1081,  1082,  1083,  1085,  1084,  1086,  1090,
    1093,  1096,  1097,  1098,  1099,  1100,  1106,  1087,  1101,  1111,
    1113,  1103,  1088,  1089,  1104,  1105,  1118,  1125,  1126,  1128,
    1129,  1130,  1131,  1135,     0,  1133,     0,     0,  1138,     0,
    1134,  1110,     0,  1139,     0,     0,     0,     0,     0,     0,
       0,  1112
};

static const short yycheck[] =
{
      55,    91,    65,   473,   969,     5,   220,     5,    63,    64,
     224,    66,     3,     3,     5,     5,     6,    19,    73,     9,
       9,    19,    12,    13,    14,     3,    16,     5,     6,    20,
      19,     9,     3,    33,    12,    13,    14,    92,    16,     6,
      11,     3,     3,    32,    42,     3,   260,   261,   262,     3,
       8,     9,    42,    44,    12,    10,    14,     3,    20,    20,
       8,    19,     5,    10,    42,    10,    20,    10,    14,     3,
      32,    42,    34,    34,    20,     9,    10,     6,    32,    41,
      34,     6,    10,    38,    42,    26,    32,    41,   558,   559,
     560,    38,   562,    38,    41,    38,    42,    10,    41,    42,
     155,     5,    15,    44,   159,    10,   161,    41,    37,    37,
      10,    10,    37,    42,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,  1089,    14,    15,    16,    17,    18,
      19,    20,    21,    38,    23,    34,    41,    26,    27,    38,
      29,    41,    41,    32,    10,    34,    35,    11,    37,    38,
      14,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    37,    14,    15,    16,    17,    18,    19,    20,    21,
      26,    23,     6,    21,    26,    27,     8,    29,    26,    35,
      32,     7,    34,    35,     6,    37,    38,     6,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,     5,     6,
       7,     8,     9,    37,    11,    12,    13,    14,    15,    16,
      32,    18,    19,    32,    21,    37,    10,    10,    37,    26,
      14,     3,    29,     6,     5,    32,    33,     6,    35,    10,
      37,    38,     6,    40,    41,    42,    43,    44,     5,     6,
       6,     8,     3,     0,    11,    38,    12,    14,    15,    16,
       5,    18,    19,   477,    37,    10,   480,    38,    37,    20,
      41,    42,    29,    37,     5,    32,    32,    34,     6,    10,
      37,    37,    17,    40,    12,    42,    43,    13,     6,    15,
       6,     9,     8,    38,    29,    13,    41,    42,    10,    10,
      18,    19,    18,    10,    32,     3,     6,    38,    10,     9,
      41,    42,   782,    13,    32,   785,   786,    35,    18,    19,
      38,    10,    20,    41,    23,    43,    42,    38,    12,    41,
      41,    38,    32,     3,    32,    35,    34,    36,    38,    41,
      38,    41,     5,    43,     4,     5,     9,     7,     8,    38,
      20,    11,     3,    11,    14,    15,    16,     3,    18,     5,
       6,    19,    10,     9,    34,     6,    16,    13,     9,    20,
      16,    10,    13,    19,    20,    33,     5,    18,     7,    18,
      40,    32,    10,    34,    26,    35,    34,    38,    34,    35,
      38,    32,    38,    41,    35,     5,     6,    38,     3,    38,
      41,    11,    44,     5,     6,    15,    16,   470,    18,    11,
      38,     5,     6,    15,    16,    20,    18,    11,    10,    29,
       3,    15,    16,     3,    18,    10,     9,    29,     3,    34,
      40,     3,    11,     3,    26,    29,     3,    20,    40,     9,
      20,     3,     9,    17,    29,    20,    40,    10,    20,     3,
      20,    34,    35,    20,    37,    38,     3,    32,    20,    34,
      32,    14,    34,    38,    34,    35,    20,    34,    35,     3,
      32,    34,    34,    20,    10,    38,    11,    10,    41,    32,
      34,    10,    10,    10,    37,    32,    20,    34,    10,    42,
      10,    10,    33,     6,   557,    10,    10,     8,    10,    10,
      34,    42,    38,    16,    18,    38,   561,    19,    10,    38,
      38,    38,    26,     6,    29,     8,    38,    10,    38,    38,
      13,    33,    15,    37,    17,     3,    19,    29,    10,    22,
      23,    24,    25,    26,    27,    28,    14,    30,    31,    32,
      10,    10,    10,     9,    10,     3,     3,    29,    26,     6,
       3,    21,     5,     3,     3,    12,    26,     6,     3,    29,
      29,    29,    20,    20,     8,     3,    10,    20,     6,     7,
      20,    20,     3,    11,    17,    20,    19,    10,    10,     3,
      23,    19,     6,    13,    23,    18,    23,    11,     0,    19,
      33,    17,    10,    26,    26,    19,    26,    36,     8,    36,
      10,    13,     3,    29,    32,    33,     6,    17,     8,    10,
       3,    10,     3,     3,    42,    15,     9,    10,    12,    10,
      10,   684,    16,     3,    10,    19,     3,     6,    10,     8,
      10,    17,     8,    10,    10,    10,    10,    10,    14,    14,
      14,    14,    10,     8,     3,     5,    14,     7,     8,    36,
       5,    13,    39,    36,    26,    36,    39,    29,    39,     7,
      10,     6,    10,     8,    28,     6,    30,     8,     6,   565,
       8,   567,     5,    10,     6,    15,    33,     3,    13,     9,
      11,    42,    35,     8,    12,    41,     7,     5,     5,     9,
      42,    10,     8,     8,     5,     9,    42,    10,     3,     3,
      42,     3,     3,     3,     3,    11,     3,    10,    37,     3,
       3,    35,    10,     3,    26,    10,     5,   780,    10,    10,
       8,     3,     3,     7,    10,    12,   781,    37,     8,    12,
      26,     3,     3,     8,    10,     9,    15,    10,   178,  1019,
      10,    10,    10,    10,    10,    10,    10,    10,    10,     3,
      35,     6,     8,     5,     3,     3,     7,    10,     8,    37,
       3,    -1,     3,    10,    10,    26,    26,    26,    10,     3,
      14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      37,    -1,    11,    -1,    -1,    -1,    -1,     3,    65,    -1,
      -1,    -1,    37,    -1,    -1,    -1,    -1,    37,    37,    37,
      37,    37,    37,    10,    37,    37,    37,    37,    12,    37,
      37,    37,    37,    37,    37,    37,    37,    37,    37,    37,
      37,     3,    37,    37,    37,    37,    37,    37,    37,    37,
      11,    37,    33,     3,    37,    37,    41,    11,    37,     3,
       3,    37,    17,    43,     3,    43,    17,     3,     3,     3,
       3,    43,    10,    26,     3,     3,     3,     3,    10,    10,
      10,     3,     3,     3,    40,    10,    10,    10,     5,     3,
      37,    10,     3,    42,    40,    10,    37,    10,    10,    17,
      10,    17,     8,    12,    12,    10,     8,     8,     7,    12,
      10,     3,    10,     9,     3,    10,    10,    14,     8,    12,
       3,     3,    10,    10,    10,    10,    10,     3,    -1,    10,
      10,    10,    10,    10,   969,    10,     5,     3,     9,     6,
      10,     3,    10,     3,     3,    10,     3,    10,     3,    10,
      41,     3,    10,     3,    10,     3,    10,    10,     3,     3,
       3,    10,     3,    10,    37,     3,     3,    41,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,    37,     3,     3,     3,
       3,     3,     3,    10,     3,     3,     3,    37,     3,     3,
      10,    37,     3,     9,    43,     3,     6,     3,    10,  1079,
       3,    -1,     3,    10,     3,    10,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    10,    -1,    10,    10,    10,    10,    10,
      10,    17,    10,    10,    37,    10,    10,    10,    37,    10,
      10,     5,    10,    37,    10,    37,     6,    37,    10,    37,
      29,    10,    10,    29,  1089,    10,    10,    29,    10,    10,
      29,     3,    29,    12,    29,    12,    29,    10,    29,     8,
       7,     3,    10,     3,    10,    10,    10,    10,     3,    10,
      37,    37,    10,    12,    37,     8,    10,    37,    10,    10,
      10,     3,    10,    10,    10,    14,    10,    10,     3,    10,
      10,    10,    10,     3,    10,     3,    10,    10,    10,    10,
      37,    10,    19,    10,    10,    10,    10,    10,    10,    10,
      10,    10,    10,    10,    10,     3,    10,    10,    10,    10,
      10,    10,    -1,    10,    10,    10,    10,    10,    10,    10,
      10,    10,    10,    10,    10,    10,    10,     3,    10,    10,
       3,    10,     3,    10,     3,    10,    10,     3,    10,     3,
      10,     3,    10,     3,     3,    23,     3,    10,    -1,    10,
      -1,    -1,    10,    18,    10,    10,    23,    10,    10,    10,
      10,    10,    10,    10,    10,    10,    -1,    -1,     3,    10,
      10,    10,    10,     3,    10,    29,    10,     3,    10,     3,
       3,    10,    10,    10,    -1,    29,    29,    29,    10,    10,
      10,    10,    29,    11,    10,    10,    10,    10,    10,    10,
      10,    10,    10,    10,    37,    10,    37,    10,    10,     3,
      10,    10,    37,    10,    10,    10,     3,     3,     3,     3,
      37,     3,     3,    13,    13,    23,    -1,    10,     3,     3,
      10,    10,    10,    10,    10,     3,    -1,    10,    10,    10,
      10,    10,     6,    10,     3,    10,     3,    10,    10,     3,
       3,    39,     3,     3,    10,    10,     3,    23,    10,     3,
       3,    10,    23,    23,    10,    10,     3,     3,     3,     3,
       3,     3,     3,    14,    -1,     5,    -1,    -1,     6,    -1,
       9,    39,    -1,    10,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    39
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,     4,     5,     7,     8,    11,    14,    15,    16,    18,
      40,    51,    52,    53,    63,     6,     8,    19,     5,     8,
       3,    10,     3,    10,     3,     5,     6,     7,     8,     9,
      11,    12,    13,    14,    15,    16,    18,    19,    21,    26,
      29,    32,    33,    35,    37,    38,    40,    41,    42,    43,
      44,     0,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    14,    15,    16,    17,    18,    19,    20,    21,
      23,    26,    27,    29,    32,    34,    35,    37,    38,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    56,    57,
      58,    62,    63,    64,    65,    67,    52,     3,    10,     7,
       3,     9,    10,     3,    10,    12,    11,    10,    10,    17,
      10,     3,     6,    12,    20,    54,     5,    44,    54,     6,
       8,    18,    42,    12,    16,    19,    26,    35,     3,     5,
       6,     9,    12,    13,    14,    16,    42,     5,    33,     3,
       5,     6,     9,    13,    16,    19,    20,    34,    35,    38,
       3,     5,    54,    54,     5,     6,     8,    11,    14,    15,
      19,    29,    32,    34,    37,    42,    43,    64,    65,    54,
       3,    11,    42,    10,    38,    41,     3,     6,     7,    11,
      19,    55,     5,    10,    38,    41,    42,     6,    12,    32,
       6,    54,     3,     8,     9,    12,    14,    19,    42,    10,
      38,    41,    33,    42,     3,     5,    19,    42,    32,    33,
      42,     3,    14,    26,     3,    10,    34,    38,    41,    10,
      34,    38,    41,    10,    34,    38,    41,     5,    10,    38,
      41,    42,     5,    10,    38,    41,    42,    11,    67,    54,
      10,    14,    10,    17,     8,    10,     5,    10,    13,     5,
       6,    15,    33,     3,    11,     9,    42,     8,     5,     9,
       9,    19,    32,    13,    35,     6,    37,    12,    41,     7,
      10,    15,     6,    32,    37,     5,     5,     6,     9,    13,
      18,    19,    32,    35,    38,    41,    43,     9,     6,     9,
      13,    18,    19,    32,    35,    38,    41,    43,     6,    32,
      37,     6,     9,    13,    18,    32,    35,    38,    41,    10,
       8,     8,    10,    14,     6,    16,     8,    26,    44,     9,
       6,    37,    42,    16,    35,    42,    42,     5,     7,     8,
       5,     7,    17,    19,    23,    33,    10,    11,    19,    33,
      14,    32,    42,     5,     3,     3,    42,     3,    14,    32,
      37,    42,     3,     3,     3,    10,    14,    10,    37,    26,
      10,    10,    38,    11,     3,    55,     3,     3,     6,     8,
      10,    13,    15,    17,    19,    22,    23,    24,    25,    26,
      27,    28,    30,    31,    32,    10,    10,    10,    38,    10,
      13,    15,     8,     3,     5,     8,    10,    37,    35,    26,
       3,    12,     3,    10,    10,    38,     7,     8,     8,    10,
      35,    12,    26,    44,     3,    11,    14,    15,     8,    10,
      17,     3,     8,    10,     3,    20,    32,    34,    38,    66,
      10,    10,    38,    32,    38,    66,    10,    10,    38,    32,
      38,    66,    10,    10,    38,    10,    10,    10,    38,    10,
      10,    10,    10,    38,    10,    10,     3,     6,     9,     8,
       5,    10,     3,    10,    41,    26,     3,     7,     8,     3,
       9,    20,    34,    35,    10,    26,    32,    41,    66,    32,
      41,    66,    32,    66,    26,    10,    37,     3,    37,    10,
      10,    38,     3,    37,     6,    37,     3,    14,    37,    37,
      37,    37,    37,    37,    37,    37,    37,    37,    37,    11,
      37,    37,    37,    37,    37,    37,    37,    37,    37,    37,
      37,    37,     3,    37,    37,    37,    37,    37,    37,    37,
      37,    10,    12,     3,    37,    41,    37,    33,    10,    38,
      11,    37,     3,    11,     6,    37,    37,     3,     3,     6,
      37,    43,    37,     6,    37,    43,     3,     9,    20,    34,
      35,    37,    38,    59,    61,    17,     3,    17,     3,     3,
       3,     3,    10,    10,    26,    10,     3,     3,     3,     3,
      10,    10,    10,     3,     3,    43,    10,     3,    10,    21,
      26,     5,     6,     8,    10,    10,    29,     6,     8,    17,
      17,    10,    29,    10,    29,     6,     8,    15,    13,    19,
      26,    17,    29,    17,    29,    28,    30,     3,    10,    10,
      38,    41,    10,    10,    10,    12,    12,     3,    10,     8,
       8,    37,     8,    10,    40,     7,    12,    10,     9,    42,
      10,    10,    10,    41,    14,     5,    10,    38,    41,    42,
      12,     3,    20,    34,     8,    37,     3,     3,     3,    10,
      10,    10,     3,    10,    10,    10,     3,    10,    10,    10,
      10,    10,    10,    37,     3,    10,     5,     3,     9,    10,
      10,    41,     6,     3,     9,    10,    41,    10,     5,    11,
      64,     3,     3,    59,    41,     3,    32,    66,    10,     3,
      32,    66,    10,     3,    10,    10,    38,     3,    10,     3,
      10,    10,     3,    37,     3,    10,    10,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,    37,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,    10,     3,     3,     3,     3,     3,     3,     3,     3,
      37,    10,    18,    26,    37,     3,    37,     3,     9,    10,
      37,     3,    10,     6,    37,     3,     3,    10,    10,    37,
       3,    37,     3,    37,     3,    37,    64,    59,    59,    59,
       9,    34,    35,    54,    60,    20,    34,    59,    23,    36,
      10,    61,    17,    61,    10,    10,    10,    43,    10,    10,
      10,    10,    10,    37,    10,    10,    10,    10,    10,    29,
      29,    10,    29,    29,    10,    29,    10,    29,    10,    10,
      10,    21,    26,    29,     5,    10,     6,     8,     6,     8,
       6,    29,    10,    29,    10,    29,    29,    26,    29,    10,
      10,    38,    10,    37,    10,    10,    12,     3,    12,    37,
      10,    37,     8,     7,    10,    40,    10,    10,    10,    38,
      10,    37,    10,     3,     3,    12,     3,    10,    10,    10,
      10,    10,     8,    10,    14,    10,    18,    38,    10,     9,
      10,    11,    64,    10,     3,    10,    19,    33,    10,    10,
      10,    10,    10,     3,    10,    10,     3,    10,    10,    10,
      10,    10,    10,     3,    10,    10,    10,    10,    10,    10,
      10,    10,    10,    10,    10,    10,     3,    10,    10,    10,
      10,    10,    10,    10,    10,    10,    10,    10,    10,    10,
      10,    10,    10,    10,    10,    10,    10,     3,    10,    26,
      10,    10,    18,    26,    10,     3,    10,    19,     3,    10,
      37,     3,    10,    10,     3,    10,     3,    10,     3,    10,
       3,    23,    36,    23,    36,    23,    64,    54,    59,    23,
      59,    59,    36,    39,     3,     3,    10,    10,    10,    10,
      10,    10,    10,    10,    10,    10,    10,    10,    10,    10,
      29,    29,    29,    29,    10,    29,    10,    10,    10,    10,
      29,    10,    10,     3,    37,    10,    37,     3,     3,    10,
      41,    18,    10,    10,     3,    10,    10,    37,    10,    10,
      10,    14,    10,    38,    10,    11,     3,    10,    10,    10,
      10,    10,    10,    10,    10,    10,    10,    10,    26,    10,
      10,    37,    10,     3,    10,    10,    10,    10,    10,     3,
       3,     3,     3,     3,    60,    36,    39,    36,    39,     3,
       3,    20,    23,    10,    10,    10,    10,    10,     7,    10,
      10,     3,     3,    10,    10,    10,    10,    10,     3,    62,
       6,    10,     3,    10,    10,     3,    10,    23,    23,    23,
       3,     3,    20,     3,     3,    20,    39,     3,     3,    10,
      10,    10,    11,    10,    10,    10,     3,     3,    20,    60,
      39,     3,    39,     3,     3,    20,    34,     5,     3,     3,
      20,    34,     3,    20,    34,     3,     3,    12,     3,     3,
       3,     3,    13,     5,     9,    14,    10,    14,     6,    10
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
#line 128 "kal3.y"
    { printf("Reached end of file\n"); }
    break;

  case 3:
#line 131 "kal3.y"
    { endBblock(); }
    break;

  case 4:
#line 132 "kal3.y"
    { endBblock(); }
    break;

  case 5:
#line 133 "kal3.y"
    { endBblock(); }
    break;

  case 6:
#line 134 "kal3.y"
    { endBblock(); }
    break;

  case 8:
#line 138 "kal3.y"
    { reserveStore('V', yyvsp[-1]); }
    break;

  case 10:
#line 140 "kal3.y"
    { reserveStore('W', yyvsp[-1]); }
    break;

  case 12:
#line 142 "kal3.y"
    { reserveStore('Y' + yyvsp[-2] * 256, yyvsp[-1]); }
    break;

  case 14:
#line 145 "kal3.y"
    { fixY0(yyvsp[-5], yyvsp[-4], yyvsp[-1]); }
    break;

  case 15:
#line 147 "kal3.y"
    { fixY0(-3, yyvsp[-4], yyvsp[-1]); }
    break;

  case 16:
#line 149 "kal3.y"
    { fprintf(stderr,"Y0=E; not understood\n"); }
    break;

  case 17:
#line 152 "kal3.y"
    { progstart = yyvsp[-1] + ptform; }
    break;

  case 21:
#line 156 "kal3.y"
    { Bblock('S', yyvsp[-1]); }
    break;

  case 22:
#line 157 "kal3.y"
    { Bblock('L', yyvsp[-1]); }
    break;

  case 25:
#line 162 "kal3.y"
    { yyval = -yyvsp[0]; }
    break;

  case 26:
#line 165 "kal3.y"
    { yyval = codeloc(-1, yyvsp[0]); }
    break;

  case 27:
#line 166 "kal3.y"
    { yyval = codeloc(yyvsp[0], 0); }
    break;

  case 28:
#line 167 "kal3.y"
    { yyval = codeloc(yyvsp[0], yyvsp[-2]); }
    break;

  case 29:
#line 168 "kal3.y"
    { yyval = yyvsp[0] * 6; }
    break;

  case 30:
#line 169 "kal3.y"
    { yyval = codeloc(2999, 0); printf("L%d not available\n", yyvsp[0]); }
    break;

  case 31:
#line 172 "kal3.y"
    { storejump(0200, 0260, yyvsp[-1]); }
    break;

  case 32:
#line 173 "kal3.y"
    { storejump(0200, 0320, yyvsp[-1]); }
    break;

  case 33:
#line 174 "kal3.y"
    { storejump(0220, 0020, yyvsp[-2]); }
    break;

  case 34:
#line 175 "kal3.y"
    { storejump(0200, 0020, yyvsp[-3]); }
    break;

  case 35:
#line 176 "kal3.y"
    { storejump(0200, 0020, yyvsp[-3]); }
    break;

  case 36:
#line 177 "kal3.y"
    { storejump(0200, 0020, yyvsp[-2]); }
    break;

  case 37:
#line 178 "kal3.y"
    { storejump(0200, 0020, yyvsp[-2]); }
    break;

  case 38:
#line 179 "kal3.y"
    { storejump(0200, 0020, yyvsp[-3]); }
    break;

  case 39:
#line 180 "kal3.y"
    { storejump(0200, 0020, yyvsp[-4]); }
    break;

  case 40:
#line 181 "kal3.y"
    { storejump(0220, 0040, yyvsp[-3]); }
    break;

  case 41:
#line 182 "kal3.y"
    { storejump(0220, 0040, yyvsp[-4]); }
    break;

  case 42:
#line 183 "kal3.y"
    { storejump(0220, 0040, yyvsp[-5]); }
    break;

  case 43:
#line 184 "kal3.y"
    { storejump(0200, 0040, yyvsp[-4]); }
    break;

  case 44:
#line 185 "kal3.y"
    { storejump(0200, 0040, yyvsp[-4]); }
    break;

  case 45:
#line 186 "kal3.y"
    { storejump(0200, 0040, yyvsp[-4]); }
    break;

  case 46:
#line 187 "kal3.y"
    { storejump(0200, 0040, yyvsp[-5]); }
    break;

  case 47:
#line 188 "kal3.y"
    { storejump(0220, 0100, yyvsp[-3]); }
    break;

  case 48:
#line 189 "kal3.y"
    { storejump(0220, 0100, yyvsp[-5]); }
    break;

  case 49:
#line 190 "kal3.y"
    { storejump(0220, 0100, yyvsp[-4]); }
    break;

  case 50:
#line 191 "kal3.y"
    { storejump(0200, 0100, yyvsp[-4]); }
    break;

  case 51:
#line 192 "kal3.y"
    { storejump(0200, 0100, yyvsp[-5]); }
    break;

  case 52:
#line 193 "kal3.y"
    { storejump(0200, 0100, yyvsp[-4]); }
    break;

  case 53:
#line 194 "kal3.y"
    { storejump(0200, 0100, yyvsp[-4]); }
    break;

  case 54:
#line 195 "kal3.y"
    { storejump(0220, 0140, yyvsp[-3]); }
    break;

  case 55:
#line 196 "kal3.y"
    { storejump(0200, 0140, yyvsp[-4]); }
    break;

  case 56:
#line 197 "kal3.y"
    { storejump(0200, 0140, yyvsp[-4]); }
    break;

  case 57:
#line 198 "kal3.y"
    { storejump(0200, 0140, yyvsp[-3]); }
    break;

  case 58:
#line 199 "kal3.y"
    { storejump(0200, 0140, yyvsp[-3]); }
    break;

  case 59:
#line 200 "kal3.y"
    { storejump(0200, 0140, yyvsp[-5]); }
    break;

  case 60:
#line 201 "kal3.y"
    { storejump(0200, 0140, yyvsp[-4]); }
    break;

  case 61:
#line 202 "kal3.y"
    { storejump(0220, 0200, yyvsp[-2]); }
    break;

  case 62:
#line 203 "kal3.y"
    { storejump(0200, 0200, yyvsp[-3]); }
    break;

  case 63:
#line 204 "kal3.y"
    { storejump(0220, 0240, yyvsp[-3]); }
    break;

  case 64:
#line 205 "kal3.y"
    { storejump(0200, 0240, yyvsp[-4]); }
    break;

  case 65:
#line 206 "kal3.y"
    { storejump(0220, 0300, yyvsp[-3]); }
    break;

  case 66:
#line 207 "kal3.y"
    { storejump(0200, 0300, yyvsp[-4]); }
    break;

  case 67:
#line 208 "kal3.y"
    { storejump(0220, 0340, yyvsp[-3]); }
    break;

  case 68:
#line 209 "kal3.y"
    { storejump(0200, 0340, yyvsp[-4]); }
    break;

  case 69:
#line 210 "kal3.y"
    { qnum(yyvsp[-2]); storejump(0240, yyvsp[-2]<<4, yyvsp[-4]); }
    break;

  case 70:
#line 211 "kal3.y"
    { qnum(yyvsp[-3]); storejump(0260, yyvsp[-3]<<4, yyvsp[-5]); }
    break;

  case 71:
#line 212 "kal3.y"
    { store2syl(twosylqx(0177, yyvsp[-3], 0)); }
    break;

  case 72:
#line 215 "kal3.y"
    { setprinterconst(yyvsp[-1], yyvsp[-1]); }
    break;

  case 73:
#line 216 "kal3.y"
    { setprinterconst(yyvsp[-3], yyvsp[-1]); }
    break;

  case 74:
#line 219 "kal3.y"
    { setvstore(yyvsp[-3], 0, 6); }
    break;

  case 75:
#line 220 "kal3.y"
    { setvstore(yyvsp[-4], 0, 3); }
    break;

  case 76:
#line 221 "kal3.y"
    { setvstore(yyvsp[-4], 3, 6); }
    break;

  case 78:
#line 225 "kal3.y"
    { yyval = keepMarker(); }
    break;

  case 80:
#line 229 "kal3.y"
    { yyval = yyvsp[0]; }
    break;

  case 81:
#line 230 "kal3.y"
    { yyval = yyvsp[0]; }
    break;

  case 82:
#line 231 "kal3.y"
    { yyval = octalval(yyvsp[0]); }
    break;

  case 83:
#line 234 "kal3.y"
    { vfraction(yyvsp[0], 47); }
    break;

  case 84:
#line 235 "kal3.y"
    { voctal(yyvsp[0], 47); }
    break;

  case 85:
#line 236 "kal3.y"
    { voctal(yyvsp[-2], yyvsp[0]); }
    break;

  case 86:
#line 237 "kal3.y"
    { vfraction(yyvsp[-2], 47); }
    break;

  case 87:
#line 238 "kal3.y"
    { vfraction(yyvsp[-4], yyvsp[0]); }
    break;

  case 88:
#line 239 "kal3.y"
    { vfraction(yyvsp[-2], yyvsp[0]); }
    break;

  case 89:
#line 240 "kal3.y"
    { vfraction(yyvsp[0], 47); negatevstore(); }
    break;

  case 90:
#line 241 "kal3.y"
    { vfraction(yyvsp[-2], 47); negatevstore(); }
    break;

  case 91:
#line 242 "kal3.y"
    { vfraction(yyvsp[-4], yyvsp[0]); negatevstore(); }
    break;

  case 92:
#line 243 "kal3.y"
    { vfraction(yyvsp[-2], yyvsp[0]); negatevstore(); }
    break;

  case 93:
#line 244 "kal3.y"
    { vfraction(yyvsp[0], 47); }
    break;

  case 94:
#line 245 "kal3.y"
    { vfraction(yyvsp[-2], 47); }
    break;

  case 95:
#line 246 "kal3.y"
    { vfraction(yyvsp[-4], yyvsp[0]); }
    break;

  case 96:
#line 247 "kal3.y"
    { vfraction(yyvsp[-2], yyvsp[0]); }
    break;

  case 97:
#line 248 "kal3.y"
    { vqformat(yyvsp[-4], yyvsp[-2], yyvsp[0]); }
    break;

  case 98:
#line 249 "kal3.y"
    { vqformat(0, 0, yyvsp[0]); }
    break;

  case 99:
#line 250 "kal3.y"
    { vfloat(yyvsp[-2], 0); vfppos(); }
    break;

  case 100:
#line 251 "kal3.y"
    { vfloat(yyvsp[0], 0); vfppos(); }
    break;

  case 101:
#line 252 "kal3.y"
    { vfloat(yyvsp[-4], yyvsp[0]); vfppos(); }
    break;

  case 102:
#line 253 "kal3.y"
    { vfloat(yyvsp[-5], -yyvsp[0]); vfppos(); }
    break;

  case 103:
#line 254 "kal3.y"
    { vfloat(yyvsp[-5], yyvsp[0]); vfppos(); }
    break;

  case 104:
#line 255 "kal3.y"
    { vfloat(yyvsp[-2], yyvsp[0]); vfppos(); }
    break;

  case 105:
#line 256 "kal3.y"
    { vfloat(yyvsp[-3], -yyvsp[0]); vfppos(); }
    break;

  case 106:
#line 257 "kal3.y"
    { vfloat(yyvsp[-2], 0); vfpneg(); }
    break;

  case 107:
#line 258 "kal3.y"
    { vfloat(yyvsp[0], 0); vfpneg(); }
    break;

  case 108:
#line 259 "kal3.y"
    { vfloat(yyvsp[-4], yyvsp[0]); vfpneg(); }
    break;

  case 109:
#line 260 "kal3.y"
    { vfloat(yyvsp[-5], -yyvsp[0]); vfpneg(); }
    break;

  case 110:
#line 261 "kal3.y"
    { vfloat(yyvsp[-5], yyvsp[0]); vfpneg(); }
    break;

  case 111:
#line 262 "kal3.y"
    { vfloat(yyvsp[-2], yyvsp[0]); vfpneg(); }
    break;

  case 112:
#line 263 "kal3.y"
    { vfloat(yyvsp[-3], -yyvsp[0]); vfpneg(); }
    break;

  case 113:
#line 264 "kal3.y"
    { vfloat(yyvsp[-2], 0); vfppos(); }
    break;

  case 114:
#line 265 "kal3.y"
    { vfloat(yyvsp[0], 0); vfppos(); }
    break;

  case 115:
#line 266 "kal3.y"
    { vfloat(yyvsp[-4], yyvsp[0]); vfppos(); }
    break;

  case 116:
#line 267 "kal3.y"
    { vfloat(yyvsp[-5], -yyvsp[0]); vfppos(); }
    break;

  case 117:
#line 268 "kal3.y"
    { vfloat(yyvsp[-5], yyvsp[0]); vfppos(); }
    break;

  case 118:
#line 269 "kal3.y"
    { vfloat(yyvsp[-2], yyvsp[0]); vfppos(); }
    break;

  case 119:
#line 270 "kal3.y"
    { vfloat(yyvsp[-3], -yyvsp[0]); vfppos(); }
    break;

  case 120:
#line 272 "kal3.y"
    { vfraction(yyvsp[-5], -yyvsp[0]); }
    break;

  case 123:
#line 279 "kal3.y"
    {  yyval = 0; }
    break;

  case 124:
#line 280 "kal3.y"
    {  yyval = 1; }
    break;

  case 125:
#line 281 "kal3.y"
    {  yyval = 2; }
    break;

  case 126:
#line 282 "kal3.y"
    {  yyval = 3; }
    break;

  case 127:
#line 283 "kal3.y"
    {  yyval = 4; }
    break;

  case 128:
#line 284 "kal3.y"
    {  yyval = 5; }
    break;

  case 129:
#line 285 "kal3.y"
    {  yyval = 6; }
    break;

  case 130:
#line 286 "kal3.y"
    {  yyval = 7; }
    break;

  case 131:
#line 287 "kal3.y"
    {  yyval = 8; }
    break;

  case 132:
#line 288 "kal3.y"
    {  yyval = 9; }
    break;

  case 133:
#line 289 "kal3.y"
    {  yyval = 10; }
    break;

  case 134:
#line 290 "kal3.y"
    {  yyval = 11; }
    break;

  case 135:
#line 291 "kal3.y"
    {  yyval = 12; }
    break;

  case 136:
#line 292 "kal3.y"
    {  yyval = 13; }
    break;

  case 137:
#line 293 "kal3.y"
    {  yyval = 14; }
    break;

  case 138:
#line 294 "kal3.y"
    {  yyval = 15; }
    break;

  case 139:
#line 295 "kal3.y"
    {  yyval = 16; }
    break;

  case 140:
#line 296 "kal3.y"
    {  yyval = 17; }
    break;

  case 141:
#line 297 "kal3.y"
    {  yyval = 18; }
    break;

  case 142:
#line 298 "kal3.y"
    {  yyval = 19; }
    break;

  case 143:
#line 299 "kal3.y"
    {  yyval = 20; }
    break;

  case 144:
#line 300 "kal3.y"
    {  yyval = 21; }
    break;

  case 145:
#line 301 "kal3.y"
    {  yyval = 22; }
    break;

  case 146:
#line 302 "kal3.y"
    {  yyval = 23; }
    break;

  case 147:
#line 303 "kal3.y"
    {  yyval = 24; }
    break;

  case 148:
#line 304 "kal3.y"
    {  yyval = 25; }
    break;

  case 149:
#line 305 "kal3.y"
    {  yyval = 26; }
    break;

  case 150:
#line 308 "kal3.y"
    {  yyval = dataloc(-1, yyvsp[0]); }
    break;

  case 151:
#line 309 "kal3.y"
    {  yyval = dataloc(-1, yyvsp[-1]) << 1; }
    break;

  case 152:
#line 310 "kal3.y"
    {  yyval = (dataloc(-1, yyvsp[-1]) << 1) | 1; }
    break;

  case 153:
#line 311 "kal3.y"
    {  yyval = dataloc(yyvsp[0], yyvsp[-2]); }
    break;

  case 154:
#line 312 "kal3.y"
    {  yyval = wordform(codeloc(-1, yyvsp[0])); }
    break;

  case 155:
#line 313 "kal3.y"
    {  yyval = wordform(codeloc(yyvsp[0], yyvsp[-2])); }
    break;

  case 156:
#line 314 "kal3.y"
    {  yyval = codeloc(yyvsp[0], 0)/6; }
    break;

  case 157:
#line 315 "kal3.y"
    {  yyval = yyvsp[0]; }
    break;

  case 158:
#line 316 "kal3.y"
    {  yyval = ystoreloc(yyvsp[-1], yyvsp[0]); }
    break;

  case 159:
#line 317 "kal3.y"
    {  yyval = ystoreloc(-1, yyvsp[0]); }
    break;

  case 160:
#line 318 "kal3.y"
    {  yyval = ystoreloc(-2, yyvsp[0]); }
    break;

  case 161:
#line 319 "kal3.y"
    {  yyval = ystoreloc(-3, yyvsp[0]); }
    break;

  case 162:
#line 322 "kal3.y"
    {yyval = twosylqx(0100, yyvsp[-1], yyvsp[-3]); }
    break;

  case 163:
#line 323 "kal3.y"
    {yyval = twosylqx(0102, yyvsp[-2], yyvsp[-4]); }
    break;

  case 164:
#line 324 "kal3.y"
    {yyval = twosylqx(0110, yyvsp[-2], yyvsp[-4]); }
    break;

  case 165:
#line 325 "kal3.y"
    {yyval = twosylqx(0112, yyvsp[-3], yyvsp[-5]); }
    break;

  case 166:
#line 326 "kal3.y"
    {yyval = twosylqx(0104, yyvsp[-2], yyvsp[-4]); }
    break;

  case 167:
#line 327 "kal3.y"
    {yyval = twosylqx(0106, yyvsp[-3], yyvsp[-5]); }
    break;

  case 168:
#line 328 "kal3.y"
    {yyval = twosylqx(0114, yyvsp[-3], yyvsp[-5]); }
    break;

  case 169:
#line 329 "kal3.y"
    {yyval = twosylqx(0116, yyvsp[-4], yyvsp[-6]); }
    break;

  case 170:
#line 332 "kal3.y"
    {  yyval = yyvsp[0]; }
    break;

  case 171:
#line 333 "kal3.y"
    {  yyval = (-yyvsp[0]) & 0177; }
    break;

  case 175:
#line 339 "kal3.y"
    { setlabel(yyvsp[-1]); }
    break;

  case 177:
#line 341 "kal3.y"
    { newproutine(yyvsp[-1], -1); }
    break;

  case 178:
#line 342 "kal3.y"
    { newproutine(yyvsp[-3], yyvsp[-1]); }
    break;

  case 179:
#line 343 "kal3.y"
    { newproutine(-yyvsp[-1], -1); }
    break;

  case 180:
#line 345 "kal3.y"
    { storememinstr(0300, yyvsp[-1], 0); }
    break;

  case 181:
#line 346 "kal3.y"
    { storememinstr(0301, yyvsp[-1], 0); }
    break;

  case 182:
#line 348 "kal3.y"
    { qnum(yyvsp[-1]); storememinstr(0300, yyvsp[-3], yyvsp[-1]); }
    break;

  case 183:
#line 349 "kal3.y"
    { qnum(yyvsp[-1]); storememinstr(0301, yyvsp[-3], yyvsp[-1]); }
    break;

  case 184:
#line 350 "kal3.y"
    { qnum(yyvsp[-2]); storememinstr(0302, yyvsp[-4], yyvsp[-2]); }
    break;

  case 185:
#line 351 "kal3.y"
    { qnum(yyvsp[-2]); storememinstr(0303, yyvsp[-4], yyvsp[-2]); }
    break;

  case 186:
#line 353 "kal3.y"
    {  store3syl(0304, yyvsp[-1]); }
    break;

  case 187:
#line 354 "kal3.y"
    {  store3syl(0304, octalval(yyvsp[-1])); }
    break;

  case 188:
#line 355 "kal3.y"
    {  store3syl(0304, (-yyvsp[-1])&0177777); }
    break;

  case 189:
#line 356 "kal3.y"
    {  store3syl(0304, yyvsp[-1]); }
    break;

  case 190:
#line 357 "kal3.y"
    {  store3syl(0304, yyvsp[-1]); }
    break;

  case 191:
#line 358 "kal3.y"
    {  store3syl(0304, yyvsp[-2]<<1); }
    break;

  case 192:
#line 359 "kal3.y"
    {  store3syl(0304, (yyvsp[-2]<<1) + 1); }
    break;

  case 193:
#line 360 "kal3.y"
    {  store3syl(0304, codeloc(yyvsp[-1], 0)/6); }
    break;

  case 194:
#line 362 "kal3.y"
    {  store2syl(yyvsp[0]); }
    break;

  case 195:
#line 363 "kal3.y"
    {  store2syl(256 + yyvsp[0]); }
    break;

  case 196:
#line 365 "kal3.y"
    { store2syl(twosylqx(0151, yyvsp[-5], yyvsp[-1])); }
    break;

  case 197:
#line 366 "kal3.y"
    { store2syl(twosylqx(0152, yyvsp[-5], yyvsp[-1])); }
    break;

  case 198:
#line 367 "kal3.y"
    { store2syl(twosylqx(0153, yyvsp[-5], yyvsp[-1])); }
    break;

  case 199:
#line 368 "kal3.y"
    { store2syl(twosylqx(0154, yyvsp[-5], yyvsp[-1])); }
    break;

  case 200:
#line 369 "kal3.y"
    { store2syl(twosylqx(0155, yyvsp[-5], yyvsp[-1])); }
    break;

  case 201:
#line 370 "kal3.y"
    { store2syl(twosylqx(0156, yyvsp[-5], yyvsp[-1])); }
    break;

  case 202:
#line 371 "kal3.y"
    { store2syl(twosylqx(0157, yyvsp[-5], yyvsp[-1])); }
    break;

  case 203:
#line 372 "kal3.y"
    { storesyl(001); }
    break;

  case 204:
#line 373 "kal3.y"
    { storesyl(002); }
    break;

  case 205:
#line 374 "kal3.y"
    { storesyl(003); }
    break;

  case 206:
#line 382 "kal3.y"
    { storesyl(004); }
    break;

  case 207:
#line 383 "kal3.y"
    { storesyl(005); }
    break;

  case 208:
#line 384 "kal3.y"
    { storesyl(007); }
    break;

  case 209:
#line 385 "kal3.y"
    { storesyl(034); }
    break;

  case 210:
#line 386 "kal3.y"
    { storesyl(035); }
    break;

  case 211:
#line 387 "kal3.y"
    { store2syl(twosylshetc(0163, 2*yyvsp[-1] + 1)); }
    break;

  case 212:
#line 388 "kal3.y"
    { store2syl(twosylqx(0163, yyvsp[-1], 0)); }
    break;

  case 213:
#line 389 "kal3.y"
    { storesyl(004); }
    break;

  case 214:
#line 390 "kal3.y"
    { storesyl(005); }
    break;

  case 215:
#line 391 "kal3.y"
    { storesyl(007); }
    break;

  case 216:
#line 392 "kal3.y"
    { storesyl(034); }
    break;

  case 217:
#line 393 "kal3.y"
    { storesyl(035); }
    break;

  case 218:
#line 394 "kal3.y"
    { store2syl(twosylshetc(0163, 2*yyvsp[-1] + 1)); }
    break;

  case 219:
#line 395 "kal3.y"
    { store2syl(twosylqx(0163, yyvsp[-1], 0)); }
    break;

  case 220:
#line 396 "kal3.y"
    { storesyl(004); }
    break;

  case 221:
#line 397 "kal3.y"
    { storesyl(005); }
    break;

  case 222:
#line 398 "kal3.y"
    { storesyl(007); }
    break;

  case 223:
#line 399 "kal3.y"
    { storesyl(034); }
    break;

  case 224:
#line 400 "kal3.y"
    { storesyl(035); }
    break;

  case 225:
#line 401 "kal3.y"
    { store2syl(twosylshetc(0163, 2*yyvsp[-1] + 1)); }
    break;

  case 226:
#line 402 "kal3.y"
    { store2syl(twosylqx(0163, yyvsp[-1], 0)); }
    break;

  case 227:
#line 403 "kal3.y"
    { storesyl(010); }
    break;

  case 228:
#line 404 "kal3.y"
    { storesyl(011); }
    break;

  case 229:
#line 405 "kal3.y"
    { storesyl(012); }
    break;

  case 230:
#line 406 "kal3.y"
    { storesyl(013); }
    break;

  case 231:
#line 407 "kal3.y"
    { storesyl(014); }
    break;

  case 232:
#line 408 "kal3.y"
    { storesyl(015); }
    break;

  case 233:
#line 409 "kal3.y"
    { storesyl(016); }
    break;

  case 234:
#line 410 "kal3.y"
    { storesyl(017); }
    break;

  case 235:
#line 411 "kal3.y"
    { storesyl(020); }
    break;

  case 236:
#line 412 "kal3.y"
    { storesyl(021); }
    break;

  case 237:
#line 413 "kal3.y"
    { storesyl(022); }
    break;

  case 238:
#line 414 "kal3.y"
    { storesyl(023); }
    break;

  case 239:
#line 415 "kal3.y"
    { storesyl(024); }
    break;

  case 240:
#line 416 "kal3.y"
    { storesyl(025); }
    break;

  case 241:
#line 417 "kal3.y"
    { storesyl(026); }
    break;

  case 242:
#line 418 "kal3.y"
    { storesyl(027); }
    break;

  case 243:
#line 419 "kal3.y"
    { storesyl(030); }
    break;

  case 244:
#line 420 "kal3.y"
    { storesyl(031); }
    break;

  case 245:
#line 421 "kal3.y"
    { storesyl(032); }
    break;

  case 246:
#line 422 "kal3.y"
    { storesyl(033); }
    break;

  case 247:
#line 423 "kal3.y"
    { storesyl(036); }
    break;

  case 248:
#line 424 "kal3.y"
    { storesyl(037); }
    break;

  case 249:
#line 425 "kal3.y"
    { storesyl(041); }
    break;

  case 250:
#line 426 "kal3.y"
    { storesyl(042); }
    break;

  case 251:
#line 427 "kal3.y"
    { storesyl(043); }
    break;

  case 252:
#line 428 "kal3.y"
    { storesyl(045); }
    break;

  case 253:
#line 429 "kal3.y"
    { storesyl(047); }
    break;

  case 254:
#line 430 "kal3.y"
    { storesyl(050); }
    break;

  case 255:
#line 431 "kal3.y"
    { storesyl(051); }
    break;

  case 256:
#line 432 "kal3.y"
    { storesyl(052); }
    break;

  case 257:
#line 433 "kal3.y"
    { storesyl(053); }
    break;

  case 258:
#line 434 "kal3.y"
    { storesyl(054); }
    break;

  case 259:
#line 435 "kal3.y"
    { storesyl(056); }
    break;

  case 260:
#line 436 "kal3.y"
    { storesyl(057); }
    break;

  case 261:
#line 437 "kal3.y"
    { storesyl(044); }
    break;

  case 262:
#line 438 "kal3.y"
    { storesyl(060); }
    break;

  case 263:
#line 439 "kal3.y"
    { storesyl(061); }
    break;

  case 264:
#line 440 "kal3.y"
    { storesyl(062); }
    break;

  case 265:
#line 441 "kal3.y"
    { storesyl(063); }
    break;

  case 266:
#line 442 "kal3.y"
    { storesyl(064); }
    break;

  case 267:
#line 443 "kal3.y"
    { storesyl(044); }
    break;

  case 268:
#line 444 "kal3.y"
    { storesyl(060); }
    break;

  case 269:
#line 445 "kal3.y"
    { storesyl(061); }
    break;

  case 270:
#line 446 "kal3.y"
    { storesyl(062); }
    break;

  case 271:
#line 447 "kal3.y"
    { storesyl(063); }
    break;

  case 272:
#line 448 "kal3.y"
    { storesyl(064); }
    break;

  case 273:
#line 449 "kal3.y"
    { storesyl(044); }
    break;

  case 274:
#line 450 "kal3.y"
    { storesyl(060); }
    break;

  case 275:
#line 451 "kal3.y"
    { storesyl(061); }
    break;

  case 276:
#line 452 "kal3.y"
    { storesyl(062); }
    break;

  case 277:
#line 453 "kal3.y"
    { storesyl(063); }
    break;

  case 278:
#line 454 "kal3.y"
    { storesyl(064); }
    break;

  case 279:
#line 456 "kal3.y"
    { storesyl(044); }
    break;

  case 280:
#line 457 "kal3.y"
    { storesyl(060); }
    break;

  case 281:
#line 458 "kal3.y"
    { storesyl(061); }
    break;

  case 282:
#line 459 "kal3.y"
    { storesyl(062); }
    break;

  case 283:
#line 460 "kal3.y"
    { storesyl(063); }
    break;

  case 284:
#line 461 "kal3.y"
    { storesyl(064); }
    break;

  case 285:
#line 463 "kal3.y"
    { storesyl(065); }
    break;

  case 286:
#line 464 "kal3.y"
    { storesyl(066); }
    break;

  case 287:
#line 465 "kal3.y"
    { storesyl(067); }
    break;

  case 288:
#line 466 "kal3.y"
    { storesyl(070); }
    break;

  case 289:
#line 467 "kal3.y"
    { storesyl(071); }
    break;

  case 290:
#line 468 "kal3.y"
    { storesyl(072); }
    break;

  case 291:
#line 469 "kal3.y"
    { storesyl(074); }
    break;

  case 292:
#line 470 "kal3.y"
    { storesyl(075); }
    break;

  case 293:
#line 471 "kal3.y"
    { storesyl(077); }
    break;

  case 294:
#line 473 "kal3.y"
    { store2syl(twosylshetc(0176, 128>>yyvsp[-1]));  }
    break;

  case 295:
#line 474 "kal3.y"
    { store2syl(twosylshetc(0175, 128>>yyvsp[-1]));  }
    break;

  case 296:
#line 476 "kal3.y"
    {  newproutine(-1, -1); }
    break;

  case 297:
#line 478 "kal3.y"
    { store2syl(twosylqx(0140, yyvsp[-1], 0)); }
    break;

  case 298:
#line 479 "kal3.y"
    { store2syl(twosylqx(0141, yyvsp[-1], 0)); }
    break;

  case 299:
#line 480 "kal3.y"
    { store2syl(twosylqx(0142, yyvsp[-1], 0)); }
    break;

  case 300:
#line 481 "kal3.y"
    { store2syl(twosylqx(0143, yyvsp[-1], 0)); }
    break;

  case 301:
#line 482 "kal3.y"
    { store2syl(twosylqx(0144 + ((yyvsp[-1]-1)&1)*2, yyvsp[-4], 0)); }
    break;

  case 302:
#line 483 "kal3.y"
    { store2syl(twosylqx(0144 + ((yyvsp[-1]-1)&1)*2, yyvsp[-3], 0)); }
    break;

  case 303:
#line 484 "kal3.y"
    { store2syl(twosylqx(0145 + ((yyvsp[-1]-1)&1)*2, yyvsp[-4], 0)); }
    break;

  case 304:
#line 486 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 2)); }
    break;

  case 305:
#line 487 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 3)); }
    break;

  case 306:
#line 488 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 4)); }
    break;

  case 307:
#line 489 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 5)); }
    break;

  case 308:
#line 490 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 010)); }
    break;

  case 309:
#line 491 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 011)); }
    break;

  case 310:
#line 492 "kal3.y"
    { store2syl(twosylqx(0170, yyvsp[-1], 016)); }
    break;

  case 311:
#line 494 "kal3.y"
    { store2syl(twosylqx(0171, yyvsp[-1], 2)); }
    break;

  case 312:
#line 495 "kal3.y"
    { store2syl(twosylqx(0171, yyvsp[-1], 4)); }
    break;

  case 313:
#line 496 "kal3.y"
    { store2syl(twosylqx(0171, yyvsp[-1], 010)); }
    break;

  case 314:
#line 497 "kal3.y"
    { store2syl(twosylqx(0171, yyvsp[-1], 016)); }
    break;

  case 315:
#line 499 "kal3.y"
    { store2syl(twosylqx(0172, yyvsp[-1], 2)); }
    break;

  case 316:
#line 500 "kal3.y"
    { store2syl(twosylqx(0172, yyvsp[-1], 4)); }
    break;

  case 317:
#line 501 "kal3.y"
    { store2syl(twosylqx(0172, yyvsp[-1], 010)); }
    break;

  case 318:
#line 502 "kal3.y"
    { store2syl(twosylqx(0172, yyvsp[-1], 016)); }
    break;

  case 319:
#line 504 "kal3.y"
    { store2syl(twosylshetc(0173, 0)); }
    break;

  case 320:
#line 505 "kal3.y"
    { store2syl(twosylshetc(0174, 0)); }
    break;

  case 321:
#line 507 "kal3.y"
    { store2syl(twosylshetc(0161, 2*yyvsp[-1] + 1)); }
    break;

  case 322:
#line 508 "kal3.y"
    { store2syl(twosylshetc(0162, 2*yyvsp[-1] + 1)); }
    break;

  case 323:
#line 509 "kal3.y"
    { store2syl(twosylshetc(0164, 2*yyvsp[-1] + 1)); }
    break;

  case 324:
#line 510 "kal3.y"
    { store2syl(twosylshetc(0166, 2*yyvsp[-1] + 1)); }
    break;

  case 325:
#line 511 "kal3.y"
    { store2syl(twosylshetc(0167, 2*yyvsp[-1] + 1)); }
    break;

  case 326:
#line 513 "kal3.y"
    { store2syl(twosylqx(0161, yyvsp[-1], 0)); }
    break;

  case 327:
#line 514 "kal3.y"
    { store2syl(twosylqx(0162, yyvsp[-1], 0)); }
    break;

  case 328:
#line 515 "kal3.y"
    { store2syl(twosylqx(0164, yyvsp[-1], 0)); }
    break;

  case 329:
#line 516 "kal3.y"
    { store2syl(twosylqx(0166, yyvsp[-1], 0)); }
    break;

  case 330:
#line 517 "kal3.y"
    { store2syl(twosylqx(0167, yyvsp[-1], 0)); }
    break;

  case 332:
#line 520 "kal3.y"
    { store3syl(0200, (0220<<8)); }
    break;

  case 333:
#line 521 "kal3.y"
    { store3syl(0222, (0360<<8)); }
    break;

  case 334:
#line 522 "kal3.y"
    { store3syl(0202 - ((yyvsp[-1]&1)<<1), (0360<<8)+(yyvsp[-1]>>1)); }
    break;

  case 335:
#line 523 "kal3.y"
    { store3syl(0202, (0360<<8)); }
    break;

  case 336:
#line 524 "kal3.y"
    { store3syl(0202, (0360<<8) + yyvsp[-1]); }
    break;

  case 337:
#line 525 "kal3.y"
    { store3syl(0202, (0360<<8) + codeloc(yyvsp[-1], 0)); }
    break;

  case 338:
#line 527 "kal3.y"
    { store3syl(0202 - ((yyvsp[-4]&1)<<1), (0360<<8) + codeloc(yyvsp[-1], 0)); }
    break;

  case 339:
#line 529 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 000)); }
    break;

  case 340:
#line 530 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 000)); }
    break;

  case 341:
#line 531 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 000)); }
    break;

  case 342:
#line 532 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 000)); }
    break;

  case 343:
#line 533 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 000)); }
    break;

  case 344:
#line 534 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 000)); }
    break;

  case 345:
#line 536 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 004)); }
    break;

  case 346:
#line 537 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 010)); }
    break;

  case 347:
#line 538 "kal3.y"
    { store2syl(twosylqx(0121, yyvsp[-1], 000)); }
    break;

  case 348:
#line 539 "kal3.y"
    { store2syl(twosylqx(0122, yyvsp[-1], 000)); }
    break;

  case 349:
#line 540 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 000)); }
    break;

  case 350:
#line 541 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 002)); }
    break;

  case 351:
#line 542 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 004)); }
    break;

  case 352:
#line 543 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 010)); }
    break;

  case 353:
#line 544 "kal3.y"
    { store2syl(twosylqx(0125, yyvsp[-1], 000)); }
    break;

  case 354:
#line 545 "kal3.y"
    { store2syl(twosylqx(0125, yyvsp[-1], 000)); }
    break;

  case 355:
#line 546 "kal3.y"
    { store2syl(twosylqx(0126, yyvsp[-1], 000)); }
    break;

  case 356:
#line 547 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 010)); }
    break;

  case 357:
#line 548 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 010)); }
    break;

  case 358:
#line 549 "kal3.y"
    { store2syl(twosylqx(0134, yyvsp[-1], 002)); }
    break;

  case 359:
#line 550 "kal3.y"
    { store2syl(twosylqx(0131, yyvsp[-1], 000)); }
    break;

  case 360:
#line 551 "kal3.y"
    { store2syl(twosylqx(0131, yyvsp[-1], 000)); }
    break;

  case 361:
#line 552 "kal3.y"
    { store2syl(twosylqx(0131, yyvsp[-1], 000)); }
    break;

  case 362:
#line 554 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 002)); }
    break;

  case 363:
#line 555 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 014)); }
    break;

  case 364:
#line 556 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 014)); }
    break;

  case 365:
#line 557 "kal3.y"
    { store2syl(twosylqx(0131, yyvsp[-1], 010)); }
    break;

  case 366:
#line 558 "kal3.y"
    { store2syl(twosylqx(0134, yyvsp[-1], 000)); }
    break;

  case 367:
#line 559 "kal3.y"
    { store2syl(twosylqx(0136, yyvsp[-1], 000)); }
    break;

  case 368:
#line 560 "kal3.y"
    { store2syl(twosylqx(0136, yyvsp[-1], 010)); }
    break;

  case 369:
#line 561 "kal3.y"
    { store2syl(twosylqx(0127, yyvsp[-1], 000)); }
    break;

  case 370:
#line 562 "kal3.y"
    { store2syl(twosylqx(0125, yyvsp[-1], 000)); }
    break;

  case 371:
#line 563 "kal3.y"
    { store2syl(twosylqx(0125, yyvsp[-1], 010)); }
    break;

  case 372:
#line 565 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 004)); }
    break;

  case 373:
#line 566 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 001)); }
    break;

  case 374:
#line 569 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 000)); }
    break;

  case 375:
#line 570 "kal3.y"
    { store2syl(twosylqx(0125, yyvsp[-1], 000)); }
    break;

  case 376:
#line 571 "kal3.y"
    { store2syl(twosylqx(0124, yyvsp[-1], 010)); }
    break;

  case 377:
#line 572 "kal3.y"
    { store2syl(twosylqx(0125, yyvsp[-1], 010)); }
    break;

  case 378:
#line 573 "kal3.y"
    { store2syl(twosylqx(0126, yyvsp[-1], 000)); }
    break;

  case 379:
#line 574 "kal3.y"
    { store2syl(twosylqx(0127, yyvsp[-1], 000)); }
    break;

  case 380:
#line 575 "kal3.y"
    { store2syl(twosylqx(0126, yyvsp[-1], 010)); }
    break;

  case 381:
#line 576 "kal3.y"
    { store2syl(twosylqx(0127, yyvsp[-1], 010)); }
    break;

  case 382:
#line 578 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 000)); }
    break;

  case 383:
#line 579 "kal3.y"
    { store2syl(twosylqx(0131, yyvsp[-1], 000)); }
    break;

  case 384:
#line 580 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 010)); }
    break;

  case 385:
#line 581 "kal3.y"
    { store2syl(twosylqx(0131, yyvsp[-1], 010)); }
    break;

  case 386:
#line 582 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 014)); }
    break;

  case 387:
#line 583 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 004)); }
    break;

  case 388:
#line 585 "kal3.y"
    { store2syl(twosylqx(0134, yyvsp[-1], 000)); }
    break;

  case 389:
#line 586 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 010)); }
    break;

  case 390:
#line 587 "kal3.y"
    { store2syl(twosylqx(0120, yyvsp[-1], 004)); }
    break;

  case 391:
#line 588 "kal3.y"
    { store2syl(twosylqx(0136, yyvsp[-1], 010)); }
    break;

  case 392:
#line 589 "kal3.y"
    { store2syl(twosylqx(0136, yyvsp[-1], 000)); }
    break;

  case 393:
#line 590 "kal3.y"
    { store2syl(twosylqx(0122, yyvsp[-1], 000)); }
    break;

  case 394:
#line 591 "kal3.y"
    { store2syl(twosylqx(0122, yyvsp[-1], 004)); }
    break;

  case 395:
#line 592 "kal3.y"
    { store2syl(twosylqx(0130, yyvsp[-1], 002)); }
    break;

  case 396:
#line 593 "kal3.y"
    { store2syl(twosylqx(0134, yyvsp[-1], 004)); }
    break;

  case 397:
#line 594 "kal3.y"
    { store2syl(twosylqx(0136, yyvsp[-1], 004)); }
    break;

  case 398:
#line 596 "kal3.y"
    { store2syl(twosylqx(0132, yyvsp[-1], 000)); }
    break;

  case 399:
#line 597 "kal3.y"
    { store2syl(twosylqx(0133, yyvsp[-1], 000)); }
    break;

  case 400:
#line 598 "kal3.y"
    { store2syl(twosylqx(0133, yyvsp[-1], 010)); }
    break;

  case 401:
#line 599 "kal3.y"
    { store2syl(twosylqx(0132, yyvsp[-1], 010)); }
    break;

  case 402:
#line 602 "kal3.y"
    { startnewword(); }
    break;

  case 403:
#line 603 "kal3.y"
    { startnewword(); }
    break;


    }

/* Line 991 of yacc.c.  */
#line 3981 "y.tab.c"

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


#line 605 "kal3.y"



