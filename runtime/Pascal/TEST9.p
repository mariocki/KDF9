PROGRAM TEST9 {(TESTFILE, OUTPUT)};

TYPE
   ADDRESS = INTEGER;
   VERSION = (INTEGERS, CHARACTERS, BOOLEANS, ENUMERATIONS, POINTERS,
             RECORDS, ARRAYS, SETS);
   STRUCTURE = RECORD
               FIXEDINT1, FIXEDINT2, FIXEDINT3, FIXEDINT4, FIXEDINT5, FIXEDINT6 : INTEGER;
               REAL1, REAL2, REAL3, REAL4 : REAL;
               FIXEDCHAR1, FIXEDCHAR2, FIXEDCHAR3, FIXEDCHAR4 : CHAR;
               FIXEDBOOL1, FIXEDBOOL2, FIXEDBOOL3 : BOOLEAN;
               CASE SUBSTRUCTURE : VERSION OF
                  RECORDS :
                   (EMPTYREC : RECORD END;
                    BASEREC  : RECORD I : INTEGER;  R : REAL END;
                    UNION : RECORD CASE B : BOOLEAN OF
                              FALSE : ( );
                              TRUE :  (C : CHAR)
                              END);
                 ARRAYS :
                   (VECTOR : ARRAY[ 1..10 ] OF REAL;
                    STRING : PACKED ARRAY[ 1..32 ] OF CHAR);
                 SETS :
                   (PRIMES : SET OF 0..63;
                    PRESENT : SET OF VERSION);
                 POINTERS :
                   (PTR1, PTR2 : ^ INTEGER;  LEFT, RIGHT : ^ STRUCTURE);
                 INTEGERS :
                   ( );
                 CHARACTERS :
                   (CHAR1, CHAR2, CHAR3, CHAR4 : 'A'..'Z');
                 BOOLEANS :
                   (BOOL1A, BOOL1B : FALSE..FALSE;
                    BOOL2A, BOOL2B : TRUE..TRUE;
                    BOOL3 : FALSE..TRUE);
                 ENUMERATIONS :
                   (DAY1, DAY2, DAY3 : (MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY,
                                        SATURDAY, SUNDAY);
                    WEEK1, WEEK2, WEEK3A, WEEK3B : MONDAY..FRIDAY)
               END;

VAR
   TESTFILE : FILE OF STRUCTURE;
   RECS : ARRAY[VERSION] OF STRUCTURE;
   KIND : VERSION;
   CHARS : ARRAY[1..200] OF CHAR;
   VOIDS : ARRAY[1..200] OF RECORD END;
   I : 1..200;

   PROCEDURE INVALIDATE (A : ADDRESS);
      BEGIN
      a := -maxint;
      usercode a; =m15; e-1m15; =a end;
      { STOREWORDAT(-MAXINT, A); STOREWORDAT(WORDAT(A)-1, A); }
      END (*INVALIDATE*);

   PROCEDURE INITIALIZE (VAR REC : STRUCTURE;  KIND : VERSION);
   BEGIN
   WITH REC DO
      BEGIN
      FIXEDINT1 := -MAXINT;
      FIXEDINT2 := MAXINT;
      FIXEDINT3 := 0;
      FIXEDINT4 := -1234;
      FIXEDINT5 := 5;
      INVALIDATE(ADDRESSOF(FIXEDINT6));
      REAL1 := -1.23E4;
      REAL2 := +4.5E-6;
      REAL3 := 0.0;
      INVALIDATE(ADDRESSOF(REAL4));
      FIXEDCHAR1 := CHR(0);
      FIXEDCHAR2 := CHR(127);
      FIXEDCHAR3 := 'R';
      INVALIDATE(ADDRESSOF(FIXEDCHAR4));
      FIXEDBOOL1 := FALSE;
      FIXEDBOOL2 := TRUE;
      INVALIDATE(ADDRESSOF(FIXEDBOOL3));
      SUBSTRUCTURE := KIND;
      CASE KIND OF
         RECORDS:
            BEGIN
            BASEREC.I := 1234567890;
            BASEREC.R := -1.2345678901234567890E50;
            UNION.B := TRUE;
            UNION.C := 'R';
            END;
         ARRAYS :
            BEGIN
            FOR I := 1 TO 10 DO
            VECTOR[I] := 1/I;
            STRING := 'BILL FINDLAY, GLASGOW UNIVERSITY';
            END;
         SETS :
            BEGIN
            PRIMES :=[2,3,5,7,11,13,17,19,23,29,31,37];
            PRESENT :=[CHARACTERS..POINTERS, SETS];
            END;
         POINTERS :
            BEGIN
            NEW(PTR1);
            INVALIDATE(ADDRESSOF(PTR2));
            LEFT := NIL;
            INVALIDATE(ADDRESSOF(RIGHT));
            END;
         INTEGERS :
            BEGIN
            END;
            CHARACTERS :
            BEGIN
            STOREWORDAT(0, ADDRESSOF(CHAR1));
            STOREWORDAT(127, ADDRESSOF(CHAR2));  CHAR3 := 'R';
            INVALIDATE(ADDRESSOF(CHAR4));
            END;
         BOOLEANS :
            BEGIN
            BOOL1A := FALSE;  STOREWORDAT(1, ADDRESSOF(BOOL1B));
            STOREWORDAT(0, ADDRESSOF(BOOL2A));  BOOL2B := TRUE;
            INVALIDATE(ADDRESSOF(BOOL3));
            END;
         ENUMERATIONS :
            BEGIN
            DAY1 := MONDAY;  DAY2 := SUNDAY;
            INVALIDATE(ADDRESSOF(DAY3));
            WEEK1 := MONDAY;  WEEK2 := FRIDAY;
            STOREWORDAT(ORD(SUNDAY), ADDRESSOF(WEEK3A));
            INVALIDATE(ADDRESSOF(WEEK3B));
            END
         END;
      END;
   END (* INITIALIZE *) ;

BEGIN
FOR KIND := INTEGERS TO SETS DO
   INITIALIZE(RECS[KIND], KIND);
FOR I := 1 TO 200 DO
   CHARS[I] := CHR(ORD('0') + I MOD 10);
REWRITE(TESTFILE);
FOR KIND := INTEGERS TO SETS DO
   BEGIN
   TESTFILE^ := RECS[KIND];
   PUT(TESTFILE);
   END;
RESET(TESTFILE);
FOR KIND := INTEGERS TO RECORDS DO
   GET(TESTFILE);
(* FAIL *)
RESET(OUTPUT);
END (* TEST9 *) .
