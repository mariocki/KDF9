PROGRAM TESTT {TOKEN} {(INPUT, OUTPUT)};
LABEL 9999;
TYPE
   SYMBOLS = (ID, INT, OPEN, CLOSE, OP, STOP);
   OPS = (PLUS, MINUS);
   SPELLING = array [1..8] of char;
   TOKENS = RECORD
            CASE SYMBOL : SYMBOLS OF
               ID:  (NAME : SPELLING);
               INT: (INTVALUE : INTEGER);
               OP:  (OPERATOR : OPS)
            END;
VAR
   TOKENSEQUENCE : ARRAY[ 1..100 ] OF TOKENS;
   TOKEN : TOKENS;
   TOKENCOUNT : INTEGER;
   CH : CHAR;

   PROCEDURE READCH;
      BEGIN {READ(CH); WRITE(CH)} END (* READCH *);

   PROCEDURE FETCH (VAR TOKEN : TOKENS);
      VAR
         I : INTEGER;
      BEGIN
      WHILE CH=' ' DO
         READCH;
      WITH TOKEN DO
         CASE CH OF
            'A','B','C','D','E','F','G','H','I','J','K','L',
                'M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z':
               BEGIN
               SYMBOL := ID;
               FOR I := 1 TO 8 DO
                  NAME[I] := ' ';
               I := 0;
               REPEAT
                  I := I + 1;
                  IF I<=8 THEN
                     NAME[I] := CH;
                  READCH;
                  UNTIL (CH<'A') OR (CH>'Z')
               END;
            '0','1','2','3','4','5','6','7','8','9':
               BEGIN
               SYMBOL := INT;  INTVALUE := 0;
               REPEAT
                  INTVALUE := 10*INTVALUE + (ORD(CH)-ORD('0'));
                  READCH;
                  UNTIL (CH<'0') OR (CH>'9');
               END;
            '(':
               BEGIN SYMBOL := OPEN; READCH END;
            ')':
               BEGIN SYMBOL := CLOSE; READCH END;
            '+':
               BEGIN SYMBOL := OP; OPERATOR := PLUS; READCH END;
            '-':
               BEGIN SYMBOL := OP; OPERATOR := MINUS; READCH END;
            '.':
               BEGIN SYMBOL := STOP; READCH END;
            END (*CASE*);
      END (* FETCH *);

   PROCEDURE SCAN;
      BEGIN
      FETCH(TOKEN);
      TOKENCOUNT := TOKENCOUNT + 1;
      {TOKENSEQUENCE[TOKENCOUNT] := TOKEN;}
      END (* SCAN *);

   PROCEDURE ERROR;
      BEGIN
      {WRITELN('        ***ERROR');}
      HALT(-1);
      END (* ERROR *);

   PROCEDURE EXPRESSION;

      PROCEDURE PRIMARY;
         BEGIN
         CASE TOKEN.SYMBOL OF
            ID, INT:
               SCAN;
            OPEN:
               BEGIN
               SCAN;
               EXPRESSION;
               IF TOKEN.SYMBOL=CLOSE THEN
                  SCAN
               ELSE
                  ERROR;
               END;
            STOP, CLOSE, OP:
               ERROR;
            END (*CASE*);
         END (* PRIMARY *);

      BEGIN (* EXPRESSION *)
      PRIMARY;
      WHILE TOKEN.SYMBOL=OP DO
         BEGIN
         SCAN;
         PRIMARY;
         END;
      END (* EXPRESSION *);

BEGIN (* ANALYSEEXPRESSION *)
TOKENCOUNT := 0;
READCH;
SCAN;
EXPRESSION;
IF TOKEN.SYMBOL <> STOP THEN
   ERROR;
{WRITELN;
WRITELN('        EXPRESSION O.K.');
RESET(OUTPUT);}
9999:
END .
