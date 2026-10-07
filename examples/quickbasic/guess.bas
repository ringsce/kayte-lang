' Number guessing, QuickBASIC style (interactive): compile it with --qbs
'   bin/kayte --qbs --native examples/quickbasic/guess.bas -o guess && ./guess
RANDOMIZE TIMER
CLS
COLOR 14
PRINT "*** GUESS THE NUMBER ***"
COLOR 7
INPUT "What's your name"; player$
IF player$ = "" THEN player$ = "Player"

DO
  secret = INT(RND * 100) + 1    ' 1 .. 100
  tries = 0
  PRINT
  PRINT "I'm thinking of a number from 1 to 100, "; player$; "."
  DO
    INPUT "Your guess"; guess
    tries = tries + 1
    SELECT CASE guess
      CASE IS < secret: PRINT "Higher!"
      CASE IS > secret: PRINT "Lower!"
      CASE ELSE
        COLOR 10
        PRINT "Right! You took"; tries; "tries."
        COLOR 7
    END SELECT
  LOOP UNTIL guess = secret OR guess = 0
  LINE INPUT "Play again (y/n)? "; again$
LOOP WHILE UCASE$(LEFT$(again$, 1)) = "Y"
PRINT "Bye, "; player$; "!"
END
