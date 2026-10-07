' ON ERROR, DEF FN, bitwise logic: QuickBASIC 4.5 style. Compile with --qbs:
'   bin/kayte --qbs --native examples/quickbasic/errors.bas -o errors && ./errors
DEFINT A-Z
CONST TRUE = -1, FALSE = 0

' ---- DEF FN: one-line and block functions (they see the program's variables)
DEF FNpercent (part, whole) = part * 100 \ whole
DEF FNclamp (v, lo, hi)
  IF v < lo THEN FNclamp = lo: EXIT DEF
  IF v > hi THEN FNclamp = hi ELSE FNclamp = v
END DEF
PRINT "37 of 50 is"; FNpercent(37, 50); "%"
PRINT "clamp(-5, 0, 10) ="; FNclamp(-5, 0, 10); " clamp(42, 0, 10) ="; FNclamp(42, 0, 10)

' ---- true is -1, and AND / OR / XOR / NOT work on bits
PRINT "5 > 3 is"; 5 > 3; "  NOT TRUE is"; NOT TRUE
perms = 5                                  ' read (4) + execute (1)
IF perms AND 4 THEN PRINT "readable";
IF (perms AND 2) = 0 THEN PRINT ", not writable";
IF perms AND 1 THEN PRINT ", executable"
perms = perms OR 2                         ' add write
PRINT "permissions now"; perms; " (binary flags 4 + 2 + 1)"

' ---- string surgery: MID$ statement, LSET / RSET into fixed widths
title$ = "Kayte BASIC"
MID$(title$, 7) = "QBASI"
PRINT "MID$ changed it to: "; title$
col$ = SPACE$(12)
RSET col$ = "1,234.50": PRINT "["; col$; "]"
LSET col$ = "Total": PRINT "["; col$; "]"

' ---- ON ERROR: a handler that fixes the problem and RESUMEs
ON ERROR GOTO ErrorHandler
divisor = 0
100 PRINT "100 / divisor ="; 100 \ divisor
110 OPEN "no-such-file.dat" FOR INPUT AS #1
120 PRINT "the program went on after the missing file"
130 ERROR 250
140 PRINT "and after ERROR 250"
ON ERROR GOTO 0
PRINT "done"
END

ErrorHandler:
  PRINT "  ! error"; ERR; "at line"; ERL; ": ";
  SELECT CASE ERR
    CASE 11
      PRINT "division by zero - using 4 instead"
      divisor = 4
      RESUME                          ' try line 100 again
    CASE 53
      PRINT "file not found - skipping it"
      RESUME NEXT
    CASE ELSE
      PRINT "error"; ERR; "- ignored"
      RESUME NEXT
  END SELECT
