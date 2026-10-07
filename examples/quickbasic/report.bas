' A QuickBASIC 4.5-style program: compile it with --qbs
'   bin/kayte --qbs --native examples/quickbasic/report.bas -o report && ./report
DECLARE SUB PrintTable ()
DECLARE FUNCTION Average% (total%, count%)
DEFINT A-Z

CONST MAXITEMS = 5
TYPE Employee
  ename AS STRING * 12
  dept AS STRING * 8
  salary AS LONG
END TYPE

DIM SHARED staff(MAXITEMS - 1) AS Employee
DIM SHARED count

10 REM ---- read the records ----
RESTORE StaffData
FOR i = 0 TO MAXITEMS - 1
  READ staff(i).ename, staff(i).dept, staff(i).salary
NEXT i
count = MAXITEMS

20 REM ---- report ----
PRINT "STAFF REPORT"
PRINT STRING$(40, "=")
CALL PrintTable
PRINT STRING$(40, "-")

total& = 0: top = 0
FOR i = 0 TO count - 1
  total& = total& + staff(i).salary
  IF staff(i).salary > staff(top).salary THEN top = i
NEXT
PRINT "Total payroll:"; total&
PRINT "Average:"; Average%(total&, count)
PRINT "Top earner: "; staff(top).ename

30 REM ---- departments, with SELECT CASE and GOSUB ----
FOR i = 0 TO count - 1
  d$ = staff(i).dept
  GOSUB ShowDept
NEXT

' ON ... GOTO picks a line number
choice = 2
ON choice GOTO 100, 200
100 PRINT "never printed": GOTO 300
200 PRINT "ON GOTO went to line 200"
300 PRINT "Bonus pool:"; 2 ^ 10; "  checksum: &H"; HEX$(total&)
END

ShowDept:
  SELECT CASE d$
    CASE "Sales": PRINT staff(i).ename; "-> commission plan"
    CASE "IT", "Ops": PRINT staff(i).ename; "-> on-call rota"
    CASE ELSE: PRINT staff(i).ename; "-> standard plan"
  END SELECT
  RETURN

StaffData:
DATA "Ada", "IT", 5200
DATA "Grace", "Ops", 4800
DATA "Linus", "Sales", 3900
DATA "Margaret", "IT", 6100
DATA "Ken", "Admin", 3500

SUB PrintTable
  ' i is this SUB's own variable; staff and count are SHARED
  FOR i = 0 TO count - 1
    PRINT staff(i).ename; TAB(15); staff(i).dept; TAB(25); staff(i).salary
  NEXT
END SUB

FUNCTION Average% (total%, count%)
  Average% = total% \ count%
END FUNCTION
