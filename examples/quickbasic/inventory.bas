' Files and PRINT USING, QuickBASIC style: compile it with --qbs
'   bin/kayte --qbs --native examples/quickbasic/inventory.bas -o inventory && ./inventory
' It writes inventory.dat in the current directory, reads it back and
' prints a formatted report (also saved to inventory.txt).

file$ = "inventory.dat"

' ---- write the data file: WRITE # quotes strings, separates with commas
OPEN file$ FOR OUTPUT AS #1
FOR i = 1 TO 5
  READ item$, qty, price
  WRITE #1, item$, qty, price
NEXT
CLOSE #1

' ---- add one more record later
OPEN file$ FOR APPEND AS #1
WRITE #1, "Stapler, heavy duty", 3, 24.5
CLOSE #1

' ---- read it back and print a report, to the screen and to a file
f = FREEFILE
OPEN file$ FOR INPUT AS #f
OPEN "inventory.txt" FOR OUTPUT AS #9
PRINT "Data file: "; file$; " ("; LOF(f); "bytes)"
PRINT
head$ = "\                  \  #####   ######.##   #######.##"
PRINT "Item                   Qty       Price        Value"
PRINT STRING$(52, "-")
DO UNTIL EOF(f)
  INPUT #f, item$, qty, price
  value = qty * price
  total = total + value
  lines = lines + 1
  PRINT USING head$; item$; qty; price; value
  PRINT #9, USING head$; item$; qty; price; value
LOOP
CLOSE #f
PRINT STRING$(52, "-")
PRINT USING "Total of ## items:                    $$#,######.##"; lines; total
PRINT #9, USING "TOTAL $$#,######.##"; total
CLOSE

PRINT
PRINT "inventory.txt:"
OPEN "inventory.txt" FOR INPUT AS #1
DO UNTIL EOF(1)
  LINE INPUT #1, l$
  PRINT "  | "; l$
LOOP
CLOSE #1

KILL file$
KILL "inventory.txt"
PRINT "(both files deleted again)"
END

DATA "Pencils", 120, 0.35
DATA "Notebook A4", 40, 2.95
DATA "Ink cartridge", 8, 31.5
DATA "Desk lamp", 2, 49.99
DATA "Paper (500 sheets)", 25, 6.2
