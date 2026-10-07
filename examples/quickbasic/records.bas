' RANDOM and BINARY files, QuickBASIC style: compile it with --qbs
'   bin/kayte --qbs --native examples/quickbasic/records.bas -o records && ./records
' Keeps fixed-size account records in accounts.dat (the same bytes
' QuickBASIC 4.5 would write), updates one in place, and dumps the first
' record's bytes through a BINARY file.

TYPE Account
  id AS INTEGER
  owner AS STRING * 16
  balance AS DOUBLE
  opened AS LONG
END TYPE

DIM acct AS Account
file$ = "accounts.dat"
recLen = LEN(acct)                   ' 30 bytes: 2 + 16 + 8 + 4
PRINT "Record length:"; recLen; "bytes"

' ---- create: one record per PUT, record numbers 1, 2, 3 ...
OPEN file$ FOR RANDOM AS #1 LEN = recLen
FOR i = 1 TO 4
  READ owner$, balance, year
  acct.id = 100 + i
  acct.owner = owner$
  acct.balance = balance
  acct.opened = year
  PUT #1, i, acct
NEXT
PRINT "Wrote"; LOF(1) \ recLen; "records ("; LOF(1); "bytes)"

' ---- update record 3 in place: GET it, change it, PUT it back
GET #1, 3, acct
acct.balance = acct.balance + 250.75
PUT #1, 3, acct
CLOSE #1

' ---- list them all, reading sequentially (no record number: the next one)
OPEN file$ FOR RANDOM AS #1 LEN = recLen
PRINT
PRINT " ID  Owner             Balance     Since"
DO UNTIL EOF(1)
  GET #1, , acct
  PRINT USING " ### \              \ ##,###.##     ####"; acct.id; acct.owner; acct.balance; acct.opened
LOOP
PRINT "(read up to record"; LOC(1); ")"
CLOSE #1

' ---- the raw bytes of record 1, through a BINARY file
OPEN file$ FOR BINARY AS #2
bytes$ = INPUT$(recLen, #2)
CLOSE #2
PRINT
PRINT "Record 1 as bytes:"
FOR i = 1 TO LEN(bytes$)
  PRINT RIGHT$("0" + HEX$(ASC(MID$(bytes$, i, 1))), 2); " ";
  IF i MOD 15 = 0 THEN PRINT
NEXT
PRINT "id from its first 2 bytes (CVI):"; CVI(LEFT$(bytes$, 2))
PRINT "balance from bytes 19-26 (CVD):"; CVD(MID$(bytes$, 19, 8))

KILL file$
END

DATA "Ada Lovelace", 1520.5, 1843
DATA "Grace Hopper", 980, 1952
DATA "Alan Turing", 15.25, 1936
DATA "Margaret Hamilton", 22000, 1969
