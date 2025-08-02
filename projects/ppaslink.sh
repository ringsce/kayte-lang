#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
OFS=$IFS
IFS="
"
/Library/Developer/CommandLineTools/usr/bin/ld     -weak_framework AppKit -weak_framework UserNotifications      -order_file /Users/pedro/Projects/kayte-lang/projects/symbol_order.fpc -multiply_defined suppress -L. -o /Users/pedro/Projects/kayte-lang/projects/vb6interpreter `cat /Users/pedro/Projects/kayte-lang/projects/link51267.res` -filelist /Users/pedro/Projects/kayte-lang/projects/linkfiles51267.res
if [ $? != 0 ]; then DoExitLink ; fi
IFS=$OFS
