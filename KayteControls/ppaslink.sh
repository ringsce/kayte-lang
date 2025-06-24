#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
OFS=$IFS
IFS="
"
/Library/Developer/CommandLineTools/usr/bin/ld     -weak_framework AppKit -weak_framework UserNotifications -no_order_file      -order_file /Users/pedro/Projects/kayte-lang/KayteControls/symbol_order.fpc -multiply_defined suppress -L. -o /Users/pedro/Projects/kayte-lang/KayteControls/KayteInterpreter `cat /Users/pedro/Projects/kayte-lang/KayteControls/link22856.res` -filelist /Users/pedro/Projects/kayte-lang/KayteControls/linkfiles22856.res
if [ $? != 0 ]; then DoExitLink ; fi
IFS=$OFS
