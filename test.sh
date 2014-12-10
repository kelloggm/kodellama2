#!/bin/bash

# @ Owner Scelerus

# Exercises Kodellama by running all the programs in the sampleprograms directory and comparing their output to an oracle

FAIL=0
FAILLIST=""
echo ""

for f in sampleprograms/*.kdl
do
echo "Testing $f:"
./Kodellama < $f > $f-out
diff -wbBE $f-out $f-golden > $f-diff
FN=$f-diff
python check_diffs.py < $FN
echo ""
done