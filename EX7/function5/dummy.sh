#!/bin/bash

for i in {1..250}
do
a=$((20*i))
./main << EOF
$a 
0
EOF
#echo $a
done
