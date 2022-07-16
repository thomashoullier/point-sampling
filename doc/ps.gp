#!/bin/gnuplot
set terminal png truecolor size 1000,1000
set output "ps.png"

set datafile separator ','
unset key

plot 'ps.csv' using 1:2 with points
