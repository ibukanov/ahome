#!/bin/sh
IN="../../build/tcs.txt"
OUT="../../build/x_channels.eps"
gnuplot <<__END__
set terminal postscript eps enhanced dashed "Helvetica" 15
set output '$OUT'
set noclip points
set clip one
set noclip two
set border
set boxwidth
set dummy t,y
set format x "%g"
set format y "%g"
set format z "%g"
set nogrid
set nolabel
set noarrow
set nologscale
set logscale y 10
set offsets 50, 0, 0, 0
set nopolar
set angles radians
set parametric
set view 50, 150, 1, 1
set samples 100, 100
set isosamples 10, 10
set surface
set nocontour
set clabel
set nohidden3d
set cntrparam order 4
set cntrparam linear
set cntrparam levels auto 5
set cntrparam points 5
set size 1,1
set data style lines
set function style lines
set xzeroaxis
set yzeroaxis
set tics in
set ticslevel 0.5
set xtics
set ytics
set ztics
set title "" 0,0
set notime
set rrange [0 : 10]
set trange [-5 : 5]
set urange [-5 : 5]
set vrange [-5 : 5]
set xlabel "E_{c.m.}, Gev" 0,0
set xrange [118.367 : 1000]
set ylabel '{/Symbol s}, pb' 0,0
set yrange [0.0472884 : 16.3122]
set zlabel "" 0,0
set zrange [0 : 5.75104]
set autoscale r
set autoscale t
set autoscale xy
set autoscale z
set zero 1e-08
lambda = 1
mu_r = 2
mu_i = 3
tan_beta = 4
set key 700,0.06
plot '$IN' using 5:6 title '{/Symbol s}_{tot}', '$IN' using 5:7 title '{/Symbol g}',  '$IN' using 5:8 title 'Z', '$IN'  using 5:9 title '{/Symbol n}'
__END__
