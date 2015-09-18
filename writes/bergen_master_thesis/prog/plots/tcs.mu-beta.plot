set terminal postscript eps enhanced dashed "Helvetica" 15
set output '../fig/tcs.mu-beta.eps'
set noclip points
set clip one
set noclip two
set border
set boxwidth
set dummy u,v
set format x "%g"
set format y "%g"
set format z "%g"
set nogrid
set key
set nolabel
set noarrow
set nologscale
set offsets 0, 0, 0, 0
set nopolar
set angles radians
set parametric
set samples 100, 100
set isosamples 10, 10
set surface
set contour base
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
set rrange [-0 : 10]
set trange [-5 : 5]
set urange [-5 : 5]
set vrange [-5 : 5]
set xlabel "" 0,0
set xrange [-400 : 400]
set ylabel "" 0,0
set yrange [0.1 : 20]
set zlabel "" 0,0
set zrange [0 : 1.09746]
set autoscale r
set autoscale t
set autoscale xy
set autoscale z
set zero 1e-08
set view 30, 335, 1, 1
set xlabel "{/Symbol m}, GeV" 
set ylabel "tan_{}{/Symbol b}" 2,0
set zlabel "{/Symbol s}_{tot}, pb" 0,3.5
splot 'tcs.txt' using 2:4:6 title "{/Symbol s}_{tot} , pb"
