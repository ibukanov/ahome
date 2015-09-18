#!/bin/sh

dir=$(dirname "$0")

. "$dir/common.sh"

# Arbitartry values

export mu_as_x_and_y='yes'

export mu_re='300'
export mu_im='0'
#Interval in GEV

#export tan_beta=''
export tan_beta='1'

export Lambda_tilda_mass='300' # Interval in GEV


export scalar_neutrino_mass='100 1000 25'
export scalar_neutrino_width='10'

#Beam parameters

export com_energy='200 2000 25' # Value interval in GEV


export total_cross_section='yes' # calculate


export scatering_angle='0' # degree interval


"$dir/../../build/prog"


IN="$mass_out"
OUT="$dir/../../build/mass.mu_re-mu_im.eps"


gnuplot <<__END__
set terminal postscript eps enhanced dashed "Helvetica" 15
set output '$OUT'
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
set samples 20, 20
set isosamples 21, 21
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
set rrange [0 : 10]
set trange [-5 : 5]
set urange [-5 : 5]
set vrange [-5 : 5]
set xrange [0 : 400]
set yrange [0 : 6.28]
set zrange [6.63542 : 310.303]
set autoscale r
set autoscale t
set autoscale xy
set autoscale z
set zero 1e-08
set view 140, 315, 1, 1
set label 1 "(a)" at 0,5,450 left 
set xlabel "|{/Symbol m}|, GeV" 
set ylabel "Arg({/Symbol m})" 2,0
set zlabel "m_{/Symbol c }, GeV" 0,1 
splot '$IN' using 2:3:5 title "m_{/Symbol c} , GeV"
__END__

cleanup
