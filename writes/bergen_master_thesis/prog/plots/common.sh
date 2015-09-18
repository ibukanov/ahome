# Known constants from Phys. Rev. D V50 1173-1826

export electron_mass='0.51099906(15) MEV'

export alpha_em_inverce='137.0359895(61)'

export sin_in2_thetaW='0.2312(3)'


export Z0_mass='91.187(7) GEV'
export Z0_width='2.490(7) GEV'
export W_mass='80.22(26) GEV'
export W_width='2.08(7)  GEV'

#Conversion constant GEV^{-2} -> pbarn
export cross_normalization='0.38937966e9'

tmp_dir="$dir/../../tmp" 
export total_cross_section_out="$tmp_dir/tcs.txt"
export mass_out="$tmp_dir/mass.txt"


cleanup() {
	rm -f "$total_cross_section_out" "$mass_out"
}
