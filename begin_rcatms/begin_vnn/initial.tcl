# arrays that define all the parameters for each atom (defaults are read 
# in the file periodictable.input ) 

global atom
global full_name

# "bool" variables that are 1 if the orbital is occupied
global s
global p
global d
global f

# the actual occupation numbers
global s_occup  
global p_occup  
global d_occup 
global f_occup

# those are the cuttoffs of each orbital
global s_cutoff
global p_cutoff 
global d_cutoff
global f_cutoff

# "bool" for inclusion of excited states 1 - include, 0 - don't
global excited

# the number for the Exchange correlation functional (1-12)
global ExCorr


# load the default parameters from the file periodictable.input
set input [open periodictable.input r]
for {set i 1} {$i < 9} {incr i} { set junk1 [gets $input junk2]}
for {set i 1} {$i < 95} {incr i} {

    global s_cutoff
    global p_cutoff
    global d_cutoff
    global f_cutoff
    global s_occup
    global p_occup
    global d_occup
    global f_occup
    global full_name

    set junk1 [gets $input junk2]
    set junk1 [gets $input full_name($i)]
    set junk1 [gets $input junk2]

    set n_chars1 [gets $input cutoffs]
    set n_chars2 [gets $input occups]

    set nvalues [scan $cutoffs "%f %f %f %f" s_cutoff($i) p_cutoff($i) d_cutoff($i) f_cutoff($i)]
    set nvalues [scan $occups "%f %f %f %f" s_occup($i) p_occup($i) d_occup($i) f_occup($i)]

    set junk1 [gets $input junk2]
}
    


for {set i 1} {$i < 95} {incr i} {

    set excited($i) 0;
    set ExCorr($i) 3;

}


close $input


set atom(1) H
set atom(2) He
set atom(3) Li
set atom(4) Be
set atom(5) B
set atom(6) C
set atom(7) N
set atom(8) O
set atom(9) F
set atom(10) Ne
set atom(11) Na
set atom(12) Mg
set atom(13) Al
set atom(14) Si
set atom(15) P
set atom(16) S
set atom(17) Cl
set atom(18) Ar
set atom(19) K
set atom(20) Ca
set atom(21) Sc
set atom(22) Ti
set atom(23) V
set atom(24) Cr
set atom(25) Mn
set atom(26) Fe
set atom(27) Co
set atom(28) Ni
set atom(29) Cu
set atom(30) Zn
set atom(31) Ga
set atom(32) Ge
set atom(33) As
set atom(34) Se
set atom(35) Br
set atom(36) Kr
set atom(37) Rb
set atom(38) Sr
set atom(39) Y
set atom(40) Zr
set atom(41) Nb
set atom(42) Mo
set atom(43) Tc
set atom(44) Ru
set atom(45) Rh
set atom(46) Pd
set atom(47) Ag
set atom(48) Cd
set atom(49) In
set atom(50) Sn
set atom(51) Sb
set atom(52) Te
set atom(53) I
set atom(54) Xe
set atom(55) Cs
set atom(56) Ba
set atom(57) La

set atom(58) Ce
set atom(59) Pr
set atom(60) Nd
set atom(61) Pm
set atom(62) Sm
set atom(63) Eu
set atom(64) Gd
set atom(65) Tb
set atom(66) Dy
set atom(67) Ho
set atom(68) Er
set atom(69) Tm
set atom(70) Yb
set atom(71) Lu

set atom(72) Hf
set atom(73) Ta
set atom(74) W
set atom(75) Re
set atom(76) Os
set atom(77) Ir
set atom(78) Pt
set atom(79) Au 
set atom(80) Hg
set atom(81) Tl
set atom(82) Pb
set atom(83) Bi
set atom(84) Po
set atom(85) At
set atom(86) Rn
set atom(87) Fr
set atom(88) Ra
set atom(89) Ac

set atom(90) Th
set atom(91) Pa
set atom(92) U
set atom(93) Np
set atom(94) Pu


for {set i 1} {$i < 95} {incr i} {
    button .$i -text "\n $atom($i)" -height 3 -width 2 \
	    -font {Helvetica -14 bold} -command "configure_atom $i"
    label .$i.l -text $i -bg LightBlue -font {Helvetica -10 bold}
}

global name

. configure -background white
wm title . {Fireball Initialization}

frame .msg -relief raised -borderwidth 2
label .msg.msg -text " \n WELCOME TO BEGIN \n\n \
	Please configure the elements in your system \n\n" \
	-font {Helvetica -14 bold}
button .msg.done -text DONE -command exit

grid config .msg -column 3 -columnspan 8 -row 0 -rowspan 3 -sticky snew \
	-pady 25
pack .msg.msg
pack .msg.done


# 0th row
grid config .1 -column 0 -row 0
grid config .2 -column 17 -row 0

# 1st row
grid config .3 -column 0 -row 1
grid config .4 -column 1 -row 1

for {set i 5} {$i < 11} {incr i} {
    grid config .$i -column [expr 7 + $i] -row 1
}

# 2nd row
grid config .11 -column 0 -row 2
grid config .12 -column 1 -row 2

for {set i 13} {$i < 19} {incr i} {
    grid config .$i -column [expr $i-1] -row 2
}

# 3d row
for {set i 19} {$i < 37} {incr i} {
    grid config .$i -column [expr $i - 19] -row 3
}

# 4th row
for {set i 37} {$i < 55} {incr i} {
    grid config .$i -column [expr $i - 37] -row 4
}

# 5th row
for {set i 55} {$i < 58} {incr i} {
    grid config .$i -column [expr $i - 55] -row 5
}

for {set i 72} {$i < 87} {incr i} {
    grid config .$i -column [expr $i - 69] -row 5
}

# 6th row
for {set i 87} {$i < 90} {incr i} {
    grid config .$i -column [expr $i -87] -row 6
}


# Additional rows

for {set i 58} {$i < 72} {incr i} {
    grid config .$i -column [expr $i - 54] -row 7
}

for {set i 90} {$i < 95} {incr i} {
    grid config .$i -column [expr $i - 86] -row 8    
}

# Place the element numbers

for {set i 1} {$i < 95} {incr i} {
    place .$i.l -x 0 -y 0
}

source configure_atom.tcl

