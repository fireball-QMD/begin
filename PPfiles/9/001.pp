!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#info ../tools/psgen.com ncpp 15:31:07 Jul 01 1999 gayathri           
                                                                      
fhi pseudopotential tool gncpp - version rev 051697                   
                                                                      
               chemical symbol  H                                     
                nuclear charge   1.00                                 
                  total charge   0.00                                 
         number of core states   0                                    
      number of valence states   1                                    
    exchange-correlation model   9    0.00  GGA X Becke C Lee/Yang/Par
      scalar-relativistic mode                                        
        parameters radial mesh   387    1.024700  0.625000E-02        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    9   0.00                            ! iexc , exmix
    1                                   ! nshells
    0                                   ! L values
    1.000000                            ! Zval
    800.0000                            ! alpha
    0.200000                            ! Rcut_PP
            5
   0.0000000E+00   8678.778    
   4.9999999E-03   11078.74    
   9.9999998E-03   11798.72    
   1.5000000E-02   12134.72    
   2.0000000E-02   12326.71    
 L=0 Npoint=    5
   0.0000000E+00  0.0000000E+00
   4.9999999E-03  0.0000000E+00
   9.9999998E-03  0.0000000E+00
   1.5000000E-02  0.0000000E+00
   2.0000000E-02  0.0000000E+00
 L=0 Npoint=    5 cl=     0.0000000
   0.0000000E+00  0.0000000E+00
   4.9999999E-03  0.0000000E+00
   9.9999998E-03  0.0000000E+00
   1.5000000E-02  0.0000000E+00
   2.0000000E-02  0.0000000E+00
#info ../tools/psgen.com ncpp 15:31:07 Jul 01 1999 gayathri

fhi pseudopotential tool gncpp - version rev 051697

               chemical symbol  H 
                nuclear charge   1.00
                  total charge   0.00
         number of core states   0
      number of valence states   1
    exchange-correlation model   9    0.00  GGA X Becke C Lee/Yang/Parr   
      scalar-relativistic mode
        parameters radial mesh   387    1.024700  0.625000E-02

       === all-electron atom ===

<        n     l      occupation  eigenvalue(eV)

<  1     1     0        1.0000           -6.5315

                                  (Hartree a.u.)
                  total energy          -0.46262
                kinetic energy           0.45154
                orbital energy          -0.24003
                coulomb energy          -0.94876
                hartree energy           0.29016
   exchange-correlation energy          -0.25555
           xc potential energy          -0.32311
          number of iterations                30   convergence  0.0E+00
            integrated density           1.00000
  ... 1st derivative test 1 =            1.00000
  ... 2nd derivative test 1 =            1.00000

 gncpp - all-electron atom done

    === HAMANN mode ===    h    

  l  n     radius:     node      peak       default core
x 0  1                0.006      1.000      0.400
x 1  2                0.006      0.006      0.400

          === pseudo atom ===

  l  type  rcore       rmatch          eigenvalue(eV)      norm test   slope test
                                 all-electron     pseudo
  0  hamann                   0.3956628   2.0792334  -6.5314730  -6.5314731   1.0000000   1.0000036
  1  hamann                   0.3956628   0.9758980  -6.5314730  -6.5314730   1.0000000   1.0000000


                                  (Hartree a.u.)
                  total energy          -0.46253
                kinetic energy           0.44698
              potential energy          -0.94438
                hartree energy           0.29008
                     xc energy          -0.25521
    integrated valence density           1.00000
   ... 1st derivative test 1 =           1.00000
   ... 2nd derivative test 1 =           1.00000

 gncpp - done for input @

@  1.00  0  1  90.00E+00 : z nc nv iexc rnlc
@     1  0   1.00        : n l f
@ 1  h                   : ltmx spptype
