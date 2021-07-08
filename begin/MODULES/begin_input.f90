         module begin_input 
          use precision 
   
          integer, parameter :: ioption =  3
          integer, parameter :: nexcite = 1
          integer, parameter :: nssh = 2
   
          integer, parameter :: nznuc =  6
          integer, parameter :: nzval =  4
          integer, parameter :: nzval_ion =  0
   
          integer, dimension (nssh), parameter ::                           &
     &     lam = (/ 0, 1/)
   
          real(kind=long), dimension (nssh), parameter ::                   &
     &     a0 = (/ 2.0000, 1.0000/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     rcutoff = (/ 4.000, 4.500/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     rcut_ion = (/ 4.000, 4.500/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     xocc = (/ 2.00, 2.00/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     xocc_ion = (/ 0.00, 0.00/)
   
          character(len=10), parameter :: atomname = 'Carbon    '
          character(len=8), parameter :: ppfile = '006.pp  '
          character(len=8), parameter :: ppionfile = '006++.pp'
   
          character(len=11), dimension (0:nssh), parameter ::               &
     &     filename_na = (/'006_450.na0',                                   &
     &                     '006_400.na1',                                   &
     &                     '006_450.na2'/)
          character(len=12), dimension (nssh), parameter ::                 &
     &     filename_ena = (/'006_400.ena1',                                 &
     &                      '006_450.ena2'/)
          character(len=11), dimension (nssh), parameter ::                 &
     &     filename_wf = (/'006_400.wf1',                                   &
     &                     '006_450.wf2'/)
          character(len=12), dimension (nssh), parameter ::                 &
     &     filename_ewf = (/'006_400.ewf1',                                 &
     &                      '006_450.ewf2'/)
          integer, parameter :: ioptim =  0
          real(kind=long), dimension (2*nssh), parameter ::                   &
     &     Vo = (/    0.0000,    0.0000,    0.0000,    0.0000/)
          real(kind=long), dimension (2*nssh), parameter ::                   &
     &     r0 = (/ 0.00, 0.00, 0.00, 0.00/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     cmix = (/ 1.00, 1.00/)
   
         end module 
