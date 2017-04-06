         module begin_input 
          use precision 
   
          integer, parameter :: ioption =  3
          integer, parameter :: nexcite = 1
          integer, parameter :: nssh = 1
   
          integer, parameter :: nznuc =  1
          integer, parameter :: nzval =  1
          integer, parameter :: nzval_ion =  0
   
          integer, dimension (nssh), parameter ::                           &
     &     lam = (/ 0/)
   
          real(kind=long), dimension (nssh), parameter ::                   &
     &     a0 = (/ 2.0000/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     rcutoff = (/ 3.800/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     rcut_ion = (/ 3.800/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     xocc = (/ 1.00/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     xocc_ion = (/ 0.00/)
   
          character(len=10), parameter :: atomname = 'Hydrogen  '
          character(len=8), parameter :: ppfile = '001.pp  '
          character(len=8), parameter :: ppionfile = '001++.pp'
   
          character(len=11), dimension (0:nssh), parameter ::               &
     &     filename_na = (/'001_380.na0',                                   &
     &                     '001_380.na1'/)
          character(len=12), dimension (nssh), parameter ::                 &
     &     filename_ena = (/'001_380.ena1'/)
          character(len=11), dimension (nssh), parameter ::                 &
     &     filename_wf = (/'001_380.wf1'/)
          character(len=12), dimension (nssh), parameter ::                 &
     &     filename_ewf = (/'001_380.ewf1'/)
          integer, parameter :: ioptim =  0
          real(kind=long), dimension (2*nssh), parameter ::                   &
     &     Vo = (/    0.0000,    0.0000/)
          real(kind=long), dimension (2*nssh), parameter ::                   &
     &     r0 = (/ 0.00, 0.00/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     cmix = (/ 1.00/)
   
         end module 
