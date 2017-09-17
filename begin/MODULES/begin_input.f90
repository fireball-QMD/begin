         module begin_input 
          use precision 
   
          integer, parameter :: ioption =  3
          integer, parameter :: nexcite = 0
          integer, parameter :: nssh = 2
   
          integer, parameter :: nznuc = 14
          integer, parameter :: nzval =  4
          integer, parameter :: nzval_ion =  0
   
          integer, dimension (nssh), parameter ::                           &
     &     lam = (/ 0, 1/)
   
          real(kind=long), dimension (nssh), parameter ::                   &
     &     a0 = (/ 2.0000, 1.0000/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     rcutoff = (/ 4.800, 5.400/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     rcut_ion = (/ 4.800, 5.400/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     xocc = (/ 2.00, 2.00/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     xocc_ion = (/ 0.00, 0.00/)
   
          character(len=10), parameter :: atomname = 'Silicon   '
          character(len=8), parameter :: ppfile = '014.pp  '
          character(len=8), parameter :: ppionfile = '014++.pp'
   
          character(len=11), dimension (0:nssh), parameter ::               &
     &     filename_na = (/'014_540.na0',                                   &
     &                     '014_480.na1',                                   &
     &                     '014_540.na2'/)
          character(len=12), dimension (nssh), parameter ::                 &
     &     filename_ena = (/'014_480.ena1',                                 &
     &                      '014_540.ena2'/)
          character(len=11), dimension (nssh), parameter ::                 &
     &     filename_wf = (/'014_480.wf1',                                   &
     &                     '014_540.wf2'/)
          character(len=12), dimension (nssh), parameter ::                 &
     &     filename_ewf = (/'014_480.ewf1',                                 &
     &                      '014_540.ewf2'/)
          integer, parameter :: ioptim =  0
          real(kind=long), dimension (nssh), parameter ::                   &
     &     Vo = (/    0.0000,    0.0000/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     r0 = (/ 0.00, 0.00/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     cmix = (/ 1.00, 1.00/)
   
         end module 
