         module begin_input 
          use precision 
   
          integer, parameter :: ioption =  9
          integer, parameter :: nexcite = 0
          integer, parameter :: nssh = 3
   
          integer, parameter :: nznuc = 15
          integer, parameter :: nzval =  4
          integer, parameter :: nzval_ion =  0
   
          integer, dimension (nssh), parameter ::                           &
     &     lam = (/ 0, 1, 2/)
   
          real(kind=long), dimension (nssh), parameter ::                   &
     &     a0 = (/ 2.0000, 1.0000, 0.8000/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     rcutoff = (/ 6.000, 6.000, 6.000/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     rcut_ion = (/ 6.000, 6.000, 6.000/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     xocc = (/ 2.00, 2.00, 0.00/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     xocc_ion = (/ 0.00, 0.00, 0.00/)
   
          character(len=10), parameter :: atomname = 'Phosphorou'
          character(len=8), parameter :: ppfile = '015.pp  '
          character(len=8), parameter :: ppionfile = '015++.pp'
   
          character(len=11), dimension (0:nssh), parameter ::               &
     &     filename_na = (/'015_600.na0',                                   &
     &                     '015_600.na1',                                   &
     &                     '015_600.na2',                                   &
     &                     '015_600.na3'/)
          character(len=12), dimension (nssh), parameter ::                 &
     &     filename_ena = (/'015_600.ena1',                                 &
     &                      '015_600.ena2',                                 &
     &                      '015_600.ena3'/)
          character(len=11), dimension (nssh), parameter ::                 &
     &     filename_wf = (/'015_600.wf1',                                   &
     &                     '015_600.wf2',                                   &
     &                     '015_600.wf3'/)
          character(len=12), dimension (nssh), parameter ::                 &
     &     filename_ewf = (/'015_600.ewf1',                                 &
     &                      '015_600.ewf2',                                 &
     &                      '015_600.ewf3'/)
          integer, parameter :: ioptim =  1
          real(kind=long), dimension (nssh), parameter ::                   &
     &     Vo = (/    0.0000,    0.0000,    0.0000/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     r0 = (/ 0.00, 0.00, 0.00/)
          real(kind=long), dimension (nssh), parameter ::                   &
     &     cmix = (/ 1.00, 1.00, 1.00/)
   
         end module 
