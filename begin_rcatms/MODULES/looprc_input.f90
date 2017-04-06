         module looprc_input
          use precision

          integer :: ioption
          integer :: nssh

          integer :: nznuc
          integer :: nzval

          integer, dimension (:), allocatable :: lam

          real(kind=long) :: rc_min
          real(kind=long) :: rc_max

          real(kind=long), dimension (:), allocatable :: a0
          real(kind=long), dimension (:), allocatable :: eigenvalue
          real(kind=long), dimension (:), allocatable :: rcutoff
          real(kind=long), dimension (:), allocatable :: xocc

          character(len=10) :: element
          character(len=8) :: ppfile
         end module
