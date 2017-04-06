! copyright info:
!
!                             @Copyright 2001
!                            Fireball Committee
! Brigham Young University - James P. Lewis, Chair
! Arizona State University - Otto F. Sankey
! University of Regensburg - Juergen Fritsch
! Universidad de Madrid - Jose Ortega

! Other contributors, past and present:
! Auburn University - Jian Jun Dong
! Arizona State University - Gary B. Adams
! Arizona State University - Kevin Schmidt
! Arizona State University - John Tomfohr
! Lawrence Livermore National Laboratory - Kurt Glaesemann
! Motorola, Physical Sciences Research Labs - Alex Demkov
! Motorola, Physical Sciences Research Labs - Jun Wang
! Ohio University - Dave Drabold

!
! RESTRICTED RIGHTS LEGEND
! Use, duplication, or disclosure of this software and its documentation
! by the Government is subject to restrictions as set forth in subdivision
! { (b) (3) (ii) } of the Rights in Technical Data and Computer Software
! clause at 52.227-7013.

! glean.f90
! Program Description
! ===========================================================================
!       Read the Z.eig datafile, check deltas of the eigenvalues to be
! near the desired cutoff range.
!
! ===========================================================================
! Code rewritten by:
! James P. Lewis
! Department of Physics and Astronomy
! Brigham Young University
! N233 ESC P.O. Box 24658
! Provo, UT 841184602-4658
! FAX 801-422-2265
! Office telephone 801-422-7444
! ===========================================================================
!
! Program Declaration
! ===========================================================================
        program glean
        use precision
        implicit none

! Local Parameters and Data Declaration
! ===========================================================================
        real(kind=long) :: maximum
        real(kind=long) :: minimum

        real(kind=long), dimension (3) :: delta
        real(kind=long), dimension (3) :: eigenvalue
        real(kind=long), dimension (3) :: rcutoff

        character(len=7) eigfile

! Local Variable Declaration and Description
! ===========================================================================

! Allocate Arrays
! ===========================================================================

! Procedure
! ===========================================================================
        write (*,*) '  '
        write (*,*) ' Enter the name of the eigenvalue file: '
        read (*,*) eigfile

        write (*,*) '  '
        write (*,*) ' Enter the maximum delta desired: '
        read (*,*) maximum
        write (*,*) ' maximum = ', maximum

        write (*,*) '  '
        write (*,*) ' Enter the minimum delta desired: '
        read (*,*) minimum
        write (*,*) ' minimum = ', minimum

        open (unit = 14, file = eigfile, status = 'old')
        read (14,*)
        do
         read (14,*) rcutoff(1), eigenvalue(1), delta(1),                    &
     &               rcutoff(2), eigenvalue(2), delta(2)
!    &               rcutoff(2), eigenvalue(2), delta(2),                    &
!    &               rcutoff(3), eigenvalue(3), delta(3)

         if (delta(1) .gt. minimum .and. delta(1) .lt. maximum .and.         &
     &       delta(2) .gt. minimum .and. delta(2) .lt. maximum) then
!    &       delta(2) .gt. minimum .and. delta(2) .lt. maximum .and.         &
!    &       delta(3) .gt. minimum .and. delta(3) .lt. maximum) then
          write (*,102) rcutoff(1), eigenvalue(1), delta(1),                 &
     &                  rcutoff(2), eigenvalue(2), delta(2),                 &
     &                  rcutoff(3), eigenvalue(3), delta(3)

         end if
        end do

! Deallocate Arrays
! ===========================================================================

! Format Statements
! ===========================================================================
102     format (2x, 3(f6.2, 2(4x, f9.5)))

        end
