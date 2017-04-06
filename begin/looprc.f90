! copyright info:
!
!                             @Copyright 2001
!                           Fireball Committee
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
! Ohio State University - Dave Drabold

!
! RESTRICTED RIGHTS LEGEND
! Use, duplication, or disclosure of this software and its documentation
! by the Government is subject to restrictions as set forth in
! subdivision
! { (b) (3) (ii) } of the Rights in Technical Data and Computer Software
! clause at 52.227-7013.

! looprc.f90
! Program Description
! ===========================================================================
!       This routine loops through rmin, rmax and calls rcatms for different
! cutoffs.  A data file is generated which includes the eigenvalues for each
! shell at each given cutoff.
!
! ===========================================================================
! Code written by:
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
        program looprc
        use looprc_input
        use precision
        implicit none

! Argument Declaration and Description
! ===========================================================================

! Local Parameters and Data Declaration
! ===========================================================================

! Local Variable Declaration and Description
! ===========================================================================
        integer ione
        integer ipoint
        integer issh
        integer iten
        integer jpoint
        integer kpoint
        integer num_points

        real(kind=long) drrc

        real(kind=long), dimension (:), allocatable :: delta
        real(kind=long), dimension (:), allocatable :: e_infinity

        character(len=7) eigfile
        character(len=3) symbol

        character(len=1), dimension (0:9) :: z

! Allocate Arrays
! ===========================================================================

! Procedure
! ===========================================================================
! Initialize some symbols
        do ipoint = 0, 9
         z(ipoint) = char(48+ipoint)
        end do

! Call the initializer for looprc.  This will set up the type of atom that
! is being looped over, occupation numbers, etc.
        call initial_looprc

! Now set up the name of the file which will contain the eigenvalues.
! The variable symbol is a three character string which has the atomic number.
! For example, 001 is hydrogen, 008 is oxygen, and 094 is plutonium.
        iten = nznuc/10
        if (nznuc .lt. 10) symbol = z(0)//z(0)//z(nznuc)
        if (nznuc .ge. 10 .and. nznuc .le. 99) then
         iten = nznuc/10
         ione = nznuc - 10*iten
         symbol = z(0)//z(iten)//z(ione)
        end if

! Evaluate the names of the files
        ppfile(1:3) = symbol
        ppfile(4:6) = '.pp'
        ppfile(7:8) = '  '
        eigfile(1:3) = symbol
        eigfile(4:7) = '.eig'

! Open the file
        open (unit = 31, file = eigfile, status = 'unknown')

! Now perform a loop for different cutoff radii. Loop from rc_min to rc_max.
! Allocate the eigenvalue array.
        allocate (delta(nssh))
        allocate (eigenvalue(nssh))
        allocate (e_infinity(nssh))

        drrc = 0.10d0
        num_points = (rc_max - rc_min)/drrc

! First calculate the 'infinite' cutoff case.
        rcutoff = 12.0d0
        write (*,*) '  '
        write (*,*) '  '
        write (*,*) ' rcutoff = ', rcutoff
        call rcatms_looprc

! Write out the eigenvalues to the output file
        e_infinity = eigenvalue
        write (31,100) rcutoff(1), e_infinity

! Now loop through all other possible cutoffs.
        if (nssh .eq. 1) then
         rcutoff(nssh) = rc_min
         do ipoint = 1, num_points + 1

! Call rcatms for the given cutoff.
          write (*,*) '  '
          write (*,*) '  '
          write (*,*) ' rcutoff = ', rcutoff
          call rcatms_looprc
          delta = eigenvalue - e_infinity
          write (31,100) rcutoff(1), eigenvalue(1), delta(1)
          rcutoff(nssh) = rcutoff(nssh) + drrc
         end do

        else if (nssh .eq. 2) then
         rcutoff(nssh) = rc_min
         do ipoint = 1, num_points + 1
          rcutoff(1) = rcutoff(nssh) - 1.0d0
          do jpoint = 1, 20

! Call rcatms for the given cutoff.
           write (*,*) '  '
           write (*,*) '  '
           write (*,*) ' rcutoff = ', rcutoff
           call rcatms_looprc
           delta = eigenvalue - e_infinity
           write (31,101) rcutoff(1), eigenvalue(1), delta(1),           &
                          rcutoff(2), eigenvalue(2), delta(2)
           rcutoff(1) = rcutoff(1) + drrc
          end do
          rcutoff(nssh) = rcutoff(nssh) + drrc
         end do

        else if (nssh .eq. 3) then
         rcutoff(2) = rc_min
         do ipoint = 1, num_points + 1
          rcutoff(1) = rcutoff(2) - 1.0d0
          do jpoint = 1, 20
           rcutoff(3) = rcutoff(1) - 1.0d0
           do kpoint = 1, 20

! Call rcatms for the given cutoff.
            write (*,*) '  '
            write (*,*) '  '
            write (*,*) ' rcutoff = ', rcutoff
            call rcatms_looprc
            delta = eigenvalue - e_infinity
            write (31,102) rcutoff(1), eigenvalue(1), delta(1),          &
                           rcutoff(2), eigenvalue(2), delta(2),          &
                           rcutoff(3), eigenvalue(3), delta(3)
            rcutoff(3) = rcutoff(3) + drrc
           end do
           rcutoff(1) = rcutoff(1) + drrc
          end do
          rcutoff(2) = rcutoff(2) + drrc
         end do
        end if

! Deallocate Arrays
! ===========================================================================
! These arrays were allocated within initial_looprc.f
        deallocate (a0)
        deallocate (lam)
        deallocate (rcutoff)
        deallocate (xocc)

        deallocate (delta)
        deallocate (eigenvalue)
        deallocate (e_infinity)

! Format Statements
! ===========================================================================
100     format (2x, f6.2, 3(4x, f9.5))
101     format (2x, 2(f6.2, 2(4x, f9.5)))
102     format (2x, 3(f6.2, 2(4x, f9.5)))

        close (unit = 31)
        end
