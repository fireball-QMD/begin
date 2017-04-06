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
! Ohio University - Dave Drabold

!
! RESTRICTED RIGHTS LEGEND
! Use, duplication, or disclosure of this software and its documentation
! by the Government is subject to restrictions as set forth in
! subdivision
! { (b) (3) (ii) } of the Rights in Technical Data and Computer Software
! clause at 52.227-7013.

! begin.f90
! Program Description
! ===========================================================================
!      Driver to create wavefunctions (excited pseudoatomic orbitals) and
! neutral atom potentials.
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
        program begin
        use precision
        implicit none

! Local Parameters and Data Declaration
! ===========================================================================

! Local Variable Declaration and Description
! ===========================================================================
        real(kind=long) etotatom

        character(len=1) b

! Allocate Arrays
! ===========================================================================

! Procedure
! ===========================================================================
        b  = char(169)

        write (*,*) '  '
        write (*,*) '  '
        write (*,*) ' *--------------------------------------------------* '
        write (*,*) '  Wellcom to the begin program of Fireball-2005 '
        write (*,*) '   Official Fireball-2005 Package Jan. 10, 2005 '
        write (*,*) '  '

! Calculate the wavefunctions
        call rcatms (etotatom)

! Calculate the (non)-neutral atom potentials
!        call vnn (etotatom)

        write (*,*) '  '
        write (*,*) '  '
        write (*,*) ' *--------------------------------------------------* '
        write (*,*) ' |             THANK YOU FOR COMING !!!!            | '
        write (*,*) ' |                PLEASE COME AGAIN                 | '
        write (*,*) ' *--------------------------------------------------* '


! Deallocate Arrays
! ===========================================================================

! Format Statements
! ===========================================================================

        stop
        end
