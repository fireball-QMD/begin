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

! initial_looprc.f90
! Program Description
! ===========================================================================
!       This program prepares the input located within the global input file
! initial_looprc.f file for running looprc.x This automates the procedure
! considerably. Much of the necessary data is in periodictable.input, which
! this program (and others) reads.
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
        subroutine initial_looprc
        use looprc_input
        use precision
        implicit none

! Local Parameters and Data Declaration
! ===========================================================================
        integer, parameter :: max_z = 110
        integer, parameter :: nssh_max = 4

! Local Variable Declaration and Description
! ===========================================================================
        integer iline
        integer ipoint
        integer issh
        integer iz
        integer nznucin

        real(kind=long) xmass

        real(kind=long), dimension (nssh_max) :: rcutoffin
        real(kind=long), dimension (nssh_max) :: xnoccin

        character(len=1) answer
        character(len=50) bar
        character(len=1) copyright
        character(len=2) symbolin

        logical sshell
        logical pshell
        logical dshell
        logical fshell

! Allocate Arrays
! ===========================================================================

! Procedure
! ===========================================================================
! Initialize some symbols
        copyright = char(169)

! Set up the bar (horizontal line for optics of output file)
        do ipoint = 1, 50
         bar(ipoint:ipoint) = ' = '
        end do
        bar(25:36) = 'Fireball2000'

        write (*,100) bar
        write (*,*) '          *------------------------------------* '
        write (*,*) '          |     THIS CODE IS PROPRIETORY,      | '
        write (*,*) '          |      SEE COPYRIGHT INFORMATION!    | '
        write (*,*) '          *------------------------------------* '
        write (*,*) '  '
        write (*,*) '             Usable only with permission from '
        write (*,*) '           the Fireball2000 executive committee. '
        write (*,*) '         This program is NOT, under ANY circumstances,'
        write (*,*) '          to be transfered to an unauthorized user! '
        write (*,*) '  '
        write (*,100) bar

        write (*,*) '  '
        write (*,*) '    *-------------------------------------------------* '
        write (*,*) '    |                                                 | '
        write (*,*) '    |                    Welcome.                     | '
        write (*,*) '    |       The Fireball2000 team welcomes you.       | '
        write (*,*) '    |                                                 | '
        write (*,*) '    *-------------------------------------------------* '
        write (*,*) ' '
        write (*,*) ' This program helps you to initialize the "begin" part '
        write (*,*) ' of the Fireball2000', copyright, ' package. '
        write (*,*) '  '
        write (*,*) ' You will be asked 4 questions. Hit return for defaults. '
        write (*,*) '  '
        write (*,*) ' Ready. '
        write (*,*) '           Set. '
        write (*,*) '                    Go. '
        write (*,*) '  '
        write (*,*) '  '


        write (*,*) ' =================== Question No. 1 =================== '
        write (*,*) '  '
        write (*,*) ' Which atom would you like to begin? Your choice:'
        write (*,*) '  '

! write out the periodic table.
        call periodensystem

        write (*,*) '  '
        write (*,*) ' ===> Input desired atomic number: '
        read (*,*) nznucin
        if (nznucin .lt. 0 .or. nznucin .gt. max_z) stop ' Wrong input!! '

! *****************************************************************************
! Now put default data in by reading periodictable.dat
        write (*,*) ' Begin reading main data in periodictable.input '
        open (unit = 50, file = 'periodictable.input', status = 'old')

! The first 7 lines are comment lines
        do iline = 1, 7
         read (50,*)
        end do

! Now begin reading real data
        do iz = 1, nznucin
         read (50,*)
         read (50,200) nznuc, symbolin
         if (iz .ne. nznuc) stop ' periodictable.input mixup. '
         read (50,201) element
         read (50,*) xmass

! We include rc for s(L=0), p(L=1), etc.
         read (50,*) rcutoffin
         read (50,*) xnoccin
        end do
        close (unit = 50)

        write (*,*) ' Your chosen element: ', element
        if (element .eq. 'XXXXXXXX') then
         write (*,*) ' This element not yet in the periodictable.input file. '
         write (*,*) ' Go add an entry to the periodictable.input file. It '
         write (*,*) ' will take you a few minutes. You must put in a default '
         write (*,*) ' value for rcutoff. Think about this a minute. We allow '
         write (*,*) ' rc(L=0), rc(L=1), ... to be different for each shell. '
         write (*,*) ' For simplicity, you might want to take them as the same.'
         stop
        end if

! *****************************************************************************
        write (*,*) '  '
        write (*,*) ' =================== Question No. 2 =================== '
        write (*,*) '  '

        write (*,*) ' We suggest the following electron configuration (This '
        write (*,*) ' can of course change depending on your system.) '
        write (*,*) '  '
        write (*,202) xnoccin


! We pick up a default by looking at the highest occupied angular momentum
        write (*,*) ' Which shells do you want (insert corresponding letters): '
        write (*,*) ' [s] - the s-shell '
        write (*,*) ' [p] - the p-shell '
        write (*,*) ' [d] - the d-shell '
        write (*,*) ' [f] - the f-shell '
        write (*,*) '  '
        write (*,*) ' Use the default? Y/N '
        read (*,203) answer

! Initialize everything to false first
        sshell = .false.
        pshell = .false.
        dshell = .false.
        fshell = .false.

        nssh = 0
        if (answer .eq. 'Y' .or. answer .eq. 'y' .or. answer .eq. ' ') then
         if (xnoccin(1) .ne. 0) then
          nssh = nssh + 1
          sshell = .true.
         end if
         if (xnoccin(2) .ne. 0) then
          nssh = nssh + 1
          pshell = .true.
         end if
         if (xnoccin(3) .ne. 0) then
          nssh = nssh + 1
          dshell = .true.
         end if
         if (xnoccin(4) .ne. 0) then
          nssh = nssh + 1
          fshell = .true.
         end if
         write (*,*) '  '
         write (*,*) ' OK: We USE the default: case nssh = ', nssh
         write (*,*) '  '
        else
         write (*,*) '  '
         write (*,*) ' OK: We change the defaults. '
         write (*,*) '  '
         do issh = 1, nssh_max
          write (*,*) ' Orbital number (issh) = ', issh
          write (*,*) ' Insert s, p, d, f, or c (if finished with selections) '
          read (*,203) answer
          if (answer .eq. 'c') exit
          if ((answer .eq. 's') .and. (sshell .eqv. .false.)) then
           sshell = .true.
           nssh = nssh + 1
          else if ((answer .eq. 'p') .and. (pshell .eqv. .false.)) then
           pshell = .true.
           nssh = nssh + 1
          else if ((answer .eq. 'd') .and. (dshell .eqv. .false.)) then
           dshell = .true.
           nssh = nssh + 1
          else if ((answer .eq. 'f') .and. (fshell .neqv. .false.)) then
           fshell = .true.
           nssh = nssh + 1
          end if
          write (*,*) '  '
         end do
        end if

        if (nssh .eq. 0) stop ' Defaults are all zero - not good. '

        if (nssh .eq. 1) write (*,*) ' You have chosen 1 shell '
        if (nssh .eq. 2) write (*,*) ' You have chosen 2 shells '
        if (nssh .eq. 3) write (*,*) ' You have chosen 3 shells '
        if (nssh .eq. 4) write (*,*) ' You have chosen 4 shells '
        if (sshell) write (*,*) ' You have chosen the s-shell '
        if (pshell) write (*,*) ' You have chosen the p-shell '
        if (dshell) write (*,*) ' You have chosen the d-shell '
        if (fshell) write (*,*) ' You have chosen the f-shell '
        write (*,*) '  '

! Once nssh is known allocate arrays
        allocate (a0(nssh))
        allocate (lam(nssh))
        allocate (rcutoff(nssh))
        allocate (xocc(nssh))

! Evaluate the angular momentum quantum numbers for each shell
        issh = 1
        if (sshell) then
         lam(issh) = 0
         a0(issh) = 2.0d0
         issh = issh + 1
        end if
        if (pshell) then
         lam(issh) = 1
         a0(issh) = 1.0d0
         issh = issh + 1
        end if
        if (dshell) then
         lam(issh) = 2
         a0(issh) = 8.0d-1
         issh = issh + 1
        end if
        if (fshell) then
         lam(issh) = 3
         a0(issh) = 7.0d-1
         issh = issh + 1
        end if


! *****************************************************************************
        write (*,*) '  '
        write (*,*) '  '
        write (*,*) ' =================== Question No. 3 =================== '
        write (*,*) '  '
        write (*,*) '  '
        write (*,*) ' Determine electronic configuration. '
        write (*,*) ' Electronic configuration (fractionals possible). '
        write (*,*) '  '

        write (*,*) ' The ground state (default) atom is:'
        write (*,*) '  '
        write (*,204) xnoccin
        write (*,*) '  '
        write (*,*) ' Use the default? Y/N '
        read (*,203) answer

        xocc = 0.0d0
        if (answer .eq. 'Y' .or. answer .eq. 'y' .or. answer .eq. ' ') then
         write (*,*) '  '
         write (*,*) ' OK: We USE the default. '
         write (*,*) '  '
         issh = 1
         if (sshell) then
          xocc(issh) = xnoccin(1)
          issh = issh + 1
         end if
         if (pshell) then
          xocc(issh) = xnoccin(2)
          issh = issh + 1
         end if
         if (dshell) then
          xocc(issh) = xnoccin(3)
          issh = issh + 1
         end if
         if (fshell) then
          xocc(issh) = xnoccin(4)
          issh = issh + 1
         end if
        else
         write (*,*) '  '
         write (*,*) ' OK: We change the defaults. '
         write (*,*) '  '
         issh = 1
         if (sshell) then
          write (*,*) ' - How many electrons do you wish in the ', issh,      &
     &                '''th orbital? '
          read (*,*) xocc(issh)
          issh = issh + 1
         end if
         if (pshell) then
          write (*,*) ' - How many electrons do you wish in the ', issh,      &
     &                '''th orbital? '
          read (*,*) xocc(issh)
          issh = issh + 1
         end if
         if (dshell) then
          write (*,*) ' - How many electrons do you wish in the ', issh,      &
     &                '''th orbital? '
          read (*,*) xocc(issh)
          issh = issh + 1
         end if
         if (fshell) then
          write (*,*) ' - How many electrons do you wish in the ', issh,      &
     &                '''th orbital? '
          read (*,*) xocc(issh)
          issh = issh + 1
         end if
        end if

        write (*,*) '  '
        if (nssh .eq. 1) write (*,205) xocc
        if (nssh .eq. 2) write (*,206) xocc
        if (nssh .eq. 3) write (*,207) xocc
        if (nssh .eq. 4) write (*,208) xocc
        nzval = int(sum(xocc) + 0.1d0)

! *****************************************************************************
        write (*,*) '  '
        write (*,*) '  '
        write (*,*) ' =================== Question No. 4 =================== '
        write (*,*) '  '
        write (*,*) '  '

        write (*,*) ' A Fireball',copyright, ' is an atomic orbital which is '
        write (*,*) ' confined to a sphere of radius rc, i.e., the atomic '
        write (*,*) ' Schrodinger equation is solved with an infinite '
        write (*,*) ' potential step. We allow rc(L=0), rc(L=1), etc. to be '
        write (*,*) ' different. For simplicity, you might want to take them '
        write (*,*) ' to be the same.'
        write (*,*) '  '
        write (*,*) ' You are running looprc - the program which loops over '
        write (*,*) ' cutoff values of rc up to rc_max. '
        write (*,*) '  '

        write (*,*) ' We recommend as rc_min =  2.0 (Bohr radii).'
        write (*,*) ' We recommend as rc_max = 10.0 (Bohr radii).'
        write (*,*) ' Use the defaults? Y/N '
        read (*,203) answer

        if (answer .eq. 'Y' .or. answer .eq. 'y' .or. answer .eq. ' ') then
         write (*,*) '  '
         write (*,*) ' OK: We USE the default. '
         write (*,*) '  '
         rc_min = 2.0d0
         rc_max = 10.0d0
         write (*,*) ' The minimum, maximum cutoff radii that you are using is:'
         write (*,209) rc_min, rc_max
        else
         write (*,*) '  '
         write (*,*) ' OK: We change the defaults. '
         write (*,*) '  '
         write (*,*) ' - What minimum cutoff radius you want? '
         read (*,*) rc_min
         write (*,*) ' - What maximum cutoff radius you want? '
         read (*,*) rc_max
         write (*,*) ' The minimum, maximum cutoff radii that you are using is:'
         write (*,209) rc_min, rc_max
        end if


! *****************************************************************************
        write (*,*) '  '
        write (*,*) '  '
        write (*,*) ' =================== Question No. 5 =================== '
        write (*,*) '                     Last question ! '
        write (*,*) '  '
        write (*,*) '  '

        write (*,*) ' Which exchange-correlation functional do you want to '
        write (*,*) ' use? The standard one we have used is the Ceperely- '
        write (*,*) ' Alder form as parameterized by Perdew-Zunger '
        write (*,*) ' (ioption = 3). Here are the different options: '
        write (*,*) '  '
        write (*,*) '  '
        write (*,*) ' 1   LDA  Wigner '
        write (*,*) ' 2   LDA  Hedin/Lundqvist '
        write (*,*) ' 3   LDA  Ceperley/Alder Perdew/Zunger (1980) '
        write (*,*) ' 4   GGA  Perdew/Wang (1991) '
        write (*,*) ' 5   GGA  Becke (1988) X, Perdew (1986) '
        write (*,*) ' 6   GGA  Perdew/Burke/Ernzerhof (1996) '
        write (*,*) ' 7   LDA  Zhao/Parr '
        write (*,*) ' 8   LDA  Ceperley/Alder Perdew/Wang (1991) '
        write (*,*) ' 9   GGA  Becke (1988) X, Lee/Yang/Parr (1988) '
        write (*,*) ' 10  GGA  Perdew/Wang (1991) X, Lee/Yang/Parr (1988) '
        write (*,*) ' 11  LDA  exchange only '
        write (*,*) ' 12  B3LYP  mix exact exchange with BLYP '
        write (*,*) '  '
        write (*,*) ' Use the default? Y/N '
        read (*,*) answer

        if (answer .eq. 'Y' .or. answer .eq. 'y' .or. answer .eq. ' ') then
         write (*,*) '  '
         write (*,*) ' OK: We USE the default. '
         write (*,*) '  '
         ioption = 3
         write (*,*) ' You have chosen option = ', ioption
        else
         write (*,*) '  '
         write (*,*) ' OK: We change the defaults. '
         write (*,*) '  '
         write (*,*) ' Which exchange-correlation functional would you like? '
         read (*,*) ioption
         write (*,*) ' You have chosen option = ', ioption
        end if

! Deallocate Arrays
! ===========================================================================

! Format Statements
! ===========================================================================
100     format (a50)
200     format (i3, 1x, a2)
201     format (a10)
202     format ('s^', f4.2, ' p^', f4.2, ' d^', f5.2, ' f^', f5.2)
203     format (a1)
204     format ('  S ==>', f4.1, '; P ==>', f4.1,                           &
     &          '; D ==>', f4.1, '; F ==>', f4.1)
205     format (' Electrons in valence orbitals:', /,                       &
     &          ' 1st shell = ', f9.4)
206     format (' Electrons in valence orbitals:', /,                       &
     &          ' 1st shell = ', f9.4, ' 2nd shell = ', f9.4)
207     format (' Electrons in valence orbitals:', /,                       &
     &          ' 1st shell = ', f9.4, ' 2nd shell = ', f9.4,               &
     &          ' 3rd shell = ', f9.4)
208     format (' Electrons in valence orbitals:', /,                       &
     &          ' 1st shell = ', f9.4, ' 2nd shell = ', f9.4,               &
     &          ' 3rd shell = ', f9.4, ' 4th shell = ', f9.4)
209     format (2x, f6.2)

        return
        end

