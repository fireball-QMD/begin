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

! initial.f90
! Program Description
! ===========================================================================
!       This program pepares the begin_input.f file for running begin.x
! This automates the procedure considerably. Much of the necessary data is in
! periodictable.input, which this program (and others) reads.
! The original program was the brainchild of Wolfgang Windl.
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
        program initial
        use precision
        implicit none

! Local Parameters and Data Declaration
! ===========================================================================
        integer, parameter :: max_z = 110
        integer, parameter :: nssh_max = 4

! Local Variable Declaration and Description
! ===========================================================================
        integer iline
        integer ione
        integer ioption
        integer ipoint
        integer issh
        integer itwo
        integer ithree
        integer iten
        integer iz
        integer nexcite
        integer nssh
        integer nznucin
        integer nznuc
        integer nzval
        integer nzval_ion

        integer, dimension (:), allocatable :: lam

        real(kind=long) rcbohr
        real(kind=long) rc_max
        real(kind=long) xmass

        real(kind=long), dimension (:), allocatable :: a0
        real(kind=long), dimension (:), allocatable :: rcutoff
        real(kind=long), dimension (:), allocatable :: rcut_ion
        real(kind=long), dimension (nssh_max) :: rcutoffin
        real(kind=long), dimension (:), allocatable :: xocc
        real(kind=long), dimension (:), allocatable :: xocc_ion
        real(kind=long), dimension (nssh_max) :: xnoccin

        character(len=1) answer
        character(len=50) bar
        character(len=11) buffer
        character(len=12) buffere
        character(len=1) copyright
        character(len=10) element
        character(len=8) ppfile
        character(len=8) ppionfile
        character(len=2) symbolin
        character(len=3) symbol

        character(len=12), dimension (:), allocatable :: filename_ena
        character(len=11), dimension (:), allocatable :: filename_na
        character(len=12), dimension (:), allocatable :: filename_ewf
        character(len=11), dimension (:), allocatable :: filename_wf
        character(len=3), dimension (:), allocatable :: rcchar
        character(len=1), dimension (0:9) :: z

        logical sshell
        logical pshell
        logical dshell
        logical fshell

! w.f. optimization
        logical ioptim
        real(kind=long), dimension (:), allocatable :: r0
        real(kind=long), dimension (:), allocatable :: Vo        
        real(kind=long), dimension (:), allocatable :: cmix        

! Allocate Arrays
! ===========================================================================

! Procedure
! ===========================================================================
! Initialize some symbols
        copyright = char(169)
        do ipoint = 0, 9
         z(ipoint) = char(48+ipoint)
        end do

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
        write (*,*) '           the Fireball2004 executive committee. '
        write (*,*) '        This program is NOT, under ANY circumstances,'
        write (*,*) '          to be transfered to an unauthorized user! '
        write (*,*) '  '
        write (*,100) bar

        write (*,*) '  '
        write (*,*) '    *-------------------------------------------------* '
        write (*,*) '    |                                                 | '
        write (*,*) '    |                    Welcome.                     | '
        write (*,*) '    |       The Fireball2004 team welcomes you.       | '
        write (*,*) '    |                                                 | '
        write (*,*) '    *-------------------------------------------------* '
        write (*,*) ' '
        write (*,*) ' This program helps you to initialize the "begin" part '
        write (*,*) ' of the Fireball2000 ', copyright, ' package. '
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

! Write out the periodic table.
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
         write (*,*) ' Sorry.'
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
         if (xnoccin(1) .gt. 0.0d0) then
          nssh = nssh + 1
          sshell = .true.
         end if
         if (xnoccin(2) .gt. 0.0d0) then
          nssh = nssh + 1
          pshell = .true.
         end if
         if (xnoccin(3) .gt. 0.0d0) then
          nssh = nssh + 1
          dshell = .true.
         end if
         if (xnoccin(4) .gt. 0.0d0) then
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
        allocate (filename_na(0:nssh))
        allocate (filename_ena(nssh))
        allocate (filename_wf(nssh))
        allocate (filename_ewf(nssh))
        allocate (lam(nssh))
        allocate (rcchar(0:nssh))
        allocate (rcutoff(nssh))
        allocate (rcut_ion(nssh))
        allocate (xocc(nssh))
        allocate (xocc_ion(nssh))
! confinement potential
        allocate (r0(2*nssh))
        allocate (Vo(2*nssh))
        allocate (cmix(nssh))


! Set occupation to zero
        xocc_ion = 0.0d0
        nzval_ion = 0
        xocc = 0.0d0
! fully ground state 
        cmix(:) = 1.0d0

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

        write (*,*) ' We recommend as Fireball', copyright, ' radii: '
        write (*,*) '  '
        write (*,209) rcutoffin
        write (*,*) '  '
        write (*,*) ' Use the default? Y/N '
        read (*,203) answer

        rcutoff = 0.0d0
        if (answer .eq. 'Y' .or. answer .eq. 'y' .or. answer .eq. ' ') then
         write (*,*) '  '
         write (*,*) ' OK: We USE the default. '
         write (*,*) '  '
         issh = 1
         if (sshell) then
          rcutoff(issh) = rcutoffin(1)
          issh = issh + 1
         end if
         if (pshell) then
          rcutoff(issh) = rcutoffin(2)
          issh = issh + 1
         end if
         if (dshell) then
          rcutoff(issh) = rcutoffin(3)
          issh = issh + 1
         end if
         if (fshell) then
          rcutoff(issh) = rcutoffin(4)
          issh = issh + 1
         end if
        else
         write (*,*) '  '
         write (*,*) ' OK: We change the defaults. '
         write (*,*) '  '
         issh = 1
         if (sshell) then
          write (*,*) ' - What cutoff radius you want in the ', issh,         &
     &                '''th orbital? '
          read (*,*) rcutoff(issh)
          issh = issh + 1
         end if
         if (pshell) then
          write (*,*) ' - What cutoff radius you want in the ', issh,         &
     &                '''th orbital? '
          read (*,*) rcutoff(issh)
          issh = issh + 1
         end if
         if (dshell) then
          write (*,*) ' - What cutoff radius you want in the ', issh,         &
     &                '''th orbital? '
          read (*,*) rcutoff(issh)
          issh = issh + 1
         end if
         if (fshell) then
          write (*,*) ' - What cutoff radius you want in the ', issh,         &
     &                '''th orbital? '
          read (*,*) rcutoff(issh)
          issh = issh + 1
         end if
        end if

!JEL
       rcut_ion(:) = rcutoff(:)

        write (*,*) ' The cutoff radii that you are using are: '
        write (*,209) rcutoff
        rc_max = -1.0d0
        do issh = 1, nssh
         rc_max = max(rc_max,rcutoff(issh))
        end do

! *****************************************************************************
        write (*,*) '  '
        write (*,*) '  '
        write (*,*) ' =================== Question No. 5 =================== '
        write (*,*) '  '
        write (*,*) '  '

        write (*,*) ' You have the option of doing an excited state. Do you '
        write (*,*) ' to perform a calculation for the excited state? Y/N '
        read (*,203) answer
        nexcite = 0
        if (answer .eq. 'Y' .or. answer .eq. 'y' .or. answer .eq. ' ') then
          write (*,*) ' Enter nexcite = 1 to create default excited states '
          write (*,*) ' (i.e. just insert a node) or nexcite = 2 to use DMOL '
          write (*,*) ' formalism for creating excited states. '
          write (*,*) ' Select nexcite = 3 to design new optimized wavefunctions'
          write (*,*) ' mixing ground state and excited states. '
          read (*,*) nexcite
          if (nexcite .eq. 1) write (*,*) ' default excited states chosen '
          if (nexcite .eq. 2) then
           write (*,*) ' DMOL excited states chosen '
          end if
          if (nexcite .eq. 3) then
           write (*,*) ' Warnning: No true excited states calculations!! '
           write (*,*) ' Mixing ground and excited states chosen '
          end if
        end if ! if (answer)

! *****************************************************************************
        if ((nexcite .eq. 2) .or. (nexcite .eq. 3) ) then
          write (*,*) '  '
          write (*,*) '  '
          write (*,*) ' =================== Question No. 6 =================== '
          write (*,*) '  '
          write (*,*) '  '
          write (*,*) ' Determine electronic configuration for 2+ ion. '
          write (*,*) ' Electronic configuration (fractionals possible). '
          write (*,*) '  '

          xocc_ion = 0.0d0
          write (*,*) '  '
          write (*,*) ' OK: We change the defaults. '
          write (*,*) '  '
          issh = 1
          if (sshell) then
           write (*,*) ' - How many electrons do you wish in the ', issh,     &
      &               '''th orbital? '
           read (*,*) xocc_ion(issh)
           issh = issh + 1
          end if
          if (pshell) then
           write (*,*) ' - How many electrons do you wish in the ', issh,     &
     &               '''th orbital? '
           read (*,*) xocc_ion(issh)
           issh = issh + 1
          end if
          if (dshell) then
           write (*,*) ' - How many electrons do you wish in the ', issh,     &
     &                '''th orbital? '
           read (*,*) xocc_ion(issh)
           issh = issh + 1
          end if
          if (fshell) then
           write (*,*) ' - How many electrons do you wish in the ', issh,     &
     &                '''th orbital? '
           read (*,*) xocc_ion(issh)
           issh = issh + 1
          end if

          write (*,*) '  '
          if (nssh .eq. 1) write (*,205) xocc_ion
          if (nssh .eq. 2) write (*,206) xocc_ion
          if (nssh .eq. 3) write (*,207) xocc_ion
          if (nssh .eq. 4) write (*,208) xocc_ion
          nzval_ion = int(sum(xocc_ion) + 0.1d0)

! question about mixing
          if (nexcite .eq. 3) then 
           write (*,*) ' - Insert weight factor for ground state (0-1)'
           write (*,*) '        ( 1 means true ground state )         '
           issh = 1
           if (sshell) then
            write (*,*) ' - Mixing factor for ', issh,     &
      &               '''th orbital? '
            read (*,*) cmix(issh)
            issh = issh + 1
           end if
           if (pshell) then
            write (*,*) ' - Mixing factor for ', issh,     &
     &               '''th orbital? '
            read (*,*) cmix(issh)
            issh = issh + 1
           end if
           if (dshell) then
            write (*,*) ' - Mixing factor for ', issh,     &
     &                '''th orbital? '
            read (*,*) cmix(issh)
            issh = issh + 1
           end if
           if (fshell) then
            write (*,*) ' - Mixing factor for ', issh,     &
     &                '''th orbital? '
            read (*,*) cmix(issh)
            issh = issh + 1
           end if
          endif ! (nexcite .eq. 3)
        end if
! *****************************************************************************
        write (*,*) '  '
        write (*,*) '  '
        if (nexcite .eq. 2) then
         write (*,*) ' =================== Question No. 7 =================== '
        else
         write (*,*) ' =================== Question No. 6 =================== '
        end if
!        write (*,*) '                     Last question ! '
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
        write (*,*) ' 11  LSDA Vosko/Wilk/Nusair (1980) '
        write (*,*) ' 12  B3LYP  mix exact exchange and BLYP '
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

! *****************************************************************************
        write (*,*) '  '
        write (*,*) '  '
        write (*,*) ' =================== Question No. 6 =================== '
        write (*,*) '  '
        write (*,*) '                     Last question ! '
        write (*,*) '  '
        write (*,*) ' There is a posibility to use additional confinement '
        write (*,*) ' potential to get more localized w.f.. '
        write (*,*) ' The potential has a form:'
        write (*,*) ' for r > r0 V = Vo * exp -((rc-r0)/(r-r0))/(rc-r)'
        write (*,*) ' else r < r0 V = 0.0  '
        write (*,*) ' (For more detail see e.g. PRB 64, 235111 (2001)).'
        write (*,*) ' Do you want to use additional attraction potential to'
        write (*,*) ' optimize basis set? Y/N'

        Vo = 0.0d0
        r0 = 0.0d0
        ioptim = 0

        read (*,203) answer
        if (answer .eq. 'Y' .or. answer .eq. 'y' .or. answer .eq. ' ') then
         write (*,*) ' Please, enter the parameters of the confinement '
         write (*,*) ' potential '
         
         ioptim = 1

         issh = 1
         if (sshell) then
          write (*,*) ' - Please, input Vo parameter for ', issh,     &
      &               '''th orbital? '
          read (*,*) Vo(issh)
          write (*,*) ' - Please, input r0 cutoff for ', issh,     &
      &               '''th orbital? '
          read (*,*) r0(issh)
          issh = issh + 1
         end if
         if (pshell) then
          write (*,*) ' - Please, input Vo parameter for ', issh,     &
      &               '''th orbital? '
          read (*,*) Vo(issh)
          write (*,*) ' - Please, input r0 cutoff for ', issh,     &
      &               '''th orbital? '
          read (*,*) r0(issh)
          issh = issh + 1
         end if
         if (dshell) then
          write (*,*) ' - Please, input Vo parameter for ', issh,     &
      &               '''th orbital? '
          read (*,*) Vo(issh)
          write (*,*) ' - Please, input r0 cutoff for ', issh,     &
      &               '''th orbital? '
          read (*,*) r0(issh)
          issh = issh + 1
         end if
         if (fshell) then
          write (*,*) ' - Please, input Vo parameter for ', issh,     &
      &               '''th orbital? '
          read (*,*) Vo(issh)
          write (*,*) ' - Please, input r0 cutoff for ', issh,     &
      &               '''th orbital? '
          read (*,*) r0(issh)
          issh = issh + 1
         end if
! now excited states
         if (sshell) then
          write (*,*) ' - Please, input Vo parameter for ', issh,     &
      &               '''th (excited) orbital? '
          read (*,*) Vo(issh)
          write (*,*) ' - Please, input r0 cutoff for ', issh,     &
      &               '''th (excited) orbital? '
          read (*,*) r0(issh)
          issh = issh + 1
         end if
         if (pshell) then
          write (*,*) ' - Please, input Vo parameter for ', issh,     &
      &               '''th (excited) orbital? '
          read (*,*) Vo(issh)
          write (*,*) ' - Please, input r0 cutoff for ', issh,     &
      &               '''th (excited) orbital? '
          read (*,*) r0(issh)
          issh = issh + 1
         end if
         if (dshell) then
          write (*,*) ' - Please, input Vo parameter for ', issh,     &
      &               '''th (excited) orbital? '
          read (*,*) Vo(issh)
          write (*,*) ' - Please, input r0 cutoff for ', issh,     &
      &               '''th (excited) orbital? '
          read (*,*) r0(issh)
          issh = issh + 1
         end if
         if (fshell) then
          write (*,*) ' - Please, input Vo parameter for ', issh,     &
      &               '''th (excited) orbital? '
          read (*,*) Vo(issh)
          write (*,*) ' - Please, input r0 cutoff for ', issh,     &
      &               '''th (excited) orbital? '
          read (*,*) r0(issh)
          issh = issh + 1
         end if
        endif
! *****************************************************************************
        write (*,*) '  '
        write (*,*) ' Now write data to begin_input.f90 '

! Use the cutoff radius for each shell as part of the name of the data files.
! For instance if the cutoff for the 1st shell is 5.1234, then the name will
! have 512 as part of its name. The computer logic for this is a bit ugly, so
! breeze over this.
        do issh = 0, nssh
         if (issh .eq. 0) then
          rcbohr = rc_max
         else
          rcbohr = rcutoff(issh)
         end if
         if (rcbohr .ge. 10.0d0) rcbohr = rcbohr/10.0d0
          ione = rcbohr
          itwo = (rcbohr*10.d0 - float(ione)*10.0d0 + 0.000001d0 )
          ithree = ((rcbohr*100d0 - float(ione)*100d0) - float(itwo)*10.0d0 + 0.000001d0 )
! rcchar is a 3 character string.
         rcchar(issh) = z(ione)//z(itwo)//z(ithree)
        end do

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
        ppionfile(1:3) = symbol
        ppionfile(4:5) = '++'
        ppionfile(6:8) = '.pp'
        do issh = 0, nssh
         buffer(1:3) = symbol
         buffer(4:4) = '_'
         buffer(5:7) = rcchar(issh)
         buffer(8:10) = '.na'
         buffer(11:11) = z(issh)
         filename_na(issh) = buffer
        end do
        do issh = 1, nssh
         buffere(1:3) = symbol
         buffere(4:4) = '_'
         buffere(5:7) = rcchar(issh)
         buffere(8:11) = '.ena'
         buffere(12:12) = z(issh)
         filename_ena(issh) = buffere
        end do
        do issh = 1, nssh
         buffer(1:3) = symbol
         buffer(4:4) = '_'
         buffer(5:7) = rcchar(issh)
         buffer(8:10) = '.wf'
         buffer(11:11) = z(issh)
         filename_wf(issh) = buffer
        end do
        do issh = 1, nssh
         buffere(1:3) = symbol
         buffere(4:4) = '_'
         buffere(5:7) = rcchar(issh)
         buffere(8:11) = '.ewf'
         buffere(12:12) = z(issh)
         filename_ewf(issh) = buffere
        end do

! Write out the new begin_input.f90 file which is a module containing
! all of the necessary parameters for running begin.x.
        open (12, file = 'MODULES/begin_input.f90', status = 'unknown')
        write (12,*) '        module begin_input '
        write (12,*) '         use precision '
        write (12,*) '  '
        write (12,299) ioption
        write (12,300) nexcite
        write (12,301) nssh
        write (12,*) '  '
        write (12,302) nznuc
        write (12,303) nzval
        write (12,304) nzval_ion
        write (12,*) '  '
        if (nssh .eq. 1) write (12,311) lam
        if (nssh .eq. 2) write (12,312) lam
        if (nssh .eq. 3) write (12,313) lam
        if (nssh .eq. 4) write (12,314) lam
        write (12,*) '  '
        if (nssh .eq. 1) write (12,321) a0
        if (nssh .eq. 2) write (12,322) a0
        if (nssh .eq. 3) write (12,323) a0
        if (nssh .eq. 4) write (12,324) a0
        if (nssh .eq. 1) write (12,331) rcutoff
        if (nssh .eq. 2) write (12,332) rcutoff
        if (nssh .eq. 3) write (12,333) rcutoff
        if (nssh .eq. 4) write (12,334) rcutoff
        if (nssh .eq. 1) write (12,3310) rcut_ion
        if (nssh .eq. 2) write (12,3320) rcut_ion
        if (nssh .eq. 3) write (12,3330) rcut_ion
        if (nssh .eq. 4) write (12,3340) rcut_ion
        if (nssh .eq. 1) write (12,341) xocc
        if (nssh .eq. 2) write (12,342) xocc
        if (nssh .eq. 3) write (12,343) xocc
        if (nssh .eq. 4) write (12,344) xocc
        if (nssh .eq. 1) write (12,345) xocc_ion
        if (nssh .eq. 2) write (12,346) xocc_ion
        if (nssh .eq. 3) write (12,347) xocc_ion
        if (nssh .eq. 4) write (12,348) xocc_ion
        write (12,*) '  '
        write (12,350) element
        write (12,351) ppfile
        write (12,352) ppionfile
        write (12,*) '  '
        if (nssh .eq. 1) write (12,361) filename_na
        if (nssh .eq. 2) write (12,362) filename_na
        if (nssh .eq. 3) write (12,363) filename_na
        if (nssh .eq. 4) write (12,364) filename_na
        if (nssh .eq. 1) write (12,371) filename_ena
        if (nssh .eq. 2) write (12,372) filename_ena
        if (nssh .eq. 3) write (12,373) filename_ena
        if (nssh .eq. 4) write (12,374) filename_ena
        if (nssh .eq. 1) write (12,381) filename_wf
        if (nssh .eq. 2) write (12,382) filename_wf
        if (nssh .eq. 3) write (12,383) filename_wf
        if (nssh .eq. 4) write (12,384) filename_wf
        if (nssh .eq. 1) write (12,391) filename_ewf
        if (nssh .eq. 2) write (12,392) filename_ewf
        if (nssh .eq. 3) write (12,393) filename_ewf
        if (nssh .eq. 4) write (12,394) filename_ewf
! confinement potential
        write (12,401) ioptim 
! Vo
        if (nssh .eq. 1 .and. nexcite .eq. 0) then 
          write (12,445) (Vo(issh), issh=1,nssh)
        endif
        if (nssh .eq. 1 .and. nexcite .ne. 0) then 
          write (12,4451) (Vo(issh), issh=1,2*nssh)
        endif 
        if (nssh .eq. 2 .and. nexcite .eq. 0) then
          write (12,446) (Vo(issh), issh=1,nssh)
        endif
        if (nssh .eq. 2 .and. nexcite .ne. 0) then 
          write (12,4461) (Vo(issh), issh=1,2*nssh)
        endif
        if (nssh .eq. 3 .and. nexcite .eq. 0) then 
          write (12,447) (Vo(issh), issh=1,nssh)
        endif
        if (nssh .eq. 3 .and. nexcite .ne. 0) then 
          write (12,4471) (Vo(issh), issh=1,2*nssh)
        endif
        if (nssh .eq. 4 .and. nexcite .eq. 0) then 
          write (12,448) (Vo(issh), issh=1,nssh)
        endif
        if (nssh .eq. 4 .and. nexcite .ne. 0) then 
          write (12,4481) (Vo(issh), issh=1,2*nssh)
        endif
! r0
        if (nssh .eq. 1 .and. nexcite .eq. 0) then 
          write (12,455) (r0(issh), issh=1,nssh)
        endif
        if (nssh .eq. 1 .and. nexcite .ne. 0) then 
          write (12,4551) (r0(issh), issh=1,2*nssh)
        endif	
        if (nssh .eq. 2 .and. nexcite .eq. 0) then 
          write (12,456) (r0(issh), issh=1,nssh)
        endif	
        if (nssh .eq. 2 .and. nexcite .ne. 0) then 
          write (12,4561) (r0(issh), issh=1,2*nssh)
        endif
        if (nssh .eq. 3 .and. nexcite .eq. 0) then 
          write (12,457) (r0(issh), issh=1,nssh)
        endif
        if (nssh .eq. 3 .and. nexcite .ne. 0) then 
          write (12,4571) (r0(issh), issh=1,2*nssh)
        endif
        if (nssh .eq. 4 .and. nexcite .eq. 0) then 
          write (12,458) (r0(issh), issh=1,nssh)
        endif
        if (nssh .eq. 4 .and. nexcite .ne. 0) then 
          write (12,4581) (r0(issh), issh=1,2*nssh)
        endif

! write mixing coefficients
        if (nssh .eq. 1) then 
          write (12,600) (cmix(issh), issh=1,nssh)
        endif
        if (nssh .eq. 2) then 
          write (12,601) (cmix(issh), issh=1,nssh)
        endif
        if (nssh .eq. 3) then 
          write (12,602) (cmix(issh), issh=1,nssh)
        endif
        if (nssh .eq. 4) then 
          write (12,603) (cmix(issh), issh=1,nssh)
        endif


        write (12,*) '  '
        write (12,*) '        end module '
        close (12)

        write (*,*) '  '
        write (*,*) ' Thank you for your input. '
        write (*,*) '  '
        write (*,*) ' Now type "make begin.x", then "begin.x". '
        write (*,*) '  '
        write (*,*) ' Be SURE and type make, since this program has '
        write (*,*) ' generated a new file "begin_input.f" which must be '
        write (*,*) ' recompiled with the source code. '
        write (*,*) '  '


! Deallocate Arrays
! ===========================================================================
        deallocate (a0)
        deallocate (filename_wf)
        deallocate (filename_ewf)
        deallocate (filename_na)
        deallocate (filename_ena)
        deallocate (lam)
        deallocate (rcchar)
        deallocate (rcutoff)
        deallocate (rcut_ion)
        deallocate (xocc)
        deallocate (Vo)
        deallocate (r0)

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
209     format (2x, 4f9.3)
299     format (9x, ' integer, parameter :: ioption = ', i2)
300     format (9x, ' integer, parameter :: nexcite = ', i1)
301     format (9x, ' integer, parameter :: nssh = ', i1)
302     format (9x, ' integer, parameter :: nznuc = ', i2)
303     format (9x, ' integer, parameter :: nzval = ', i2)
304     format (9x, ' integer, parameter :: nzval_ion = ', i2)
311     format (9x, ' integer, dimension (nssh), parameter :: ', 26x, '&',  &
     &          /, 5x, '&', 4x, ' lam = (/', i2, '/)' )
312     format (9x, ' integer, dimension (nssh), parameter :: ', 26x, '&',  &
     &          /, 5x, '&', 4x, ' lam = (/', i2, ',', i2, '/)' )
313     format (9x, ' integer, dimension (nssh), parameter :: ', 26x, '&',  &
     &          /, 5x, '&', 4x, ' lam = (/', 2(i2, ','), i2, '/)' )
314     format (9x, ' integer, dimension (nssh), parameter :: ', 26x, '&',  &
     &          /, 5x, '&', 4x, ' lam = (/', 3(i2, ','), i2, '/)' )
321     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' a0 = (/', f7.4, '/)' )
322     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' a0 = (/', f7.4, ',', f7.4, '/)' )
323     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' a0 = (/', 2(f7.4, ','),         &
     &                                                  f7.4, '/)' )
324     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' a0 = (/', 3(f7.4, ','),         &
     &                                                  f7.4, '/)' )
331     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' rcutoff = (/', f6.3, '/)' )
332     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' rcutoff = (/', f6.3, ',',       &
     &                                                     f6.3, '/)' )
333     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' rcutoff = (/', 2(f6.3, ','),    &
     &                                                       f6.3, '/)' )
334     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' rcutoff = (/', 3(f6.3, ','),    &
     &                                                       f6.3, '/)' )
3310     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',   &
     &          18x, '&', /, 5x, '&', 4x, ' rcut_ion = (/', f6.3, '/)' )
3320     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',   &
     &          18x, '&', /, 5x, '&', 4x, ' rcut_ion = (/', f6.3, ',',      &
     &                                                     f6.3, '/)' )
3330     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',   &
     &          18x, '&', /, 5x, '&', 4x, ' rcut_ion = (/', 2(f6.3, ','),   &
     &                                                       f6.3, '/)' )
3340     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',   &
     &          18x, '&', /, 5x, '&', 4x, ' rcut_ion = (/', 3(f6.3, ','),   &
     &                                                       f6.3, '/)' )
341     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' xocc = (/', f5.2, '/)' )
342     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' xocc = (/', f5.2, ',',          &
     &                                                  f5.2, '/)' )
343     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' xocc = (/', 2(f5.2, ','),       &
     &                                                    f5.2, '/)' )
344     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' xocc = (/', 3(f5.2, ','),       &
     &                                                    f5.2, '/)' )
3411     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' xoccO = (/', f5.2, '/)' )
3422     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' xoccO = (/', f5.2, ',',          &
     &                                                  f5.2, '/)' )
3433     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' xoccO = (/', 2(f5.2, ','),       &
     &                                                    f5.2, '/)' )
3444     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' xoccO = (/', 3(f5.2, ','),       &
     &                                                    f5.2, '/)' )
345     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' xocc_ion = (/', f5.2, '/)' )
346     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' xocc_ion = (/', f5.2, ',',      &
     &                                                      f5.2, '/)' )
347     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' xocc_ion = (/', 2(f5.2, ','),   &
     &                                                        f5.2, '/)' )
348     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' xocc_ion = (/', 3(f5.2, ','),   &
     &                                                        f5.2, '/)' )
350     format (9x, ' character(len=10), parameter :: atomname = ',         &
     &          '''', a10, '''')
351     format (9x, ' character(len=8), parameter :: ppfile = ',            &
     &          '''', a8, '''')
352     format (9x, ' character(len=8), parameter :: ppionfile = ',         &
     &          '''', a8, '''')
361     format (9x, ' character(len=11), dimension (0:nssh), parameter :: ',&
     &          14x, '&', /, 5x, '&', 4x, ' filename_na = (/''', a11, ''',',&
     &          35x, '&', /, 5x, '&', 21x, '''', a11, '''/)')
362     format (9x, ' character(len=11), dimension (0:nssh), parameter :: ',&
     &          14x, '&', /, 5x, '&', 4x, ' filename_na = (/''', a11, ''',',&
     &          35x, '&', /, 5x, '&', 21x, '''', a11, ''',',                &
     &          35x, '&', /, 5x, '&', 21x, '''', a11, '''', '/)')
363     format (9x, ' character(len=11), dimension (0:nssh), parameter :: ',&
     &          14x, '&', /, 5x, '&', 4x, ' filename_na = (/''', a11, ''',',&
     &          2(35x, '&', /, 5x, '&', 21x, '''', a11, ''','),             &
     &          35x, '&', /, 5x, '&', 21x, '''', a11, '''', '/)')
364     format (9x, ' character(len=11), dimension (0:nssh), parameter :: ',&
     &          14x, '&', /, 5x, '&', 4x, ' filename_na = (/''', a11, ''',',&
     &          3(35x, '&', /, 5x, '&', 21x, '''', a11, ''','),             &
     &          35x, '&', /, 5x, '&', 21x, '''', a11, '''', '/)')
371     format (9x, ' character(len=12), dimension (nssh), parameter :: ',  &
     &          16x, '&', /, 5x, '&', 4x, ' filename_ena = (/''', a12, '''/)')
372     format (9x, ' character(len=12), dimension (nssh), parameter :: ',  &
     &          16x, '&', /, 5x, '&', 4x, ' filename_ena = (/''', a12, ''',',&
     &          33x, '&', /, 5x, '&', 22x, '''', a12, '''', '/)')
373     format (9x, ' character(len=12), dimension (nssh), parameter :: ',  &
     &          16x, '&', /, 5x, '&', 4x, ' filename_ena = (/''', a12, ''',',&
     &          33x, '&', /, 5x, '&', 22x, '''', a12, ''',',                 &
     &          33x, '&', /, 5x, '&', 22x, '''', a12, '''', '/)')
374     format (9x, ' character(len=12), dimension (nssh), parameter :: ',  &
     &          16x, '&', /, 5x, '&', 4x, ' filename_ena = (/''', a12, ''',',&
     &          2(33x, '&', /, 5x, '&', 22x, '''', a12, ''','),             &
     &          33x, '&', /, 5x, '&', 22x, '''', a12, '''', '/)')
381     format (9x, ' character(len=11), dimension (nssh), parameter :: ',  &
     &          16x, '&', /, 5x, '&', 4x, ' filename_wf = (/''', a11, '''/)')
382     format (9x, ' character(len=11), dimension (nssh), parameter :: ',&
     &          16x, '&', /, 5x, '&', 4x, ' filename_wf = (/''', a11, ''',',&
     &          35x, '&', /, 5x, '&', 21x, '''', a11, '''/)')
383     format (9x, ' character(len=11), dimension (nssh), parameter :: ',  &
     &          16x, '&', /, 5x, '&', 4x, ' filename_wf = (/''', a11, ''',',&
     &          35x, '&', /, 5x, '&', 21x, '''', a11, ''',',                &
     &          35x, '&', /, 5x, '&', 21x, '''', a11, '''', '/)')
384     format (9x, ' character(len=11), dimension (nssh), parameter :: ',  &
     &          16x, '&', /, 5x, '&', 4x, ' filename_wf = (/''', a11, ''',',&
     &          2(35x, '&', /, 5x, '&', 21x, '''', a11, ''','),             &
     &            35x, '&', /, 5x, '&', 21x, '''', a11, '''', '/)')
391     format (9x, ' character(len=12), dimension (nssh), parameter :: ',  &
     &          16x, '&', /, 5x, '&', 4x, ' filename_ewf = (/''', a12, '''/)')
392     format (9x, ' character(len=12), dimension (nssh), parameter :: ',&
     &          16x, '&', /, 5x, '&', 4x, ' filename_ewf = (/''', a12, ''',',&
     &          33x, '&', /, 5x, '&', 22x, '''', a12, '''/)')
393     format (9x, ' character(len=12), dimension (nssh), parameter :: ',  &
     &          16x, '&', /, 5x, '&', 4x, ' filename_ewf = (/''', a12, ''',',&
     &          33x, '&', /, 5x, '&', 22x, '''', a12, ''',',                &
     &          33x, '&', /, 5x, '&', 22x, '''', a12, '''', '/)')
394     format (9x, ' character(len=12), dimension (nssh), parameter :: ',  &
     &          16x, '&', /, 5x, '&', 4x, ' filename_ewf = (/''', a12, ''',',&
     &          2(33x, '&', /, 5x, '&', 22x, '''', a12, ''','),             &
     &            33x, '&', /, 5x, '&', 22x, '''', a12, '''', '/)')
! confinement potential
401     format (9x, ' integer, parameter :: ioptim = ', i2)
! Vo
445     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' Vo = (/', f10.4, '/)' )
4451    format (9x, ' real(kind=long), dimension (2*nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' Vo = (/', f10.4, ',',      &
     &                                                      f10.4, '/)' )
446     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' Vo = (/', f10.4, ',',      &
     &                                                      f10.4, '/)' )
4461    format (9x, ' real(kind=long), dimension (2*nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' Vo = (/', 3(f10.4, ','),   &
     &                                                        f10.4, '/)' )
447     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' Vo = (/', 2(f10.4, ','),   &
     &                                                        f10.4, '/)' )
4471    format (9x, ' real(kind=long), dimension (2*nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' Vo = (/', 5(f10.4, ','),   &
     &                                                        f10.4, '/)' )
448     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' Vo = (/', 3(f10.4, ','),   &
     &                                                        f10.4, '/)' )
4481     format (9x, ' real(kind=long), dimension (2*nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' Vo = (/', 7(f10.4, ','),   &
     &                                                        f10.4, '/)' )
455     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' r0 = (/', f5.2, '/)' )
4551    format (9x, ' real(kind=long), dimension (2*nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' r0 = (/', f5.2, ',',      &
     &                                                      f5.2, '/)' )
456     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' r0 = (/', f5.2, ',',      &
     &                                                      f5.2, '/)' )
4561    format (9x, ' real(kind=long), dimension (2*nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' r0 = (/', 3(f5.2, ','),   &
     &                                                        f5.2, '/)' )
457     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' r0 = (/', 2(f5.2, ','),   &
     &                                                        f5.2, '/)' )
4571    format (9x, ' real(kind=long), dimension (2*nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' r0 = (/', 5(f5.2, ','),   &
     &                                                        f5.2, '/)' )
458     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' r0 = (/', 3(f5.2, ','),   &
     &                                                        f5.2, '/)' )
4581    format (9x, ' real(kind=long), dimension (2*nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' r0 = (/', 7(f5.2, ','),   &
     &                                                        f5.2, '/)' )
600     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' cmix = (/', f5.2, '/)' )
601     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' cmix = (/', f5.2, ',', f5.2, '/)' )
602     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' cmix = (/', 2(f5.2, ','), f5.2, '/)' )
603     format (9x, ' real(kind=long), dimension (nssh), parameter :: ',    &
     &          18x, '&', /, 5x, '&', 4x, ' cmix = (/', 3(f5.2, ','), f5.2, '/)' )

      end program initial

