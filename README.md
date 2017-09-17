This project aims at creating the interpolation files for Fireball program.

Installation
------------

  It is difficult to differentiate between installation and usage in this package.
  Therefore, jump to Usage.

Downloading / Getting the program
---------------------------------

        git clone https://github.com/fireball-QMD/begin  

Usage
-----

        cd begin/begin
        
        Edit the Makefile in this directory and choose a MACHINE option.
        The purpose of this is in a correct definition of F90 and F77 
        variables for compilation. If you choose GLINUX, it should use
        gfortran for both variables and thus usable on most Linux systems.
         
        make initial.x           # compile a program initial.x
        cp ../PPfiles/3/014.pp   # copy a pseudo-potential (pp) file
        ./initial.x              # start initial.x
        
        Answer questions interactively
        
        make begin.x             # compile a main program of the project bedin.x
        ./begin.x                # start begin.x
        
        You should receive files
          014_480.na1
          014_480.wf1
          014_540.na0
          014_540.na2
          014_540.wf2
        which will be used in the next step of a Fireball simulation. Copy
        these files to a directory.
        
