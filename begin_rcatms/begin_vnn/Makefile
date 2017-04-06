#WARNING: Trailing spaces will mess up the variables
#Pick your machine type: SGI, AIX, LINUX
MACHINE = ILINUX
#Define debug to turn on symbols and array checking: <blank for none>, TRUE
DEBUG = TRUE 

#SGI SGI SGI SGI SGI SGI SGI SGI SGI SGI SGI SGI SGI SGI SGI SGI SGI SGI SGI
ifeq (${MACHINE},SGI)
        F90 = f90
	F77 = f77
        OPTIMIZATION = -O2
        LFLAGS = -Wl,-woff,136
endif

#AIX AIX AIX AIX AIX AIX AIX AIX AIX AIX AIX AIX AIX AIX AIX AIX AIX AIX AIX
ifeq (${MACHINE},AIX)
        OPTIMIZATION = -O2 -qnosave
	F77 = xlf77
        F90 = xlf90 -qsuffix=f=f90
        LFLAGS = -bmaxdata:0x70000000
endif

#LINUX LINUX LINUX LINUX LINUX LINUX LINUX LINUX LINUX LINUX LINUX LINUX LINUX
ifeq (${MACHINE},LINUX)
        F90 = pgf90
	F77 = pgf77
        OPTIMIZATION = -O1
        LFLAGS =
endif

#ILINUX ILINUX ILINUX ILINUX ILINUX ILINUX ILINUX ILINUX ILINUX ILINUX
ifeq (${MACHINE},ILINUX)
        F90 = ifort
        F77 = ifort
        OPTIMIZATION = -O1 -r8 -fno-alias -unroll
        OPTIMIZATION = -g -check all -fno-alias -unroll -no-ipo
        LFLAGS = -O0
endif


#Do we debug?
ifeq (${DEBUG},TRUE)
        OPTIMIZATION = -C -g
endif

FFLAGS = $(OPTIMIZATION)

all : begin.x initial.x 

BMODULES = precision.o constants.o pp_storage.o begin_input.o
RC_OBJECTS = pp.o get_nlmesh_size.o vshort.o derf0.o
VNN_OBJECTS = get_ewfmesh_size.o get_wfmesh_size.o psiofr.o \
	readpsi.o readpsi_excite.o vnn.o vnn_excite.o
OBJECTS = begin.o $(RC_OBJECTS) $(VNN_OBJECTS)
begin.x: $(BMODULES) $(OBJECTS) 
	$(F90) $(BMODULES) $(OBJECTS) $(FFLAGS) $(LFLAGS) -o begin.x 

IMODULES = precision.o 
INITIAL = initial.o periodensystem.o
initial.x: $(IMODULES) $(INITIAL)
	$(F90) $(IMODULES) $(INITIAL) $(FFLAGS) $(LFLAGS) -o initial.x 

LMODULES = precision.o constants.o pp_storage.o
LOOPRC = periodensystem.o
LOBJECTS = $(LOOPRC)   

GMODULES = precision.o 

# *****************************************************************************
# Modules
# *****************************************************************************
begin_input.o : MODULES/begin_input.f90
	$(F90) $(FFLAGS) -c MODULES/begin_input.f90
constants.o : MODULES/constants.f90
	$(F90) $(FFLAGS) -c MODULES/constants.f90
precision.o : MODULES/precision.f90
	$(F90) $(FFLAGS) -c MODULES/precision.f90
pp_storage.o : MODULES/pp_storage.f90
	$(F90) $(FFLAGS) -c MODULES/pp_storage.f90

# *****************************************************************************
# begin.x objects
# *****************************************************************************
begin.o : begin.f90 $(BMODULES) 
	$(F90) $(FFLAGS) -c begin.f90

# *****************************************************************************
# initial.x objects
# *****************************************************************************
initial.o : initial.f90
	$(F90) $(FFLAGS) -c initial.f90
periodensystem.o : periodensystem.f90
	$(F90) $(FFLAGS) -c periodensystem.f90

# *****************************************************************************
# rc objects
# *****************************************************************************

pp.o : $(BMODULES) RC/pp.f90
	$(F90) $(FFLAGS) -c RC/pp.f90
get_nlmesh_size.o : $(BMODULES) RC/get_nlmesh_size.f90
	$(F90) $(FFLAGS) -c RC/get_nlmesh_size.f90
vshort.o : $(BMODULES) RC/vshort.f90
	$(F90) $(FFLAGS) -c RC/vshort.f90
derf0.o : $(BMODULES) RC/derf0.f90
	$(F90) $(FFLAGS) -c RC/derf0.f90

# *****************************************************************************
# vnn objects
# *****************************************************************************
get_ewfmesh_size.o : $(BMODULES) VNN/get_ewfmesh_size.f90
	$(F90) $(FFLAGS) -c VNN/get_ewfmesh_size.f90
get_wfmesh_size.o : $(BMODULES) VNN/get_wfmesh_size.f90
	$(F90) $(FFLAGS) -c VNN/get_wfmesh_size.f90
psiofr.o : $(BMODULES) VNN/psiofr.f90
	$(F90) $(FFLAGS) -c VNN/psiofr.f90
readpsi.o : $(BMODULES) VNN/readpsi.f90
	$(F90) $(FFLAGS) -c VNN/readpsi.f90
readpsi_excite.o : $(BMODULES) VNN/readpsi_excite.f90
	$(F90) $(FFLAGS) -c VNN/readpsi_excite.f90
vnn.o : $(BMODULES) VNN/vnn.f90
	$(F90) $(FFLAGS) -c VNN/vnn.f90
vnn_excite.o : $(BMODULES) VNN/vnn_excite.f90
	$(F90) $(FFLAGS) -c VNN/vnn_excite.f90

.PHONY: clean veryclean

clean:
	-rm -f -r core *.o .nfs* rii_files .#* ldtmp* *.mod
veryclean: clean
	-rm -f initial.x begin.x 
	-./cleanup.com
