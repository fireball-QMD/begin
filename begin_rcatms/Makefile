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

all : begin.x initial.x looprc.x glean.x

BMODULES = precision.o constants.o pp_storage.o begin_input.o
RCATMS_OBJECTS = clebsch_gordon.o derf0.o get_nlmesh_size.o get_psi.o \
	get_vee.o integrate_hpsi.o pp.o psirc.o rcatms.o rcatms_excite.o \
	rcatms_DMOL.o vshort.o rcatms_GSM.o
XCOBJECTS = bpgcxc.o Exx.o corlyp.o get_uxc.o ggacrad.o ggaxrad.o gga91_sr.o \
	ldaxc.o lsdavwn.o pbe.o 
OBJECTS = begin.o $(RCATMS_OBJECTS) $(VNN_OBJECTS) $(XCOBJECTS)
begin.x: $(BMODULES) $(OBJECTS) 
	$(F90) $(BMODULES) $(OBJECTS) $(FFLAGS) $(LFLAGS) -o begin.x 

IMODULES = precision.o 
INITIAL = initial.o periodensystem.o
initial.x: $(IMODULES) $(INITIAL)
	$(F90) $(IMODULES) $(INITIAL) $(FFLAGS) $(LFLAGS) -o initial.x 

LMODULES = precision.o constants.o looprc_input.o pp_storage.o
LOOPRC = initial_looprc.o looprc.o periodensystem.o
LRCATMS_OBJECTS = clebsch_gordon.o derf0.o get_nlmesh_size.o get_psi.o \
	get_vee.o integrate_hpsi.o psirc.o rcatms_looprc.o vshort.o \
        pp_looprc.o
XCOBJECTS = bpgcxc.o Exx.o corlyp.o get_uxc.o ggacrad.o ggaxrad.o gga91_sr.o \
	ldaxc.o pbe.o lsdavwn.o
LOBJECTS = $(LOOPRC) $(LRCATMS_OBJECTS) $(XCOBJECTS)
looprc.x: $(LMODULES) $(LOBJECTS) 
	$(F90) $(LMODULES) $(LOBJECTS) $(FFLAGS) $(LFLAGS) -o looprc.x 

GMODULES = precision.o 
GOBJECTS = glean.o
glean.x: $(GMODULES) $(GOBJECTS) 
	$(F90) $(GMODULES) $(GOBJECTS) $(LFLAGS) -o glean.x

# *****************************************************************************
# Modules
# *****************************************************************************
begin_input.o : MODULES/begin_input.f90
	$(F90) $(FFLAGS) -c MODULES/begin_input.f90
constants.o : MODULES/constants.f90
	$(F90) $(FFLAGS) -c MODULES/constants.f90
looprc_input.o : MODULES/looprc_input.f90
	$(F90) $(FFLAGS) -c MODULES/looprc_input.f90
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
# glean.x objects
# *****************************************************************************
glean.o : glean.f90
	$(F90) $(FFLAGS) -c glean.f90

# *****************************************************************************
# initial.x objects
# *****************************************************************************
initial.o : initial.f90
	$(F90) $(FFLAGS) -c initial.f90
periodensystem.o : periodensystem.f90
	$(F90) $(FFLAGS) -c periodensystem.f90

# *****************************************************************************
# looprc.x objects
# *****************************************************************************
initial_looprc.o : $(LMODULES) initial_looprc.f90
	$(F90) $(FFLAGS) -c initial_looprc.f90
looprc.o : looprc.f90 $(LMODULES)
	$(F90) $(FFLAGS) -c looprc.f90

# *****************************************************************************
# rcatms objects
# *****************************************************************************
clebsch_gordon.o : $(BMODULES) RCATMS/clebsch_gordon.f90
	$(F90) $(FFLAGS) -c RCATMS/clebsch_gordon.f90
derf0.o : $(BMODULES) RCATMS/derf0.f90
	$(F90) $(FFLAGS) -c RCATMS/derf0.f90
get_nlmesh_size.o : $(BMODULES) RCATMS/get_nlmesh_size.f90
	$(F90) $(FFLAGS) -c RCATMS/get_nlmesh_size.f90
get_psi.o : $(BMODULES) RCATMS/get_psi.f90
	$(F90) $(FFLAGS) -c RCATMS/get_psi.f90
get_vee.o : $(BMODULES) RCATMS/get_vee.f90
	$(F90) $(FFLAGS) -c RCATMS/get_vee.f90
integrate_hpsi.o : $(BMODULES) RCATMS/integrate_hpsi.f90
	$(F90) $(FFLAGS) -c RCATMS/integrate_hpsi.f90
pp.o : $(BMODULES) RCATMS/pp.f90
	$(F90) $(FFLAGS) -c RCATMS/pp.f90
psirc.o : $(BMODULES) RCATMS/psirc.f90
	$(F90) $(FFLAGS) -c RCATMS/psirc.f90
rcatms.o : $(BMODULES) RCATMS/rcatms.f90
	$(F90) $(FFLAGS) -c RCATMS/rcatms.f90
rcatms_excite.o : $(BMODULES) RCATMS/rcatms_excite.f90
	$(F90) $(FFLAGS) -c RCATMS/rcatms_excite.f90
rcatms_DMOL.o : $(BMODULES) RCATMS/rcatms_DMOL.f90
	$(F90) $(FFLAGS) -c RCATMS/rcatms_DMOL.f90
rcatms_looprc.o : $(BMODULES) RCATMS/rcatms_looprc.f90
	$(F90) $(FFLAGS) -c RCATMS/rcatms_looprc.f90
vshort.o : $(BMODULES) RCATMS/vshort.f90
	$(F90) $(FFLAGS) -c RCATMS/vshort.f90
rcatms_GSM.o : $(BMODULES) RCATMS/rcatms_GSM.f90
	$(F90) $(FFLAGS) -c RCATMS/rcatms_GSM.f90
pp_looprc.o : $(BMODULES) RCATMS/pp_looprc.f90
	$(F90) $(FFLAGS) -c RCATMS/pp_looprc.f90

# *****************************************************************************
# exchange-correlation objects
# *****************************************************************************
bpgcxc.o : $(BMODULES) XC/bpgcxc.f
	$(F77) $(FFLAGS) -c XC/bpgcxc.f
corlyp.o : $(BMODULES) XC/corlyp.f90
	$(F90) $(FFLAGS) -c XC/corlyp.f90
Exx.o : $(BMODULES) XC/Exx.f90
	$(F90) $(FFLAGS) -c XC/Exx.f90
get_uxc.o : $(BMODULES) XC/get_uxc.f90
	$(F90) $(FFLAGS) -c XC/get_uxc.f90
ggacrad.o : $(BMODULES) XC/ggacrad.f90
	$(F90) $(FFLAGS) -c XC/ggacrad.f90
ggaxrad.o : $(BMODULES) XC/ggaxrad.f90
	$(F90) $(FFLAGS) -c XC/ggaxrad.f90
gga91_sr.o : $(BMODULES) XC/gga91_sr.f
	$(F77) $(FFLAGS) -c XC/gga91_sr.f
ldaxc.o : $(BMODULES) XC/ldaxc.f
	$(F77) $(FFLAGS) -c XC/ldaxc.f
lsdavwn.o : $(BMODULES) XC/lsdavwn.f90
	$(F90) $(FFLAGS) -c XC/lsdavwn.f90
pbe.o : $(BMODULES) XC/pbe.f
	$(F77) $(FFLAGS) -c XC/pbe.f

.PHONY: clean veryclean

clean:
	-rm -f -r core *.o .nfs* rii_files .#* ldtmp* *.mod
veryclean: clean
	-rm -f initial.x looprc.x begin.x glean.x
	-./cleanup.com
