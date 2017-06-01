
PROGRAM=trj

SOURCE= trj_netcdf.f90 interpol.f90 trj_mod.f90 ptotheta.f90 date_conv.f90 trj_winds.f90 derivs.f90 derivstr.f90 ode.f90 odeint.f90 stereograph.f90 trj_main_mod.f90

F95=gfortran

FFLAGS=-fdefault-real-8 -fbounds-check -O2
#LIBS= -L../netcdf/lib -lnetcdf
#INCLUDES = -I../netcdf/include
LIBS= -lnetcdff
INCLUDES = -I "C:\cygwin64\usr\include" 


all : $(SOURCE)
	$(F95)  $(FFLAGS) $(INCLUDES) $(SOURCE) $(LIBS)  -o $(PROGRAM) 
