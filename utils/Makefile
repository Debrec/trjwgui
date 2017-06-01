PRGSTR=strgrid
PRGRD=trjread

SRCSTR=strgridmain.f90 ../f90trj/stereograph.f90
SRCRD=../f90trj/date_conv.f90 trj_netcdf_r.f90

F95=gfortran

FFLAGS=-fdefault-real-8 -fbounds-check

LIBS= -L../netcdf/lib -lnetcdf
INCLUDES = -I../netcdf/include

#all : $(SOURCE)
#	$(F95) $(FFLAGS) $(INCLUDES) $(SOURCE) -o $(PROGRAM) $(LIBS)
#

all : $(SRCSTR) $(SRCRD)
	$(F95) $(FFLAGS) $(INCLUDES) $(SRCSTR) -o $(PRGSTR) $(LIBS)
	$(F95) $(FFLAGS) $(INCLUDES) $(SRCRD) -o $(PRGRD) $(LIBS)

#$(PRGSTR) : $(SRCSTR)
#	$(F95) $(FFLAGS) $(INCLUDES) $(SRCSTR) -o $(PRGSTR) $(LIBS)

#$(PRGRD) : $(SRCRD)
#	$(F95) $(FFLAGS) $(INCLUDES) $(SRCRD) -o $(PRGRD) $(LIBS)
