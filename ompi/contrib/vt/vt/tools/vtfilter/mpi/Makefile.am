if AMBUILDBINARIES
bin_PROGRAMS = vtfilter-mpi
endif

VTFILTERSRCDIR = $(srcdir)/..
include $(srcdir)/../Makefile.common

CXX = $(MPICXX)

vtfilter_mpi_CXXFLAGS = $(COMMONCXXFLAGS) -DVT_MPI $(MPICXXFLAGS) $(MPIINCDIR)
vtfilter_mpi_LDFLAGS = $(COMMONLDFLAGS) $(MPICXXFLAGS)
vtfilter_mpi_SOURCES = $(COMMONSOURCES)
vtfilter_mpi_LDADD = $(COMMONLDADD) $(MPILIBDIR) $(MPILIB)
vtfilter_mpi_DEPENDENCIES = $(COMMONDEPENDENCIES)

