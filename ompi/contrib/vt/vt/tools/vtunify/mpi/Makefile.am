AUTOMAKE_OPTIONS = subdir-objects

if AMBUILDBINARIES
bin_PROGRAMS = vtunify-mpi
endif

if AMBUILDMPIUNIFYLIB
lib_LTLIBRARIES = libvt-mpi-unify.la
endif

include ../Makefile.common

$(COMMONSOURCES):
	$(AM_V_at)$(MKDIR_P) `dirname $@`
	$(LN_S) $(abs_top_srcdir)/tools/vtunify/$@ $@

CC = $(MPICC)
CXX = $(MPICXX)

vtunify_mpi_CFLAGS = -DVT_MPI $(MPICFLAGS) $(MPIINCDIR)
vtunify_mpi_CXXFLAGS = $(COMMONCXXFLAGS) -DVT_MPI $(MPICXXFLAGS) $(MPIINCDIR)
vtunify_mpi_LDFLAGS = $(COMMONLDFLAGS) $(MPICXXFLAGS)

vtunify_mpi_SOURCES = \
	vt_unify_lib.h \
	vt_unify_mpi.h

nodist_vtunify_mpi_SOURCES = \
	$(COMMONSOURCES)

vtunify_mpi_LDADD = \
	$(COMMONLDADD) \
	$(MPILIBDIR) $(MPILIB)

vtunify_mpi_DEPENDENCIES = \
	$(COMMONDEPENDENCIES)

libvt_mpi_unify_la_CFLAGS = -DVT_LIB $(vtunify_mpi_CFLAGS)
libvt_mpi_unify_la_CXXFLAGS = -DVT_LIB -DVT_MPI $(HOOKSCXXFLAGS) $(ETIMESYNCCXXFLAGS) $(MPICXXFLAGS) $(MPIINCDIR)
libvt_mpi_unify_la_LDFLAGS = $(MPICXXFLAGS)
libvt_mpi_unify_la_SOURCES = $(vtunify_mpi_SOURCES)
nodist_libvt_mpi_unify_la_SOURCES = $(nodist_vtunify_mpi_SOURCES)
libvt_mpi_unify_la_LIBADD = $(COMMONLDADD) $(MPILIBDIR) $(PMPILIB)
libvt_mpi_unify_la_DEPENDENCIES = $(vtunify_mpi_DEPENDENCIES)

CLEANFILES = $(COMMONSOURCES)

