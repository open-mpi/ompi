if AMBUILDBINARIES
bin_PROGRAMS = vtunify-mpi
endif

if AMBUILDETIMESYNC
ETIMESYNCSOURCES = ../vt_unify_sync.cc
ETIMESYNCCXXFLAGS = -DVT_ETIMESYNC
ETIMESYNCINCLUDE = $(CLAPACKINCDIR)
ETIMESYNCLIBRARY = $(CLAPACKLIBDIR) $(CLAPACKLIB)
else
ETIMESYNCSOURCES =
ETIMESYNCCXXFLAGS =
ETIMESYNCINCLUDE =
ETIMESYNCLIBRARY =
endif

if AMEXTERNOTF
OTFINCLUDE = $(OTFINCDIR)
OTFLIBRARY = $(OTFLIBDIR) $(OTFLIB)
OTFDEPENDENCIES =
else
OTFINCLUDE = -I$(top_srcdir)/extlib/otf/otflib -I$(top_builddir)/extlib/otf/otflib
OTFLIBRARY = $(top_builddir)/extlib/otf/otflib/libotf.la
OTFDEPENDENCIES = $(OTFLIBRARY)
endif

INCLUDES = $(OTFINCLUDE) $(ETIMESYNCINCLUDE) -I$(top_srcdir)/vtlib/

CC = $(MPICC)
CXX = $(MPICXX)

vtunify_mpi_CFLAGS = -DVT_MPI $(MPICFLAGS) $(MPIINCDIR)
if AMHAVEOMP
vtunify_mpi_CXXFLAGS = $(OPENMP_CFLAGS) $(MPICXXFLAGS) -DVT_MPI $(ETIMESYNCCXXFLAGS)
vtunify_mpi_LDFLAGS = $(OPENMP_CFLAGS) $(MPICXXFLAGS)
else
vtunify_mpi_CXXFLAGS = $(MPICXXFLAGS) -DVT_MPI $(ETIMESYNCCXXFLAGS)
vtunify_mpi_LDFLAGS =
endif

vtunify_mpi_SOURCES = \
	vt_unify_mpi.h \
	vt_unify_mpi.c \
	../vt_unify.h \
	../vt_unify_defs.h \
	../vt_unify_defs_hdlr.h \
	../vt_unify_events.h \
	../vt_unify_events_hdlr.h \
	../vt_unify_markers.h \
	../vt_unify_markers_hdlr.h \
	../vt_unify_stats.h \
	../vt_unify_stats_hdlr.h \
	../vt_unify_sync.h \
	../vt_unify_tkfac.h \
	../vt_unify.cc \
	../vt_unify_defs.cc \
	../vt_unify_defs_hdlr.cc \
	../vt_unify_events.cc \
	../vt_unify_events_hdlr.cc \
	../vt_unify_markers.cc \
	../vt_unify_markers_hdlr.cc \
	../vt_unify_stats.cc \
	../vt_unify_stats_hdlr.cc \
	$(ETIMESYNCSOURCES) \
	../vt_unify_tkfac.cc

vtunify_mpi_LDADD = \
	$(top_builddir)/util/libvt_util.la \
	$(OTFLIBRARY) $(ETIMESYNCLIBRARY) \
	$(MPILIBDIR) $(MPILIB)

vtunify_mpi_DEPENDENCIES = \
	$(top_builddir)/util/libvt_util.la \
	$(OTFDEPENDENCIES)

