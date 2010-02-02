if AMHAVEMPI
SUBDIRS = mpi
endif

if AMBUILDBINARIES
bin_PROGRAMS = vtunify
endif

if AMBUILDETIMESYNC
ETIMESYNCSOURCES = vt_unify_sync.cc
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

if AMHAVEOMP
vtunify_CXXFLAGS = $(OPENMP_CFLAGS) $(ETIMESYNCCXXFLAGS)
vtunify_LDFLAGS = $(OPENMP_CFLAGS)
else
vtunify_CXXFLAGS = $(ETIMESYNCCXXFLAGS)
vtunify_LDFLAGS =
endif

vtunify_SOURCES = \
	vt_unify.h \
	vt_unify_defs.h \
	vt_unify_defs_hdlr.h \
	vt_unify_events.h \
	vt_unify_events_hdlr.h \
	vt_unify_markers.h \
	vt_unify_markers_hdlr.h \
	vt_unify_stats.h \
	vt_unify_stats_hdlr.h \
	vt_unify_sync.h \
	vt_unify_tkfac.h \
	vt_unify.cc \
	vt_unify_defs.cc \
	vt_unify_defs_hdlr.cc \
	vt_unify_events.cc \
	vt_unify_events_hdlr.cc \
	vt_unify_markers.cc \
	vt_unify_markers_hdlr.cc \
	vt_unify_stats.cc \
	vt_unify_stats_hdlr.cc \
	$(ETIMESYNCSOURCES) \
	vt_unify_tkfac.cc

vtunify_LDADD = \
	$(top_builddir)/util/libutil.la \
	$(OTFLIBRARY) $(ETIMESYNCLIBRARY)

vtunify_DEPENDENCIES = \
	$(top_builddir)/util/libutil.la \
	$(OTFDEPENDENCIES)

