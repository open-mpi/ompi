if AMBUILDBINARIES
bin_PROGRAMS = vtunify
endif

if AMEXTERNOTF
OTFINCLUDE = $(OTFINCDIR)
OTFLIBRARY = $(OTFLIBDIR) $(OTFLIB)
else
OTFINCLUDE = -I$(top_srcdir)/extlib/otf/otflib -I$(top_builddir)/extlib/otf/otflib
OTFLIBRARY = -L$(top_builddir)/extlib/otf/otflib -L$(top_builddir)/extlib/otf/otflib/.libs $(OTFLIB)
endif

INCLUDES = $(OTFINCLUDE) -I$(top_srcdir)/vtlib/

if AMHAVEOMP
vtunify_CXXFLAGS = $(OMPFLAG) -DVT_OMP
vtunify_LDFLAGS = $(OMPFLAG)
else
vtunify_CXXFLAGS =
vtunify_LDFLAGS =
endif

vtunify_SOURCES = \
	vt_unify_defs.h \
	vt_unify_defs_hdlr.h \
	vt_unify_events.h \
	vt_unify_events_hdlr.h \
	vt_unify.h \
	vt_unify_stats.h \
	vt_unify_stats_hdlr.h \
	vt_unify_tkfac.h \
	vt_unify.cc \
	vt_unify_defs.cc \
	vt_unify_defs_hdlr.cc \
	vt_unify_events.cc \
	vt_unify_events_hdlr.cc \
	vt_unify_stats.cc \
	vt_unify_stats_hdlr.cc \
	vt_unify_tkfac.cc

vtunify_LDADD = \
	$(top_builddir)/util/util.$(OBJEXT) \
	$(OTFLIBRARY)

