if AMBUILDBINARIES
bin_PROGRAMS = vtfilter
endif

if AMEXTERNOTF
OTFINCLUDE = $(OTFINCDIR)
OTFLIBRARY = $(OTFLIBDIR) $(OTFLIB)
else
OTFINCLUDE = -I$(top_srcdir)/extlib/otf/otflib -I$(top_builddir)/extlib/otf/otflib
OTFLIBRARY = -L$(top_builddir)/extlib/otf/otflib -L$(top_builddir)/extlib/otf/otflib/.libs $(OTFLIB)
endif

INCLUDES = $(OTFINCLUDE) -I$(top_srcdir)/vtlib/ -I$(top_builddir)/vtlib

if AMHAVEOMP
vtfilter_CXXFLAGS = $(OMPFLAG) -DVT_OMP
vtfilter_LDFLAGS = $(OMPFLAG)
else
vtfilter_CXXFLAGS =
vtfilter_LDFLAGS =
endif

vtfilter_SOURCES = \
	vt_filter.h \
	vt_filthandler.h \
	vt_otfhandler.h \
	vt_filter.cc \
	vt_filthandler.cc \
	vt_otfhandler.cc \
	vt_tracefilter.cc

vtfilter_LDADD = \
	$(top_builddir)/util/util.$(OBJEXT) \
	$(OTFLIBRARY)

