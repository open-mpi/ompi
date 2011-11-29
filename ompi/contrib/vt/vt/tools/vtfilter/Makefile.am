if AMHAVEMPI
SUBDIRS = . mpi
endif

if AMBUILDBINARIES
bin_PROGRAMS = vtfilter
endif

VTFILTERSRCDIR = $(srcdir)
include $(srcdir)/Makefile.common

vtfilter_CXXFLAGS = $(COMMONCXXFLAGS)
vtfilter_LDFLAGS = $(COMMONLDFLAGS)
vtfilter_SOURCES = $(COMMONSOURCES)
vtfilter_LDADD = $(COMMONLDADD)
vtfilter_DEPENDENCIES = $(COMMONDEPENDENCIES)

