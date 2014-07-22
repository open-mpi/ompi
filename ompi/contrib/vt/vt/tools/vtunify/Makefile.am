AUTOMAKE_OPTIONS = subdir-objects

if AMHAVEMPI
SUBDIRS = . mpi
endif

if AMBUILDBINARIES
bin_PROGRAMS = vtunify
endif

include Makefile.common

vtunify_CXXFLAGS = $(COMMONCXXFLAGS)
vtunify_LDFLAGS = $(COMMONLDFLAGS)
vtunify_SOURCES = $(COMMONSOURCES)
vtunify_LDADD = $(COMMONLDADD)
vtunify_DEPENDENCIES = $(COMMONDEPENDENCIES)

EXTRA_DIST = \
	hooks/vt_unify_hooks_raw.h \
	hooks/vt_unify_hooks_raw.cc
