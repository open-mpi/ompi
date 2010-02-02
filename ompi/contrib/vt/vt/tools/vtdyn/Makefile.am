if AMBUILDDYNATTLIB
SUBDIRS = dynattlib
endif

if AMBUILDBINARIES
bin_PROGRAMS = vtdyn
endif

INCLUDES = $(DYNIINCDIR)

vtdyn_SOURCES = \
	vt_dyn.h \
	vt_dyn.cc

vtdyn_LDADD = $(DYNILIBDIR) $(DYNILIB)

