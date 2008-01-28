if AMBUILDDYNATTLIB
SUBDIRS = dynattlib
endif

bin_PROGRAMS = vtdyn

INCLUDES = $(DYNIINCDIR)

vtdyn_SOURCES = \
	vt_dyn.h \
	vt_dyn.cc
vtdyn_LDADD = \
        $(DYNILIBDIR) $(DYNILIB)

