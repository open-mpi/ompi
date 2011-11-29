if AMBUILDDYNATTLIB
SUBDIRS = . dynattlib
endif

if AMBUILDBINARIES
bin_PROGRAMS = vtdyn
endif

INCLUDES = $(DYNIINCDIR) -I$(top_srcdir)/include -I$(top_builddir)/include -I$(top_srcdir)/rfg

vtdyn_SOURCES = \
	vt_dyn.h \
	vt_dyn.cc

vtdyn_LDADD = \
	$(top_builddir)/rfg/libvt_rfg.la \
	$(top_builddir)/util/libvt_util.la \
	$(DYNILIBDIR) $(DYNILIB)

vtdyn_DEPENDENCIES = \
	$(top_builddir)/rfg/libvt_rfg.la \
	$(top_builddir)/util/libvt_util.la

