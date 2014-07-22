AUTOMAKE_OPTIONS = subdir-objects

if AMHAVEMPI
SUBDIRS = . mpi
endif

if AMBUILDBINARIES
bin_PROGRAMS = vtfilter
endif

include Makefile.common

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f vtfiltergen$(EXEEXT); \
	$(LN_S) vtfilter$(EXEEXT) vtfiltergen$(EXEEXT))
uninstall-local:
	rm -f $(DESTDIR)$(bindir)/vtfiltergen$(EXEEXT)

vtfilter_CXXFLAGS = $(COMMONCXXFLAGS)
vtfilter_LDFLAGS = $(COMMONLDFLAGS)
vtfilter_SOURCES = $(COMMONSOURCES)
vtfilter_LDADD = $(COMMONLDADD)
vtfilter_DEPENDENCIES = $(COMMONDEPENDENCIES)

