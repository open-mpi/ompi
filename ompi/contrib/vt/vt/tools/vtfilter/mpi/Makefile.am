AUTOMAKE_OPTIONS = subdir-objects

if AMBUILDBINARIES
bin_PROGRAMS = vtfilter-mpi
endif

include ../Makefile.common

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f vtfiltergen-mpi$(EXEEXT); \
	$(LN_S) vtfilter-mpi$(EXEEXT) vtfiltergen-mpi$(EXEEXT))
uninstall-local:
	rm -f $(DESTDIR)$(bindir)/vtfiltergen-mpi$(EXEEXT)

$(COMMONSOURCES):
	$(AM_V_at)$(MKDIR_P) `dirname $@`
	$(LN_S) $(abs_top_srcdir)/tools/vtfilter/$@ $@

CXX = $(MPICXX)

vtfilter_mpi_CXXFLAGS = $(COMMONCXXFLAGS) -DVT_MPI $(MPICXXFLAGS) $(MPIINCDIR)
vtfilter_mpi_LDFLAGS = $(COMMONLDFLAGS) $(MPICXXFLAGS)
nodist_vtfilter_mpi_SOURCES = $(COMMONSOURCES)
vtfilter_mpi_LDADD = $(COMMONLDADD) $(MPILIBDIR) $(MPILIB)
vtfilter_mpi_DEPENDENCIES = $(COMMONDEPENDENCIES)

CLEANFILES = $(COMMONSOURCES)

