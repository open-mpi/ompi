if AMBUILDBINARIES
bin_PROGRAMS = otfmerge-mpi
endif

include ../Makefile.common

$(COMMONSOURCES):
	$(AM_V_at)$(MKDIR_P) `dirname $@`
	$(LN_S) $(top_srcdir)/tools/otfmerge/$@ $@

CC = $(MPICC)

AM_CPPFLAGS = -I$(top_srcdir)/tools/otfmerge $(COMMONINCLUDES) $(MPI_INCLUDE_LINE)

otfmerge_mpi_CFLAGS = -DOTFMERGE_MPI $(COMMONCFLAGS) $(MPICFLAGS)
otfmerge_mpi_LDADD = $(COMMONLDADD) $(MPI_LIB_LINE)
otfmerge_mpi_DEPENDENCIES = $(COMMONDEPENDENCIES)
nodist_otfmerge_mpi_SOURCES = $(COMMONSOURCES)

CLEANFILES = $(COMMONSOURCES)

