if AMBUILDBINARIES
bin_PROGRAMS = otfprofile-mpi
endif

include ../Makefile.common

$(COMMONSOURCES):
	$(AM_V_at)$(MKDIR_P) `dirname $@`
	$(LN_S) $(top_srcdir)/tools/otfprofile/$@ $@

CXX = $(MPICXX)

AM_CPPFLAGS = -I$(top_srcdir)/tools/otfprofile $(COMMONINCLUDES) $(MPI_INCLUDE_LINE)

otfprofile_mpi_CXXFLAGS = -DOTFPROFILE_MPI $(COMMONCXXFLAGS) $(MPICXXFLAGS)
otfprofile_mpi_LDADD = $(COMMONLDADD) $(MPI_LIB_LINE)
otfprofile_mpi_DEPENDENCIES = $(COMMONDEPENDENCIES)
otfprofile_mpi_SOURCES = \
	reduce_data.h \
	reduce_data.cpp
nodist_otfprofile_mpi_SOURCES = $(COMMONSOURCES)

CLEANFILES = $(COMMONSOURCES)

