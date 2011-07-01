INCLUDES = \
	-I$(top_builddir)/otflib -I$(top_srcdir)/otflib \
	-I$(top_builddir)/otfauxlib -I$(top_srcdir)/otfauxlib \
	$(MPI_INCLUDE_LINE)

if AMBUILDBINARIES
bin_PROGRAMS = \
	otfprofile-mpi
endif

CXX = $(MPICXX)

otfprofile_mpi_CXXFLAGS = $(MPICXXFLAGS)
otfprofile_mpi_LDADD = $(top_builddir)/otflib/libotf.la $(MATHLIB) $(MPI_LIB_LINE)
otfprofile_mpi_DEPENDENCIES = $(top_builddir)/otflib/libotf.la
otfprofile_mpi_SOURCES = \
	collect_data.h \
	create_latex.h \
	datastructs.h \
	otfprofile-mpi.h \
	reduce_data.h \
	summarize_data.h \
	collect_data.cpp \
	create_latex.cpp \
	otfprofile-mpi.cpp \
	reduce_data.cpp \
	summarize_data.cpp \
	summarize_data.h

