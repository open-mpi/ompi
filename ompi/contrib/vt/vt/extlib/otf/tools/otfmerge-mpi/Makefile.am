INCLUDES = -I$(top_builddir)/otflib -I$(top_srcdir)/otflib $(MPI_INCLUDE_LINE)

if AMBUILDBINARIES
bin_PROGRAMS = \
	otfmerge-mpi
endif

CC = $(MPICC)

otfmerge_mpi_CFLAGS = $(MPICFLAGS)
otfmerge_mpi_LDADD = $(top_builddir)/otflib/libotf.la $(MPI_LIB_LINE)
otfmerge_mpi_DEPENDENCIES = $(top_builddir)/otflib/libotf.la
otfmerge_mpi_SOURCES = \
	handler.h \
	otfmerge-mpi.c \
	handler.c

