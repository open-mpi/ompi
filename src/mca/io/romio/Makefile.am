#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

SUBDIRS = src romio-dist

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_io_romio_DSO
component_noinst =
component_install = mca_io_romio.la
else
component_noinst = libmca_mpi_io_romio.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_io_romio_la_SOURCES = 
mca_io_romio_la_LIBADD = src/libmca_io_romio.la
mca_io_romio_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_mpi_io_romio_la_SOURCES =
libmca_mpi_io_romio_la_LIBADD = src/libmca_io_romio.la
libmca_mpi_io_romio_la_LDFLAGS = -module -avoid-version
