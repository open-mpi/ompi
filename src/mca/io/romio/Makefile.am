#
# $HEADER$
#

# Use the top-level OMPI Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

SUBDIRS = src

EXTRA_DIST = VERSION

# According to the MCA spec, we have to make the output library here
# in the top-level directory, and it has to be named
# libompi_ssi_coll_ompi_basic.la

if OMPI_BUILD_io_romio_LOADABLE_MODULE
module_noinst =
module_install = mca_io_romio.la
else
module_noinst = libmca_mpi_io_romio.la
module_install =
endif

mcamoduledir = $(libdir)/openmpi
mcamodule_LTLIBRARIES = $(module_install)
mca_io_romio_la_SOURCES = 
mca_io_romio_la_LIBADD = \
        src/libmca_io_romio.la \
        $(LIBOMPI_LA)
mca_io_romio_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_mpi_io_romio_la_SOURCES =
libmca_mpi_io_romio_la_LIBADD = src/libmca_io_romio.la
libmca_mpi_io_romio_la_LDFLAGS = -module -avoid-version
