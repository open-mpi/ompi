#
# $HEADER$
#

# Use the top-level LAM Makefile.options

include $(top_lam_srcdir)/config/Makefile.options

SUBDIRS = src
DIST_SUBDIRS = config $(SUBDIRS)

EXTRA_DIST = VERSION

# According to the MCA spec, we have to make the output library here
# in the top-level directory, and it has to be named
# liblam_ssi_coll_lam_basic.la

if LAM_BUILD_LOADABLE_MODULE
module_noinst =
module_install = mca_io_romio.la
else
module_noinst = libmca_mpi_io_romio.la
module_install =
endif

mcamoduledir = $(libdir)/lam
mcamodule_LTLIBRARIES = $(module_install)
mca_io_romio_la_SOURCES = 
mca_io_romio_la_LIBADD = \
        src/libmca_io_romio.la \
        $(LIBLAM_LA)
mca_io_romio_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_mpi_io_romio_la_SOURCES =
libmca_mpi_io_romio_la_LIBADD = src/libmca_io_romio.la
libmca_mpi_io_romio_la_LDFLAGS = -module -avoid-version
