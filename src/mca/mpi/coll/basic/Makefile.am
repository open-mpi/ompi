# -*- makefile -*-
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
module_install = mca_coll_basic.la
else
module_noinst = libmca_mpi_coll_basic.la
module_install =
endif

mcamoduledir = $(libdir)/lam
mcamodule_LTLIBRARIES = $(module_install)
mca_coll_basic_la_SOURCES =
mca_coll_basic_la_LIBADD = \
        src/libmca_coll_basic.la \
        $(top_lam_builddir)/src/mpi/libmpi.la \
        $(top_lam_builddir)/src/lam/liblam.la
mca_coll_basic_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_mpi_coll_basic_la_SOURCES =
libmca_mpi_coll_basic_la_LIBADD = src/libmca_coll_basic.la
libmca_mpi_coll_basic_la_LDFLAGS = -module -avoid-version
