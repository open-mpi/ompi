# -*- makefile -*-
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

if OMPI_BUILD_coll_basic_LOADABLE_MODULE
module_noinst =
module_install = mca_coll_basic.la
else
module_noinst = libmca_coll_basic.la
module_install =
endif

mcamoduledir = $(libdir)/ompi
mcamodule_LTLIBRARIES = $(module_install)
mca_coll_basic_la_SOURCES =
mca_coll_basic_la_LIBADD = \
        src/libmca_coll_basic.la \
        $(LIBMPI_LA)
mca_coll_basic_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_coll_basic_la_SOURCES =
libmca_coll_basic_la_LIBADD = src/libmca_coll_basic.la
libmca_coll_basic_la_LDFLAGS = -module -avoid-version
