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

if OMPI_BUILD_registry_cofs_LOADABLE_MODULE
module_noinst =
module_install = mca_registry_cofs.la
else
module_noinst = libmca_registry_cofs.la
module_install =
endif

mcamoduledir = $(libdir)/ompi
mcamodule_LTLIBRARIES = $(module_install)
mca_registry_cofs_la_SOURCES =
mca_registry_cofs_la_LIBADD = \
        src/libmca_registry_cofs.la \
        $(LIBOMPI_LA)
mca_registry_cofs_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_registry_cofs_la_SOURCES =
libmca_registry_cofs_la_LIBADD = src/libmca_registry_cofs.la
libmca_registry_cofs_la_LDFLAGS = -module -avoid-version
