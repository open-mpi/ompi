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
# libompi_ssi_topo_ompi_unity.la

if OMPI_BUILD_topo_unity_LOADABLE_MODULE
module_noinst =
module_install = mca_topo_unity.la
else
module_noinst = libmca_topo_unity.la
module_install =
endif

mcamoduledir = $(libdir)/ompi
mcamodule_LTLIBRARIES = $(module_install)
mca_topo_unity_la_SOURCES =
mca_topo_unity_la_LIBADD = \
        src/libmca_topo_unity.la \
        $(LIBMPI_LA)
mca_topo_unity_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_topo_unity_la_SOURCES =
libmca_topo_unity_la_LIBADD = src/libmca_topo_unity.la
libmca_topo_unity_la_LDFLAGS = -module -avoid-version

