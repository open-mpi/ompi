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
# liblam_ssi_topo_lam_unity.la

if LAM_BUILD_LOADABLE_MODULE
module_noinst =
module_install = mca_topo_unity.la
else
module_noinst = libmca_topo_unity.la
module_install =
endif

mcamoduledir = $(libdir)/lam
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

