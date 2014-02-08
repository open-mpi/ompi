#
# Copyright (c)      2010 The Trustees of Indiana University.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

CFLAGS = $(crs_dmtcp_CFLAGS)
AM_CPPFLAGS = $(crs_dmtcp_CPPFLAGS)

dist_ompidata_DATA = help-opal-crs-dmtcp.txt

sources = \
        crs_dmtcp.h \
        crs_dmtcp_component.c \
        crs_dmtcp_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_crs_dmtcp_DSO
component_noinst  =
component_install = mca_crs_dmtcp.la
else
component_noinst  = libmca_crs_dmtcp.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_crs_dmtcp_la_SOURCES  = $(sources)
mca_crs_dmtcp_la_LDFLAGS  = -module -avoid-version $(crs_dmtcp_LDFLAGS)
mca_crs_dmtcp_la_LIBADD = $(crs_dmtcp_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_crs_dmtcp_la_SOURCES = $(sources)
libmca_crs_dmtcp_la_LDFLAGS = -module -avoid-version $(crs_dmtcp_LDFLAGS)
libmca_crs_dmtcp_la_LIBADD  = $(crs_dmtcp_LIBS)
