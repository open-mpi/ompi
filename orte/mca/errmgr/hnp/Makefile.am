#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-orte-errmgr-hnp.txt

sources = \
        errmgr_hnp.h \
        errmgr_hnp_component.c \
        errmgr_hnp.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_errmgr_hnp_DSO
component_noinst =
component_install = mca_errmgr_hnp.la
else
component_noinst = libmca_errmgr_hnp.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_errmgr_hnp_la_SOURCES = $(sources)
mca_errmgr_hnp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_errmgr_hnp_la_SOURCES =$(sources)
libmca_errmgr_hnp_la_LDFLAGS = -module -avoid-version
