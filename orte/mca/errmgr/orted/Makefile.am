#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-orte-errmgr-orted.txt

sources = \
        errmgr_orted.h \
        errmgr_orted_component.c \
        errmgr_orted.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_errmgr_orted_DSO
component_noinst =
component_install = mca_errmgr_orted.la
else
component_noinst = libmca_errmgr_orted.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_errmgr_orted_la_SOURCES = $(sources)
mca_errmgr_orted_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_errmgr_orted_la_SOURCES =$(sources)
libmca_errmgr_orted_la_LDFLAGS = -module -avoid-version
