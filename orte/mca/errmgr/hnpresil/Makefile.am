#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = .windows

dist_pkgdata_DATA = help-orte-errmgr-hnp.txt

sources = \
        errmgr_hnpresil.h \
        errmgr_hnpresil_component.c \
        errmgr_hnpresil.c \
        errmgr_hnpresil_autor.c \
        errmgr_hnpresil_crmig.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_errmgr_hnpresil_DSO
component_noinst =
component_install = mca_errmgr_hnpresil.la
else
component_noinst = libmca_errmgr_hnpresil.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_errmgr_hnpresil_la_SOURCES = $(sources)
mca_errmgr_hnpresil_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_errmgr_hnpresil_la_SOURCES =$(sources)
libmca_errmgr_hnpresil_la_LDFLAGS = -module -avoid-version
