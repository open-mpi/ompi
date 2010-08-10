#
# Copyright (c) 2009-2010 The Trustees of Indiana University.
#                         All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-orte-errmgr-crmig.txt

sources = \
        errmgr_crmig.h \
        errmgr_crmig_component.c \
        errmgr_crmig_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_errmgr_crmig_DSO
component_noinst =
component_install = mca_errmgr_crmig.la
else
component_noinst = libmca_errmgr_crmig.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_errmgr_crmig_la_SOURCES = $(sources)
mca_errmgr_crmig_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_errmgr_crmig_la_SOURCES = $(sources)
libmca_errmgr_crmig_la_LDFLAGS = -module -avoid-version
