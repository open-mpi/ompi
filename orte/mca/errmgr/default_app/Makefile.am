#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        errmgr_default_app.h \
        errmgr_default_app_component.c \
        errmgr_default_app.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_errmgr_default_app_DSO
component_noinst =
component_install = mca_errmgr_default_app.la
else
component_noinst = libmca_errmgr_default_app.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_errmgr_default_app_la_SOURCES = $(sources)
mca_errmgr_default_app_la_LDFLAGS = -module -avoid-version
mca_errmgr_default_app_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_errmgr_default_app_la_SOURCES =$(sources)
libmca_errmgr_default_app_la_LDFLAGS = -module -avoid-version
