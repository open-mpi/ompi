#
# Copyright (c) 2011      Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        state_app.h \
        state_app_component.c \
        state_app.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_state_app_DSO
component_noinst =
component_install = mca_state_app.la
else
component_noinst = libmca_state_app.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_state_app_la_SOURCES = $(sources)
mca_state_app_la_LDFLAGS = -module -avoid-version
mca_state_app_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_state_app_la_SOURCES =$(sources)
libmca_state_app_la_LDFLAGS = -module -avoid-version
