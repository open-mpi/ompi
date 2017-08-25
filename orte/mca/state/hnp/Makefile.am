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
        state_hnp.h \
        state_hnp_component.c \
        state_hnp.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_state_hnp_DSO
component_noinst =
component_install = mca_state_hnp.la
else
component_noinst = libmca_state_hnp.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_state_hnp_la_SOURCES = $(sources)
mca_state_hnp_la_LDFLAGS = -module -avoid-version
mca_state_hnp_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_state_hnp_la_SOURCES =$(sources)
libmca_state_hnp_la_LDFLAGS = -module -avoid-version
