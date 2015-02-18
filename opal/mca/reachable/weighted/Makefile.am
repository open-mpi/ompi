#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        reachable_weighted.h \
        reachable_weighted_component.c \
        reachable_weighted.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_reachable_weighted_DSO
component_noinst =
component_install = mca_reachable_weighted.la
else
component_noinst = libmca_reachable_weighted.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_reachable_weighted_la_SOURCES = $(sources)
mca_reachable_weighted_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_reachable_weighted_la_SOURCES =$(sources)
libmca_reachable_weighted_la_LDFLAGS = -module -avoid-version
