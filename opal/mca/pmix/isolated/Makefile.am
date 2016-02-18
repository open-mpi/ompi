#
# Copyright (c) 2016      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        pmix_isolated.h \
        pmix_isolated_component.c \
        pmix_isolated.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_isolated_DSO
component_noinst =
component_install = mca_pmix_isolated.la
else
component_noinst = libmca_pmix_isolated.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_isolated_la_SOURCES = $(sources)
mca_pmix_isolated_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_isolated_la_SOURCES =$(sources)
libmca_pmix_isolated_la_LDFLAGS = -module -avoid-version
