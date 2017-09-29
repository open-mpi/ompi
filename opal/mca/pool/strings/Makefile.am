#
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        pool_strings.h \
        pool_strings_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pool_strings_DSO
component_noinst =
component_install = mca_pool_strings.la
else
component_noinst = libmca_pool_strings.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pool_strings_la_SOURCES = $(sources)
mca_pool_strings_la_LDFLAGS = -module -avoid-version
mca_pool_strings_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_pool_strings_la_SOURCES = $(sources)
libmca_pool_strings_la_LDFLAGS = -module -avoid-version
