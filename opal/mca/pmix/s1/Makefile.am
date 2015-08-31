#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        pmix_s1.h \
        pmix_s1_component.c \
        pmix_s1.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_s1_DSO
component_noinst =
component_install = mca_pmix_s1.la
else
component_noinst = libmca_pmix_s1.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_s1_la_SOURCES = $(sources)
mca_pmix_s1_la_CPPFLAGS = $(opal_pmi1_CPPFLAGS)
mca_pmix_s1_la_LDFLAGS = -module -avoid-version $(opal_pmi1_LDFLAGS)
mca_pmix_s1_la_LIBADD = $(opal_pmi1_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_s1_la_SOURCES =$(sources)
libmca_pmix_s1_la_CPPFLAGS = $(opal_pmi1_CPPFLAGS)
libmca_pmix_s1_la_LDFLAGS = -module -avoid-version $(opal_pmi1_LDFLAGS)
libmca_pmix_s1_la_LIBADD = $(opal_pmi1_LIBS)
