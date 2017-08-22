#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        pmix_s2.h \
        pmix_s2_component.c \
        pmix_s2.c \
        pmi2_pmap_parser.h \
        pmi2_pmap_parser.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_s2_DSO
component_noinst =
component_install = mca_pmix_s2.la
else
component_noinst = libmca_pmix_s2.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_s2_la_SOURCES = $(sources)
mca_pmix_s2_la_CPPFLAGS = $(opal_pmi2_CPPFLAGS)
mca_pmix_s2_la_LDFLAGS = -module -avoid-version $(opal_pmi2_LDFLAGS)
mca_pmix_s2_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(opal_pmi2_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_s2_la_SOURCES =$(sources)
libmca_pmix_s2_la_CPPFLAGS = $(opal_pmi2_CPPFLAGS)
libmca_pmix_s2_la_LDFLAGS = -module -avoid-version $(opal_pmi2_LDFLAGS)
libmca_pmix_s2_la_LIBADD = $(opal_pmi2_LIBS)
