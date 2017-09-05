#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_opaldata_DATA = help-pmix-cray.txt

sources = \
        pmix_cray.h \
        pmix_cray_component.c \
        pmix_cray_pmap_parser.c \
        pmix_cray_pmap_parser.h \
        pmix_cray.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_cray_DSO
component_noinst =
component_install = mca_pmix_cray.la
else
component_noinst = libmca_pmix_cray.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_cray_la_SOURCES = $(sources)
mca_pmix_cray_la_CPPFLAGS = $(pmix_cray_CPPFLAGS) $(pmix_alps_CPPFLAGS)
mca_pmix_cray_la_LDFLAGS = -module -avoid-version $(pmix_cray_LDFLAGS) $(pmix_alps_LDFLAGS)
mca_pmix_cray_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(pmix_cray_LIBS) $(pmix_alps_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_cray_la_SOURCES =$(sources)
libmca_pmix_cray_la_CPPFLAGS = $(pmix_cray_CPPFLAGS) $(pmix_alps_CPPFLAGS)
libmca_pmix_cray_la_LDFLAGS = -module -avoid-version $(pmix_cray_LDFLAGS) $(pmix_alps_LDFLAGS)
libmca_pmix_cray_la_LIBADD = $(pmix_cray_LIBS) $(pmix_alps_LIBS)
