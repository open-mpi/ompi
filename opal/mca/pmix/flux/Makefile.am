#
# Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        pmix_flux.h \
        pmix_flux_component.c \
        pmix_flux.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_flux_DSO
component_noinst =
component_install = mca_pmix_flux.la
else
component_noinst = libmca_pmix_flux.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_flux_la_SOURCES = $(sources)
mca_pmix_flux_la_CPPFLAGS = $(FLUX_PMI_CFLAGS)
mca_pmix_flux_la_LDFLAGS = -module -avoid-version
mca_pmix_flux_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(FLUX_PMI_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_flux_la_SOURCES =$(sources)
libmca_pmix_flux_la_CPPFLAGS = $(FLUX_PMI_CFLAGS)
libmca_pmix_flux_la_LDFLAGS = -module -avoid-version
libmca_pmix_flux_la_LIBADD = $(FLUX_PMI_LIBS)
