# -*- indent-tabs-mode:nil -*-
#
# Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
#                         reserved.
#
# Additional copyrights may follow
#
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

AM_CPPFLAGS = $(mpool_memkind_CPPFLAGS)

if MCA_BUILD_opal_mpool_memkind_DSO
component_noinst =
component_install = mca_mpool_memkind.la
else
component_noinst = libmca_mpool_memkind.la
component_install =
endif

memkind_SOURCES = \
        mpool_memkind_component.c \
        mpool_memkind_module.c \
        mpool_memkind.h

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mpool_memkind_la_SOURCES = $(memkind_SOURCES)
nodist_mca_mpool_memkind_la_SOURCES = $(memkind_nodist_SOURCES)
mca_mpool_memkind_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(mpool_memkind_LIBS)
mca_mpool_memkind_la_LDFLAGS = -module -avoid-version $(mpool_memkind_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mpool_memkind_la_SOURCES = $(memkind_SOURCES)
nodist_libmca_mpool_memkind_la_SOURCES = $(memkind_nodist_SOURCES)
libmca_mpool_memkind_la_LIBADD = $(mpool_memkind_LIBS)
libmca_mpool_memkind_la_LDFLAGS = -module -avoid-version $(mpool_memkind_LDFLAGS)
