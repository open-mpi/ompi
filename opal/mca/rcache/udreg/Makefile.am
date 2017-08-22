#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(rcache_udreg_CPPFLAGS)

sources = rcache_udreg_module.c rcache_udreg_component.c

if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
opal_HEADERS = rcache_udreg.h
endif

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_rcache_udreg_DSO
component_noinst =
component_install = mca_rcache_udreg.la
else
component_noinst = libmca_rcache_udreg.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rcache_udreg_la_SOURCES = $(sources)
mca_rcache_udreg_la_LDFLAGS = -module -avoid-version $(rcache_udreg_LDFLAGS)
mca_rcache_udreg_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(rcache_udreg_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_rcache_udreg_la_SOURCES = $(sources)
libmca_rcache_udreg_la_LIBADD = $(rcache_udreg_LIBS)
libmca_rcache_udreg_la_LDFLAGS = -module -avoid-version $(rcache_udreg_LDFLAGS)
