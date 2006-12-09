#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = -I$(top_ompi_builddir)/src/include $(odls_bproc_CPPFLAGS)

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_odls_bproc_DSO
component_noinst =
component_install = mca_odls_bproc.la
else
component_noinst = libmca_odls_bproc.la
component_install =
endif

sources = \
    odls_bproc.h \
    odls_bproc.c \
    odls_bproc_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_odls_bproc_la_SOURCES = $(sources)
mca_odls_bproc_la_LIBADD = \
    $(odls_bproc_LIBS) \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la
mca_odls_bproc_la_LDFLAGS = -module -avoid-version $(odls_bproc_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_odls_bproc_la_SOURCES = $(sources)
libmca_odls_bproc_la_LIBADD = $(odls_bproc_LIBS)
libmca_odls_bproc_la_LDFLAGS = -module -avoid-version $(odls_bproc_LDFLAGS)
