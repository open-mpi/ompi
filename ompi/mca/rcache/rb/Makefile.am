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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
    rcache_rb.c \
    rcache_rb.h \
    rcache_rb_component.c \
    rcache_rb_tree.c \
    rcache_rb_tree.h \
    rcache_rb_mru.c \
    rcache_rb_mru.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_rcache_rb_DSO
lib =
lib_sources =
component = mca_rcache_rb.la
component_sources = $(sources)
else
lib = libmca_rcache_rb.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_rcache_rb_la_SOURCES = $(component_sources)
mca_rcache_rb_la_LDFLAGS = -module -avoid-version $(rcache_rb_LDFLAGS)
mca_rcache_rb_la_LIBADD = $(rcache_rb_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_rcache_rb_la_SOURCES = $(lib_sources)
libmca_rcache_rb_la_LDFLAGS = -module -avoid-version$ $(rcache_rb_LDFLAGS)
libmca_rcache_rb_la_LIBADD = $(rcache_rb_LIBS)
