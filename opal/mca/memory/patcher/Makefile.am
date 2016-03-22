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
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This component is only ever built statically (i.e., slurped into
# libopen-pal) -- it is never built as a DSO.
noinst_LTLIBRARIES = libmca_memory_patcher.la
libmca_memory_patcher_la_SOURCES = \
    memory_patcher.h \
    memory_patcher_component.c
libmca_memory_patcher_la_LDFLAGS = \
   -module -avoid-version $(memory_patcher_LDFLAGS)
libmca_memory_patcher_la_LIBADD = $(memory_patcher_LIBS)
