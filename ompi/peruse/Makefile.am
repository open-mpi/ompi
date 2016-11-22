# -*- makefile -*-
#
# Copyright (c) 2004-2006 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from ompi/Makefile.am

headers += peruse/peruse-internal.h

if WANT_PERUSE
# do NOT want this nobase - we want the peruse stripped off...
include_HEADERS += peruse/peruse.h

lib@OMPI_LIBMPI_NAME@_la_SOURCES += \
        peruse/peruse.c \
	peruse/peruse_module.c
endif
