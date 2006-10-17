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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libdebuggers.la
inst_LTLIBRARIES = libompitv.la

# This is not quite in the Automake spirit, but we have to do it.
# Since the totalview portion of the library must be built with -g, we
# must eliminate the CFLAGS that are passed in here by default (which
# may already have debugging and/or optimization flags).  We use
# post-processed forms of the CFLAGS in the library targets down
# below.

CFLAGS = $(CFLAGS_WITHOUT_OPTFLAGS) $(TOTALVIEW_DEBUG_FLAGS)

# Source code files

headers = debuggers.h mpi_interface.h ompi_dll_defs.h

instdir = $(libdir)/openmpi

libdebuggers_la_SOURCES = \
        $(headers) \
        ompi_totalview.c
libdebuggers_la_CFLAGS = -DOMPI_TV_DLL=\"$(instdir)/libompitv.so\"

libompitv_la_SOURCES = ompi_dll.c
libompitv_la_CFLAGS = -g
libompitv_la_LDFLAGS = -module -avoid-version

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/debuggers
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
