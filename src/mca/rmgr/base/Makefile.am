#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_orte_rmgr_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h

# Library

libmca_orte_rmgr_base_la_SOURCES = \
        $(headers) \
        rmgr_base_context.c \
        rmgr_base_close.c \
        rmgr_base_open.c \
        rmgr_base_select.c \
        rmgr_base_stage_gate.c \
        rmgr_base_stubs.c


# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/orte_rmgr/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
