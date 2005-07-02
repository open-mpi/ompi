#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

SUBDIRS = data_type_support

noinst_LTLIBRARIES = libmca_ns_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        ns_base_nds.h \
        base.h

# Library

libmca_ns_base_la_SOURCES = \
        $(headers) \
        ns_base_close.c \
        ns_base_select.c \
        ns_base_open.c \
        ns_base_nds.c \
        ns_base_nds_bproc.c \
        ns_base_nds_env.c \
        ns_base_nds_pipe.c \
        ns_base_local_fns.c


# Conditionally install the header files

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/mca/ns/base
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif
