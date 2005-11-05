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



noinst_LTLIBRARIES = libmca_rmgr_base.la

# Source code files

headers = \
        base.h

# Library

libmca_rmgr_base_la_SOURCES = \
        $(headers) \
        rmgr_base_context.c \
        rmgr_base_close.c \
        rmgr_base_open.c \
        rmgr_base_pack.c \
        rmgr_base_unpack.c \
        rmgr_base_select.c \
        rmgr_base_stage_gate.c \
        rmgr_base_stubs.c \
        data_type_support/rmgr_data_type_packing_fns.c \
        data_type_support/rmgr_data_type_unpacking_fns.c


# Conditionally install the header files

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/mca/rmgr/base
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif

distclean-local:
	rm -f static-components.h
