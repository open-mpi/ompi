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



noinst_LTLIBRARIES = libmca_soh_base.la

# Source code files

headers = \
        base.h

# Library

libmca_soh_base_la_SOURCES = \
        $(headers) \
        soh_base_close.c \
        soh_base_select.c \
        soh_base_local_functions.c \
        soh_base_get_proc_soh.c \
        soh_base_set_proc_soh.c \
        soh_base_get_job_soh.c \
        soh_base_set_job_soh.c \
        soh_base_open.c \
        data_type_support/soh_data_type_packing_fns.c \
        data_type_support/soh_data_type_unpacking_fns.c


# Conditionally install the header files

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/mca/soh/base
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif

distclean-local:
	rm -f static-components.h
