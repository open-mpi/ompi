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

noinst_LTLIBRARIES = libmca_gpr_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h

libmca_gpr_base_la_SOURCES = \
        $(headers) \
        gpr_base_open.c \
        gpr_base_close.c \
        gpr_base_select.c \
        gpr_base_xfer_payload.c \
	data_type_support/gpr_data_type_packing_fns.c \
        data_type_support/gpr_data_type_unpacking_fns.c \
        unpack_api_response/gpr_base_unpack_cleanup.c \
        unpack_api_response/gpr_base_unpack_del_index.c \
        unpack_api_response/gpr_base_print_dump.c \
        unpack_api_response/gpr_base_dump_notify.c \
        unpack_api_response/gpr_base_unpack_arithmetic_ops.c \
        unpack_api_response/gpr_base_unpack_put_get.c \
        unpack_api_response/gpr_base_unpack_subscribe.c \
        pack_api_cmd/gpr_base_pack_cleanup.c \
        pack_api_cmd/gpr_base_pack_del_index.c \
        pack_api_cmd/gpr_base_pack_dump.c \
        pack_api_cmd/gpr_base_pack_arithmetic_ops.c \
        pack_api_cmd/gpr_base_pack_put_get.c \
        pack_api_cmd/gpr_base_pack_subscribe.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/mca/gpr/base
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif

distclean-local:
	rm -f static-components.h
