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

noinst_LTLIBRARIES = libmca_gpr_base_pack.la

# Source code files

headers =

libmca_gpr_base_pack_la_SOURCES = \
        $(headers) \
        gpr_base_pack_cleanup.c \
        gpr_base_pack_del_index.c \
        gpr_base_pack_dump.c \
        gpr_base_pack_arithmetic_ops.c \
        gpr_base_pack_put_get.c \
        gpr_base_pack_subscribe.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/gpr/base/pack_api_cmd
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

