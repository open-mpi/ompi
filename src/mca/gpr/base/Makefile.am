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
        gpr_base_pack_cleanup.c \
        gpr_base_pack_del_index.c \
        gpr_base_unpack_del_index.c \
        gpr_base_pack_dump.c \
        gpr_base_print_dump.c \
        gpr_base_pack_mode_ops.c \
        gpr_base_unpack_mode_ops.c \
        gpr_base_pack_put_get.c \
        gpr_base_unpack_put_get.c \
        gpr_base_pack_startup_msg.c \
        gpr_base_unpack_startup_msg.c \
        gpr_base_pack_subscribe.c \
        gpr_base_unpack_subscribe.c \
        gpr_base_pack_synchro.c \
        gpr_base_unpack_synchro.c \
        gpr_base_pack_test_internals.c \
        gpr_base_unpack_test_internals.c \
        gpr_base_select.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/gpr/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

