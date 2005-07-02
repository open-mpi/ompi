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

noinst_LTLIBRARIES = libmca_oob_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h

# Library

libmca_oob_base_la_SOURCES = \
        $(headers) \
        oob_base_barrier.c \
        oob_base_close.c \
        oob_base_init.c \
        oob_base_open.c \
        oob_base_ping.c \
        oob_base_recv.c \
        oob_base_recv_nb.c \
        oob_base_send.c \
        oob_base_xcast.c \
        oob_base_send_nb.c 

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/oob/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
