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

noinst_LTLIBRARIES = libmca_pml_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h \
	pml_base_bsend.h \
	pml_base_request.h \
	pml_base_recvreq.h \
	pml_base_sendreq.h

libmca_pml_base_la_SOURCES = \
        $(headers) \
	pml_base_bsend.c \
	pml_base_close.c \
	pml_base_request.c \
	pml_base_open.c \
	pml_base_select.c \
	pml_base_recvreq.c \
	pml_base_sendreq.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/pml/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

