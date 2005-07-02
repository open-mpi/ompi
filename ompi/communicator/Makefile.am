# -*- makefile -*-
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

noinst_LTLIBRARIES = libcommunicator.la

# Source code files

headers = \
        communicator.h

libcommunicator_la_SOURCES = \
        $(headers) \
        comm_init.c \
	comm.c \
	comm_cid.c \
	comm_dyn.c \
	comm_publish.c 

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/communicator
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
