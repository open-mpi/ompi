# -*- makefile -*-
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

noinst_LTLIBRARIES = libthreads.la

# Source code files

headers = \
	condition.h \
	mutex.h \
	mutex_unix.h \
	mutex_windows.h \
	thread.h

libthreads_la_SOURCES = \
	$(headers) \
	condition.c \
	mutex.c \
	thread.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/threads
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
