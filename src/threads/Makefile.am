# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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
	condition_pthread.h \
	condition_spinlock.h \
	mutex.h \
	mutex_unix.h \
	mutex_windows.h \
	thread.h

libthreads_la_SOURCES = \
	$(headers) \
	condition_pthread.c \
	condition_spinlock.c \
	mutex.c \
	thread.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/threads
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
