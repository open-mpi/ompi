# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libthreads.la

# Source code files

headers = \
	mutex.h \
	mutex_spinlock.h \
	mutex_spinwait.h \
	thread.h

libthreads_la_SOURCES = \
	$(headers) \
	thread.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/lam/threads
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
