# -*- makefile -*-
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
	condition_spinwait.h \
	mutex.h \
	mutex_pthread.h \
	mutex_spinlock.h \
	mutex_spinwait.h \
	thread.h

libthreads_la_SOURCES = \
	$(headers) \
	condition_pthread.c \
	condition_spinlock.c \
	condition_spinwait.c \
	mutex.c \
	mutex_pthread.c \
	mutex_spinlock.c \
	mutex_spinwait.c \
	thread.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/threads
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
