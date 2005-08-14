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

# Apple requires us to include the copyright notice in the software.
# So install it in the same place that we install ROMIO's copyright
# notices.
docdir			= $(datadir)/openmpi/doc
doc_DATA		= APPLE_LICENSE

noinst_LTLIBRARIES	= libmca_memory_darwin7.la

libmca_memory_darwin7_la_SOURCES = \
			pthread_machdep.h \
			pthread_spinlock.h \
			scalable_malloc.c \
			scalable_malloc.h \
			darwin7_component.c \
			darwin7_munmap.c

EXTRA_DIST		= \
			$(doc_DATA)
