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

AM_CPPFLAGS		= -DMALLOC_DEBUG=0

if OMPI_HAVE_POSIX_THREADS
AM_CPPFLAGS +=  -DUSE_TSD_DATA_HACK \
		-Isysdeps/pthread
endif
if OMPI_HAVE_SOLARIS_THREADS
AM_CPPFLAGS += -Isysdeps/solaris
endif
# this must come *after* the threads -Is
AM_CPPFLAGS += -Isysdeps/generic

# ptmalloc requires us to include the copyright notice in the
# software.  So install it in the same place that we install ROMIO's
# copyright notices.

docdir			= $(datadir)/openmpi/doc
doc_DATA		= ptmalloc2-COPYRIGHT

noinst_LTLIBRARIES	= libmca_memory_ptmalloc2.la

libmca_memory_ptmalloc2_la_SOURCES = \
			malloc.c \
			malloc-stats.c \
			malloc.h

# these are included directly and shouldn't be built solo
EXTRA_libmca_memory_ptmalloc2_la_SOURCES = \
			arena.c \
			hooks.c

EXTRA_DIST		= \
			ChangeLog \
			README \
			ChangeLog \
			lran2.h \
			t-test.h \
			t-test1.c \
			t-test2.c \
			tst-mallocstate.c \
			tst-mstats.c \
			sysdeps \
			$(doc_DATA)
