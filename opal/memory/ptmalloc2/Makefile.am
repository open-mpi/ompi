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

if OMPI_WANT_PTMALLOC2
noinst_LTLIBRARIES	= libptmalloc2.la
endif

libptmalloc2_la_SOURCES = \
			malloc.c \
			malloc-stats.c \
			malloc.h \
			thread-m.h \
			thread-st.h

# these are included directly and shouldn't be built solo
EXTRA_libptmalloc2_la_SOURCES = \
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
			sysdeps \
			$(doc_DATA)
