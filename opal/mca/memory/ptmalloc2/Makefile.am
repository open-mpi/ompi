#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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



AM_CPPFLAGS = -DMALLOC_DEBUG=0

if OMPI_HAVE_POSIX_THREADS
AM_CPPFLAGS +=  \
    -D_GNU_SOURCE=1 \
    -DUSE_TSD_DATA_HACK=1 \
    -DMALLOC_HOOKS=1 \
    -I$(srcdir)/sysdeps/pthread
endif
if OMPI_HAVE_SOLARIS_THREADS
AM_CPPFLAGS += -I$(srcdir)/sysdeps/solaris
endif
# this must come *after* the threads -Is
AM_CPPFLAGS += -I$(srcdir)/sysdeps/generic

# ptmalloc requires us to include the copyright notice in the
# software.  So install it in the same place that we install ROMIO's
# copyright notices.

docdir      = $(datadir)/openmpi/doc
doc_DATA    = ptmalloc2-COPYRIGHT

noinst_LTLIBRARIES = libmca_memory_ptmalloc2.la

libmca_memory_ptmalloc2_la_SOURCES = \
   opal_ptmalloc2_component.c \
   opal_ptmalloc2_munmap.c \
   malloc.c \
   malloc-stats.c \
   malloc.h
libmca_memory_ptmalloc2_la_LDFLAGS = -module -avoid-version $(memory_ptmalloc2_LDFLAGS)
libmca_memory_ptmalloc2_la_LIBADD = $(memory_ptmalloc2_LIBS)

# these are included directly and shouldn't be built solo
EXTRA_libmca_memory_ptmalloc2_la_SOURCES = \
    arena.c \
    hooks.c

EXTRA_DIST = \
    ChangeLog \
    README \
    ChangeLog \
    lran2.h \
    t-test.h \
    t-test1.c \
    t-test2.c \
    tst-mallocstate.c \
    tst-mstats.c \
        sysdeps/sproc/malloc-machine.h \
        sysdeps/sproc/thread-st.h \
        sysdeps/pthread/malloc-machine.h \
        sysdeps/pthread/thread-st.h \
        sysdeps/solaris/malloc-machine.h \
        sysdeps/solaris/thread-st.h \
        sysdeps/generic/malloc-machine.h \
        sysdeps/generic/thread-st.h \
        sysdeps/generic/atomic.h \
        $(doc_DATA)
