# Copyright (c) 2018      Los Alamos National Security, LLC.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

TESTS = mpool_memkind

check_PROGRAMS = $(TESTS) $(MPI_CHECKS)

mpool_memkind_SOURCES = mpool_memkind.c

LDFLAGS = $(OPAL_PKG_CONFIG_LDFLAGS)
LDADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

distclean:
	rm -rf *.dSYM .deps .libs *.log *.o *.trs $(check_PROGRAMS) Makefile

