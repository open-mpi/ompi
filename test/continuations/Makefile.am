# Copyright (c) 2018      Los Alamos National Security, LLC.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if PROJECT_OMPI
    noinst_PROGRAMS = continuations continuations-mt continuations-persistent
    continuations_SOURCES = continuations.c
    continuations_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    continuations_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

    continuations-mt_SOURCES = continuations-mt.c
    continuations-mt_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    continuations-mt_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

    continuations-persistent_SOURCES = continuations-persistent.c
    continuations-persistent_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    continuations-persistent_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
endif # PROJECT_OMPI

distclean-local:
	rm -rf *.dSYM .deps .libs *.log *.o *.trs $(noinst_PROGRAMS) Makefile

