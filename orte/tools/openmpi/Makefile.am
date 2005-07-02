# -*- makefile -*-
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

AM_CPPFLAGS = \
            -DOMPI_PREFIX="\"$(prefix)\"" \
            -DOMPI_BINDIR="\"$(bindir)\"" \
            -DOMPI_LIBDIR="\"$(libdir)\"" \
            -DOMPI_INCDIR="\"$(includedir)\"" \
            -DOMPI_PKGLIBDIR="\"$(pkglibdir)\"" \
            -DOMPI_SYSCONFDIR="\"$(sysconfdir)\"" \
            -DOMPI_CONFIGURE_USER="\"@OMPI_CONFIGURE_USER@\"" \
            -DOMPI_CONFIGURE_HOST="\"@OMPI_CONFIGURE_HOST@\"" \
            -DOMPI_CONFIGURE_DATE="\"@OMPI_CONFIGURE_DATE@\""

libs = \
        $(top_builddir)/orte/liborte.la \
        $(top_builddir)/opal/libopal.la

bin_PROGRAMS = openmpi

openmpi_SOURCES = openmpi.h openmpi.c
openmpi_LDADD = $(LIBMPI_EXTRA_LIBS) $(libs)
openmpi_DFLAGS = $(LIBMPI_EXTRA_LDFLAGS)
openmpi_DEPENDENCIES = $(libs)

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)
