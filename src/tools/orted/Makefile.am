#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
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
            -DOMPI_CONFIGURE_DATE="\"@OMPI_CONFIGURE_DATE@\"" \
            -DOMPI_BUILD_CFLAGS="\"@CFLAGS@\"" \
            -DOMPI_BUILD_CPPFLAGS="\"@CPPFLAGS@\"" \
            -DOMPI_BUILD_CXXFLAGS="\"@CXXFLAGS@\"" \
            -DOMPI_BUILD_CXXCPPFLAGS="\"@CXXCPPFLAGS@\"" \
            -DOMPI_BUILD_FFLAGS="\"@FFLAGS@\"" \
            -DOMPI_BUILD_FCFLAGS="\"@FCFLAGS@\"" \
            -DOMPI_BUILD_LDFLAGS="\"@LDFLAGS@\"" \
            -DOMPI_BUILD_LIBS="\"@LIBS@\""

libs = $(top_builddir)/src/libmpi.la

bin_PROGRAMS = orted
orted_SOURCES = \
        orted.h \
        orted.c \
        orted_bootproxy.c 

orted_LDADD = $(libs) $(LIBMPI_EXTRA_LIBS)
orted_LDFLAGS = $(LIBMPI_EXTRA_LDFLAGS)
orted_DEPENDENCIES = $(libs)

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)
