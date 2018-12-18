#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        pdl_pdlopen.h \
        pdl_pdlopen_component.c \
        pdl_pdlopen_module.c

# This component will only ever be built statically -- never as a DSO.

noinst_LTLIBRARIES = libmca_pdl_pdlopen.la

libmca_pdl_pdlopen_la_SOURCES = $(sources)
libmca_pdl_pdlopen_la_LDFLAGS = -module -avoid-version
libmca_pdl_pdlopen_la_LIBADD = $(pmix_pdl_pdlopen_LIBS)
