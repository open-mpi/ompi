#
# Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

EXTRA_DIST = autogen.subdirs

SUBDIRS = openpmix

sources = \
        pmix4x.h \
        pmix4x_component.c

# We only ever build this component statically
noinst_LTLIBRARIES = libmca_pmix_pmix4x.la
libmca_pmix_pmix4x_la_SOURCES =$(sources)
nodist_libmca_pmix_pmix4x_la_SOURCES = $(nodist_headers)
libmca_pmix_pmix4x_la_LDFLAGS = -module -avoid-version $(opal_pmix_pmix4x_LDFLAGS)
libmca_pmix_pmix4x_la_LIBADD = $(opal_pmix_pmix4x_LIBS)
