#
# Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This is a special component -- its main purpose in life is to have
# its configure.m4 add some things to CPPFLAGS, LDFLAGS, and
# LIBS.  Hence, there's nothing to build here.  :-)

# We only ever build this component statically

noinst_LTLIBRARIES = libmca_pmix_external.la

libmca_pmix_external_la_SOURCES = \
        external.h \
        pmix_external_component.c
libmca_pmix_external_la_LDFLAGS = \
    -module -avoid-version

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
nobase_opal_HEADERS = external.h
endif
