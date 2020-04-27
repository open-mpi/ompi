#
# Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

EXTRA_DIST = autogen.options
dist_opaldata_DATA =

# main library setup
noinst_LTLIBRARIES = libmca_pmix.la
libmca_pmix_la_SOURCES =

# local files
headers = pmix-internal.h \
	pmix-3rdparty.h
libmca_pmix_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
nobase_opal_HEADERS = $(headers)
endif

include base/Makefile.am
