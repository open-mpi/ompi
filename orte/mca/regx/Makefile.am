#
# Copyright (c) 2015-2018 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_regx.la
libmca_regx_la_SOURCES =

# pkgdata setup
dist_ortedata_DATA =

# local files
headers = regx.h
libmca_regx_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ortedir = $(orteincludedir)/$(subdir)
nobase_orte_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
