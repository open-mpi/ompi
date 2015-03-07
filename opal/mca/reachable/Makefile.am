#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(LTDLINCL) $(reachable_CPPFLAGS)

# main library setup
noinst_LTLIBRARIES = libmca_reachable.la
libmca_reachable_la_SOURCES =

# pkgdata setup
dist_opaldata_DATA =

# local files
headers = reachable.h
libmca_reachable_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
nobase_opal_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
