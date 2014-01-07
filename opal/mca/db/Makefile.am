#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(LTDLINCL)

# main library setup
noinst_LTLIBRARIES = libmca_db.la
libmca_db_la_SOURCES =

# pkgdata setup
dist_ompidata_DATA =

# local files
headers = db.h db_types.h
libmca_db_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(ompiincludedir)/$(subdir)
nobase_opal_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
