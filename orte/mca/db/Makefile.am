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

# header setup
nobase_orte_HEADERS =
dist_pkgdata_DATA =

# local files
headers = db.h
libmca_db_la_SOURCES += $(headers)

# Conditionally install the header files
ortedir = $(includedir)/openmpi/$(subdir)
if WANT_INSTALL_HEADERS
nobase_orte_HEADERS += $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
