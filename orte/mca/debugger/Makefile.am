#
# Copyright (c) 2010      Cisco Systems, Inc.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_debugger.la
libmca_debugger_la_SOURCES =

# local files
headers = debugger.h

libmca_debugger_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/$(subdir)
nobase_orte_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
