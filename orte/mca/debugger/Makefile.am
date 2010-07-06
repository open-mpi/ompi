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

# header setup
nobase_orte_HEADERS =

# local files
headers = debugger.h

libmca_debugger_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_orte_HEADERS += $(headers)
ortedir = $(includedir)/openmpi/orte/mca/debugger
else
ortedir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
