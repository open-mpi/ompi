#
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_fddp.la
libmca_fddp_la_SOURCES =

# header setup
nobase_orte_HEADERS =

# local files
headers = fddp.h

libmca_fddp_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_orte_HEADERS += $(headers)
ortedir = $(includedir)/openmpi/orte/mca/fddp
else
ortedir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
