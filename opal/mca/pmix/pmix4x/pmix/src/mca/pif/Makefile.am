#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_pif.la
libmca_pif_la_SOURCES =

# local files
headers = pif.h
libmca_pif_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
pmixdir = $(pmixincludedir)/pmix/mca/pif
nobase_pmix_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
