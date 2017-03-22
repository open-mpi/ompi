#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
#
# Copyright (c) 2017      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(LTDLINCL)

# main library setup
noinst_LTLIBRARIES = libmca_psensor.la
libmca_psensor_la_SOURCES =

# local files
headers = psensor.h

libmca_psensor_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
pmixdir = $(pmixincludedir)/$(subdir)
nobase_pmix_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
