#
# Copyright (c) 2014      Intel, Inc. All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_rtc.la
libmca_rtc_la_SOURCES =

# local files
headers = rtc.h
libmca_rtc_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ortedir = $(orteincludedir)/$(subdir)
nobase_orte_HEADERS = $(headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
