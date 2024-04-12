#
# Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(LTDLINCL) $(prtereachable_CPPFLAGS)

# main library setup
noinst_LTLIBRARIES = libprtemca_prtereachable.la
libprtemca_prtereachable_la_SOURCES =

# pkgdata setup
dist_prtedata_DATA =

# local files
headers = prtereachable.h
libprtemca_prtereachable_la_SOURCES += $(headers)

# Conditionally install the header files
prtedir = $(prteincludedir)/$(subdir)
nobase_prte_HEADERS = $(headers)

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
