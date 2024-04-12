#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
#
# Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(LTDLINCL)

# main library setup
noinst_LTLIBRARIES = libmca_pstrg.la
libmca_pstrg_la_SOURCES =

# local files
headers = pstrg.h

libmca_pstrg_la_SOURCES += $(headers)

# install the header files
pmixdir = $(pmixincludedir)/$(subdir)
nobase_pmix_HEADERS = $(headers)

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
