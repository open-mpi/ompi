#
# Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Intel, Inc. All rights reserved
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_pinstalldirs.la
libmca_pinstalldirs_la_SOURCES =

# local files
headers = pinstalldirs.h pinstalldirs_types.h
libmca_pinstalldirs_la_SOURCES += $(headers)

# install the header files
pmixdir = $(pmixincludedir)/$(subdir)
nobase_pmix_HEADERS = $(headers)

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
