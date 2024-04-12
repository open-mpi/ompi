#
# Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libprtemca_prteinstalldirs.la
libprtemca_prteinstalldirs_la_SOURCES =

# local files
headers = prteinstalldirs.h
libprtemca_prteinstalldirs_la_SOURCES += $(headers)

# Conditionally install the header files
prtedir = $(prteincludedir)/$(subdir)
nobase_prte_HEADERS = $(headers)

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
