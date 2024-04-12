#
# Copyright (c) 2011      Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libprtemca_state.la
libprtemca_state_la_SOURCES =

# pkgdata setup
dist_prtedata_DATA =

# local files
headers = state.h state_types.h
libprtemca_state_la_SOURCES += $(headers)

# Conditionally install the header files
prtedir = $(prteincludedir)/$(subdir)
nobase_prte_HEADERS = $(headers)

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
