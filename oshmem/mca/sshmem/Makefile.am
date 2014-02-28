#
# Copyright (c) 2014      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_sshmem.la
libmca_sshmem_la_SOURCES =

# local files
headers = sshmem.h sshmem_types.h 
libmca_sshmem_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
oshmemdir = $(ompiincludedir)/$(subdir)
nobase_oshmem_HEADERS = $(headers)
nobase_nodist_oshmem_HEADERS = $(nodist_headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
