#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_atomic.la
libmca_atomic_la_SOURCES =

# local files
headers = atomic.h
libmca_atomic_la_SOURCES += $(headers) $(nodist_headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
oshmemdir = $(ompiincludedir)/$(subdir)
nobase_oshmem_HEADERS = $(headers)
nobase_nodist_oshmem_HEADERS = $(nodist_headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h 
