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
noinst_LTLIBRARIES = libmca_memheap.la
libmca_memheap_la_SOURCES =
libmca_memheap_la_LDFLAGS =
libmca_memheap_la_LIBADD =

dist_ompidata_DATA = 

# local files
headers = memheap.h
libmca_memheap_la_SOURCES += $(headers) $(nodist_headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
oshmemdir = $(ompiincludedir)/$(subdir)
nobase_oshmem_HEADERS = $(headers)
nobase_nodist_oshmem_HEADERS = $(nodist_headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
