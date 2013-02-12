#
# Copyright (c) 2012      Mellanox Technologies, Inc.
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

# header setup
nobase_oshmem_HEADERS =
nobase_nodist_oshmem_HEADERS =

dist_pkgdata_DATA = 

# local files
headers = memheap.h
libmca_memheap_la_SOURCES += $(headers) $(nodist_headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_oshmem_HEADERS += $(headers)
nobase_nodist_oshmem_HEADERS += $(nodist_headers)
oshmemdir = $(includedir)/oshmem/oshmem/mca/memheap
else
oshmemdir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
