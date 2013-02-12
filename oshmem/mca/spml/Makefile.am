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
noinst_LTLIBRARIES = libmca_spml.la
libmca_spml_la_SOURCES =

# header setup
nobase_oshmem_HEADERS =
nobase_nodist_oshmem_HEADERS =

# local files
headers = spml.h
libmca_spml_la_SOURCES += $(headers) $(nodist_headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_oshmem_HEADERS += $(headers)
nobase_nodist_oshmem_HEADERS += $(nodist_headers)
oshmemdir = $(includedir)/oshmem/oshmem/mca/spml
else
oshmemdir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h 
