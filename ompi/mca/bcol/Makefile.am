#
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_bcol.la
libmca_bcol_la_SOURCES =

# header setup
nobase_ompi_HEADERS =
nobase_nodist_ompi_HEADERS =

# local files
headers = bcol.h
libmca_bcol_la_SOURCES += $(headers) $(nodist_headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_ompi_HEADERS += $(headers)
nobase_nodist_ompi_HEADERS += $(nodist_headers)
ompidir = $(ompiincludedir)/ompi/mca/bcol
else
ompidir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h 
