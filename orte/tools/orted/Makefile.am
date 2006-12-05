#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

if OMPI_INSTALL_BINARIES

dist_pkgdata_DATA = help-orted.txt

bin_PROGRAMS = orted

endif

libs = \
        $(top_builddir)/orte/libopen-rte.la

headers = \
	orted.h

orted_SOURCES = \
	$(headers) \
        orted.c 

orted_LDADD = $(libs)
orted_LDFLAGS =
orted_DEPENDENCIES = $(libs)


# Conditionally install the header files

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/tools/orted
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif
