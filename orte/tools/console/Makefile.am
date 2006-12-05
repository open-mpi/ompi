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

libs = \
        $(top_builddir)/orte/libopen-rte.la

headers = orteconsole.h

if OMPI_INSTALL_BINARIES

dist_pkgdata_DATA = help-orteconsole.txt

bin_PROGRAMS = \
        orteconsole

endif

orteconsole_SOURCES = \
		$(headers) \
        orteconsole.c 

orteconsole_LDADD = $(libs) 
orteconsole_DEPENDENCIES = $(libs)

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/tools/orteconsole
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif
