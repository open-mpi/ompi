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

# main library setup
noinst_LTLIBRARIES = libmca_smr.la
libmca_smr_la_SOURCES =

# header setup
nobase_orte_HEADERS =

# local files
headers = smr.h smr_types.h
libmca_smr_la_SOURCES += $(headers)

include base/Makefile.am

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_orte_HEADERS += $(headers)
ortedir = $(includedir)/openmpi/orte/mca/smr
else
ortedir = $(includedir)
endif

distclean-local:
	rm -f base/static-components.h
