#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

# main library setup
noinst_LTLIBRARIES = libmca_crs.la
libmca_crs_la_SOURCES =

# header setup
nobase_opal_HEADERS =

# local files
headers = crs.h
libmca_crs_la_SOURCES += $(headers)

# Manual pages
dist_man_MANS = opal_crs.7
EXTRA_DIST = $(dist_man_MANS:.7=.7in)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
nobase_opal_HEADERS += $(headers)
opaldir = $(includedir)/openmpi/opal/mca/crs
else
opaldir = $(includedir)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
