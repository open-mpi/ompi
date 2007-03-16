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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

nodist_pkgdata_DATA = 
dist_pkgdata_DATA = \
	help-opal-restart.txt

bin_PROGRAMS = opal-restart

opal_restart_SOURCES = \
	opal-restart.c

# Manual pages
man_MANS = opal-restart.1
EXTRA_DIST = opal-restart.1

opal_restart_LDADD = $(top_builddir)/opal/libopen-pal.la
opal_restart_DEPENDENCIES = $(top_builddir)/opal/libopen-pal.la

