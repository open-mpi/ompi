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
# Copyright (c) 2008      Cisco, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

man_pages = opal-restart.1
EXTRA_DIST = $(man_pages) $(man_pages:.1=.1in)

if OMPI_INSTALL_BINARIES

bin_PROGRAMS = opal-restart

man_MANS = $(man_pages)

dist_pkgdata_DATA = help-opal-restart.txt

endif # OMPI_INSTALL_BINARIES

opal_restart_SOURCES = opal-restart.c
opal_restart_LDADD = $(top_builddir)/opal/libopen-pal.la

maintainer-clean-local:
	rm -f $(man_pages)
