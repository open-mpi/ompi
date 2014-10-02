#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2014      Intel, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

SUBDIRS = config contrib $(MCA_PROJECT_SUBDIRS) test
EXTRA_DIST = README INSTALL VERSION Doxyfile LICENSE autogen.pl autogen.sh \
             README.JAVA.txt

include examples/Makefile.include

dist-hook:
	env LS_COLORS= csh "$(top_srcdir)/config/distscript.csh" "$(top_srcdir)" "$(distdir)" "$(OMPI_VERSION)" "$(OMPI_REPO_REV)"

ACLOCAL_AMFLAGS = -I config
