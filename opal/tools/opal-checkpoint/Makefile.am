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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

if OMPI_INSTALL_BINARIES

bin_PROGRAMS = opal-checkpoint

dist_man_MANS = opal-checkpoint.1

dist_pkgdata_DATA = help-opal-checkpoint.txt

endif # OMPI_INSTALL_BINARIES

opal_checkpoint_SOURCES = opal-checkpoint.c
opal_checkpoint_LDADD = $(top_builddir)/opal/libopen-pal.la

