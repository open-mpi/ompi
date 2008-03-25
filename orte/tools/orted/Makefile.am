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
# Copyright (c) 2007-2008 Cisco, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

if OMPI_INSTALL_BINARIES

bin_PROGRAMS = orted

dist_man_MANS = orted.1

endif # OMPI_INSTALL_BINARIES

orted_SOURCES = orted.c
# the following empty orted_LDFLAGS is used 
#  so that the orted can be compiled statically 
#  by simply changing the value of this from 
#  nothing to -all-static in the Makefile.in
#  nice for systems that don't have all the shared 
#  libraries on the computes 
orted_LDFLAGS =
orted_LDADD = $(top_builddir)/orte/libopen-rte.la
