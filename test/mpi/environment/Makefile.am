# -*- makefile -*-
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
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_LDFLAGS = -lpthread

noinst_PROGRAMS = chello

chello_SOURCES = chello.c
chello_LDADD = \
	$(top_builddir)/src/libmpi.la \
	$(top_builddir)/src/libompi.la
chello_DEPENDENCIES = $(chello_LDADD)


distclean:
	rm -rf *.dSYM .deps *.log *.o *.trs $(noinst_PROGRAMS) Makefile
