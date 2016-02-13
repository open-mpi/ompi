# -*- makefile -*-
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
# Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from orte/Makefile.am

dist_ortedata_DATA += orted/help-orted.txt

headers += \
	orted/orted.h \
	orted/orted_submit.h

lib@ORTE_LIB_PREFIX@open_rte_la_SOURCES += \
        orted/orted_main.c \
        orted/orted_comm.c \
        orted/orted_submit.c

include orted/pmix/Makefile.am
