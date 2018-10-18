# -*- makefile -*-
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
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from ompi/Makefile.am

headers += \
        request/grequest.h \
        request/request_default.h \
        request/request.h \
	request/request_dbg.h

if OMPI_ENABLE_GREQUEST_EXTENSIONS
headers += \
        request/grequestx.h
endif

lib@OMPI_LIBMPI_NAME@_la_SOURCES += \
        request/grequest.c \
        request/request.c \
        request/req_test.c \
        request/req_wait.c

if OMPI_ENABLE_GREQUEST_EXTENSIONS
lib@OMPI_LIBMPI_NAME@_la_SOURCES += \
        request/grequestx.c
endif
