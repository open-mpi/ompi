# -*- makefile -*-
#
# Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2012-2013 Inria.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules


if OMPI_FORTRAN_HAVE_TS

AM_CPPFLAGS = -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

noinst_LTLIBRARIES = libusempif08_ts.la

endif

libusempif08_ts_la_SOURCES = \
        bindings.h \
        ts.h \
        ts.c \
        \
        bsend_ts.c \
        bsend_init_ts.c \
        buffer_attach_ts.c \
        f_sync_reg_ts.c \
        free_mem_ts.c \
        get_address_ts.c \
        ibsend_ts.c \
        imrecv_ts.c \
        irecv_ts.c \
        irsend_ts.c \
        isend_ts.c \
        issend_ts.c \
        mrecv_ts.c \
        recv_ts.c \
        recv_init_ts.c \
        rsend_ts.c \
        rsend_init_ts.c \
        send_ts.c \
        send_init_ts.c \
        sendrecv_ts.c \
        sendrecv_replace_ts.c \
        ssend_ts.c \
        ssend_init_ts.c
