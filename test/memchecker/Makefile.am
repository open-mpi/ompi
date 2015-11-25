#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if WANT_MEMCHECKER
check_PROGRAMS = access_buffer_test irecv_init_check irecv_uninit_check non_blocking_send_test\
                 non_blocking_recv_test

else
check_PROGRAMS =
endif

TESTS = \
	$(check_PROGRAMS)

access_buffer_test_SOURCES = access_buffer_test.c
access_buffer_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
access_buffer_test_LDADD = \
        $(top_builddir)/ompi/libmpi.la
access_buffer_test_DEPENDENCIES = $(access_buffer_test_LDADD)

irecv_init_check_SOURCES = irecv_init_check.c
irecv_init_check_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
irecv_init_check_LDADD = \
        $(top_builddir)/ompi/libmpi.la
irecv_init_check_DEPENDENCIES = $(irecv_init_check_LDADD)


irecv_uninit_check_SOURCES = irecv_uninit_check.c
irecv_uninit_check_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
irecv_uninit_check_LDADD = \
        $(top_builddir)/ompi/libmpi.la
irecv_uninit_check_DEPENDENCIES = $(irecv_uninit_check_LDADD)

non_blocking_send_test_SOURCES = non_blocking_send_test.c
non_blocking_send_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
non_blocking_send_test_LDADD = \
        $(top_builddir)/ompi/libmpi.la
non_blocking_send_test_DEPENDENCIES = $(non_blocking_send_test_LDADD)


non_blocking_recv_test_SOURCES = non_blocking_recv_test.c
non_blocking_recv_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
non_blocking_recv_test_LDADD = \
        $(top_builddir)/ompi/libmpi.la
non_blocking_recv_test_DEPENDENCIES = $(non_blocking_recv_test_LDADD)

distclean:
	rm -rf *.dSYM .deps *.log *.o *.trs $(check_PROGRAMS) Makefile
