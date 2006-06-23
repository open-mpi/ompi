#
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

TESTS = ddt_test checksum position

check_PROGRAMS = $(TESTS) to_self

ddt_test_SOURCES = ddt_test.c
ddt_test_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
ddt_test_LDADD = $(top_builddir)/ompi/libmpi.la

checksum_SOURCES = checksum.c
checksum_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
checksum_LDADD = $(top_builddir)/ompi/libmpi.la

position_SOURCES = position.c
position_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
position_LDADD = $(top_builddir)/ompi/libmpi.la

to_self_SOURCES = to_self.c
to_self_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
to_self_LDADD = $(top_builddir)/ompi/libmpi.la
