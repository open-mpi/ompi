#
# Copyright (c) 2004-2009 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2006-2008 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

TESTS = checksum position ddt_test ddt_raw opal_datatype_test

check_PROGRAMS = $(TESTS) to_self ddt_pack

# This test currently not built because of #1242.
ddt_test_SOURCES = ddt_test.c ddt_lib.c ddt_lib.h
ddt_test_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
ddt_test_LDADD = $(top_builddir)/ompi/libmpi.la

ddt_raw_SOURCES = ddt_raw.c ddt_lib.c ddt_lib.h
ddt_raw_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
ddt_raw_LDADD = $(top_builddir)/ompi/libmpi.la

ddt_pack_SOURCES = ddt_pack.c
ddt_pack_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
ddt_pack_LDADD = $(top_builddir)/ompi/libmpi.la

checksum_SOURCES = checksum.c
checksum_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
checksum_LDADD = $(top_builddir)/ompi/libmpi.la

position_SOURCES = position.c
position_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
position_LDADD = $(top_builddir)/ompi/libmpi.la

to_self_SOURCES = to_self.c
to_self_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
to_self_LDADD = $(top_builddir)/ompi/libmpi.la

opal_datatype_test_SOURCES = opal_datatype_test.c
opal_datatype_test_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
opal_datatype_test_LDADD = opal_ddt_lib.o $(top_builddir)/opal/libopen-pal.la

