#
# Copyright (c) 2004-2009 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2014-2015 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if PROJECT_OMPI
    MPI_TESTS = checksum position position_noncontig ddt_test ddt_raw unpack_ooo ddt_pack
    MPI_CHECKS = to_self
endif
TESTS = opal_datatype_test $(MPI_TESTS)

check_PROGRAMS = $(TESTS) $(MPI_CHECKS)

unpack_ooo_SOURCES = unpack_ooo.c ddt_lib.c ddt_lib.h
unpack_ooo_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
unpack_ooo_LDADD = \
        $(top_builddir)/ompi/libmpi.la \
        $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

ddt_test_SOURCES = ddt_test.c ddt_lib.c ddt_lib.h
ddt_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
ddt_test_LDADD = \
        $(top_builddir)/ompi/libmpi.la \
        $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

ddt_raw_SOURCES = ddt_raw.c ddt_lib.c ddt_lib.h
ddt_raw_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
ddt_raw_LDADD = \
        $(top_builddir)/ompi/libmpi.la \
        $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

ddt_pack_SOURCES = ddt_pack.c
ddt_pack_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
ddt_pack_LDADD = \
        $(top_builddir)/ompi/libmpi.la \
        $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

checksum_SOURCES = checksum.c
checksum_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
checksum_LDADD = \
        $(top_builddir)/ompi/libmpi.la \
        $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

position_SOURCES = position.c
position_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
position_LDADD = \
        $(top_builddir)/ompi/libmpi.la \
        $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

position_noncontig_SOURCES = position_noncontig.c
position_noncontig_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
position_noncontig_LDADD = \
        $(top_builddir)/ompi/libmpi.la \
        $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

to_self_SOURCES = to_self.c
to_self_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
to_self_LDADD = $(top_builddir)/ompi/libmpi.la

opal_datatype_test_SOURCES = opal_datatype_test.c opal_ddt_lib.c opal_ddt_lib.h
opal_datatype_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
opal_datatype_test_LDADD = \
        $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

distclean:
	rm -rf *.dSYM .deps .libs *.log *.o *.trs $(check_PROGRAMS) Makefile
