#
# Copyright (c) 2004-2009 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2014-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# Single-process tests of the OMPI datatype engine (ompi/datatype) and
# the MPI datatype API.  The OPAL-level datatype tests, which link only
# against libopen-pal, remain in test/datatype.
#

# mpi_datatype_bigcount calls MPI_Init().  Make "make check" work in an
# uninstalled --enable-mca-dso build tree.
include $(top_srcdir)/Makefile.mca-dso-check

# to_self and reduce_local are built but not run, as they were in
# test/datatype before this directory existed.  (reduce_local is driven
# by getopt() command-line arguments; see check_op.sh.)
TESTS = \
        position \
        position_noncontig \
        ddt_test \
        ddt_raw \
        ddt_raw2 \
        unpack_ooo \
        ddt_pack \
        resized_extent \
        external32 \
        large_data \
        partial \
        ompi_datatype_bigcount \
        mpi_datatype_bigcount \
        sizedtypes \
        opt_desc_equiv

MPI_CHECKS = to_self pack_description_sweep reduce_local

check_PROGRAMS = $(TESTS) $(MPI_CHECKS)

common_ldflags = $(OMPI_PKG_CONFIG_LDFLAGS)
common_ldadd = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

unpack_ooo_SOURCES = unpack_ooo.c ddt_lib.c ddt_lib.h
unpack_ooo_LDFLAGS = $(common_ldflags)
unpack_ooo_LDADD = $(common_ldadd)

ddt_test_SOURCES = ddt_test.c ddt_lib.c ddt_lib.h
ddt_test_LDFLAGS = $(common_ldflags)
ddt_test_LDADD = $(common_ldadd)

ddt_raw_SOURCES = ddt_raw.c ddt_lib.c ddt_lib.h
ddt_raw_LDFLAGS = $(common_ldflags)
ddt_raw_LDADD = $(common_ldadd)

ddt_raw2_SOURCES = ddt_raw2.c ddt_lib.c ddt_lib.h
ddt_raw2_LDFLAGS = $(common_ldflags)
ddt_raw2_LDADD = $(common_ldadd)

ddt_pack_SOURCES = ddt_pack.c
ddt_pack_LDFLAGS = $(common_ldflags)
ddt_pack_LDADD = $(common_ldadd)

resized_extent_SOURCES = resized_extent.c
resized_extent_LDFLAGS = $(common_ldflags)
resized_extent_LDADD = $(common_ldadd)

position_SOURCES = position.c
position_LDFLAGS = $(common_ldflags)
position_LDADD = $(common_ldadd)

position_noncontig_SOURCES = position_noncontig.c
position_noncontig_LDFLAGS = $(common_ldflags)
position_noncontig_LDADD = $(common_ldadd)

to_self_SOURCES = to_self.c datatype_corpus.c datatype_corpus.h
to_self_LDFLAGS = $(common_ldflags)
to_self_LDADD = $(common_ldadd)

opt_desc_equiv_SOURCES = opt_desc_equiv.c datatype_corpus.c datatype_corpus.h
opt_desc_equiv_LDFLAGS = $(common_ldflags)
opt_desc_equiv_LDADD = $(common_ldadd)

pack_description_sweep_SOURCES = pack_description_sweep.c
pack_description_sweep_LDFLAGS = $(common_ldflags)
pack_description_sweep_LDADD = $(common_ldadd)

large_data_SOURCES = large_data.c
large_data_LDFLAGS = $(common_ldflags)
large_data_LDADD = $(common_ldadd)

ompi_datatype_bigcount_SOURCES = ompi_datatype_bigcount.c
ompi_datatype_bigcount_LDFLAGS = $(common_ldflags)
ompi_datatype_bigcount_LDADD = $(common_ldadd)

mpi_datatype_bigcount_SOURCES = mpi_datatype_bigcount.c
mpi_datatype_bigcount_LDFLAGS = $(common_ldflags)
mpi_datatype_bigcount_LDADD = $(common_ldadd)

external32_SOURCES = external32.c
external32_LDFLAGS = $(common_ldflags)
external32_LDADD = $(common_ldadd)

reduce_local_SOURCES = reduce_local.c
reduce_local_LDFLAGS = $(common_ldflags)
reduce_local_LDADD = $(common_ldadd)

partial_SOURCES = partial.c
partial_LDFLAGS = $(common_ldflags)
partial_LDADD = $(common_ldadd)

sizedtypes_SOURCES = sizedtypes.c
sizedtypes_LDFLAGS = $(common_ldflags)
sizedtypes_LDADD = $(common_ldadd)

distclean-local:
	rm -rf *.dSYM .deps .libs *.log *.o *.trs $(check_PROGRAMS) Makefile
