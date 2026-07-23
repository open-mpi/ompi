#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# General unit tests for the OMPI layer (ompi/* top-level directories).
# Sibling directories under ompi/test/ are named for the single MPI
# object or interface that they exercise; this one is the catch-all, with
# one test program per OMPI-layer module.  Each test is a self-checking
# single-process program (no launcher required) that reports through the
# local harness in support.c.
#

# Nearly every test here calls MPI_Init(), and the file test additionally
# opens an MPI_File (io/ompio, which depends on libmca_common_ompio).
# Make "make check" work in an uninstalled --enable-mca-dso build tree.
include $(top_srcdir)/Makefile.mca-dso-check

# A self-contained copy of the test/support harness so that these tests
# do not depend on the top-level test/ tree.
EXTRA_DIST = README.md attr_interlang_source.F90 values-f.h

check_LIBRARIES = libompitest_support.a
libompitest_support_a_SOURCES = support.c support.h

# NOTE: test program names must avoid Automake's built-in recursive
# target names (info, dvi, ps, pdf, html, tags, check, ...).  The
# ompi/info test is therefore named "info_t" rather than "info".
check_PROGRAMS = \
	info_t \
	seq_tracker \
	mpi_init \
	op \
	datatype \
	communicator \
	proc \
	group \
	attribute \
	errhandler \
	info_mpi \
	request \
	win \
	instance \
	file \
	message \
	abi_fortran

TESTS = $(check_PROGRAMS)

# Common link line for OMPI-level unit tests.  These tests call MPI_*
# APIs (libmpi), but they also call internal OMPI routines directly
# (e.g. ompi_info_get(), ompi_datatype_*()) and reference predefined
# objects such as ompi_mpi_int that MPI_* macros expand to; all of those
# live in libopen_mpi.  libopen_mpi is a dependency of libmpi, but the
# linker does not always pull it in transitively (e.g. in some
# uninstalled "make check" / distro linker configurations), so link it
# explicitly.  libopen-pal underlies both.
ompi_test_ldadd = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/ompi/libopen_mpi.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
        libompitest_support.a

info_t_SOURCES = info.c
info_t_LDADD = $(ompi_test_ldadd)
info_t_DEPENDENCIES = $(ompi_test_ldadd)

seq_tracker_SOURCES = seq_tracker.c
seq_tracker_LDADD = $(ompi_test_ldadd)
seq_tracker_DEPENDENCIES = $(ompi_test_ldadd)

mpi_init_SOURCES = mpi_init.c
mpi_init_LDADD = $(ompi_test_ldadd)
mpi_init_DEPENDENCIES = $(ompi_test_ldadd)

op_SOURCES = op.c
op_LDADD = $(ompi_test_ldadd)
op_DEPENDENCIES = $(ompi_test_ldadd)

datatype_SOURCES = datatype.c
datatype_LDADD = $(ompi_test_ldadd)
datatype_DEPENDENCIES = $(ompi_test_ldadd)

communicator_SOURCES = communicator.c
communicator_LDADD = $(ompi_test_ldadd)
communicator_DEPENDENCIES = $(ompi_test_ldadd)

proc_SOURCES = proc.c
proc_LDADD = $(ompi_test_ldadd)
proc_DEPENDENCIES = $(ompi_test_ldadd)

group_SOURCES = group.c
group_LDADD = $(ompi_test_ldadd)
group_DEPENDENCIES = $(ompi_test_ldadd)

attribute_SOURCES = attribute.c
attribute_LDADD = $(ompi_test_ldadd)
attribute_DEPENDENCIES = $(ompi_test_ldadd)

errhandler_SOURCES = errhandler.c
errhandler_LDADD = $(ompi_test_ldadd)
errhandler_DEPENDENCIES = $(ompi_test_ldadd)

info_mpi_SOURCES = info_mpi.c
info_mpi_LDADD = $(ompi_test_ldadd)
info_mpi_DEPENDENCIES = $(ompi_test_ldadd)

request_SOURCES = request.c
request_LDADD = $(ompi_test_ldadd)
request_DEPENDENCIES = $(ompi_test_ldadd)

win_SOURCES = win.c
win_LDADD = $(ompi_test_ldadd)
win_DEPENDENCIES = $(ompi_test_ldadd)

instance_SOURCES = instance.c
instance_LDADD = $(ompi_test_ldadd)
instance_DEPENDENCIES = $(ompi_test_ldadd)

file_SOURCES = file.c
file_LDADD = $(ompi_test_ldadd)
file_DEPENDENCIES = $(ompi_test_ldadd)

message_SOURCES = message.c
message_LDADD = $(ompi_test_ldadd)
message_DEPENDENCIES = $(ompi_test_ldadd)

abi_fortran_SOURCES = abi_fortran.c
abi_fortran_LDADD = $(ompi_test_ldadd)
abi_fortran_DEPENDENCIES = $(ompi_test_ldadd)

# The Fortran attribute test drives the Fortran attribute back-end
# (ompi_attr_create_keyval_fint / set_fint / get_fint), which is only
# reachable through the mpif.h bindings.  Build it only when those
# bindings are present.
# --------------------------------------------------------------------------
# C <-> Fortran attribute interlanguage test.  Ported from
# open-mpi/ompi-tests simple/attr: a Fortran driver
# (attr_interlang_source.F90, a template) plus a shared C back-end
# (attr_interlang.c) exhaustively exercise the 9 cases of setting an
# attribute in one language and reading it in another (C, Fortran MPI-1
# default-INTEGER, Fortran MPI-2 address-kind), for communicators,
# datatypes, and windows.  Single process (MPI_COMM_SELF / MPI_INTEGER /
# a local window).  Built in all three Fortran flavors, each generated
# from the template by sed.  (The C back-end uses deprecated MPI-1
# attribute calls on purpose; it silences the deprecation warnings itself,
# after including ompi_config.h, rather than via a -D here.)
# --------------------------------------------------------------------------
# Empty, but kept so the shared attr_interlang.c object is named
# per-program (each flavor compiles it with its own Fortran flags).
attr_interlang_cppflags =
# Fortran INCLUDE path: generated mpif.h (build tree) + its mpif-*.h
# fragments (source tree) + values-f.h (this directory).
attr_interlang_fcflags = -I$(top_builddir)/ompi/include \
	-I$(top_srcdir)/ompi/include -I$(srcdir)

CLEANFILES = attr_interlang_mpifh.f90 attr_interlang_usempi.f90 \
	attr_interlang_usempif08.f90

if OMPI_BUILD_FORTRAN_MPIFH_BINDINGS
check_PROGRAMS += attr_interlang_mpifh
attr_interlang_mpifh_SOURCES = attr_interlang.c
nodist_attr_interlang_mpifh_SOURCES = attr_interlang_mpifh.f90
attr_interlang_mpifh_CPPFLAGS = $(attr_interlang_cppflags)
attr_interlang_mpifh_FCFLAGS = $(attr_interlang_fcflags)
attr_interlang_mpifh_LDADD = \
	$(top_builddir)/ompi/mpi/fortran/mpif-h/libmpi_mpifh.la \
	$(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(top_builddir)/ompi/libopen_mpi.la \
	$(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
attr_interlang_mpifh_DEPENDENCIES = $(attr_interlang_mpifh_LDADD)
attr_interlang_mpifh.f90: $(srcdir)/attr_interlang_source.F90
	$(AM_V_GEN)rm -f $@; sed \
	  -e 's/FORTRAN_SUB_LINE1/implicit none/g' \
	  -e 's@FORTRAN_SUB_LINE2@include "mpif.h"@g' \
	  -e 's/FORTRAN_[A-Z]*_TYPE/integer/g' \
	  $(srcdir)/attr_interlang_source.F90 > $@
endif

if OMPI_BUILD_FORTRAN_USEMPI_IGNORE_TKR_BINDINGS
check_PROGRAMS += attr_interlang_usempi
attr_interlang_usempi_SOURCES = attr_interlang.c
nodist_attr_interlang_usempi_SOURCES = attr_interlang_usempi.f90
attr_interlang_usempi_CPPFLAGS = $(attr_interlang_cppflags)
attr_interlang_usempi_FCFLAGS = $(attr_interlang_fcflags) \
	-I$(top_builddir)/ompi/mpi/fortran/use-mpi-ignore-tkr
attr_interlang_usempi_LDADD = \
	$(top_builddir)/ompi/mpi/fortran/use-mpi-ignore-tkr/libmpi_usempi_ignore_tkr.la \
	$(top_builddir)/ompi/mpi/fortran/mpif-h/libmpi_mpifh.la \
	$(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(top_builddir)/ompi/libopen_mpi.la \
	$(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
attr_interlang_usempi_DEPENDENCIES = $(attr_interlang_usempi_LDADD)
attr_interlang_usempi.f90: $(srcdir)/attr_interlang_source.F90
	$(AM_V_GEN)rm -f $@; sed \
	  -e 's/FORTRAN_SUB_LINE1/use mpi/g' \
	  -e 's/FORTRAN_SUB_LINE2/implicit none/g' \
	  -e 's/FORTRAN_[A-Z]*_TYPE/integer/g' \
	  $(srcdir)/attr_interlang_source.F90 > $@
endif

if OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS
check_PROGRAMS += attr_interlang_usempif08
attr_interlang_usempif08_SOURCES = attr_interlang.c
nodist_attr_interlang_usempif08_SOURCES = attr_interlang_usempif08.f90
attr_interlang_usempif08_CPPFLAGS = $(attr_interlang_cppflags)
attr_interlang_usempif08_FCFLAGS = $(attr_interlang_fcflags) \
	-I$(top_builddir)/ompi/mpi/fortran/use-mpi-f08
attr_interlang_usempif08_LDADD = \
	$(top_builddir)/ompi/mpi/fortran/use-mpi-f08/libmpi_usempif08.la \
	$(top_builddir)/ompi/mpi/fortran/use-mpi-ignore-tkr/libmpi_usempi_ignore_tkr.la \
	$(top_builddir)/ompi/mpi/fortran/mpif-h/libmpi_mpifh.la \
	$(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(top_builddir)/ompi/libopen_mpi.la \
	$(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
attr_interlang_usempif08_DEPENDENCIES = $(attr_interlang_usempif08_LDADD)
attr_interlang_usempif08.f90: $(srcdir)/attr_interlang_source.F90
	$(AM_V_GEN)rm -f $@; sed \
	  -e 's/FORTRAN_SUB_LINE1/use mpi_f08/g' \
	  -e 's/FORTRAN_SUB_LINE2/implicit none/g' \
	  -e 's/FORTRAN_COMM_TYPE/type(MPI_Comm)/g' \
	  -e 's/FORTRAN_WIN_TYPE/type(MPI_Win)/g' \
	  -e 's/FORTRAN_DATATYPE_TYPE/type(MPI_Datatype)/g' \
	  -e 's/MPI_NULL_COPY_FN/MPI_COMM_NULL_COPY_FN/g' \
	  -e 's/MPI_NULL_DELETE_FN/MPI_COMM_NULL_DELETE_FN/g' \
	  $(srcdir)/attr_interlang_source.F90 > $@
endif

distclean-local:
	rm -rf *.dSYM .deps .libs *.log *.o *.trs $(check_PROGRAMS) Makefile
