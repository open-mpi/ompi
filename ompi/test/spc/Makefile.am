#
# Copyright (c) 2018 The University of Tennessee and The University
#                    of Tennessee Research Foundation.  All rights
#                    reserved.
# Copyright (c) 2026 Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This test requires multiple processes to run. Don't run it as part
# of 'make check'
noinst_PROGRAMS = spc_test
spc_test_SOURCES = spc_test.c
spc_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
# spc_test uses MPI_* APIs (libmpi) but also references internal OMPI
# symbols (e.g. predefined datatypes such as ompi_mpi_int, pulled in
# via MPI_* macro expansion) that live in libopen_mpi.  libopen_mpi is
# a dependency of libmpi, but the linker does not always pull it in
# transitively (e.g. in some uninstalled "make check" / distro linker
# configurations), so link it explicitly.
spc_test_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/ompi/libopen_mpi.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

distclean-local:
	rm -rf *.dSYM .deps .libs *.la *.lo spc_test prof *.log *.o *.trs Makefile
