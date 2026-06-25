# -*- makefile -*-
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# These are single-process OMPIO MPI_File info tests.  Each calls
# MPI_Init and performs all of its I/O on MPI_COMM_SELF, so they need no
# launcher and can run as part of 'make check'.  They exercise the OMPIO
# MPI_Info hint reporting required by MPI-5.0 sec. 15.2.8 (Open MPI issue
# #13367).
if PROJECT_OMPI
    check_PROGRAMS = \
        file_info_defaults \
        file_info_hints \
        file_info_set_view \
        file_info_memkind
    TESTS = $(check_PROGRAMS)

    common_headers = ompio_file_info_common.h
    common_ldflags = $(OMPI_PKG_CONFIG_LDFLAGS)
    common_ldadd = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

    file_info_defaults_SOURCES = file_info_defaults.c $(common_headers)
    file_info_defaults_LDFLAGS = $(common_ldflags)
    file_info_defaults_LDADD = $(common_ldadd)

    file_info_hints_SOURCES = file_info_hints.c $(common_headers)
    file_info_hints_LDFLAGS = $(common_ldflags)
    file_info_hints_LDADD = $(common_ldadd)

    file_info_set_view_SOURCES = file_info_set_view.c $(common_headers)
    file_info_set_view_LDFLAGS = $(common_ldflags)
    file_info_set_view_LDADD = $(common_ldadd)

    file_info_memkind_SOURCES = file_info_memkind.c $(common_headers)
    file_info_memkind_LDFLAGS = $(common_ldflags)
    file_info_memkind_LDADD = $(common_ldadd)
endif # PROJECT_OMPI

distclean-local:
	rm -rf *.dSYM .deps .libs *.la *.lo *.log *.o *.trs \
		file_info_defaults file_info_hints file_info_set_view \
		file_info_memkind Makefile
