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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

check_PROGRAMS = \
	dss_buffer \
        dss_copy \
        dss_set_get \
        dss_inc_dec \
        dss_cmp \
        dss_release \
        dss_size \
        dss_print

TESTS = \
	$(check_PROGRAMS)

dss_buffer_SOURCES = dss_buffer.c
dss_buffer_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
dss_buffer_DEPENDENCIES = $(dss_buffer_LDADD)

dss_copy_SOURCES = dss_copy.c
dss_copy_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
dss_copy_DEPENDENCIES = $(dss_copy_LDADD)

dss_set_get_SOURCES = dss_set_get.c
dss_set_get_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
dss_set_get_DEPENDENCIES = $(dss_set_get_LDADD)

dss_inc_dec_SOURCES = dss_inc_dec.c
dss_inc_dec_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
dss_inc_dec_DEPENDENCIES = $(dss_inc_dec_LDADD)

dss_cmp_SOURCES = dss_cmp.c
dss_cmp_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
dss_cmp_DEPENDENCIES = $(dss_cmp_LDADD)

dss_release_SOURCES = dss_release.c
dss_release_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
dss_release_DEPENDENCIES = $(dss_release_LDADD)

dss_size_SOURCES = dss_size.c
dss_size_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
dss_size_DEPENDENCIES = $(dss_size_LDADD)

dss_print_SOURCES = dss_print.c
dss_print_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
dss_print_DEPENDENCIES = $(dss_print_LDADD)
