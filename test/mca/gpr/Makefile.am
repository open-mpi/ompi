# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1 -g

noinst_PROGRAMS = \
    gpr_internal_fns \
    gpr_mem_leaks \
    gpr_overwrite \
    gpr_put_get \
    gpr_triggers

gpr_internals_SOURCES = gpr_internals.c
gpr_internals_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
gpr_internals_DEPENDENCIES = $(gpr_internals_LDADD)

gpr_mem_leaks_SOURCES = gpr_mem_leaks.c
gpr_mem_leaks_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
gpr_mem_leaks_DEPENDENCIES = $(gpr_mem_leaks_LDADD)

gpr_overwrite_SOURCES = gpr_overwrite.c
gpr_overwrite_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
gpr_overwrite_DEPENDENCIES = $(gpr_overwrite_LDADD)

gpr_put_get_SOURCES = gpr_put_get.c
gpr_put_get_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
gpr_put_get_DEPENDENCIES = $(gpr_put_get_LDADD)

gpr_triggers_SOURCES = gpr_triggers.c
gpr_triggers_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
gpr_triggers_DEPENDENCIES = $(gpr_triggers_LDADD)
