# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -g

TESTS_ENVIRONMENT = TEST_WRITE_TO_FILE=1

# JMS Commented out so that the default "make check" will skip these
# (per rhc request)
#check_PROGRAMS = \
bogus = \
    gpr_internal_fns \
    gpr_mem_leaks \
    gpr_overwrite \
    gpr_put_get \
    gpr_threads \
    gpr_triggers

TESTS = 

# JMS Commented out so that the default "make check" will skip these
# (per rhc request)
#gpr_internal_fns_SOURCES = gpr_internal_fns.c
#gpr_internal_fns_LDADD = \
#        $(top_builddir)/src/libmpi.la \
#        $(top_builddir)/test/support/libsupport.a 
#gpr_internal_fns_DEPENDENCIES = $(gpr_internal_fns_LDADD)

#gpr_mem_leaks_SOURCES = gpr_mem_leaks.c
#gpr_mem_leaks_LDADD = \
#        $(top_builddir)/src/libmpi.la \
#        $(top_builddir)/test/support/libsupport.a 
#gpr_mem_leaks_DEPENDENCIES = $(gpr_mem_leaks_LDADD)

#gpr_overwrite_SOURCES = gpr_overwrite.c
#gpr_overwrite_LDADD = \
#        $(top_builddir)/src/libmpi.la \
#        $(top_builddir)/test/support/libsupport.a 
#gpr_overwrite_DEPENDENCIES = $(gpr_overwrite_LDADD)

#gpr_put_get_SOURCES = gpr_put_get.c
#gpr_put_get_LDADD = \
#        $(top_builddir)/src/libmpi.la \
#        $(top_builddir)/test/support/libsupport.a 
#gpr_put_get_DEPENDENCIES = $(gpr_put_get_LDADD)

#gpr_threads_SOURCES = gpr_threads.c
#gpr_threads_LDADD = \
#        $(top_builddir)/src/libmpi.la \
#        $(top_builddir)/test/support/libsupport.a 
#gpr_threads_DEPENDENCIES = $(gpr_threads_LDADD)

#gpr_triggers_SOURCES = gpr_triggers.c
#gpr_triggers_LDADD = \
#        $(top_builddir)/src/libmpi.la \
#        $(top_builddir)/test/support/libsupport.a 
#gpr_triggers_DEPENDENCIES = $(gpr_triggers_LDADD)
