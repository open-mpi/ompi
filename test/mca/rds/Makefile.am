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
AM_CPPFLAGS = -I$(top_srcdir)/test/support

check_PROGRAMS = \
	test_rds

TESTS = \
	$(check_PROGRAMS)

test_rds_SOURCES = test_rds.c
test_rds_LDADD = \
        $(top_builddir)/test/support/libsupport.a \
        $(top_builddir)/orte/liborte.la \
        $(top_builddir)/opal/libopal.la 
test_rds_DEPENDENCIES = $(test_rds_LDADD)

# JMS Temporarily commented out -- this test only checks the selection
# logic, which is not [yet] well supported by the unit test infrastructure
#test_rds2_SOURCES = test_rds2.c
#test_rds2_LDADD = \
#        $(top_builddir)/test/support/libsupport.a \
#        $(top_builddir)/src/libmpi.la 
#test_rds2_DEPENDENCIES = $(test_rds2_LDADD)
