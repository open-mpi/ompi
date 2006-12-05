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
	opal_memory_basic \
	opal_memory_speed \
	opal_memory_cxx

TESTS = \
	$(check_PROGRAMS)

opal_memory_basic_SOURCES = opal_memory_basic.c
opal_memory_basic_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
opal_memory_basic_LDADD = \
        $(top_builddir)/opal/libopen-pal.la
opal_memory_basic_DEPENDENCIES = $(opal_memory_basic_LDADD)

opal_memory_speed_SOURCES = opal_memory_speed.c
opal_memory_speed_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
opal_memory_speed_LDADD = \
        $(top_builddir)/opal/libopen-pal.la
opal_memory_speed_DEPENDENCIES = $(opal_memory_speed_LDADD)

opal_memory_cxx_SOURCES = opal_memory_cxx.cc
opal_memory_cxx_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
opal_memory_cxx_LDADD = \
        $(top_builddir)/opal/libopen-pal.la
opal_memory_cxx_DEPENDENCIES = $(opal_memory_cxx_LDADD)
