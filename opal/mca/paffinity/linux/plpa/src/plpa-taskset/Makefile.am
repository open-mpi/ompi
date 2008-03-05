#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Default to empty
bin_PROGRAMS=

AM_CPPFLAGS = -I$(top_srcdir)/src/libplpa
AM_YFLAGS = -d

if PLPA_BUILD_EXECUTABLES
bin_PROGRAMS += plpa-taskset
endif

# Force recognition that tokens.h is generated via tokens.c; there's a
# rule to create tokens.c from tokens.y.
tokens.c: tokens.y
tokens.h: tokens.c

# Force parser.c to be re-compiled when tokens.h is re-generated
parser.c: tokens.h

plpa_taskset_SOURCES = \
        plpa-taskset.c \
        plpa-taskset.h \
        parser.l \
        tokens.y
plpa_taskset_LDADD = $(top_builddir)/src/libplpa/libplpa.la
