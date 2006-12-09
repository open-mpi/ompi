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

# This is not quite in the Automake spirit, but we have to do it.
# Since the totalview portion of the library must be built with -g, we
# must eliminate the CFLAGS that are passed in here by default (which
# may already have debugging and/or optimization flags).  We use
# post-processed forms of the CFLAGS in the library targets down
# below.

CFLAGS = $(CFLAGS_WITHOUT_OPTFLAGS) $(TOTALVIEW_DEBUG_FLAGS)

libs = \
        $(top_builddir)/orte/libopen-rte.la

orterun_SOURCES = \
        main.c \
        orterun.c \
        orterun.h \
        totalview.c \
        totalview.h 

orterun_LDADD = $(libs) 
orterun_DEPENDENCIES = $(libs)

if OMPI_INSTALL_BINARIES

bin_PROGRAMS = orterun

dist_pkgdata_DATA = help-orterun.txt

# AM 1.9.6 seems to have a bug in it's dependencies for install-man if
#dist_ and nodist_ are used, so explicitly add to EXTRA_DIST...
man_MANS = orterun.1
EXTRA_DIST = orterun.1

endif
