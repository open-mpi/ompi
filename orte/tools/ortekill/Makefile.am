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

libs = \
        $(top_builddir)/orte/liborte.la

ortekill_SOURCES = \
        ortekill.c

ortekill_LDADD = $(libs) 
ortekill_DEPENDENCIES = $(libs)

if OMPI_INSTALL_BINARIES

bin_PROGRAMS = ortekill

dist_pkgdata_DATA = help-ortekill.txt

# AM 1.9.6 seems to have a bug in it's dependencies for install-man if
#dist_ and nodist_ are used, so explicitly add to EXTRA_DIST...
man_MANS = ortekill.1
EXTRA_DIST = ortekill.1

endif
