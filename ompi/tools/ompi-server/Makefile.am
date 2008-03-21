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

if OMPI_INSTALL_BINARIES

dist_pkgdata_DATA = help-ompi-server.txt

bin_PROGRAMS = ompi-server

endif

libs = \
        $(top_builddir)/ompi/libmpi.la

ompi_server_SOURCES = \
        ompi-server.c 
ompi_server_LDADD = $(libs)
ompi_server_LDFLAGS = 
ompi_server_DEPENDENCIES = $(libs)

# AM 1.9.6 seems to have a bug in its dependencies for install-man if
# dist_ and nodist_ are used, so explicitly add to EXTRA_DIST...
man_MANS = ompi-server.1
EXTRA_DIST = $(man_MANS)
