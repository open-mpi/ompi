#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = autogen.sh

dist_pkgdata_DATA = help-mpi-btl-tcp2.txt

sources = \
    btl_tcp2.c \
    btl_tcp2.h \
    btl_tcp2_addr.h \
    btl_tcp2_component.c \
    btl_tcp2_endpoint.c \
    btl_tcp2_endpoint.h \
    btl_tcp2_frag.c \
    btl_tcp2_frag.h \
    btl_tcp2_hdr.h \
    btl_tcp2_proc.c \
    btl_tcp2_proc.h \
    btl_tcp2_ft.c \
    btl_tcp2_ft.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = mca_btl_tcp2.la
mca_btl_tcp2_la_SOURCES = $(sources)
mca_btl_tcp2_la_LDFLAGS = -module -avoid-version
