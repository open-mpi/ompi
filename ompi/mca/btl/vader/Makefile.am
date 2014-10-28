#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2009 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(btl_vader_CPPFLAGS)

dist_ompidata_DATA = help-btl-vader.txt

libmca_btl_vader_la_sources = \
    btl_vader_module.c \
    btl_vader.h \
    btl_vader_component.c \
    btl_vader_endpoint.h \
    btl_vader_fifo.h \
    btl_vader_frag.c \
    btl_vader_frag.h \
    btl_vader_send.c \
    btl_vader_sendi.c \
    btl_vader_fbox.h \
    btl_vader_get.c \
    btl_vader_put.c \
    btl_vader_xpmem.c \
    btl_vader_xpmem.h \
    btl_vader_knem.c \
    btl_vader_knem.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_btl_vader_DSO
component_noinst =
component_install = mca_btl_vader.la
else
component_noinst = libmca_btl_vader.la
component_install =
endif

# See ompi/mca/common/vader/Makefile.am for an explanation of
# libmca_common_vader.la.

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_vader_la_SOURCES = $(libmca_btl_vader_la_sources)
mca_btl_vader_la_LDFLAGS = -module -avoid-version $(btl_vader_LDFLAGS)
mca_btl_vader_la_LIBADD = $(btl_vader_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_vader_la_SOURCES = $(libmca_btl_vader_la_sources)
libmca_btl_vader_la_LIBADD = $(btl_vader_LIBS)
libmca_btl_vader_la_LDFLAGS = -module -avoid-version $(btl_vader_LDFLAGS)
