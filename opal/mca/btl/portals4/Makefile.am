#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010-2012 Sandia National Laboratories.  All rights reserved.
# Copyright (c) 2014      Bull SAS.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

AM_CPPFLAGS = $(btl_portals4_CPPFLAGS)

if MCA_BUILD_opal_btl_portals4_DSO
component_noinst =
component_install = mca_btl_portals4.la
else
component_noinst = libmca_btl_portals4.la
component_install =
endif

local_sources = \
	btl_portals4.c \
	btl_portals4.h \
	btl_portals4_component.c \
	btl_portals4_frag.c \
	btl_portals4_frag.h \
	btl_portals4_rdma.c \
	btl_portals4_recv.c \
	btl_portals4_recv.h \
	btl_portals4_send.c

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_portals4_la_SOURCES = $(local_sources)
mca_btl_portals4_la_LIBADD = \
    $(btl_portals4_LIBS)
mca_btl_portals4_la_LDFLAGS = -module -avoid-version $(btl_portals4_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_portals4_la_SOURCES = $(local_sources)
libmca_btl_portals4_la_LIBADD = $(btl_portals4_LIBS)
libmca_btl_portals4_la_LDFLAGS = -module -avoid-version $(btl_portals4_LDFLAGS)
