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
# Copyright (c) 2014      Intel, Inc. All rights reserved
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

AM_CPPFLAGS = $(mtl_portals4_CPPFLAGS)

EXTRA_DIST = post_configure.sh

if MCA_BUILD_ompi_mtl_portals4_DSO
component_noinst =
component_install = mca_mtl_portals4.la
else
component_noinst = libmca_mtl_portals4.la
component_install =
endif

local_sources = \
	mtl_portals4.c \
	mtl_portals4.h \
	mtl_portals4_cancel.c \
	mtl_portals4_component.c \
    mtl_portals4_endpoint.h \
	mtl_portals4_message.c \
	mtl_portals4_message.h \
	mtl_portals4_probe.c \
	mtl_portals4_recv.c \
	mtl_portals4_recv_short.c \
	mtl_portals4_recv_short.h \
	mtl_portals4_request.h \
	mtl_portals4_send.c

if OMPI_MTL_PORTALS4_FLOW_CONTROL
local_sources += \
	mtl_portals4_flowctl.h \
	mtl_portals4_flowctl.c
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mtl_portals4_la_SOURCES = $(local_sources)
mca_mtl_portals4_la_LIBADD = $(mtl_portals4_LIBS)
mca_mtl_portals4_la_LDFLAGS = -module -avoid-version $(mtl_portals4_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mtl_portals4_la_SOURCES = $(local_sources)
libmca_mtl_portals4_la_LIBADD = $(mtl_portals4_LIBS)
libmca_mtl_portals4_la_LDFLAGS = -module -avoid-version $(mtl_portals4_LDFLAGS)
