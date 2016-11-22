#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

EXTRA_DIST = post_configure.sh

AM_CPPFLAGS = $(mtl_psm2_CPPFLAGS)

dist_ompidata_DATA = help-mtl-psm2.txt

mtl_psm2_sources = \
	       mtl_psm2.c \
	       mtl_psm2.h \
	       mtl_psm2_cancel.c \
	       mtl_psm2_component.c \
	       mtl_psm2_endpoint.c \
	       mtl_psm2_endpoint.h \
	       mtl_psm2_probe.c \
	       mtl_psm2_recv.c \
	       mtl_psm2_request.h \
	       mtl_psm2_send.c \
	       mtl_psm2_types.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_mtl_psm2_DSO
component_noinst =
component_install = mca_mtl_psm2.la
else
component_noinst = libmca_mtl_psm2.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mtl_psm2_la_SOURCES = $(mtl_psm2_sources)
mca_mtl_psm2_la_LIBADD = $(mtl_psm2_LIBS)
mca_mtl_psm2_la_LDFLAGS = -module -avoid-version $(mtl_psm2_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mtl_psm2_la_SOURCES = $(mtl_psm2_sources)
libmca_mtl_psm2_la_LIBADD = $(mtl_psm2_LIBS)
libmca_mtl_psm2_la_LDFLAGS = -module -avoid-version $(mtl_psm2_LDFLAGS)
