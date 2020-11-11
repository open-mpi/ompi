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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
	coll_libpnbc_osc.h \
	coll_libpnbc_osc_component.c \
	pnbc_osc.c \
	pnbc_osc_action_put.c \
	pnbc_osc_trigger.c \
	pnbc_osc_trigger_array.c \
	pnbc_osc_internal.h \
	pnbc_osc_alltoallv_init.c 

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_libpnbc_osc_DSO
component_noinst =
component_install = mca_coll_libpnbc_osc.la
else
component_noinst = libmca_coll_libpnbc_osc.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_libpnbc_osc_la_SOURCES = $(sources)
mca_coll_libpnbc_osc_la_LDFLAGS = -module -avoid-version
mca_coll_libpnbc_osc_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_libpnbc_osc_la_SOURCES =$(sources)
libmca_coll_libpnbc_osc_la_LDFLAGS = -module -avoid-version
