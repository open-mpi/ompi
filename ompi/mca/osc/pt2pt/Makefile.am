#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

pt2pt_sources = \
	osc_pt2pt.h \
	osc_pt2pt_module.c \
	osc_pt2pt_comm.c \
        osc_pt2pt_component.c \
	osc_pt2pt_data_move.h \
	osc_pt2pt_data_move.c \
	osc_pt2pt_frag.h \
	osc_pt2pt_frag.c \
	osc_pt2pt_header.h \
        osc_pt2pt_pending_frag.h \
	osc_pt2pt_request.h \
	osc_pt2pt_request.c \
	osc_pt2pt_active_target.c \
	osc_pt2pt_passive_target.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_osc_pt2pt_DSO
component_noinst =
component_install = mca_osc_pt2pt.la
else
component_noinst = libmca_osc_pt2pt.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_osc_pt2pt_la_SOURCES = $(pt2pt_sources)
mca_osc_pt2pt_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_osc_pt2pt_la_SOURCES = $(pt2pt_sources)
libmca_osc_pt2pt_la_LDFLAGS = -module -avoid-version
