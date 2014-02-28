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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


rdma_sources = \
	osc_rdma.h \
	osc_rdma.c \
	osc_rdma_comm.c \
        osc_rdma_component.c \
	osc_rdma_data_move.h \
	osc_rdma_data_move.c \
	osc_rdma_frag.h \
	osc_rdma_frag.c \
	osc_rdma_header.h \
	osc_rdma_obj_convert.h \
	osc_rdma_request.h \
	osc_rdma_request.c \
	osc_rdma_active_target.c \
	osc_rdma_passive_target.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_osc_rdma_DSO
component_noinst =
component_install = mca_osc_rdma.la
else
component_noinst = libmca_osc_rdma.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_osc_rdma_la_SOURCES = $(rdma_sources)
mca_osc_rdma_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_osc_rdma_la_SOURCES = $(rdma_sources)
libmca_osc_rdma_la_LDFLAGS = -module -avoid-version
