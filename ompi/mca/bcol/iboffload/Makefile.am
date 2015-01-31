#
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2012-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(bcol_iboffload_CPPFLAGS) $(btl_openib_CPPFLAGS)

sources = \
        bcol_iboffload.h \
        bcol_iboffload_device.h \
        bcol_iboffload_module.c \
        bcol_iboffload_mca.h \
        bcol_iboffload_mca.c \
        bcol_iboffload_endpoint.h \
        bcol_iboffload_endpoint.c \
        bcol_iboffload_frag.h \
        bcol_iboffload_frag.c \
        bcol_iboffload_collfrag.h \
        bcol_iboffload_collfrag.c \
        bcol_iboffload_task.h \
        bcol_iboffload_task.c \
        bcol_iboffload_component.c \
        bcol_iboffload_barrier.c \
        bcol_iboffload_bcast.h \
        bcol_iboffload_bcast.c \
        bcol_iboffload_allgather.c \
        bcol_iboffload_collreq.h \
        bcol_iboffload_collreq.c \
        bcol_iboffload_qp_info.c \
        bcol_iboffload_qp_info.h \
        bcol_iboffload_fanin.c \
        bcol_iboffload_fanout.c \
        bcol_iboffload_allreduce.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_bcol_iboffload_DSO
component_install += mca_bcol_iboffload.la
else
component_noinst += libmca_bcol_iboffload.la
endif

# See ompi/mca/btl/sm/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_bcol_iboffload_la_SOURCES = $(sources)
mca_bcol_iboffload_la_LDFLAGS = -module -avoid-version $(btl_openib_LDFLAGS) $(bcol_iboffload_LDFLAGS)
mca_bcol_iboffload_la_LIBADD = $(btl_openib_LIBS) $(bcol_iboffload_LIBS) \
        $(OMPI_TOP_BUILDDIR)/ompi/mca/common/ofacm/libmca_common_ofacm.la \
        $(OMPI_TOP_BUILDDIR)/ompi/mca/common/verbs/libmca_common_verbs.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_bcol_iboffload_la_SOURCES =$(sources)
libmca_bcol_iboffload_la_LDFLAGS = -module -avoid-version  $(btl_openib_LDFLAGS) $(bcol_iboffload_LDFLAGS)
