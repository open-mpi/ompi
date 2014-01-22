#
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

not_used_yet = 

sources = \
        bcol_basesmuma.h \
        bcol_basesmuma_utils.h \
        bcol_basesmuma_bcast.c \
        bcol_basesmuma_component.c  \
        bcol_basesmuma_module.c \
        bcol_basesmuma_buf_mgmt.c \
        bcol_basesmuma_mem_mgmt.c \
	bcol_basesmuma_fanin.c \
        bcol_basesmuma_fanout.c \
        bcol_basesmuma_progress.c \
        bcol_basesmuma_reduce.h \
        bcol_basesmuma_reduce.c \
        bcol_basesmuma_allreduce.c \
        bcol_basesmuma_setup.c \
	bcol_basesmuma_rd_barrier.c  \
        bcol_basesmuma_rd_nb_barrier.c \
        bcol_basesmuma_rk_barrier.c \
        bcol_basesmuma_utils.c    \
        bcol_basesmuma_bcast_prime.c \
        bcol_basesmuma_lmsg_knomial_bcast.c \
        bcol_basesmuma_lmsg_bcast.c \
        bcol_basesmuma_gather.c \
        bcol_basesmuma_allgather.c \
        bcol_basesmuma_smcm.h \
        bcol_basesmuma_smcm.c 

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_bcol_basesmuma_DSO
component_install += mca_bcol_basesmuma.la
else
component_noinst += libmca_bcol_basesmuma.la
endif

# See ompi/mca/btl/sm/Makefile.am for an explanation of
# libmca_common_sm.la.

AM_CPPFLAGS = $(btl_portals_CPPFLAGS)

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_bcol_basesmuma_la_SOURCES = $(sources)
mca_bcol_basesmuma_la_LDFLAGS = -module -avoid-version $(btl_portals_LDFLAGS)
mca_bcol_basesmuma_la_LIBADD = \
    	$(btl_portals_LIBS)


noinst_LTLIBRARIES = $(component_noinst)
libmca_bcol_basesmuma_la_SOURCES =$(sources)
libmca_bcol_basesmuma_la_LDFLAGS = -module -avoid-version $(btl_portals_LDFLAGS)
