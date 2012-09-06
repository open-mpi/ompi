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
        bcol_ptpcoll.h \
        bcol_ptpcoll_utils.h \
        bcol_ptpcoll_utils.c \
        bcol_ptpcoll_mca.h \
        bcol_ptpcoll_mca.c \
        bcol_ptpcoll_barrier.c \
        bcol_ptpcoll_bcast.c \
        bcol_ptpcoll_bcast.h \
        bcol_ptpcoll_component.c  \
        bcol_ptpcoll_fanin.c \
        bcol_ptpcoll_fanout.c \
        bcol_ptpcoll_module.c 
	
	
# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_bcol_ptpcoll_DSO
component_install += mca_bcol_ptpcoll.la
else
component_noinst += libmca_bcol_ptpcoll.la
endif

# See ompi/mca/btl/sm/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_bcol_ptpcoll_la_SOURCES = $(sources)
mca_bcol_ptpcoll_la_LDFLAGS = -module -avoid-version
mca_bcol_ptpcoll_la_LIBADD = \
                             $(top_ompi_builddir)/ompi/mca/common/netpatterns/libmca_common_netpatterns.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_bcol_ptpcoll_la_SOURCES =$(sources)
libmca_bcol_ptpcoll_la_LDFLAGS = -module -avoid-version
