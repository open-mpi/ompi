#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

not_used_yet = 

sources = \
        coll_sm2.h \
        coll_sm2_component.c  \
        coll_sm2_module.c \
        coll_sm2_bcast.c \
        coll_sm2_reduce.c \
        coll_sm2_allreduce.c \
        coll_sm2_barrier.c \
        coll_sm2_service.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_coll_sm2_DSO
component_install += mca_coll_sm2.la
else
component_noinst += libmca_coll_sm2.la
endif

# See ompi/mca/btl/sm/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_sm2_la_SOURCES = $(sources)
mca_coll_sm2_la_LDFLAGS = -module -avoid-version
mca_coll_sm2_la_LIBADD = \
        $(top_ompi_builddir)/ompi/mca/common/sm/libmca_common_sm.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_sm2_la_SOURCES =$(sources)
libmca_coll_sm2_la_LDFLAGS = -module -avoid-version
