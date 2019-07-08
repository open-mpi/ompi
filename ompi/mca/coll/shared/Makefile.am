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
# Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        coll_shared.h \
        coll_shared_reduce.c \
        coll_shared_bcast.c \
        coll_shared_allreduce.c \
        coll_shared_component.c \
        coll_shared_module.c 

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_coll_shared_DSO
component_install += mca_coll_shared.la
else
component_noinst += libmca_coll_shared.la
endif

# See ompi/mca/btl/sm/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_shared_la_SOURCES = $(sources)
mca_coll_shared_la_LDFLAGS = -module -avoid-version
mca_coll_shared_la_LIBADD = 

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_shared_la_SOURCES =$(sources)
libmca_coll_shared_la_LDFLAGS = -module -avoid-version
