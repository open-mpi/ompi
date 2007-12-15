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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
					dict.c \
					hb_tree.c \
					nbc.c \
					nbc_iallgather.c \
					nbc_iallgatherv.c \
					nbc_iallreduce.c \
					nbc_ialltoall.c \
					nbc_ialltoallv.c \
					nbc_ibarrier.c \
					nbc_ibcast.c \
					nbc_igather.c \
					nbc_igatherv.c \
					nbc_ireduce.c \
					nbc_ireduce_scatter.c \
					nbc_iscan.c \
					nbc_iscatter.c \
					nbc_iscatterv.c \
					nbc_op.c \
					ompi_component.c \
					ompi_component.h \
					dict.h \
					dict_private.h \
					hb_tree.h \
					nbc.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_coll_libnbc_DSO
component_noinst =
component_install = mca_coll_libnbc.la
else
component_noinst = libmca_coll_libnbc.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_libnbc_la_SOURCES = $(sources)
mca_coll_libnbc_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_libnbc_la_SOURCES =$(sources)
libmca_coll_libnbc_la_LDFLAGS = -module -avoid-version
