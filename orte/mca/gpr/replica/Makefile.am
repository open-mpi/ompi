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

SUBDIRS = api_layer functional_layer transition_layer communications

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_gpr_replica_DSO
component_noinst = 
component_install = mca_gpr_replica.la
else
component_noinst = libmca_gpr_replica.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_gpr_replica_la_SOURCES = gpr_replica.h \
                             gpr_replica_class_instances.h \
                             gpr_replica_component.c

mca_gpr_replica_la_LIBADD = \
	api_layer/libmca_gpr_replica_api.la \
	transition_layer/libmca_gpr_replica_tl.la \
	functional_layer/libmca_gpr_replica_fn.la \
	communications/libmca_gpr_replica_comm.la \
        $(top_ompi_builddir)/orte/libopen-rte.la \
        $(top_ompi_builddir)/opal/libopen-pal.la
mca_gpr_replica_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_gpr_replica_la_SOURCES = gpr_replica.h \
                                gpr_replica_class_instances.h \
                                gpr_replica_component.c
libmca_gpr_replica_la_LIBADD = \
	api_layer/libmca_gpr_replica_api.la \
	transition_layer/libmca_gpr_replica_tl.la \
	functional_layer/libmca_gpr_replica_fn.la \
	communications/libmca_gpr_replica_comm.la
libmca_gpr_replica_la_LDFLAGS = -module -avoid-version
