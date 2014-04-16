#
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
AM_LFLAGS = -Pcoll_ml_config_yy
LEX_OUTPUT_ROOT = lex.coll_ml_config_yy

dist_ompidata_DATA = \
        mca-coll-ml.config \
		help-mpi-coll-ml.txt

not_used_yet = 

sources = coll_ml.h \
        coll_ml_inlines.h \
        coll_ml_module.c \
        coll_ml_allocation.h \
        coll_ml_allocation.c \
        coll_ml_barrier.c \
        coll_ml_bcast.c \
        coll_ml_colls.h \
        coll_ml_component.c \
        coll_ml_copy_fns.c \
        coll_ml_descriptors.c  \
        coll_ml_functions.h \
        coll_ml_hier_algorithms.c \
        coll_ml_hier_algorithms_setup.c \
        coll_ml_hier_algorithms_bcast_setup.c \
        coll_ml_hier_algorithms_allreduce_setup.c \
        coll_ml_hier_algorithms_reduce_setup.c \
        coll_ml_hier_algorithms_common_setup.c \
        coll_ml_hier_algorithms_common_setup.h \
        coll_ml_hier_algorithms_allgather_setup.c \
        coll_ml_hier_algorithm_memsync_setup.c \
        coll_ml_custom_utils.h \
        coll_ml_custom_utils.c \
        coll_ml_progress.c \
        coll_ml_reduce.c \
        coll_ml_allreduce.c \
        coll_ml_allgather.c \
        coll_ml_mca.h \
        coll_ml_mca.c \
        coll_ml_lmngr.h \
        coll_ml_lmngr.c \
        coll_ml_hier_algorithms_barrier_setup.c \
        coll_ml_select.h \
        coll_ml_select.c \
        coll_ml_memsync.c \
        coll_ml_lex.h \
        coll_ml_lex.l \
        coll_ml_config.c \
        coll_ml_config.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_coll_ml_DSO
component_install += mca_coll_ml.la
else
component_noinst += libmca_coll_ml.la
endif

# See ompi/mca/btl/ml/Makefile.am for an explanation of
# libmca_common_ml.la.

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_ml_la_SOURCES = $(sources)
mca_coll_ml_la_LDFLAGS = -module -avoid-version
mca_coll_ml_la_LIBADD =


noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_ml_la_SOURCES =$(sources)
libmca_coll_ml_la_LDFLAGS = -module -avoid-version
