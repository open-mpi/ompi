# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
#  Additional copyrights may follow
# 
# $HEADER$
# 
# 
scoll_mpi_sources = \
					scoll_mpi.h \
					scoll_mpi_debug.h \
					scoll_mpi_dtypes.h \
					scoll_mpi_module.c \
					scoll_mpi_component.c \
					scoll_mpi_ops.c
if MCA_BUILD_oshmem_scoll_mpi_DSO
component_noinst =
component_install = mca_scoll_mpi.la
else
component_noinst = libmca_scoll_mpi.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_scoll_mpi_la_SOURCES = $(scoll_mpi_sources)
mca_scoll_mpi_la_LIBADD = $(scoll_mpi_LIBS)
mca_scoll_mpi_la_LDFLAGS = -module -avoid-version $(scoll_mpi_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_scoll_mpi_la_SOURCES =$(scoll_mpi_sources)
libmca_scoll_mpi_la_LIBADD = $(scoll_mpi_LIBS)
libmca_scoll_mpi_la_LDFLAGS = -module -avoid-version $(scoll_mpi_LDFLAGS)
