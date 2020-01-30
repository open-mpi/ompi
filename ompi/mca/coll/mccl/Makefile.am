# -*- shell-script -*-
#
#
# Copyright (c) 2020 Mellanox Technologies. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
#

AM_CPPFLAGS = $(coll_mccl_CPPFLAGS)

coll_mccl_sources = \
		coll_mccl.h \
		coll_mccl_debug.h \
		coll_mccl_dtypes.h \
		coll_mccl_module.c \
		coll_mccl_component.c \
		coll_mccl_ops.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_mccl_DSO
component_noinst =
component_install = mca_coll_mccl.la
else
component_noinst = libmca_coll_mccl.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES  = $(component_install)
mca_coll_mccl_la_SOURCES = $(coll_mccl_sources)
mca_coll_mccl_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(coll_mccl_LIBS)
mca_coll_mccl_la_LDFLAGS = -module -avoid-version $(coll_mccl_LDFLAGS)

noinst_LTLIBRARIES           = $(component_noinst)
libmca_coll_mccl_la_SOURCES = $(coll_mccl_sources)
libmca_coll_mccl_la_LIBADD  = $(coll_mccl_LIBS)
libmca_coll_mccl_la_LDFLAGS = -module -avoid-version $(coll_mccl_LDFLAGS)

