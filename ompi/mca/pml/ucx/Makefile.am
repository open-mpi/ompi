#
# Copyright (C) 2001-2017 Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

AM_CPPFLAGS = $(pml_ucx_CPPFLAGS)

local_sources = \
        pml_ucx.h \
        pml_ucx.c \
        pml_ucx_request.h \
        pml_ucx_request.c \
        pml_ucx_datatype.h \
        pml_ucx_datatype.c \
        pml_ucx_freelist.h \
        pml_ucx_component.c

if MCA_BUILD_ompi_pml_ucx_DSO
component_noinst =
component_install = mca_pml_ucx.la
else
component_noinst = libmca_pml_ucx.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_ucx_la_SOURCES = $(local_sources)
mca_pml_ucx_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la $(pml_ucx_LIBS) \
                        $(OPAL_TOP_BUILDDIR)/opal/mca/common/ucx/lib@OPAL_LIB_PREFIX@mca_common_ucx.la
mca_pml_ucx_la_LDFLAGS = -module -avoid-version $(pml_ucx_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_ucx_la_SOURCES = $(local_sources)
libmca_pml_ucx_la_LIBADD = $(pml_ucx_LIBS)
libmca_pml_ucx_la_LDFLAGS = -module -avoid-version $(pml_ucx_LDFLAGS)

