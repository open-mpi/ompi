# -*- shell-script -*-
#
#
# Copyright (c) 2011 Mellanox Technologies. All rights reserved.
# Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2019 Huawei Technologies Co., Ltd. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
#

AM_CPPFLAGS = $(coll_ucx_CPPFLAGS) -DCOLL_UCX_HOME=\"$(coll_ucx_HOME)\" $(coll_ucx_extra_CPPFLAGS)

#dist_ompidata_DATA = help-coll-ucx.txt
coll_ucx_sources = \
        coll_ucx.h \
        coll_ucx_request.h \
        coll_ucx_freelist.h \
        coll_ucx_op.c \
        coll_ucx_module.c \
        coll_ucx_request.c \
        coll_ucx_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_ucx_DSO
component_noinst =
component_install = mca_coll_ucx.la
else
component_noinst = libmca_coll_ucx.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_ucx_la_SOURCES = $(coll_ucx_sources)
mca_coll_ucx_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la $(coll_ucx_LIBS) \
                         $(OPAL_TOP_BUILDDIR)/opal/mca/common/ucx/lib@OPAL_LIB_PREFIX@mca_common_ucx.la
mca_coll_ucx_la_LDFLAGS = -module -avoid-version $(coll_ucx_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_ucx_la_SOURCES =$(coll_ucx_sources)
libmca_coll_ucx_la_LIBADD = $(coll_ucx_LIBS)
libmca_coll_ucx_la_LDFLAGS = -module -avoid-version $(coll_ucx_LDFLAGS)
