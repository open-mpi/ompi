#
# Copyright (c) 2013-2015 Intel, Inc. All rights reserved
#
# Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

EXTRA_DIST = post_configure.sh

AM_CPPFLAGS = $(ompi_mtl_ofi_CPPFLAGS) $(opal_common_libfabric_CPPFLAGS)

dist_ompidata_DATA = help-mtl-ofi.txt

mtl_ofi_sources = \
				   mtl_ofi.h \
				   mtl_ofi.c \
				   mtl_ofi_compat.h \
				   mtl_ofi_component.c \
				   mtl_ofi_endpoint.h \
				   mtl_ofi_endpoint.c \
				   mtl_ofi_request.h \
				   mtl_ofi_types.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_mtl_ofi_DSO
component_noinst =
component_install = mca_mtl_ofi.la
else
component_noinst = libmca_mtl_ofi.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mtl_ofi_la_SOURCES = $(mtl_ofi_sources)
mca_mtl_ofi_la_LDFLAGS = \
        $(ompi_mtl_ofi_LDFLAGS) \
        -module -avoid-version
mca_mtl_ofi_la_LIBADD = $(ompi_mtl_ofi_LIBS) \
        $(OPAL_TOP_BUILDDIR)/opal/mca/common/libfabric/lib@OPAL_LIB_PREFIX@mca_common_libfabric.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_mtl_ofi_la_SOURCES = $(mtl_ofi_sources)
libmca_mtl_ofi_la_LDFLAGS = \
        $(ompi_mtl_ofi_LDFLAGS) \
        -module -avoid-version
libmca_mtl_ofi_la_LIBADD = $(ompi_mtl_ofi_LIBS)
