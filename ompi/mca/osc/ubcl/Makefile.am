# Copyright (c) 2025 Bull SAS.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(osc_ubcl_CPPFLAGS)

dist_ompidata_DATA =

# EXTRA_DIST = post_configure.sh

ubcl_sources  = \
    osc_ubcl_utils.h \
    osc_ubcl.h \
    osc_ubcl.c \
    osc_ubcl_put.c \
    osc_ubcl_accumulate.c \
    osc_ubcl_datatype.c \
    osc_ubcl_info.c \
    osc_ubcl_info.h \
    osc_ubcl_get.c\
    osc_ubcl_request.c \
    osc_ubcl_request.h \
    osc_ubcl_sync.c \
    osc_ubcl_sync.h

if MCA_BUILD_ompi_osc_ubcl_DSO
component_noinst =
component_install = mca_osc_ubcl.la
else
component_noinst = libmca_osc_ubcl.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_osc_ubcl_la_SOURCES = $(ubcl_sources)
mca_osc_ubcl_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(osc_ubcl_LIBS) \
	$(OPAL_TOP_BUILDDIR)/opal/mca/common/ubcl/lib@OPAL_LIB_NAME@mca_common_ubcl.la \
	$(OMPI_TOP_BUILDDIR)/ompi/mca/common/ubcl/libmca_common_ubcl.la

mca_osc_ubcl_la_LDFLAGS = -module -avoid-version $(osc_ubcl_LDFLAGS)
mca_osc_ubcl_la_CPPFLAGS = -Wextra -Wall -Werror -Wno-unused-parameter -Wno-missing-field-initializers $(osc_ubcl_CPPFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_osc_ubcl_la_SOURCES = $(ubcl_sources)
libmca_osc_ubcl_la_LIBADD = $(osc_ubcl_LIBS)
libmca_osc_ubcl_la_LDFLAGS = -module -avoid-version $(osc_ubcl_LDFLAGS)
libmca_osc_ubcl_la_CPPFLAGS = $(mca_osc_ubcl_la_CPPFLAGS)
