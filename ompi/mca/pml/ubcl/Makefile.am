# Copyright (c) 2019-2024 Bull SAS.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(pml_ubcl_CPPFLAGS)

EXTRA_DIST = post_configure.sh

ubcl_sources  = \
	pml_ubcl_utils.h \
	pml_ubcl_request.h \
	pml_ubcl.h \
	pml_ubcl.c \
	pml_ubcl_utils.c \
	pml_ubcl_isend.c \
	pml_ubcl_irecv.c \
	pml_ubcl_iprobe.c \
	pml_ubcl_progress.c \
	pml_ubcl_request.c \
	pml_ubcl_datatype.c \
	pml_ubcl_component.c \
	pml_ubcl_endpoint.c \
	pml_ubcl_endpoint.h

if MCA_BUILD_ompi_pml_ubcl_DSO
component_noinst =
component_install = mca_pml_ubcl.la
else
component_noinst = libmca_pml_ubcl.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_ubcl_la_SOURCES = $(ubcl_sources)
mca_pml_ubcl_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(pml_ubcl_LIBS) \
	$(OPAL_TOP_BUILDDIR)/opal/mca/common/ubcl/lib@OPAL_LIB_NAME@mca_common_ubcl.la \
	$(OMPI_TOP_BUILDDIR)/ompi/mca/common/ubcl/libmca_common_ubcl.la

mca_pml_ubcl_la_LDFLAGS = -module -avoid-version $(pml_ubcl_LDFLAGS)
mca_pml_ubcl_la_CPPFLAGS = -Wextra -Wall -Werror -Wno-unused-parameter -Wno-missing-field-initializers $(pml_ubcl_CPPFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_ubcl_la_SOURCES = $(ubcl_sources)
libmca_pml_ubcl_la_LIBADD = $(pml_ubcl_LIBS)
libmca_pml_ubcl_la_LDFLAGS = -module -avoid-version $(pml_ubcl_LDFLAGS)
libmca_pml_ubcl_la_CPPFLAGS = $(mca_pml_ubcl_la_CPPFLAGS)
