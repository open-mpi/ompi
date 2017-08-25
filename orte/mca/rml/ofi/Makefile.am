#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015-2017 Intel, Inc. All rights reserved.
# Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(opal_common_ofi_CPPFLAGS)

sources = \
	rml_ofi.h \
	rml_ofi_request.h \
	rml_ofi_component.c \
	rml_ofi_send.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_rml_ofi_DSO
component_noinst =
component_install = mca_rml_ofi.la
else
component_noinst = libmca_rml_ofi.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rml_ofi_la_SOURCES = $(sources)
mca_rml_ofi_la_LDFLAGS = -module -avoid-version
mca_rml_ofi_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la \
	$(OPAL_TOP_BUILDDIR)/opal/mca/common/ofi/lib@OPAL_LIB_PREFIX@mca_common_ofi.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_rml_ofi_la_SOURCES = $(sources)
libmca_rml_ofi_la_LDFLAGS = -module -avoid-version
