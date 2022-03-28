#
# Copyright (c) 2013-2015 Intel, Inc. All rights reserved
#
# Copyright (c) 2014-2019 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2019      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2020      Triad National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

EXTRA_DIST = post_configure.sh \
    $(generated_source_modules)

MAINTAINERCLEANFILES = \
    $(generated_sources)

AM_CPPFLAGS = $(mtl_ofi_CPPFLAGS)

dist_ompidata_DATA = help-mtl-ofi.txt

generated_source_modules = \
    mtl_ofi_send_opt.pm \
    mtl_ofi_isend_opt.pm \
    mtl_ofi_irecv_opt.pm \
    mtl_ofi_iprobe_opt.pm \
    mtl_ofi_improbe_opt.pm

generated_sources = \
    mtl_ofi_send_opt.c \
    mtl_ofi_isend_opt.c \
    mtl_ofi_irecv_opt.c \
    mtl_ofi_iprobe_opt.c \
    mtl_ofi_improbe_opt.c

mtl_ofi_sources = \
    mtl_ofi.h \
    mtl_ofi.c \
    mtl_ofi_compat.h \
    mtl_ofi_component.c \
    mtl_ofi_endpoint.h \
    mtl_ofi_endpoint.c \
    mtl_ofi_request.h \
    mtl_ofi_types.h \
    mtl_ofi_opt.h \
    $(generated_sources)

# A number of files are generated from macro expansion to minimize
# branches in the critical path.  These files have perl modules with the suffix
# .pm that generate the corresponding .c file with all possible branches as
# their own function and symbol. Additional input
# files should be added to generated_source_modules, as well as adding
# their .c variants to generated_sources.
%.c : %.pm;
	$(PERL) -I$(top_srcdir)/ompi/mca/mtl/ofi  $(top_srcdir)/ompi/mca/mtl/ofi/generate-opt-funcs.pl $@

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
        $(mtl_ofi_LDFLAGS) \
        -module -avoid-version
mca_mtl_ofi_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(OPAL_TOP_BUILDDIR)/opal/mca/common/ofi/lib@OPAL_LIB_NAME@mca_common_ofi.la \
        $(mtl_ofi_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mtl_ofi_la_SOURCES = $(mtl_ofi_sources)
libmca_mtl_ofi_la_LDFLAGS = \
        $(mtl_ofi_LDFLAGS) \
        -module -avoid-version
libmca_mtl_ofi_la_LIBADD = $(mtl_ofi_LIBS)
