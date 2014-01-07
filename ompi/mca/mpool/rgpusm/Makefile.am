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
# Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(mpool_rgpusm_CPPFLAGS)

sources = \
    mpool_rgpusm_module.c \
    mpool_rgpusm_component.c

if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/$(subdir)
ompi_HEADERS = mpool_rgpusm.h
endif

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_mpool_rgpusm_DSO
component_noinst =
component_install = mca_mpool_rgpusm.la
else
component_noinst = libmca_mpool_rgpusm.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mpool_rgpusm_la_SOURCES = $(sources)
mca_mpool_rgpusm_la_LDFLAGS = -module -avoid-version
mca_mpool_rgpusm_la_LIBADD = $(mpool_rgpusm_LIBS)
if OPAL_cuda_support
mca_mpool_rgpusm_la_LIBADD += \
    $(top_ompi_builddir)/ompi/mca/common/cuda/libmca_common_cuda.la
endif

noinst_LTLIBRARIES = $(component_noinst)
libmca_mpool_rgpusm_la_SOURCES = $(sources)
libmca_mpool_rgpusm_la_LDFLAGS = -module -avoid-version
libmca_mpool_rgpusm_la_LIBADD = $(mpool_rgpusm_LIBS)
