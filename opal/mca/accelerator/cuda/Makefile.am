#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


AM_CPPFLAGS = $(common_cuda_CPPFLAGS)
dist_opaldata_DATA = help-accelerator-cuda.txt
sources = \
        accelerator_cuda.h \
        accelerator_cuda_component.c \
        accelerator_cuda.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_accelerator_cuda_DSO
component_noinst =
component_install = mca_accelerator_cuda.la
else
component_noinst = libmca_accelerator_cuda.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)

mca_accelerator_cuda_la_SOURCES = $(sources)
mca_accelerator_cuda_la_LDFLAGS = -module -avoid-version $(accelerator_cuda_LDFLAGS)
mca_accelerator_cuda_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
        $(accelerator_cuda_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_accelerator_cuda_la_SOURCES =$(sources)
libmca_accelerator_cuda_la_LDFLAGS = -module -avoid-version $(accelerator_cuda_LDFLAGS)
libmca_accelerator_cuda_la_LIBADD = $(accelerator_cuda_LIBS)
