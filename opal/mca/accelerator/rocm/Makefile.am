#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2022      Advanced Micro Devices, Inc.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


AM_CPPFLAGS = $(opal_rocm_CPPFLAGS)

sources = \
        accelerator_rocm.h \
        accelerator_rocm_component.c \
        accelerator_rocm_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_accelerator_rocm_DSO
component_noinst =
component_install = mca_accelerator_rocm.la
else
component_noinst = libmca_accelerator_rocm.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_accelerator_rocm_la_SOURCES = $(sources)
mca_accelerator_rocm_la_LDFLAGS = -module -avoid-version $(opal_rocm_LDFLAGS)
mca_accelerator_rocm_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
	$(opal_rocm_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_accelerator_rocm_la_SOURCES =$(sources)
libmca_accelerator_rocm_la_LDFLAGS = -module -avoid-version $(opal_rocm_LDFLAGS)
libmca_accelerator_rocm_la_LIBADD = $(opal_rocm_LIBS)
