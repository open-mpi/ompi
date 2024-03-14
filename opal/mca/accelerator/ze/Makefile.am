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


AM_CPPFLAGS = $(common_ze_CPPFLAGS)

sources = \
        accelerator_ze.h \
        accelerator_ze_component.c \
        accelerator_ze_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_accelerator_ze_DSO
component_noinst =
component_install = mca_accelerator_ze.la
else
component_noinst = libmca_accelerator_ze.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_accelerator_ze_la_SOURCES = $(sources)
mca_accelerator_ze_la_LDFLAGS = -module -avoid-version $(opal_ze_LDFLAGS)
mca_accelerator_ze_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
	$(opal_ze_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_accelerator_ze_la_SOURCES =$(sources)
libmca_accelerator_ze_la_LDFLAGS = -module -avoid-version $(opal_ze_LDFLAGS)
libmca_accelerator_ze_la_LIBADD = $(opal_ze_LIBS)
