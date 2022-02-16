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

sources = \
        accelerator_null_component.h \
        accelerator_null_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_accelerator_null_DSO
component_noinst =
component_install = mca_accelerator_null.la
else
component_noinst = libmca_accelerator_null.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_accelerator_null_la_SOURCES = $(sources)
mca_accelerator_null_la_LDFLAGS = -module -avoid-version
mca_accelerator_null_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_accelerator_null_la_SOURCES =$(sources)
libmca_accelerator_null_la_LDFLAGS = -module -avoid-version
