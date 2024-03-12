#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2009 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2020-2021 Google, LLC. All rights reserved.
# Copyright (c) 2024      Advanced Micro Devices, Inc. All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

EXTRA_DIST = post_configure.sh

AM_CPPFLAGS = $(smsc_accelerator_CPPFLAGS)


libmca_smsc_accelerator_la_sources = \
    smsc_accelerator_component.c \
    smsc_accelerator_module.c \
    smsc_accelerator_internal.h \
    smsc_accelerator.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_smsc_accelerator_DSO
component_noinst =
component_install = mca_smsc_accelerator.la
else
component_noinst = libmca_smsc_accelerator.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_smsc_accelerator_la_SOURCES = $(libmca_smsc_accelerator_la_sources)
mca_smsc_accelerator_la_LDFLAGS = -module -avoid-version $(smsc_accelerator_LDFLAGS)
mca_smsc_accelerator_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
	$(smsc_accelerator_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_smsc_accelerator_la_SOURCES = $(libmca_smsc_accelerator_la_sources)
libmca_smsc_accelerator_la_LIBADD = $(smsc_accelerator_LIBS)
libmca_smsc_accelerator_la_LDFLAGS = -module -avoid-version $(smsc_accelerator_LDFLAGS)
