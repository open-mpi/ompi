#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        pstat_linux.h \
        pstat_linux_component.c \
        pstat_linux_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_pmix_pstat_linux_DSO
component_noinst =
component_install = pmix_mca_pstat_linux.la
else
component_noinst = libpmix_mca_pstat_linux.la
component_install =
endif

mcacomponentdir = $(pmixlibdir)
mcacomponent_LTLIBRARIES = $(component_install)
pmix_mca_pstat_linux_la_SOURCES = $(sources)
pmix_mca_pstat_linux_la_LDFLAGS = -module -avoid-version
pmix_mca_pstat_linux_la_LIBADD = $(top_builddir)/src/libpmix.la

noinst_LTLIBRARIES = $(component_noinst)
libpmix_mca_pstat_linux_la_SOURCES =$(sources)
libpmix_mca_pstat_linux_la_LDFLAGS = -module -avoid-version
