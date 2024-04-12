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
# Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2019      Triad National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2019      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        ess_alps.h \
        ess_alps_component.c \
        ess_alps_module.c    \
        ess_alps_utils.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_prte_ess_alps_DSO
component_noinst =
component_install = prte_mca_ess_alps.la
else
component_noinst = libprtemca_ess_alps.la
component_install =
endif

mcacomponentdir = $(prtelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
prte_mca_ess_alps_la_SOURCES = $(sources)
prte_mca_ess_alps_la_CPPFLAGS = $(ess_alps_CPPFLAGS)
prte_mca_ess_alps_la_LDFLAGS = -module -avoid-version $(ess_alps_LDFLAGS)
prte_mca_ess_alps_la_LIBADD = $(ess_alps_LIBS) \
                         $(top_builddir)/src/libprrte.la \
                         $(PRTE_TOP_BUILDDIR)/src/mca/common/alps/libmca_common_alps.la


noinst_LTLIBRARIES = $(component_noinst)
libprtemca_ess_alps_la_SOURCES =$(sources)
libprtemca_ess_alps_la_CPPFLAGS = $(ess_alps_CPPFLAGS)
libprtemca_ess_alps_la_LDFLAGS = -module -avoid-version  $(ess_alps_LDFLAGS)
libprtemca_ess_alps_la_LIBADD = $(ess_alps_LIBS)
