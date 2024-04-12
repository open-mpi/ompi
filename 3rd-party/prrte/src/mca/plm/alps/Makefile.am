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
# Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
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
        plm_alps.h \
        plm_alps_component.c \
        plm_alps_module.c

dist_prtedata_DATA = help-plm-alps.txt

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_prte_plm_alps_DSO
component_noinst =
component_install = prte_mca_plm_alps.la
else
component_noinst = libprtemca_plm_alps.la
component_install =
endif

mcacomponentdir = $(prtelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
prte_mca_plm_alps_la_SOURCES = $(sources)
prte_mca_plm_alps_la_CPPFLAGS = $(plm_alps_CPPFLAGS)
prte_mca_plm_alps_la_LDFLAGS = -module -avoid-version $(plm_alps_LDFLAGS)
prte_mca_plm_alps_la_LIBADD = $(plm_alps_LIBS) \
    $(top_builddir)/src/libprrte.la   \
    $(PRTE_TOP_BUILDDIR)/src/mca/common/alps/libmca_common_alps.la

noinst_LTLIBRARIES = $(component_noinst)
libprtemca_plm_alps_la_SOURCES =$(sources)
libprtemca_plm_alps_la_CPPFLAGS = $(plm_alps_CPPFLAGS)
libprtemca_plm_alps_la_LDFLAGS = -module -avoid-version $(plm_alps_LDFLAGS)
libprtemca_plm_alps_la_LIBADD = $(plm_alps_LIBS)
