#
# Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        errmgr_dvm.h \
        errmgr_dvm_component.c \
        errmgr_dvm.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_prte_errmgr_dvm_DSO
component_noinst =
component_install = prte_mca_errmgr_dvm.la
else
component_noinst = libprtemca_errmgr_dvm.la
component_install =
endif

mcacomponentdir = $(prtelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
prte_mca_errmgr_dvm_la_SOURCES = $(sources)
prte_mca_errmgr_dvm_la_LDFLAGS = -module -avoid-version
prte_mca_errmgr_dvm_la_LIBADD = $(top_builddir)/src/libprrte.la

noinst_LTLIBRARIES = $(component_noinst)
libprtemca_errmgr_dvm_la_SOURCES =$(sources)
libprtemca_errmgr_dvm_la_LDFLAGS = -module -avoid-version
