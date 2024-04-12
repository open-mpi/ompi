#
# Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CFLAGS = \
    -DPRTE_GREEK_VERSION="\"@PRTE_GREEK_VERSION@\""

sources = \
          schizo_prte_component.c \
          schizo_prte.h \
          schizo_prte.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_prte_schizo_prte_DSO
component_noinst =
component_install = prte_mca_schizo_prte.la
else
component_noinst = libprtemca_schizo_prte.la
component_install =
endif

mcacomponentdir = $(prtelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
prte_mca_schizo_prte_la_SOURCES = $(sources)
prte_mca_schizo_prte_la_LDFLAGS = -module -avoid-version
prte_mca_schizo_prte_la_LIBADD = $(top_builddir)/src/libprrte.la

noinst_LTLIBRARIES = $(component_noinst)
libprtemca_schizo_prte_la_SOURCES = $(sources)
libprtemca_schizo_prte_la_LDFLAGS = -module -avoid-version
