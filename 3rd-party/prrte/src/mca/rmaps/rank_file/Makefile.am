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
# Copyright (c) 2008      Voltaire. All rights reserved
#
# Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_LFLAGS = -Pprte_rmaps_rank_file_
LEX_OUTPUT_ROOT = lex.prte_rmaps_rank_file_
dist_prtedata_DATA = help-rmaps_rank_file.txt

sources = \
        rmaps_rank_file.c \
        rmaps_rank_file.h \
        rmaps_rank_file_component.c \
        rmaps_rank_file_lex.l \
        rmaps_rank_file_lex.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_prte_rmaps_rank_file_DSO
component_noinst =
component_install = prte_mca_rmaps_rank_file.la
else
component_noinst = libprtemca_rmaps_rank_file.la
component_install =
endif

mcacomponentdir = $(prtelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
prte_mca_rmaps_rank_file_la_SOURCES = $(sources)
prte_mca_rmaps_rank_file_la_LDFLAGS = -module -avoid-version
prte_mca_rmaps_rank_file_la_LIBADD = $(top_builddir)/src/libprrte.la

noinst_LTLIBRARIES = $(component_noinst)
libprtemca_rmaps_rank_file_la_SOURCES =$(sources)
libprtemca_rmaps_rank_file_la_LDFLAGS = -module -avoid-version

maintainer-clean-local:
	rm -f rmaps_rank_file_lex.c
