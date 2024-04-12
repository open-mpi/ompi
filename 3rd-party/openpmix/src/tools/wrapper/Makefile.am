#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2006 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2020      IBM Corporation.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_LDFLAGS = $(pmix_hwloc_LDFLAGS) $(pmix_libevent_LDFLAGS)

if PMIX_INSTALL_BINARIES

bin_PROGRAMS = pmixcc

nodist_pmixdata_DATA = pmixcc-wrapper-data.txt

dist_pmixdata_DATA = help-pmixcc.txt

pmixcc_SOURCES = \
        pmixcc.c

pmixcc_LDADD = \
    $(pmix_libevent_LIBS) \
    $(pmix_hwloc_LIBS) \
	$(top_builddir)/src/libpmix.la

endif # PMIX_INSTALL_BINARIES
