#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2022 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2022 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

DEFS="-DEXEEXT=\"$(EXEEXT)\""

if !OPAL_WANT_SCRIPT_WRAPPER_COMPILERS
if OPAL_INSTALL_BINARIES

bin_PROGRAMS = opal_wrapper

dist_opaldata_DATA = help-opal-wrapper.txt

# Only install the following for developer-level installs
if WANT_INSTALL_HEADERS

nodist_opaldata_DATA = \
	opalcc-wrapper-data.txt

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = opal.pc

endif # WANT_INSTALL_HEADERS

endif # OPAL_INSTALL_BINARIES
endif # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS

opal_wrapper_SOURCES = opal_wrapper.c
opal_wrapper_LDADD = $(top_builddir)/opal/libopen-pal_core.la
