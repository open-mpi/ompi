#
# Copyright (c) 2012      Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016-2017 Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

headers = rte_pmix.h

sources = \
        rte_pmix_component.c \
        rte_pmix_module.c

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/$(subdir)
nobase_ompi_HEADERS = $(headers)
endif

# We only ever build this component statically
noinst_LTLIBRARIES = libmca_rte_pmix.la
libmca_rte_pmix_la_SOURCES =$(sources) $(headers)
libmca_rte_pmix_la_LDFLAGS = -module -avoid-version
libmca_rte_pmix_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la
