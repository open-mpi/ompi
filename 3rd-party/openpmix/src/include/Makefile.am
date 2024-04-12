# -*- makefile -*-
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
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2007-2022 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022-2023 Triad National Security, LLC. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# The Automake BUILT_SOURCES hook ultimately calls
# construct_dictionary.py to generate both pmix_dictionary.h and
# pmix_dictionary.c.  However, in a parallel "make" invocation, this
# script could be executed twice simultaneously, which *could* lead to
# the generated pmix_dictionary.* files being corrupted (i.e., two
# invokcations simultaneously writing to these generated files).  To
# prevent that from happening, force this one Makefile to be run
# serially via the .NOTPARALLEL target.  This has two side-effects:
#
# 1. construct_dictionary.py will only be invokved once, because since
#    both targets are created in the first invocation, make won't need
#    to invoke it a second time for the second target.  This is both
#    expected and good.
# 2. There is a (very) minor performance implication for the
#    other make steps in this Makefile.am (i.e., they won't be run in
#    parallel).  But this simple solution to avoid a devious potential
#    file corruption issue is an acceptable tradeoff.
.NOTPARALLEL:

AM_CFLAGS = \
            -DPMIX_PROXY_VERSION_STRING="\"@PMIX_VERSION@\"" \
            -DPMIX_PROXY_BUGREPORT_STRING="\"@PMIX_PROXY_BUGREPORT_STRING@\""

pmixdir = $(pmixincludedir)/$(subdir)

noinst_LTLIBRARIES = libpmixglobal.la

libpmixglobal_la_SOURCES = \
	pmix_globals.h \
    pmix_globals.c \
    pmix_dictionary.c \
    pmix_frameworks.c \
    pmix_event_strings.c

headers = pmix_globals.h

headers += \
        pmix_atomic.h \
        pmix_hash_string.h \
        pmix_socket_errno.h \
        pmix_stdatomic.h \
        pmix_stdint.h \
        pmix_prefetch.h \
        pmix_types.h \
        pmix_config.h \
        pmix_config_top.h \
        pmix_config_bottom.h \
        pmix_portable_platform.h \
        pmix_portable_platform_real.h \
        pmix_frameworks.h \
        pmix_dictionary.h \
        pmix_event_strings.h

nobase_pmix_HEADERS = $(headers)

# Files that are generated here.
libpmixglobal_gen = \
	pmix_dictionary.h \
    pmix_dictionary.c \
    pmix_event_strings.h \
    pmix_event_strings.c

# Need to ensure that pmix_dictionary.h and pmix_dictionary.c are built first --
# before any other targets.
BUILT_SOURCES = $(libpmixglobal_gen)

$(libpmixglobal_gen): $(top_srcdir)/include/pmix_common.h.in \
                      $(top_srcdir)/include/pmix_deprecated.h \
                      $(top_srcdir)/src/common/pmix_attributes.c
	        $(PYTHON) $(top_srcdir)/contrib/construct_dictionary.py
	        $(PYTHON) $(top_srcdir)/contrib/construct_event_strings.py

MAINTAINERCLEANFILES = $(libpmixglobal_gen)
