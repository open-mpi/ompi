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
# Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from src/Makefile.am

headers += \
        include/constants.h \
        include/types.h \
        include/prte_frameworks.h \
        include/hash_string.h \
        include/prefetch.h \
        include/prte_config_top.h \
        include/prte_config_bottom.h \
        include/prte_portable_platform.h \
        include/prte_portable_platform_real.h \
        include/prte_stdint.h \
        include/prte_stdatomic.h \
        include/prte_socket_errno.h \
        include/align.h

nodist_headers += \
        include/prte_config.h \
        include/version.h

libprrte_la_SOURCES += \
        $(headers) \
        include/prte_frameworks.c
