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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
# Copyright (c) 2016-2018 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2020-2021 Google, LLC. All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from opal/Makefile.am

headers += \
	opal/sys/atomic.h \
	opal/sys/atomic_stdc.h \
	opal/sys/atomic_impl_minmax_math.h \
	opal/sys/atomic_impl_ptr_cswap.h \
	opal/sys/atomic_impl_ptr_llsc.h \
	opal/sys/atomic_impl_ptr_swap.h \
	opal/sys/atomic_impl_size_t_math.h \
	opal/sys/atomic_impl_spinlock.h \
	opal/sys/timer.h \
        opal/sys/cma.h

include opal/sys/x86_64/Makefile.am
include opal/sys/arm64/Makefile.am
include opal/sys/powerpc/Makefile.am
include opal/sys/gcc_builtin/Makefile.am
