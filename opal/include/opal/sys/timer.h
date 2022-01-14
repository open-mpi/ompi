/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2016      Broadcom Limited. All rights reserved.
 * Copyright (c) 2016-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file
 *
 * Cycle counter reading instructions.  Do not use directly - see the
 * timer interface instead
 */

#ifndef OPAL_SYS_TIMER_H
#define OPAL_SYS_TIMER_H 1

#include "opal/opal_portable_platform.h"

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif


/**********************************************************************
 *
 * Load the appropriate architecture files and set some reasonable
 * default values for our support
 *
 *********************************************************************/

/* By default we suppose all timers are monotonic per node. */
#define OPAL_TIMER_MONOTONIC 1

BEGIN_C_DECLS

/* If you update this list, you probably also want to update
   opal/mca/timer/linux/configure.m4.  Or not. */

#if defined(DOXYGEN)
/* don't include system-level gorp when generating doxygen files */
#elif defined(PLATFORM_ARCH_X86_64) || defined(PLATFORM_ARCH_X86)
#    include "opal/sys/x86_64/timer.h"
#elif defined(PLATFORM_ARCH_ARM) || defined(PLATFORM_ARCH_AARCH64)
#    include "opal/sys/arm64/timer.h"
#elif defined(PLATFORM_ARCH_POWERPC)
#    include "opal/sys/powerpc/timer.h"
#endif

#ifndef DOXYGEN
#    ifndef OPAL_HAVE_SYS_TIMER_GET_CYCLES
#        define OPAL_HAVE_SYS_TIMER_GET_CYCLES 0

typedef long opal_timer_t;
#    endif

#    ifndef OPAL_HAVE_SYS_TIMER_GET_FREQ
#        define OPAL_HAVE_SYS_TIMER_GET_FREQ 0
#    endif
#endif

#ifndef OPAL_HAVE_SYS_TIMER_IS_MONOTONIC

#    define OPAL_HAVE_SYS_TIMER_IS_MONOTONIC 1

static inline bool opal_sys_timer_is_monotonic(void)
{
    return OPAL_TIMER_MONOTONIC;
}

#endif

END_C_DECLS

#endif /* OPAL_SYS_TIMER_H */
