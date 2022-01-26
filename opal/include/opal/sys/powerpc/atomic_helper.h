/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 > * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2021 IBM Corporation.  All rights reserved.
 * Copyright (c) 2015-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_SYS_ARCH_ATOMIC_HELPER_H
#define OPAL_SYS_ARCH_ATOMIC_HELPER_H 1

#if defined(__xlC__) || defined(__IBMC__) || defined(__IBMCPP__) || defined(__ibmxl__)
/* work-around bizzare xlc bug in which it sign-extends
   a pointer to a 32-bit signed integer */
#    define OPAL_ASM_ADDR(a) ((uintptr_t) a)
#else
#    define OPAL_ASM_ADDR(a) (a)
#endif

#if defined(__PGI)
/* work-around for bug in PGI 16.5-16.7 where the compiler fails to
 * correctly emit load instructions for 64-bit operands. without this
 * it will emit lwz instead of ld to load the 64-bit operand. */
#    define OPAL_ASM_VALUE64(x) (void *) (intptr_t)(x)
#else
#    define OPAL_ASM_VALUE64(x) x
#endif

#endif /* OPAL_SYS_ARCH_ATOMIC_HELPER_H */
