/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2020 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Voltaire All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_ALIGN_H
#define OPAL_ALIGN_H

#define OPAL_DOWN_ALIGN(x,a,t)       ((x) & ~(((t)(a)-1)))
#define OPAL_DOWN_ALIGN_PTR(x,a,t)   ((t)OPAL_DOWN_ALIGN((uintptr_t)x, a, uintptr_t))
#define OPAL_ALIGN(x,a,t)            (((x)+((t)(a)-1)) & ~(((t)(a)-1)))
#define OPAL_ALIGN_PTR(x,a,t)        ((t)OPAL_ALIGN((uintptr_t)x, a, uintptr_t))
#define OPAL_ALIGN_PAD_AMOUNT(x,s)   ((~((uintptr_t)(x))+1) & ((uintptr_t)(s)+(!(uintptr_t)(s))-1))

#if __STDC_VERSION__ >= 201101L
#    include <stddef.h>
#    define OPAL_ALIGN_MIN (_Alignof(max_align_t))
#else
#    if defined(OPAL_ALIGNMENT__FLOAT128)
#        define OPAL_ALIGN_MIN (OPAL_ALIGNMENT__FLOAT128)
#    elif defined(OPAL_ALIGNMENT___FLOAT128)
#        define OPAL_ALIGN_MIN (OPAL_ALIGNMENT___FLOAT128)
#    elif defined(OPAL_ALIGNMENT_LONG_DOUBLE_COMPLEX)
#        define OPAL_ALIGN_MIN (OPAL_ALIGNMENT_LONG_DOUBLE_COMPLEX)
#    elif defined(OPAL_ALIGNMENT_LONG_DOUBLE)
#        define OPAL_ALIGN_MIN (OPAL_ALIGNMENT_LONG_DOUBLE)
#    else
#        define OPAL_ALIGN_MIN (OPAL_ALIGNMENT_DOUBLE)
#    endif
#endif /* __STDC_VERSION__ >= 201101L */

#endif /* OPAL_ALIGN_H */
