/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef OPAL_CONVERTOR_INTERNAL_HAS_BEEN_INCLUDED
#define OPAL_CONVERTOR_INTERNAL_HAS_BEEN_INCLUDED

#include "opal_config.h"

#include "opal/datatype/opal_convertor.h"

BEGIN_C_DECLS

typedef size_t (*conversion_fct_t)(opal_convertor_t *pConvertor, size_t count, size_t blocklen,
                                   size_t elem_count, char **from, size_t from_len,
                                   ptrdiff_t from_extent, char **to, size_t to_length,
                                   ptrdiff_t to_extent);

typedef struct opal_convertor_master_t {
    struct opal_convertor_master_t *next;
    uint32_t remote_arch;
    uint32_t flags;
    uint32_t hetero_mask;
    /*
     * Subset of hetero_mask covering only the predefined types whose remote size differs from the
     * local one (size-changing conversions), excluding pure byte-swap. A datatype that touches any
     * of these types cannot have its predefined elements split across a fragment boundary; see
     * CONVERTOR_UNSAFE_SPLIT.
     */
    uint32_t size_mismatch_mask;
    const size_t remote_sizes[OPAL_DATATYPE_MAX_PREDEFINED];
    conversion_fct_t *pFunctions; /**< the convertor functions pointer */
} opal_convertor_master_t;

/*
 * Find or create a new master convertor based on a specific architecture. The master
 * convertor hold all information related to a defined architecture, such as the sizes
 * of the predefined data-types, the conversion functions, ...
 */
opal_convertor_master_t *opal_convertor_find_or_create_master(uint32_t remote_arch);

/*
 * Destroy all pending master convertors. This function is usually called when we
 * shutdown the data-type engine, once all convertors have been destroyed.
 */
void opal_convertor_destroy_masters(void);

END_C_DECLS

#endif /* OPAL_CONVERTOR_INTERNAL_HAS_BEEN_INCLUDED */
