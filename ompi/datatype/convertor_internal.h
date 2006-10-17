/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef CONVERTOR_INTERNAL_HAS_BEEN_INCLUDED
#define CONVERTOR_INTERNAL_HAS_BEEN_INCLUDED

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"

typedef int32_t (*conversion_fct_t)( ompi_convertor_t* pConvertor, uint32_t count,
                                     const void* from, size_t from_len, ptrdiff_t from_extent,
                                     void* to, size_t to_length, ptrdiff_t to_extent, 
                                     ptrdiff_t *advance );

typedef struct ompi_convertor_master_t {
    struct ompi_convertor_master_t* next;
    uint32_t                        remote_arch;
    uint32_t                        flags;
    uint64_t                        hetero_mask;
    const size_t                    remote_sizes[DT_MAX_PREDEFINED];
    conversion_fct_t*               pFunctions;   /**< the convertor functions pointer */
} ompi_convertor_master_t;

/*
 * Find or create a new master convertor based on a specific architecture. The master
 * convertor hold all informations related to a defined architecture, such as the sizes
 * of the predefined data-types, the conversion functions, ...
 */
ompi_convertor_master_t* ompi_convertor_find_or_create_master( uint32_t remote_arch );

/*
 * Destroy all pending master convertors. This function is usually called when we
 * shutdown the data-type engine, once all convertors have been destroyed.
 */
void ompi_convertor_destroy_masters( void );

#endif  /* CONVERTOR_INTERNAL_HAS_BEEN_INCLUDED */
