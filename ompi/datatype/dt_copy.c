/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/prefetch.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"
#include "ompi/datatype/datatype_internal.h"
#include "ompi/datatype/datatype_checksum.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

#if OMPI_ENABLE_DEBUG
extern int ompi_copy_debug;
#define DO_DEBUG(INST)  if( ompi_copy_debug ) { INST }
#else
#define DO_DEBUG(INST)
#endif  /* OMPI_ENABLE_DEBUG */

static size_t ompi_datatype_memcpy_block_size = 128 * 1024;

/**
 * Non overlapping memory regions
 */
#undef MEM_OP_NAME
#define MEM_OP_NAME  non_overlap
#undef MEM_OP
#define MEM_OP       MEMCPY
#include "dt_copy.h"

#define MEMMOVE(d, s, l)                                  \
    do {                                                  \
        if( (((d) < (s)) && (((d) + (l)) > (s))) ||       \
            (((s) < (d)) && (((s) + (l)) > (d))) ) {      \
            memmove( (d), (s), (l) );                     \
        } else {                                          \
            MEMCPY( (d), (s), (l) );                      \
        }                                                 \
    } while (0)

/**
 * Overlapping memory regions
 */
#undef MEM_OP_NAME
#define MEM_OP_NAME  overlap
#undef MEM_OP
#define MEM_OP       MEMMOVE
#include "dt_copy.h"

int32_t ompi_ddt_copy_content_same_ddt( const ompi_datatype_t* datatype, int32_t count,
                                        char* destination_base, char* source_base )
{
    ptrdiff_t extent;
    int32_t (*fct)( const ompi_datatype_t*, int32_t, char*, char*);

    if( 0 == count ) return 1;

    /**
     * see discussion in coll_basic_reduce.c for the computation of extent when
     * count != 1. Short version of the story:
     * (true_extent + ((count - 1) * extent))
     */
    extent = (datatype->true_ub - datatype->true_lb) + (count - 1) * (datatype->ub - datatype->lb);
    fct = non_overlap_copy_content_same_ddt;
    if( destination_base < source_base ) {
        if( (destination_base + extent) > source_base ) {
            /* memmove */
            fct = overlap_copy_content_same_ddt;
        }
    } else {
        if( (source_base + extent) > destination_base ) {
            /* memmove */
            fct = overlap_copy_content_same_ddt;
        }
    }
    return fct( datatype, count, destination_base, source_base );
}

