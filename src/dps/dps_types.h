/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * Resource Discovery & Allocation Subsystem (RDAS)
 *
 * The RDAS is responsible for discovering the resources available to the universe, and
 * for allocating them to the requesting job.
 *
 */

#ifndef ORTE_DPS_TYPES_H_
#define ORTE_DPS_TYPES_H_

#include "orte_config.h"

#include "class/ompi_object.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

typedef struct orte_buffer_t {
    /* first member must be the objects parent */
    ompi_object_t parent;
    
    void*   base_ptr;  /* start of my memory */
    void*   data_ptr;  /* location of where next data will go */
    void*   from_ptr;  /* location of where to get the next data from */

    /* counters */

    int32_t   pages;     /* number of orte_dps_size pages of memory used (pages) */
    int32_t   size;      /* total size of this buffer (bytes) */
    int32_t   len;       /* total amount already packed (bytes) */
    int32_t   space;     /* how much space we have left (bytes) */
                         /* yep, size=len+space */

    int32_t   toend;     /* how many bytes till the end when unpacking :) */
                         /* yep, toend is the opposite of len */

} orte_buffer_t;

/* formalise the declaration */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION (orte_buffer_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTE_DPS_TYPES_H */
