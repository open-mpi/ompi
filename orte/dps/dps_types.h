/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * Buffer management types.
 */

#ifndef ORTE_DPS_TYPES_H_
#define ORTE_DPS_TYPES_H_

#include "orte_config.h"

#include "opal/class/opal_object.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Structure for holding a buffer to be used with the RML or OOB
     * subsystems.
     */
    struct orte_buffer_t {
        /** First member must be the object's parent */
        opal_object_t parent;
        
        /** Start of my memory */
        char *base_ptr;
        /** Where the next data will be packed to (within the allocated
            memory starting at base_ptr) */
        char *pack_ptr;
        /** Where the next data will be unpacked from (within the
            allocated memory starting as base_ptr) */
        char *unpack_ptr;
        
        /** Number of bytes allocated (starting at base_ptr),
            typically in multiples of orte_dps_pages, but may not be
            if the buffer was initialized with orte_dps_load(). */
        size_t bytes_allocated;
        /** Number of bytes used by the buffer (i.e., amount of data --
            including overhead -- packed in the buffer) */
        size_t bytes_used;
        /** How many bytes are available in the allocated buffer -- kept
            here for convenience rather than recalculating it all the
            time */
        size_t bytes_avail;
    };
    /**
     * Convenience typedef
     */
    typedef struct orte_buffer_t orte_buffer_t;

    /** formalize the declaration */
    OMPI_DECLSPEC OBJ_CLASS_DECLARATION (orte_buffer_t);
        
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTE_DPS_TYPES_H */
