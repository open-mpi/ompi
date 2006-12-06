/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#ifndef ORTE_DSS_TYPES_H_
#define ORTE_DSS_TYPES_H_

#include "orte_config.h"
#include "orte/orte_types.h"

#include "opal/class/opal_object.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* define arithmetic operations for readability */
typedef uint8_t orte_dss_arith_op_t;

#define ORTE_DSS_ADD    1
#define ORTE_DSS_SUB    2
#define ORTE_DSS_MUL    3
#define ORTE_DSS_DIV    4


/* Data value object */
typedef struct {
    opal_object_t super;                /* required for this to be an object */
    orte_data_type_t type;              /* the type of value stored */
    void *data;
} orte_data_value_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_data_value_t);

#define ORTE_DATA_VALUE_EMPTY {{OBJ_CLASS(orte_data_value_t),0}, ORTE_UNDEF, NULL}

/* structured-unstructured data flags */
#define ORTE_DSS_STRUCTURED     true
#define ORTE_DSS_UNSTRUCTURED   false

/**
 * buffer type
 */
typedef uint8_t orte_dss_buffer_type_t;
#define ORTE_DSS_BUFFER_NON_DESC        0x00
#define ORTE_DSS_BUFFER_FULLY_DESC      0x01

#define ORTE_DSS_BUFFER_TYPE_HTON(h);
#define ORTE_DSS_BUFFER_TYPE_NTOH(h);

/**
     * Structure for holding a buffer to be used with the RML or OOB
     * subsystems.
     */
    struct orte_buffer_t {
        /** First member must be the object's parent */
        opal_object_t parent;
        /** type of buffer */
        orte_dss_buffer_type_t type;
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
    ORTE_DECLSPEC OBJ_CLASS_DECLARATION (orte_buffer_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTE_DSS_TYPES_H */
