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

#include "class/ompi_object.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Structure for holding a buffer to be used with the RML or OOB
     * subsystems.
     */
    struct orte_buffer_t {
        /** First member must be the object's parent */
        ompi_object_t parent;
        
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
    
    /* define the basic orte data type */
    typedef uint8_t orte_data_type_t;
    
    
    /* define an object for holding blocks of bytes */
    typedef struct {
        size_t size;
        uint8_t *bytes;
    } orte_byte_object_t;

    /* DEFINE INTRINSIC DATA TYPE NAMES */
    /** Intrinsic type: nothing */
#define ORTE_NULL   42
    /** Intrinsic type: byte */
#define ORTE_BYTE   1
    /** Intrinsic type: bool */
#define ORTE_BOOL   2

    /** Intrinsic type: int */
#define ORTE_INT    6
    /** Intrinsic type: uint */
#define ORTE_UINT   11

    /** Intrinsic type: int8 */
#define ORTE_INT8   7
    /** Intrinsic type: uint8 */
#define ORTE_UINT8  12
    /** Intrinsic type: int16 */
#define ORTE_INT16  8
    /** Intrinsic type: uint16 */
#define ORTE_UINT16 13
    /** Intrinsic type: int32 */
#define ORTE_INT32  9
    /** Intrinsic type: uint32 */
#define ORTE_UINT32 14
    /** Intrinsic type: int64 */
#define ORTE_INT64  10
    /** Intrinsic type: uint64 */
#define ORTE_UINT64 15

    /** Intrinsic type: size_t */
#define ORTE_SIZE   4

    /** Intrinsic type: string */
#define ORTE_STRING 3

    /** Intrinsic type: byte object */
#define ORTE_BYTE_OBJECT    30

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTE_DPS_TYPES_H */
