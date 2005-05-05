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
 *
 */
#ifndef ORTE_DPS_INTERNAL_H_
#define ORTE_DPS_INTERNAL_H_

#include "orte_config.h"

#include "include/orte_constants.h"
#include "class/orte_pointer_array.h"

#include "dps/dps.h"

#if HAVE_STRING_H
#    if !defined(STDC_HEADERS) && HAVE_MEMORY_H
#        include <memory.h>
#    endif
#    include <string.h>
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * DEFINE THE DEFAULT PAGE SIZE FOR THE DPS BUFFERS - IN KILOBYTES
 */
#define ORTE_DPS_DEFAULT_PAGE_SIZE  1

/*
 * ORTE type corresponding to size_t
 */
#if SIZEOF_SIZE_T == 1
#define DPS_TYPE_SIZE_T ORTE_UINT8
#elif SIZEOF_SIZE_T == 2
#define DPS_TYPE_SIZE_T ORTE_UINT16
#elif SIZEOF_SIZE_T == 4
#define DPS_TYPE_SIZE_T ORTE_UINT32
#elif SIZEOF_SIZE_T == 8
#define DPS_TYPE_SIZE_T ORTE_UINT64
#else
#error Unsupported size_t size!
#endif

/*
 * ORTE type corresponding to bool
 */
#if SIZEOF_BOOL == 1
#define DPS_TYPE_BOOL ORTE_UINT8
#elif SIZEOF_BOOL == 2
#define DPS_TYPE_BOOL ORTE_UINT16
#elif SIZEOF_BOOL == 4
#define DPS_TYPE_BOOL ORTE_UINT32
#elif SIZEOF_BOOL == 8
#define DPS_TYPE_BOOL ORTE_UINT64
#else
#error Unsupported bool size!
#endif

/*
 * ORTE type corresponding to int and unsigned int
 */
#if SIZEOF_INT == 1
#define DPS_TYPE_INT ORTE_INT8
#define DPS_TYPE_UINT ORTE_UINT8
#elif SIZEOF_INT == 2
#define DPS_TYPE_INT ORTE_INT16
#define DPS_TYPE_UINT ORTE_UINT16
#elif SIZEOF_INT == 4
#define DPS_TYPE_INT ORTE_INT32
#define DPS_TYPE_UINT ORTE_UINT32
#elif SIZEOF_INT == 8
#define DPS_TYPE_INT ORTE_INT64
#define DPS_TYPE_UINT ORTE_UINT64
#else
#error Unsupported int size!
#endif

/**
 * Internal struct used for holding registered dps functions
 */
struct orte_dps_type_info_t {
    /* type identifier */
    orte_data_type_t odti_type;
    /** Debugging string name */
    char *odti_name;
    /** Pack function */
    orte_dps_pack_fn_t odti_pack_fn;
    /** Unpack function */
    orte_dps_unpack_fn_t odti_unpack_fn;
};
/**
 * Convenience typedef
 */
typedef struct orte_dps_type_info_t orte_dps_type_info_t;

/*
 * globals needed within dps
 */
extern bool orte_dps_initialized;
extern bool orte_dps_debug;
extern int orte_dps_verbose;
extern int orte_dps_page_size;
extern orte_pointer_array_t *orte_dps_types;

    /*
     * Implementations of API functions
     */
    int orte_dps_pack(orte_buffer_t *buffer, void *src,
                      size_t num_vals,
                      orte_data_type_t type);
    int orte_dps_unpack(orte_buffer_t *buffer, void *dest,
                        size_t *max_num_vals,
                        orte_data_type_t type);
    int orte_dps_peek(orte_buffer_t *buffer, orte_data_type_t *type,
                      size_t *number);
    
    int orte_dps_unload(orte_buffer_t *buffer, void **payload, 
                        size_t *bytes_used);
    int orte_dps_load(orte_buffer_t *buffer, void *payload, size_t bytes_used);

    int orte_dps_register(orte_dps_pack_fn_t pack_fn,
                          orte_dps_unpack_fn_t unpack_fn,
                          const char *name, orte_data_type_t *type);

    char *orte_dps_lookup_data_type(orte_data_type_t type);

    /*
     * Non-API functions that need to be reachable
     */
    int orte_dps_pack_buffer(orte_buffer_t *buffer, void *src, size_t num_vals,
                  orte_data_type_t type);

    int orte_dps_unpack_buffer(orte_buffer_t *buffer, void *dst, size_t *num_vals,
                    orte_data_type_t type);

    /*
     * Internal pack functions
     */

    int orte_dps_pack_null(orte_buffer_t *buffer, void *src,
                           size_t num_vals, orte_data_type_t type);
    int orte_dps_pack_byte(orte_buffer_t *buffer, void *src,
                           size_t num_vals, orte_data_type_t type);

    int orte_dps_pack_bool(orte_buffer_t *buffer, void *src,
                           size_t num_vals, orte_data_type_t type);

    int orte_dps_pack_int(orte_buffer_t *buffer, void *src,
                          size_t num_vals, orte_data_type_t type);
    int orte_dps_pack_int16(orte_buffer_t *buffer, void *src,
                            size_t num_vals, orte_data_type_t type);
    int orte_dps_pack_int32(orte_buffer_t *buffer, void *src,
                            size_t num_vals, orte_data_type_t type);
    int orte_dps_pack_int64(orte_buffer_t *buffer, void *src,
                            size_t num_vals, orte_data_type_t type);

    int orte_dps_pack_sizet(orte_buffer_t *buffer, void *src,
                            size_t num_vals, orte_data_type_t type);

    int orte_dps_pack_string(orte_buffer_t *buffer, void *src,
                             size_t num_vals, orte_data_type_t type);

    int orte_dps_pack_data_type(orte_buffer_t *buffer, void *src,
                           size_t num_vals, orte_data_type_t type);

    int orte_dps_pack_byte_object(orte_buffer_t *buffer, void *src,
                           size_t num_vals, orte_data_type_t type);

    /*
     * Internal unpack functions
     */

    int orte_dps_unpack_null(orte_buffer_t *buffer, void *dest,
                             size_t *num_vals, orte_data_type_t type);
    int orte_dps_unpack_byte(orte_buffer_t *buffer, void *dest,
                             size_t *num_vals, orte_data_type_t type);

    int orte_dps_unpack_bool(orte_buffer_t *buffer, void *dest,
                             size_t *num_vals, orte_data_type_t type);

    int orte_dps_unpack_int(orte_buffer_t *buffer, void *dest,
                            size_t *num_vals, orte_data_type_t type);
    int orte_dps_unpack_int16(orte_buffer_t *buffer, void *dest,
                              size_t *num_vals, orte_data_type_t type);
    int orte_dps_unpack_int32(orte_buffer_t *buffer, void *dest,
                              size_t *num_vals, orte_data_type_t type);
    int orte_dps_unpack_int64(orte_buffer_t *buffer, void *dest,
                              size_t *num_vals, orte_data_type_t type);

    int orte_dps_unpack_sizet(orte_buffer_t *buffer, void *dest,
                              size_t *num_vals, orte_data_type_t type);

    int orte_dps_unpack_string(orte_buffer_t *buffer, void *dest,
                               size_t *num_vals, orte_data_type_t type);

    int orte_dps_unpack_data_type(orte_buffer_t *buffer, void *dest,
                             size_t *num_vals, orte_data_type_t type);

    int orte_dps_unpack_byte_object(orte_buffer_t *buffer, void *dest,
                             size_t *num_vals, orte_data_type_t type);

    /*
     * Internal helper functions
     */
    
    char* orte_dps_buffer_extend(orte_buffer_t *bptr, size_t bytes_to_add);

    bool orte_dps_too_small(orte_buffer_t *buffer, size_t bytes_reqd);
    
    int orte_dps_store_data_type(orte_buffer_t *buffer, orte_data_type_t type);

    int orte_dps_get_data_type(orte_buffer_t *buffer, orte_data_type_t *type);

    orte_dps_type_info_t* orte_dps_find_type(orte_data_type_t type);
    
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
