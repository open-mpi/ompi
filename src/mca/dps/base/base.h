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

#include "dps.h"

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

/*
 * globals needed within dps
 */
extern bool orte_dps_debug;
extern int orte_dps_page_size;

/*
 * Implementations of API functions
 */
int orte_dps_pack(orte_buffer_t *buffer, void *src,
                  size_t num_vals,
                  orte_data_type_t type);

int orte_dps_unpack(orte_buffer_t *buffer, void *dest,
                  size_t *max_num_vals,
                  orte_data_type_t type);
                  
int orte_dps_pack_nobuffer(void *dst, void *src, size_t num_vals,
                    orte_data_type_t type, size_t *num_bytes);

int orte_dps_unpack_nobuffer(void *dst, void *src, size_t num_values,
                             orte_data_type_t type,
                             size_t *mem_left, size_t *num_bytes);

int orte_dps_peek(orte_buffer_t *buffer,
                  orte_data_type_t *type,
                  size_t *number);

int orte_dps_unload(orte_buffer_t *buffer,
                    void **payload,
                    size_t *size);

int orte_dps_load(orte_buffer_t *buffer,
                  void *payload,
                  size_t size);

int orte_dps_dump_buffer_simple(orte_buffer_t *buffer, int outid);

int orte_dps_dump_buffer(orte_buffer_t *buffer, int outid);

/*
 * Totally internal functions
 */
size_t orte_dps_memory_required(void *src, size_t num_vals, orte_data_type_t type);

int orte_dps_buffer_extend (orte_buffer_t *bptr, size_t mem_req);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
