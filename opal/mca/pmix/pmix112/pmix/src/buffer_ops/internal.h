/* -*- C -*-
 *
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#ifndef PMIX_BFROP_INTERNAL_H_
#define PMIX_BFROP_INTERNAL_H_

#include <private/autogen/config.h>
#include <pmix/rename.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h> /* for struct timeval */
#endif

#include "src/class/pmix_pointer_array.h"

#include "buffer_ops.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#if PMIX_HAVE_HWLOC
#include PMIX_HWLOC_HEADER
#endif

BEGIN_C_DECLS

/*
 * The default starting chunk size
 */
#define PMIX_BFROP_DEFAULT_INITIAL_SIZE  128
/*
 * The default threshold size when we switch from doubling the
 * buffer size to addatively increasing it
 */
#define PMIX_BFROP_DEFAULT_THRESHOLD_SIZE 1024

/*
 * Internal type corresponding to size_t.  Do not use this in
 * interface calls - use PMIX_SIZE instead.
 */
#if SIZEOF_SIZE_T == 1
#define BFROP_TYPE_SIZE_T PMIX_UINT8
#elif SIZEOF_SIZE_T == 2
#define BFROP_TYPE_SIZE_T PMIX_UINT16
#elif SIZEOF_SIZE_T == 4
#define BFROP_TYPE_SIZE_T PMIX_UINT32
#elif SIZEOF_SIZE_T == 8
#define BFROP_TYPE_SIZE_T PMIX_UINT64
#else
#error Unsupported size_t size!
#endif

/*
 * Internal type corresponding to bool.  Do not use this in interface
 * calls - use PMIX_BOOL instead.
 */
#if SIZEOF__BOOL == 1
#define BFROP_TYPE_BOOL PMIX_UINT8
#elif SIZEOF__BOOL == 2
#define BFROP_TYPE_BOOL PMIX_UINT16
#elif SIZEOF__BOOL == 4
#define BFROP_TYPE_BOOL PMIX_UINT32
#elif SIZEOF__BOOL == 8
#define BFROP_TYPE_BOOL PMIX_UINT64
#else
#error Unsupported bool size!
#endif

/*
 * Internal type corresponding to int and unsigned int.  Do not use
 * this in interface calls - use PMIX_INT / PMIX_UINT instead.
 */
#if SIZEOF_INT == 1
#define BFROP_TYPE_INT PMIX_INT8
#define BFROP_TYPE_UINT PMIX_UINT8
#elif SIZEOF_INT == 2
#define BFROP_TYPE_INT PMIX_INT16
#define BFROP_TYPE_UINT PMIX_UINT16
#elif SIZEOF_INT == 4
#define BFROP_TYPE_INT PMIX_INT32
#define BFROP_TYPE_UINT PMIX_UINT32
#elif SIZEOF_INT == 8
#define BFROP_TYPE_INT PMIX_INT64
#define BFROP_TYPE_UINT PMIX_UINT64
#else
#error Unsupported int size!
#endif

/*
 * Internal type corresponding to pid_t.  Do not use this in interface
 * calls - use PMIX_PID instead.
 */
#if SIZEOF_PID_T == 1
#define BFROP_TYPE_PID_T PMIX_UINT8
#elif SIZEOF_PID_T == 2
#define BFROP_TYPE_PID_T PMIX_UINT16
#elif SIZEOF_PID_T == 4
#define BFROP_TYPE_PID_T PMIX_UINT32
#elif SIZEOF_PID_T == 8
#define BFROP_TYPE_PID_T PMIX_UINT64
#else
#error Unsupported pid_t size!
#endif

/* Unpack generic size macros */
#define UNPACK_SIZE_MISMATCH(unpack_type, remote_type, ret)                 \
    do {                                                                    \
        switch(remote_type) {                                               \
        case PMIX_UINT8:                                                    \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, uint8_t, remote_type);  \
            break;                                                          \
        case PMIX_INT8:                                                     \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, int8_t, remote_type);   \
            break;                                                          \
        case PMIX_UINT16:                                                   \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, uint16_t, remote_type); \
            break;                                                          \
        case PMIX_INT16:                                                    \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, int16_t, remote_type);  \
            break;                                                          \
        case PMIX_UINT32:                                                   \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, uint32_t, remote_type); \
            break;                                                          \
        case PMIX_INT32:                                                    \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, int32_t, remote_type);  \
            break;                                                          \
        case PMIX_UINT64:                                                   \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, uint64_t, remote_type); \
            break;                                                          \
        case PMIX_INT64:                                                    \
            UNPACK_SIZE_MISMATCH_FOUND(unpack_type, int64_t, remote_type);  \
            break;                                                          \
        default:                                                            \
            ret = PMIX_ERR_NOT_FOUND;                                       \
        }                                                                   \
    } while (0)

/* NOTE: do not need to deal with endianness here, as the unpacking of
   the underling sender-side type will do that for us.  Repeat: the
   data in tmpbuf[] is already in host byte order. */
#define UNPACK_SIZE_MISMATCH_FOUND(unpack_type, tmptype, tmpbfroptype)        \
    do {                                                                    \
        int32_t i;                                                          \
        tmptype *tmpbuf = (tmptype*)malloc(sizeof(tmptype) * (*num_vals));  \
        ret = pmix_bfrop_unpack_buffer(buffer, tmpbuf, num_vals, tmpbfroptype); \
        for (i = 0 ; i < *num_vals ; ++i) {                                 \
            ((unpack_type*) dest)[i] = (unpack_type)(tmpbuf[i]);            \
        }                                                                   \
        free(tmpbuf);                                                       \
    } while (0)


/**
 * Internal struct used for holding registered bfrop functions
 */
typedef struct {
    pmix_object_t super;
    /* type identifier */
    pmix_data_type_t odti_type;
    /** Debugging string name */
    char *odti_name;
    /** Pack function */
    pmix_bfrop_pack_fn_t odti_pack_fn;
    /** Unpack function */
    pmix_bfrop_unpack_fn_t odti_unpack_fn;
    /** copy function */
    pmix_bfrop_copy_fn_t odti_copy_fn;
    /** print function */
    pmix_bfrop_print_fn_t odti_print_fn;
} pmix_bfrop_type_info_t;
PMIX_DECLSPEC PMIX_CLASS_DECLARATION(pmix_bfrop_type_info_t);

/*
 * globals needed within bfrop
 */
extern bool pmix_bfrop_initialized;
extern int pmix_bfrop_initial_size;
extern int pmix_bfrop_threshold_size;
extern pmix_pointer_array_t pmix_bfrop_types;
extern pmix_data_type_t pmix_bfrop_num_reg_types;

/* macro for registering data types */
#define PMIX_REGISTER_TYPE(n, t, p, u, c, pr)                           \
    do {                                                                \
        pmix_bfrop_type_info_t *_info;                                  \
        _info = PMIX_NEW(pmix_bfrop_type_info_t);                        \
        _info->odti_name = strdup((n));                                 \
        _info->odti_type = (t);                                         \
        _info->odti_pack_fn = (pmix_bfrop_pack_fn_t)(p);                \
        _info->odti_unpack_fn = (pmix_bfrop_unpack_fn_t)(u);            \
        _info->odti_copy_fn = (pmix_bfrop_copy_fn_t)(c) ;               \
        _info->odti_print_fn = (pmix_bfrop_print_fn_t)(pr) ;            \
        pmix_pointer_array_set_item(&pmix_bfrop_types, (t), _info);     \
        ++pmix_bfrop_num_reg_types;                                     \
    } while(0);

/*
 * Implementations of API functions
 */

int pmix_bfrop_pack(pmix_buffer_t *buffer, const void *src,
                  int32_t num_vals,
                  pmix_data_type_t type);
int pmix_bfrop_unpack(pmix_buffer_t *buffer, void *dest,
                    int32_t *max_num_vals,
                    pmix_data_type_t type);

int pmix_bfrop_copy(void **dest, void *src, pmix_data_type_t type);

int pmix_bfrop_print(char **output, char *prefix, void *src, pmix_data_type_t type);

int pmix_bfrop_copy_payload(pmix_buffer_t *dest, pmix_buffer_t *src);

/*
 * Specialized functions
 */
PMIX_DECLSPEC    pmix_status_t pmix_bfrop_pack_buffer(pmix_buffer_t *buffer, const void *src,
                                                      int32_t num_vals, pmix_data_type_t type);

PMIX_DECLSPEC    pmix_status_t pmix_bfrop_unpack_buffer(pmix_buffer_t *buffer, void *dst,
                                                        int32_t *num_vals, pmix_data_type_t type);

/*
 * Internal pack functions
 */

int pmix_bfrop_pack_bool(pmix_buffer_t *buffer, const void *src,
                         int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_byte(pmix_buffer_t *buffer, const void *src,
                         int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_string(pmix_buffer_t *buffer, const void *src,
                           int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_sizet(pmix_buffer_t *buffer, const void *src,
                          int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_pid(pmix_buffer_t *buffer, const void *src,
                        int32_t num_vals, pmix_data_type_t type);

int pmix_bfrop_pack_int(pmix_buffer_t *buffer, const void *src,
                        int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_int16(pmix_buffer_t *buffer, const void *src,
                          int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_int32(pmix_buffer_t *buffer, const void *src,
                          int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_datatype(pmix_buffer_t *buffer, const void *src,
                          int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_int64(pmix_buffer_t *buffer, const void *src,
                          int32_t num_vals, pmix_data_type_t type);

int pmix_bfrop_pack_float(pmix_buffer_t *buffer, const void *src,
                          int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_double(pmix_buffer_t *buffer, const void *src,
                           int32_t num_vals, pmix_data_type_t type);

int pmix_bfrop_pack_timeval(pmix_buffer_t *buffer, const void *src,
                            int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_time(pmix_buffer_t *buffer, const void *src,
                         int32_t num_vals, pmix_data_type_t type);

#if PMIX_HAVE_HWLOC
int pmix_bfrop_pack_topo(pmix_buffer_t *buffer, const void *src,
                         int32_t num_vals, pmix_data_type_t type);
#endif
int pmix_bfrop_pack_value(pmix_buffer_t *buffer, const void *src,
                          int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_array(pmix_buffer_t *buffer, const void *src,
                          int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_proc(pmix_buffer_t *buffer, const void *src,
                          int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_app(pmix_buffer_t *buffer, const void *src,
                        int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_info(pmix_buffer_t *buffer, const void *src,
                         int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_buf(pmix_buffer_t *buffer, const void *src,
                        int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_kval(pmix_buffer_t *buffer, const void *src,
                         int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_modex(pmix_buffer_t *buffer, const void *src,
                         int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_persist(pmix_buffer_t *buffer, const void *src,
                            int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_bo(pmix_buffer_t *buffer, const void *src,
                       int32_t num_vals, pmix_data_type_t type);
int pmix_bfrop_pack_pdata(pmix_buffer_t *buffer, const void *src,
                          int32_t num_vals, pmix_data_type_t type);

/*
 * Internal unpack functions
 */
int pmix_bfrop_unpack_bool(pmix_buffer_t *buffer, void *dest,
                           int32_t *num_vals, pmix_data_type_t type);
pmix_status_t pmix_bfrop_unpack_byte(pmix_buffer_t *buffer, void *dest,
                                     int32_t *num_vals, pmix_data_type_t type);
pmix_status_t pmix_bfrop_unpack_string(pmix_buffer_t *buffer, void *dest,
                                       int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_sizet(pmix_buffer_t *buffer, void *dest,
                            int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_pid(pmix_buffer_t *buffer, void *dest,
                          int32_t *num_vals, pmix_data_type_t type);

pmix_status_t pmix_bfrop_unpack_int(pmix_buffer_t *buffer, void *dest,
                                    int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_int16(pmix_buffer_t *buffer, void *dest,
                            int32_t *num_vals, pmix_data_type_t type);
pmix_status_t pmix_bfrop_unpack_int32(pmix_buffer_t *buffer, void *dest,
                                      int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_datatype(pmix_buffer_t *buffer, void *dest,
                            int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_int64(pmix_buffer_t *buffer, void *dest,
                            int32_t *num_vals, pmix_data_type_t type);

int pmix_bfrop_unpack_float(pmix_buffer_t *buffer, void *dest,
                            int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_double(pmix_buffer_t *buffer, void *dest,
                             int32_t *num_vals, pmix_data_type_t type);

int pmix_bfrop_unpack_timeval(pmix_buffer_t *buffer, void *dest,
                              int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_time(pmix_buffer_t *buffer, void *dest,
                           int32_t *num_vals, pmix_data_type_t type);

#if PMIX_HAVE_HWLOC
int pmix_bfrop_unpack_topo(pmix_buffer_t *buffer, void *dest,
                           int32_t *num_vals, pmix_data_type_t type);
#endif
pmix_status_t pmix_bfrop_unpack_value(pmix_buffer_t *buffer, void *dest,
                                      int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_array(pmix_buffer_t *buffer, void *dest,
                            int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_proc(pmix_buffer_t *buffer, void *dest,
                            int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_app(pmix_buffer_t *buffer, void *dest,
                          int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_info(pmix_buffer_t *buffer, void *dest,
                           int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_buf(pmix_buffer_t *buffer, void *dest,
                          int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_kval(pmix_buffer_t *buffer, void *dest,
                           int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_modex(pmix_buffer_t *buffer, void *dest,
                           int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_persist(pmix_buffer_t *buffer, void *dest,
                              int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_bo(pmix_buffer_t *buffer, void *dest,
                         int32_t *num_vals, pmix_data_type_t type);
int pmix_bfrop_unpack_pdata(pmix_buffer_t *buffer, void *dest,
                            int32_t *num_vals, pmix_data_type_t type);

/*
 * Internal copy functions
 */

int pmix_bfrop_std_copy(void **dest, void *src, pmix_data_type_t type);

int pmix_bfrop_copy_string(char **dest, char *src, pmix_data_type_t type);

#if PMIX_HAVE_HWLOC
int pmix_bfrop_copy_topo(hwloc_topology_t *dest,
                         hwloc_topology_t src,
                         pmix_data_type_t type);
#endif
int pmix_bfrop_copy_value(pmix_value_t **dest, pmix_value_t *src,
                          pmix_data_type_t type);
int pmix_bfrop_copy_array(pmix_info_array_t **dest, pmix_info_array_t *src,
                          pmix_data_type_t type);
int pmix_bfrop_copy_proc(pmix_proc_t **dest, pmix_proc_t *src,
                          pmix_data_type_t type);
int pmix_bfrop_copy_app(pmix_app_t **dest, pmix_app_t *src,
                        pmix_data_type_t type);
int pmix_bfrop_copy_info(pmix_info_t **dest, pmix_info_t *src,
                         pmix_data_type_t type);
int pmix_bfrop_copy_buf(pmix_buffer_t **dest, pmix_buffer_t *src,
                        pmix_data_type_t type);
int pmix_bfrop_copy_kval(pmix_kval_t **dest, pmix_kval_t *src,
                         pmix_data_type_t type);
int pmix_bfrop_copy_modex(pmix_modex_data_t **dest, pmix_modex_data_t *src,
                          pmix_data_type_t type);
int pmix_bfrop_copy_persist(pmix_persistence_t **dest, pmix_persistence_t *src,
                            pmix_data_type_t type);
int pmix_bfrop_copy_bo(pmix_byte_object_t **dest, pmix_byte_object_t *src,
                       pmix_data_type_t type);
int pmix_bfrop_copy_pdata(pmix_pdata_t **dest, pmix_pdata_t *src,
                          pmix_data_type_t type);

/*
 * Internal print functions
 */
int pmix_bfrop_print_bool(char **output, char *prefix, bool *src, pmix_data_type_t type);
int pmix_bfrop_print_byte(char **output, char *prefix, uint8_t *src, pmix_data_type_t type);
int pmix_bfrop_print_string(char **output, char *prefix, char *src, pmix_data_type_t type);
int pmix_bfrop_print_size(char **output, char *prefix, size_t *src, pmix_data_type_t type);
int pmix_bfrop_print_pid(char **output, char *prefix, pid_t *src, pmix_data_type_t type);

int pmix_bfrop_print_int(char **output, char *prefix, int *src, pmix_data_type_t type);
int pmix_bfrop_print_int8(char **output, char *prefix, int8_t *src, pmix_data_type_t type);
int pmix_bfrop_print_int16(char **output, char *prefix, int16_t *src, pmix_data_type_t type);
int pmix_bfrop_print_int32(char **output, char *prefix, int32_t *src, pmix_data_type_t type);
int pmix_bfrop_print_int64(char **output, char *prefix, int64_t *src, pmix_data_type_t type);

int pmix_bfrop_print_uint(char **output, char *prefix, int *src, pmix_data_type_t type);
int pmix_bfrop_print_uint8(char **output, char *prefix, uint8_t *src, pmix_data_type_t type);
int pmix_bfrop_print_uint16(char **output, char *prefix, uint16_t *src, pmix_data_type_t type);
int pmix_bfrop_print_uint32(char **output, char *prefix, uint32_t *src, pmix_data_type_t type);
int pmix_bfrop_print_uint64(char **output, char *prefix, uint64_t *src, pmix_data_type_t type);

int pmix_bfrop_print_float(char **output, char *prefix, float *src, pmix_data_type_t type);
int pmix_bfrop_print_double(char **output, char *prefix, double *src, pmix_data_type_t type);

int pmix_bfrop_print_timeval(char **output, char *prefix, struct timeval *src, pmix_data_type_t type);
int pmix_bfrop_print_time(char **output, char *prefix, time_t *src, pmix_data_type_t type);

#if PMIX_HAVE_HWLOC
int pmix_bfrop_print_topo(char **output, char *prefix,
                          hwloc_topology_t src, pmix_data_type_t type);
#endif
int pmix_bfrop_print_value(char **output, char *prefix, pmix_value_t *src, pmix_data_type_t type);
int pmix_bfrop_print_array(char **output, char *prefix,
                           pmix_info_array_t *src, pmix_data_type_t type);
int pmix_bfrop_print_proc(char **output, char *prefix,
                           pmix_proc_t *src, pmix_data_type_t type);
int pmix_bfrop_print_app(char **output, char *prefix,
                         pmix_app_t *src, pmix_data_type_t type);
int pmix_bfrop_print_info(char **output, char *prefix,
                          pmix_info_t *src, pmix_data_type_t type);
int pmix_bfrop_print_buf(char **output, char *prefix,
                          pmix_buffer_t *src, pmix_data_type_t type);
int pmix_bfrop_print_kval(char **output, char *prefix,
                          pmix_kval_t *src, pmix_data_type_t type);
int pmix_bfrop_print_modex(char **output, char *prefix,
                           pmix_modex_data_t *src, pmix_data_type_t type);
int pmix_bfrop_print_persist(char **output, char *prefix,
                             pmix_persistence_t *src, pmix_data_type_t type);
int pmix_bfrop_print_bo(char **output, char *prefix,
                        pmix_byte_object_t *src, pmix_data_type_t type);
int pmix_bfrop_print_pdata(char **output, char *prefix,
                           pmix_pdata_t *src, pmix_data_type_t type);

/*
 * Internal helper functions
 */

char* pmix_bfrop_buffer_extend(pmix_buffer_t *bptr, size_t bytes_to_add);

bool pmix_bfrop_too_small(pmix_buffer_t *buffer, size_t bytes_reqd);

pmix_bfrop_type_info_t* pmix_bfrop_find_type(pmix_data_type_t type);

int pmix_bfrop_store_data_type(pmix_buffer_t *buffer, pmix_data_type_t type);

pmix_status_t pmix_bfrop_get_data_type(pmix_buffer_t *buffer, pmix_data_type_t *type);

END_C_DECLS

#endif
