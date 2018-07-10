/*
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
 * Copyright (c) 2015-2018 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>


#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/include/pmix_globals.h"

#include "src/mca/bfrops/base/base.h"

/* define two public functions */
PMIX_EXPORT void pmix_value_load(pmix_value_t *v, const void *data,
                                 pmix_data_type_t type)
{
    pmix_bfrops_base_value_load(v, data, type);
}

PMIX_EXPORT pmix_status_t pmix_value_xfer(pmix_value_t *dest,
                                          pmix_value_t *src)
{
    return pmix_bfrops_base_value_xfer(dest, src);
}

void pmix_bfrops_base_value_load(pmix_value_t *v, const void *data,
                                 pmix_data_type_t type)
{
    pmix_byte_object_t *bo;
    pmix_proc_info_t *pi;

    v->type = type;
    if (NULL == data) {
        /* just set the fields to zero */
        memset(&v->data, 0, sizeof(v->data));
        if (PMIX_BOOL == type) {
          v->data.flag = true; // existence of the attribute indicates true unless specified different
        }
    } else {
        switch(type) {
        case PMIX_UNDEF:
            break;
        case PMIX_BOOL:
            memcpy(&(v->data.flag), data, 1);
            break;
        case PMIX_BYTE:
            memcpy(&(v->data.byte), data, 1);
            break;
        case PMIX_STRING:
            v->data.string = strdup(data);
            break;
        case PMIX_SIZE:
            memcpy(&(v->data.size), data, sizeof(size_t));
            break;
        case PMIX_PID:
            memcpy(&(v->data.pid), data, sizeof(pid_t));
            break;
        case PMIX_INT:
            memcpy(&(v->data.integer), data, sizeof(int));
            break;
        case PMIX_INT8:
            memcpy(&(v->data.int8), data, 1);
            break;
        case PMIX_INT16:
            memcpy(&(v->data.int16), data, 2);
            break;
        case PMIX_INT32:
            memcpy(&(v->data.int32), data, 4);
            break;
        case PMIX_INT64:
            memcpy(&(v->data.int64), data, 8);
            break;
        case PMIX_UINT:
            memcpy(&(v->data.uint), data, sizeof(int));
            break;
        case PMIX_UINT8:
            memcpy(&(v->data.uint8), data, 1);
            break;
        case PMIX_UINT16:
            memcpy(&(v->data.uint16), data, 2);
            break;
        case PMIX_UINT32:
            memcpy(&(v->data.uint32), data, 4);
            break;
        case PMIX_UINT64:
            memcpy(&(v->data.uint64), data, 8);
            break;
        case PMIX_FLOAT:
            memcpy(&(v->data.fval), data, sizeof(float));
            break;
        case PMIX_DOUBLE:
            memcpy(&(v->data.dval), data, sizeof(double));
            break;
        case PMIX_TIMEVAL:
            memcpy(&(v->data.tv), data, sizeof(struct timeval));
            break;
        case PMIX_TIME:
            memcpy(&(v->data.time), data, sizeof(time_t));
            break;
        case PMIX_STATUS:
            memcpy(&(v->data.status), data, sizeof(pmix_status_t));
            break;
        case PMIX_PROC_RANK:
            memcpy(&(v->data.rank), data, sizeof(pmix_rank_t));
            break;
        case PMIX_PROC:
            PMIX_PROC_CREATE(v->data.proc, 1);
            if (NULL == v->data.proc) {
                PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
                return;
            }
            memcpy(v->data.proc, data, sizeof(pmix_proc_t));
            break;
        case PMIX_BYTE_OBJECT:
            bo = (pmix_byte_object_t*)data;
            v->data.bo.bytes = (char*)malloc(bo->size);
            if (NULL == v->data.bo.bytes) {
                PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
                return;
            }
            memcpy(v->data.bo.bytes, bo->bytes, bo->size);
            memcpy(&(v->data.bo.size), &bo->size, sizeof(size_t));
            break;
        case PMIX_PERSIST:
            memcpy(&(v->data.persist), data, sizeof(pmix_persistence_t));
            break;
        case PMIX_SCOPE:
            memcpy(&(v->data.scope), data, sizeof(pmix_scope_t));
            break;
        case PMIX_DATA_RANGE:
            memcpy(&(v->data.range), data, sizeof(pmix_data_range_t));
            break;
        case PMIX_PROC_STATE:
            memcpy(&(v->data.state), data, sizeof(pmix_proc_state_t));
            break;
        case PMIX_PROC_INFO:
            PMIX_PROC_INFO_CREATE(v->data.pinfo, 1);
            if (NULL == v->data.pinfo) {
                PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
                return;
            }
            pi = (pmix_proc_info_t*)data;
            memcpy(&(v->data.pinfo->proc), &pi->proc, sizeof(pmix_proc_t));
            if (NULL != pi->hostname) {
                v->data.pinfo->hostname = strdup(pi->hostname);
            }
            if (NULL != pi->executable_name) {
                v->data.pinfo->executable_name = strdup(pi->executable_name);
            }
            memcpy(&(v->data.pinfo->pid), &pi->pid, sizeof(pid_t));
            memcpy(&(v->data.pinfo->exit_code), &pi->exit_code, sizeof(int));
            break;
        case PMIX_POINTER:
            memcpy(&(v->data.ptr), data, sizeof(void*));
            break;
        default:
            /* silence warnings */
            break;
        }
    }
    return;
}

pmix_status_t pmix_bfrops_base_value_unload(pmix_value_t *kv,
                                            void **data,
                                            size_t *sz)
{
    pmix_status_t rc;

    rc = PMIX_SUCCESS;
    if (NULL == data ||
       (NULL == *data && PMIX_STRING != kv->type && PMIX_BYTE_OBJECT != kv->type)) {
        rc = PMIX_ERR_BAD_PARAM;
    } else {
        switch(kv->type) {
        case PMIX_UNDEF:
            rc = PMIX_ERR_UNKNOWN_DATA_TYPE;
            break;
        case PMIX_BOOL:
            memcpy(*data, &(kv->data.flag), 1);
            *sz = 1;
            break;
        case PMIX_BYTE:
            memcpy(*data, &(kv->data.byte), 1);
            *sz = 1;
            break;
        case PMIX_STRING:
            if (NULL != kv->data.string) {
                *data = strdup(kv->data.string);
                *sz = strlen(kv->data.string);
            }
            break;
        case PMIX_SIZE:
            memcpy(*data, &(kv->data.size), sizeof(size_t));
            *sz = sizeof(size_t);
            break;
        case PMIX_PID:
            memcpy(*data, &(kv->data.pid), sizeof(pid_t));
            *sz = sizeof(pid_t);
            break;
        case PMIX_INT:
            memcpy(*data, &(kv->data.integer), sizeof(int));
            *sz = sizeof(int);
            break;
        case PMIX_INT8:
            memcpy(*data, &(kv->data.int8), 1);
            *sz = 1;
            break;
        case PMIX_INT16:
            memcpy(*data, &(kv->data.int16), 2);
            *sz = 2;
            break;
        case PMIX_INT32:
            memcpy(*data, &(kv->data.int32), 4);
            *sz = 4;
            break;
        case PMIX_INT64:
            memcpy(*data, &(kv->data.int64), 8);
            *sz = 8;
            break;
        case PMIX_UINT:
            memcpy(*data, &(kv->data.uint), sizeof(int));
            *sz = sizeof(int);
            break;
        case PMIX_UINT8:
            memcpy(*data, &(kv->data.uint8), 1);
            *sz = 1;
            break;
        case PMIX_UINT16:
            memcpy(*data, &(kv->data.uint16), 2);
            *sz = 2;
            break;
        case PMIX_UINT32:
            memcpy(*data, &(kv->data.uint32), 4);
            *sz = 4;
            break;
        case PMIX_UINT64:
            memcpy(*data, &(kv->data.uint64), 8);
            *sz = 8;
            break;
        case PMIX_FLOAT:
            memcpy(*data, &(kv->data.fval), sizeof(float));
            *sz = sizeof(float);
            break;
        case PMIX_DOUBLE:
            memcpy(*data, &(kv->data.dval), sizeof(double));
            *sz = sizeof(double);
            break;
        case PMIX_TIMEVAL:
            memcpy(*data, &(kv->data.tv), sizeof(struct timeval));
            *sz = sizeof(struct timeval);
            break;
        case PMIX_TIME:
            memcpy(*data, &(kv->data.time), sizeof(time_t));
            *sz = sizeof(time_t);
            break;
        case PMIX_BYTE_OBJECT:
            if (NULL != kv->data.bo.bytes && 0 < kv->data.bo.size) {
                *data = kv->data.bo.bytes;
                *sz = kv->data.bo.size;
            } else {
                *data = NULL;
                *sz = 0;
            }
            break;
        case PMIX_PERSIST:
            memcpy(*data, &(kv->data.persist), sizeof(pmix_persistence_t));
            *sz = sizeof(pmix_persistence_t);
            break;
        case PMIX_SCOPE:
            memcpy(*data, &(kv->data.scope), sizeof(pmix_scope_t));
            *sz = sizeof(pmix_scope_t);
            break;
        case PMIX_DATA_RANGE:
            memcpy(*data, &(kv->data.range), sizeof(pmix_data_range_t));
            *sz = sizeof(pmix_data_range_t);
            break;
        case PMIX_PROC_STATE:
            memcpy(*data, &(kv->data.state), sizeof(pmix_proc_state_t));
            *sz = sizeof(pmix_proc_state_t);
            break;
        case PMIX_POINTER:
            memcpy(*data, &(kv->data.ptr), sizeof(void*));
            *sz = sizeof(void*);
            break;
        default:
            /* silence warnings */
            rc = PMIX_ERROR;
            break;
        }
    }
    return rc;
}

/* compare function for pmix_value_t */
pmix_value_cmp_t pmix_bfrops_base_value_cmp(pmix_value_t *p,
                                            pmix_value_t *p1)
{
    pmix_value_cmp_t rc = PMIX_VALUE1_GREATER;

    if (p->type != p1->type) {
        return rc;
    }

    switch (p->type) {
        case PMIX_UNDEF:
            rc = PMIX_EQUAL;
            break;
        case PMIX_BOOL:
            if (p->data.flag == p1->data.flag) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_BYTE:
            if (p->data.byte == p1->data.byte) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_SIZE:
            if (p->data.size == p1->data.size) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_INT:
            if (p->data.integer == p1->data.integer) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_INT8:
            if (p->data.int8 == p1->data.int8) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_INT16:
            if (p->data.int16 == p1->data.int16) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_INT32:
            if (p->data.int32 == p1->data.int32) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_INT64:
            if (p->data.int64 == p1->data.int64) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_UINT:
            if (p->data.uint == p1->data.uint) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_UINT8:
            if (p->data.uint8 == p1->data.int8) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_UINT16:
            if (p->data.uint16 == p1->data.uint16) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_UINT32:
            if (p->data.uint32 == p1->data.uint32) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_UINT64:
            if (p->data.uint64 == p1->data.uint64) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_STRING:
            if (0 == strcmp(p->data.string, p1->data.string)) {
                rc = PMIX_EQUAL;
            }
            break;
        case PMIX_COMPRESSED_STRING:
            if (p->data.bo.size != p1->data.bo.size) {
                return false;
            } else {
                return true;
            }
        case PMIX_STATUS:
            if (p->data.status == p1->data.status) {
                rc = PMIX_EQUAL;
            }
            break;
        default:
            pmix_output(0, "COMPARE-PMIX-VALUE: UNSUPPORTED TYPE %d", (int)p->type);
    }
    return rc;
}

/* Xfer FUNCTIONS FOR GENERIC PMIX TYPES */
pmix_status_t pmix_bfrops_base_value_xfer(pmix_value_t *p,
                                          pmix_value_t *src)
{
    size_t n, m;
    pmix_status_t rc;
    char **prarray, **strarray;
    pmix_value_t *pv, *sv;
    pmix_info_t *p1, *s1;
    pmix_app_t *pa, *sa;
    pmix_pdata_t *pd, *sd;
    pmix_buffer_t *pb, *sb;
    pmix_byte_object_t *pbo, *sbo;
    pmix_kval_t *pk, *sk;
    pmix_modex_data_t *pm, *sm;
    pmix_proc_info_t *pi, *si;
    pmix_query_t *pq, *sq;

    /* copy the right field */
    p->type = src->type;
    switch (src->type) {
    case PMIX_UNDEF:
    break;
    case PMIX_BOOL:
        p->data.flag = src->data.flag;
        break;
    case PMIX_BYTE:
        p->data.byte = src->data.byte;
        break;
    case PMIX_STRING:
        if (NULL != src->data.string) {
            p->data.string = strdup(src->data.string);
        } else {
            p->data.string = NULL;
        }
        break;
    case PMIX_SIZE:
        p->data.size = src->data.size;
        break;
    case PMIX_PID:
        p->data.pid = src->data.pid;
        break;
    case PMIX_INT:
        /* to avoid alignment issues */
        memcpy(&p->data.integer, &src->data.integer, sizeof(int));
        break;
    case PMIX_INT8:
        p->data.int8 = src->data.int8;
        break;
    case PMIX_INT16:
        /* to avoid alignment issues */
        memcpy(&p->data.int16, &src->data.int16, 2);
        break;
    case PMIX_INT32:
        /* to avoid alignment issues */
        memcpy(&p->data.int32, &src->data.int32, 4);
        break;
    case PMIX_INT64:
        /* to avoid alignment issues */
        memcpy(&p->data.int64, &src->data.int64, 8);
        break;
    case PMIX_UINT:
        /* to avoid alignment issues */
        memcpy(&p->data.uint, &src->data.uint, sizeof(unsigned int));
        break;
    case PMIX_UINT8:
        p->data.uint8 = src->data.uint8;
        break;
    case PMIX_UINT16:
        /* to avoid alignment issues */
        memcpy(&p->data.uint16, &src->data.uint16, 2);
        break;
    case PMIX_UINT32:
        /* to avoid alignment issues */
        memcpy(&p->data.uint32, &src->data.uint32, 4);
        break;
    case PMIX_UINT64:
        /* to avoid alignment issues */
        memcpy(&p->data.uint64, &src->data.uint64, 8);
        break;
    case PMIX_FLOAT:
        p->data.fval = src->data.fval;
        break;
    case PMIX_DOUBLE:
        p->data.dval = src->data.dval;
        break;
    case PMIX_TIMEVAL:
        memcpy(&p->data.tv, &src->data.tv, sizeof(struct timeval));
        break;
    case PMIX_TIME:
        memcpy(&p->data.time, &src->data.time, sizeof(time_t));
        break;
    case PMIX_STATUS:
        memcpy(&p->data.status, &src->data.status, sizeof(pmix_status_t));
        break;
    case PMIX_PROC:
        PMIX_PROC_CREATE(p->data.proc, 1);
        if (NULL == p->data.proc) {
            return PMIX_ERR_NOMEM;
        }
        memcpy(p->data.proc, src->data.proc, sizeof(pmix_proc_t));
        break;
    case PMIX_PROC_RANK:
        memcpy(&p->data.rank, &src->data.rank, sizeof(pmix_rank_t));
        break;
    case PMIX_BYTE_OBJECT:
    case PMIX_COMPRESSED_STRING:
        memset(&p->data.bo, 0, sizeof(pmix_byte_object_t));
        if (NULL != src->data.bo.bytes && 0 < src->data.bo.size) {
            p->data.bo.bytes = malloc(src->data.bo.size);
            memcpy(p->data.bo.bytes, src->data.bo.bytes, src->data.bo.size);
            p->data.bo.size = src->data.bo.size;
        } else {
            p->data.bo.bytes = NULL;
            p->data.bo.size = 0;
        }
        break;
    case PMIX_PERSIST:
        memcpy(&p->data.persist, &src->data.persist, sizeof(pmix_persistence_t));
        break;
    case PMIX_SCOPE:
        memcpy(&p->data.scope, &src->data.scope, sizeof(pmix_scope_t));
        break;
    case PMIX_DATA_RANGE:
        memcpy(&p->data.range, &src->data.range, sizeof(pmix_data_range_t));
        break;
    case PMIX_PROC_STATE:
        memcpy(&p->data.state, &src->data.state, sizeof(pmix_proc_state_t));
        break;
    case PMIX_PROC_INFO:
        PMIX_PROC_INFO_CREATE(p->data.pinfo, 1);
        if (NULL != src->data.pinfo->hostname) {
            p->data.pinfo->hostname = strdup(src->data.pinfo->hostname);
        }
        if (NULL != src->data.pinfo->executable_name) {
            p->data.pinfo->executable_name = strdup(src->data.pinfo->executable_name);
        }
        memcpy(&p->data.pinfo->pid, &src->data.pinfo->pid, sizeof(pid_t));
        memcpy(&p->data.pinfo->exit_code, &src->data.pinfo->exit_code, sizeof(int));
        memcpy(&p->data.pinfo->state, &src->data.pinfo->state, sizeof(pmix_proc_state_t));
        break;
    case PMIX_DATA_ARRAY:
        p->data.darray = (pmix_data_array_t*)calloc(1, sizeof(pmix_data_array_t));
        p->data.darray->type = src->data.darray->type;
        p->data.darray->size = src->data.darray->size;
        if (0 == p->data.darray->size || NULL == src->data.darray->array) {
            p->data.darray->array = NULL;
            p->data.darray->size = 0;
            break;
        }
        /* allocate space and do the copy */
        switch (src->data.darray->type) {
            case PMIX_UINT8:
            case PMIX_INT8:
            case PMIX_BYTE:
                p->data.darray->array = (char*)malloc(src->data.darray->size);
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size);
                break;
            case PMIX_UINT16:
            case PMIX_INT16:
                p->data.darray->array = (char*)malloc(src->data.darray->size * sizeof(uint16_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(uint16_t));
                break;
            case PMIX_UINT32:
            case PMIX_INT32:
                p->data.darray->array = (char*)malloc(src->data.darray->size * sizeof(uint32_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(uint32_t));
                break;
            case PMIX_UINT64:
            case PMIX_INT64:
                p->data.darray->array = (char*)malloc(src->data.darray->size * sizeof(uint64_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(uint64_t));
                break;
            case PMIX_BOOL:
                p->data.darray->array = (char*)malloc(src->data.darray->size * sizeof(bool));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(bool));
                break;
            case PMIX_SIZE:
                p->data.darray->array = (char*)malloc(src->data.darray->size * sizeof(size_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(size_t));
                break;
            case PMIX_PID:
                p->data.darray->array = (char*)malloc(src->data.darray->size * sizeof(pid_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(pid_t));
                break;
            case PMIX_STRING:
                p->data.darray->array = (char**)malloc(src->data.darray->size * sizeof(char*));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                prarray = (char**)p->data.darray->array;
                strarray = (char**)src->data.darray->array;
                for (n=0; n < src->data.darray->size; n++) {
                    if (NULL != strarray[n]) {
                        prarray[n] = strdup(strarray[n]);
                    }
                }
                break;
            case PMIX_INT:
            case PMIX_UINT:
                p->data.darray->array = (char*)malloc(src->data.darray->size * sizeof(int));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(int));
                break;
            case PMIX_FLOAT:
                p->data.darray->array = (char*)malloc(src->data.darray->size * sizeof(float));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(float));
                break;
            case PMIX_DOUBLE:
                p->data.darray->array = (char*)malloc(src->data.darray->size * sizeof(double));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(double));
                break;
            case PMIX_TIMEVAL:
                p->data.darray->array = (struct timeval*)malloc(src->data.darray->size * sizeof(struct timeval));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(struct timeval));
                break;
            case PMIX_TIME:
                p->data.darray->array = (time_t*)malloc(src->data.darray->size * sizeof(time_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(time_t));
                break;
            case PMIX_STATUS:
                p->data.darray->array = (pmix_status_t*)malloc(src->data.darray->size * sizeof(pmix_status_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(pmix_status_t));
                break;
            case PMIX_VALUE:
                PMIX_VALUE_CREATE(p->data.darray->array, src->data.darray->size);
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                pv = (pmix_value_t*)p->data.darray->array;
                sv = (pmix_value_t*)src->data.darray->array;
                for (n=0; n < src->data.darray->size; n++) {
                    if (PMIX_SUCCESS != (rc = pmix_value_xfer(&pv[n], &sv[n]))) {
                        PMIX_VALUE_FREE(pv, src->data.darray->size);
                        return rc;
                    }
                }
                break;
            case PMIX_PROC:
                PMIX_PROC_CREATE(p->data.darray->array, src->data.darray->size);
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(pmix_proc_t));
                break;
            case PMIX_APP:
                PMIX_APP_CREATE(p->data.darray->array, src->data.darray->size);
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                pa = (pmix_app_t*)p->data.darray->array;
                sa = (pmix_app_t*)src->data.darray->array;
                for (n=0; n < src->data.darray->size; n++) {
                    if (NULL != sa[n].cmd) {
                        pa[n].cmd = strdup(sa[n].cmd);
                    }
                    if (NULL != sa[n].argv) {
                        pa[n].argv = pmix_argv_copy(sa[n].argv);
                    }
                    if (NULL != sa[n].env) {
                        pa[n].env = pmix_argv_copy(sa[n].env);
                    }
                    if (NULL != sa[n].cwd) {
                        pa[n].cwd = strdup(sa[n].cwd);
                    }
                    pa[n].maxprocs = sa[n].maxprocs;
                    if (0 < sa[n].ninfo && NULL != sa[n].info) {
                        PMIX_INFO_CREATE(pa[n].info, sa[n].ninfo);
                        if (NULL == pa[n].info) {
                            PMIX_APP_FREE(pa, src->data.darray->size);
                            return PMIX_ERR_NOMEM;
                        }
                        pa[n].ninfo = sa[n].ninfo;
                        for (m=0; m < pa[n].ninfo; m++) {
                            PMIX_INFO_XFER(&pa[n].info[m], &sa[n].info[m]);
                        }
                    }
                }
                break;
            case PMIX_INFO:
                PMIX_INFO_CREATE(p->data.darray->array, src->data.darray->size);
                p1 = (pmix_info_t*)p->data.darray->array;
                s1 = (pmix_info_t*)src->data.darray->array;
                for (n=0; n < src->data.darray->size; n++) {
                    PMIX_INFO_XFER(&p1[n], &s1[n]);
                }
                break;
            case PMIX_PDATA:
                PMIX_PDATA_CREATE(p->data.darray->array, src->data.darray->size);
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                pd = (pmix_pdata_t*)p->data.darray->array;
                sd = (pmix_pdata_t*)src->data.darray->array;
                for (n=0; n < src->data.darray->size; n++) {
                    PMIX_PDATA_XFER(&pd[n], &sd[n]);
                }
                break;
            case PMIX_BUFFER:
                p->data.darray->array = (pmix_buffer_t*)malloc(src->data.darray->size * sizeof(pmix_buffer_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                pb = (pmix_buffer_t*)p->data.darray->array;
                sb = (pmix_buffer_t*)src->data.darray->array;
                for (n=0; n < src->data.darray->size; n++) {
                    PMIX_CONSTRUCT(&pb[n], pmix_buffer_t);
                    pmix_bfrops_base_copy_payload(&pb[n], &sb[n]);
                }
                break;
            case PMIX_BYTE_OBJECT:
            case PMIX_COMPRESSED_STRING:
                p->data.darray->array = (pmix_byte_object_t*)malloc(src->data.darray->size * sizeof(pmix_byte_object_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                pbo = (pmix_byte_object_t*)p->data.darray->array;
                sbo = (pmix_byte_object_t*)src->data.darray->array;
                for (n=0; n < src->data.darray->size; n++) {
                    if (NULL != sbo[n].bytes && 0 < sbo[n].size) {
                        pbo[n].size = sbo[n].size;
                        pbo[n].bytes = (char*)malloc(pbo[n].size);
                        memcpy(pbo[n].bytes, sbo[n].bytes, pbo[n].size);
                    } else {
                        pbo[n].bytes = NULL;
                        pbo[n].size = 0;
                    }
                }
                break;
            case PMIX_KVAL:
                p->data.darray->array = (pmix_kval_t*)calloc(src->data.darray->size , sizeof(pmix_kval_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                pk = (pmix_kval_t*)p->data.darray->array;
                sk = (pmix_kval_t*)src->data.darray->array;
                for (n=0; n < src->data.darray->size; n++) {
                    if (NULL != sk[n].key) {
                        pk[n].key = strdup(sk[n].key);
                    }
                    if (NULL != sk[n].value) {
                        PMIX_VALUE_CREATE(pk[n].value, 1);
                        if (NULL == pk[n].value) {
                            free(p->data.darray->array);
                            return PMIX_ERR_NOMEM;
                        }
                        if (PMIX_SUCCESS != (rc = pmix_value_xfer(pk[n].value, sk[n].value))) {
                            return rc;
                        }
                    }
                }
                break;
            case PMIX_MODEX:
                PMIX_MODEX_CREATE(p->data.darray->array, src->data.darray->size);
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                pm = (pmix_modex_data_t*)p->data.darray->array;
                sm = (pmix_modex_data_t*)src->data.darray->array;
                for (n=0; n < src->data.darray->size; n++) {
                    memcpy(&pm[n], &sm[n], sizeof(pmix_modex_data_t));
                    if (NULL != sm[n].blob && 0 < sm[n].size) {
                        pm[n].blob = (uint8_t*)malloc(sm[n].size);
                        if (NULL == pm[n].blob) {
                            return PMIX_ERR_NOMEM;
                        }
                        memcpy(pm[n].blob, sm[n].blob, sm[n].size);
                        pm[n].size = sm[n].size;
                    } else {
                        pm[n].blob = NULL;
                        pm[n].size = 0;
                    }
                }
                break;
            case PMIX_PERSIST:
                p->data.darray->array = (pmix_persistence_t*)malloc(src->data.darray->size * sizeof(pmix_persistence_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(pmix_persistence_t));
                break;
            case PMIX_POINTER:
                p->data.darray->array = (char**)malloc(src->data.darray->size * sizeof(char*));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                prarray = (char**)p->data.darray->array;
                strarray = (char**)src->data.darray->array;
                for (n=0; n < src->data.darray->size; n++) {
                    prarray[n] = strarray[n];
                }
                break;
            case PMIX_SCOPE:
                p->data.darray->array = (pmix_scope_t*)malloc(src->data.darray->size * sizeof(pmix_scope_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(pmix_scope_t));
                break;
            case PMIX_DATA_RANGE:
                p->data.darray->array = (pmix_data_range_t*)malloc(src->data.darray->size * sizeof(pmix_data_range_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(pmix_data_range_t));
                break;
            case PMIX_COMMAND:
                p->data.darray->array = (pmix_cmd_t*)malloc(src->data.darray->size * sizeof(pmix_cmd_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(pmix_cmd_t));
                break;
            case PMIX_INFO_DIRECTIVES:
                p->data.darray->array = (pmix_info_directives_t*)malloc(src->data.darray->size * sizeof(pmix_info_directives_t));
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                memcpy(p->data.darray->array, src->data.darray->array, src->data.darray->size * sizeof(pmix_info_directives_t));
                break;
            case PMIX_PROC_INFO:
                PMIX_PROC_INFO_CREATE(p->data.darray->array, src->data.darray->size);
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                pi = (pmix_proc_info_t*)p->data.darray->array;
                si = (pmix_proc_info_t*)src->data.darray->array;
                for (n=0; n < src->data.darray->size; n++) {
                    memcpy(&pi[n].proc, &si[n].proc, sizeof(pmix_proc_t));
                    if (NULL != si[n].hostname) {
                        pi[n].hostname = strdup(si[n].hostname);
                    } else {
                        pi[n].hostname = NULL;
                    }
                    if (NULL != si[n].executable_name) {
                        pi[n].executable_name = strdup(si[n].executable_name);
                    } else {
                        pi[n].executable_name = NULL;
                    }
                    pi[n].pid = si[n].pid;
                    pi[n].exit_code = si[n].exit_code;
                    pi[n].state = si[n].state;
                }
                break;
            case PMIX_DATA_ARRAY:
                PMIX_ERROR_LOG(PMIX_ERR_NOT_SUPPORTED);
                return PMIX_ERR_NOT_SUPPORTED;  // don't support iterative arrays
            case PMIX_QUERY:
                PMIX_QUERY_CREATE(p->data.darray->array, src->data.darray->size);
                if (NULL == p->data.darray->array) {
                    return PMIX_ERR_NOMEM;
                }
                pq = (pmix_query_t*)p->data.darray->array;
                sq = (pmix_query_t*)src->data.darray->array;
                for (n=0; n < src->data.darray->size; n++) {
                    if (NULL != sq[n].keys) {
                        pq[n].keys = pmix_argv_copy(sq[n].keys);
                    }
                    if (NULL != sq[n].qualifiers && 0 < sq[n].nqual) {
                        PMIX_INFO_CREATE(pq[n].qualifiers, sq[n].nqual);
                        if (NULL == pq[n].qualifiers) {
                            PMIX_QUERY_FREE(pq, src->data.darray->size);
                            return PMIX_ERR_NOMEM;
                        }
                        for (m=0; m < sq[n].nqual; m++) {
                            PMIX_INFO_XFER(&pq[n].qualifiers[m], &sq[n].qualifiers[m]);
                        }
                        pq[n].nqual = sq[n].nqual;
                    } else {
                        pq[n].qualifiers = NULL;
                        pq[n].nqual = 0;
                    }
                }
                break;
            default:
                return PMIX_ERR_UNKNOWN_DATA_TYPE;
        }
        break;
    case PMIX_POINTER:
        memcpy(&p->data.ptr, &src->data.ptr, sizeof(void*));
        break;
    /**** DEPRECATED ****/
    case PMIX_INFO_ARRAY:
        p->data.array->size = src->data.array->size;
        if (0 < src->data.array->size) {
            p->data.array->array = (pmix_info_t*)malloc(src->data.array->size * sizeof(pmix_info_t));
            if (NULL == p->data.array->array) {
                return PMIX_ERR_NOMEM;
            }
            p1 = (pmix_info_t*)p->data.array->array;
            s1 = (pmix_info_t*)src->data.array->array;
            for (n=0; n < src->data.darray->size; n++) {
                PMIX_INFO_XFER(&p1[n], &s1[n]);
            }
        }
        break;
    /********************/
    default:
        pmix_output(0, "XFER-PMIX-VALUE: UNSUPPORTED TYPE %d", (int)src->type);
        assert(0);
        return PMIX_ERROR;
    }
    return PMIX_SUCCESS;
}


/**
 * Internal function that resizes (expands) an inuse buffer if
 * necessary.
 */
char* pmix_bfrop_buffer_extend(pmix_buffer_t *buffer, size_t bytes_to_add)
{
    size_t required, to_alloc;
    size_t pack_offset, unpack_offset;

    /* Check to see if we have enough space already */


    if ((buffer->bytes_allocated - buffer->bytes_used) >= bytes_to_add) {
        return buffer->pack_ptr;
    }

    required = buffer->bytes_used + bytes_to_add;
    if (required >= pmix_bfrops_globals.threshold_size) {
        to_alloc = ((required + pmix_bfrops_globals.threshold_size - 1)
                    / pmix_bfrops_globals.threshold_size) * pmix_bfrops_globals.threshold_size;
    } else {
        to_alloc = buffer->bytes_allocated;
        if (0 == to_alloc) {
            to_alloc = pmix_bfrops_globals.initial_size;
        }
        while (to_alloc < required) {
            to_alloc <<= 1;
        }
    }

    if (NULL != buffer->base_ptr) {
        pack_offset = ((char*) buffer->pack_ptr) - ((char*) buffer->base_ptr);
        unpack_offset = ((char*) buffer->unpack_ptr) -
            ((char*) buffer->base_ptr);
        buffer->base_ptr = (char*)realloc(buffer->base_ptr, to_alloc);
        memset(buffer->base_ptr + pack_offset, 0, to_alloc - buffer->bytes_allocated);
    } else {
        pack_offset = 0;
        unpack_offset = 0;
        buffer->bytes_used = 0;
        buffer->base_ptr = (char*)malloc(to_alloc);
        memset(buffer->base_ptr, 0, to_alloc);
    }

    if (NULL == buffer->base_ptr) {
        return NULL;
    }
    buffer->pack_ptr = ((char*) buffer->base_ptr) + pack_offset;
    buffer->unpack_ptr = ((char*) buffer->base_ptr) + unpack_offset;
    buffer->bytes_allocated = to_alloc;

    /* All done */
    return buffer->pack_ptr;
}

/*
 * Internal function that checks to see if the specified number of bytes
 * remain in the buffer for unpacking
 */
bool pmix_bfrop_too_small(pmix_buffer_t *buffer, size_t bytes_reqd)
{
    size_t bytes_remaining_packed;

    if (buffer->pack_ptr < buffer->unpack_ptr) {
        return true;
    }

    bytes_remaining_packed = buffer->pack_ptr - buffer->unpack_ptr;

    if (bytes_remaining_packed < bytes_reqd) {
        /* don't error log this - it could be that someone is trying to
         * simply read until the buffer is empty
         */
        return true;
    }

    return false;
}

pmix_status_t pmix_bfrop_store_data_type(pmix_buffer_t *buffer, pmix_data_type_t type)
{
    uint16_t tmp;
    char *dst;

    /* check to see if buffer needs extending */
     if (NULL == (dst = pmix_bfrop_buffer_extend(buffer, sizeof(tmp)))) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    tmp = pmix_htons(type);
    memcpy(dst, &tmp, sizeof(tmp));
    buffer->pack_ptr += sizeof(tmp);
    buffer->bytes_used += sizeof(tmp);

    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrop_get_data_type(pmix_buffer_t *buffer, pmix_data_type_t *type)
{
    uint16_t tmp;

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, sizeof(tmp))) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    memcpy(&tmp, buffer->unpack_ptr, sizeof(tmp));
    tmp = pmix_ntohs(tmp);
    memcpy(type, &tmp, sizeof(tmp));
    buffer->unpack_ptr += sizeof(tmp);

    return PMIX_SUCCESS;
}

const char* pmix_bfrops_base_data_type_string(pmix_pointer_array_t *regtypes,
                                              pmix_data_type_t type)
{
    pmix_bfrop_type_info_t *info;

    /* Lookup the object for this type and call it */
    if (NULL == (info = (pmix_bfrop_type_info_t*)pmix_pointer_array_get_item(regtypes, type))) {
        return NULL;
    }

    return info->odti_name;
}
