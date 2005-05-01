/*
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
/** @file:
 *
 */
#include "orte_config.h"

#include <stdio.h>
#include <sys/types.h>
#include <math.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>

#include "util/output.h"

#include "mca/gpr/gpr_types.h"
#include "mca/ns/ns_types.h"
#include "mca/rmgr/rmgr_types.h"

#include "dps/dps_internal.h"

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h> 
#endif

int orte_dps_pack_system_types(orte_buffer_t *buffer, void *src,
                               size_t num_vals, orte_data_type_t type)
{
    int rc;
    
    /* CHECK FOR A GENERIC SYSTEM TYPE */
    real_type = orte_dps_get_real_type(type);
    if (real_type != type) {
        if (ORTE_SUCCESS != (rc = orte_dps_store_data_type(buffer, real_type))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    /* now pack the native values */
    if (ORTE_SUCCESS != (rc = orte_dps_pack_native_types(buffer, src, num_vals, real_type))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


int orte_dps_pack_native_types(orte_buffer_t *buffer, void *src,
                               size_t num_vals, orte_data_type_t type)
{
    switch(type) {
        case ORTE_BYTE:
        case ORTE_INT8:
        case ORTE_UINT8:
            /* check to see if buffer needs extending */
            if (NULL == (dst = orte_dps_buffer_extend(buffer, num_vals))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE);
            }
            /* store the data */
            memcpy(dst, src, num_vals);
            /* update the buffer counters and pointers */
            buffer->data_ptr = (void*)((char*)dst + num_vals);
            buffer->len += num_vals;
            buffer->space -= num_vals;
            buffer->toend += num_vals;
            return ORTE_SUCCESS;
        
        case ORTE_INT16:
        case ORTE_UINT16:
            elementsize = sizeof (uint16_t);
            /* check to see if buffer needs extending */
            if (NULL == (dst = orte_dps_buffer_extend(buffer, elementsize*num_vals))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE);
            }
            /* store the data */
            dptr = (char *) dst;
            s16 = (uint16_t *) src;
            for (i=0; i<num_vals; i++) {
                /* convert the host order to network order */
                tmp_16 = htons(*s16);
                memcpy (dptr, (char*) &tmp_16, elementsize);
                dptr+=elementsize; 
                s16++;
            }
            /* update the buffer counters and pointers */
            buffer->data_ptr = (void*)dptr;
            buffer->len += elementsize*num_vals;
            buffer->space -= elementsize*num_vals;
            buffer->toend += elementsize*num_vals;
            return ORTE_SUCCESS;
        
        case ORTE_INT32:
        case ORTE_UINT32:
            elementsize = sizeof (uint32_t);
            /* check to see if buffer needs extending */
            if (NULL == (dst = orte_dps_buffer_extend(buffer, elementsize*num_vals))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE);
            }
            /* store the data */
            dptr = (char *) dst;
            s32 = (uint32_t *) src;
            for (i=0; i<num_vals; i++) {
                /* convert the host order to network order */
                tmp_32 = htonl(*s32);
                memcpy (dptr, (char*) &tmp_32, elementsize);
                dptr+=elementsize; 
                s32++;
            }
            /* update the buffer counters and pointers */
            buffer->data_ptr = (void*)dptr;
            buffer->len += elementsize*num_vals;
            buffer->space -= elementsize*num_vals;
            buffer->toend += elementsize*num_vals;
            return ORTE_SUCCESS;
        
        case ORTE_INT64:
        case ORTE_UINT64:
            elementsize = 2*sizeof (uint32_t);
            /* check to see if buffer needs extending */
            if (NULL == (dst = orte_dps_buffer_extend(buffer, elementsize*num_vals))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE);
            }
            /* store the data */
            dptr = (char *) dst;
            s32 = (uint32_t *) src;
            for (i=0; i<num_vals; i++) {
                /* convert the host order to network order */
                tmp_32 = htonl(*s32);
                memcpy (dptr, (char*) &tmp_32, sizeof(uint32_t));
                dptr += sizeof(uint32_t);
                s32++;

                /* do it twice to get 64 bits */
                tmp_32 = htonl(*s32);
                memcpy (dptr, (char*) &tmp_32, sizeof(uint32_t));
                dptr += sizeof(uint32_t);
                s32++;
            }
            /* update the buffer counters and pointers */
            buffer->data_ptr = (void*)dptr;
            buffer->len += elementsize*num_vals;
            buffer->space -= elementsize*num_vals;
            buffer->toend += elementsize*num_vals;
            return ORTE_SUCCESS;
                        
        case ORTE_FLOAT:
        case ORTE_FLOAT4:
        case ORTE_FLOAT8:
        case ORTE_FLOAT12:
        case ORTE_FLOAT16:
        case ORTE_DOUBLE:
        case ORTE_LONG_DOUBLE:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            return ORTE_ERR_NOT_IMPLEMENTED;
            break;

        case ORTE_STRING:
            str = (char **) src;
            for (i=0; i<num_vals; i++) {
                len = strlen(str[i]);  /* exclude the null terminator */
                /* store the size data type */
                if (ORTE_SUCCESS != (rc = orte_dps_store_data_type(buffer, ORTE_SIZE))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* store the real size data type */
                if (ORTE_SUCCESS != (rc = orte_dps_store_data_type(buffer, DPS_TYPE_SIZE_T))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* store the length */
                if (ORTE_SUCCESS != (rc =
                        orte_dps_pack_buffer(buffer, &len, 1, DPS_TYPE_SIZE_T))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* check to see if buffer needs extending */
                if (NULL == (dst = orte_dps_buffer_extend(buffer, len))) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE);
                }
                dptr = (char*)dst;
                memcpy(dptr, str[i], len); /* copy str to buffer */
                /* update the buffer counters and pointers */
                buffer->data_ptr = (void*)(dptr + len);
                buffer->len += len;
                buffer->space -= len;
                buffer->toend += len;
            }
            return ORTE_SUCCESS;
            
    }
}
