/*
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
 
/*
 * DPS Buffer Operations
 */
 
/** @file:
 *
 */

#include "orte_config.h"

#include <sys/types.h>
#include <netinet/in.h>

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "util/output.h"
#include "mca/gpr/gpr_types.h"
#include "mca/ns/ns_types.h"
#include "mca/rmgr/rmgr_types.h"
#include "dps_internal.h"

/**
 * DPS_PACK_VALUE
 */

int orte_dps_pack(orte_buffer_t *buffer, void *src,
                  size_t num_vals,
                  orte_data_type_t type)
{
    int rc;
    void *dst;
    int32_t op_size=0;
    size_t num_bytes, hdr_bytes;

	/* hdr_bytes = header for each packed type. */ 
	/* num_bytes = packed size of data type. */
	/* op_size = total size = (num_bytes+hdr_bytes) */
    
    /* check for error */
    if (!buffer || !src || 0 >= num_vals) { return (ORTE_ERROR); }
    
    dst = buffer->data_ptr;    /* get location in buffer */

    /* change into to fixed size type for local size */
    if (ORTE_INT == type || ORTE_UINT == type) {
        switch(sizeof(int)) {
            case 1:
                type = (type == ORTE_INT) ? ORTE_INT8 : ORTE_UINT8;
                break;
            case 2:
                type = (type == ORTE_INT) ? ORTE_INT16 : ORTE_UINT16;
                break;
            case 4:
                type = (type == ORTE_INT) ? ORTE_INT32 : ORTE_UINT32;
                break;
            case 8:
                type = (type == ORTE_INT) ? ORTE_INT64 : ORTE_UINT64;
                break;
            default:
                return ORTE_ERR_NOT_IMPLEMENTED;
        }
    }

    /* calculate the required memory size for this operation */
    if (0 == (op_size = orte_dps_memory_required(src, num_vals, type))) {  /* got error */
        return ORTE_ERROR;
    }
   
    /* add in the correct space for the pack type */
    hdr_bytes = orte_dps_memory_required(NULL, 1, ORTE_DATA_TYPE);
    
    /* add in the space to store the number of values */
    hdr_bytes += sizeof(uint32_t);

	/* total space needed */
	op_size += hdr_bytes;

    /* check to see if current buffer has enough room */
    if (op_size > buffer->space) {  /* need to extend the buffer */
        if (ORTE_SUCCESS != (rc = orte_dps_buffer_extend(buffer, op_size))) { /* got an error */
            return rc;
        }
        dst = buffer->data_ptr;  /* need to reset the dst since it could have moved */
    }
    
    /* check for size of generic data types so they can be properly packed
     * NOTE we convert the generic data type flag to a hard type for storage
     * to handle heterogeneity
     */

    
    if (ORTE_SIZE == type) {
        switch(sizeof(size_t)) {
            case 1:
                type = (type == ORTE_SIZE) ? ORTE_INT8 : ORTE_UINT8;
                break;
            case 2:
                type = (type == ORTE_SIZE) ? ORTE_INT16 : ORTE_UINT16;
                break;
            case 4:
                type = (type == ORTE_SIZE) ? ORTE_INT32 : ORTE_UINT32;
                break;
            case 8:
                type = (type == ORTE_SIZE) ? ORTE_INT64 : ORTE_UINT64;
                break;
            default:
                return ORTE_ERR_NOT_IMPLEMENTED;
        }
    }
    
    /* store the data type */
    if (ORTE_SUCCESS != (rc = orte_dps_pack_nobuffer(dst, &type, 1,
                                        ORTE_DATA_TYPE, &num_bytes))) {
        return rc;
    }

    dst = (void *)((char*)dst + num_bytes);
    
    /* store the number of values as uint32_t */
    if (ORTE_SUCCESS != (rc = orte_dps_pack_nobuffer(dst, &num_vals, 1,
                                        ORTE_UINT32, &num_bytes))) {
        return rc;
    }
    dst = (void *)((char*)dst + num_bytes);

    /* pack the data */
    if (ORTE_SUCCESS != (rc = orte_dps_pack_nobuffer(dst, src, num_vals,
                                        type, &num_bytes))) {
        return rc;
    }

	/* debugging */
	if (num_bytes+sizeof(uint32_t)+orte_dps_memory_required(NULL, 1, ORTE_DATA_TYPE) != (size_t)op_size) {
		fprintf(stderr,"orte_dps_pack: Ops, num_bytes %d + headers %d = %d, but op_size was %d?!\n", 
				(int)num_bytes, (int)hdr_bytes,  (int)(num_bytes+hdr_bytes), (int)op_size);
	}
/* 	fflush(stdout); fflush(stderr); */
/* 	fprintf(stderr,"packed total of %d bytes. Hdr %d datatype %d\n", op_size, hdr_bytes, num_bytes); */
/* 	fflush(stdout); fflush(stderr); */

    
    /* ok, we managed to pack some more stuff, so update all ptrs/cnts */
    buffer->data_ptr = (void*)((char*)dst + num_bytes);
    buffer->len += op_size;
    buffer->toend += op_size;
    buffer->space -= op_size;

    return ORTE_SUCCESS;
}


int orte_dps_pack_nobuffer(void *dst, void *src, size_t num_vals,
                    orte_data_type_t type, size_t *num_bytes)
{
    size_t i, len, n;
    uint16_t * d16;
    uint16_t * s16;
    uint32_t * d32;
    uint32_t * s32;
    bool *bool_src;
    uint8_t *bool_dst;
    uint8_t *dbyte;
    char **str;
    char *dstr;
    orte_process_name_t *dn, *sn;
    orte_byte_object_t *sbyteptr;
    orte_gpr_keyval_t **keyval;
    orte_gpr_value_t **values;
	orte_app_context_t **app_context;
	orte_app_context_map_t **app_context_map;
    orte_gpr_subscription_t **subs;
    orte_gpr_notify_data_t **data;

    /* initialize the number of bytes */
    *num_bytes = 0;
    
    /* pack the data */
    switch(type) {

        case ORTE_DATA_TYPE:
        case ORTE_NODE_STATE:
        case ORTE_PROC_STATE:
        case ORTE_EXIT_CODE:
        case ORTE_BYTE:
        case ORTE_INT8:
        case ORTE_UINT8:
            
            memcpy(dst, src, num_vals);
            *num_bytes = num_vals;
            break;
        
        case ORTE_NOTIFY_ACTION:
        case ORTE_GPR_ADDR_MODE:
        case ORTE_GPR_CMD:
        case ORTE_INT16:
        case ORTE_UINT16:
            d16 = (uint16_t *) dst;
            s16 = (uint16_t *) src;
            for (i=0; i<num_vals; i++) {
                /* convert the host order to network order */
                *d16 = htons(*s16);
                d16++; s16++;
            }
            *num_bytes = num_vals * sizeof(uint16_t);
            break;
        
        case ORTE_VPID:
        case ORTE_JOBID:
        case ORTE_CELLID:
        case ORTE_GPR_NOTIFY_ID:
        case ORTE_INT32:
        case ORTE_UINT32:
            d32 = (uint32_t *) dst;
            s32 = (uint32_t *) src;
            for (i=0; i<num_vals; i++) {
                /* convert the host order to network order */
                *d32 = htonl(*s32);
                d32++; s32++;
            }
            *num_bytes = num_vals * sizeof(uint32_t);
            break;

        case ORTE_INT64:
        case ORTE_UINT64:
            return ORTE_ERR_NOT_IMPLEMENTED;
            break;
                        
        case ORTE_FLOAT:
        case ORTE_FLOAT4:
        case ORTE_FLOAT8:
        case ORTE_FLOAT12:
        case ORTE_FLOAT16:
        case ORTE_DOUBLE:
        case ORTE_LONG_DOUBLE:
            return ORTE_ERR_NOT_IMPLEMENTED;
            break;

        case ORTE_BOOL:
            bool_src = (bool *) src;
            bool_dst = (uint8_t *) dst;
            for (i=0; i<num_vals; i++) {
                /* pack native bool as uint8_t */
                *bool_dst = *bool_src ? (uint8_t)true : (uint8_t)false;
                bool_dst++; bool_src++;
            }
            *num_bytes = num_vals * sizeof(uint8_t);
            break;

        case ORTE_STRING:
            str = (char **) src;
            d32 = (uint32_t *) dst;
            for (i=0; i<num_vals; i++) {
                len = strlen(str[i]);  /* exclude the null terminator */
                *d32 = htonl(len);
                d32++;
                dstr = (char *) d32;
                memcpy(dstr, str[i], len);
                d32 = (uint32_t *)(dstr + len);
                *num_bytes += len + sizeof(uint32_t);
            }
            break;
            
        case ORTE_NAME:
            dn = (orte_process_name_t*) dst;
            sn = (orte_process_name_t*) src;
            for (i=0; i<num_vals; i++) {
                dn->cellid = htonl(sn->cellid);
                dn->jobid = htonl(sn->jobid);
                dn->vpid = htonl(sn->vpid);
                dn++; sn++;
            }
            *num_bytes = num_vals * sizeof(orte_process_name_t);
            break;
            
        case ORTE_BYTE_OBJECT:
            sbyteptr = (orte_byte_object_t *) src;
            dbyte = (uint8_t *) dst;
            for (i=0; i<num_vals; i++) {
                /* pack number of bytes */
                d32 = (uint32_t*)dbyte;
                *d32 = htonl(sbyteptr->size);
                d32++;
                dbyte = (void*)d32;
                *num_bytes += sizeof(uint32_t);
                /* pack actual bytes */
                memcpy(dbyte, sbyteptr->bytes, sbyteptr->size);
                dbyte = (uint8_t*)(dbyte + sbyteptr->size);
                *num_bytes += sbyteptr->size;
                sbyteptr++;
            }
            break;

        case ORTE_KEYVAL:
            /* array of pointers to keyval objects - need to pack the objects */
            keyval = (orte_gpr_keyval_t**) src;
			/* use temp count of bytes packed 'n'. Must add these to num_bytes at each stage */
            for (i=0; i < num_vals; i++) {
                /* pack the key */
				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(keyval[i]->key)), 1, ORTE_STRING, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
				*num_bytes+=n;

                /* pack the data type so we can read it for unpacking */
				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst, &(keyval[i]->type), 1,
                                 ORTE_DATA_TYPE, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
				*num_bytes+=n;

                /* pack the value */
				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst, &(keyval[i]->value), 1,
                                 keyval[i]->type, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
				*num_bytes+=n;
            }
            break;
        
        case ORTE_GPR_VALUE:
            /* array of pointers to value objects - need to pack the objects */
            values = (orte_gpr_value_t**) src;
            for (i=0; i<num_vals; i++) {
                /* pack the address mode */
                n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(values[i]->addr_mode)), 1, ORTE_GPR_ADDR_MODE, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes+=n;
                
                /* pack the segment name */
				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(values[i]->segment)), 1, ORTE_STRING, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
				*num_bytes+=n;

                /* pack the number of tokens so we can read it for unpacking */
				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(values[i]->num_tokens)), 1, ORTE_INT32, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
				*num_bytes+=n;

                /* if there are tokens, pack them */
                if (0 < values[i]->num_tokens) {
    				   n = 0;
                    if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                    (void*)((values[i]->tokens)), values[i]->num_tokens, ORTE_STRING, &n)) {
                        return ORTE_ERROR;
                    }
                    dst = (void*)((char*)dst + n);
    				   *num_bytes+=n;
                }
                
                /* pack the number of keyval pairs so we can read it for unpacking */
				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(values[i]->cnt)), 1, ORTE_INT32, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
				*num_bytes+=n;

                /* pack the keyval pairs */
				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)((values[i]->keyvals)), values[i]->cnt, ORTE_KEYVAL, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
				*num_bytes+=n;
            }
            break;
            
        case ORTE_APP_CONTEXT:
            /* array of pointers to orte_app_context objects - need to pack the objects a set of fields at a time */
            app_context = (orte_app_context_t**) src;
            for (i=0; i < num_vals; i++) {

				n = 0; /* must always start count at zero! */

                /* pack the application index (for multiapp jobs) */
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(app_context[i]->idx)), 1, ORTE_INT32, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
				*num_bytes+=n;

                /* pack the application name */
				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(app_context[i]->app)), 1, ORTE_STRING, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
				*num_bytes+=n;

                /* pack the number of processes */
				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(app_context[i]->num_procs)), 1, ORTE_INT32, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
				*num_bytes+=n;

                /* pack the number of entries in the argv array */
				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(app_context[i]->argc)), 1, ORTE_INT32, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
				*num_bytes+=n;

                /* if there are entries, pack the argv entries */
                if (0 < app_context[i]->argc) {
					n = 0;
                    if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                    (void*)(app_context[i]->argv), app_context[i]->argc, ORTE_STRING, &n)) {
                        return ORTE_ERROR;
                    }
                    dst = (void*)((char*)dst + n);
    				   *num_bytes+=n;
                }
                
                /* pack the number of entries in the enviro array */
				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(app_context[i]->num_env)), 1, ORTE_INT32, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
			    *num_bytes+=n;

                /* if there are entries, pack the enviro entries */
                if (0 < app_context[i]->num_env) {
				    n = 0;
                    if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                    (void*)(app_context[i]->env), app_context[i]->num_env, ORTE_STRING, &n)) {
                        return ORTE_ERROR;
                    }
                    dst = (void*)((char*)dst + n);
    				*num_bytes+=n;
                }
                
                /* pack the cwd */
				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(app_context[i]->cwd)), 1, ORTE_STRING, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes+=n;
                                
                /* Pack the map data */

				n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                        (void*)(&(app_context[i]->num_map)), 1, ORTE_INT32, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes += n;

                if (app_context[i]->num_map > 0) {
						n = 0;
                        if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                             (void*)(app_context[i]->map_data), app_context[i]->num_map, ORTE_APP_CONTEXT_MAP, &n)) {
                            return ORTE_ERROR;
                        }
                        dst = (void*)((char*)dst + n);
                        *num_bytes += n;
               }
            }
            break;

		case ORTE_APP_CONTEXT_MAP:
            app_context_map = (orte_app_context_map_t**) src;
            for (i=0; i < num_vals; i++) {
                n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                        (void*)(&(app_context_map[i]->map_type)), 1, ORTE_UINT8, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes += n;

                n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                        (void*)(&(app_context_map[i]->map_data)), 1, ORTE_STRING, &n)) {
                   return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes += n;
            }
			break;

        case ORTE_GPR_SUBSCRIPTION:
            /* array of pointers to subscription objects - need to pack the objects */
            subs = (orte_gpr_subscription_t**) src;
            for (i=0; i<num_vals; i++) {
                /* pack the address mode */
                n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(subs[i]->addr_mode)), 1, ORTE_GPR_ADDR_MODE, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes+=n;
                
                /* pack the segment name */
                n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(subs[i]->segment)), 1, ORTE_STRING, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes+=n;

                /* pack the number of tokens so we can read it for unpacking */
                n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(subs[i]->num_tokens)), 1, ORTE_INT32, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes+=n;

                /* if there are tokens, pack them */
                if (0 < subs[i]->num_tokens) {
                    n = 0;
                    if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                    (void*)((subs[i]->tokens)), subs[i]->num_tokens, ORTE_STRING, &n)) {
                        return ORTE_ERROR;
                    }
                    dst = (void*)((char*)dst + n);
                    *num_bytes+=n;
                }

                /* pack the number of keys so we can read it for unpacking */
                n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(subs[i]->num_keys)), 1, ORTE_INT32, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes+=n;

                /* if there are keys, pack them */
                if (0 < subs[i]->num_keys) {
                    n = 0;
                    if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                    (void*)((subs[i]->keys)), subs[i]->num_keys, ORTE_STRING, &n)) {
                        return ORTE_ERROR;
                    }
                    dst = (void*)((char*)dst + n);
                    *num_bytes+=n;
                }
                
                /* skip the pointers for cb_func and user_tag */
            }
            break;
            
        case ORTE_GPR_NOTIFY_DATA:
            /* array of pointers to notify data objects - need to pack the objects */
            data = (orte_gpr_notify_data_t**) src;
            for (i=0; i<num_vals; i++) {
                /* pack the callback number */
                n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(data[i]->cb_num)), 1, ORTE_INT32, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes+=n;
                
                /* pack the address mode */
                n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(data[i]->addr_mode)), 1, ORTE_GPR_ADDR_MODE, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes+=n;
                
                /* pack the segment name */
                n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(data[i]->segment)), 1, ORTE_STRING, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes+=n;

                /* pack the number of values so we can read it for unpacking */
                n = 0;
                if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                (void*)(&(data[i]->cnt)), 1, ORTE_INT32, &n)) {
                    return ORTE_ERROR;
                }
                dst = (void*)((char*)dst + n);
                *num_bytes+=n;

                /* if there are values, pack the values */
                if (0 < data[i]->cnt) {
                    n = 0;
                    if (ORTE_SUCCESS != orte_dps_pack_nobuffer(dst,
                                    (void*)((data[i]->values)), data[i]->cnt, ORTE_GPR_VALUE, &n)) {
                        return ORTE_ERROR;
                    }
                    dst = (void*)((char*)dst + n);
                    *num_bytes+=n;
                }
            }
            break;
            
        case ORTE_NULL:
            break;

        default:
            return ORTE_ERR_BAD_PARAM;
    }

    return ORTE_SUCCESS;
}

