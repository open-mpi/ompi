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
#include "mca/gpr/gpr_types.h"
#include "mca/ns/ns_types.h"
#include "mca/rmgr/rmgr_types.h"
#include "dps_internal.h"

/**
 * DPS UNPACK VALUE
 */

int orte_dps_unpack(orte_buffer_t *buffer, void *dst,
                    size_t *max_num_vals,
                    orte_data_type_t type)
{
    int rc=ORTE_SUCCESS;
    size_t num_vals;
    size_t mem_left;
    size_t num_bytes, hdr_bytes;
    void *src;
    uint32_t * s32;
    orte_data_type_t stored_type;


    /* check for errors */
    if (buffer == NULL || dst == NULL || max_num_vals == NULL) { 
        return (ORTE_ERR_BAD_PARAM); 
    }

	num_bytes = 0; /* have not unpacked any yet */
	hdr_bytes = 0; 

    src = buffer->from_ptr;  /* get location in buffer */
    mem_left = buffer->toend;  /* how much data is left in buffer */

    /* check to see if there is enough in the buffer to hold the pack type */
    if (mem_left < sizeof(orte_data_type_t)) {
        return ORTE_ERR_UNPACK_FAILURE;
    }

    /* first thing in the current buffer space must be the type */
    if (ORTE_SUCCESS != (rc =orte_dps_unpack_nobuffer(&stored_type, src, 1,
                                    ORTE_DATA_TYPE, &mem_left, &hdr_bytes))) {
        return rc;
    }
    src = (void*)((char*)src + hdr_bytes);
    
    if(type == ORTE_INT || type == ORTE_UINT) {
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

    /* check for type match - for now we require this to be an exact match -
     * though we should probably support conversions when there is no loss
     * of precision.
     */
    if (stored_type != type) {
        return ORTE_PACK_MISMATCH;
    }
    
    /* got enough left for num_vals? */
    if (sizeof(uint32_t) > mem_left) { /* not enough memory  */
        return ORTE_ERR_UNPACK_FAILURE;
    }

    /* unpack the number of values */
    s32 = (uint32_t *) src;
    num_vals = (size_t)ntohl(*s32);
    if (num_vals > *max_num_vals) {  /* not enough space provided */
        return ORTE_UNPACK_INADEQUATE_SPACE;
    }
    s32++;
    src = (void *)s32;
    mem_left -= sizeof(uint32_t);	/* we do this here but this is normally a function of unpack_nobuffer */
	hdr_bytes += sizeof(uint32_t);

    /* will check to see if adequate storage in buffer prior
     * to unpacking the item
     */
    if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(dst, src, num_vals,
                                    stored_type, &mem_left, &num_bytes))) {
        return rc;
    }
/* fflush(stdout); fflush(stderr); */
/* fprintf(stderr,"unpacked total bytes %d, (hdr %d datatype %d)\n", num_bytes+hdr_bytes, hdr_bytes, num_bytes); */
/* fflush(stdout); fflush(stderr); */
    
    /* ok, we managed to unpack some stuff, so update all ptrs/cnts */
    buffer->from_ptr = (void*)((char*)src + num_bytes); /* move by data type size only as src is after the header */
    buffer->toend = mem_left; /* closer to the end */
    buffer->len   -= (num_bytes+hdr_bytes); /* and less data left */

    /* return the number of values unpacked */
    *max_num_vals = num_vals;
    return rc;
}


int orte_dps_unpack_nobuffer(void *dst, void *src, size_t num_vals,
                             orte_data_type_t type,
                             size_t *mem_left, size_t *num_bytes)
{
    int rc;
    size_t i;
    size_t n;
    uint16_t * d16;
    uint32_t * d32;
    uint16_t * s16;
    uint32_t * s32;
    uint8_t* bool_src;
    bool *bool_dst;
    char **dstr;
    orte_process_name_t* dn;
    orte_process_name_t* sn;
    orte_byte_object_t* dbyteptr;
    orte_gpr_keyval_t **keyval;
    orte_gpr_value_t **values;
	orte_app_context_t **app_context;
	orte_app_context_map_t **app_context_map;
    uint32_t len;
    char *str, *sstr;
    void *sptr;
    orte_gpr_subscription_t **subs;
    orte_gpr_notify_data_t **data;

    /* defaults */
    rc = ORTE_SUCCESS;
    *num_bytes = 0;
    
    switch(type) {
       
        case ORTE_DATA_TYPE:
        case ORTE_NODE_STATE:
        case ORTE_PROC_STATE:
        case ORTE_EXIT_CODE:
        case ORTE_BYTE:
        case ORTE_INT8:
        case ORTE_UINT8:

            if (num_vals > *mem_left) {
                num_vals = *mem_left;
                rc = ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
            }
            memcpy(dst, src, num_vals);
            *num_bytes = num_vals * sizeof(uint8_t);
            break;
            
        case ORTE_NOTIFY_ACTION:
        case ORTE_GPR_ADDR_MODE:
        case ORTE_GPR_CMD:
        case ORTE_INT16:
        case ORTE_UINT16:
       
            if(num_vals * sizeof(uint16_t) > *mem_left) {
                num_vals = *mem_left / sizeof(uint16_t);
                rc = ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
            }
            s16 = (uint16_t *) src;
            d16 = (uint16_t *) dst;
            for(i=0; i<num_vals; i++) {
                /* convert the network order to host order */
                *d16 = ntohs(*s16);
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

            if(num_vals * sizeof(uint32_t) > *mem_left) {
                num_vals = *mem_left / sizeof(uint32_t);
                rc = ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
            }
            s32 = (uint32_t *) src;
            d32 = (uint32_t *) dst;
            for(i=0; i<num_vals; i++) {
                /* convert the network order to host order */
                *d32 = ntohl(*s32);
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

            if(num_vals * sizeof(uint8_t) > *mem_left) {
                num_vals = *mem_left / sizeof(uint8_t);
                rc = ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
            }
            bool_src = (uint8_t *) src;
            bool_dst = (bool *) dst;
            for(i=0; i<num_vals; i++) {
                /* convert packed uint8_t to native bool */
                *bool_dst = (*bool_src) ? true : false;
                 bool_dst++; bool_src++;
            }
            *num_bytes = num_vals * sizeof(uint8_t);
            break;

        case ORTE_STRING:
 
            dstr = (char**)dst;
            sstr = (char *) src;
            for(i=0; i<num_vals; i++) {
                if(*mem_left < sizeof(uint32_t)) {
                    return ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
                }
                d32 = (uint32_t*)sstr;
                len = ntohl(*d32);
                d32++;
                sstr= (char*)d32;
                *num_bytes += sizeof(uint32_t);
                *mem_left -= sizeof(uint32_t);
                if(*mem_left < len) {
                    return ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
                }
                if(NULL == (str = malloc(len+1)))
                    return ORTE_ERR_OUT_OF_RESOURCE;
                memcpy(str,sstr,len);
                str[len] = '\0';
                dstr[i] = str;
                sstr = (char*)(sstr + len);
                *mem_left -= len;
                *num_bytes += len;
            }
            return ORTE_SUCCESS;
            break;

        case ORTE_NAME:

            dn = (orte_process_name_t*) dst;
            sn = (orte_process_name_t*) src;
            for (i=0; i<num_vals; i++) {
                dn->cellid = ntohl(sn->cellid);
                dn->jobid = ntohl(sn->jobid);
                dn->vpid = ntohl(sn->vpid);
                dn++; sn++;
            }
            *num_bytes = num_vals * sizeof(orte_process_name_t);
            break;
        
        case ORTE_BYTE_OBJECT:
 
            dbyteptr = (orte_byte_object_t*)dst;
            sptr = src; /* iterate from start of buffer */
            for(i=0; i<num_vals; i++) {
                if(*mem_left < sizeof(uint32_t)) {
                    return ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
                }
                d32 = (uint32_t*)sptr;
                dbyteptr->size = (size_t)ntohl(*d32);
                d32++;
                sptr = (void*)d32;
                *mem_left -= sizeof(uint32_t);
                *num_bytes += sizeof(uint32_t);
                if(*mem_left < dbyteptr->size) {
                    return ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
                }
                if(NULL == (dbyteptr->bytes = malloc(dbyteptr->size)))
                    return ORTE_ERR_OUT_OF_RESOURCE;
                memcpy(dbyteptr->bytes,sptr,dbyteptr->size);
                sptr = (void*)((uint8_t*)sptr + dbyteptr->size);
                *mem_left -= dbyteptr->size;
                *num_bytes += dbyteptr->size;
                dbyteptr++;
            }
            return ORTE_SUCCESS;
            break;

        case ORTE_KEYVAL:
        
            /* unpack into an array of keyval objects */
            keyval = (orte_gpr_keyval_t**) dst;
			/* use temp count of unpacked 'n' which we sum to produce correct value later */
            for (i=0; i < num_vals; i++) {
                keyval[i] = OBJ_NEW(orte_gpr_keyval_t);
                if (NULL == keyval[i]) {
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(keyval[i]->key),
                            src, 1, ORTE_STRING, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
				*num_bytes+=n;

				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(keyval[i]->type),
                            src, 1, ORTE_DATA_TYPE, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
				*num_bytes+=n;

				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(keyval[i]->value),
                            src, 1, keyval[i]->type, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
				*num_bytes+=n;
				
            }
			/* must return here for composite unpacks that change mem_left directly */
            return ORTE_SUCCESS;
            break;
        
        case ORTE_GPR_VALUE:
        
            /* unpack into array of value objects */
            values = (orte_gpr_value_t**) dst;
            for (i=0; i < num_vals; i++) {
                /* create the value object */
                values[i] = OBJ_NEW(orte_gpr_value_t);
                if (NULL == values[i]) {
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }

                /* unpack the address mode */
                n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(values[i]->addr_mode),
                            src, 1, ORTE_GPR_ADDR_MODE, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;
                
                /* unpack the segment name */
				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(values[i]->segment),
                            src, 1, ORTE_STRING, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
				*num_bytes+=n;

                /* get the number of tokens */
				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(values[i]->num_tokens),
                            src, 1, ORTE_INT32, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
				*num_bytes+=n;

                /* if there are tokens, allocate the required space for the char * pointers */
                if (0 < values[i]->num_tokens) {
                    values[i]->tokens = (char **)malloc(values[i]->num_tokens * sizeof(char*));
                    if (NULL == values[i]->tokens) {
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
    
                    /* and unpack them */
    				n = 0;
                    if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(values[i]->tokens,
                                src, values[i]->num_tokens, ORTE_STRING, mem_left, &n))) {
                        return rc;
                    }
                    src = (void*)((char*)src + n);
    				*num_bytes+=n;
                }
                
                /* get the number of keyval pairs */
				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(values[i]->cnt),
                            src, 1, ORTE_INT32, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
				*num_bytes+=n;

                /* allocate the required space for the keyval object pointers */
                if(values[i]->cnt) {
                    values[i]->keyvals = (orte_gpr_keyval_t**)malloc(values[i]->cnt * sizeof(orte_gpr_keyval_t*));
                    if (NULL == values[i]->keyvals) {
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }

                    /* unpack the keyval pairs */
				   n = 0;
                    if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(values[i]->keyvals,
                            src, values[i]->cnt, ORTE_KEYVAL, mem_left, &n))) {
                        return rc;
                    }
                    src = (void*)((char*)src + n);
				   *num_bytes+=n;
                } 
            }
			/* must return here for composite unpacks that change mem_left directly */
            return ORTE_SUCCESS;
            break;
            
        case ORTE_APP_CONTEXT:
            
            /* unpack into array of app_context objects */
            app_context = (orte_app_context_t**) dst;
            for (i=0; i < num_vals; i++) {

                /* create the app_context object */
                app_context[i] = OBJ_NEW(orte_app_context_t);
                if (NULL == app_context[i]) {
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }

                /* get the app index number */
				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(app_context[i]->idx),
                            src, 1, ORTE_INT32, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
				*num_bytes+=n;

                /* unpack the application name */
				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(app_context[i]->app),
                            src, 1, ORTE_STRING, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
				*num_bytes+=n;

                /* get the number of processes */
				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(app_context[i]->num_procs),
                            src, 1, ORTE_INT32, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
				*num_bytes+=n;

                /* get the number of argv strings */
				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(app_context[i]->argc),
                            src, 1, ORTE_INT32, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
				*num_bytes+=n;

                /* if there are argv strings, allocate the required space for the char * pointers */
                if (0 < app_context[i]->argc) {
                    app_context[i]->argv = (char **)malloc((app_context[i]->argc+1) * sizeof(char*));
                    if (NULL == app_context[i]->argv) {
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                    app_context[i]->argv[app_context[i]->argc] = NULL;
    
                    /* and unpack them */
					n = 0;
                    if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(app_context[i]->argv,
                                src, app_context[i]->argc, ORTE_STRING, mem_left, &n))) {
                        return rc;
                    }
                    src = (void*)((char*)src + n);
    				*num_bytes+=n;
                }
                
                /* get the number of env strings */
				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(app_context[i]->num_env),
                            src, 1, ORTE_INT32, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
				*num_bytes+=n;

                /* if there are env strings, allocate the required space for the char * pointers */
                if (0 < app_context[i]->num_env) {
                    app_context[i]->env = (char **)malloc((app_context[i]->num_env+1) * sizeof(char*));
                    if (NULL == app_context[i]->env) {
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                    app_context[i]->env[app_context[i]->num_env] = NULL;
            
                    /* and unpack them */
					n = 0;
                    if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(app_context[i]->env,
                                src, app_context[i]->num_env, ORTE_STRING, mem_left, &n))) {
                        return rc;
                    }
                    src = (void*)((char*)src + n);
    				   *num_bytes+=n;
                }
                
                /* unpack the cwd */
				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&app_context[i]->cwd,
                            src, 1, ORTE_STRING, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;

                /* unpack the map data */

				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(app_context[i]->num_map),
                           src, 1, ORTE_INT32, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;

                if (app_context[i]->num_map > 0) {
                    app_context[i]->map_data = malloc(sizeof(orte_app_context_map_t*) * app_context[i]->num_map);
                    if (NULL == app_context[i]->map_data) {
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
					n = 0;
                    if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(app_context[i]->map_data,
                                src, app_context[i]->num_map, ORTE_APP_CONTEXT_MAP, mem_left, &n))) {
                            return rc;
                    }
                    src = (void*)((char*)src + n);
                    *num_bytes += n;

                }

            }
			/* must return here for composite unpacks that change mem_left directly */
            return ORTE_SUCCESS;
            break;

        case ORTE_APP_CONTEXT_MAP:
            
            /* unpack into array of app_context_map objects */
            app_context_map = (orte_app_context_map_t**) dst;
            for (i=0; i < num_vals; i++) {

                /* create the app_context object */
                app_context_map[i] = OBJ_NEW(orte_app_context_map_t);
                if (NULL == app_context_map[i]) {
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }

				/* map type */
				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(app_context_map[i]->map_type),
                           src, 1, ORTE_UINT8, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;

				/* map data */
				n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(app_context_map[i]->map_data),
                           src, 1, ORTE_STRING, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;
            }
			/* must return here for composite unpacks that change mem_left directly */
            return ORTE_SUCCESS;
            break;

        case ORTE_GPR_SUBSCRIPTION:
        
            /* unpack into array of subscription objects */
            subs = (orte_gpr_subscription_t**) dst;
            for (i=0; i < num_vals; i++) {
                /* create the subscription object */
                subs[i] = OBJ_NEW(orte_gpr_subscription_t);
                if (NULL == subs[i]) {
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }

                /* unpack the address mode */
                n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(subs[i]->addr_mode),
                            src, 1, ORTE_GPR_ADDR_MODE, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;
                
                /* unpack the segment name */
                n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(subs[i]->segment),
                            src, 1, ORTE_STRING, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;

                /* get the number of tokens */
                n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(subs[i]->num_tokens),
                            src, 1, ORTE_INT32, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;

                /* if there are tokens, allocate the required space for the char * pointers */
                if (0 < subs[i]->num_tokens) {
                    subs[i]->tokens = (char **)malloc(subs[i]->num_tokens * sizeof(char*));
                    if (NULL == subs[i]->tokens) {
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
    
                    /* and unpack them */
                    n = 0;
                    if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(subs[i]->tokens,
                                src, subs[i]->num_tokens, ORTE_STRING, mem_left, &n))) {
                        return rc;
                    }
                    src = (void*)((char*)src + n);
                    *num_bytes+=n;
                }
                
                /* get the number of keys */
                n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(subs[i]->num_keys),
                            src, 1, ORTE_INT32, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;

                /* if there are keys, allocate the required space for the char * pointers */
                if (0 < subs[i]->num_keys) {
                    subs[i]->keys = (char **)malloc(subs[i]->num_keys * sizeof(char*));
                    if (NULL == subs[i]->keys) {
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
    
                    /* and unpack them */
                    n = 0;
                    if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(subs[i]->keys,
                                src, subs[i]->num_keys, ORTE_STRING, mem_left, &n))) {
                        return rc;
                    }
                    src = (void*)((char*)src + n);
                    *num_bytes+=n;
                }
                
                /* the pointer fields for cb_func and user_tag were NOT packed
                 * so ignore them here as well
                 */
            }
            /* must return here for composite unpacks that change mem_left directly */
            return ORTE_SUCCESS;
            break;
            
        case ORTE_GPR_NOTIFY_DATA:
        
            /* unpack into array of notify_data objects */
            data = (orte_gpr_notify_data_t**) dst;
            for (i=0; i < num_vals; i++) {
                /* create the data object */
                data[i] = OBJ_NEW(orte_gpr_notify_data_t);
                if (NULL == data[i]) {
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }

                /* unpack the callback number */
                n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(data[i]->cb_num),
                            src, 1, ORTE_INT32, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;
                
                /* unpack the address mode */
                n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(data[i]->addr_mode),
                            src, 1, ORTE_GPR_ADDR_MODE, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;
                
                /* unpack the segment name */
                n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(data[i]->segment),
                            src, 1, ORTE_STRING, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;

                /* get the number of values */
                n = 0;
                if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(&(data[i]->cnt),
                            src, 1, ORTE_INT32, mem_left, &n))) {
                    return rc;
                }
                src = (void*)((char*)src + n);
                *num_bytes+=n;

                /* if there are values, allocate the required space for the value pointers */
                if (0 < data[i]->cnt) {
                    data[i]->values = (orte_gpr_value_t**)malloc(data[i]->cnt * sizeof(orte_gpr_value_t*));
                    if (NULL == data[i]->values) {
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
    
                    /* and unpack them */
                    n = 0;
                    if (ORTE_SUCCESS != (rc = orte_dps_unpack_nobuffer(data[i]->values,
                                src, data[i]->cnt, ORTE_GPR_VALUE, mem_left, &n))) {
                        return rc;
                    }
                    src = (void*)((char*)src + n);
                    *num_bytes+=n;
                }
            }
            /* must return here for composite unpacks that change mem_left directly */
            return ORTE_SUCCESS;
            break;
            
        case ORTE_NULL:
            break;

        default:
            return ORTE_ERROR;
    }
    
    *mem_left -= *num_bytes;
    return ORTE_SUCCESS;
}
