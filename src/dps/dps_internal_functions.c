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
/** @file:
 *
 */
#include "ompi_config.h"

#include <stdio.h>
#include <sys/types.h>
#include <math.h>
#include <unistd.h>

#include "util/output.h"

#include "mca/gpr/gpr_types.h"
#include "mca/ns/ns_types.h"
#include "mca/rmgr/rmgr_types.h"

#include "dps_internal.h"

#include <netinet/in.h> 

/**
 * Internal-use only functions
 */
 
/**
 * Calculate the memory storage required for the requested operation
 */
size_t orte_dps_memory_required(void *src, size_t num_vals, orte_data_type_t type)
{
    char **strptr=NULL;
    size_t i=0, mem_req=0;
    orte_byte_object_t *sbyteptr=NULL;
    orte_gpr_keyval_t **keyval;
    orte_gpr_value_t **values;
    orte_app_context_t **app_context;
    orte_app_context_map_t **app_context_map;
    orte_gpr_subscription_t **subs;
    orte_gpr_notify_data_t **data;

    switch(type) {

        case ORTE_DATA_TYPE:
        case ORTE_NODE_STATE:
        case ORTE_PROC_STATE:
        case ORTE_EXIT_CODE:
        case ORTE_BOOL:
        case ORTE_BYTE:
        case ORTE_INT8:
        case ORTE_UINT8:
            return num_vals;
            
        case ORTE_NOTIFY_ACTION:
        case ORTE_GPR_ADDR_MODE:
        case ORTE_GPR_CMD:
        case ORTE_INT16:
        case ORTE_UINT16:
            return (size_t)(num_vals * sizeof(uint16_t));
            
        case ORTE_VPID:
        case ORTE_JOBID:
        case ORTE_CELLID:
        case ORTE_GPR_NOTIFY_ID:
        case ORTE_INT32:
        case ORTE_UINT32:
            return (size_t)(num_vals * sizeof(uint32_t));
            
        case ORTE_INT64:
        case ORTE_UINT64:
            return (size_t)(num_vals * sizeof(uint64_t));
            
        case ORTE_NULL:
            return 0;

        case ORTE_STRING:

            strptr = (char **) src;
            for (i=0; i<num_vals; i++) { 
                /* need to reserve sizeof(uint32_t) for length */
                mem_req += sizeof(uint32_t);
                mem_req += strlen(*strptr);   /* string - null-terminator */
                strptr++;
            }
            return mem_req;
            
        case ORTE_NAME:
            return (size_t)(num_vals * sizeof(orte_process_name_t));
            
        case ORTE_BYTE_OBJECT:
            sbyteptr = (orte_byte_object_t *) src;
            for (i=0; i<num_vals; i++) {
                mem_req += sizeof(uint32_t);  /* length */
                mem_req += sbyteptr->size; /* bytes */
                sbyteptr++;
            }
            return mem_req;

        case ORTE_KEYVAL:
            mem_req = 0;
            keyval = (orte_gpr_keyval_t**) src;
            for (i=0; i < num_vals; i++) {
                mem_req += orte_dps_memory_required(
                                (void*)(&(keyval[i]->key)), 1, ORTE_STRING);
                mem_req += sizeof(orte_data_type_t); /* store data type */
                mem_req += orte_dps_memory_required(
                                (void*)(&(keyval[i]->value)), 1, keyval[i]->type);
            }
            return mem_req;
            
        case ORTE_GPR_VALUE:
            mem_req = 0;
            values = (orte_gpr_value_t**) src;
            for (i=0; i<num_vals; i++) {
                mem_req += orte_dps_memory_required(
                                (void*)(&(values[i]->addr_mode)), 1,
                                ORTE_GPR_ADDR_MODE);
                mem_req += orte_dps_memory_required(
                                (void*)(&(values[i]->segment)), 1, ORTE_STRING);
                mem_req += sizeof(int32_t); /* number of tokens */
                mem_req += orte_dps_memory_required(
                                (void*)(values[i]->tokens), values[i]->num_tokens,
                                ORTE_STRING);
                mem_req += sizeof(int32_t);  /* number of keyvals */
                mem_req += orte_dps_memory_required(
                                (void*)(values[i]->keyvals), values[i]->cnt,
                                ORTE_KEYVAL);
            }
            return mem_req;
            
        case ORTE_APP_CONTEXT:
            mem_req = 0;
            app_context = (orte_app_context_t**) src;
            for (i=0; i < num_vals; i++) {
                mem_req += sizeof(int32_t); /* app index number */
                mem_req += orte_dps_memory_required(
                                (void*)(&(app_context[i]->app)), 1, ORTE_STRING); /* application name */
                mem_req += sizeof(int32_t); /* number or processes */
                mem_req += sizeof(int32_t); /* number of argv entries */
                mem_req += orte_dps_memory_required(
                                (void*)(app_context[i]->argv),
                                app_context[i]->argc, ORTE_STRING); /* length of all argvs */
                mem_req += sizeof(int32_t); /* number of env entries */
                mem_req += orte_dps_memory_required(
                                (void*)(app_context[i]->env),
                                app_context[i]->num_env, ORTE_STRING); /* length of all envs */
                mem_req += orte_dps_memory_required(
                                (void*)(&(app_context[i]->cwd)), 1, ORTE_STRING); /* cwd string */
				mem_req += sizeof(int32_t); /* number of maps */
                mem_req += orte_dps_memory_required(
                                (void*)(app_context[i]->map_data), 
								app_context[i]->num_map, ORTE_APP_CONTEXT_MAP); /* proc map */
            }
            return mem_req;

        case ORTE_APP_CONTEXT_MAP:
            mem_req = 0;
            app_context_map = (orte_app_context_map_t**) src;
            for (i=0; i < num_vals; i++) {
				mem_req += sizeof(uint8_t);	/* map_type */
				mem_req += orte_dps_memory_required(
			                    (void*)(&(app_context_map[i]->map_data)), 1, ORTE_STRING); /* map data */
			}
            return mem_req;

        case ORTE_GPR_SUBSCRIPTION:
            mem_req = 0;
            subs = (orte_gpr_subscription_t**) src;
            for (i=0; i<num_vals; i++) {
                mem_req += orte_dps_memory_required(
                                (void*)(&(subs[i]->addr_mode)), 1,
                                ORTE_GPR_ADDR_MODE);
                mem_req += orte_dps_memory_required(
                                (void*)(&(subs[i]->segment)), 1, ORTE_STRING);
                mem_req += sizeof(int32_t); /* number of tokens */
                mem_req += orte_dps_memory_required(
                                (void*)(subs[i]->tokens), subs[i]->num_tokens,
                                ORTE_STRING);
                mem_req += sizeof(int32_t); /* number of keys */
                mem_req += orte_dps_memory_required(
                                (void*)(subs[i]->keys), subs[i]->num_keys,
                                ORTE_STRING);
                /* don't store the cb_func and user_tag pointers, so
                 * don't reserve memory for them
                 */
            }
            return mem_req;
            
        case ORTE_GPR_NOTIFY_DATA:
            mem_req = 0;
            data = (orte_gpr_notify_data_t**) src;
            for (i=0; i<num_vals; i++) {
                mem_req += sizeof(int32_t); /* callback number */
                mem_req += orte_dps_memory_required(
                                (void*)(&(data[i]->addr_mode)), 1,
                                ORTE_GPR_ADDR_MODE);
                mem_req += orte_dps_memory_required(
                                (void*)(&(data[i]->segment)), 1, ORTE_STRING);
                mem_req += sizeof(int32_t); /* number of values */
                mem_req += orte_dps_memory_required(
                                (void*)(data[i]->values), data[i]->cnt,
                                ORTE_GPR_VALUE);
            }
            return mem_req;

        default:
            return 0;  /* unrecognized type */
    }

}


/**
 * Internal function that resizes (expands) an inuse buffer...adds
 * requested memory in units of memory pages to the current buffer.
 */
int orte_dps_buffer_extend(orte_buffer_t *bptr, size_t mem_req)
{
    /* no buffer checking, we should know what we are doing in here */
    
    size_t newsize; 
    int pages;
    void*  newbaseptr;
    int num_pages;
    float frac_pages;
    ssize_t mdiff;
    size_t  sdiff;          /* difference (increase) in space */
    
    /* how many pages are required */
    frac_pages = (float)mem_req/(float)orte_dps_page_size;
    frac_pages = ceilf(frac_pages);
    num_pages = (int)frac_pages;

    /* push up page count */
    pages = bptr->pages + num_pages;
    
    newsize = (size_t)(pages*orte_dps_page_size);
    
    sdiff = newsize - bptr->size; /* actual increase in space */
    /* have to use relative change as no absolute without */
    /* doing pointer maths for some counts such as space */
    
    newbaseptr = realloc (bptr->base_ptr, newsize);
    
    if (!newbaseptr) { return (ORTE_ERR_OUT_OF_RESOURCE); }
    
    /* ok, we have new memory */
    
    /* update all the pointers in the buffer */
    /* first calc change in memory location */
    mdiff = ((char*)newbaseptr) - ((char*)bptr->base_ptr);
    
    bptr->base_ptr = newbaseptr;
    bptr->data_ptr = ((char*)bptr->data_ptr) + mdiff;
    bptr->from_ptr = ((char*)bptr->from_ptr) + mdiff;
    
    /* now update all pointers & counters */
    bptr->size = newsize;
    bptr->space += sdiff;
    bptr->pages = pages;
    
    return (ORTE_SUCCESS);
}

int orte_dps_dump_buffer_simple (orte_buffer_t *buffer, int outid)
{
    void *src;
    size_t mem_left;
	char* dptr; 
	char* sptr;
    
    src = buffer->from_ptr;
    mem_left = buffer->toend;


	dptr = (char*) buffer->data_ptr;
	sptr = (char*) buffer->base_ptr;

    
    /* output buffer's vitals */
    ompi_output(outid, "Buffer vitals:\n\tbase_ptr: %p\tdata_ptr %p\tfrom_ptr %p\n",
                        buffer->base_ptr, buffer->data_ptr, buffer->from_ptr);
    ompi_output(outid, "\tpages %d\tsize %d\tlen %d\tspace %d\ttoend %d\n\n",
                        buffer->pages, buffer->size, buffer->len,
                        buffer->space, buffer->toend);
	if ((size_t)buffer->len != (size_t)(dptr-sptr)) {
		ompi_output(outid, "data_ptr - base_ptr = %ld length %ld diff %ld\n\n",
						(dptr-sptr), buffer->len, buffer->len - (dptr-sptr));
		return (ORTE_ERROR);
		}
	return (ORTE_SUCCESS);
}

int orte_dps_dump_buffer(orte_buffer_t *buffer, int outid)
{
    void *src;
    uint32_t *s32, *d32;
    char *sstr;
    uint8_t *sptr;
    size_t num, nbytes, mem_left, i, len;
    orte_data_type_t type;
    
    src = buffer->from_ptr;
    mem_left = buffer->toend;
    
    /* output buffer's vitals */
    ompi_output(outid, "Buffer vitals:\n\tbase_ptr: %p\tdata_ptr %p\tfrom_ptr %p\n",
                        buffer->base_ptr, buffer->data_ptr, buffer->from_ptr);
    ompi_output(outid, "\tpages %d\tsize %d\tlen %d\tspace %d\ttoend %d\n\n",
                        buffer->pages, buffer->size, buffer->len,
                        buffer->space, buffer->toend);
	if (buffer->len != ((char*)buffer->data_ptr - (char*)buffer->base_ptr)) {
		ompi_output(outid, "data_ptr - base_ptr = %ld length %ld diff %ld\n\n",
						((char*)buffer->data_ptr - (char*)buffer->base_ptr), buffer->len,
                          buffer->len - ((char*)buffer->data_ptr - (char*)buffer->base_ptr));
	}

    while (0 < mem_left) {
        /* got enough for type? */
        if (sizeof(uint32_t) > mem_left) {
            ompi_output(outid, "Not enough memory for type");
            return ORTE_ERR_UNPACK_FAILURE;
        }
        
        s32 = (uint32_t *) src;
        type = (orte_data_type_t)ntohl(*s32);
        s32++;
        src = (void *)s32;
        mem_left -= sizeof(uint32_t);
        
        /* got enough left for num_vals? */
        if (sizeof(uint32_t) > mem_left) { /* not enough memory  */
            ompi_output(outid, "Not enough memory for number of values");
            return ORTE_ERR_UNPACK_FAILURE;
        }
    
        /* unpack the number of values */
        s32 = (uint32_t *) src;
        num = (size_t)ntohl(*s32);
        s32++;
        src = (void *)s32;
        mem_left -= sizeof(uint32_t);

        ompi_output(outid, "Item: type %d number %d", (int)type, (int)num);

        switch(type) {
           
            case ORTE_BYTE:
            case ORTE_INT8:
            case ORTE_UINT8:
                mem_left -= num*sizeof(uint8_t);
                src = ((char*) src) + num * sizeof(uint8_t);
                break;
                
            case ORTE_INT16:
            case ORTE_UINT16:
                mem_left -= num * sizeof(uint16_t);
                src = ((char*) src) + num * sizeof(uint16_t);
                break;
                
            case ORTE_INT32:
            case ORTE_UINT32:
                mem_left -= num * sizeof(uint32_t);
                src = ((char*) src) + num * sizeof(uint32_t);
                break;
            
            case ORTE_INT64:
            case ORTE_UINT64:
            case ORTE_FLOAT:
            case ORTE_FLOAT4:
            case ORTE_FLOAT8:
            case ORTE_FLOAT12:
            case ORTE_FLOAT16:
            case ORTE_DOUBLE:
            case ORTE_LONG_DOUBLE:
                ompi_output(outid, "Attempt to unpack unimplemented type");
                return ORTE_ERR_PACK_FAILURE;
                break;
    
            case ORTE_BOOL:
                mem_left -= num * sizeof(uint8_t);
                src = ((char*) src) + num * sizeof(uint8_t);
                break;
    
            case ORTE_NAME:
                mem_left -= num * sizeof(orte_process_name_t);
                src = ((char*) src) + num * sizeof(orte_process_name_t);
                break;

        case ORTE_STRING:
            sstr = (char *) src;
            for(i=0; i<num; i++) {
                if(mem_left < sizeof(uint32_t)) {
                    ompi_output(outid, "Attempt to read past end of buffer");
                    return ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
                }
                d32 = (uint32_t*)sstr;
                len = ntohl(*d32);
                d32++;
                sstr= (char*)d32;
                mem_left -= sizeof(uint32_t);
                if(mem_left < len) {
                    ompi_output(outid, "Attempt to read past end of buffer");
                    return ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
                }
                src = sstr = (char*)(sstr + len);
                mem_left -= len;
            }
            break;

        case ORTE_BYTE_OBJECT:
 
            for(i=0; i<num; i++) {
                if(mem_left < sizeof(uint32_t)) {
                    ompi_output(outid, "Attempt to read past end of buffer");
                    return ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
                }
                d32 = (uint32_t*)src;
                nbytes = (size_t)ntohl(*d32);
                d32++;
                sptr = (void*)d32;
                mem_left -= sizeof(uint32_t);
                if(mem_left < nbytes) {
                    ompi_output(outid, "Attempt to read past end of buffer");
                    return ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
                }
                src = sptr = (void*)((uint8_t*)sptr + nbytes);
                mem_left -= nbytes;
            }
            break;

        case ORTE_NULL:
            break;

        default:
            ompi_output(outid, "Attempt to unpack unknown type");
            return ORTE_ERROR;
        }
        
        /* output buffer's vitals */
        ompi_output(outid, "Buffer vitals:\n\tbase_ptr: %p\tdata_ptr %p\tfrom_ptr %p\n",
                            buffer->base_ptr, buffer->data_ptr, buffer->from_ptr);
        ompi_output(outid, "\tpages %d\tsize %d\tlen %d\tspace %d\ttoend %d\n\n",
                            buffer->pages, buffer->size, buffer->len,
                            buffer->space, buffer->toend);
	    if (buffer->len != ((char*)buffer->data_ptr - (char*)buffer->base_ptr)) {
	   		 ompi_output(outid, "data_ptr - base_ptr = %ld length %ld diff %ld\n\n",
						((char*)buffer->data_ptr - (char*)buffer->base_ptr), buffer->len,
                          buffer->len - ((char*)buffer->data_ptr - (char*)buffer->base_ptr));
		} 

    }
    return ORTE_SUCCESS;
}
