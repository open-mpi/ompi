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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "include/orte_constants.h"
#include "include/orte_types.h"

#include "dps/dps.h"
#include "util/output.h"

#include "mca/ns/ns_types.h"
#include "mca/soh/soh_types.h"

#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"
#include "gpr_replica_fn.h"

static void orte_gpr_replica_dump_load_string(orte_buffer_t *buffer, char **tmp);

void orte_gpr_replica_dump_itagval_value(orte_buffer_t *buffer,
                                         orte_gpr_replica_itagval_t *iptr);

static void orte_gpr_replica_dump_trigger(orte_buffer_t *buffer, int cnt,
                                          orte_gpr_replica_triggers_t *trig);


int orte_gpr_replica_dump_all_fn(orte_buffer_t *buffer)
{
    char *tmp_out;
    int rc;
    
    asprintf(&tmp_out, "\n\n\nDUMP OF GENERAL PURPOSE REGISTRY");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_triggers_fn(buffer))) {
        return rc;
    }
    
    rc = orte_gpr_replica_dump_segments_fn(buffer);
    
    return rc;
}

int orte_gpr_replica_dump_segments_fn(orte_buffer_t *buffer)
{
    orte_gpr_replica_segment_t **seg;
    orte_gpr_replica_container_t **cptr;
    orte_gpr_replica_itag_t *itaglist;
    orte_gpr_replica_itagval_t **iptr;
    char *token;
    int num_objects;
    int i, j, k;
    char *tmp_out;

    asprintf(&tmp_out, "\nDUMP OF GPR SEGMENTS");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    /* loop through all segments */
    seg = (orte_gpr_replica_segment_t**)(orte_gpr_replica.segments)->addr;
    for (i=0; i < (orte_gpr_replica.segments)->size; i++) {
         if (NULL != seg[i]) {

            	asprintf(&tmp_out, "\nGPR Dump for Segment: %s", seg[i]->name);
            	orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            
            	num_objects = (seg[i]->containers)->size - (seg[i]->containers)->number_free;
            
            	asprintf(&tmp_out, "\tNumber of containers: %d\n", num_objects);
            	orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            
            	/* loop through all containers and print their info and contents */
             cptr = (orte_gpr_replica_container_t**)(seg[i]->containers)->addr;
            	for (j=0; j < (seg[i]->containers)->size; j++) {
                if (NULL != cptr[j]) {
                	    asprintf(&tmp_out, "\n\tInfo for container %d\tNumber of keyvals: %d\n\tTokens:\n",
                                 j, (cptr[j]->itagvals)->size - (cptr[j]->itagvals)->number_free);
                	    orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                
                	    /* reverse lookup tokens and print them */
                     itaglist = cptr[j]->itags;
                	    for (k=0; k < cptr[j]->num_itags; k++) {
                		     if (ORTE_SUCCESS != orte_gpr_replica_dict_reverse_lookup(
                                                            &token, seg[i], itaglist[k])) {
                		          asprintf(&tmp_out, "\t\titag num %d: No entry found for itag %X",
                			             k, itaglist[k]);
                		     } else {
                		          asprintf(&tmp_out, "\t\titag num %d: itag %d\tToken: %s",
                			             k, itaglist[k], token);
                		          free(token);
                		     }
                		     orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                	    }
                     
                     asprintf(&tmp_out, "\n\tKeyval info:");
                     orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                
                     /* loop through all itagvals and print their info */
                     iptr = (orte_gpr_replica_itagval_t**)(cptr[j]->itagvals)->addr;
                     for (k=0; k < (cptr[j]->itagvals)->size; k++) {
                          if (NULL != iptr[k]) {
                              if (ORTE_SUCCESS != orte_gpr_replica_dict_reverse_lookup(
                                                        &token, seg[i], iptr[k]->itag)) {
                                   asprintf(&tmp_out, "\n\t\titag num %d: No entry found for itag %X",
                                       k, iptr[k]->itag);
                              } else {
                                   asprintf(&tmp_out, "\n\t\tEntry %d: itag %d\tKey: %s",
                                        k, iptr[k]->itag, token);
                                   free(token);
                              }
                              orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                              orte_gpr_replica_dump_itagval_value(buffer, iptr[k]);
                          }
                     }
                }
            	}
         }
    }
    
    return ORTE_SUCCESS;
}

int orte_gpr_replica_dump_triggers_fn(orte_buffer_t *buffer)
{
    orte_gpr_replica_triggers_t **trig;
    ompi_list_item_t *item;
    orte_gpr_replica_callbacks_t *cb;
    char *tmp_out;
    int i, j, k;
    
    asprintf(&tmp_out, "\nDUMP OF GPR TRIGGERS\n");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    if (0 < ompi_list_get_size(&(orte_gpr_replica.callbacks))) {
        asprintf(&tmp_out, "Registered Callbacks");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        
        for (i=0, item=ompi_list_get_first(&(orte_gpr_replica.callbacks));
             item != ompi_list_get_end(&(orte_gpr_replica.callbacks));
             i++, item=ompi_list_get_next(item)) {
             cb = (orte_gpr_replica_callbacks_t*)item;
             asprintf(&tmp_out, "\tInfo for callback %d", i);
             orte_gpr_replica_dump_load_string(buffer, &tmp_out);
             if (NULL == cb->requestor) {
                asprintf(&tmp_out, "\t\tLocal requestor - local notify idtag %d", (cb->message)->idtag);
             } else {
                asprintf(&tmp_out, "\t\tRequestor: [%d,%d,%d] - remote notify idtag %d",
                        ORTE_NAME_ARGS(cb->requestor), cb->remote_idtag);
             }
             orte_gpr_replica_dump_load_string(buffer, &tmp_out);
             asprintf(&tmp_out, "\t\tNum values: %d", (cb->message)->cnt);
             orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        }
        asprintf(&tmp_out, "\n");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }

    trig = (orte_gpr_replica_triggers_t**)((orte_gpr_replica.triggers)->addr);
    k = 0;
    for (j=0; j < (orte_gpr_replica.triggers)->size; j++) {
        if (NULL != trig[j]) k++;
    }
    
    asprintf(&tmp_out, "Number of triggers: %d\n", k);
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    
    /* dump the trigger info for the registry */
    for (j=0, k=0; j < (orte_gpr_replica.triggers)->size; j++) {
        if (NULL != trig[j]) {
            orte_gpr_replica_dump_trigger(buffer, k, trig[j]);
            k++;
        }
    }

    return ORTE_SUCCESS;
}    
    
static void orte_gpr_replica_dump_trigger(orte_buffer_t *buffer, int cnt,
                                          orte_gpr_replica_triggers_t *trig)
{
    char *tmp_out, *token;
    int i, j, k;
    orte_gpr_replica_subscribed_data_t **data;
    orte_gpr_replica_counter_t **cntr;
    
	asprintf(&tmp_out, "\nData for trigger %d", cnt);
	orte_gpr_replica_dump_load_string(buffer, &tmp_out);

	/* output recipient info */
		if (NULL == trig->requestor) {
		    asprintf(&tmp_out, "\tIntended recipient: LOCAL @ notifier idtag %d",
                    trig->index);
    	} else {
	    asprintf(&tmp_out, "\tIntended recipient: [%d,%d,%d] @ notifier idtag %d",
            ORTE_NAME_ARGS(trig->requestor), trig->remote_idtag);
	}
 
	orte_gpr_replica_dump_load_string(buffer, &tmp_out);
	tmp_out = strdup("\tActions:");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);
	if (ORTE_GPR_NOTIFY_VALUE_CHG & trig->action) {
        tmp_out = strdup("\t\tORTE_GPR_NOTIFY_VALUE_CHG");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
	} else if (ORTE_GPR_NOTIFY_VALUE_CHG_TO & trig->action) {
        tmp_out = strdup("\t\tORTE_GPR_NOTIFY_VALUE_CHG_TO");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
	} else if (ORTE_GPR_NOTIFY_VALUE_CHG_FRM & trig->action) {
        tmp_out = strdup("\t\tORTE_GPR_NOTIFY_VALUE_CHG_FRM");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
	if (ORTE_GPR_NOTIFY_DEL_ENTRY & trig->action) {
	    tmp_out = strdup("\t\tORTE_GPR_NOTIFY_DEL_ENTRY");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
	}
	if (ORTE_GPR_NOTIFY_ADD_ENTRY & trig->action) {
	    tmp_out = strdup("\t\tORTE_GPR_NOTIFY_ADD_ENTRY");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
	}
	if (ORTE_GPR_NOTIFY_PRE_EXISTING & trig->action) {
	    tmp_out = strdup("\t\tORTE_GPR_NOTIFY_PRE_EXISTING");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
	}
	if (ORTE_GPR_TRIG_ONE_SHOT & trig->action) {
	    tmp_out = strdup("\t\tORTE_GPR_TRIG_ONE_SHOT");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
	}
    if (ORTE_GPR_TRIG_CMP_LEVELS & trig->action) {
        tmp_out = strdup("\t\tORTE_GPR_TRIG_CMP_LEVELS");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TRIG_MONITOR_ONLY & trig->action) {
        tmp_out = strdup("\t\tORTE_GPR_TRIG_MONITOR_ONLY");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TRIG_NOTIFY_START & trig->action) {
        tmp_out = strdup("\t\tORTE_GPR_TRIG_NOTIFY_START");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TRIG_INCLUDE_DATA & trig->action) {
        tmp_out = strdup("\t\tORTE_GPR_TRIG_INCLUDE_DATA");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }

	asprintf(&tmp_out, "\tData covered by this subscription");
	orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    data = (orte_gpr_replica_subscribed_data_t**)((trig->subscribed_data)->addr);
    for (i=0; i < (trig->subscribed_data)->size; i++) {
        if (NULL != data[i]) {
            asprintf(&tmp_out, "\t\tData on segment %s", (data[i]->seg)->name);
            orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            
            k = (int)orte_value_array_get_size(&(data[i]->tokentags));
            if (0 == k) {
                asprintf(&tmp_out, "\t\tNULL token (wildcard)");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            } else {
                asprintf(&tmp_out, "\t\tNumber of tokens: %d", k);
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            
                for (j=0; j < k; j++) {
                    if (ORTE_SUCCESS == orte_gpr_replica_dict_reverse_lookup(&token, data[i]->seg,
                            ORTE_VALUE_ARRAY_GET_ITEM(&(data[i]->tokentags), orte_gpr_replica_itag_t, j))) {
                        asprintf(&tmp_out, "\t\t\tToken: %s", token);
            		       orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                        free(token);
                    }
            	   }
            }
    
            asprintf(&tmp_out, "\t\tToken addressing mode:\n");
            orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        
            if (ORTE_GPR_TOKENS_NOT & data[i]->addr_mode) {
                asprintf(&tmp_out, "\t\t\tORTE_GPR_TOKENS_NOT\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_TOKENS_AND & data[i]->addr_mode) {
                asprintf(&tmp_out, "\t\t\tORTE_GPR_TOKENS_AND\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_TOKENS_OR & data[i]->addr_mode) {
                asprintf(&tmp_out, "\t\t\tORTE_GPR_TOKENS_OR\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_TOKENS_XAND & data[i]->addr_mode) {
                asprintf(&tmp_out, "\t\t\tORTE_GPR_TOKENS_XAND\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_TOKENS_XOR & data[i]->addr_mode) {
                asprintf(&tmp_out, "\t\t\tORTE_GPR_TOKENS_XOR\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            
            k = (int)orte_value_array_get_size(&(data[i]->keytags));
            if (0 == k) {
                asprintf(&tmp_out, "\t\tNULL key (wildcard)");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            } else {
                asprintf(&tmp_out, "\t\tNumber of keys: %d", k);
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        
                for (j=0; j < k; j++) {
                    if (ORTE_SUCCESS == orte_gpr_replica_dict_reverse_lookup(&token, data[i]->seg,
                            ORTE_VALUE_ARRAY_GET_ITEM(&(data[i]->keytags), orte_gpr_replica_itag_t, j))) {
                        asprintf(&tmp_out, "\t\t\tKey: %s", token);
                        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                        free(token);
                    }
                }
            }
            
            asprintf(&tmp_out, "\t\tKey addressing mode:\n");
            orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        
            if (ORTE_GPR_KEYS_NOT & data[i]->addr_mode) {
                asprintf(&tmp_out, "\t\t\tORTE_GPR_KEYS_NOT\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_KEYS_AND & data[i]->addr_mode) {
                asprintf(&tmp_out, "\t\t\tORTE_GPR_KEYS_AND\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_KEYS_OR & data[i]->addr_mode) {
                asprintf(&tmp_out, "\t\t\tORTE_GPR_KEYS_OR\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_KEYS_XAND & data[i]->addr_mode) {
                asprintf(&tmp_out, "\t\t\tORTE_GPR_KEYS_XAND\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_KEYS_XOR & data[i]->addr_mode) {
                asprintf(&tmp_out, "\t\t\tORTE_GPR_KEYS_XOR\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
    
        }  /* if data[i] not NULL */
    }  /* for i */
    
    if (0 < trig->num_counters) {
        asprintf(&tmp_out, "\tTrigger monitoring %d counters", trig->num_counters);
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        cntr = (orte_gpr_replica_counter_t**)((trig->counters)->addr);
        for (i=0; i < (trig->counters)->size; i++) {
            if (NULL != cntr[i] &&
                ORTE_SUCCESS == orte_gpr_replica_dict_reverse_lookup(&token, cntr[i]->seg,
                    (cntr[i]->iptr)->itag)) {
                asprintf(&tmp_out, "\t\tCounter: %d\tSegment: %s\tName: %s", i,
                                        (cntr[i]->seg)->name, token);
                free(token);
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                orte_gpr_replica_dump_itagval_value(buffer, cntr[i]->iptr);
            }
        }
    }
    
    return;
}


void orte_gpr_replica_dump_itagval_value(orte_buffer_t *buffer,
                                         orte_gpr_replica_itagval_t *iptr)
{
    char *tmp;
    
    switch(iptr->type) {

        case ORTE_BYTE:
            asprintf(&tmp, "\t\tData type: ORTE_BYTE");
            break;
            
        case ORTE_BOOL:
            asprintf(&tmp, "\t\tData type: ORTE_BOOL");
            break;
            
        case ORTE_STRING:
            asprintf(&tmp, "\t\tData type: ORTE_STRING\tValue: %s", iptr->value.strptr);
            break;
            
        case ORTE_SIZE:
            asprintf(&tmp, "\t\tData type: ORTE_SIZE");
            break;
            
        case ORTE_INT:
            asprintf(&tmp, "\t\tData type: ORTE_INT\tValue: %d", (int)iptr->value.i32);
            break;
            
        case ORTE_UINT8:
            asprintf(&tmp, "\t\tData type: ORTE_UINT8\tValue: %d", (int)iptr->value.ui8);
            break;
            
        case ORTE_UINT16:
            asprintf(&tmp, "\t\tData type: ORTE_UINT16\tValue: %d", (int)iptr->value.ui16);
            break;
            
        case ORTE_UINT32:
            asprintf(&tmp, "\t\tData type: ORTE_UINT32\tValue: %d", (int)iptr->value.ui32);
            break;
            
#ifdef HAVE_I64
        case ORTE_UINT64:
            asprintf(&tmp, "\t\tData type: ORTE_UINT64\tValue: %d", (int)iptr->value.ui64);
            break;
#endif

        case ORTE_INT8:
            asprintf(&tmp, "\t\tData type: ORTE_INT8\tValue: %d", (int)iptr->value.i8);
            break;
        
        case ORTE_INT16:
            asprintf(&tmp, "\t\tData type: ORTE_INT16\tValue: %d", (int)iptr->value.i16);
            break;
        
        case ORTE_INT32:
            asprintf(&tmp, "\t\tData type: ORTE_INT32\tValue: %d", (int)iptr->value.i32);
            break;
        
#ifdef HAVE_I64
        case ORTE_INT64:
            asprintf(&tmp, "\t\tData type: ORTE_INT64\tValue: %d", (int)iptr->value.i64);
            break;
#endif

        case ORTE_BYTE_OBJECT:
            asprintf(&tmp, "\t\tData type: ORTE_BYTE_OBJECT\tSize: %d", (int)(iptr->value.byteobject).size);
            break;
            
        case ORTE_NAME:
            asprintf(&tmp, "\t\tData type: ORTE_NAME\tValue: [%d,%d,%d]", ORTE_NAME_ARGS(&(iptr->value.proc)));
            break;
            
        case ORTE_VPID:
            asprintf(&tmp, "\t\tData type: ORTE_VPID\tValue: %d", (int)iptr->value.vpid);
            break;
            
        case ORTE_JOBID:
            asprintf(&tmp, "\t\tData type: ORTE_JOBID\tValue: %d", (int)iptr->value.jobid);
            break;
            
        case ORTE_CELLID:
            asprintf(&tmp, "\t\tData type: ORTE_CELLID\tValue: %d", (int)iptr->value.cellid);
            break;
            
        case ORTE_NODE_STATE:
            asprintf(&tmp, "\t\tData type: ORTE_NODE_STATE\tValue: %d", (int)iptr->value.node_state);
            break;
            
        case ORTE_PROC_STATE:
            asprintf(&tmp, "\t\tData type: ORTE_PROC_STATE\tValue: %d", (int)iptr->value.proc_state);
            break;
            
        case ORTE_EXIT_CODE:
            asprintf(&tmp, "\t\tData type: ORTE_EXIT_CODE\tValue: %d", (int)iptr->value.exit_code);
            break;
            
        case ORTE_NULL:
            asprintf(&tmp, "\t\tData type: ORTE_NULL");
            break;
        
        case ORTE_APP_CONTEXT:
            asprintf(&tmp, "\t\tData type: ORTE_APP_CONTEXT");
            break;
            
        default:
            asprintf(&tmp, "\t\tData type: UNKNOWN");
            break;
    }
    
    orte_gpr_replica_dump_load_string(buffer, &tmp);
}


static void orte_gpr_replica_dump_load_string(orte_buffer_t *buffer, char **tmp)
{
    orte_dps.pack(buffer, tmp, 1, ORTE_STRING);
    free(*tmp);

}
