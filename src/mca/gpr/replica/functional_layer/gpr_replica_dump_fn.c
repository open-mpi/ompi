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

#include "mca/errmgr/errmgr.h"
#include "mca/ns/ns_types.h"
#include "mca/soh/soh_types.h"

#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"
#include "gpr_replica_fn.h"

static void orte_gpr_replica_dump_load_string(orte_buffer_t *buffer, char **tmp);

void orte_gpr_replica_dump_itagval_value(orte_buffer_t *buffer,
                                         orte_gpr_replica_itagval_t *iptr);

static void orte_gpr_replica_dump_trigger(orte_buffer_t *buffer, size_t cnt,
                                          orte_gpr_replica_triggers_t *trig);


int orte_gpr_replica_dump_all_fn(orte_buffer_t *buffer)
{
    char tmp_out[80], *tmp;
    int rc;
    
    tmp = tmp_out;
    sprintf(tmp_out, "\n\n\nDUMP OF GENERAL PURPOSE REGISTRY");
    orte_gpr_replica_dump_load_string(buffer, &tmp);
    
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
    size_t num_objects;
    size_t i, j, k;
    char *tmp_out;

    tmp_out = (char*)malloc(1000);
    if (NULL == tmp_out) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    sprintf(tmp_out, "\nDUMP OF GPR SEGMENTS");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    /* loop through all segments */
    seg = (orte_gpr_replica_segment_t**)(orte_gpr_replica.segments)->addr;
    for (i=0; i < (orte_gpr_replica.segments)->size; i++) {
         if (NULL != seg[i]) {

             sprintf(tmp_out, "\nGPR Dump for Segment: %s", seg[i]->name);
             orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            
             num_objects = (seg[i]->containers)->size - (seg[i]->containers)->number_free;
            
             sprintf(tmp_out, "\tNumber of containers: %lu\n", 
                     (unsigned long) num_objects);
             orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            
             /* loop through all containers and print their info and contents */
             cptr = (orte_gpr_replica_container_t**)(seg[i]->containers)->addr;
             for (j=0; j < (seg[i]->containers)->size; j++) {
                 if (NULL != cptr[j]) {
                     sprintf(tmp_out, "\n\tInfo for container %lu" 
                                      "\tNumber of keyvals: %lu"
                                      "\n\tTokens:\n",
                             (unsigned long) j, 
                             ((unsigned long) (cptr[j]->itagvals)->size - (cptr[j]->itagvals)->number_free));
                     orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                     
                     /* reverse lookup tokens and print them */
                     itaglist = cptr[j]->itags;
                     for (k=0; k < cptr[j]->num_itags; k++) {
                         if (ORTE_SUCCESS != orte_gpr_replica_dict_reverse_lookup(
                                                               &token, seg[i], itaglist[k])) {
                             sprintf(tmp_out, "\t\titag num %lu"
                                     ": No entry found for itag %lu",
                                     (unsigned long) k, 
                                     (unsigned long) itaglist[k]);
                         } else {
                             sprintf(tmp_out, "\t\titag num %lu: itag %lu\tToken: %s",
                                     (unsigned long) k, 
                                     (unsigned long) itaglist[k], token);
                             free(token);
                         }
                         orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                     }
                     
                     sprintf(tmp_out, "\n\tKeyval info:");
                     orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                
                     /* loop through all itagvals and print their info */
                     iptr = (orte_gpr_replica_itagval_t**)(cptr[j]->itagvals)->addr;
                     for (k=0; k < (cptr[j]->itagvals)->size; k++) {
                         if (NULL != iptr[k]) {
                             if (ORTE_SUCCESS != orte_gpr_replica_dict_reverse_lookup(
                                                             &token, seg[i], iptr[k]->itag)) {
                                 sprintf(tmp_out, "\n\t\titag num %lu: No entry found for itag %lu",
                                         (unsigned long) k, 
                                         (unsigned long) iptr[k]->itag);
                             } else {
                                 sprintf(tmp_out, "\n\t\tEntry %lu: itag %lu\tKey: %s",
                                         (unsigned long) k, 
                                         (unsigned long) iptr[k]->itag, token);
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
    
    free(tmp_out);
    return ORTE_SUCCESS;
}


int orte_gpr_replica_dump_callbacks_fn(orte_buffer_t *buffer)
{
    ompi_list_item_t *item;
    orte_gpr_replica_callbacks_t *cb;
    orte_gpr_replica_notify_msg_list_t *msg;
    char *tmp_out;
    size_t i, j, k;
    
    tmp_out = (char*)malloc(1000);
    if (NULL == tmp_out) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    sprintf(tmp_out, "\nDUMP OF GPR REGISTERED CALLBACKS\n");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    if (0 >= (k= ompi_list_get_size(&(orte_gpr_replica.callbacks)))) {
        sprintf(tmp_out, "--- None registered at this time ---");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        return ORTE_SUCCESS;
    } else {
        sprintf(tmp_out, "--- %lu callback(s) registered at this time", 
                (unsigned long) k);
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    
    i=0;
    for (cb = (orte_gpr_replica_callbacks_t*)ompi_list_get_first(&(orte_gpr_replica.callbacks));
         cb != (orte_gpr_replica_callbacks_t*)ompi_list_get_end(&(orte_gpr_replica.callbacks));
         cb = (orte_gpr_replica_callbacks_t*)ompi_list_get_next(cb)) {
         if (NULL == cb) {
            sprintf(tmp_out, "\n\t---  BAD CALLBACK POINTER %lu ---", 
                    (unsigned long) i);
            orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            return ORTE_SUCCESS;
         }
         sprintf(tmp_out, "\nInfo for callback %lu", (unsigned long) i);
         orte_gpr_replica_dump_load_string(buffer, &tmp_out);
         if (NULL == cb->requestor) {
            sprintf(tmp_out, "Local requestor");
         } else {
            sprintf(tmp_out, "Requestor: [%lu,%lu,%lu]", 
                    ORTE_NAME_ARGS(cb->requestor));
         }
         orte_gpr_replica_dump_load_string(buffer, &tmp_out);
         j = ompi_list_get_size(&(cb->messages));
         sprintf(tmp_out, "Num messages: %lu", (unsigned long) j);
         orte_gpr_replica_dump_load_string(buffer, &tmp_out);
         for (k=0, item = ompi_list_get_first(&(cb->messages));
              item != ompi_list_get_end(&(cb->messages));
              item = ompi_list_get_next(item), k++) {
              msg = (orte_gpr_replica_notify_msg_list_t*)item;
              sprintf(tmp_out, "\n\nInfo for message %lu sending %lu "
                      " data objects to notifier id %lu",
                      (unsigned long) k, (unsigned long) (msg->message)->cnt,
                      (unsigned long) (msg->message)->idtag);
              orte_gpr_replica_dump_load_string(buffer, &tmp_out);
              orte_gpr_base_dump_notify_msg(buffer, msg->message);
         }
         i++;
     }
         
    sprintf(tmp_out, "\n");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    free(tmp_out);
    return ORTE_SUCCESS;
}


int orte_gpr_replica_dump_triggers_fn(orte_buffer_t *buffer)
{
    orte_gpr_replica_triggers_t **trig;
    char tmp_out[100], *tmp;
    size_t j, k;
    
    tmp = tmp_out;
    sprintf(tmp_out, "\nDUMP OF GPR TRIGGERS\n");
    orte_gpr_replica_dump_load_string(buffer, &tmp);

    trig = (orte_gpr_replica_triggers_t**)((orte_gpr_replica.triggers)->addr);
    k = 0;
    for (j=0; j < (orte_gpr_replica.triggers)->size; j++) {
        if (NULL != trig[j]) k++;
    }
    
    sprintf(tmp_out, "Number of triggers: %lu\n", (unsigned long) k);
    orte_gpr_replica_dump_load_string(buffer, &tmp);
    
    /* dump the trigger info for the registry */
    for (j=0, k=0; j < (orte_gpr_replica.triggers)->size; j++) {
        if (NULL != trig[j]) {
            orte_gpr_replica_dump_trigger(buffer, k, trig[j]);
            k++;
        }
    }

    return ORTE_SUCCESS;
}    
    
static void orte_gpr_replica_dump_trigger(orte_buffer_t *buffer, size_t cnt,
                                          orte_gpr_replica_triggers_t *trig)
{
    char *tmp_out, *token;
    size_t i, j, k;
    orte_gpr_replica_subscribed_data_t **data;
    orte_gpr_replica_counter_t **cntr;
    
    tmp_out = (char*)malloc(1000);
    if (NULL == tmp_out) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return;
    }
    
    sprintf(tmp_out, "\nData for trigger %lu", (unsigned long) cnt);
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    /* output recipient info */
    if (NULL == trig->requestor) {
        sprintf(tmp_out, "\tIntended recipient: LOCAL @ notifier idtag %lu",
                (unsigned long) trig->index);
    } else {
        sprintf(tmp_out, "\tIntended recipient: [%lu,%lu,%lu] @ notifier idtag %lu",
                ORTE_NAME_ARGS(trig->requestor), 
                (unsigned long) trig->remote_idtag);
    }
 
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    sprintf(tmp_out, "\tActions:");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    if (ORTE_GPR_NOTIFY_VALUE_CHG & trig->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_VALUE_CHG");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    } else if (ORTE_GPR_NOTIFY_VALUE_CHG_TO & trig->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_VALUE_CHG_TO");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    } else if (ORTE_GPR_NOTIFY_VALUE_CHG_FRM & trig->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_VALUE_CHG_FRM");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_NOTIFY_DEL_ENTRY & trig->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_DEL_ENTRY");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_NOTIFY_ADD_ENTRY & trig->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_ADD_ENTRY");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_NOTIFY_PRE_EXISTING & trig->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_PRE_EXISTING");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TRIG_ONE_SHOT & trig->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_TRIG_ONE_SHOT");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TRIG_AT_LEVEL & trig->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_TRIG_AT_LEVEL");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TRIG_CMP_LEVELS & trig->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_TRIG_CMP_LEVELS");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TRIG_MONITOR_ONLY & trig->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_TRIG_MONITOR_ONLY");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TRIG_NOTIFY_START & trig->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_TRIG_NOTIFY_START");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TRIG_INCLUDE_DATA & trig->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_TRIG_INCLUDE_DATA");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }

    sprintf(tmp_out, "\n\tData covered by this subscription");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    data = (orte_gpr_replica_subscribed_data_t**)((trig->subscribed_data)->addr);
    for (i=0; i < (trig->subscribed_data)->size; i++) {
        if (NULL != data[i]) {
            sprintf(tmp_out, "\n\t\tData on segment %s", (data[i]->seg)->name);
            orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            
            k = (int)orte_value_array_get_size(&(data[i]->tokentags));
            if (0 == k) {
                sprintf(tmp_out, "\t\tNULL token (wildcard)");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            } else {
                sprintf(tmp_out, "\t\tNumber of tokens: %lu", 
                        (unsigned long) k);
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            
                for (j=0; j < k; j++) {
                    if (ORTE_SUCCESS == orte_gpr_replica_dict_reverse_lookup(&token, data[i]->seg,
                            ORTE_VALUE_ARRAY_GET_ITEM(&(data[i]->tokentags), orte_gpr_replica_itag_t, j))) {
                        sprintf(tmp_out, "\t\t\tToken: %s", token);
            		       orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                        free(token);
                    }
            	   }
            }
    
            sprintf(tmp_out, "\t\tToken addressing mode:\n");
            orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        
            if (ORTE_GPR_TOKENS_NOT & data[i]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_TOKENS_NOT\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_TOKENS_AND & data[i]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_TOKENS_AND\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_TOKENS_OR & data[i]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_TOKENS_OR\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_TOKENS_XAND & data[i]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_TOKENS_XAND\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_TOKENS_XOR & data[i]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_TOKENS_XOR\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            
            k = (int)orte_value_array_get_size(&(data[i]->keytags));
            if (0 == k) {
                sprintf(tmp_out, "\t\tNULL key (wildcard)");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            } else {
                sprintf(tmp_out, "\t\tNumber of keys: %lu", (unsigned long) k);
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        
                for (j=0; j < k; j++) {
                    if (ORTE_SUCCESS == orte_gpr_replica_dict_reverse_lookup(&token, data[i]->seg,
                            ORTE_VALUE_ARRAY_GET_ITEM(&(data[i]->keytags), orte_gpr_replica_itag_t, j))) {
                        sprintf(tmp_out, "\t\t\tKey: %s", token);
                        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                        free(token);
                    }
                }
            }
            
            sprintf(tmp_out, "\t\tKey addressing mode:\n");
            orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        
            if (ORTE_GPR_KEYS_NOT & data[i]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_KEYS_NOT\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_KEYS_AND & data[i]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_KEYS_AND\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_KEYS_OR & data[i]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_KEYS_OR\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_KEYS_XAND & data[i]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_KEYS_XAND\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_KEYS_XOR & data[i]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_KEYS_XOR\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
    
        }  /* if data[i] not NULL */
    }  /* for i */
        
    if (0 < trig->num_counters) {
        if (ORTE_GPR_TRIG_AT_LEVEL & trig->action) {
            sprintf(tmp_out, "\tTrigger monitoring %lu counters for level", 
                    (unsigned long) trig->num_counters);
        } else {
            sprintf(tmp_out, "\tTrigger monitoring %lu counters for compare",
                    (unsigned long) trig->num_counters);
        }
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        cntr = (orte_gpr_replica_counter_t**)((trig->counters)->addr);
        for (i=0; i < (trig->counters)->size; i++) {
            if (NULL != cntr[i] &&
                ORTE_SUCCESS == orte_gpr_replica_dict_reverse_lookup(&token, cntr[i]->seg,
                    (cntr[i]->iptr)->itag)) {
                sprintf(tmp_out, "\t\tCounter: %lu\tSegment: %s\tName: %s", 
                        (unsigned long) i, (cntr[i]->seg)->name, token);
                free(token);
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                if (ORTE_GPR_TRIG_AT_LEVEL & trig->action) {
                    sprintf(tmp_out, "\t\tTrigger Level:");
                    orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                    orte_gpr_replica_dump_itagval_value(buffer, &(cntr[i]->trigger_level));
                }
                sprintf(tmp_out, "\t\tCurrent Value:");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                orte_gpr_replica_dump_itagval_value(buffer, cntr[i]->iptr);
            }
        }
    }
    
    free(tmp_out);
    return;
}


void orte_gpr_replica_dump_itagval_value(orte_buffer_t *buffer,
                                         orte_gpr_replica_itagval_t *iptr)
{
    char tmp[132], *tmp2;
    
    tmp2 = tmp;
    switch(iptr->type) {

        case ORTE_BYTE:
            sprintf(tmp, "\t\tData type: ORTE_BYTE");
            break;
            
        case ORTE_BOOL:
            sprintf(tmp, "\t\tData type: ORTE_BOOL\tValue: %s", iptr->value.tf_flag ? "TRUE" : "FALSE");
            break;
            
        case ORTE_STRING:
            sprintf(tmp, "\t\tData type: ORTE_STRING\tValue: %s", iptr->value.strptr);
            break;
            
        case ORTE_SIZE:
            sprintf(tmp, "\t\tData type: ORTE_SIZE\tValue: %lu",
                    (unsigned long) iptr->value.size);
            break;
            
        case ORTE_PID:
            sprintf(tmp, "\t\tData type: ORTE_PID\tValue: " ORTE_PID_T_PRINTF,
                            iptr->value.pid);
            break;
            
        case ORTE_INT:
            sprintf(tmp, "\t\tData type: ORTE_INT\tValue: %d", (int)iptr->value.i32);
            break;
            
        case ORTE_UINT8:
            sprintf(tmp, "\t\tData type: ORTE_UINT8\tValue: %d", (int)iptr->value.ui8);
            break;
            
        case ORTE_UINT16:
            sprintf(tmp, "\t\tData type: ORTE_UINT16\tValue: %d", (int)iptr->value.ui16);
            break;
            
        case ORTE_UINT32:
            sprintf(tmp, "\t\tData type: ORTE_UINT32\tValue: %d", (int)iptr->value.ui32);
            break;
            
#ifdef HAVE_INT64_T
        case ORTE_UINT64:
            sprintf(tmp, "\t\tData type: ORTE_UINT64\tValue: %d", (int)iptr->value.ui64);
            break;
#endif

        case ORTE_INT8:
            sprintf(tmp, "\t\tData type: ORTE_INT8\tValue: %d", (int)iptr->value.i8);
            break;
        
        case ORTE_INT16:
            sprintf(tmp, "\t\tData type: ORTE_INT16\tValue: %d", (int)iptr->value.i16);
            break;
        
        case ORTE_INT32:
            sprintf(tmp, "\t\tData type: ORTE_INT32\tValue: %d", (int)iptr->value.i32);
            break;
        
#ifdef HAVE_INT64_T
        case ORTE_INT64:
            sprintf(tmp, "\t\tData type: ORTE_INT64\tValue: %d", (int)iptr->value.i64);
            break;
#endif

        case ORTE_BYTE_OBJECT:
            sprintf(tmp, "\t\tData type: ORTE_BYTE_OBJECT\tSize: %lu",
                    (unsigned long) (iptr->value.byteobject).size);
            break;
            
        case ORTE_NAME:
            sprintf(tmp, "\t\tData type: ORTE_NAME\tValue: [%lu,%lu,%lu]",
                    ORTE_NAME_ARGS(&(iptr->value.proc)));
            break;
            
        case ORTE_VPID:
            sprintf(tmp, "\t\tData type: ORTE_VPID\tValue: %lu",
                    (unsigned long) iptr->value.vpid);
            break;
            
        case ORTE_JOBID:
            sprintf(tmp, "\t\tData type: ORTE_JOBID\tValue: %lu",
                    (unsigned long) iptr->value.jobid);
            break;
            
        case ORTE_CELLID:
            sprintf(tmp, "\t\tData type: ORTE_CELLID\tValue: %lu",
                    (unsigned long) iptr->value.cellid);
            break;
            
        case ORTE_NODE_STATE:
            sprintf(tmp, "\t\tData type: ORTE_NODE_STATE\tValue: %d", (int)iptr->value.node_state);
            break;
            
        case ORTE_PROC_STATE:
            sprintf(tmp, "\t\tData type: ORTE_PROC_STATE\tValue: %d", (int)iptr->value.proc_state);
            break;
            
        case ORTE_EXIT_CODE:
            sprintf(tmp, "\t\tData type: ORTE_EXIT_CODE\tValue: %d", (int)iptr->value.exit_code);
            break;
            
        case ORTE_NULL:
            sprintf(tmp, "\t\tData type: ORTE_NULL");
            break;
        
        case ORTE_APP_CONTEXT:
            sprintf(tmp, "\t\tData type: ORTE_APP_CONTEXT");
            break;
            
        default:
            sprintf(tmp, "\t\tData type: UNKNOWN");
            break;
    }
    
    if (NULL == buffer) {
        ompi_output(0, "%s", tmp);
    } else {
        orte_gpr_replica_dump_load_string(buffer, &tmp2);
    }
}


static void orte_gpr_replica_dump_load_string(orte_buffer_t *buffer, char **tmp)
{
    orte_dps.pack(buffer, tmp, 1, ORTE_STRING);
}
