/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "orte/dss/dss.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/smr/smr_types.h"

#include "orte/mca/gpr/replica/transition_layer/gpr_replica_tl.h"
#include "gpr_replica_fn.h"

static void orte_gpr_replica_dump_load_string(orte_buffer_t *buffer, char **tmp);

void orte_gpr_replica_dump_itagval_value(orte_buffer_t *buffer,
                                         orte_gpr_replica_itagval_t *iptr);

static int orte_gpr_replica_get_segment_size_fn(size_t *segsize, orte_gpr_replica_segment_t *seg);


int orte_gpr_replica_dump_all_fn(orte_buffer_t *buffer)
{
    char tmp_out[80], *tmp;
    int rc;

    tmp = tmp_out;
    sprintf(tmp_out, "\n\n\nDUMP OF GENERAL PURPOSE REGISTRY");
    orte_gpr_replica_dump_load_string(buffer, &tmp);

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_triggers_fn(buffer, 0))) {
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_subscriptions_fn(buffer, 0))) {
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_callbacks_fn(buffer))) {
        return rc;
    }

    rc = orte_gpr_replica_dump_segments_fn(buffer, NULL);

    return rc;
}

int orte_gpr_replica_dump_segments_fn(orte_buffer_t *buffer, char *segment)
{
    orte_gpr_replica_segment_t **seg, *segptr;
    orte_std_cntr_t i, m;
    int rc;

    /* if segment = NULL, loop through all segments */
    if (NULL == segment) {
        seg = (orte_gpr_replica_segment_t**)(orte_gpr_replica.segments)->addr;
        for (i=0, m=0; m < orte_gpr_replica.num_segs &&
                       i < (orte_gpr_replica.segments)->size; i++) {
             if (NULL != seg[i]) {
                 m++;
                 if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_a_segment_fn(buffer, seg[i]))) {
                     ORTE_ERROR_LOG(rc);
                     return rc;
                 }
             }
         }
         return ORTE_SUCCESS;
     }

     /* otherwise, dump just the one specified */
     if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&segptr, false, segment))) {
         ORTE_ERROR_LOG(rc);
         return rc;
     }
     if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_a_segment_fn(buffer, segptr))) {
         ORTE_ERROR_LOG(rc);
         return rc;
     }
     return ORTE_SUCCESS;
 }

 int orte_gpr_replica_dump_a_segment_fn(orte_buffer_t *buffer, orte_gpr_replica_segment_t *seg)
 {
    orte_gpr_replica_container_t **cptr;
    orte_gpr_replica_itag_t *itaglist;
    orte_gpr_replica_itagval_t **iptr;
    char *token;
    orte_std_cntr_t num_objects;
    orte_std_cntr_t j, k, n, p;
    char *tmp_out;

    tmp_out = (char*)malloc(1000);
    if (NULL == tmp_out) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    sprintf(tmp_out, "\nDUMP OF GPR SEGMENT %s", seg->name);
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

     num_objects = (seg->containers)->size - (seg->containers)->number_free;

     sprintf(tmp_out, "\tNumber of containers: %lu\n",
             (unsigned long) num_objects);
     orte_gpr_replica_dump_load_string(buffer, &tmp_out);

     /* loop through all containers and print their info and contents */
     cptr = (orte_gpr_replica_container_t**)(seg->containers)->addr;
     for (j=0, n=0; n < seg->num_containers &&
                    j < (seg->containers)->size; j++) {
         if (NULL != cptr[j]) {
             n++;
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
                                                       &token, seg, itaglist[k])) {
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
             for (k=0, p=0; p < cptr[j]->num_itagvals &&
                            k < (cptr[j]->itagvals)->size; k++) {
                 if (NULL != iptr[k]) {
                     p++;
                     if (ORTE_SUCCESS != orte_gpr_replica_dict_reverse_lookup(
                                                     &token, seg, iptr[k]->itag)) {
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

    free(tmp_out);
    return ORTE_SUCCESS;
}


int orte_gpr_replica_dump_callbacks_fn(orte_buffer_t *buffer)
{
    orte_gpr_replica_callbacks_t *cb;
    orte_gpr_replica_action_taken_t **action;
    orte_gpr_replica_itag_t *itaglist;
    char *tmp_out, *token;
    orte_std_cntr_t i, j, k;

    tmp_out = (char*)malloc(1000);
    if (NULL == tmp_out) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    sprintf(tmp_out, "\nDUMP OF GPR REGISTERED CALLBACKS\n");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    if (0 >= (k = (orte_std_cntr_t)opal_list_get_size(&(orte_gpr_replica.callbacks)))) {
        sprintf(tmp_out, "--- None registered at this time ---");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    } else {
        sprintf(tmp_out, "--- %lu callback(s) registered at this time",
                (unsigned long) k);
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);

        i=0;
        for (cb = (orte_gpr_replica_callbacks_t*)opal_list_get_first(&(orte_gpr_replica.callbacks));
             cb != (orte_gpr_replica_callbacks_t*)opal_list_get_end(&(orte_gpr_replica.callbacks));
             cb = (orte_gpr_replica_callbacks_t*)opal_list_get_next(cb)) {
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
             orte_gpr_base_dump_notify_msg(buffer, cb->message);
             i++;
        }
    }

    sprintf(tmp_out, "\n");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    if (0 < orte_gpr_replica_globals.num_acted_upon) {
        sprintf(tmp_out, "\nDUMP OF GPR ACTION RECORDS\n");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);

        action = (orte_gpr_replica_action_taken_t**)orte_gpr_replica_globals.acted_upon->addr;
        for (i=0, j=0; j < orte_gpr_replica_globals.num_acted_upon &&
                       i < (orte_gpr_replica_globals.acted_upon)->size; i++) {
            if (NULL != action[i]) {
                j++;
                if (NULL != action[i]->seg) {
                    sprintf(tmp_out, "\nAction Taken on Segment: %s", action[i]->seg->name);
                    orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                } else {
                    sprintf(tmp_out, "\nAction Taken on NULL Segment");
                    orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                }
                if (NULL != action[i]->cptr) {
                     sprintf(tmp_out, "\tContainer Tokens:");
                     orte_gpr_replica_dump_load_string(buffer, &tmp_out);

                     /* reverse lookup tokens and print them */
                     itaglist = action[i]->cptr->itags;
                     for (k=0; k < action[i]->cptr->num_itags; k++) {
                         if (ORTE_SUCCESS != orte_gpr_replica_dict_reverse_lookup(
                                                               &token, action[i]->seg, itaglist[k])) {
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
                } else {
                    sprintf(tmp_out, "\tNULL Container");
                    orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                }
                if (NULL != action[i]->iptr) {
                     if (ORTE_GPR_REPLICA_ENTRY_ADDED & action[i]->action) {
                         sprintf(tmp_out, "\n\tKeyval ADDED:");
                         orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                     }
                     if (ORTE_GPR_REPLICA_ENTRY_DELETED & action[i]->action) {
                         sprintf(tmp_out, "\n\tKeyval DELETED:");
                         orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                     }
                     if (ORTE_GPR_REPLICA_ENTRY_CHANGED & action[i]->action) {
                         sprintf(tmp_out, "\n\tKeyval CHANGED");
                         orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                     }
                     if (ORTE_GPR_REPLICA_ENTRY_CHG_TO & action[i]->action) {
                         sprintf(tmp_out, "\t\tKeyval CHANGED TO:");
                         orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                     }
                     if (ORTE_GPR_REPLICA_ENTRY_CHG_FRM & action[i]->action) {
                         sprintf(tmp_out, "\t\tKeyval CHANGED FROM:");
                         orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                     }

                     if (ORTE_SUCCESS != orte_gpr_replica_dict_reverse_lookup(
                                                     &token, action[i]->seg, action[i]->iptr->itag)) {
                         sprintf(tmp_out, "\t\tNo entry found for itag %lu",
                                 (unsigned long) action[i]->iptr->itag);
                     } else {
                         sprintf(tmp_out, "\t\titag %lu\tKey: %s",
                                 (unsigned long) action[i]->iptr->itag, token);
                         free(token);
                     }
                     orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                     orte_gpr_replica_dump_itagval_value(buffer, action[i]->iptr);
                 } else {
                    sprintf(tmp_out, "\tNULL Keyval");
                     orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                 }
             }
        }
    } else {
        sprintf(tmp_out, "\nNO GPR ACTION RECORDS STORED\n");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }

    free(tmp_out);
    return ORTE_SUCCESS;
}


int orte_gpr_replica_dump_triggers_fn(orte_buffer_t *buffer,
                                      orte_gpr_trigger_id_t start)
{
    orte_gpr_replica_trigger_t **trig;
    char tmp_out[100], *tmp;
    orte_std_cntr_t j, k, m;
    int rc;

    tmp = tmp_out;
    sprintf(tmp_out, "\nDUMP OF GPR TRIGGERS\n");
    orte_gpr_replica_dump_load_string(buffer, &tmp);

    trig = (orte_gpr_replica_trigger_t**)((orte_gpr_replica.triggers)->addr);
    sprintf(tmp_out, "Number of triggers: %lu\n", (unsigned long) orte_gpr_replica.num_trigs);
    orte_gpr_replica_dump_load_string(buffer, &tmp);

    /* dump the trigger info for the registry */
    if (0 == start) { /* dump the whole thing */
        m = 0;
    } else {
        m = orte_gpr_replica.num_trigs - start;
    }

    for (j=0, k=0; k < orte_gpr_replica.num_trigs &&
                   j < (orte_gpr_replica.triggers)->size; j++) {
        if (NULL != trig[j]) {
            if (k >= m) {
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_trigger(buffer, trig[j]))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
            k++;
        }
    }

    return ORTE_SUCCESS;
}

int orte_gpr_replica_dump_trigger(orte_buffer_t *buffer,
                                  orte_gpr_replica_trigger_t *trig)
{
    char *tmp_out, *token;
    orte_std_cntr_t i, j;
    orte_gpr_replica_counter_t **cntr;
    orte_gpr_replica_subscription_t **subs;
    orte_gpr_replica_trigger_requestor_t **attached;

    tmp_out = (char*)malloc(1000);
    if (NULL == tmp_out) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    sprintf(tmp_out, "\nData for trigger %lu", (unsigned long) trig->index);
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    if (NULL == trig->name) {
        sprintf(tmp_out, "\tNOT a named trigger");
    } else {
        sprintf(tmp_out, "\ttrigger name: %s", trig->name);
    }
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    if (0 < trig->num_attached) {
        sprintf(tmp_out, "\t%lu requestors attached to this trigger",
            (unsigned long) trig->num_attached);
    } else {
        sprintf(tmp_out, "\tNo requestors attached to this trigger");
    }
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    attached = (orte_gpr_replica_trigger_requestor_t**)
                    (trig->attached)->addr;
    for (i=0, j=0; j < trig->num_attached &&
                   i < (trig->attached)->size; i++) {
        if (NULL != attached[i]) {
            j++;
            if (NULL == attached[i]->requestor) {
                sprintf(tmp_out, "\t\tRequestor %lu: LOCAL@idtag %lu",
                    (unsigned long)j, (unsigned long)attached[i]->idtag);
            } else {
                sprintf(tmp_out, "\t\tRequestor %lu: [%lu,%lu,%lu]@idtag %lu",
                    (unsigned long)j, ORTE_NAME_ARGS(attached[i]->requestor),
                    (unsigned long)attached[i]->idtag);
            }
            orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        }
    }

    if (NULL == trig->master) {
        sprintf(tmp_out, "\tNO MASTER registered");
    } else {
        if (NULL == trig->master->requestor) {
            sprintf(tmp_out, "\tTRIGGER MASTER: LOCAL@idtag %lu",
                (unsigned long)trig->master->idtag);
        } else {
            sprintf(tmp_out, "\tTRIGGER MASTER: [%lu,%lu,%lu]@idtag %lu",
                ORTE_NAME_ARGS(trig->master->requestor),
                (unsigned long)trig->master->idtag);
        }
    }
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    if (ORTE_GPR_TRIG_ONE_SHOT & trig->action) {
        sprintf(tmp_out, "\tORTE_GPR_TRIG_ONE_SHOT");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TRIG_AT_LEVEL & trig->action) {
        sprintf(tmp_out, "\tORTE_GPR_TRIG_AT_LEVEL");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TRIG_CMP_LEVELS & trig->action) {
        sprintf(tmp_out, "\tORTE_GPR_TRIG_CMP_LEVELS");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS & trig->action) {
        sprintf(tmp_out, "\tORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }

    if (trig->one_shot_fired) {
        sprintf(tmp_out, "\tONE SHOT HAS FIRED");
    } else {
        sprintf(tmp_out, "\tONE SHOT HAS NOT FIRED");
    }
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

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
        for (i=0, j=0; j < trig->num_counters &&
                       i < (trig->counters)->size; i++) {
            if (NULL != cntr[i] &&
                ORTE_SUCCESS == orte_gpr_replica_dict_reverse_lookup(&token, cntr[i]->seg,
                    (cntr[i]->iptr)->itag)) {
                j++;
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

    if (0 < trig->num_subscriptions) {
        sprintf(tmp_out, "\tTrigger has %lu subscriptions attached to it",
                        (unsigned long) trig->num_subscriptions);
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        subs = (orte_gpr_replica_subscription_t**)((trig->subscriptions)->addr);
        for (i=0, j=0; j < trig->num_subscriptions &&
                       i < (trig->subscriptions)->size; i++) {
            if (NULL != subs[i]) {
                j++;
                orte_gpr_replica_dump_subscription(buffer, subs[i]);
            }
        }
    }

    free(tmp_out);
    return ORTE_SUCCESS;
}

int orte_gpr_replica_dump_subscriptions_fn(orte_buffer_t *buffer,
                    orte_gpr_subscription_id_t start)
{
    char *tmp_out, *tmp;
    orte_std_cntr_t i, m, n;
    orte_gpr_replica_subscription_t **subs;
    int rc;

    tmp_out = (char*)malloc(1000);
    if (NULL == tmp_out) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    tmp = tmp_out;

    sprintf(tmp_out, "\nDUMP OF GPR SUBSCRIPTIONS\n");
    orte_gpr_replica_dump_load_string(buffer, &tmp);

    subs = (orte_gpr_replica_subscription_t**)((orte_gpr_replica.subscriptions)->addr);
    sprintf(tmp_out, "Number of subscriptions: %lu\n", (unsigned long) orte_gpr_replica.num_subs);
    orte_gpr_replica_dump_load_string(buffer, &tmp);

    /* dump the subscription info for the registry */
    if (0 == start) {  /* dump the whole thing */
        n = 0;
    } else {
        n = orte_gpr_replica.num_subs - start;
    }

    for (i=0, m=0; m < orte_gpr_replica.num_subs &&
                   i < (orte_gpr_replica.subscriptions)->size; i++) {
        if (NULL != subs[i]) {
            if (m >= n) {
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_dump_subscription(buffer, subs[i]))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
            m++;
        }
    }
    free(tmp_out);
    return ORTE_SUCCESS;
}

int orte_gpr_replica_dump_subscription(orte_buffer_t *buffer,
                                       orte_gpr_replica_subscription_t *sub)
{
    char *tmp_out, *token, *tmp;
    orte_std_cntr_t j, k, n, p;
    orte_gpr_replica_requestor_t **reqs;
    orte_gpr_replica_ivalue_t **ivals;

    tmp_out = (char*)malloc(1000);
    if (NULL == tmp_out) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    tmp = tmp_out;

    if (NULL == sub->name) {
        sprintf(tmp, "\nSubscription %lu: UNNAMED idtag %lu",
            (unsigned long) sub->index, (unsigned long) sub->idtag);
    } else {
        sprintf(tmp, "\nSubscription %lu: name %s idtag %lu",
                (unsigned long) sub->index,
                sub->name, (unsigned long) sub->idtag);
    }
    orte_gpr_replica_dump_load_string(buffer, &tmp);

    if (sub->active) {
        sprintf(tmp_out, "\tSubscription ACTIVE");
    } else {
        sprintf(tmp_out, "\tSubscription INACTIVE");
    }
    orte_gpr_replica_dump_load_string(buffer, &tmp);

    if (sub->cleanup) {
        sprintf(tmp_out, "\tSubscription scheduled for cleanup");
    } else {
        sprintf(tmp_out, "\tSubscription NOT scheduled for cleanup");
    }
    orte_gpr_replica_dump_load_string(buffer, &tmp);

    /* output recipient info */
    sprintf(tmp_out, "\tList of requestors for this subscription:");
    orte_gpr_replica_dump_load_string(buffer, &tmp);
    reqs = (orte_gpr_replica_requestor_t**)(sub->requestors)->addr;
    for (j=0, k=0; k < sub->num_requestors &&
                   j < (sub->requestors)->size; j++) {
        if (NULL != reqs[j]) {
            k++;
            if (NULL == reqs[j]->requestor) {
                sprintf(tmp_out, "\t\tRequestor: LOCAL @ subscription id %lu",
                        (unsigned long) reqs[j]->idtag);
            } else {
                sprintf(tmp_out, "\t\tRequestor: [%lu,%lu,%lu] @ subscription id %lu",
                        ORTE_NAME_ARGS(reqs[j]->requestor),
                        (unsigned long) reqs[j]->idtag);
            }
            orte_gpr_replica_dump_load_string(buffer, &tmp_out);
        }
    }

    sprintf(tmp_out, "\tActions:");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    if (ORTE_GPR_NOTIFY_VALUE_CHG & sub->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_VALUE_CHG");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    } else if (ORTE_GPR_NOTIFY_VALUE_CHG_TO & sub->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_VALUE_CHG_TO");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    } else if (ORTE_GPR_NOTIFY_VALUE_CHG_FRM & sub->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_VALUE_CHG_FRM");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_NOTIFY_DEL_ENTRY & sub->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_DEL_ENTRY");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_NOTIFY_ADD_ENTRY & sub->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_ADD_ENTRY");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_NOTIFY_PRE_EXISTING & sub->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_PRE_EXISTING");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_NOTIFY_STARTS_AFTER_TRIG & sub->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_STARTS_AFTER_TRIG");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG & sub->action) {
        sprintf(tmp_out, "\t\tORTE_GPR_NOTIFY_DELETE_AFTER_TRIG");
        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
    }

    sprintf(tmp_out, "\n\tData covered by this subscription");
    orte_gpr_replica_dump_load_string(buffer, &tmp_out);

    ivals = (orte_gpr_replica_ivalue_t**)(sub->values)->addr;
    for (n=0, p=0; p < sub->num_values &&
                   n < (sub->values)->size; n++) {
        if (NULL != ivals[n]) {
            p++;
            sprintf(tmp_out, "\t\tData on segment %s", (ivals[n]->seg)->name);
            orte_gpr_replica_dump_load_string(buffer, &tmp_out);

            k = (int)orte_value_array_get_size(&(ivals[n]->tokentags));
            if (0 == k) {
                sprintf(tmp_out, "\t\tNULL token (wildcard)");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            } else {
                sprintf(tmp_out, "\t\tNumber of tokens: %lu",
                        (unsigned long) k);
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);

                for (j=0; j < k; j++) {
                    if (ORTE_SUCCESS == orte_gpr_replica_dict_reverse_lookup(&token, ivals[n]->seg,
                            ORTE_VALUE_ARRAY_GET_ITEM(&(ivals[n]->tokentags), orte_gpr_replica_itag_t, j))) {
                        sprintf(tmp_out, "\t\t\tToken: %s", token);
                           orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                        free(token);
                    }
                   }
            }

            sprintf(tmp_out, "\t\tToken addressing mode:\n");
            orte_gpr_replica_dump_load_string(buffer, &tmp_out);

            if (ORTE_GPR_TOKENS_NOT & ivals[n]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_TOKENS_NOT\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_TOKENS_AND & ivals[n]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_TOKENS_AND\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_TOKENS_OR & ivals[n]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_TOKENS_OR\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_TOKENS_XAND & ivals[n]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_TOKENS_XAND\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_TOKENS_XOR & ivals[n]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_TOKENS_XOR\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }

            k = (int)orte_value_array_get_size(&(ivals[n]->keytags));
            if (0 == k) {
                sprintf(tmp_out, "\t\tNULL key (wildcard)");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            } else {
                sprintf(tmp_out, "\t\tNumber of keys: %lu", (unsigned long) k);
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);

                for (j=0; j < k; j++) {
                    if (ORTE_SUCCESS == orte_gpr_replica_dict_reverse_lookup(&token, ivals[n]->seg,
                            ORTE_VALUE_ARRAY_GET_ITEM(&(ivals[n]->keytags), orte_gpr_replica_itag_t, j))) {
                        sprintf(tmp_out, "\t\t\tKey: %s", token);
                        orte_gpr_replica_dump_load_string(buffer, &tmp_out);
                        free(token);
                    }
                }
            }

            sprintf(tmp_out, "\t\tKey addressing mode:\n");
            orte_gpr_replica_dump_load_string(buffer, &tmp_out);

            if (ORTE_GPR_KEYS_NOT & ivals[n]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_KEYS_NOT\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_KEYS_AND & ivals[n]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_KEYS_AND\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_KEYS_OR & ivals[n]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_KEYS_OR\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_KEYS_XAND & ivals[n]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_KEYS_XAND\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
            if (ORTE_GPR_KEYS_XOR & ivals[n]->addr_mode) {
                sprintf(tmp_out, "\t\t\tORTE_GPR_KEYS_XOR\n");
                orte_gpr_replica_dump_load_string(buffer, &tmp_out);
            }
        } /* if ivals[n] not NULL */
    } /* for n */

    free(tmp_out);
    return ORTE_SUCCESS;
}


void orte_gpr_replica_dump_itagval_value(orte_buffer_t *buffer,
                                         orte_gpr_replica_itagval_t *iptr)
{
    char *tmp;
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss.print(&tmp, "\t\t\t", iptr->value, ORTE_DATA_VALUE))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    if (NULL == buffer) {
        opal_output(0, "%s", tmp);
    } else {
        orte_gpr_replica_dump_load_string(buffer, &tmp);
    }

    free(tmp);
}


int orte_gpr_replica_dump_segment_size_fn(orte_buffer_t *buffer, char *segment)
{
    orte_gpr_replica_segment_t **seg, *segptr;
    orte_std_cntr_t i, m;
    size_t segsize, total;
    char tmp[100], *tptr;
    int rc;

    tptr = tmp;

    /* if segment = NULL, loop through all segments */
    if (NULL == segment) {
        seg = (orte_gpr_replica_segment_t**)(orte_gpr_replica.segments)->addr;
        total = 0;
        for (i=0, m=0; m < orte_gpr_replica.num_segs &&
             i < (orte_gpr_replica.segments)->size; i++) {
             if (NULL != seg[i]) {
                 m++;
                 if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_segment_size_fn(&segsize, seg[i]))) {
                     ORTE_ERROR_LOG(rc);
                     return rc;
                 }
                 total += segsize;
             }
         }
         sprintf(tmp, "Total registry size: %lu bytes", (unsigned long)total);
         orte_gpr_replica_dump_load_string(buffer, &tptr);

         return ORTE_SUCCESS;
    }

    /* otherwise, get the size of just the one specified */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&segptr, false, segment))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_segment_size_fn(&segsize, segptr))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    sprintf(tmp, "Size of segment %s: %lu bytes", segment, (unsigned long)segsize);
    orte_gpr_replica_dump_load_string(buffer, &tptr);

    return ORTE_SUCCESS;
}


static void orte_gpr_replica_dump_load_string(orte_buffer_t *buffer, char **tmp)
{
    orte_dss.pack(buffer, tmp, 1, ORTE_STRING);
}

static int orte_gpr_replica_get_segment_size_fn(size_t *segsize, orte_gpr_replica_segment_t *seg)
{
    size_t data_size, isize;
    orte_std_cntr_t i, j, k, m;
    char **dict;
    orte_gpr_replica_container_t **cptr;
    orte_gpr_replica_itagval_t **iptr;
    int rc;

    data_size = strlen(seg->name);
    data_size += 2*sizeof(orte_gpr_replica_itag_t); /* itag, num_dict_entries */

    data_size += (seg->dict)->size * sizeof(void*);  /* account for size of pointer array */
    dict = (char**)(seg->dict)->addr;
    for (i=0, j=0; j < seg->num_dict_entries &&
         i < (seg->dict)->size; i++) {
             if (NULL != dict[i]) {
                 j++;
                 data_size += strlen(dict[i]) + 1;
             }
         }

         data_size += sizeof(orte_std_cntr_t);  /* num_containers */
         cptr = (orte_gpr_replica_container_t**)(seg->containers)->addr;
         for (i=0, j=0; j < (seg->num_containers) &&
              i < (seg->containers)->size; i++) {
                  if (NULL != cptr[i]) {
                      j++;
                      data_size += sizeof(orte_std_cntr_t);  /* index */
                      data_size += cptr[i]->num_itags * sizeof(orte_gpr_replica_itag_t);  /* itags array */
                      data_size += sizeof(orte_std_cntr_t);  /* num_itags */
                      data_size += (cptr[i]->itagvals)->size * sizeof(void*);  /* account for size of pointer array */
                      data_size += sizeof(orte_std_cntr_t);  /* num_itagvals */
                      iptr = (orte_gpr_replica_itagval_t**)(cptr[i]->itagvals)->addr;
                      for (k=0, m=0; m < cptr[i]->num_itagvals &&
                           k < (cptr[i]->itagvals)->size; k++) {
                               if (NULL != iptr[k]) {
                                   m++;
                                   data_size += sizeof(orte_std_cntr_t);  /* index */
                                   data_size += sizeof(orte_gpr_replica_itag_t);
                                   data_size += sizeof(orte_data_type_t);
                                   if (ORTE_SUCCESS != (rc = orte_dss.size(&isize, iptr[k]->value->data, iptr[k]->value->type))) {
                                       ORTE_ERROR_LOG(rc);
                                       *segsize = 0;
                                       return rc;
                                   }
                                   data_size += isize;
                               }
                           }
                           data_size += 3*sizeof(orte_std_cntr_t);
                           data_size += (cptr[i]->itaglist).array_size * sizeof(unsigned char*);
                  }
              }

              *segsize = data_size;
              return ORTE_SUCCESS;
}

