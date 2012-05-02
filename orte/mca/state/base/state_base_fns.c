/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/class/opal_list.h"
#include "opal/mca/event/event.h"

#include "orte/runtime/orte_globals.h"
#include "orte/mca/plm/plm_types.h"

#include "orte/mca/state/base/base.h"
#include "orte/mca/state/base/state_private.h"

#if !ORTE_DISABLE_FULL_SUPPORT

void orte_state_base_activate_job_state(orte_job_t *jdata,
                                        orte_job_state_t state)
{
    opal_list_item_t *itm, *any=NULL, *error=NULL;
    orte_state_t *s;
    orte_state_caddy_t *caddy;

    for (itm = opal_list_get_first(&orte_job_states);
         itm != opal_list_get_end(&orte_job_states);
         itm = opal_list_get_next(itm)) {
        s = (orte_state_t*)itm;
        if (s->job_state == ORTE_JOB_STATE_ANY) {
            /* save this place */
            any = itm;
        }
        if (s->job_state == ORTE_JOB_STATE_ERROR) {
            error = itm;
        }
        if (s->job_state == state) {
            OPAL_OUTPUT_VERBOSE((1, orte_state_base_output,
                                 "%s ACTIVATING JOB %s STATE %s PRI %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (NULL == jdata) ? "NULL" : ORTE_JOBID_PRINT(jdata->jobid),
                                 orte_job_state_to_str(state), s->priority));
            if (NULL == s->cbfunc) {
                OPAL_OUTPUT_VERBOSE((1, orte_state_base_output,
                                     "%s NULL CBFUNC FOR JOB %s STATE %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     (NULL == jdata) ? "ALL" : ORTE_JOBID_PRINT(jdata->jobid),
                                     orte_job_state_to_str(state)));
                return;
            }
            caddy = OBJ_NEW(orte_state_caddy_t);
            if (NULL != jdata) {
                caddy->jdata = jdata;
                caddy->job_state = state;
                OBJ_RETAIN(jdata);
            }
            opal_event_set(orte_event_base, &caddy->ev, -1, OPAL_EV_WRITE, s->cbfunc, caddy);
            opal_event_set_priority(&caddy->ev, s->priority);
            opal_event_active(&caddy->ev, OPAL_EV_WRITE, 1);
            return;
        }
    }
    /* if we get here, then the state wasn't found, so execute
     * the default handler if it is defined
     */
    if (ORTE_JOB_STATE_ERROR < state && NULL != error) {
        s = (orte_state_t*)error;
    } else if (NULL != any) {
        s = (orte_state_t*)any;
    } else {
        OPAL_OUTPUT_VERBOSE((1, orte_state_base_output,
                             "ACTIVATE: ANY STATE NOT FOUND"));
        return;
    }
    if (NULL == s->cbfunc) {
        OPAL_OUTPUT_VERBOSE((1, orte_state_base_output,
                             "ACTIVATE: ANY STATE HANDLER NOT DEFINED"));
        return;
    }
    caddy = OBJ_NEW(orte_state_caddy_t);
    if (NULL != jdata) {
        caddy->jdata = jdata;
        caddy->job_state = state;
        OBJ_RETAIN(jdata);
    }
            OPAL_OUTPUT_VERBOSE((1, orte_state_base_output,
                                 "%s ACTIVATING JOB %s STATE %s PRI %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (NULL == jdata) ? "NULL" : ORTE_JOBID_PRINT(jdata->jobid),
                                 orte_job_state_to_str(state), s->priority));
    opal_event_set(orte_event_base, &caddy->ev, -1, OPAL_EV_WRITE, s->cbfunc, caddy);
    opal_event_set_priority(&caddy->ev, s->priority);
    opal_event_active(&caddy->ev, OPAL_EV_WRITE, 1);
}


int orte_state_base_add_job_state(orte_job_state_t state,
                                  orte_state_cbfunc_t cbfunc,
                                  int priority)
{
    opal_list_item_t *item;
    orte_state_t *st;

    /* check for uniqueness */
    for (item = opal_list_get_first(&orte_job_states);
         item != opal_list_get_end(&orte_job_states);
         item = opal_list_get_next(item)) {
        st = (orte_state_t*)item;
        if (st->job_state == state) {
            OPAL_OUTPUT_VERBOSE((1, orte_state_base_output,
                                 "DUPLICATE STATE DEFINED: %s",
                                 orte_job_state_to_str(state)));
            return ORTE_ERR_BAD_PARAM;
        }
    }

    st = OBJ_NEW(orte_state_t);
    st->job_state = state;
    st->cbfunc = cbfunc;
    st->priority = priority;
    opal_list_append(&orte_job_states, &(st->super));

    return ORTE_SUCCESS;
}

int orte_state_base_set_job_state_callback(orte_job_state_t state,
                                           orte_state_cbfunc_t cbfunc)
{
    opal_list_item_t *item;
    orte_state_t *st;

    for (item = opal_list_get_first(&orte_job_states);
         item != opal_list_get_end(&orte_job_states);
         item = opal_list_get_next(item)) {
        st = (orte_state_t*)item;
        if (st->job_state == state) {
            st->cbfunc = cbfunc;
            return ORTE_SUCCESS;
        }
    }
    return ORTE_ERR_NOT_FOUND;
}

int orte_state_base_set_job_state_priority(orte_job_state_t state,
                                           int priority)
{
    opal_list_item_t *item;
    orte_state_t *st;

    for (item = opal_list_get_first(&orte_job_states);
         item != opal_list_get_end(&orte_job_states);
         item = opal_list_get_next(item)) {
        st = (orte_state_t*)item;
        if (st->job_state == state) {
            st->priority = priority;
            return ORTE_SUCCESS;
        }
    }
    return ORTE_ERR_NOT_FOUND;
}

int orte_state_base_remove_job_state(orte_job_state_t state)
{
    opal_list_item_t *item;
    orte_state_t *st;

    for (item = opal_list_get_first(&orte_job_states);
         item != opal_list_get_end(&orte_job_states);
         item = opal_list_get_next(item)) {
        st = (orte_state_t*)item;
        if (st->job_state == state) {
            opal_list_remove_item(&orte_job_states, item);
            OBJ_RELEASE(item);
            return ORTE_SUCCESS;
        }
    }
    return ORTE_ERR_NOT_FOUND;
}

void orte_state_base_print_job_state_machine(void)
{
    opal_list_item_t *item;
    orte_state_t *st;

    opal_output(0, "ORTE_JOB_STATE_MACHINE:");
    for (item = opal_list_get_first(&orte_job_states);
         item != opal_list_get_end(&orte_job_states);
         item = opal_list_get_next(item)) {
        st = (orte_state_t*)item;
        opal_output(0, "\tState: %s cbfunc: %s",
                    orte_job_state_to_str(st->job_state),
                    (NULL == st->cbfunc) ? "NULL" : "DEFINED");
    }
}
#endif

/****    PROC STATE MACHINE    ****/
void orte_state_base_activate_proc_state(orte_process_name_t *proc,
                                         orte_proc_state_t state)
{
    opal_list_item_t *itm, *any=NULL, *error=NULL;
    orte_state_t *s;
    orte_state_caddy_t *caddy;

    for (itm = opal_list_get_first(&orte_proc_states);
         itm != opal_list_get_end(&orte_proc_states);
         itm = opal_list_get_next(itm)) {
        s = (orte_state_t*)itm;
        if (s->proc_state == ORTE_PROC_STATE_ANY) {
            /* save this place */
            any = itm;
        }
        if (s->proc_state == ORTE_PROC_STATE_ERROR) {
            error = itm;
        }
        if (s->proc_state == state) {
            OPAL_OUTPUT_VERBOSE((1, orte_state_base_output,
                                 "%s ACTIVATING PROC %s STATE %s PRI %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(proc),
                                 orte_proc_state_to_str(state), s->priority));
            if (NULL == s->cbfunc) {
                OPAL_OUTPUT_VERBOSE((1, orte_state_base_output,
                                     "%s NULL CBFUNC FOR PROC %s STATE %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(proc),
                                     orte_proc_state_to_str(state)));
                return;
            }
            caddy = OBJ_NEW(orte_state_caddy_t);
            caddy->name = *proc;
            caddy->proc_state = state;
            opal_event_set(orte_event_base, &caddy->ev, -1, OPAL_EV_WRITE, s->cbfunc, caddy);
            opal_event_set_priority(&caddy->ev, s->priority);
            opal_event_active(&caddy->ev, OPAL_EV_WRITE, 1);
            return;
        }
    }
    /* if we get here, then the state wasn't found, so execute
     * the default handler if it is defined
     */
    if (ORTE_PROC_STATE_ERROR < state && NULL != error) {
        s = (orte_state_t*)error;
    } else if (NULL != any) {
        s = (orte_state_t*)any;
    } else {
        OPAL_OUTPUT_VERBOSE((1, orte_state_base_output,
                             "INCREMENT: ANY STATE NOT FOUND"));
        return;
    }
    if (NULL == s->cbfunc) {
        OPAL_OUTPUT_VERBOSE((1, orte_state_base_output,
                             "ACTIVATE: ANY STATE HANDLER NOT DEFINED"));
        return;
    }
    caddy = OBJ_NEW(orte_state_caddy_t);
    caddy->name = *proc;
    caddy->proc_state = state;
            OPAL_OUTPUT_VERBOSE((1, orte_state_base_output,
                                 "%s ACTIVATING PROC %s STATE %s PRI %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(proc),
                                 orte_proc_state_to_str(state), s->priority));
    opal_event_set(orte_event_base, &caddy->ev, -1, OPAL_EV_WRITE, s->cbfunc, caddy);
    opal_event_set_priority(&caddy->ev, s->priority);
    opal_event_active(&caddy->ev, OPAL_EV_WRITE, 1);
}

int orte_state_base_add_proc_state(orte_proc_state_t state,
                                   orte_state_cbfunc_t cbfunc,
                                   int priority)
{
    opal_list_item_t *item;
    orte_state_t *st;

    /* check for uniqueness */
    for (item = opal_list_get_first(&orte_proc_states);
         item != opal_list_get_end(&orte_proc_states);
         item = opal_list_get_next(item)) {
        st = (orte_state_t*)item;
        if (st->proc_state == state) {
            OPAL_OUTPUT_VERBOSE((1, orte_state_base_output,
                                 "DUPLICATE STATE DEFINED: %s",
                                 orte_proc_state_to_str(state)));
            return ORTE_ERR_BAD_PARAM;
        }
    }

    st = OBJ_NEW(orte_state_t);
    st->proc_state = state;
    st->cbfunc = cbfunc;
    st->priority = priority;
    opal_list_append(&orte_proc_states, &(st->super));

    return ORTE_SUCCESS;
}

int orte_state_base_set_proc_state_callback(orte_proc_state_t state,
                                            orte_state_cbfunc_t cbfunc)
{
    opal_list_item_t *item;
    orte_state_t *st;

    for (item = opal_list_get_first(&orte_proc_states);
         item != opal_list_get_end(&orte_proc_states);
         item = opal_list_get_next(item)) {
        st = (orte_state_t*)item;
        if (st->proc_state == state) {
            st->cbfunc = cbfunc;
            return ORTE_SUCCESS;
        }
    }
    return ORTE_ERR_NOT_FOUND;
}

int orte_state_base_set_proc_state_priority(orte_proc_state_t state,
                                            int priority)
{
    opal_list_item_t *item;
    orte_state_t *st;

    for (item = opal_list_get_first(&orte_proc_states);
         item != opal_list_get_end(&orte_proc_states);
         item = opal_list_get_next(item)) {
        st = (orte_state_t*)item;
        if (st->proc_state == state) {
            st->priority = priority;
            return ORTE_SUCCESS;
        }
    }
    return ORTE_ERR_NOT_FOUND;
}

int orte_state_base_remove_proc_state(orte_proc_state_t state)
{
    opal_list_item_t *item;
    orte_state_t *st;

    for (item = opal_list_get_first(&orte_proc_states);
         item != opal_list_get_end(&orte_proc_states);
         item = opal_list_get_next(item)) {
        st = (orte_state_t*)item;
        if (st->proc_state == state) {
            opal_list_remove_item(&orte_proc_states, item);
            OBJ_RELEASE(item);
            return ORTE_SUCCESS;
        }
    }
    return ORTE_ERR_NOT_FOUND;
}

void orte_state_base_print_proc_state_machine(void)
{
    opal_list_item_t *item;
    orte_state_t *st;

    opal_output(0, "ORTE_PROC_STATE_MACHINE:");
    for (item = opal_list_get_first(&orte_proc_states);
         item != opal_list_get_end(&orte_proc_states);
         item = opal_list_get_next(item)) {
        st = (orte_state_t*)item;
        opal_output(0, "\tState: %s cbfunc: %s",
                    orte_proc_state_to_str(st->proc_state),
                    (NULL == st->cbfunc) ? "NULL" : "DEFINED");
    }
}

