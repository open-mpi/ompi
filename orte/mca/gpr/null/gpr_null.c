/* -*- C -*-
 *
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

#include "orte_config.h"

#include "orte/mca/gpr/gpr.h"
#include "orte/orte_constants.h"
#include "opal/util/output.h"
#include "orte/mca/gpr/base/base.h"


static int
orte_gpr_null_module_init(void)
{
  return ORTE_SUCCESS;
}


static int
orte_gpr_null_begin_compound_cmd(void)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_stop_compound_cmd(void)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_exec_compound_cmd(void)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_cleanup_job(orte_jobid_t jobid)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_cleanup_proc(orte_process_name_t * proc)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_preallocate_segment(char *name, orte_std_cntr_t num_slots)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_delete_segment(char *segment)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_delete_segment_nb(char *segment,
                       orte_gpr_notify_cb_fn_t cbfunc,
                       void *user_tag)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_put(orte_std_cntr_t cnt, orte_gpr_value_t ** values)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_put_nb(orte_std_cntr_t cnt, orte_gpr_value_t ** values,
                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_get(orte_gpr_addr_mode_t addr_mode,
             char *segment, char **tokens, char **keys,
             orte_std_cntr_t * cnt, orte_gpr_value_t *** values)
{
  return ORTE_ERR_NOT_IMPLEMENTED;
}

static int
orte_gpr_null_get_conditional(orte_gpr_addr_mode_t addr_mode,
             char *segment, char **tokens, char **keys,
             orte_std_cntr_t num_conditions, orte_gpr_keyval_t **conditions,
             orte_std_cntr_t * cnt, orte_gpr_value_t *** values)
{
  return ORTE_ERR_NOT_IMPLEMENTED;
}

static int
orte_gpr_null_get_nb(orte_gpr_addr_mode_t addr_mode,
                char *segment, char **tokens, char **keys,
                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
  return ORTE_ERR_NOT_IMPLEMENTED;
}

static int
orte_gpr_null_delete_entries(orte_gpr_addr_mode_t addr_mode,
                    char *segment, char **tokens,
                    char **keys)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_delete_entries_nb(orte_gpr_addr_mode_t addr_mode,
                       char *segment, char **tokens,
                       char **keys,
                       orte_gpr_notify_cb_fn_t cbfunc,
                       void *user_tag)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_index(char *segment, orte_std_cntr_t * cnt, char ***index)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_index_nb(char *segment,
                  orte_gpr_notify_cb_fn_t cbfunc,
                  void *user_tag)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_subscribe(orte_std_cntr_t num_subs,
                   orte_gpr_subscription_t ** subscriptions,
                   orte_std_cntr_t num_trigs,
                   orte_gpr_trigger_t ** triggers)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_unsubscribe(orte_gpr_subscription_id_t sub_number)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_cancel_trigger(orte_gpr_trigger_id_t trig_number)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_dump_all(void)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_dump_segments(char *segment)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_dump_triggers(orte_gpr_trigger_id_t start)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_dump_subscriptions(orte_gpr_subscription_id_t start)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_dump_local_triggers(void)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_dump_local_subscriptions(void)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_dump_callbacks(void)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_dump_notify_msg(orte_gpr_notify_message_t * msg)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_dump_notify_data(orte_gpr_notify_data_t * data)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_dump_value(orte_gpr_value_t * value)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_dump_segment_size(char *segment)
{
    return ORTE_SUCCESS;
}

static int
orte_gpr_null_increment_value(orte_gpr_value_t * value)
{
  return ORTE_SUCCESS;
}

static int
orte_gpr_null_decrement_value(orte_gpr_value_t * value)
{
  return ORTE_SUCCESS;
}

static int orte_gpr_null_put_1(orte_gpr_addr_mode_t addr_mode,
                               char *segment, char **tokens,
                               char *key, orte_data_value_t* value)
{
    return ORTE_SUCCESS;
}


static int orte_gpr_null_put_N(orte_gpr_addr_mode_t addr_mode,
                               char *segment, char **tokens,
                               orte_std_cntr_t n, char **keys,
                               orte_data_value_t **data_values)
{
    return ORTE_SUCCESS;
}

static int orte_gpr_null_subscribe_1(orte_gpr_subscription_id_t *id,
                                     char *trig_name,
                                     char *sub_name,
                                     orte_gpr_notify_action_t action,
                                     orte_gpr_addr_mode_t addr_mode,
                                     char *segment,
                                     char **tokens,
                                     char *key,
                                     orte_gpr_notify_cb_fn_t cbfunc,
                                     void *user_tag)
{
    return ORTE_SUCCESS;
}


static int orte_gpr_null_subscribe_N(orte_gpr_subscription_id_t *id,
                                     char *trig_name,
                                     char *sub_name,
                                     orte_gpr_notify_action_t action,
                                     orte_gpr_addr_mode_t addr_mode,
                                     char *segment,
                                     char **tokens,
                                     orte_std_cntr_t n,
                                     char **keys,
                                     orte_gpr_notify_cb_fn_t cbfunc,
                                     void *user_tag)
{
    return ORTE_SUCCESS;
}


static int orte_gpr_null_define_trigger(orte_gpr_trigger_id_t *id,
                                        char *trig_name,
                                        orte_gpr_trigger_action_t action,
                                        orte_gpr_addr_mode_t addr_mode,
                                        char *segment,
                                        char **tokens,
                                        orte_std_cntr_t n,
                                        char **keys,
                                        orte_gpr_trigger_cb_fn_t cbfunc,
                                        void *user_tag)
{
    return ORTE_SUCCESS;
}

static int orte_gpr_null_define_trigger_level(orte_gpr_trigger_id_t *id,
                                        char *trig_name,
                                        orte_gpr_trigger_action_t action,
                                        orte_gpr_addr_mode_t addr_mode,
                                        char *segment,
                                        char **tokens,
                                        orte_std_cntr_t n,
                                        char **keys,
                                        orte_std_cntr_t *levels,
                                        orte_gpr_trigger_cb_fn_t cbfunc,
                                        void *user_tag)
{
    return ORTE_SUCCESS;
}

static int orte_gpr_null_deliver_notify_msg(orte_gpr_notify_message_t *msg)
{
    return ORTE_SUCCESS;
}

static int orte_gpr_null_dump_a_trigger(
                            char *name,
                            orte_gpr_trigger_id_t id)
{
    return ORTE_SUCCESS;
}

static int orte_gpr_null_dump_a_subscription(
                            char *name,
                            orte_gpr_subscription_id_t id)
{
    return ORTE_SUCCESS;
}
/*
 * setup the function pointers for the module
 */
orte_gpr_base_module_t orte_gpr_null_module = {
    /* INIT */
    orte_gpr_null_module_init,
    /* BLOCKING OPERATIONS */
    orte_gpr_null_get,
    orte_gpr_null_get_conditional,
    orte_gpr_null_put,
    orte_gpr_null_put_1,
    orte_gpr_null_put_N,
    orte_gpr_null_delete_entries,
    orte_gpr_null_delete_segment,
    orte_gpr_null_index,
    /* NON-BLOCKING OPERATIONS */
    orte_gpr_null_get_nb,
    orte_gpr_null_put_nb,
    orte_gpr_null_delete_entries_nb,
    orte_gpr_null_delete_segment_nb,
    orte_gpr_null_index_nb,
    /* GENERAL OPERATIONS */
    orte_gpr_base_create_value,
    orte_gpr_base_create_keyval,
    orte_gpr_null_preallocate_segment,
    orte_gpr_null_deliver_notify_msg,
    /* ARITHMETIC OPERATIONS */
    orte_gpr_null_increment_value,
    orte_gpr_null_decrement_value,
    /* SUBSCRIBE OPERATIONS */
    orte_gpr_null_subscribe,
    orte_gpr_null_subscribe_1,
    orte_gpr_null_subscribe_N,
    orte_gpr_null_define_trigger,
    orte_gpr_null_define_trigger_level,
    orte_gpr_null_unsubscribe,
    orte_gpr_null_cancel_trigger,
    /* COMPOUND COMMANDS */
    orte_gpr_null_begin_compound_cmd,
    orte_gpr_null_stop_compound_cmd,
    orte_gpr_null_exec_compound_cmd,
    /* DIAGNOSTIC OPERATIONS */
    orte_gpr_null_dump_all,
    orte_gpr_null_dump_segments,
    orte_gpr_null_dump_triggers,
    orte_gpr_null_dump_subscriptions,
    orte_gpr_null_dump_a_trigger,
    orte_gpr_null_dump_a_subscription,
    orte_gpr_null_dump_local_triggers,
    orte_gpr_null_dump_local_subscriptions,
    orte_gpr_null_dump_callbacks,
    orte_gpr_null_dump_notify_msg,
    orte_gpr_null_dump_notify_data,
    orte_gpr_null_dump_value,
    orte_gpr_null_dump_segment_size,
    /* CLEANUP OPERATIONS */
    orte_gpr_null_cleanup_job,
    orte_gpr_null_cleanup_proc
};
