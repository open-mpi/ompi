/* -*- C -*-
 *
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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#ifndef ORTE_GPR_PROXY_H
#define ORTE_GPR_PROXY_H


#include "orte_config.h"

#include "orte/orte_types.h"
#include "opal/class/opal_object.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "orte/class/orte_pointer_array.h"
#include "orte/dss/dss_types.h"
#include "orte/util/proc_info.h"

#include "orte/mca/gpr/base/base.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close
 */
int orte_gpr_proxy_open(void);
int orte_gpr_proxy_close(void);


/*
 * Startup / Shutdown
 */
orte_gpr_base_module_t*
orte_gpr_proxy_component_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority);
int orte_gpr_proxy_module_init(void);

int orte_gpr_proxy_finalize(void);

/*
 * proxy-local types
 */
typedef struct {
     opal_object_t super;                   /**< Allows this to be an object */
     orte_gpr_subscription_id_t id;         /**< id of this subscription */
     orte_std_cntr_t index;                          /**< location of this subscription in array */
     char *name;
     orte_gpr_notify_cb_fn_t callback;      /**< Function to be called for notificaiton */
     void *user_tag;                        /**< User-provided tag for callback function */
} orte_gpr_proxy_subscriber_t;

OBJ_CLASS_DECLARATION(orte_gpr_proxy_subscriber_t);


typedef struct {
     opal_object_t super;                   /**< Allows this to be an object */
     orte_gpr_trigger_id_t id;              /**< id of this trigger */
     orte_std_cntr_t index;                          /**< location of this trigger in array */
     char *name;
     orte_gpr_trigger_cb_fn_t callback;     /**< Function to be called for notification */
     void *user_tag;                        /**< User-provided tag for callback function */
} orte_gpr_proxy_trigger_t;

OBJ_CLASS_DECLARATION(orte_gpr_proxy_trigger_t);


/*
 * globals used within proxy component
 */
typedef struct {
    int debug;
    orte_gpr_subscription_id_t num_subs;
    orte_pointer_array_t *subscriptions;
    orte_gpr_trigger_id_t num_trigs;
    orte_pointer_array_t *triggers;
    opal_mutex_t mutex;
    bool compound_cmd_mode;
    orte_buffer_t *compound_cmd;
    opal_mutex_t wait_for_compound_mutex;
    opal_condition_t compound_cmd_condition;
    int compound_cmd_waiting;
    bool timing;
} orte_gpr_proxy_globals_t;

extern orte_gpr_proxy_globals_t orte_gpr_proxy_globals;

/*
 * Compound cmd functions
 */
int orte_gpr_proxy_begin_compound_cmd(void);

int orte_gpr_proxy_stop_compound_cmd(void);

int orte_gpr_proxy_exec_compound_cmd(void);

/*
 * Arithmetic operations
 */
int orte_gpr_proxy_increment_value(orte_gpr_value_t *value);

int orte_gpr_proxy_decrement_value(orte_gpr_value_t *value);

/*
 * Delete-index functions
 */
int orte_gpr_proxy_delete_segment(char *segment);

int orte_gpr_proxy_delete_segment_nb(char *segment,
                                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);

int orte_gpr_proxy_delete_entries(orte_gpr_addr_mode_t mode,
                char *segment, char **tokens, char **keys);

int orte_gpr_proxy_delete_entries_nb(
                            orte_gpr_addr_mode_t addr_mode,
                            char *segment, char **tokens, char **keys,
                            orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);

int orte_gpr_proxy_index(char *segment, orte_std_cntr_t *cnt, char ***index);

int orte_gpr_proxy_index_nb(char *segment,
                        orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);


/*
 * Cleanup functions
 */
int orte_gpr_proxy_cleanup_job(orte_jobid_t jobid);

int orte_gpr_proxy_cleanup_proc(orte_process_name_t *proc);


/*
 * Put-get functions
 */
int orte_gpr_proxy_put(orte_std_cntr_t cnt, orte_gpr_value_t **values);

int orte_gpr_proxy_put_nb(orte_std_cntr_t cnt, orte_gpr_value_t **values,
                          orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);

int orte_gpr_proxy_get(orte_gpr_addr_mode_t addr_mode,
                       char *segment, char **tokens, char **keys,
                       orte_std_cntr_t *cnt, orte_gpr_value_t ***values);

int orte_gpr_proxy_get_conditional(orte_gpr_addr_mode_t addr_mode,
                                   char *segment, char **tokens, char **keys,
                                   orte_std_cntr_t num_conditions, orte_gpr_keyval_t **conditions,
                                   orte_std_cntr_t *cnt, orte_gpr_value_t ***values);

int orte_gpr_proxy_get_nb(orte_gpr_addr_mode_t addr_mode,
                          char *segment, char **tokens, char **keys,
                          orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);


/*
 * Subscribe functions
 */
int orte_gpr_proxy_subscribe(orte_std_cntr_t num_subs,
                             orte_gpr_subscription_t **subscriptions,
                             orte_std_cntr_t num_trigs,
                             orte_gpr_trigger_t **trigs);

int orte_gpr_proxy_unsubscribe(orte_gpr_subscription_id_t sub_number);

int orte_gpr_proxy_cancel_trigger(orte_gpr_trigger_id_t trig);


/*
 * Diagnostic functions
 */
int orte_gpr_proxy_dump_all(void);

int orte_gpr_proxy_dump_segments(char *segment);

int orte_gpr_proxy_dump_triggers(orte_gpr_trigger_id_t start);

int orte_gpr_proxy_dump_subscriptions(orte_gpr_subscription_id_t start);

int orte_gpr_proxy_dump_a_trigger(char *name,
                            orte_gpr_trigger_id_t id);

int orte_gpr_proxy_dump_a_subscription(char *name,
                            orte_gpr_subscription_id_t id);

int orte_gpr_proxy_dump_local_triggers(void);

int orte_gpr_proxy_dump_local_subscriptions(void);

int orte_gpr_proxy_dump_callbacks(void);

int orte_gpr_proxy_dump_notify_msg(orte_gpr_notify_message_t *msg);

int orte_gpr_proxy_dump_notify_data(orte_gpr_notify_data_t *data);

int orte_gpr_proxy_dump_value(orte_gpr_value_t *value);

int orte_gpr_proxy_dump_segment_size(char *segment);

/*
 * General operations
 */
int orte_gpr_proxy_preallocate_segment(char *name, orte_std_cntr_t num_slots);

int orte_gpr_proxy_deliver_notify_msg(orte_gpr_notify_message_t *msg);

/*
 * Functions that interface to the replica
 */
void orte_gpr_proxy_notify_recv(int status, orte_process_name_t* sender,
                   orte_buffer_t *buffer, orte_rml_tag_t tag,
                   void* cbdata);


/*
 * Internal functions
 */

int
orte_gpr_proxy_enter_subscription(orte_std_cntr_t cnt, orte_gpr_subscription_t **subscriptions);

int
orte_gpr_proxy_remove_subscription(orte_gpr_proxy_subscriber_t *sub);

int
orte_gpr_proxy_enter_trigger(orte_std_cntr_t cnt, orte_gpr_trigger_t **triggers);


int
orte_gpr_proxy_remove_trigger(orte_gpr_proxy_trigger_t *trig);

/*
 *
 */
ORTE_MODULE_DECLSPEC extern mca_gpr_base_component_t mca_gpr_proxy_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
