/* -*- C -*-
 * 
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
 *
 */
/** @file 
 */

#ifndef ORTE_GPR_REPLICA_API_H
#define ORTE_GPR_REPLICA_API_H


#include "orte_config.h"

#include <time.h>

#include "class/orte_pointer_array.h"

#include "threads/mutex.h"
#include "threads/condition.h"

#include "mca/ns/ns_types.h"

#include "mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"

#include "mca/gpr/replica/gpr_replica.h"

/*
 * Implemented registry functions - see gpr.h for documentation
 */

/*
 * Compound cmd functions
 */
int orte_gpr_replica_begin_compound_cmd(void);

int orte_gpr_replica_stop_compound_cmd(void);

int orte_gpr_replica_exec_compound_cmd(void);

/*
 * Arithmetic operations
 */
int orte_gpr_replica_increment_value(orte_gpr_value_t *value);

int orte_gpr_replica_decrement_value(orte_gpr_value_t *value);

/*
 * Delete-index functions
 */
int orte_gpr_replica_delete_segment(char *segment);

int orte_gpr_replica_delete_segment_nb(char *segment,
                                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);

int orte_gpr_replica_delete_entries(orte_gpr_addr_mode_t mode,
                char *segment, char **tokens, char **keys);

int orte_gpr_replica_delete_entries_nb(
                            orte_gpr_addr_mode_t addr_mode,
                            char *segment, char **tokens, char **keys,
                            orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);
                            
int orte_gpr_replica_index(char *segment, size_t *cnt, char **index);

int orte_gpr_replica_index_nb(char *segment,
                        orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);


/*
 * Cleanup functions
 */
int orte_gpr_replica_cleanup_job(orte_jobid_t jobid);

int orte_gpr_replica_cleanup_proc(orte_process_name_t *proc);


/*
 * Put-get functions
 */
int orte_gpr_replica_put(int cnt, orte_gpr_value_t **values);

int orte_gpr_replica_put_nb(int cnt, orte_gpr_value_t **values,
                            orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);
                      
int orte_gpr_replica_get(orte_gpr_addr_mode_t addr_mode,
                                char *segment, char **tokens, char **keys,
                                int *cnt, orte_gpr_value_t ***values);

int orte_gpr_replica_get_nb(orte_gpr_addr_mode_t addr_mode,
                                char *segment, char **tokens, char **keys,
                                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);


/*
 * Subscribe functions
 */
int orte_gpr_replica_subscribe(orte_gpr_notify_action_t action,
                               int num_subs,
                               orte_gpr_subscription_t **subscriptions,
                               int num_trigs,
                               orte_gpr_value_t **trigs,
                               orte_gpr_notify_id_t *sub_number);

int orte_gpr_replica_unsubscribe(orte_gpr_notify_id_t sub_number);


/*
 * Diagnostic functions
 */
int orte_gpr_replica_dump_all(int output_id);

int orte_gpr_replica_dump_segments(int output_id);

int orte_gpr_replica_dump_triggers(int output_id);


/*
 * General functions
 */
int orte_gpr_replica_preallocate_segment(char *name, int num_slots);

int orte_gpr_replica_deliver_notify_msg(orte_gpr_notify_message_t *msg);


#endif
