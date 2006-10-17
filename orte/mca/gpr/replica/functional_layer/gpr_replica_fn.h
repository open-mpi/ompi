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
 * The Open MPI general purpose registry - support functions.
 *
 */

#ifndef MCA_GPR_REPLICA_FN_H_
#define MCA_GPR_REPLICA_FN_H_

#include "orte_config.h"

#include "orte/mca/ns/ns_types.h"

#include "orte/mca/gpr/replica/gpr_replica.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * The "fn" layer of the registry API functions - not accessible from outside
 * the replica
 */

 /*
  * Arithemetic operations
  */
int orte_gpr_replica_increment_value_fn(orte_gpr_addr_mode_t addr_mode,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t *itags,
                                orte_std_cntr_t num_tokens, orte_std_cntr_t cnt,
                                orte_gpr_keyval_t **keyvals);

int orte_gpr_replica_decrement_value_fn(orte_gpr_addr_mode_t addr_mode,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t *itags,
                                orte_std_cntr_t num_tokens, orte_std_cntr_t cnt,
                                orte_gpr_keyval_t **keyvals);

/*
 * Delete-index functions
 */
int orte_gpr_replica_delete_entries_fn(orte_gpr_addr_mode_t mode,
                    orte_gpr_replica_segment_t *seg,
                    orte_gpr_replica_itag_t *token_itags, orte_std_cntr_t num_tokens,
                    orte_gpr_replica_itag_t *key_tags, orte_std_cntr_t num_keys);

int orte_gpr_replica_delete_entries_nb_fn(
                    orte_gpr_addr_mode_t addr_mode,
                    orte_gpr_replica_segment_t *seg,
                    orte_gpr_replica_itag_t *token_itags, orte_std_cntr_t num_tokens,
                    orte_gpr_replica_itag_t *key_tags, orte_std_cntr_t num_keys);

int orte_gpr_replica_index_fn(orte_gpr_replica_segment_t *seg,
                            orte_std_cntr_t *cnt, char ***index);

int orte_gpr_replica_index_nb_fn(orte_gpr_replica_segment_t *seg,
                        orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);


/*
 * Cleanup functions
 */
int orte_gpr_replica_cleanup_job_fn(orte_jobid_t jobid);

int orte_gpr_replica_cleanup_proc_fn(orte_process_name_t *proc);


/*
 * Put-get functions
 */
int orte_gpr_replica_put_fn(orte_gpr_addr_mode_t addr_mode,
                orte_gpr_replica_segment_t *seg,
                orte_gpr_replica_itag_t *token_itags, orte_std_cntr_t num_tokens,
                orte_std_cntr_t cnt, orte_gpr_keyval_t **keyvals);

int orte_gpr_replica_put_nb_fn(orte_gpr_addr_mode_t addr_mode,
                orte_gpr_replica_segment_t *seg,
                orte_gpr_replica_itag_t *token_itags, orte_std_cntr_t num_tokens,
                orte_std_cntr_t cnt, orte_gpr_keyval_t **keyvals,
                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);

int orte_gpr_replica_get_fn(orte_gpr_addr_mode_t addr_mode,
                            orte_gpr_replica_segment_t *seg,
                            orte_gpr_replica_itag_t *tokentags, orte_std_cntr_t num_tokens,
                            orte_gpr_replica_itag_t *keytags, orte_std_cntr_t num_keys,
                            orte_std_cntr_t *cnt, orte_gpr_value_t ***values);

int orte_gpr_replica_get_conditional_fn(orte_gpr_addr_mode_t addr_mode,
                            orte_gpr_replica_segment_t *seg,
                            orte_gpr_replica_itag_t *tokentags, orte_std_cntr_t num_tokens,
                            orte_gpr_replica_itag_t *keytags, orte_std_cntr_t num_keys,
                            orte_std_cntr_t num_conditions, orte_gpr_replica_itagval_t **conditions,
                            orte_std_cntr_t *cnt, orte_gpr_value_t ***values);

int orte_gpr_replica_get_nb_fn(orte_gpr_addr_mode_t addr_mode,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t *tokentags, orte_std_cntr_t num_tokens,
                                orte_gpr_replica_itag_t *keytags, orte_std_cntr_t num_keys,
                                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);


/*
 * Subscribe functions
 */
int orte_gpr_replica_subscribe_fn(orte_process_name_t *requestor,
                                  orte_std_cntr_t num_subs,
                                  orte_gpr_subscription_t **subscriptions,
                                  orte_std_cntr_t num_trigs,
                                  orte_gpr_trigger_t **trigs);

/*
 * Diagnostic functions
 */
int orte_gpr_replica_dump_all_fn(orte_buffer_t *buffer);

int orte_gpr_replica_dump_segments_fn(orte_buffer_t *buffer, char *segment);

int orte_gpr_replica_dump_a_segment_fn(orte_buffer_t *buffer, orte_gpr_replica_segment_t *seg);

int orte_gpr_replica_dump_triggers_fn(orte_buffer_t *buffer,
                orte_gpr_trigger_id_t start);

int orte_gpr_replica_dump_subscriptions_fn(orte_buffer_t *buffer,
                orte_gpr_subscription_id_t start);

int orte_gpr_replica_dump_trigger(orte_buffer_t *buffer,
                                  orte_gpr_replica_trigger_t *trig);

int orte_gpr_replica_dump_subscription(orte_buffer_t *buffer,
                                       orte_gpr_replica_subscription_t *sub);

int orte_gpr_replica_dump_callbacks_fn(orte_buffer_t *buffer);

int orte_gpr_replica_dump_segment_size_fn(orte_buffer_t *buffer, char *segment);

/*
 * *********    INTERNAL UTILITY FUNCTIONS     **********
 */

/** SEGMENT OPERATIONS
 */
int orte_gpr_replica_release_segment(orte_gpr_replica_segment_t **seg);

int orte_gpr_replica_find_containers(orte_gpr_replica_segment_t *seg,
                                     orte_gpr_replica_addr_mode_t addr_mode,
                                     orte_gpr_replica_itag_t *taglist, orte_std_cntr_t num_tags);

int orte_gpr_replica_create_container(orte_gpr_replica_container_t **cptr,
                                      orte_gpr_replica_segment_t *seg,
                                      orte_std_cntr_t num_itags,
                                      orte_gpr_replica_itag_t *itags);

int orte_gpr_replica_release_container(orte_gpr_replica_segment_t *seg,
                                       orte_gpr_replica_container_t *cptr);

int orte_gpr_replica_add_keyval(orte_gpr_replica_itagval_t **ivalptr,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_container_t *cptr,
                                orte_gpr_keyval_t *kptr);

int orte_gpr_replica_update_keyval(orte_gpr_replica_itagval_t **iptr,
                                   orte_gpr_replica_segment_t *seg,
                                   orte_gpr_replica_container_t *cptr,
                                   orte_gpr_keyval_t *kptr);


int orte_gpr_replica_purge_itag(orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t itag);

int orte_gpr_replica_search_container(orte_gpr_replica_addr_mode_t addr_mode,
                                      orte_gpr_replica_itag_t *itags, orte_std_cntr_t num_itags,
                                      orte_gpr_replica_container_t *cptr);

bool orte_gpr_replica_value_in_container(orte_gpr_replica_container_t *cptr,
                                      orte_gpr_replica_itagval_t *iptr);

int orte_gpr_replica_delete_itagval(orte_gpr_replica_segment_t *seg,
                                   orte_gpr_replica_container_t *cptr,
                                   orte_gpr_replica_itagval_t *iptr);

/*
 * DICTIONARY OPERATIONS
 */

bool orte_gpr_replica_check_itag_list(orte_gpr_replica_addr_mode_t mode,
                    orte_std_cntr_t num_itags_search,
                    orte_gpr_replica_itag_t *itags,
                    orte_std_cntr_t num_itags_entry,
                    orte_gpr_replica_itag_t *entry_itags);

int orte_gpr_replica_copy_itag_list(orte_gpr_replica_itag_t **dest,
                                    orte_gpr_replica_itag_t *src, orte_std_cntr_t num_itags);

void orte_gpr_replica_dump_itagval_value(orte_buffer_t *buffer,
                                         orte_gpr_replica_itagval_t *iptr);

/*
 * Trigger Operations
 */
int orte_gpr_replica_enter_local_subscription(orte_std_cntr_t cnt, orte_gpr_subscription_t **subscriptions);

int orte_gpr_replica_enter_local_trigger(orte_std_cntr_t cnt, orte_gpr_trigger_t **trigs);

int orte_gpr_replica_remove_local_subscription(orte_gpr_replica_local_subscriber_t *sub);

int orte_gpr_replica_remove_local_trigger(orte_gpr_replica_local_trigger_t *trig);

int orte_gpr_replica_record_action(orte_gpr_replica_segment_t *seg,
                                   orte_gpr_replica_container_t *cptr,
                                   orte_gpr_replica_itagval_t *iptr,
                                   orte_gpr_replica_action_t action);

int orte_gpr_replica_check_events(void);

int orte_gpr_replica_check_subscription(orte_gpr_replica_subscription_t *sub);

bool orte_gpr_replica_check_notify_matches(orte_gpr_addr_mode_t *addr_mode,
                                           orte_gpr_replica_subscription_t *sub,
                                           orte_gpr_replica_action_taken_t *ptr);

int orte_gpr_replica_check_trig(orte_gpr_replica_trigger_t *trig);

int orte_gpr_replica_update_storage_locations(orte_gpr_replica_itagval_t *new_iptr);

int orte_gpr_replica_construct_notify_message(orte_gpr_notify_message_t *msg,
                                              orte_gpr_replica_trigger_t *trig,
                                              orte_gpr_replica_subscription_t *sub,
                                              orte_gpr_value_t *value);

int
orte_gpr_replica_register_subscription(orte_gpr_replica_subscription_t **subptr,
                                       orte_process_name_t *requestor,
                                       orte_gpr_subscription_t *subscription);

int
orte_gpr_replica_register_trigger(orte_gpr_replica_trigger_t **trigptr,
                                  orte_process_name_t *requestor,
                                  orte_gpr_trigger_t *trigger);

int
orte_gpr_replica_remove_subscription(orte_process_name_t *requestor,
                                     orte_gpr_subscription_id_t id);

int
orte_gpr_replica_remove_trigger(orte_process_name_t *requestor,
                                     orte_gpr_trigger_id_t id);

int orte_gpr_replica_register_callback(orte_gpr_replica_subscription_t *sub,
                                       orte_gpr_value_t *value);

int orte_gpr_replica_register_trigger_callback(orte_gpr_replica_trigger_t *trig);

int orte_gpr_replica_define_callback(orte_gpr_notify_msg_type_t msg_type,
                                     orte_gpr_replica_callbacks_t **cbptr,
                                     orte_process_name_t *recipient);

int orte_gpr_replica_process_callbacks(void);

int orte_gpr_replica_purge_subscriptions(orte_process_name_t *proc);

int orte_gpr_replica_store_value_in_msg(orte_gpr_replica_requestor_t *req,
                                        orte_gpr_notify_message_t *msg,
                                        char *sub_name,
                                        orte_std_cntr_t cnt,
                                        orte_gpr_value_t **values);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
