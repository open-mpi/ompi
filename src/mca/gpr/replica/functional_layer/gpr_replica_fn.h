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
 * The Open MPI general purpose registry - support functions.
 *
 */

#ifndef MCA_GPR_REPLICA_FN_H_
#define MCA_GPR_REPLICA_FN_H_

#include "orte_config.h"

#include "mca/ns/ns_types.h"

#include "mca/gpr/replica/gpr_replica.h"

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
                                int num_tokens, int cnt,
                                orte_gpr_keyval_t **keyvals);

int orte_gpr_replica_decrement_value_fn(orte_gpr_addr_mode_t addr_mode,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t *itags,
                                int num_tokens, int cnt,
                                orte_gpr_keyval_t **keyvals);

/*
 * Delete-index functions
 */
int orte_gpr_replica_delete_entries_fn(orte_gpr_addr_mode_t mode,
                    orte_gpr_replica_segment_t *seg,
                    orte_gpr_replica_itag_t *token_itags, int num_tokens,
                    orte_gpr_replica_itag_t *key_tags, int num_keys);

int orte_gpr_replica_delete_entries_nb_fn(
                    orte_gpr_addr_mode_t addr_mode,
                    orte_gpr_replica_segment_t *seg,
                    orte_gpr_replica_itag_t *token_itags, int num_tokens,
                    orte_gpr_replica_itag_t *key_tags, int num_keys);
                            
int orte_gpr_replica_index_fn(orte_gpr_replica_segment_t *seg,
                            size_t *cnt, char **index);

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
                orte_gpr_replica_itag_t *token_itags, int num_tokens,
                int cnt, orte_gpr_keyval_t **keyvals,
                int8_t *action_taken);

int orte_gpr_replica_put_nb_fn(orte_gpr_addr_mode_t addr_mode,
                orte_gpr_replica_segment_t *seg,
                orte_gpr_replica_itag_t *token_itags, int num_tokens,
                int cnt, orte_gpr_keyval_t **keyvals,
                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);
                      
int orte_gpr_replica_get_fn(orte_gpr_addr_mode_t addr_mode,
                            orte_gpr_replica_segment_t *seg,
                            orte_gpr_replica_itag_t *tokentags, int num_tokens,
                            orte_gpr_replica_itag_t *keytags, int num_keys,
                            int *cnt, orte_gpr_value_t ***values);

int orte_gpr_replica_get_nb_fn(orte_gpr_addr_mode_t addr_mode,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t *tokentags, int num_tokens,
                                orte_gpr_replica_itag_t *keytags, int num_keys,
                                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);


/*
 * Subscribe functions
 */
int orte_gpr_replica_subscribe_fn(orte_gpr_notify_action_t action, int num_subs,
                                  orte_gpr_subscription_t **subscriptions,
                                  int num_trigs,
                                  orte_gpr_value_t **trigs,
                                  orte_gpr_notify_id_t idtag);

int orte_gpr_replica_unsubscribe_fn(orte_gpr_notify_id_t sub_number);


/*
 * Diagnostic functions
 */
int orte_gpr_replica_dump_all_fn(orte_buffer_t *buffer);

int orte_gpr_replica_dump_segments_fn(orte_buffer_t *buffer);

int orte_gpr_replica_dump_triggers_fn(orte_buffer_t *buffer);


/*
 * *********    INTERNAL UTILITY FUNCTIONS     **********
 */

/** SEGMENT OPERATIONS
 */
int orte_gpr_replica_release_segment(orte_gpr_replica_segment_t **seg);

int orte_gpr_replica_find_containers(int *num_found, orte_gpr_replica_segment_t *seg,
                                     orte_gpr_replica_addr_mode_t addr_mode,
                                     orte_gpr_replica_itag_t *taglist, int num_tags);

int orte_gpr_replica_create_container(orte_gpr_replica_container_t **cptr,
                                      orte_gpr_replica_segment_t *seg,
                                      int num_itags,
                                      orte_gpr_replica_itag_t *itags);

int orte_gpr_replica_release_container(orte_gpr_replica_segment_t *seg,
                                       orte_gpr_replica_container_t *cptr);

int orte_gpr_replica_add_keyval(orte_gpr_replica_itagval_t **ivalptr,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_container_t *cptr,
                                orte_gpr_keyval_t *kptr);

int orte_gpr_replica_update_keyval(orte_gpr_replica_segment_t *seg,
                                   orte_gpr_replica_container_t *cptr,
                                   orte_gpr_keyval_t *kptr);

int orte_gpr_replica_xfer_payload(orte_gpr_value_union_t *dest,
                                  orte_gpr_value_union_t *src,
                                  orte_data_type_t type);

int orte_gpr_replica_purge_itag(orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t itag);

int orte_gpr_replica_search_container(int *cnt, orte_gpr_replica_addr_mode_t addr_mode,
                                      orte_gpr_replica_itag_t *itags, int num_itags,
                                      orte_gpr_replica_container_t *cptr);

int orte_gpr_replica_get_value(void *value, orte_gpr_replica_itagval_t *ival);

int orte_gpr_replica_delete_itagval(orte_gpr_replica_segment_t *seg,
                                   orte_gpr_replica_container_t *cptr,
                                   orte_gpr_replica_itagval_t *iptr);

/*
 * DICTIONARY OPERATIONS
 */
 
bool orte_gpr_replica_check_itag_list(orte_gpr_replica_addr_mode_t mode,
				    int num_itags_search,
				    orte_gpr_replica_itag_t *itags,
				    int num_itags_entry,
				    orte_gpr_replica_itag_t *entry_itags);

int orte_gpr_replica_copy_itag_list(orte_gpr_replica_itag_t **dest,
                                    orte_gpr_replica_itag_t *src, int num_itags);

void orte_gpr_replica_dump_itagval_value(orte_buffer_t *buffer,
                                         orte_gpr_replica_itagval_t *iptr);

/*
 * Trigger Operations
 */
int orte_gpr_replica_check_subscriptions(orte_gpr_replica_segment_t *seg, int8_t action_taken);

int orte_gpr_replica_check_trig(orte_gpr_replica_triggers_t *trig);

int orte_gpr_replica_update_storage_locations(orte_gpr_replica_itagval_t *new_iptr);

int orte_gpr_replica_construct_notify_message(orte_gpr_notify_message_t **msg,
                                              orte_gpr_replica_triggers_t *trig);

int
orte_gpr_replica_enter_notify_request(orte_gpr_notify_id_t *local_idtag,
                                      orte_process_name_t *requestor,
                                      orte_gpr_notify_id_t remote_idtag,
                                      int cnt, orte_gpr_subscription_t **subscriptions);

int
orte_gpr_replica_remove_notify_request(orte_gpr_notify_id_t local_idtag,
                                       orte_gpr_notify_id_t *remote_idtag);

int orte_gpr_replica_register_callback(orte_gpr_replica_triggers_t *trig);

int orte_gpr_replica_process_callbacks(void);

int orte_gpr_replica_purge_subscriptions(orte_process_name_t *proc);

int orte_gpr_replica_add_values(orte_gpr_notify_data_t **data,
                                orte_gpr_replica_subscribed_data_t *sptr);


#endif
