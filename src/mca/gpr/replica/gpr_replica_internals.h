/*
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry - support functions.
 *
 */

#ifndef MCA_GPR_REPLICA_INTERNALS_H_
#define MCA_GPR_REPLICA_INTERNALS_H_

/** Retrieve a registry key value for a given token string.
 * The ompi_registry_getkey() function is used to translate a token string for a particular
 * segment of the registry into its associated (integer) key value.
 *
 * @param seg Pointer to the segment of the registry being queried.
 * @param token Pointer to a character string containing the token to be translated. If token=NULL,
 * the function returns the key value corresponding to the specified segment itself.
 *
 * @retval key Unsigned long integer value corresponding to the specified token within the specified segment.
 * @retval -1 Indicates that the segment and/or token could not be found.
 */
mca_gpr_replica_key_t mca_gpr_replica_get_key(mca_gpr_replica_segment_t *seg, char *token);

/** Add a token to a segment's dictionary.
 * The gpr_replica_define_key() function allows the addition of a new definition to
 * the registry's token-key dictionaries. The specified token is assigned an integer
 * value within the specified segment, and the entry is added to the segment's token-key
 * dictionary.
 *
 * @param segment Pointer to a character string defining the segment of the registry being queried.
 * @param token Pointer to a character string containing the token to be defined. If segment=NULL,
 * the function adds the token to the segment dictionary, thus defining a new segment name.
 *
 * @retval key New key value
 * @retval MCA_GPR_REPLICA_KEY_MAX Indicates that the dictionary is full or some other error.
 */
mca_gpr_replica_key_t mca_gpr_replica_define_key(mca_gpr_replica_segment_t *seg, char *token);

/** Delete a token from a segment's dictionary.
 * The gpr_replica_deletekey() function allows the removal of a definition from the
 * registry's token-key dictionaries. This should be used with caution! Deletion of
 * a token-key pair causes the registry to search through all entries within that segment
 * for objects that include the specified token-key pair in their description. The reference
 * is subsequently removed, and any object for which this was the SOLE key will also
 * be removed from the registry!
 *
 * @param segment Pointer to a character string defining the segment of the registry.
 * @param token Pointer to a character string containing the token to be deleted. If token=NULL,
 * the function deletes the specified segment name from the segment dictionary.
 *
 * @retval OMPI_SUCCESS Indicating that the operation was successful.
 * @retval OMPI_ERROR Indicates that the operation failed - most likely caused by specifying
 * a token that did not exist within the specified segment, or a non-existent segment.
 */
int mca_gpr_replica_delete_key(mca_gpr_replica_segment_t *seg, char *token);

/** Find a requested registry segment.
 * The gpr_replica_findseq() function finds the registry segment corresponding to
 * the specified name.
 *
 * @param segment Pointer to a string containing the name of the segment to be found.
 *
 * @retval *seg Pointer to the segment
 * @retval NULL Indicates that the specified segment could not be found
 */
mca_gpr_replica_segment_t *mca_gpr_replica_find_seg(bool create, char *segment,
						    mca_ns_base_jobid_t jobid);

mca_gpr_replica_keytable_t
*mca_gpr_replica_find_dict_entry(mca_gpr_replica_segment_t *seg, char *token);

int mca_gpr_replica_empty_segment(mca_gpr_replica_segment_t *seg);

mca_gpr_replica_key_t
*mca_gpr_replica_get_key_list(mca_gpr_replica_segment_t *seg, char **tokens,
			      int *num_tokens);

bool mca_gpr_replica_check_key_list(ompi_registry_mode_t mode,
				    mca_gpr_replica_key_t num_keys_search,
				    mca_gpr_replica_key_t *keys,
				    mca_gpr_replica_key_t num_keys_entry,
				    mca_gpr_replica_key_t *entry_keys);

mca_gpr_replica_segment_t *mca_gpr_replica_define_segment(char *segment,
							  mca_ns_base_jobid_t jobid);

mca_gpr_replica_trigger_list_t
*mca_gpr_replica_construct_trigger(ompi_registry_synchro_mode_t synchro_mode,
				   ompi_registry_notify_action_t action,
				   ompi_registry_mode_t addr_mode,
				   mca_gpr_replica_segment_t *seg,
				   mca_gpr_replica_key_t *keys,
				   int num_keys,
				   int trigger,
				   ompi_registry_notify_id_t id_tag);

ompi_registry_notify_message_t
*mca_gpr_replica_construct_notify_message(mca_gpr_replica_segment_t *seg,
					  mca_gpr_replica_trigger_list_t *trig);

bool mca_gpr_replica_process_triggers(mca_gpr_replica_segment_t *seg,
				      mca_gpr_replica_trigger_list_t *trig,
				      ompi_registry_notify_message_t *message);

ompi_registry_notify_id_t
mca_gpr_replica_remove_trigger(ompi_registry_notify_id_t idtag);

char *mca_gpr_replica_get_token(mca_gpr_replica_segment_t *seg, mca_gpr_replica_key_t key);

ompi_registry_notify_id_t
mca_gpr_replica_enter_notify_request(mca_gpr_replica_segment_t *seg,
				     ompi_registry_notify_action_t action,
				     ompi_process_name_t *requestor,
				     ompi_registry_notify_id_t idtag,
				     ompi_registry_notify_cb_fn_t cb_func,
				     void *user_tag);

ompi_registry_notify_id_t
mca_gpr_replica_remove_notify_request(ompi_registry_notify_id_t idtag);

void mca_gpr_replica_process_callbacks(void);

int mca_gpr_replica_check_synchros(mca_gpr_replica_segment_t *seg);

void mca_gpr_replica_check_subscriptions(mca_gpr_replica_segment_t *seg, int8_t action_taken);

int mca_gpr_replica_purge_subscriptions(ompi_process_name_t *proc);

ompi_buffer_t
mca_gpr_replica_process_command_buffer(ompi_buffer_t buffer,
				       ompi_process_name_t *sender,
				       bool *return_requested,
				       bool *compound_cmd_detected);


#endif
