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
/** @file
 * @page gpr_api
 */

/**
 *  \brief General Purpose Registry (GPR) API
 *
 * The Open MPI General Purpose Registry (GPR)
 */

#ifndef ORTE_GPR_H_
#define ORTE_GPR_H_

/*
 * includes
 */

#include "orte_config.h"

#include <sys/types.h>

#include "orte/orte_constants.h"
#include "opal/class/opal_list.h"

#include "opal/mca/mca.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rml/rml_types.h"

#include "orte/dss/dss_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/rml/rml_types.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Component functions that MUST be provided
 */

/*
 * Perform any one-time initialization required by the module
 * after RML/NS are available.
 */
typedef int (*orte_gpr_base_module_init_fn_t)(void);

/*
 * Begin recording a compound command.
 * Normally, the registry executes each command as it is called. This, however, can result
 * in an undesirable amount of network traffic. To reduce the traffic, this command allows
 * the user to aggregate a set of registry commands - in any combination of put, get, index,
 * or any other command - to be executed via a single communication to the registry.
 *
 * While recording, all registry commands are stored in a buffer instead of being immediately
 * executed. Thus, commands that retrieve information (e.g., "get") will return a NULL
 * during recording. Values from these commands will be returned when the compound
 * command is actually executed.
 *
 * The process of recording a compound command is thread safe. Threads attempting to
 * record commands are held on a lock until given access in their turn.
 *
 * @param None
 * @retval ORTE_SUCCESS Compound command recorder is active.
 * @retval ORTE_ERROR Compound command recorder did not activate.
 *
 * @code
 * ompi_gpr.begin_compound_cmd();
 * @endcode
 *
 */
typedef int (*orte_gpr_base_module_begin_compound_cmd_fn_t)(void);

/*
 * Stop recording a compound command
 * Terminates the recording process and clears the buffer of any previous commands
 *
 * @param None
 * @retval ORTE_SUCCESS Recording stopped and buffer successfully cleared
 * @retval ORTE_ERROR Didn't work - no idea why it wouldn't
 *
 * @code
 * orte_gpr.stop_compound_cmd();
 * @endcode
 *
 */
typedef int (*orte_gpr_base_module_stop_compound_cmd_fn_t)(void);

/*
 * Execute the compound command (BLOCKING)
 * Execute the compound command that has been recorded. The function returns a status
 * code that indicates whether or not all the included commands were successfully
 * executed. Failure of any command contained in the compound command will terminate
 * execution of the compound command list and return an error to the caller.
 *
 * @param none
 * @retval ORTE_SUCCESS All commands in the list were successfully executed.
 * @retval ORTE_ERROR(s) A command in the list failed, returning the indicated
 * error code.
 *
 * @code
 * status_code = orte_gpr.exec_compound_cmd();
 * @endcode
 *
 */
typedef int (*orte_gpr_base_module_exec_compound_cmd_fn_t)(void);


/*
 * Cleanup a job from the registry
 * Remove all references to a given job from the registry. This includes removing
 * all segments "owned" by the job, and removing all process names from dictionaries
 * in the registry.
 *
 * @param jobid The jobid to be cleaned up.
 *
 * @retval ORTE_SUCCESS Operation was successfully completed.
 * @retval ORTE_ERROR(s) Operation failed, returning the provided error code.
 *
 * @code
 * status_code = orte_gpr.cleanup_job(jobid);
 * @endcode
 *
 */
typedef int (*orte_gpr_base_module_cleanup_job_fn_t)(orte_jobid_t jobid);

/*
 * Cleanup a process from the registry
 * Remove all references to a given process from the registry. This includes removing
 * the process name from all dictionaries in the registry, all subscriptions, etc.
 * It also includes reducing any synchros on the job segment.
 *
 * @param proc A pointer to the process name to be cleaned up.
 *
 * @retval ORTE_SUCCESS Operation was successfully completed.
 * @retval ORTE_ERROR(s) Operation failed, returning the provided error code.
 *
 * @code
 * status_code = orte_gpr.cleanup_process(&proc);
 * @endcode
 *
 */
typedef int (*orte_gpr_base_module_cleanup_proc_fn_t)(orte_process_name_t *proc);

/*
 * Define and initialize a job segment
 * The registry contains a segment for each job that stores data on each
 * process within that job. Although the registry can create this segment
 * "on-the-fly", it is more efficient to initialize the segment via a separate
 * command - thus allowing the registry to allocate the base storage for all
 * the processes in a single malloc.
 *
 * @param name A character string indicating the name of the segment.
 * @param num_slots The number of containers expected in this segment. This
 * is just the starting number requested by the user - the registry will
 * dynamically expand the segment as required.
 *
 * @retval ORTE_SUCCESS The operation was successfully executed.
 * @retval ORTE_ERROR(s) An appropriate error code is returned.
 *
 * @code
 * status_code = orte_gpr.preallocate_segment("MY_SEGMENT", num_slots);
 * @endcode
 */
typedef int (*orte_gpr_base_module_preallocate_segment_fn_t)(char *name, orte_std_cntr_t num_slots);

/*
 * Delete a segment from the registry (BLOCKING)
 * This command removes an entire segment from the registry, including all data objects,
 * associated subscriptions, and synchros. This is a non-reversible process, so it should
 * be used with care.
 *
 * @param segment Character string specifying the name of the segment to be removed.
 *
 * @retval ORTE_SUCCESS Segment successfully removed.
 * @retval ORTE_ERROR(s) Segment could not be removed for some reason - most
 * likely, the segment name provided was not found in the registry.
 *
 * @code
 * status_code = orte_gpr.delete_segment(segment);
 * @endcode
 */
typedef int (*orte_gpr_base_module_delete_segment_fn_t)(char *segment);

/*
 * Delete a segment from the registry (NON-BLOCKING)
 * A non-blocking version of delete segment.
 */
typedef int (*orte_gpr_base_module_delete_segment_nb_fn_t)(char *segment,
                                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);


/*
 * Put a data object on the registry (BLOCKING)
 * Place a data item on the registry using a blocking operation - i.e., the calling
 * program will be blocked until the operation completes.
 *
 * Each value contains the addressing mode to be used. Addresses are defined by the tokens provided
 * that describe the object being stored. The caller has the option of specifying how
 * those tokens are to be combined in describing the object. Passing a value of
 * "ORTE_REGISTRY_AND", for example, indicates that all provided tokens are to be used.
 * In contrast, a value of "ORTE_REGISTRY_OR" indicates that any of the provided tokens
 * can adequately describe the object. For the "put" command, only "ORTE_REGISTRY_XAND"
 * is accepted - in other words, the tokens must exactly match those of any existing
 * object in order for the object to be updated. In addition, the "ORTE_REGISTRY_OVERWRITE"
 * flag must be or'd into the mode to enable update of the data object. If a data object
 * is found with the identical token description, but ORTE_REGISTRY_OVERWRITE is NOT specified,
 * then an error will be generated - the data object will NOT be overwritten in this
 * situation.
 *
 * Upon completing the "put", all subscriptions registered on the
 * specified segment are checked and appropriately processed.
 *
 * @param cnt The number of value structures to be stored.
 *
 * @param **values A pointer to the start of a contiguous array of one or more
 * pointers to orte_gpr_value_t
 * objects to be stored. The registry will copy this data onto the specified segment - the
 * calling program is responsible for freeing any memory, if appropriate.
 *
 * @retval ORTE_SUCCESS The data has been stored on the specified segment, or the
 * corresponding existing data has been updated.
 *
 * @retval ORTE_ERROR(s) The data was not stored on the specified segment, or the
 * corresponding existing data was not found, or the data was found but the overwrite
 * flag was not set.
 *
 * @code
 * orte_gpr_value_t *value;
 *
 * status_code = orte_gpr.put(1, &value);
 * @endcode
 */
typedef int (*orte_gpr_base_module_put_fn_t)(orte_std_cntr_t cnt, orte_gpr_value_t **values);

/* simplified version of the put command */
typedef int (*orte_gpr_base_module_put_1_fn_t)(orte_gpr_addr_mode_t addr_mode,
                                               char *segment, char **tokens,
                                               char *key, orte_data_value_t *value);

typedef int (*orte_gpr_base_module_put_N_fn_t)(orte_gpr_addr_mode_t addr_mode,
                                               char *segment, char **tokens,
                                               orte_std_cntr_t n, char **keys,
                                               orte_data_value_t **data_values);


/*
 * Put data on the registry (NON-BLOCKING)
 * A non-blocking version of put.
 */
typedef int (*orte_gpr_base_module_put_nb_fn_t)(orte_std_cntr_t cnt, orte_gpr_value_t **values,
                      orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);


/*
 * Get data from the registry (BLOCKING)
 * Returns data from the registry. Given an addressing mode, segment name, and a set
 * of tokens describing the data to be retrieved, the "get" function will search the specified
 * registry segment and return all data items that "match" the description. Addressing
 * modes specify how the provided tokens are to be combined to determine the match -
 * a value of "ORTE_REGISTRY_AND", for example, indictates that all the tokens must be
 * included in the object's description, but allows for other tokens to also be present.
 * A value of "ORTE_REGISTRY_XAND", in contrast, requires that all the tokens be present,
 * and that ONLY those tokens be present.
 *
 * The data is returned as a list of orte_gpr_value_t objects. The caller is
 * responsible for freeing this data storage. Only copies of the registry data are
 * returned - thus, any actions taken by the caller will NOT impact data stored on the
 * registry.
 *
 * @param addr_mode (IN) The addressing mode to be used in the search.
 * @param *segment (IN) A character string indicating the name of the segment to be searched.
 * @param **tokens (IN) A NULL-terminated **char list of tokens describing the objects to be
 * returned. A value of NULL indicates that ALL data on the segment is to be returned.
 * @param **keys (IN) A NULL-terminated **char array of keys describing the specific
 * key-value data to be returned. A value of NULL indicates that ALL key-value pairs
 * described by the segment/token combination are to be returned.
 *
 * @param *cnt (OUT) A pointer to the number of objects returned by the request.
 * @param ***values (OUT) A pointer to an array of orte_gpr_value_t object pointers
 * containing the data
 * returned by the specified search, including the segment and container id info
 * for each keyval pair.
 *
 * @retval ORTE_SUCCESS Operation was successfully completed.
 * @retval ORTE_ERROR(s) Operation failed, returning the provided error code.
 *
 * @code
 * opal_list_t *keyval_list;
 * orte_std_cntr_t cnt;
 * orte_gpr_value_t **values;
 *
 * status_code = orte_gpr.get(addr_mode, segment, tokens, keyval_list,
 *                            &cnt, &values);
 * @endcode
 */
typedef int (*orte_gpr_base_module_get_fn_t)(orte_gpr_addr_mode_t addr_mode,
                                char *segment, char **tokens, char **keys,
                                orte_std_cntr_t *cnt, orte_gpr_value_t ***values);

typedef int (*orte_gpr_base_module_get_conditional_fn_t)(orte_gpr_addr_mode_t addr_mode,
                                char *segment, char **tokens, char **keys,
                                orte_std_cntr_t num_conditions, orte_gpr_keyval_t **conditions,
                                orte_std_cntr_t *cnt, orte_gpr_value_t ***values);

/*
 * Get data from the registry (NON-BLOCKING)
 * A non-blocking version of get. Data is returned to the callback function in the
 * notify message format.
 */
typedef int (*orte_gpr_base_module_get_nb_fn_t)(orte_gpr_addr_mode_t addr_mode,
                                char *segment, char **tokens, char **keys,
                                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);


/*
 * Delete an object from the registry (BLOCKING)
 * Remove an object from the registry. Given an addressing mode, segment name, and a set
 * of tokens describing the data object, the function will search the specified
 * registry segment and delete all data items that "match" the description. Addressing
 * modes specify how the provided tokens are to be combined to determine the match -
 * a value of "ORTE_REGISTRY_AND", for example, indictates that all the tokens must be
 * included in the object's description, but allows for other tokens to also be present.
 * A value of "ORTE_REGISTRY_XAND", in contrast, requires that all the tokens be present,
 * and that ONLY those tokens be present.
 *
 * Note: A value of NULL for the tokens will delete ALL data items from the specified
 * segment.
 *
 * @param addr_mode The addressing mode to be used in the search.
 * @param *segment A character string indicating the name of the segment to be searched.
 * @param **tokens A NULL-terminated **char list of tokens describing the objects to be
 * returned. A value of NULL indicates that ALL data on the segment is to be removed.
 *
 * @retval ORTE_SUCCESS Operation was successfully completed.
 * @retval ORTE_ERROR(s) Operation failed, returning the provided error code.
 *
 * @code
 * status_code = orte_gpr.delete_object(mode, segment, tokens);
 * @endcode
 */
typedef int (*orte_gpr_base_module_delete_entries_fn_t)(orte_gpr_addr_mode_t addr_mode,
                              char *segment, char **tokens, char **keys);

/*
 * Delete an object from the registry (NON-BLOCKING)
 * A non-blocking version of delete object. Result of the command is returned
 * to the callback function in the notify msg format.
 */
typedef int (*orte_gpr_base_module_delete_entries_nb_fn_t)(
                            orte_gpr_addr_mode_t addr_mode,
                            char *segment, char **tokens, char **keys,
                            orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);
/*
 * Obtain an index of a specified dictionary (BLOCKING)
 * The registry contains a dictionary at the global level (containing names of all the
 * segments) and a dictionary for each segment (containing the names of all tokens used
 * in that segment). This command allows the caller to obtain a list of all entries
 * in the specified dictionary.
 *
 * @param *segment (IN) A character string indicating the segment whose dictionary is to be
 * indexed. A value of NULL indicates that the global level dictionary is to be used.
 *
 * @param *cnt (IN) A pointer to a orte_std_cntr_t location for storing the number of
 * tokens in the index.
 * @param ***index (IN) The address to place a char** array of strings containing an
 * index of the specified dictionary.
 *
 * @retval ORTE_SUCCESS Operation was successfully completed.
 * @retval ORTE_ERROR(s) Operation failed, returning the provided error code.
 *
 * @code
 * int32_t cnt;
 * char *index;
 * char *segment;
 *
 * status_code = orte_gpr.index(segment, &cnt, &index);
 * @endcode
 */
typedef int (*orte_gpr_base_module_index_fn_t)(char *segment, orte_std_cntr_t *cnt, char ***index);

/*
 * Obtain an index of a specified dictionary (NON-BLOCKING)
 * A non-blocking version of index. Result of the command is returned to the
 * callback function in the notify msg format.
 */
typedef int (*orte_gpr_base_module_index_nb_fn_t)(char *segment,
                        orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);

/*
 * Subscribe to be notified upon a specified action
 * The registry includes a publish/subscribe mechanism by which callers can be notified
 * upon certain actions occuring to data objects stored on the registry. This function
 * allows the caller to register for such notifications. The registry allows a subscription
 * to be placed upon any segment, and upon the entire registry if desired.
 *
 * Two types of subscriptions are supported:
 * (a) notifications - these occur whenever the specified action occurs on the
 * identified data entries in the registry.
 *
 * (b) triggers - these occur whenever a count of the number of identified data
 * entries reaches the specified level. The caller can specify that the trigger
 * maintain its own count - in this case, the trigger will count the number of data
 * entries in the registry that meet the specifications provided in \em value, and store
 * the running count in the location specified by \em trig_value. Alternatively, the
 * caller can specify that the trigger only monitor a count that is being maintained
 * by someone else - in this case, the \em trig_value information is used to identify
 * one or more "counters" that are to be monitored, with the trigger fired when either all
 * identified counters reach the respective levels provided in \em trig_value (using
 * the AT mode) or when the levels reach the same value (the CMP mode).
 *
 * Note that all addressing mode rules apply to both \em value and \em trig_value,
 * including wildcards.
 *
 * @param actions (IN) The actions which are to generate a notification message and/or define
 * the trigger operation. These can
 * be OR'd together from the defined registry action flags.
 *
 * @param num_subs (IN) The numbr of subscription objects being provided
 *
 * @param **subscriptions (IN) A pointer to an array of subscription objects that
 * contain descriptions of the data that is to be returned when a subscription fires.
 * For subscribe requests that do NOT include a trigger, this is the data that
 * will be monitored per the specified action. All of
 * the described values will be returned in a notification message when the specified
 * action occurs.
 *
 * @param num_trigs (IN) The number of trigger objects being provided
 *
 * @param **triggers (IN) A pointer to an array of orte_gpr_trigger_t objects that describe the
 * conditions (as described above) which will generate a trigger message to be sent
 * to the callback function. Trigger messages include all data specified in the
 * subscription objects, but do NOT include the trigger counters themselves unless
 * so specified with the ORTE_GPR_TRIG_INCLUDE_DATA command.
 *
 * @param *sub_number (OUT) The notify id for the resulting subscription
 * is returned in the provided memory location. Callers should save this
 * number for later use if (for example) it is desired to remove the
 * subscription from the registry
 *
 * @retval ORTE_SUCCESS Operation was successfully completed.
 * @retval ORTE_ERROR(s) Operation failed, returning the provided error code.
 *
 * @code
 * orte_gpr_subscription_t *subscription;
 * orte_gpr_notify_id_t sub_number;
 * orte_gpr_value_t trig_value;
 *
 * status_code = orte_gpr.subscribe(action, 1, &subscription, &trig_value,
 *                                       &sub_number);
 * @endcode
 */
typedef int (*orte_gpr_base_module_subscribe_fn_t)(
                            orte_std_cntr_t num_subs,
                            orte_gpr_subscription_t **subscriptions,
                            orte_std_cntr_t num_trigs,
                            orte_gpr_trigger_t **triggers);

/* simplified subscription functions */
typedef int (*orte_gpr_base_module_subscribe_1_fn_t)(orte_gpr_subscription_id_t *id,
                                                     char *trig_name,
                                                     char *sub_name,
                                                     orte_gpr_notify_action_t action,
                                                     orte_gpr_addr_mode_t addr_mode,
                                                     char *segment,
                                                     char **tokens,
                                                     char *key,
                                                     orte_gpr_notify_cb_fn_t cbfunc,
                                                     void *user_tag);

typedef int (*orte_gpr_base_module_subscribe_N_fn_t)(orte_gpr_subscription_id_t *id,
                                                     char *trig_name,
                                                     char *sub_name,
                                                     orte_gpr_notify_action_t action,
                                                     orte_gpr_addr_mode_t addr_mode,
                                                     char *segment,
                                                     char **tokens,
                                                     orte_std_cntr_t n,
                                                     char **keys,
                                                     orte_gpr_notify_cb_fn_t cbfunc,
                                                     void *user_tag);

typedef int (*orte_gpr_base_module_define_trigger_fn_t)(orte_gpr_trigger_id_t *id,
                                                     char *trig_name,
                                                     orte_gpr_trigger_action_t action,
                                                     orte_gpr_addr_mode_t addr_mode,
                                                     char *segment,
                                                     char **tokens,
                                                     orte_std_cntr_t n,
                                                     char **keys,
                                                     orte_gpr_trigger_cb_fn_t cbfunc,
                                                     void *user_tag);

typedef int (*orte_gpr_base_module_define_trigger_level_fn_t)(orte_gpr_trigger_id_t *id,
                                                     char *trig_name,
                                                     orte_gpr_trigger_action_t action,
                                                     orte_gpr_addr_mode_t addr_mode,
                                                     char *segment,
                                                     char **tokens,
                                                     orte_std_cntr_t n,
                                                     char **keys,
                                                     orte_std_cntr_t *levels,
                                                     orte_gpr_trigger_cb_fn_t cbfunc,
                                                     void *user_tag);

/*
 * Cancel a subscription.
 * Once a subscription has been entered on the registry, a caller may choose to permanently
 * remove it at a later time. This function supports that request.
 *
 * @param sub_number The orte_gpr_subscription_id_t value returned by the original subscribe
 * command.
 *
 * @retval ORTE_SUCCESS The subscription was removed.
 * @retval ORTE_ERROR The subscription could not be removed - most likely caused by specifying
 * a non-existent (or previously removed) subscription number.
 *
 * @code
 * status_code = orte_gpr.unsubscribe(sub_number);
 * @endcode
 */
typedef int (*orte_gpr_base_module_unsubscribe_fn_t)(orte_gpr_subscription_id_t sub_number);

/*
 * Cancel a trigger.
 * Once a trigger has been entered on the registry, a caller may choose to permanently
 * remove it at a later time. This function supports that request.
 *
 * @param trig_number The orte_gpr_trigger_id_t value returned by the original subscribe
 * command.
 *
 * @retval ORTE_SUCCESS The trigger was removed.
 * @retval ORTE_ERROR The trigger could not be removed - most likely caused by specifying
 * a non-existent (or previously removed) trigger number.
 *
 * @code
 * status_code = orte_gpr.cancel_trigger(trig_number);
 * @endcode
 */
typedef int (*orte_gpr_base_module_cancel_trigger_fn_t)(orte_gpr_trigger_id_t trig_number);


/* Output the registry's contents to an output stream
 * For debugging purposes, it is helpful to be able to obtain a complete formatted printout
 * of the registry's contents. This function provides that ability.
 *
 * @param output_id The output stream id to which the registry's contents are to be
 * printed.
 *
 * @retval ORTE_SUCCESS Operation was successfully completed.
 * @retval ORTE_ERROR(s) Operation failed, returning the provided error code.
 *
 * @code
 * orte_gpr.dump(output_id);
 * @endcode
 */
typedef int (*orte_gpr_base_module_dump_all_fn_t)(void);

typedef int (*orte_gpr_base_module_dump_segment_fn_t)(char *segment);

typedef int (*orte_gpr_base_module_dump_triggers_fn_t)(
                            orte_gpr_trigger_id_t tail);

typedef int (*orte_gpr_base_module_dump_subscriptions_fn_t)(
                            orte_gpr_subscription_id_t tail);

typedef int (*orte_gpr_base_module_dump_a_trigger_fn_t)(
                            char *name,
                            orte_gpr_trigger_id_t id);

typedef int (*orte_gpr_base_module_dump_a_subscription_fn_t)(
                            char *name,
                            orte_gpr_subscription_id_t id);

typedef int (*orte_gpr_base_module_dump_local_triggers_fn_t)(void);

typedef int (*orte_gpr_base_module_dump_local_subscriptions_fn_t)(void);

typedef int (*orte_gpr_base_module_dump_callbacks_fn_t) (void);

typedef int (*orte_gpr_base_module_dump_notify_msg_fn_t)(orte_gpr_notify_message_t *msg);

typedef int (*orte_gpr_base_module_dump_notify_data_fn_t)(orte_gpr_notify_data_t *data);

typedef int (*orte_gpr_base_module_dump_value_fn_t)(orte_gpr_value_t *value);

typedef int (*orte_gpr_base_module_dump_segment_size_fn_t)(char *segment);

/*
 * Increment value
 * This function increments the stored value of an existing registry entry by one. Failure
 * to find the entry on the registry will result in an error.
 */
typedef int (*orte_gpr_base_module_increment_value_fn_t)(orte_gpr_value_t *value);

/*
 * Decrement value
 * This function decrements the stored value of an existing registry entry by one. Failure
 * to find the entry on the registry will result in an error.
 */
typedef int (*orte_gpr_base_module_decrement_value_fn_t)(orte_gpr_value_t *value);


/* Deliver a notify message
 * To support the broadcast of stage gate messages that supply all subscribed
 * data in a single message, we have to provide an API that allows the xcast
 * to "inject" the message back into the registry's local delivery system.
 *
 * @param msg A pointer to the orte_gpr_notify_message_t object to be delivered.
 * Note that the calling program is responsible for releasing this object.
 *
 * @retval None
 */
typedef int (*orte_gpr_base_module_deliver_notify_msg_t)(orte_gpr_notify_message_t *msg);


/* Create a gpr value structure
 * To make it easier for users, this function will create an orte_gpr_value_t structure,
 * including performing all the error checks to ensure adequate memory is available.
 *
 * Any data that the caller wishes to provide will be pre-loaded into the returned value.
 * The function will allocate space for the value object and for the number of keyvals
 * and tokens to be stored in the object. If the caller wishes to allocate that space
 * themselves, or does not want space allocated for those purposes, then just pass a
 * value of "0" (zero) and the function will not allocate memory to those areas. Likewise,
 * a value of NULL for segment will cause the function to ignore that field in the
 * value object.
 *
 * @retval ORTE_SUCCESS Value structure successfully created
 * @retval ORTE_XXXX Appropriate error code indicating problem encountered.
 */
typedef int (*orte_gpr_base_module_create_value_fn_t)(orte_gpr_value_t **value,
                                                orte_gpr_addr_mode_t addr_mode,
                                                char *segment,
                                                orte_std_cntr_t cnt,  /**< Number of keyval objects */
                                                orte_std_cntr_t num_tokens);
/* Create a keyval object
 * To make it easier for users, this function will create an orte_gpr_keyval_t object,
 * including performing all the error checks to ensure adequate memory is available.
 *
 * Any data that the caller provides will be copied into the returned keyval object.
 * If key or data are set to NULL, then those fields will be left to their default NULL
 * values.
 *
 * @retval ORTE_SUCCESS Value structure successfully created
 * @retval ORTE_XXXX Appropriate error code indicating problem encountered.
 */
typedef int (*orte_gpr_base_module_create_keyval_fn_t)(orte_gpr_keyval_t **keyval,
                                                 char *key,
                                                 orte_data_type_t type,
                                                 void *data);
/*
 * Ver 1.0.0
 */
struct orte_gpr_base_module_1_0_0_t {
    /* INIT */
    orte_gpr_base_module_init_fn_t init;
    /* BLOCKING OPERATIONS */
    orte_gpr_base_module_get_fn_t get;
    orte_gpr_base_module_get_conditional_fn_t get_conditional;
    orte_gpr_base_module_put_fn_t put;
    orte_gpr_base_module_put_1_fn_t put_1;
    orte_gpr_base_module_put_N_fn_t put_N;
    orte_gpr_base_module_delete_entries_fn_t delete_entries;
    orte_gpr_base_module_delete_segment_fn_t delete_segment;
    orte_gpr_base_module_index_fn_t index;
    /* NON-BLOCKING OPERATIONS */
    orte_gpr_base_module_get_nb_fn_t get_nb;
    orte_gpr_base_module_put_nb_fn_t put_nb;
    orte_gpr_base_module_delete_entries_nb_fn_t delete_entries_nb;
    orte_gpr_base_module_delete_segment_nb_fn_t delete_segment_nb;
    orte_gpr_base_module_index_nb_fn_t index_nb;
    /* GENERAL OPERATIONS */
    orte_gpr_base_module_create_value_fn_t create_value;
    orte_gpr_base_module_create_keyval_fn_t create_keyval;
    orte_gpr_base_module_preallocate_segment_fn_t preallocate_segment;
    orte_gpr_base_module_deliver_notify_msg_t deliver_notify_msg;
    /* ARITHMETIC OPERATIONS */
    orte_gpr_base_module_increment_value_fn_t increment_value;
    orte_gpr_base_module_decrement_value_fn_t decrement_value;
    /* SUBSCRIBE OPERATIONS */
    orte_gpr_base_module_subscribe_fn_t subscribe;
    orte_gpr_base_module_subscribe_1_fn_t subscribe_1;
    orte_gpr_base_module_subscribe_N_fn_t subscribe_N;
    orte_gpr_base_module_define_trigger_fn_t define_trigger;
    orte_gpr_base_module_define_trigger_level_fn_t define_trigger_level;
    orte_gpr_base_module_unsubscribe_fn_t unsubscribe;
    orte_gpr_base_module_cancel_trigger_fn_t cancel_trigger;
    /* COMPOUND COMMANDS */
    orte_gpr_base_module_begin_compound_cmd_fn_t begin_compound_cmd;
    orte_gpr_base_module_stop_compound_cmd_fn_t stop_compound_cmd;
    orte_gpr_base_module_exec_compound_cmd_fn_t exec_compound_cmd;
    /* DIAGNOSTIC OPERATIONS */
    orte_gpr_base_module_dump_all_fn_t dump_all;
    orte_gpr_base_module_dump_segment_fn_t dump_segment;
    orte_gpr_base_module_dump_triggers_fn_t dump_triggers;
    orte_gpr_base_module_dump_subscriptions_fn_t dump_subscriptions;
    orte_gpr_base_module_dump_a_trigger_fn_t dump_a_trigger;
    orte_gpr_base_module_dump_a_subscription_fn_t dump_a_subscription;
    orte_gpr_base_module_dump_local_triggers_fn_t dump_local_triggers;
    orte_gpr_base_module_dump_local_subscriptions_fn_t dump_local_subscriptions;
    orte_gpr_base_module_dump_callbacks_fn_t dump_callbacks;
    orte_gpr_base_module_dump_notify_msg_fn_t dump_notify_msg;
    orte_gpr_base_module_dump_notify_data_fn_t dump_notify_data;
    orte_gpr_base_module_dump_value_fn_t dump_value;
    orte_gpr_base_module_dump_segment_size_fn_t dump_segment_size;
    /* CLEANUP OPERATIONS */
    orte_gpr_base_module_cleanup_job_fn_t cleanup_job;
    orte_gpr_base_module_cleanup_proc_fn_t cleanup_process;
};
typedef struct orte_gpr_base_module_1_0_0_t orte_gpr_base_module_1_0_0_t;
typedef orte_gpr_base_module_1_0_0_t orte_gpr_base_module_t;

/*
 * GPR Component
 */

typedef orte_gpr_base_module_t* (*orte_gpr_base_component_init_fn_t)(
                                   bool *allow_multi_user_threads,
                                   bool *have_hidden_threads,
                                   int *priority);

typedef int (*orte_gpr_base_component_finalize_fn_t)(void);

/*
 * the standard component data structure
 */


struct mca_gpr_base_component_1_0_0_t {
    mca_base_component_t gpr_version;
    mca_base_component_data_1_0_0_t gpr_data;

    orte_gpr_base_component_init_fn_t gpr_init;
    orte_gpr_base_component_finalize_fn_t gpr_finalize;
};
typedef struct mca_gpr_base_component_1_0_0_t mca_gpr_base_component_1_0_0_t;
typedef mca_gpr_base_component_1_0_0_t mca_gpr_base_component_t;

/*
 * Macro for use in modules that are of type gpr v1.0.0
 */
#define MCA_GPR_BASE_VERSION_1_0_0      \
    /* gpr v1.0 is chained to MCA v1.0 */   \
    MCA_BASE_VERSION_1_0_0,         \
    /* gpr v1.0 */              \
    "gpr", 1, 0, 0

/*
 * global module that holds function pointers
 */
ORTE_DECLSPEC extern orte_gpr_base_module_t orte_gpr; /* holds selected module's function pointers */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
