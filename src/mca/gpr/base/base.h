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
 * The Open MPI general purpose registry.
 *
 * The Open MPI system contains a general purpose registry for use by both
 * applications and internal systems to dynamically share information. For
 * speed purposes, the registry is divided into "segments", each labelled
 * with an appropriate "token" string that describes its contents. Segments
 * are automatically provided for the "universe" and for each MPI CommWorld.
 * At this time, all segments may be accessed by any application within the universe, thus
 * providing a mechanism for cross-CommWorld communications (with the requirement
 * that all participating CommWorlds must reside within the same universe). In the future,
 * some form of security may be provided to limit access privileges between
 * segments.
 *
 * Within each registry segment, there exists a list of objects that have
 * been "put" onto the registry. Each object must be tagged with at least
 * one token, but may be tagged with as many tokens as the creator desires.
 * Retrieval requests must specify the segment and at least one token, but
 * can specify an arbitrary number of tokens to describe the search. The registry
 * will return a list of all objects that meet the search criteria.
 *
 * Tokens are defined as character strings, thus allowing for clarity in
 * the program. However, for speed purposes, tokens are translated into
 * integer keys prior to storing an object. A table of token-key pairs
 * is independently maintained for each registry segment. Users can obtain
 * an index of tokens within a dictionary by requesting it through the orte_registry_index()
 * function.
 *
 * The registry also provides a subscription capability whereby a caller
 * can subscribe to a stored object and receive notification when various actions
 * are performed on that object. Currently supported actions include modification,
 * the addition of another subscriber, and deletion. Notifications are sent via
 * the OOB communication channel.
 *
 * 
 */

#ifndef ORTE_GPR_BASE_H_
#define ORTE_GPR_BASE_H_

/*
 * includes
 */
#include "orte_config.h"

#include "include/orte_constants.h"
#include "include/orte_types.h"

#include "threads/mutex.h"
#include "threads/condition.h"

#include "class/ompi_list.h"
#include "dps/dps_types.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/rml/rml_types.h"

#include "mca/gpr/gpr.h"

/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Define flag values for remote commands
 */
#define ORTE_GPR_DELETE_SEGMENT_CMD     (uint8_t)  1
#define ORTE_GPR_PUT_CMD                (uint8_t)  2
#define ORTE_GPR_DELETE_ENTRIES_CMD     (uint8_t)  3
#define ORTE_GPR_INDEX_CMD              (uint8_t)  4
#define ORTE_GPR_SUBSCRIBE_CMD          (uint8_t)  5
#define ORTE_GPR_UNSUBSCRIBE_CMD        (uint8_t)  6
#define ORTE_GPR_CANCEL_TRIGGER_CMD     (uint8_t)  7
#define ORTE_GPR_GET_CMD                (uint8_t)  8
#define ORTE_GPR_TEST_INTERNALS_CMD     (uint8_t)  9
#define ORTE_GPR_NOTIFY_CMD             (uint8_t) 10
#define ORTE_GPR_DUMP_ALL_CMD           (uint8_t) 11
#define ORTE_GPR_DUMP_SEGMENTS_CMD      (uint8_t) 12
#define ORTE_GPR_DUMP_TRIGGERS_CMD      (uint8_t) 13
#define ORTE_GPR_DUMP_SUBSCRIPTIONS_CMD (uint8_t) 14
#define ORTE_GPR_DUMP_CALLBACKS_CMD     (uint8_t) 15
#define ORTE_GPR_INCREMENT_VALUE_CMD    (uint8_t) 16
#define ORTE_GPR_DECREMENT_VALUE_CMD    (uint8_t) 17
#define ORTE_GPR_COMPOUND_CMD           (uint8_t) 18
#define ORTE_GPR_CLEANUP_JOB_CMD        (uint8_t) 19
#define ORTE_GPR_CLEANUP_PROC_CMD       (uint8_t) 20
#define ORTE_GPR_ERROR                  (uint8_t)0xff

typedef uint8_t orte_gpr_cmd_flag_t;
#define ORTE_GPR_CMD_T ORTE_UINT8

   OMPI_DECLSPEC int orte_gpr_base_open(void);
   OMPI_DECLSPEC int orte_gpr_base_select(void);
   OMPI_DECLSPEC int orte_gpr_base_close(void);

    /* general usage functions */
    OMPI_DECLSPEC int orte_gpr_base_pack_delete_segment(orte_buffer_t *cmd,
                                                        char *segment);
    OMPI_DECLSPEC int orte_gpr_base_unpack_delete_segment(orte_buffer_t *buffer, int *ret);

    OMPI_DECLSPEC int orte_gpr_base_pack_delete_entries(orte_buffer_t *buffer,
					orte_gpr_addr_mode_t mode,
					char *segment, char **tokens, char **keys);
    OMPI_DECLSPEC int orte_gpr_base_unpack_delete_entries(orte_buffer_t *buffer, int *ret);

    OMPI_DECLSPEC int orte_gpr_base_pack_index(orte_buffer_t *cmd, char *segment);
    OMPI_DECLSPEC int orte_gpr_base_unpack_index(orte_buffer_t *cmd, int *ret, size_t *cnt,
                                                 char **index);

    OMPI_DECLSPEC int orte_gpr_base_pack_subscribe(orte_buffer_t *cmd,
                    size_t num_subs,
                    orte_gpr_subscription_t **subscriptions,
                    size_t num_trigs, orte_gpr_trigger_t **trig);
    OMPI_DECLSPEC int orte_gpr_base_unpack_subscribe(orte_buffer_t *buffer, int *ret);

    OMPI_DECLSPEC int orte_gpr_base_pack_unsubscribe(orte_buffer_t *cmd,
				      orte_gpr_subscription_id_t id);
    OMPI_DECLSPEC int orte_gpr_base_unpack_unsubscribe(orte_buffer_t *buffer, int *ret);

    OMPI_DECLSPEC int orte_gpr_base_pack_cancel_trigger(orte_buffer_t *cmd,
                     orte_gpr_trigger_id_t id);
    OMPI_DECLSPEC int orte_gpr_base_unpack_cancel_trigger(orte_buffer_t *buffer, int *ret);

    OMPI_DECLSPEC int orte_gpr_base_pack_put(orte_buffer_t *cmd,
			                                 size_t cnt, orte_gpr_value_t **values);
    OMPI_DECLSPEC int orte_gpr_base_unpack_put(orte_buffer_t *buffer, int *ret);

    OMPI_DECLSPEC int orte_gpr_base_pack_get(orte_buffer_t *cmd,
			      orte_gpr_addr_mode_t mode,
			      char *segment, char **tokens, char **keys);
    OMPI_DECLSPEC int orte_gpr_base_unpack_get(orte_buffer_t *buffer, int *ret,
                   size_t *cnt, orte_gpr_value_t ***values);

    OMPI_DECLSPEC int orte_gpr_base_pack_dump_all(orte_buffer_t *cmd);
    OMPI_DECLSPEC int orte_gpr_base_pack_dump_segments(orte_buffer_t *cmd);
    OMPI_DECLSPEC int orte_gpr_base_pack_dump_triggers(orte_buffer_t *cmd);
    OMPI_DECLSPEC int orte_gpr_base_pack_dump_subscriptions(orte_buffer_t *cmd);
    OMPI_DECLSPEC int orte_gpr_base_pack_dump_callbacks(orte_buffer_t *cmd);
    OMPI_DECLSPEC int orte_gpr_base_print_dump(orte_buffer_t *buffer, int output_id);
    OMPI_DECLSPEC void orte_gpr_base_dump_keyval_value(orte_buffer_t *buffer,
                        orte_gpr_keyval_t *iptr);

    OMPI_DECLSPEC int orte_gpr_base_dump_notify_msg(orte_buffer_t *buffer,
                        orte_gpr_notify_message_t *msg);
    OMPI_DECLSPEC int orte_gpr_base_dump_notify_data(orte_buffer_t *buffer,
                        orte_gpr_notify_data_t *data);
    OMPI_DECLSPEC int orte_gpr_base_dump_value(orte_buffer_t *buffer,
                        orte_gpr_value_t *value);

    OMPI_DECLSPEC int orte_gpr_base_pack_cleanup_job(orte_buffer_t *buffer,
                                                     orte_jobid_t jobid);
    OMPI_DECLSPEC int orte_gpr_base_unpack_cleanup_job(orte_buffer_t *buffer, int *ret);
    
    OMPI_DECLSPEC int orte_gpr_base_pack_cleanup_proc(orte_buffer_t *buffer,
                                                      orte_process_name_t *proc);
    OMPI_DECLSPEC int orte_gpr_base_unpack_cleanup_proc(orte_buffer_t *buffer, int *ret);
    
    OMPI_DECLSPEC int orte_gpr_base_pack_increment_value(orte_buffer_t *cmd, orte_gpr_value_t *value);
    OMPI_DECLSPEC int orte_gpr_base_unpack_increment_value(orte_buffer_t *buffer, int *ret);

    OMPI_DECLSPEC int orte_gpr_base_pack_decrement_value(orte_buffer_t *cmd, orte_gpr_value_t *value);
    OMPI_DECLSPEC int orte_gpr_base_unpack_decrement_value(orte_buffer_t *buffer, int *ret);

/* GPR DATA TYPE PACKING FUNCTIONS */
int orte_gpr_base_pack_cmd(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_subscription_id(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_trigger_id(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_notify_action(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_trigger_action(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_addr_mode(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_keyval(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_value(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_subscription(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_trigger(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_notify_data(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

/* GPR DATA TYPE UNPACKING FUNCTIONS */
int orte_gpr_base_unpack_cmd(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_subscription_id(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_trigger_id(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_notify_action(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_trigger_action(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_addr_mode(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_keyval(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_value(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_subscription(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_trigger(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_notify_data(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

/* general utilities */
OMPI_DECLSPEC int orte_gpr_base_xfer_payload(orte_gpr_value_union_t *dest,
                               orte_gpr_value_union_t *src,
                               orte_data_type_t type);

/*
 * globals that might be needed inside the gpr
 */
OMPI_DECLSPEC extern int orte_gpr_base_output;
OMPI_DECLSPEC extern bool orte_gpr_base_selected;
OMPI_DECLSPEC extern ompi_list_t orte_gpr_base_components_available;
OMPI_DECLSPEC extern mca_gpr_base_component_t orte_gpr_base_selected_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
