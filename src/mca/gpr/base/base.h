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
#define ORTE_GRP_BASE_H_

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
 * packing type definitions for GPR-specific types
 */
/* CAUTION - any changes here must also change corresponding
 * typedefs in gpr_types.h
 */
#define ORTE_GPR_PACK_CMD                ORTE_GPR_CMD
#define ORTE_GPR_PACK_ACTION             ORTE_NOTIFY_ACTION
#define ORTE_GPR_PACK_ADDR_MODE          ORTE_UINT16
#define ORTE_GPR_PACK_SYNCHRO_MODE       ORTE_SYNCHRO_MODE
#define ORTE_GPR_PACK_NOTIFY_ID          ORTE_UINT32

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
                    orte_gpr_notify_action_t action, size_t num_subs,
                    orte_gpr_subscription_t **subscriptions,
                    size_t num_trigs, orte_gpr_value_t **trig);
    OMPI_DECLSPEC int orte_gpr_base_unpack_subscribe(orte_buffer_t *buffer, int *ret,
				      orte_gpr_notify_id_t *remote_idtag);

    OMPI_DECLSPEC int orte_gpr_base_pack_unsubscribe(orte_buffer_t *cmd,
				      orte_gpr_notify_id_t remote_idtag);
    OMPI_DECLSPEC int orte_gpr_base_unpack_unsubscribe(orte_buffer_t *buffer, int *ret);

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

int orte_gpr_base_pack_notify_id(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_notify_action(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_addr_mode(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_keyval(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_value(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_subscription(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

int orte_gpr_base_pack_notify_data(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type);

/* GPR DATA TYPE UNPACKING FUNCTIONS */
int orte_gpr_base_unpack_cmd(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_notify_id(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_notify_action(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_addr_mode(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_keyval(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_value(orte_buffer_t *buffer, void *dest,
                       size_t *num_vals, orte_data_type_t type);

int orte_gpr_base_unpack_subscription(orte_buffer_t *buffer, void *dest,
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
