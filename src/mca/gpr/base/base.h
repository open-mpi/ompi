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
 * an index of tokens within a dictionary by requesting it through the ompi_registry_index()
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

#ifndef MCA_GPR_BASE_H_
#define MCA_GRP_BASE_H_

/*
 * includes
 */
#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>
#include <sys/types.h>

#include "include/constants.h"

#include "threads/mutex.h"
#include "threads/condition.h"

#include "runtime/runtime.h"

#include "util/output.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "util/bufpack.h"

#include "class/ompi_list.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/oob/base/base.h"

#include "mca/gpr/gpr.h"

/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    OMPI_DECLSPEC int mca_gpr_base_open(void);
    OMPI_DECLSPEC int mca_gpr_base_select(bool *allow_multi_user_threads,
			    bool *have_hidden_threads);
    OMPI_DECLSPEC int mca_gpr_base_close(void);

    /* general usage functions */
    OMPI_DECLSPEC int mca_gpr_base_pack_delete_segment(ompi_buffer_t cmd, bool silent, char *segment);
    OMPI_DECLSPEC int mca_gpr_base_unpack_delete_segment(ompi_buffer_t buffer);

    OMPI_DECLSPEC int mca_gpr_base_pack_delete_object(ompi_buffer_t buffer, bool silent,
					ompi_registry_mode_t mode,
					char *segment, char **tokens);
    OMPI_DECLSPEC int mca_gpr_base_unpack_delete_object(ompi_buffer_t buffer);

    OMPI_DECLSPEC int mca_gpr_base_pack_index(ompi_buffer_t cmd, char *segment);
    OMPI_DECLSPEC int mca_gpr_base_unpack_index(ompi_buffer_t cmd, ompi_list_t *return_list);

    OMPI_DECLSPEC int mca_gpr_base_pack_cleanup(ompi_buffer_t cmd, mca_ns_base_jobid_t jobid);

    OMPI_DECLSPEC int mca_gpr_base_pack_synchro(ompi_buffer_t cmd,
				  ompi_registry_synchro_mode_t synchro_mode,
				  ompi_registry_mode_t mode,
				  char *segment, char **tokens, int trigger);
    OMPI_DECLSPEC int mca_gpr_base_unpack_synchro(ompi_buffer_t buffer,
				    ompi_registry_notify_id_t *remote_idtag);

    OMPI_DECLSPEC int mca_gpr_base_pack_cancel_synchro(ompi_buffer_t cmd,
					 bool silent,
					 ompi_registry_notify_id_t remote_idtag);
    OMPI_DECLSPEC int mca_gpr_base_unpack_cancel_synchro(ompi_buffer_t buffer);

    OMPI_DECLSPEC int mca_gpr_base_pack_subscribe(ompi_buffer_t cmd,
				    ompi_registry_mode_t mode,
				    ompi_registry_notify_action_t action,
				    char *segment, char **tokens);
    OMPI_DECLSPEC int mca_gpr_base_unpack_subscribe(ompi_buffer_t buffer,
				      ompi_registry_notify_id_t *remote_idtag);

    OMPI_DECLSPEC int mca_gpr_base_pack_unsubscribe(ompi_buffer_t cmd, bool silent,
				      ompi_registry_notify_id_t remote_idtag);
    OMPI_DECLSPEC int mca_gpr_base_unpack_unsubscribe(ompi_buffer_t buffer);

    OMPI_DECLSPEC int mca_gpr_base_pack_put(ompi_buffer_t cmd, bool silent,
			      ompi_registry_mode_t mode, char *segment,
			      char **tokens, ompi_registry_object_t object,
			      ompi_registry_object_size_t size);
    OMPI_DECLSPEC int mca_gpr_base_unpack_put(ompi_buffer_t buffer);

    OMPI_DECLSPEC int mca_gpr_base_pack_get(ompi_buffer_t cmd,
			      ompi_registry_mode_t mode,
			      char *segment, char **tokens);
    OMPI_DECLSPEC int mca_gpr_base_unpack_get(ompi_buffer_t buffer, ompi_list_t *return_list);

    OMPI_DECLSPEC int mca_gpr_base_pack_dump(ompi_buffer_t cmd);
    OMPI_DECLSPEC void mca_gpr_base_print_dump(ompi_buffer_t buffer, int output_id);

    OMPI_DECLSPEC int mca_gpr_base_pack_cleanup_job(ompi_buffer_t buffer, mca_ns_base_jobid_t jobid);
    OMPI_DECLSPEC int mca_gpr_base_pack_cleanup_proc(ompi_buffer_t buffer, bool purge, ompi_process_name_t *proc);

    OMPI_DECLSPEC int mca_gpr_base_pack_test_internals(ompi_buffer_t cmd, int level);
    OMPI_DECLSPEC int mca_gpr_base_unpack_test_internals(ompi_buffer_t buffer, ompi_list_t *return_list);

    OMPI_DECLSPEC int mca_gpr_base_pack_notify_off(ompi_buffer_t cmd,
       			             ompi_process_name_t *proc,
				     ompi_registry_notify_id_t sub_number);
    OMPI_DECLSPEC int mca_gpr_base_unpack_notify_off(ompi_buffer_t buffer);

    OMPI_DECLSPEC int mca_gpr_base_pack_notify_on(ompi_buffer_t cmd,
				    ompi_process_name_t *proc,
				    ompi_registry_notify_id_t sub_number);
    OMPI_DECLSPEC int mca_gpr_base_unpack_notify_on(ompi_buffer_t buffer);

    OMPI_DECLSPEC int mca_gpr_base_pack_assign_ownership(ompi_buffer_t cmd, bool silent,
					   mca_ns_base_jobid_t jobid, char *segment);
    OMPI_DECLSPEC int mca_gpr_base_unpack_assign_ownership(ompi_buffer_t buffer);

    OMPI_DECLSPEC int mca_gpr_base_pack_get_startup_msg(ompi_buffer_t cmd, mca_ns_base_jobid_t jobid);
    OMPI_DECLSPEC ompi_buffer_t mca_gpr_base_unpack_get_startup_msg(ompi_buffer_t buffer, ompi_list_t *recipients);

    OMPI_DECLSPEC int mca_gpr_base_pack_triggers_active_cmd(ompi_buffer_t cmd, mca_ns_base_jobid_t jobid);
    OMPI_DECLSPEC int mca_gpr_base_unpack_triggers_active_cmd(ompi_buffer_t cmd);

    OMPI_DECLSPEC int mca_gpr_base_pack_triggers_inactive_cmd(ompi_buffer_t cmd, mca_ns_base_jobid_t jobid);
    OMPI_DECLSPEC int mca_gpr_base_unpack_triggers_inactive_cmd(ompi_buffer_t cmd);
    
    OMPI_DECLSPEC void mca_gpr_base_release_notify_msg(ompi_registry_notify_message_t *msg);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * globals that might be needed
 */
extern int mca_gpr_base_output;
extern mca_gpr_base_module_t ompi_registry; /* holds selected module's function pointers */
extern bool mca_gpr_base_selected;
extern ompi_list_t mca_gpr_base_components_available;
extern mca_gpr_base_component_t mca_gpr_base_selected_component;


#endif
