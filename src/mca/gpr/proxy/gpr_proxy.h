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
#ifndef GPR_PROXY_H
#define GPR_PROXY_H


#include "ompi_config.h"

#include "mca/gpr/base/base.h"

#if defined(c_plusplus) || defined (__cplusplus) 
extern "C" {
#endif
/*
 * Module open / close
 */
int mca_gpr_proxy_open(void);
int mca_gpr_proxy_close(void);


/*
 * Startup / Shutdown
 */
mca_gpr_base_module_t*
mca_gpr_proxy_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority);

int mca_gpr_proxy_finalize(void);

/*
 * proxy-local types
 */

struct mca_gpr_proxy_notify_request_tracker_t {
    ompi_list_item_t item;                   /**< Allows this item to be placed on a list */
    ompi_registry_notify_cb_fn_t callback;   /**< Function to be called for notificaiton */
    void *user_tag;                          /**< User-provided tag for callback function */
    ompi_registry_notify_id_t local_idtag;   /**< Local ID tag of associated subscription */
    ompi_registry_notify_id_t remote_idtag;  /**< Remote ID tag of subscription */
    char *segment;                           /**< Pointer to name of segment */
    ompi_registry_notify_action_t action;    /**< Action that triggers notification */
};
typedef struct mca_gpr_proxy_notify_request_tracker_t mca_gpr_proxy_notify_request_tracker_t;

OMPI_COMP_EXPORT OBJ_CLASS_DECLARATION(mca_gpr_proxy_notify_request_tracker_t);


/*
 * globals used within proxy component
 */

extern ompi_process_name_t *mca_gpr_my_replica;
extern ompi_list_t mca_gpr_proxy_notify_request_tracker;
extern ompi_registry_notify_id_t mca_gpr_proxy_last_notify_id_tag;
extern ompi_list_t mca_gpr_proxy_free_notify_id_tags;
extern int mca_gpr_proxy_debug;
extern ompi_mutex_t mca_gpr_proxy_mutex;
extern bool mca_gpr_proxy_compound_cmd_mode;
extern ompi_buffer_t mca_gpr_proxy_compound_cmd;
extern ompi_mutex_t mca_gpr_proxy_wait_for_compound_mutex;
extern ompi_condition_t mca_gpr_proxy_compound_cmd_condition;
extern int mca_gpr_proxy_compound_cmd_waiting;
extern bool mca_gpr_proxy_silent_mode;

OMPI_COMP_EXPORT extern mca_gpr_base_component_t mca_gpr_proxy_component;

/*
 * Compound cmd functions
 */
int mca_gpr_proxy_begin_compound_cmd(void);

int mca_gpr_proxy_stop_compound_cmd(void);

ompi_list_t* mca_gpr_proxy_exec_compound_cmd(bool return_requested);

/*
 * Mode operations
 */
void mca_gpr_proxy_silent_mode_on(void);

void mca_gpr_proxy_silent_mode_off(void);

void mca_gpr_proxy_notify_off(ompi_registry_notify_id_t sub_number);

void mca_gpr_proxy_notify_on(ompi_registry_notify_id_t sub_number);

void mca_gpr_proxy_triggers_active(mca_ns_base_jobid_t jobid);

void mca_gpr_proxy_triggers_inactive(mca_ns_base_jobid_t jobid);

int mca_gpr_proxy_assign_ownership(char *segment, mca_ns_base_jobid_t jobid);

/*
 * Delete-index functions
 */
int mca_gpr_proxy_delete_segment(char *segment);

int mca_gpr_proxy_delete_object(ompi_registry_mode_t mode,
			    char *segment, char **tokens);

ompi_list_t* mca_gpr_proxy_index(char *segment);

void mca_gpr_proxy_cleanup(mca_ns_base_jobid_t jobid);


/*
 * Cleanup functions
 */
void mca_gpr_proxy_cleanup_job(mca_ns_base_jobid_t jobid);

void mca_gpr_proxy_cleanup_proc(bool purge, ompi_process_name_t *proc);


/*
 * Put-get functions
 */
int mca_gpr_proxy_put(ompi_registry_mode_t mode, char *segment,
		  char **tokens, ompi_registry_object_t object,
		  ompi_registry_object_size_t size);

ompi_list_t* mca_gpr_proxy_get(ompi_registry_mode_t mode,
			   char *segment, char **tokens);



/*
 * Subscribe functions
 */
ompi_registry_notify_id_t
mca_gpr_proxy_subscribe(ompi_registry_mode_t mode,
			ompi_registry_notify_action_t action,
			char *segment, char **tokens,
			ompi_registry_notify_cb_fn_t cb_func, void *user_tag);

int mca_gpr_proxy_unsubscribe(ompi_registry_notify_id_t sub_number);


/*
 * Synchro functions
 */
ompi_registry_notify_id_t
mca_gpr_proxy_synchro(ompi_registry_synchro_mode_t synchro_mode,
		      ompi_registry_mode_t mode,
		      char *segment, char **tokens, int trigger,
		      ompi_registry_notify_cb_fn_t cb_func, void *user_tag);

int mca_gpr_proxy_cancel_synchro(ompi_registry_notify_id_t synch_number);


/*
 * Dump function
 */
void mca_gpr_proxy_dump(int output_id);


/*
 * Messaging functions
 */
void mca_gpr_proxy_deliver_notify_msg(ompi_registry_notify_action_t state,
				      ompi_registry_notify_message_t *message);


/*
 * Test internals
 */
ompi_list_t* mca_gpr_proxy_test_internals(int level);


/*
 * Startup functions
 */
ompi_buffer_t mca_gpr_proxy_get_startup_msg(mca_ns_base_jobid_t jobid,
					    ompi_list_t *recipients);


/*
 * Functions that interface to the replica
 */
void mca_gpr_proxy_notify_recv(int status, ompi_process_name_t* sender,
			       ompi_buffer_t buffer, int tag,
			       void* cbdata);


/*
 * Internal functions
 */

ompi_registry_notify_id_t
mca_gpr_proxy_enter_notify_request(char *segment, ompi_registry_notify_action_t action,
				   ompi_registry_notify_cb_fn_t cb_func,
				   void *user_tag);

ompi_registry_notify_id_t
mca_gpr_proxy_remove_notify_request(ompi_registry_notify_id_t local_idtag);

int mca_gpr_proxy_set_remote_idtag(ompi_registry_notify_id_t local_idtag,
				   ompi_registry_notify_id_t remote_idtag);

#if defined(c_plusplus) || defined (__cplusplus) 
}
#endif

#endif
