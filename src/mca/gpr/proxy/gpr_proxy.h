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
#ifndef ORTE_GPR_PROXY_H
#define ORTE_GPR_PROXY_H


#include "orte_config.h"

#include "include/orte_types.h"
#include "class/ompi_object.h"
#include "class/orte_pointer_array.h"
#include "dps/dps_types.h"
#include "util/proc_info.h"

#include "mca/gpr/base/base.h"

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
     ompi_object_t super;                    /**< Allows this to be an object */
     int index;                              /**< Index of this callback */
     orte_gpr_notify_cb_fn_t callback;       /**< Function to be called for notificaiton */
     void *user_tag;                         /**< User-provided tag for callback function */
} orte_gpr_proxy_subscriber_t;

OBJ_CLASS_DECLARATION(orte_gpr_proxy_subscriber_t);

struct orte_gpr_proxy_notify_tracker_t {
    ompi_object_t super;                    /**< Allows this to be an object */
    orte_gpr_notify_id_t remote_idtag;      /**< Remote ID tag of subscription */
    orte_pointer_array_t *callbacks;        /**< Array of registered callbacks for this subscription */
};
typedef struct orte_gpr_proxy_notify_tracker_t orte_gpr_proxy_notify_tracker_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(orte_gpr_proxy_notify_tracker_t);

#define ORTE_GPR_PROXY_MAX_SIZE INT32_MAX
#define ORTE_GPR_PROXY_BLOCK_SIZE 100



/*
 * globals used within proxy component
 */
typedef struct {
    int debug;
    int32_t block_size;
    int32_t max_size;
    orte_pointer_array_t *notify_tracker;
    ompi_mutex_t mutex;
    bool compound_cmd_mode;
    orte_buffer_t *compound_cmd;
    ompi_mutex_t wait_for_compound_mutex;
    ompi_condition_t compound_cmd_condition;
    int compound_cmd_waiting;
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
                            
int orte_gpr_proxy_index(char *segment, size_t *cnt, char **index);

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
int orte_gpr_proxy_put(int cnt, orte_gpr_value_t **values);

int orte_gpr_proxy_put_nb(int cnt, orte_gpr_value_t **values,
                          orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);
                      
int orte_gpr_proxy_get(orte_gpr_addr_mode_t addr_mode,
                                char *segment, char **tokens, char **keys,
                                int *cnt, orte_gpr_value_t ***values);

int orte_gpr_proxy_get_nb(orte_gpr_addr_mode_t addr_mode,
                                char *segment, char **tokens, char **keys,
                                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag);


/*
 * Subscribe functions
 */
int orte_gpr_proxy_subscribe(orte_gpr_notify_action_t action,
                             int num_subs,
                             orte_gpr_subscription_t **subscriptions,
                             int num_trigs,
                             orte_gpr_value_t **trigs,
                             orte_gpr_notify_id_t *sub_number);

int orte_gpr_proxy_unsubscribe(orte_gpr_notify_id_t sub_number);


/*
 * Diagnostic functions
 */
int orte_gpr_proxy_dump_all(int output_id);

int orte_gpr_proxy_dump_segments(int output_id);

int orte_gpr_proxy_dump_triggers(int output_id);


/*
 * General operations
 */
int orte_gpr_proxy_preallocate_segment(char *name, int num_slots);

int orte_gpr_proxy_deliver_notify_msg(orte_gpr_notify_message_t *message);

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
orte_gpr_proxy_enter_notify_request(orte_gpr_notify_id_t *local_idtag,
                    int cnt, orte_gpr_subscription_t **subscriptions);

int
orte_gpr_proxy_remove_notify_request(orte_gpr_notify_id_t local_idtag,
                                     orte_gpr_notify_id_t *remote_idtag);

int orte_gpr_proxy_set_remote_idtag(orte_gpr_notify_id_t local_idtag,
                                     orte_gpr_notify_id_t remote_idtag);

#endif
