/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
/** @file 
 */

#ifndef GPR_REPLICA_H
#define GPR_REPLICA_H


#include "ompi_config.h"

#include <time.h>

#include "threads/mutex.h"
#include "threads/condition.h"

#include "mca/gpr/base/base.h"

/*
 * typedefs needed in replica component
 */

/*
 * Define the registry "key"
 */
typedef uint32_t mca_gpr_replica_key_t;
#define MCA_GPR_REPLICA_KEY_MAX UINT32_MAX

/*
 * Registry "head"
 */
struct mca_gpr_replica_t {
    ompi_list_t registry;
    ompi_list_t segment_dict;
    ompi_list_t triggers;
    mca_gpr_replica_key_t lastkey;
    ompi_list_t freekeys;
};
typedef struct mca_gpr_replica_t mca_gpr_replica_t;

OBJ_CLASS_DECLARATION(mca_gpr_replica_t);

/*
 * Callback list "head"
 */
struct mca_gpr_replica_callbacks_t {
    ompi_list_item_t item;
    ompi_registry_notify_cb_fn_t cb_func;
    void *user_tag;
    ompi_registry_notify_message_t *message;
    ompi_process_name_t *requestor;
    ompi_registry_notify_id_t remote_idtag;
};
typedef struct mca_gpr_replica_callbacks_t mca_gpr_replica_callbacks_t;

OBJ_CLASS_DECLARATION(mca_gpr_replica_callbacks_t);

/*
 * List of process names who have notification turned OFF
 */
struct mca_gpr_replica_notify_off_t {
    ompi_list_item_t item;
    ompi_registry_notify_id_t sub_number;
    ompi_process_name_t *proc;
};
typedef struct mca_gpr_replica_notify_off_t mca_gpr_replica_notify_off_t;

OBJ_CLASS_DECLARATION(mca_gpr_replica_notify_off_t);

/** Dictionary of token-key pairs.
 * This structure is used to create a linked list of token-key pairs. All calls to
 * registry functions pass character string tokens for programming clarity - the ompi_keytable
 * structure is used to translate those strings into an integer key value, thus allowing
 * for faster searches of the registry. This structure is also used to return token-key pairs
 * from the dictionary in response to an ompi_registry_index() call.
 */
struct mca_gpr_replica_keytable_t {
    ompi_list_item_t item;         /**< Allows this item to be placed on a list */
    char *token;                   /**< Char string that defines the key */
    mca_gpr_replica_key_t key;     /**< Numerical value assigned by registry to represent token string */
};
typedef struct mca_gpr_replica_keytable_t mca_gpr_replica_keytable_t;

OBJ_CLASS_DECLARATION(mca_gpr_replica_keytable_t);

/** List of keys that describe a stored object.
 * Each object stored in the registry may have as many keys describing it as the
 * creator desires. This structure is used to create a linked list of keys
 * associated with each object.
 */
struct mca_gpr_replica_keylist_t {
    ompi_list_item_t item;      /**< Allows this item to be placed on a list */
    mca_gpr_replica_key_t key;  /**< Numerical key that defines stored object */
};
typedef struct mca_gpr_replica_keylist_t mca_gpr_replica_keylist_t;

OBJ_CLASS_DECLARATION(mca_gpr_replica_keylist_t);

/** List of trigger actions for a segment.
 * Each object can have an arbitrary number of subscribers desiring notification
 * upon specified actions being performed against the object. This structure is
 * used to create a linked list of subscribers for objects. Both synchro and
 * non-synchro triggers are supported.
 */
struct mca_gpr_replica_trigger_list_t {
    ompi_list_item_t item;                     /**< Allows this item to be placed on a list */
    mca_ns_base_jobid_t owning_job;            /**< Job ID of the process that registered trigger */
    ompi_registry_synchro_mode_t synch_mode;   /**< Synchro mode - ascending, descending, ... */
    ompi_registry_notify_action_t action;      /**< Bit-mask of actions that trigger non-synchro notification */
    ompi_registry_mode_t addr_mode;            /**< Addressing mode */
    mca_gpr_replica_key_t num_keys;            /**< Number of keys in array */
    mca_gpr_replica_key_t *keys;               /**< Array of keys describing objects to be counted */
    char **tokens;                             /**< Array of tokens - null terminated for use in other functions */
    uint32_t trigger;                          /**< Number of objects that trigger notification */
    uint32_t count;                            /**< Number of qualifying objects currently in segment */
    int8_t above_below;                        /**< Tracks transitions across level */
    ompi_registry_notify_id_t local_idtag;     /**< Tag into the list of notify structures */
};
typedef struct mca_gpr_replica_trigger_list_t mca_gpr_replica_trigger_list_t;

#define MCA_GPR_REPLICA_TRIGGER_ABOVE_LEVEL   (int8_t) 1
#define MCA_GPR_REPLICA_TRIGGER_BELOW_LEVEL   (int8_t) -1
#define MCA_GPR_REPLICA_TRIGGER_AT_LEVEL      (int8_t) 0

/* define a few action flags for trigger evaluation
 */
#define MCA_GPR_REPLICA_OBJECT_ADDED      (int8_t) 1
#define MCA_GPR_REPLICA_OBJECT_DELETED    (int8_t) 2
#define MCA_GPR_REPLICA_OBJECT_UPDATED    (int8_t) 3
#define MCA_GPR_REPLICA_SUBSCRIBER_ADDED  (int8_t) 4


OBJ_CLASS_DECLARATION(mca_gpr_replica_trigger_list_t);

/** List of replicas that hold a stored object.
 * Each object can have an arbitrary number of replicas that hold a copy
 * of the object. The GPR requires that each object be replicated in at least
 * two locations. This structure is used to create a linked list of
 * replicas for the object.
 */
struct mca_gpr_replica_list_t {
    ompi_list_item_t item;         /**< Allows this item to be placed on a list */
    ompi_process_name_t *replica;  /**< Name of the replica */
};
typedef struct mca_gpr_replica_list_t mca_gpr_replica_list_t;

OBJ_CLASS_DECLARATION(mca_gpr_replica_list_t);

/** Write invalidate structure.
 * The structure used to indicate that an object has been updated somewhere else in the GPR.
 * The structure contains a flag indicating that the locally stored copy of the object
 * is no longer valid, a time tag indicating the time of the last known modification
 * of the object within the global registry, and the replica holding the last known
 * up-to-date version of the object.
 */
struct mca_gpr_replica_write_invalidate_t {
    bool invalidate;
    time_t last_mod;
    ompi_process_name_t *valid_replica;
};
typedef struct mca_gpr_replica_write_invalidate_t mca_gpr_replica_write_invalidate_t;


/** The core registry structure.
 * Each segment of the registry contains a linked list of registry entries. This structure
 * represents a link in that list. The structure contains a linked list of the keys that
 * define this particular object, the size of the object, a pointer to the object, and a linked
 * list of subscribers to this object. Objects are stored as unsigned bytes - knowledge of any
 * structure within the objects is the responsibility of the calling functions. The repository
 * has no knowledge of what is in the structure, nor any way of determining such structure.
 *
 * At this time, no security is provided on an object-level basis. Thus, all requests for an
 * object are automatically granted. This may be changed at some future time by adding an
 * "authorization" linked list of ID's and their access rights to this structure.
 */
struct mca_gpr_replica_core_t {
    ompi_list_item_t item;                         /**< Allows this item to be placed on a list */
    mca_gpr_replica_key_t num_keys;                /**< Number of keys in array */
    mca_gpr_replica_key_t *keys;                   /**< Array of keys that define stored object */
    ompi_registry_object_size_t object_size;       /**< Size of stored object, in bytes */
    ompi_registry_object_t object;                /**< Pointer to stored object */
    ompi_list_t replicas;                          /**< Linked list of replicas that also contain this object */
    mca_gpr_replica_write_invalidate_t write_invalidate;   /**< Structure containing write invalidate info */
};
typedef struct mca_gpr_replica_core_t mca_gpr_replica_core_t;

OBJ_CLASS_DECLARATION(mca_gpr_replica_core_t);

/** Registry segment definition.
 * The registry is subdivided into segments, each defining a unique domain. The "universe" segment
 * is automatically created to allow the exchange of information supporting universe-level functions.
 * Similarly, a segment is automatically created for each MPI CommWorld within the universe - the
 * name for that segment is stored in each CommWorld's ompi_system_info structure so program
 * elements within that CommWorld can access it. The segment structure serves as the "head" of a linked
 * list of registry elements for that segment. Each segment also holds its own token-key dictionary
 * to avoid naming conflicts between tokens from CommWorlds sharing a given universe.
 */
struct mca_gpr_replica_segment_t {
    ompi_list_item_t item;             /**< Allows this item to be placed on a list */
    char *name;                        /**< Name of the segment */
    mca_ns_base_jobid_t owning_job;    /**< Job that "owns" this segment */
    mca_gpr_replica_key_t key;         /**< Key corresponding to name of registry segment */
    mca_gpr_replica_key_t lastkey;     /**< Highest key value used */
    ompi_list_t registry_entries;      /**< Linked list of stored objects within this segment */
    ompi_list_t triggers;              /**< List of triggers on this segment */
    bool triggers_active;              /**< Indicates if triggers are active or not */
    ompi_list_t keytable;              /**< Token-key dictionary for this segment */
    ompi_list_t freekeys;              /**< List of keys that have been made available */
};
typedef struct mca_gpr_replica_segment_t mca_gpr_replica_segment_t;

OBJ_CLASS_DECLARATION(mca_gpr_replica_segment_t);


struct mca_gpr_replica_notify_request_tracker_t {
    ompi_list_item_t item;                   /**< Allows this item to be placed on a list */
    ompi_process_name_t *requestor;          /**< Name of requesting process */
    ompi_registry_notify_cb_fn_t callback;   /**< Function to be called for notificaiton */
    void *user_tag;                          /**< User-provided tag for callback function */
    ompi_registry_notify_id_t local_idtag;   /**< Local ID tag of associated subscription */
    ompi_registry_notify_id_t remote_idtag;  /**< Remote ID tag of subscription */
    mca_gpr_replica_segment_t *segptr;       /**< Pointer to segment that subscription was
                                                  placed upon */
    ompi_registry_notify_action_t action;    /**< The action that triggers the request */
};
typedef struct mca_gpr_replica_notify_request_tracker_t mca_gpr_replica_notify_request_tracker_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_gpr_replica_notify_request_tracker_t);


/*
 * globals needed within component
 */
extern mca_gpr_replica_t mca_gpr_replica_head;                    /**< Head of the entire registry */
extern ompi_list_t mca_gpr_replica_notify_request_tracker;        /**< List of requested notifications */
extern ompi_list_t mca_gpr_replica_callbacks;                     /**< List of callbacks currently pending */
extern ompi_list_t mca_gpr_replica_notify_off_list;               /**< List of processes and subscriptions with notify turned off */
extern ompi_registry_notify_id_t mca_gpr_replica_last_notify_id_tag;    /**< Next available notify id tag */
extern ompi_list_t mca_gpr_replica_free_notify_id_tags;           /**< List of free notify id tags */
extern int mca_gpr_replica_debug;                                 /**< Debug flag to control debugging output */
extern ompi_mutex_t mca_gpr_replica_mutex;                        /**< Thread lock for registry functions */
extern bool mca_gpr_replica_compound_cmd_mode;                    /**< Indicates if we are building compound cmd */
extern bool mca_gpr_replica_exec_compound_cmd_mode;               /**< Indicates if we are executing compound cmd */
extern ompi_buffer_t mca_gpr_replica_compound_cmd;                /**< Compound cmd buffer */
extern ompi_mutex_t mca_gpr_replica_wait_for_compound_mutex;      /**< Lock to protect build compound cmd */
extern ompi_condition_t mca_gpr_replica_compound_cmd_condition;   /**< Condition variable to control thread access to build compound cmd */
extern int mca_gpr_replica_compound_cmd_waiting;                  /**< Count number of threads waiting to build compound cmd */
extern bool mca_gpr_replica_silent_mode;                          /**< Indicates if local silent mode active */


/*
 * Module open / close
 */
int mca_gpr_replica_open(void);
int mca_gpr_replica_close(void);


/*
 * Startup / Shutdown
 */
mca_gpr_base_module_t *mca_gpr_replica_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority);
int mca_gpr_replica_finalize(void);

/*
 * Implemented registry functions - see gpr.h for documentation
 */

/*
 * Compound cmd functions
 */
int mca_gpr_replica_begin_compound_cmd(void);

int mca_gpr_replica_stop_compound_cmd(void);

ompi_list_t* mca_gpr_replica_exec_compound_cmd(bool return_requested);

/*
 * Mode operations
 */
void mca_gpr_replica_silent_mode_on(void);

void mca_gpr_replica_silent_mode_off(void);

void mca_gpr_replica_notify_off(ompi_registry_notify_id_t sub_number);
void mca_gpr_replica_notify_off_nl(ompi_process_name_t *proc, ompi_registry_notify_id_t sub_number);

void mca_gpr_replica_triggers_active(mca_ns_base_jobid_t jobid);
void mca_gpr_replica_triggers_active_nl(mca_ns_base_jobid_t jobid);

void mca_gpr_replica_triggers_inactive(mca_ns_base_jobid_t jobid);
void mca_gpr_replica_triggers_inactive_nl(mca_ns_base_jobid_t jobid);

void mca_gpr_replica_notify_on(ompi_registry_notify_id_t sub_number);
void mca_gpr_replica_notify_on_nl(ompi_process_name_t *proc, ompi_registry_notify_id_t sub_number);

int mca_gpr_replica_assume_ownership(char *segment);
int mca_gpr_replica_assume_ownership_nl(mca_gpr_replica_segment_t *seg,
					mca_ns_base_jobid_t jobid);

/*
 * Delete-index functions
 */
int mca_gpr_replica_delete_segment(char *segment);
void  mca_gpr_replica_delete_segment_nl(mca_gpr_replica_segment_t *seg);

int mca_gpr_replica_delete_object(ompi_registry_mode_t addr_mode,
			      char *segment, char **tokens);
int mca_gpr_replica_delete_object_nl(ompi_registry_mode_t addr_mode,
				     mca_gpr_replica_segment_t *seg,
				     mca_gpr_replica_key_t *keys,
				     int num_keys);

ompi_list_t* mca_gpr_replica_index(char *segment);
ompi_list_t* mca_gpr_replica_index_nl(mca_gpr_replica_segment_t *seg);

/*
 * Cleanup functions
 */
void mca_gpr_replica_cleanup_job(mca_ns_base_jobid_t jobid);
void mca_gpr_replica_cleanup_job_nl(mca_ns_base_jobid_t jobid);

void mca_gpr_replica_cleanup_proc(bool purge, ompi_process_name_t *proc);
void mca_gpr_replica_cleanup_proc_nl(bool purge, ompi_process_name_t *proc);

/*
 * Put-get functions
 */
int mca_gpr_replica_put(ompi_registry_mode_t addr_mode, char *segment,
			char **tokens, ompi_registry_object_t object,
			ompi_registry_object_size_t size);
int mca_gpr_replica_put_nl(ompi_registry_mode_t addr_mode,
			   mca_gpr_replica_segment_t *seg,
			   mca_gpr_replica_key_t *keys,
			   int num_keys, ompi_registry_object_t object,
			   ompi_registry_object_size_t size,
			   int8_t *action_taken);

ompi_list_t* mca_gpr_replica_get(ompi_registry_mode_t addr_mode,
				 char *segment, char **tokens);
void mca_gpr_replica_get_nl(ompi_list_t *answer, ompi_registry_mode_t addr_mode,
			    mca_gpr_replica_segment_t *seg,
			    mca_gpr_replica_key_t *keys,
			    int num_keys);

/*
 * Subscribe functions
 */
ompi_registry_notify_id_t mca_gpr_replica_subscribe(ompi_registry_mode_t addr_mode,
			  ompi_registry_notify_action_t action,
			  char *segment, char **tokens,
			  ompi_registry_notify_cb_fn_t cb_func, void *user_tag);
int mca_gpr_replica_subscribe_nl(ompi_registry_mode_t addr_mode,
				 ompi_registry_notify_action_t action,
				 mca_gpr_replica_segment_t *seg,
				 mca_gpr_replica_key_t *keys,
				 int num_keys,
				 ompi_registry_notify_id_t id_tag);

int mca_gpr_replica_unsubscribe(ompi_registry_notify_id_t sub_number);
ompi_registry_notify_id_t mca_gpr_replica_unsubscribe_nl(ompi_registry_notify_id_t sub_number);

/*
 * Synchro functions
 */
ompi_registry_notify_id_t mca_gpr_replica_synchro(ompi_registry_synchro_mode_t synchro_mode,
			ompi_registry_mode_t addr_mode,
			char *segment, char **tokens, int trigger,
			ompi_registry_notify_cb_fn_t cb_func, void *user_tag);
int mca_gpr_replica_synchro_nl(ompi_registry_synchro_mode_t synchro_mode,
			       ompi_registry_mode_t addr_mode,
			       mca_gpr_replica_segment_t *seg,
			       mca_gpr_replica_key_t *keys,
			       int num_keys,
			       int trigger,
			       ompi_registry_notify_id_t id_tag);

int mca_gpr_replica_cancel_synchro(ompi_registry_notify_id_t synch_number);
ompi_registry_notify_id_t mca_gpr_replica_cancel_synchro_nl(ompi_registry_notify_id_t synch_number);

/*
 * Dump function
 */
void mca_gpr_replica_dump(int output_id);
void mca_gpr_replica_dump_nl(ompi_buffer_t buffer);


/*
 * Messaging functions
 */
void mca_gpr_replica_deliver_notify_msg(ompi_registry_notify_action_t state,
					ompi_registry_notify_message_t *message);


/*
 * Test internals
 */
ompi_list_t* mca_gpr_replica_test_internals(int level);

/*
 * Startup/shutdown functions
 */
ompi_buffer_t mca_gpr_replica_get_startup_msg(mca_ns_base_jobid_t jobid,
					      ompi_list_t *recipients);

ompi_buffer_t mca_gpr_replica_get_shutdown_msg(mca_ns_base_jobid_t jobid,
					       ompi_list_t *recipients);

ompi_buffer_t
mca_gpr_replica_construct_startup_shutdown_msg_nl(int mode,
						  mca_ns_base_jobid_t jobid,
						  ompi_list_t *recipients);

/*
 * Functions that interface to the proxy, but aren't available outside the gpr subsystem
 */
void mca_gpr_replica_recv(int status, ompi_process_name_t* sender,
			  ompi_buffer_t buffer, int tag,
			  void* cbdata);

void mca_gpr_replica_remote_notify(ompi_process_name_t *recipient, int recipient_tag,
			       ompi_registry_notify_message_t *message);

#endif
