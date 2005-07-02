/* -*- C -*-
 * 
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
 *
 */
/** @file 
 */

#ifndef ORTE_GPR_REPLICA_H
#define ORTE_GPR_REPLICA_H


#include "orte_config.h"

#include <time.h>

#include "class/orte_bitmap.h"
#include "class/orte_pointer_array.h"
#include "class/orte_value_array.h"

#include "threads/mutex.h"
#include "threads/condition.h"

#include "mca/ns/ns_types.h"

#include "mca/gpr/base/base.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * typedefs needed in replica component
 */

/* JMS: This is only INT_MAX until bug 1345 is fixed, because this
   value is used to set an MAC parameter, which can [currently] only
   take an int. */
#define ORTE_GPR_REPLICA_MAX_SIZE INT_MAX
#define ORTE_GPR_REPLICA_BLOCK_SIZE 100


typedef size_t orte_gpr_replica_itag_t;
#define ORTE_GPR_REPLICA_ITAG_MAX SIZE_MAX


typedef uint8_t orte_gpr_replica_addr_mode_t;

#define ORTE_GPR_REPLICA_AND    (uint8_t)0x01
#define ORTE_GPR_REPLICA_OR     (uint8_t)0x02
#define ORTE_GPR_REPLICA_XAND   (uint8_t)0x04
#define ORTE_GPR_REPLICA_XOR    (uint8_t)0x08
#define ORTE_GPR_REPLICA_NOT    (uint8_t)0x40


/* define a few action flags for trigger evaluation
 */
#define ORTE_GPR_REPLICA_NO_ACTION         (int8_t) 0x00
#define ORTE_GPR_REPLICA_ENTRY_ADDED       (int8_t) 0x01
#define ORTE_GPR_REPLICA_ENTRY_DELETED     (int8_t) 0x02
#define ORTE_GPR_REPLICA_ENTRY_CHANGED     (int8_t) 0x04
#define ORTE_GPR_REPLICA_ENTRY_CHG_TO      (int8_t) 0x08
#define ORTE_GPR_REPLICA_ENTRY_CHG_FRM     (int8_t) 0x10


typedef int8_t orte_gpr_replica_action_t;

/*
 * Local subscription tracker for use by processes
 * that are operating on the same node as the replica
 */
typedef struct {
     ompi_object_t super;                   /**< Allows this to be an object */
     orte_gpr_subscription_id_t id;         /**< id of this subscription */
     orte_gpr_notify_cb_fn_t callback;      /**< Function to be called for notificaiton */
     void *user_tag;                        /**< User-provided tag for callback function */
} orte_gpr_replica_local_subscriber_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_local_subscriber_t);


typedef struct {
    int debug;
    int isolate;
    size_t block_size;
    size_t max_size;
    ompi_mutex_t mutex;
    size_t num_local_subs;
    orte_pointer_array_t *local_subscriptions;
    size_t trig_cntr;
    size_t num_srch_cptr;
    orte_pointer_array_t *srch_cptr;
    orte_pointer_array_t *sub_ptrs;
    size_t num_srch_ival;
    orte_pointer_array_t *srch_ival;
    size_t num_acted_upon;
    orte_pointer_array_t *acted_upon;
    orte_bitmap_t srch_itag;
} orte_gpr_replica_globals_t;


/** Dictionary of string-itag pairs.
 * This structure is used to create a linked list of string-itag pairs. All calls to
 * registry functions pass character strings for programming clarity - the replica_dict
 * structure is used to translate those strings into an integer itag value, thus allowing
 * for faster searches of the registry.
 */
struct orte_gpr_replica_dict_t {
    char *entry;                   /**< Char string that defines the itag */
    orte_gpr_replica_itag_t itag;  /**< Numerical value assigned by registry to represent string */
};
typedef struct orte_gpr_replica_dict_t orte_gpr_replica_dict_t;

/*
 * Registry "head"
 * The registry "head" contains:
 * 
 * (2) the next available itag for the segment dictionary.
 * 
 * (3) a managed array of pointers to segment objects.
 *
 * (4) a managed array of pointers to triggers acting on the entire registry
 * 
 */
struct orte_gpr_replica_t {
    orte_pointer_array_t *segments;  /**< Managed array of pointers to segment objects */
    size_t num_segs;
    orte_pointer_array_t *triggers;     /**< Managed array of pointers to triggers */
    size_t num_trigs;
    orte_pointer_array_t *subscriptions; /**< Managed array of pointers to subscriptions */
    size_t num_subs;
    bool processing_callbacks;
    ompi_list_t callbacks;          /**< List of callbacks to be processed */
};
typedef struct orte_gpr_replica_t orte_gpr_replica_t;


/** Registry segment definition.
 * The registry is subdivided into segments, each defining a unique domain. The "universe" segment
 * is automatically created to allow the exchange of information supporting universe-level functions.
 * Similarly, a segment is automatically created for each MPI CommWorld within the universe - the
 * name for that segment is stored in each CommWorld's ompi_system_info structure so program
 * elements within that CommWorld can access it. The segment structure serves as the "head" of a linked
 * list of registry elements for that segment. Each segment also holds its own token-itag dictionary
 * to avoid naming conflicts between tokens from CommWorlds sharing a given universe.
 */
struct orte_gpr_replica_segment_t {
    ompi_object_t super;                /**< Make this an object */
    char *name;                         /**< Name of the segment */
    orte_gpr_replica_itag_t itag;       /**< itag of this segment */
    size_t num_dict_entries;
    orte_pointer_array_t *dict;         /**< Managed array of dict structs */
    size_t num_containers;
    orte_pointer_array_t *containers;   /**< Managed array of pointers to containers on this segment */
};
typedef struct orte_gpr_replica_segment_t orte_gpr_replica_segment_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_segment_t);


/** The core registry structure.
 * Each segment of the registry contains an array of registry containers, each composed
 * of:
 * 
 * (1) An object structure that allows the structure to be treated with the OBJ 
 * memory management system
 * 
 * (2) An array of itags that define the container - these are 1:1 correspondents with
 * the character string tokens provided by caller
 * 
 * (3) An array of indices into the trigger notifier array - each index points to
 * a notifier whose trigger refers to this container.
 * 
 * (4) An array of pointers to keyval objects that actually hold the data.
 * 
 * At this time, no security is provided on an object-level basis. Thus, all requests for an
 * object are automatically granted. This may be changed at some future time by adding an
 * "authorization" linked list of ID's and their access rights to this structure.
 */
struct orte_gpr_replica_container_t {
    ompi_object_t super;              /**< Make this an object */
    size_t index;                        /**< Location in the pointer array */
    orte_gpr_replica_itag_t *itags;   /**< Array of itags that define this container */
    size_t num_itags;                    /**< Number of itags in array */
    orte_pointer_array_t *itagvals;   /**< Array of itagval pointers */
    size_t num_itagvals;                /**< Number of itagvals in container */
    orte_value_array_t itaglist;      /**< Array of itags from all itagvals - used for rapid search */
};
typedef struct orte_gpr_replica_container_t orte_gpr_replica_container_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_container_t);


/* The itag-value pair for storing data entries in the registry
 */
typedef struct {
    ompi_object_t super;                /**< required for this to be an object */
    size_t index;                          /**< index of this itagval on the container array */
    orte_gpr_replica_itag_t itag;       /**< itag for this value's key */
    orte_data_type_t type;              /**< the type of value stored */
    orte_gpr_value_union_t value;       /**< Actual stored value */
} orte_gpr_replica_itagval_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_itagval_t);

/* The equivalent of the value structure, only using internal
 * itags for the tokens/keys and pointers to internal structures
 */
typedef struct {
    ompi_object_t super;    /**< Makes this an object */
    size_t index;
    /* the segment upon which this data is located */
    orte_gpr_replica_segment_t *seg;
    /* describe the data */
    orte_gpr_addr_mode_t addr_mode; /**< Tokens/keys addressing mode */
    orte_value_array_t tokentags;   /**< Array of tokens defining which containers are affected */
    orte_value_array_t keytags;     /**< Array of keys defining which key-value pairs are affected */
} orte_gpr_replica_ivalue_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_ivalue_t);


typedef struct {
    ompi_object_t super;
    orte_gpr_replica_segment_t *seg;
    orte_gpr_replica_container_t *cptr;
    orte_gpr_replica_itagval_t *iptr;
    orte_gpr_replica_itagval_t trigger_level;
} orte_gpr_replica_counter_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_counter_t);

typedef struct {
    ompi_object_t super;
    /* index of this entry in requestor array */
    size_t index;
    /* process name of the recipient - set to NULL if local */
    orte_process_name_t *requestor;
    /* idtag associated with this subscription */
    orte_gpr_subscription_id_t idtag;
    /* for a local subscription, where this block of data goes */
    orte_gpr_notify_cb_fn_t callback;  /**< Function to be called for notification */
    void *user_tag;                    /**< User-provided tag for callback function */
} orte_gpr_replica_requestor_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_requestor_t);

typedef struct {
    ompi_object_t super;                    /**< Makes this an object */
    /* index of this entry in subscription array - corresponds to local idtag */
    size_t index;
    /* name of this subscription, if provided */
    char *name;
    /* boolean indicating if this subscription is active or not */
    bool active;
    /* boolean indicating that this subscription
     * should be removed after processing
     * is completed
     */
    bool cleanup;
    /* action flags describing when the subscription should
     * generate a notification message. This can be NULL if
     * the subscription only operates in conjunction
     * with a trigger
     */
    orte_gpr_notify_action_t action;
    /* Array of ivalues that describe the data to be
     * returned when this subscription is "fired"
     */
    size_t num_values;
    orte_pointer_array_t *values;
    /*
     * Array of requestors that are "attached" to this subscription
     */
    size_t num_requestors;
    orte_pointer_array_t *requestors;
} orte_gpr_replica_subscription_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_subscription_t);


typedef struct {
    ompi_object_t super;
    /* index of this entry in array */
    size_t index;
    /* process name of the requestor - set to NULL if local */
    orte_process_name_t *requestor;
    /* requestor's id for this trigger */
    orte_gpr_trigger_id_t idtag;
} orte_gpr_replica_trigger_requestor_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_trigger_requestor_t);


struct orte_gpr_replica_trigger_t {
    ompi_object_t super;                            /**< Make this an object */
    /* name of this trigger, if provided */
    char *name;
    /* index of this trigger in the triggers array - corresponds to local idtag */
    size_t index;
    /* array of requestors that have "attached" themselves to this trigger */
    size_t num_attached;
    orte_pointer_array_t *attached;
    /* the action that causes the trigger to be fired */
    orte_gpr_notify_action_t action;
    /* flag that indicates this trigger is a one-shot, has fired and
     * now should be cleaned up
     */
    bool one_shot_fired;
    /* pointers to the counters being monitored. This could
     * be counters we are using ourselves, or could be counters being run by someone
     * else. For those triggers that fire at a specified level (as opposed to
     * comparing values in two or more counters), store the trigger level for
     * each counter that we are monitoring until they reach a specified level.
     */
    size_t num_counters;
    orte_pointer_array_t *counters;
    /* a pointer to the subscriptions associated with this trigger. These
     * describe the data that will be returned when the trigger fires, and to
     * whom and where it goes.
     */
    size_t num_subscriptions;
    orte_pointer_array_t *subscriptions;
};
typedef struct orte_gpr_replica_trigger_t orte_gpr_replica_trigger_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_trigger_t);


/*
 * Action taken object - used to track what action was taken against what
 * registry object during the course of a registry request. For example, if
 * a PUT modifies an existing registry entry, then we store a pointer to that
 * entry and a flag indicating that it was modified. This info is required for
 * processing notification subscriptions.
 */
typedef struct {
    ompi_object_t super;        /**< Make this an object */
    orte_gpr_replica_action_t action;
    orte_gpr_replica_segment_t *seg;
    orte_gpr_replica_container_t *cptr;
    orte_gpr_replica_itagval_t *iptr;
} orte_gpr_replica_action_taken_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_action_taken_t);

/*
 * Callback list objects
 */
struct orte_gpr_replica_callbacks_t {
    ompi_list_item_t item;
    orte_process_name_t *requestor;
    orte_gpr_notify_message_t *message;
};
typedef struct orte_gpr_replica_callbacks_t orte_gpr_replica_callbacks_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_callbacks_t);

/** List of replicas that hold a stored entry.
 * Each entry can have an arbitrary number of replicas that hold a copy
 * of the entry. The GPR requires that each entry be replicated in at least
 * two locations. This structure is used to create a linked list of
 * replicas for the entry.
 * 
 * THIS IS NOT IMPLEMENTED YET
 */
struct orte_gpr_replica_list_t {
    ompi_list_item_t item;         /**< Allows this item to be placed on a list */
    orte_process_name_t *replica;  /**< Name of the replica */
};
typedef struct orte_gpr_replica_list_t orte_gpr_replica_list_t;

OBJ_CLASS_DECLARATION(orte_gpr_replica_list_t);

/** Write invalidate structure.
 * The structure used to indicate that an entry has been updated somewhere else in the GPR.
 * The structure contains a flag indicating that the locally stored copy of the entry
 * is no longer valid, a time tag indicating the time of the last known modification
 * of the entry within the global registry, and the replica holding the last known
 * up-to-date version of the entry.
 * 
 * THIS IS NOT IMPLEMENTED YET
 */
struct orte_gpr_replica_write_invalidate_t {
    bool invalidate;
    time_t last_mod;
    orte_process_name_t *valid_replica;
};
typedef struct orte_gpr_replica_write_invalidate_t orte_gpr_replica_write_invalidate_t;


/*
 * globals needed within component
 */
extern orte_gpr_replica_t orte_gpr_replica;
extern orte_gpr_replica_globals_t orte_gpr_replica_globals;


/*
 * Module open / close
 */
int orte_gpr_replica_open(void);
int orte_gpr_replica_close(void);


/*
 * Startup / Shutdown
 */
orte_gpr_base_module_t *orte_gpr_replica_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority);
int orte_gpr_replica_finalize(void);
int orte_gpr_replica_module_init(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
