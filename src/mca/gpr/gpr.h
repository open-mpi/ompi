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
 */
/** @file 
 * @page gpr_api
 */

/** 
 *  \brief General Purpose Registry (GPR) API
 *
 * The Open MPI General Purpose Registry (GPR) 
 */

#ifndef MCA_GPR_H_
#define MCA_GPR_H_

/*
 * includes
 */

#include "ompi_config.h"

#include <sys/types.h>
#include <limits.h>

#include "include/types.h"
#include "include/constants.h"
#include "class/ompi_list.h"
#include "util/bufpack.h"
#include "runtime/runtime_types.h"

#include "mca/mca.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/** Define the notification actions for the subscription system
 */
#define OMPI_REGISTRY_NOTIFY_NONE                   (uint16_t)0x0000   /**< Null case */
#define OMPI_REGISTRY_NOTIFY_MODIFICATION           (uint16_t)0x0001   /**< Notifies subscriber when object modified */
#define OMPI_REGISTRY_NOTIFY_ADD_SUBSCRIBER         (uint16_t)0x0002   /**< Notifies subscriber when another subscriber added */
#define OMPI_REGISTRY_NOTIFY_DELETE_ENTRY           (uint16_t)0x0004   /**< Notifies subscriber when object deleted */
#define OMPI_REGISTRY_NOTIFY_ADD_ENTRY              (uint16_t)0x0008   /**< Notifies subscriber when object added */
#define OMPI_REGISTRY_NOTIFY_ON_STARTUP             (uint16_t)0x0010   /**< Provide me with startup message - no data */
#define OMPI_REGISTRY_NOTIFY_ON_SHUTDOWN            (uint16_t)0x0020   /**< Provide me with shutdown message - no data */
#define OMPI_REGISTRY_NOTIFY_PRE_EXISTING           (uint16_t)0x0040   /**< Provide list of all pre-existing data */
#define OMPI_REGISTRY_NOTIFY_INCLUDE_STARTUP_DATA   (uint16_t)0x0080   /**< Provide data with startup message */
#define OMPI_REGISTRY_NOTIFY_INCLUDE_SHUTDOWN_DATA  (uint16_t)0x0100   /**< Provide data with shutdown message */
#define OMPI_REGISTRY_NOTIFY_ONE_SHOT               (uint16_t)0x0200   /**< Only trigger once - then delete subscription */
#define OMPI_REGISTRY_NOTIFY_ALL                    (uint16_t)0x8000   /**< Notifies subscriber upon any action */

typedef uint16_t ompi_registry_notify_action_t;

typedef uint32_t ompi_registry_notify_id_t;
#define OMPI_REGISTRY_NOTIFY_ID_MAX UINT32_MAX

/*
 * Define synchro mode flags
 */
#define OMPI_REGISTRY_SYNCHRO_MODE_NONE        (uint8_t)0x00   /**< No synchronization */
#define OMPI_REGISTRY_SYNCHRO_MODE_ASCENDING   (uint8_t)0x01   /**< Notify when trigger is reached, ascending mode */
#define OMPI_REGISTRY_SYNCHRO_MODE_DESCENDING  (uint8_t)0x02   /**< Notify when trigger is reached, descending mode */
#define OMPI_REGISTRY_SYNCHRO_MODE_LEVEL       (uint8_t)0x04   /**< Notify when trigger is reached, regardless of direction */
#define OMPI_REGISTRY_SYNCHRO_MODE_GT_EQUAL    (uint8_t)0x08   /**< Notify if level greater than or equal */
#define OMPI_REGISTRY_SYNCHRO_MODE_LT_EQUAL    (uint8_t)0x10   /**< Notify if level less than or equal */
#define OMPI_REGISTRY_SYNCHRO_MODE_CONTINUOUS  (uint8_t)0x80   /**< Notify whenever conditions are met */
#define OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT    (uint8_t)0x81   /**< Fire once, then terminate synchro command */
#define OMPI_REGISTRY_SYNCHRO_MODE_STARTUP     (uint8_t)0x82   /**< Indicates associated with application startup */
#define OMPI_REGISTRY_SYNCHRO_MODE_SHUTDOWN    (uint8_t)0x84   /**< Indicates associated with application shutdown */

typedef uint8_t ompi_registry_synchro_mode_t;

/** Return value for notify requests
 */
struct ompi_registry_notify_message_t {
    char *segment;                               /**< Name of originating segment */
    mca_ns_base_jobid_t owning_job;              /**< Job that owns that segment */
    ompi_list_t data;                            /**< List of data objects */
    ompi_registry_notify_action_t trig_action;   /**< If subscription, action that triggered message */
    ompi_registry_synchro_mode_t trig_synchro;   /**< If synchro, action that triggered message */
    uint32_t num_tokens;                         /**< Number of tokens in subscription/synchro */
    char **tokens;                               /**< List of tokens in subscription/synchro */
};
typedef struct ompi_registry_notify_message_t ompi_registry_notify_message_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_registry_notify_message_t);

/** Notify callback function */
typedef void (*ompi_registry_notify_cb_fn_t)(ompi_registry_notify_message_t *notify_msg, void *user_tag);



/** Define the addressing mode bit-masks for registry operations.
 */
#define OMPI_REGISTRY_NONE       (uint16_t)0x0000   /**< None */
#define OMPI_REGISTRY_OVERWRITE  (uint16_t)0x0001   /**< Overwrite Permission */
#define OMPI_REGISTRY_AND        (uint16_t)0x0002   /**< AND tokens together for search results */
#define OMPI_REGISTRY_OR         (uint16_t)0x0004   /**< OR tokens for search results */
#define OMPI_REGISTRY_XAND       (uint16_t)0x0008   /**< All tokens required, nothing else allowed */
#define OMPI_REGISTRY_XOR        (uint16_t)0x0010   /**< Any one of the tokens required, nothing else allowed */

typedef uint16_t ompi_registry_mode_t;

/** Define flag values for requesting return data from compound commands
 */

#define OMPI_REGISTRY_RETURN_REQUESTED      true    /**< Return information from compound command */
#define OMPI_REGISTRY_NO_RETURN_REQUESTED   false   /**< Do not return information from compound cmd */


/*
 * Define flag values for remote commands - only used internally
 */
#define MCA_GPR_DELETE_SEGMENT_CMD     (uint16_t)0x0001
#define MCA_GPR_PUT_CMD                (uint16_t)0x0002
#define MCA_GPR_DELETE_OBJECT_CMD      (uint16_t)0x0004
#define MCA_GPR_INDEX_CMD              (uint16_t)0x0008
#define MCA_GPR_SUBSCRIBE_CMD          (uint16_t)0x0010
#define MCA_GPR_UNSUBSCRIBE_CMD        (uint16_t)0x0020
#define MCA_GPR_SYNCHRO_CMD            (uint16_t)0x0040
#define MCA_GPR_CANCEL_SYNCHRO_CMD     (uint16_t)0x0080
#define MCA_GPR_GET_CMD                (uint16_t)0x0100
#define MCA_GPR_TEST_INTERNALS_CMD     (uint16_t)0x0200
#define MCA_GPR_NOTIFY_CMD             (uint16_t)0x0400   /**< Indicates a notify message */
#define MCA_GPR_DUMP_CMD               (uint16_t)0x2000
#define MCA_GPR_ASSUME_OWNERSHIP_CMD   (uint16_t)0x4000
#define MCA_GPR_NOTIFY_ON_CMD          (uint16_t)0x8000
#define MCA_GPR_NOTIFY_OFF_CMD         (uint16_t)0x8001
#define MCA_GPR_COMPOUND_CMD           (uint16_t)0x8010
#define MCA_GPR_GET_STARTUP_MSG_CMD    (uint16_t)0x8020
#define MCA_GPR_GET_SHUTDOWN_MSG_CMD   (uint16_t)0x8040
#define MCA_GPR_TRIGGERS_ACTIVE_CMD    (uint16_t)0x8080
#define MCA_GPR_TRIGGERS_INACTIVE_CMD  (uint16_t)0x8100
#define MCA_GPR_CLEANUP_JOB_CMD        (uint16_t)0x8200
#define MCA_GPR_CLEANUP_PROC_CMD       (uint16_t)0x8400
#define MCA_GPR_ERROR                  (uint16_t)0xffff

typedef uint16_t mca_gpr_cmd_flag_t;

/*
 * packing type definitions
 */
/* CAUTION - any changes here must also change corresponding
 * typedefs above
 */
#define MCA_GPR_OOB_PACK_CMD                OMPI_INT16
#define MCA_GPR_OOB_PACK_ACTION             OMPI_INT16
#define MCA_GPR_OOB_PACK_MODE               OMPI_INT16
#define MCA_GPR_OOB_PACK_OBJECT_SIZE        OMPI_INT32
#define MCA_GPR_OOB_PACK_SYNCHRO_MODE       OMPI_INT8
#define MCA_GPR_OOB_PACK_NOTIFY_ID          OMPI_INT32
#define MCA_GPR_OOB_PACK_BOOL               OMPI_INT8
#define MCA_GPR_OOB_PACK_STATUS_KEY         OMPI_INT8
#define MCA_GPR_OOB_PACK_EXIT_CODE          OMPI_INT8
#define MCA_GPR_OOB_PACK_JOBID              OMPI_JOBID
#define MCA_GPR_OOB_PACK_NAME               OMPI_NAME

/*
 * typedefs
 */

typedef void* ompi_registry_object_t;
typedef uint32_t ompi_registry_object_size_t;

/*
 * structures
 */

/** Return value structure for registry requests.
 * A request for information stored within the registry returns a linked list of values that
 * correspond to the provided tokens. The linked list is terminated by a "next" value of NULL.
 * Each link in the list contains a pointer to a copy of the registry object, and the size
 * of that object in bytes. Note that the pointer is to a \em copy of the object, and not
 * to the registry object itself. This prevents inadvertent modification of the registry, but
 * may require the recipient to release the structure's memory when done.
 */
struct ompi_registry_value_t {
    ompi_list_item_t item;                    /**< Allows this item to be placed on a list */
    ompi_registry_object_t object;           /**< Pointer to object being returned */
    ompi_registry_object_size_t object_size;  /**< Size of returned object, in bytes */
};
typedef struct ompi_registry_value_t ompi_registry_value_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_registry_value_t);

/** Return value structure for index requests.
 */
struct ompi_registry_index_value_t {
    ompi_list_item_t item;           /**< Allows this item to be placed on a list */
    char *token;                     /**< Pointer to the token string */
};
typedef struct ompi_registry_index_value_t ompi_registry_index_value_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_registry_index_value_t);

/** Return value structure for compound registry commands.
 * A compound registry command contains multiple registry commands, all transferred
 * in a single communication. Because of this, data returned by the individual
 * commands within the compound command must be separated out so it can be clearly
 * retrieved. This structure provides a wrapper for data returned by each of the
 * individual commands.
 */
struct ompi_registry_compound_cmd_results_t {
    ompi_list_item_t item;          /**< Allows this item to be placed on a list */
    int32_t status_code;            /**< Status code resulting from the command */
    ompi_list_t data;              /**< Any returned data coming from the command */
};
typedef struct ompi_registry_compound_cmd_results_t ompi_registry_compound_cmd_results_t;

OBJ_CLASS_DECLARATION(ompi_registry_compound_cmd_results_t);


/** Return value for test results on internal test
 */
struct ompi_registry_internal_test_results_t {
    ompi_list_item_t item;          /**< Allows this item to be placed on a list */
    char *test;
    char *message;
};
typedef struct ompi_registry_internal_test_results_t ompi_registry_internal_test_results_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_registry_internal_test_results_t);


struct mca_gpr_idtag_list_t {
    ompi_list_item_t item;
    ompi_registry_notify_id_t id_tag;
};
typedef struct mca_gpr_idtag_list_t mca_gpr_idtag_list_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_gpr_idtag_list_t);


/*
 * Component functions that MUST be provided
 */

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
 * @retval OMPI_SUCCESS Compound command recorder is active.
 * @retval OMPI_ERROR Compound command recorder did not activate.
 *
 * @code
 * ompi_registry.begin_compound_cmd();
 * @endcode
 *
 */
typedef int (*mca_gpr_base_module_begin_compound_cmd_fn_t)(void);

/*
 * Stop recording a compound command
 * Terminates the recording process and clears the buffer of any previous commands
 *
 * @param None
 * @retval OMPI_SUCCESS Recording stopped and buffer successfully cleared
 * @retval OMPI_ERROR Didn't work - no idea why it wouldn't
 *
 * @code
 * ompi_registry.stop_compound_cmd();
 * @endcode
 *
 */
typedef int (*mca_gpr_base_module_stop_compound_cmd_fn_t)(void);

/*
 * Execute the compound command
 * Execute the compound command that has been recorded. Any output from each command
 * is captured in a list that can be returned to the caller, depending upon the
 * value of the input parameter.
 *
 * @param OMPI_REGISTRY_RETURN_REQUESTED Data and status codes returned by commands in
 * the recorded compound command are to be returned in a list of ompi_registry_compound_cmd_value_t
 * structures.
 * @param OMPI_REGISTRY_NO_RETURN_REQUESTED Data and status codes returned by commands
 * in the recorded compound command are to be discarded.
 *
 * @retval return_values A list of ompi_registry_compound_cmd_value_t structures that
 * contain the results from each command (in sequence they were issued) of the compound command.
 * @retval NULL No values returned.
 *
 * @code
 * return_values = ompi_registry.exec_compound_cmd(OMPI_REGISTRY_RETURN_REQUESTED);
 *
 * ompi_registry.exec_compound_cmd(OMPI_REGISTRY_NO_RETURN_REQUESTED);
 * @endcode
 *
 */
typedef ompi_list_t* (*mca_gpr_base_module_exec_compound_cmd_fn_t)(bool return_requested);

/*
 * Turn return of status codes OFF.
 * All registry functions normally return a status code, with the exception of those
 * functions that return data values. This function sets a flag that turns OFF the
 * status code returns. Normally used to reduce network traffic by eliminating the
 * return of status codes. Commands will automatically return a default value of OMPI_SUCCESS.
 *
 * @param None
 * @retval None
 *
 * @code
 * ompi_registry.silent_mode_on();
 * @endcode
 */
typedef void (*mca_gpr_base_module_silent_mode_on_fn_t)(void);

/* Turn return of status codes ON.
 * All registry functions normally return a status code, with the exception of those
 * functions that return data values. This function sets a flag that turns ON the
 * status code returns (i.e., restores the default condition).
 *
 * @param None
 * @retval None
 *
 * @code
 * ompi_registry.silent_mode_off();
 * @endcode
 */
typedef void (*mca_gpr_base_module_silent_mode_off_fn_t)(void);

/* Turn off subscriptions for this process
 * Temporarily turn off subscriptions for this process on the registry. Until restored,
 * the specified subscription will be ignored - no message will be sent. Providing a
 * value of OMPI_REGISTRY_NOTIFY_ID_MAX for the subscription number will turn off ALL
 * subscriptions with this process as the subscriber.
 *
 * Note: synchro messages will continue to be sent - only messages from subscriptions
 * are affected.
 *
 * @param sub_number Notify id number of the subscription to be turned "off". A value
 * of OMPI_REGISTRY_NOTIFY_ID_MAX indicates that ALL subscriptions with this process as the subscriber are to be
 * turned "off" until further notice.
 *
 * @code
 * ompi_registry.notify_off(subscription_number);
 * @endcode
 */
typedef void (*mca_gpr_base_module_notify_off_fn_t)(ompi_registry_notify_id_t sub_number);

/* Turn on subscriptions for this process
 * Turn on subscriptions for this process on the registry. This is the default condition
 * for subscriptions, indicating that messages generated by triggered subscriptions are to
 * be sent to the subscribing process.
 *
 * @param sub_number Notify id number of the subscription to be turned "on". A value
 * of OMPI_REGISTRY_NOTIFY_ID_MAX indicates that ALL subscriptions with this process as the subscriber are to be
 * turned "on" until further notice.
 *
 * @code
 * ompi_registry.notify_on(subscription_number);
 * @endcode
 */
typedef void (*mca_gpr_base_module_notify_on_fn_t)(ompi_registry_notify_id_t sub_number);

/* Turn triggers on for this jobid
 * Activate all triggers for this jobid on the registry. Does not counteract the subscription on/off
 * for each process. When created, segments default to triggers being INACTIVE. All
 * subscriptions and synchros, therefore, are rendered inactive until the segment's
 * triggers are turned "on".
 *
 * @param jobid The jobid whose triggers are to be activated.
 *
 * @code
 * ompi_registry.triggers_active(jobid);
 * @endcode
 */
typedef void (*mca_gpr_base_module_triggers_active_fn_t)(mca_ns_base_jobid_t jobid);

/* Turn triggers off for this jobid.
 * Deactivate all triggers for the specified job. All subscriptions and synchros will be
 * rendered inactive regardless of recipients and/or conditions.
 *
 * @param jobid The jobid whose triggers are to be
 * deactivated.
 *
 * @code
 * ompi_registry.triggers_inactive(jobid);
 * @endcode
 */
typedef void (*mca_gpr_base_module_triggers_inactive_fn_t)(mca_ns_base_jobid_t jobid);

/*
 * Get the job startup message.
 * At the startup of any job, there is a set of information that needs to be sent to every
 * process - this is known as the job startup message. This function provides an entry point
 * for the controlling process (i.e., the one that is spawning the application - usually
 * mpirun) to obtain the job startup message so it can subsequently "broadcast" it to all
 * of the application's processes.
 *
 * @param jobid The id of the job being started.
 *
 * @param recipients A list of process names for the recipients - the input parameter
 * is a pointer to the list; the function returns the list in that location.
 *
 * @retval msg A packed buffer containing all the information required. This
 * information is obtained by gathering all data on all segments "owned" by the specified
 * jobid. The registry has NO knowledge of what is in the data elements, where it should go,
 * etc. The data from each segment is preceded by the name of the segment from which it came.
 * A function for parsing this message and distributing the data is provided elsewhere - such
 * functionality is beyond the purview of the registry.
 *
 * @code
 * msg_buffer = ompi_registry.get_startup_msg(jobid, recipients);
 * @endcode
 *
 */
typedef ompi_buffer_t (*mca_gpr_base_module_get_startup_msg_fn_t)(mca_ns_base_jobid_t jobid,
								  ompi_list_t *recipients);

/*
 * Get the job shutdown message.
 * Upon completing, each process waits for a final synchronizing message to arrive. This ensures
 * that process all exit together and prevents, for example, "hangs" as one process tries to talk
 * to another that has completed. Not much data should need to be shared during this operation, but
 * this function provides an entry point in case something is identified.
 *
 * @param jobid The id of the job being shutdown.
 * @param recipients A list of process names for the recipients - the input parameter
 * is a pointer to the list; the function returns the list in that location.
 *
 * @retval msg A packed buffer containing the required information. At the moment, this will be an
 * empty buffer as no information has yet been identified.
 *
 * @code
 * msg_buffer = ompi_registry.get_shutdown_msg(jobid, recipients);
 * @endcode
 *
 */
typedef ompi_buffer_t (*mca_gpr_base_module_get_shutdown_msg_fn_t)(mca_ns_base_jobid_t jobid,
								   ompi_list_t *recipients);

/* Cleanup a job from the registry
 * Remove all references to a given job from the registry. This includes removing
 * all segments "owned" by the job, and removing all process names from dictionaries
 * in the registry.
 *
 * @param jobid The jobid to be cleaned up.
 *
 * @code
 * ompi_registry.cleanup_job(jobid);
 * @endcode
 *
 */
typedef void (*mca_gpr_base_module_cleanup_job_fn_t)(mca_ns_base_jobid_t jobid);

/* Cleanup a process from the registry
 * Remove all references to a given process from the registry. This includes removing
 * the process name from all dictionaries in the registry, all subscriptions, etc.
 * It also includes reducing any syncrhos on segments owned by the associated job.
 *
 * @param proc A pointer to the process name to be cleaned up.
 *
 * @code
 * ompi_registry.cleanup_process(&proc);
 * @endcode
 *
 */
typedef void (*mca_gpr_base_module_cleanup_proc_fn_t)(bool purge, ompi_process_name_t *proc);

/*
 * Delete a segment from the registry
 * This command removes an entire segment from the registry, including all data objects,
 * associated subscriptions, and synchros. This is a non-reversible process, so it should
 * be used with care.
 *
 * @param segment Character string specifying the name of the segment to be removed.
 *
 * @retval OMPI_SUCCESS Segment successfully removed.
 * @retval OMPI_ERROR Segment could not be removed for some reason - most
 * likely, the segment name provided was not found in the registry.
 *
 * @code
 * status_code = ompi_registry.delete_segment(segment);
 * @endcode
 */
typedef int (*mca_gpr_base_module_delete_segment_fn_t)(char *segment);

/*
 * Put a data object on the registry
 * 
 * @param mode The addressing mode to be used. Addresses are defined by the tokens provided
 * that describe the object being stored. The caller has the option of specifying how
 * those tokens are to be combined in describing the object. Passing a value of
 * "OMPI_REGISTRY_AND", for example, indicates that all provided tokens are to be used.
 * In contrast, a value of "OMPI_REGISTRY_OR" indicates that any of the provided tokens
 * can adequately describe the object. For the "put" command, only "OMPI_REGISTRY_XAND"
 * is accepted - in other words, the tokens must exactly match those of any existing
 * object in order for the object to be updated. In addition, the "OMPI_REGISTRY_OVERWRITE"
 * flag must be or'd into the mode to enable update of the data object. If a data object
 * is found with the identical token description, but OMPI_REGISTRY_OVERWRITE is NOT specified,
 * then an error will be generated - the data object will NOT be overwritten in this
 * situation.
 *
 * Upon completing the "put", all subscription and synchro requests registered on the
 * specified segment are checked and appropriately processed.
 *
 * @param segment A character string specifying the name of the segment upon which
 * the data object is to be placed.
 *
 * @param tokens A **char list of tokens describing the object.
 *
 * @param object An ompi_registry_object_t data object that is to be placed
 * on the registry. The registry will copy this data object onto the specified segment - the
 * calling program is responsible for freeing any memory, if appropriate.
 *
 * @param size An ompi_registry_object_size_t value indicating the size of the data
 * object in bytes.
 *
 * @retval OMPI_SUCCESS The data object has been stored on the specified segment, or the
 * corresponding existing data object has been updated.
 *
 * @retval OMPI_ERROR The data object was not stored on the specified segment, or the
 * corresponding existing data object was not found, or the object was found but the overwrite
 * flag was not set.
 *
 * @code
 * status_code = ompi_registry.put(mode, segment, tokens, object, object_size);
 * @endcode
 */
typedef int (*mca_gpr_base_module_put_fn_t)(ompi_registry_mode_t mode, char *segment,
					    char **tokens, ompi_registry_object_t object,
					    ompi_registry_object_size_t size);

/*
 * Get data from the registry.
 * Returns data from the registry. Given an addressing mode, segment name, and a set
 * of tokens describing the data object, the "get" function will search the specified
 * registry segment and return all data items that "match" the description. Addressing
 * modes specify how the provided tokens are to be combined to determine the match -
 * a value of "OMPI_REGISTRY_AND", for example, indictates that all the tokens must be
 * included in the object's description, but allows for other tokens to also be present.
 * A value of "OMPI_REGISTRY_XAND", in contrast, requires that all the tokens be present,
 * and that ONLY those tokens be present.
 *
 * The data is returned as a list of ompi_registry_value_t objects. The caller is
 * responsible for freeing this data storage. Only copies of the registry data are
 * returned - thus, any actions taken by the caller will NOT impact data stored on the
 * registry.
 *
 * @param addr_mode The addressing mode to be used in the search.
 * @param segment A character string indicating the name of the segment to be searched.
 * @param tokens A NULL-terminated **char list of tokens describing the objects to be
 * returned. A value of NULL indicates that ALL data on the segment is to be returned.
 *
 * @retval data_list A list of ompi_registry_value_t objects containing the data objects
 * returned by the specified search.
 *
 * @code
 * data_list = ompi_registry.get(mode, segment, tokens);
 * @endcode
 */
typedef ompi_list_t* (*mca_gpr_base_module_get_fn_t)(ompi_registry_mode_t addr_mode,
						     char *segment, char **tokens);

/*
 * Delete an object from the registry
 * Remove an object from the registry. Given an addressing mode, segment name, and a set
 * of tokens describing the data object, the function will search the specified
 * registry segment and delete all data items that "match" the description. Addressing
 * modes specify how the provided tokens are to be combined to determine the match -
 * a value of "OMPI_REGISTRY_AND", for example, indictates that all the tokens must be
 * included in the object's description, but allows for other tokens to also be present.
 * A value of "OMPI_REGISTRY_XAND", in contrast, requires that all the tokens be present,
 * and that ONLY those tokens be present.
 *
 * Note: A value of NULL for the tokens will delete ALL data items from the specified
 * segment. 
 *
 * @param addr_mode The addressing mode to be used in the search.
 * @param segment A character string indicating the name of the segment to be searched.
 * @param tokens A NULL-terminated **char list of tokens describing the objects to be
 * returned. A value of NULL indicates that ALL data on the segment is to be removed.
 *
 * @code
 * status_code = ompi_registry.delete_object(mode, segment, tokens);
 * @endcode
 */
typedef int (*mca_gpr_base_module_delete_object_fn_t)(ompi_registry_mode_t addr_mode,
						      char *segment, char **tokens);

/*
 * Obtain an index of a specified dictionary
 * The registry contains a dictionary at the global level (containing names of all the
 * segments) and a dictionary for each segment (containing the names of all tokens used
 * in that segment). This command allows the caller to obtain a list of all entries
 * in the specified dictionary.
 *
 * @param segment A character string indicating the segment whose dictionary is to be
 * indexed. A value of NULL indicates that the global level dictionary is to be used.
 *
 * @retval index_list A list of ompi_registry_index_value_t objects containing the
 * dictionary entries. A list of zero length is returned if the specified segment
 * cannot be found, or if the specified dictionary is empty.
 *
 * @code
 * index_list = ompi_registry.index(segment);
 * @endcode
 */
typedef ompi_list_t* (*mca_gpr_base_module_index_fn_t)(char *segment);

/*
 * Subscribe to be notified upon a specified action
 * The registry includes a publish/subscribe mechanism by which callers can be notified
 * upon certain actions occuring to data objects stored on the registry. This function
 * allows the caller to register for such notifications. The registry allows a subscription
 * to be placed upon any segment, and upon the entire registry if desired.
 *
 * @param addr_mode The addressing mode to be used in specifying the objects to be
 * monitored by this subscription.
 * @param action The actions which are to trigger a notification message. These can
 * be OR'd together from the defined registry action flags.
 * @param segment A character string indicating the name of the segment upon which the
 * subscription is being requested. A value of NULL indicates that the subscription
 * is to be placed on the entire registry - this should be done with caution as the
 * subscription will trigger on ALL registry events matching the specified action and
 * addressing, potentially including those from jobs other than the one generating the
 * subscription request.
 * @param tokens A NULL-terminated **char list of tokens describing the objects to be
 * monitored. A value of NULL indicates that ALL data on the segment is to be monitored.
 * @param cb_func The ompi_registry_notify_cb_fn_t callback function to be called when
 * a subscription is triggered. The data from each monitored object will be returned
 * to the callback function in an ompi_registry_notify_message_t structure.
 * @param user_tag A void* user-provided storage location that the caller can
 * use for its own purposes. A NULL value is acceptable.
 *
 * @retval sub_number The subscription number of this request. Callers should save this
 * number for later use if (for example) it is desired to temporarily turn "off" the subscription
 * or to permanently remove the subscription from the registry.
 *
 * @code
 * sub_number = ompi_registry.subscribe(addr_mode, action, segment, tokens, cb_func, user_tag);
 * @endcode
 */
typedef ompi_registry_notify_id_t (*mca_gpr_base_module_subscribe_fn_t)(ompi_registry_mode_t addr_mode,
						  ompi_registry_notify_action_t action,
						  char *segment, char **tokens,
						  ompi_registry_notify_cb_fn_t cb_func, void *user_tag);

/*
 * Cancel a subscription.
 * Once a subscription has been entered on the registry, a caller may choose to permanently
 * remove it at a later time. This function supports that request.
 *
 * @param sub_number The ompi_registry_notify_id_t value returned by the original subscribe
 * command.
 *
 * @retval OMPI_SUCCESS The subscription was removed.
 * @retval OMPI_ERROR The subscription could not be removed - most likely caused by specifying
 * a non-existent (or previously removed) subscription number.
 *
 * @code
 * status_code = ompi_registry.unsubscribe(sub_number);
 * @endcode
 */
typedef int (*mca_gpr_base_module_unsubscribe_fn_t)(ompi_registry_notify_id_t sub_number);

/*
 * Request a synchro call from the registry
 * Subscriptions indicate when a specified action has occurred on one or more data objects.
 * In some conditions, however, it is desirable to simply know when a specified number of
 * data objects is present on a given registry segment. For example, since each process must
 * register its contact information on the registry, knowing when the number of registrations
 * equals the number of processes can serve as an indicator that all process are ready to run.
 *
 * This function allows the caller to request notification of data object count meeting
 * specified criteria on the indicated registry segment. Supported counting modes include
 * "edge-triggered" (i.e., ascending or descending through a specified level) and "level"
 * (the count being equal to, above, or below a specified value).
 *
 * Any objects already on the specified segment prior to issuing the synchro request
 * will be counted when the request is registered on the registry.
 *
 * Upon triggering, the synchro returns all data objects included in the count in the
 * notification message.
 *
 * @param addr_mode The addressing mode to be used in specifying the objects to be
 * counted by this synchro.
 * @param segment A character string indicating the name of the segment upon which the
 * synchro is being requested. A value of NULL indicates that the synchro
 * is to be placed on the entire registry - this should be done with caution as the
 * synchro will fire based on counting  ALL registry objects matching the specified
 * addressing, potentially including those from jobs other than the one generating the
 * synchro request.
 * @param tokens A NULL-terminated **char list of tokens describing the objects to be
 * counted. A value of NULL indicates that ALL objects on the segment are to be counted.
 * @param cb_func The ompi_registry_notify_cb_fn_t callback function to be called when
 * the synchro is triggered. The data from each counted object will be returned
 * to the callback function in an ompi_registry_notify_message_t structure.
 * @param user_tag A void* user-provided storage location that the caller can
 * use for its own purposes. A NULL value is acceptable.
 *
 * @retval synch_number The synchro number of this request. Callers should save this
 * number for later use if it is desired to permanently remove the synchro from the registry.
 * Note: ONE_SHOT synchros are automatically removed from the registry when triggered.
 *
 * @code
 * synch_number = ompi_registry.synchro(synch_mode, addr_mode, segment, tokens, trigger,
 *                                      cb_func, user_tag);
 * @endcode
 */
typedef ompi_registry_notify_id_t (*mca_gpr_base_module_synchro_fn_t)(ompi_registry_synchro_mode_t synchro_mode,
						ompi_registry_mode_t addr_mode,
						char *segment, char **tokens, int trigger,
						ompi_registry_notify_cb_fn_t cb_func, void *user_tag);

/*
 * Cancel a synchro.
 * Once a synchro has been entered on the registry, a caller may choose to
 * remove it at a later time. This function supports that request.
 *
 * Note: ONE_SHOT synchros are automatically removed from the registry when triggered.
 *
 * @param synch_number The ompi_registry_notify_id_t value returned by the original synchro
 * command.
 *
 * @retval OMPI_SUCCESS The synchro was removed.
 * @retval OMPI_ERROR The synchro could not be removed - most likely caused by specifying
 * a non-existent (or previously removed) synchro number.
 *
 * @code
 * status_code = ompi_registry.cancel_synchro(synch_number);
 * @endcode
 */
typedef int (*mca_gpr_base_module_cancel_synchro_fn_t)(ompi_registry_notify_id_t synch_number);

/* Output the registry's contents to an output stream
 * For debugging purposes, it is helpful to be able to obtain a complete formatted printout
 * of the registry's contents. This function provides that ability.
 *
 * @param output_id The output stream id to which the registry's contents are to be
 * printed.
 *
 * @retval None
 *
 * @code
 * ompi_registry.dump(output_id);
 * @endcode
 */
typedef void (*mca_gpr_base_module_dump_fn_t)(int output_id);

/* Assume ownership of a segment.
 * Although each segment of the registry can contain data from anywhere, each segment is "owned"
 * by a specific job at any given time. This allows the registry to associate a segment with a jobid,
 * thus enabling support for startup and shutdown processes. Transferring ownership of registry
 * segments can occur when the segment is shared by multiple jobs, one or more of which subsequently
 * terminate. In this case, another job must assume "ownership" of the segment.
 *
 * @param segment A character string indicating the segment for which this process is
 * assuming ownership.
 *
 * @retval OMPI_SUCCESS Ownership successfully transferred.
 * @retval OMPI_ERROR Ownership could not be transferred, most likely due to specifying a non-existing
 * segment (or one that has been previously removed).
 *
 * @code
 * status_code = ompi_registry.assume_ownership(segment);
 * @endcode
 */
typedef int (*mca_gpr_base_module_assume_ownership_fn_t)(char *segment);

/* Deliver a notify message.
 * The registry generates notify messages whenever a subscription or synchro is fired. Normally,
 * this happens completely "under the covers" - i.e., the notification process is transparent
 * to the rest of the system, with the message simply delivered to the specified callback function.
 * However, there are two circumstances when the system needs to explicitly deliver a notify
 * message - namely, during startup and shutdown. In these two cases, a special message is
 * "xcast" to all processes, with each process receiving the identical message. In order to
 * ensure that the correct data gets to each subsystem, the message must be disassembled and
 * the appropriate callback function called.
 *
 * This, unfortunately, means that the decoder must explicitly call the message notification
 * subsystem in order to find the callback function. Alternatively, the entire startup/shutdown
 * logic could be buried in the registry, but this violates the design philosophy of the registry
 * acting solely as a publish/subscribe-based cache memory - it should not contain logic pertinent
 * to any usage of that memory.
 *
 * This function provides the necessary "hook" for an external program to request delivery of
 * a message via the publish/subscribe's notify mechanism.
 *
 * @param state The notify action associated with the message. In this case, only two values are
 * supported: OMPI_REGISTRY_NOTIFY_ON_STARTUP and OMPI_REGISTRY_NOTIFY_ON_SHUTDOWN. The function
 * will search the notification system for all requests that match this state and also match
 * the segment name specified in the message itself. Each of the matching requests will be
 * called with the message.
 *
 * @param message The message to be delivered.
 *
 * @retval None
 *
 * @code
 * ompi_registry.deliver_notify_msg(state, message);
 * @endcode
 *
 */
typedef void (*mca_gpr_base_module_deliver_notify_msg_fn_t)(ompi_registry_notify_action_t state,
							    ompi_registry_notify_message_t *message);
/*
 * test interface for internal functions - optional to provide
 */
typedef ompi_list_t* (*mca_gpr_base_module_test_internals_fn_t)(int level);


/*
 * Ver 1.0.0
 */
struct mca_gpr_base_module_1_0_0_t {
    mca_gpr_base_module_get_fn_t get;
    mca_gpr_base_module_put_fn_t put;
    mca_gpr_base_module_delete_segment_fn_t delete_segment;
    mca_gpr_base_module_subscribe_fn_t subscribe;
    mca_gpr_base_module_unsubscribe_fn_t unsubscribe;
    mca_gpr_base_module_synchro_fn_t synchro;
    mca_gpr_base_module_cancel_synchro_fn_t cancel_synchro;
    mca_gpr_base_module_delete_object_fn_t delete_object;
    mca_gpr_base_module_index_fn_t index;
    mca_gpr_base_module_test_internals_fn_t test_internals;
    mca_gpr_base_module_begin_compound_cmd_fn_t begin_compound_cmd;
    mca_gpr_base_module_stop_compound_cmd_fn_t stop_compound_cmd;
    mca_gpr_base_module_exec_compound_cmd_fn_t exec_compound_cmd;
    mca_gpr_base_module_dump_fn_t dump;
    mca_gpr_base_module_silent_mode_on_fn_t silent_mode_on;
    mca_gpr_base_module_silent_mode_off_fn_t silent_mode_off;
    mca_gpr_base_module_notify_off_fn_t notify_off;
    mca_gpr_base_module_notify_on_fn_t notify_on;
    mca_gpr_base_module_assume_ownership_fn_t assume_ownership;
    mca_gpr_base_module_triggers_active_fn_t triggers_active;
    mca_gpr_base_module_triggers_inactive_fn_t triggers_inactive;
    mca_gpr_base_module_get_startup_msg_fn_t get_startup_msg;
    mca_gpr_base_module_get_shutdown_msg_fn_t get_shutdown_msg;
    mca_gpr_base_module_cleanup_job_fn_t cleanup_job;
    mca_gpr_base_module_cleanup_proc_fn_t cleanup_process;
    mca_gpr_base_module_deliver_notify_msg_fn_t deliver_notify_msg;
};
typedef struct mca_gpr_base_module_1_0_0_t mca_gpr_base_module_1_0_0_t;
typedef mca_gpr_base_module_1_0_0_t mca_gpr_base_module_t;

/*
 * GPR Component
 */

typedef mca_gpr_base_module_t* (*mca_gpr_base_component_init_fn_t)(
								   bool *allow_multi_user_threads,
								   bool *have_hidden_threads,
								   int *priority);

typedef int (*mca_gpr_base_component_finalize_fn_t)(void);
 
/*
 * the standard component data structure
 */


struct mca_gpr_base_component_1_0_0_t {
    mca_base_component_t gpr_version;
    mca_base_component_data_1_0_0_t gpr_data;

    mca_gpr_base_component_init_fn_t gpr_init;
    mca_gpr_base_component_finalize_fn_t gpr_finalize;
};
typedef struct mca_gpr_base_component_1_0_0_t mca_gpr_base_component_1_0_0_t;
typedef mca_gpr_base_component_1_0_0_t mca_gpr_base_component_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * Macro for use in modules that are of type coll v1.0.0
 */
#define MCA_GPR_BASE_VERSION_1_0_0		\
    /* gpr v1.0 is chained to MCA v1.0 */	\
    MCA_BASE_VERSION_1_0_0,			\
	/* gpr v1.0 */				\
	"gpr", 1, 0, 0

#endif
