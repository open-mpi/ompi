/* -*- C -*-
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
#include "util/pack.h"

#include "mca/mca.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"

/** Define the notification actions for the subscription system
 */
#define OMPI_REGISTRY_NOTIFY_NONE           0x0000   /**< Null case */
#define OMPI_REGISTRY_NOTIFY_MODIFICATION   0x0001   /**< Notifies subscriber when object modified */
#define OMPI_REGISTRY_NOTIFY_ADD_SUBSCRIBER 0x0002   /**< Notifies subscriber when another subscriber added */
#define OMPI_REGISTRY_NOTIFY_DELETE_ENTRY   0x0004   /**< Notifies subscriber when object deleted */
#define OMPI_REGISTRY_NOTIFY_ADD_ENTRY      0x0008   /**< Notifies subscriber when object added */
#define OMPI_REGISTRY_NOTIFY_PRE_EXISTING   0x0010   /**< Send all pre-existing entries that meet conditions */
#define OMPI_REGISTRY_NOTIFY_ALL            0xffff   /**< Notifies subscriber upon any action */

typedef uint16_t ompi_registry_notify_action_t;

typedef uint32_t mca_gpr_notify_id_t;
#define MCA_GPR_NOTIFY_ID_MAX UINT32_MAX

/*
 * Define synchro mode flags
 */
#define OMPI_REGISTRY_SYNCHRO_MODE_NONE        0x00   /**< No synchronization */
#define OMPI_REGISTRY_SYNCHRO_MODE_ASCENDING   0x01   /**< Notify when trigger is reached, ascending mode */
#define OMPI_REGISTRY_SYNCHRO_MODE_DESCENDING  0x02   /**< Notify when trigger is reached, descending mode */
#define OMPI_REGISTRY_SYNCHRO_MODE_LEVEL       0x04   /**< Notify when trigger is reached, regardless of direction */
#define OMPI_REGISTRY_SYNCHRO_MODE_GT_EQUAL    0x08   /**< Notify if level greater than or equal */
#define OMPI_REGISTRY_SYNCHRO_MODE_LT_EQUAL    0x10   /**< Notify if level less than or equal */
#define OMPI_REGISTRY_SYNCHRO_MODE_CONTINUOUS  0x80   /**< Notify whenever conditions are met */
#define OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT    0x81   /**< Fire once, then terminate synchro command */

typedef uint16_t ompi_registry_synchro_mode_t;


/** Return value for notify requests
 */
struct ompi_registry_notify_message_t {
    ompi_list_t data;    /**< List of data objects */
    ompi_registry_notify_action_t trig_action;
    ompi_registry_synchro_mode_t trig_synchro;
    uint32_t num_tokens;
    char **tokens;
};
typedef struct ompi_registry_notify_message_t ompi_registry_notify_message_t;

OBJ_CLASS_DECLARATION(ompi_registry_notify_message_t);

/** Notify callback function */
typedef void (*ompi_registry_notify_cb_fn_t)(ompi_registry_notify_message_t *notify_msg, void *user_tag);



/** Define the mode bit-masks for registry operations.
 */
#define OMPI_REGISTRY_NONE       0x0000   /**< None */
#define OMPI_REGISTRY_OVERWRITE  0x0001   /**< Overwrite Permission */
#define OMPI_REGISTRY_AND        0x0002   /**< AND tokens together for search results */
#define OMPI_REGISTRY_OR         0x0004   /**< OR tokens for search results */
#define OMPI_REGISTRY_XAND       0x0008   /**< All tokens required, nothing else allowed */
#define OMPI_REGISTRY_XOR        0x0010   /**< Any one of the tokens required, nothing else allowed */

typedef uint16_t ompi_registry_mode_t;


/*
 * Define flag values for remote commands - only used internally
 */
#define MCA_GPR_DELETE_SEGMENT_CMD     0x0001
#define MCA_GPR_PUT_CMD                0x0002
#define MCA_GPR_DELETE_OBJECT_CMD      0x0004
#define MCA_GPR_INDEX_CMD              0x0008
#define MCA_GPR_SUBSCRIBE_CMD          0x0010
#define MCA_GPR_UNSUBSCRIBE_CMD        0x0020
#define MCA_GPR_SYNCHRO_CMD            0x0040
#define MCA_GPR_CANCEL_SYNCHRO_CMD     0x0080
#define MCA_GPR_GET_CMD                0x0100
#define MCA_GPR_TEST_INTERNALS_CMD     0x0200
#define MCA_GPR_NOTIFY_CMD             0x0400   /**< Indicates a notify message */
#define MCA_GPR_ERROR                  0xffff

typedef uint16_t mca_gpr_cmd_flag_t;

/*
 * packing type definitions
 */
/* CAUTION - any changes here must also change corresponding
 * typedefs above
 */
#define MCA_GPR_OOB_PACK_CMD           OMPI_INT16
#define MCA_GPR_OOB_PACK_ACTION        OMPI_INT16
#define MCA_GPR_OOB_PACK_MODE          OMPI_INT16
#define MCA_GPR_OOB_PACK_OBJECT_SIZE   OMPI_INT32
#define MCA_GPR_OOB_PACK_SYNCHRO_MODE  OMPI_INT16


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

OBJ_CLASS_DECLARATION(ompi_registry_value_t);

/** Return value structure for index requests.
 */
struct ompi_registry_index_value_t {
    ompi_list_item_t item;           /**< Allows this item to be placed on a list */
    char *token;                     /**< Pointer to the token string */
};
typedef struct ompi_registry_index_value_t ompi_registry_index_value_t;

OBJ_CLASS_DECLARATION(ompi_registry_index_value_t);

/** Return value for test results on internal test
 */
struct ompi_registry_internal_test_results_t {
    ompi_list_item_t item;          /**< Allows this item to be placed on a list */
    char *test;
    char *message;
};
typedef struct ompi_registry_internal_test_results_t ompi_registry_internal_test_results_t;

OBJ_CLASS_DECLARATION(ompi_registry_internal_test_results_t);


struct mca_gpr_notify_request_tracker_t {
    ompi_list_item_t item;
    ompi_process_name_t *requestor;
    int req_tag;
    ompi_registry_notify_cb_fn_t callback;
    void *user_tag;
    mca_gpr_notify_id_t id_tag;
};
typedef struct mca_gpr_notify_request_tracker_t mca_gpr_notify_request_tracker_t;

OBJ_CLASS_DECLARATION(mca_gpr_notify_request_tracker_t);


struct mca_gpr_idtag_list_t {
    ompi_list_item_t item;
    mca_gpr_notify_id_t id_tag;
};
typedef struct mca_gpr_idtag_list_t mca_gpr_idtag_list_t;

OBJ_CLASS_DECLARATION(mca_gpr_idtag_list_t);

/*
 * Component functions that MUST be provided
 */
typedef int (*mca_gpr_base_module_delete_segment_fn_t)(char *segment);
typedef int (*mca_gpr_base_module_put_fn_t)(ompi_registry_mode_t mode, char *segment,
				     char **tokens, ompi_registry_object_t object,
				     ompi_registry_object_size_t size);
typedef ompi_list_t* (*mca_gpr_base_module_get_fn_t)(ompi_registry_mode_t addr_mode,
						     char *segment, char **tokens);
typedef int (*mca_gpr_base_module_delete_fn_t)(ompi_registry_mode_t addr_mode,
					char *segment, char **tokens);
typedef ompi_list_t* (*mca_gpr_base_module_index_fn_t)(char *segment);
typedef int (*mca_gpr_base_module_subscribe_fn_t)(ompi_registry_mode_t addr_mode,
						  ompi_registry_notify_action_t action,
						  char *segment, char **tokens,
						  ompi_registry_notify_cb_fn_t cb_func, void *user_tag);
typedef int (*mca_gpr_base_module_unsubscribe_fn_t)(ompi_registry_mode_t addr_mode,
						    ompi_registry_notify_action_t action,
						    char *segment, char **tokens);
typedef int (*mca_gpr_base_module_synchro_fn_t)(ompi_registry_synchro_mode_t synchro_mode,
						ompi_registry_mode_t addr_mode,
						char *segment, char **tokens, int trigger,
						ompi_registry_notify_cb_fn_t cb_func, void *user_tag);
typedef int (*mca_gpr_base_module_cancel_synchro_fn_t)(ompi_registry_synchro_mode_t synchro_mode,
						       ompi_registry_mode_t addr_mode,
						       char *segment, char **tokens, int trigger);

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
    mca_gpr_base_module_delete_fn_t delete_object;
    mca_gpr_base_module_index_fn_t index;
    mca_gpr_base_module_test_internals_fn_t test_internals;
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


/*
 * Macro for use in modules that are of type coll v1.0.0
 */
#define MCA_GPR_BASE_VERSION_1_0_0 \
  /* gpr v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* gpr v1.0 */ \
  "gpr", 1, 0, 0

#endif
