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

#include <sys/types.h>
#include <stdint.h>
#include <limits.h>

#include "ompi_config.h"
#include "include/types.h"
#include "include/constants.h"
#include "class/ompi_list.h"
#include "util/pack.h"

#include "mca/mca.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"

/** Define the notification actions for the subscription system
 */
/** Notifies subscriber when object is modified */
#define OMPI_REGISTRY_NOTIFY_MODIFICATION     0x0001
/** Notifies subscriber when another subscriber is added */
#define OMPI_REGISTRY_NOTIFY_ADD_SUBSCRIBER   0x0002
/** Notifies subscriber when object is removed from registry */
#define OMPI_REGISTRY_NOTIFY_DELETE           0x0004
/** Notifies subscriber upon any action - effectively an OR of all other flags */
#define OMPI_REGISTRY_NOTIFY_ALL              0xffff


/** Define the mode bit-masks for registry operations.
 */
/** Overwrite permission */
#define OMPI_REGISTRY_OVERWRITE       0x0001
/** AND tokens together for search results */
#define OMPI_REGISTRY_AND             0x0002
/** OR tokens for search results */
#define OMPI_REGISTRY_OR              0x0004
/** XAND - all tokens required, nothing else allowed - must be exact match */
#define OMPI_REGISTRY_XAND            0x0008
/** XOR - any one of the tokens required, nothing else allowed */
#define OMPI_REGISTRY_XOR             0x0010

/*
 * typedefs
 */

typedef uint16_t ompi_registry_notify_action_t;
typedef uint16_t ompi_registry_mode_t;
typedef ompi_buffer_t ompi_registry_object_t;
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
    ompi_registry_object_t *object;           /**< Pointer to object being returned */
    ompi_registry_object_size_t object_size;  /**< Size of returned object, in bytes */
};
typedef struct ompi_registry_value_t ompi_registry_value_t;

OBJ_CLASS_DECLARATION(ompi_registry_value_t);

/** Return value structure for index requests.
 */
struct ompi_registry_index_t {
    ompi_list_item_t item;           /**< Allows this item to be placed on a list */
    char *token;                     /**< Pointer to the token string */
};
typedef struct ompi_registry_index_t ompi_registry_index_t;

OBJ_CLASS_DECLARATION(ompi_registry_index_t);

/*
 * Component functions that MUST be provided
 */
typedef int (*mca_gpr_base_module_define_segment_fn_t)(char *segment);
typedef int (*mca_gpr_base_module_delete_segment_fn_t)(char *segment);
typedef int (*mca_gpr_base_module_put_fn_t)(ompi_registry_mode_t mode, char *segment,
				     char **tokens, ompi_registry_object_t *object,
				     ompi_registry_object_size_t size);
typedef ompi_registry_value_t* (*mca_gpr_base_module_get_fn_t)(ompi_registry_mode_t mode,
							char *segment, char **tokens);
typedef int (*mca_gpr_base_module_delete_fn_t)(ompi_registry_mode_t mode,
					char *segment, char **tokens);
typedef ompi_registry_index_t* (*mca_gpr_base_module_index_fn_t)(char *segment);
typedef int (*mca_gpr_base_module_subscribe_fn_t)(ompi_process_name_t* subscriber,
						  ompi_registry_mode_t mode,
						  ompi_registry_notify_action_t action,
						  char *segment, char **tokens);
typedef int (*mca_gpr_base_module_unsubscribe_fn_t)(ompi_process_name_t* subscriber,
						    ompi_registry_mode_t mode,
						    char *segment, char **tokens);


/*
 * Ver 1.0.0
 */
struct mca_gpr_base_module_1_0_0_t {
    mca_gpr_base_module_get_fn_t get;
    mca_gpr_base_module_put_fn_t put;
    mca_gpr_base_module_define_segment_fn_t define_segment;
    mca_gpr_base_module_delete_segment_fn_t delete_segment;
    mca_gpr_base_module_subscribe_fn_t subscribe;
    mca_gpr_base_module_unsubscribe_fn_t unsubscribe;
    mca_gpr_base_module_delete_fn_t delete;
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
