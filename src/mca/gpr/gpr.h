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

#include "mca/mca.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"

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

typedef uint16_t ompi_registry_action_t;
typedef uint16_t ompi_registry_mode_t;


/*
 * Global constants / types
 */
typedef mca_oob_base_msgbuf_t ompi_registry_buf_t;
typedef mca_oob_msgbuf_data_t ompi_registry_bufdata_t;
typedef ompi_registry_buf_t ompi_registry_object_t;

/*
 * Component functions
 */

/*
 * utility functions that may be provided, or use defaults
 */
typedef int (*mca_gpr_base_module_send_fn_t)(ompi_process_name_t *target,
				       ompi_registry_buf_t *buf);
typedef ompi_registry_buf_t (*mca_gpr_base_module_recv_fn_t)(void);


/*
 * public functions that MUST be provided
 */
typedef int (*mca_gpr_base_module_define_segment_fn_t)(char *segment);
typedef int (*mca_gpr_base_module_delete_segment_fn_t)(char *segment);
typedef int (*mca_gpr_base_module_put_fn_t)(ompi_registry_mode_t mode, char *segment,
				     char **tokens, ompi_registry_object_t *object,
				     int size);
typedef ompi_registry_value_t* (*mca_gpr_base_module_get_fn_t)(ompi_registry_mode_t mode,
							char *segment, char **tokens);
typedef int (*mca_gpr_base_module_delete_fn_t)(ompi_registry_mode_t mode,
					char *segment, char **tokens);
typedef ompi_keytable_t* (*mca_gpr_base_module_index_fn_t)(char *segment);
typedef int (*mca_gpr_base_module_subscribe_fn_t)(ompi_registry_mode_t mode,
					   ompi_registry_action_t action,
					   char *segment, char **tokens);
typedef int (*mca_gpr_base_module_unsubscribe_fn_t)(ompi_registry_mode_t mode,
					     char *segment, char **tokens);

/*
 * block functions that may be provided, or use defaults
 */
typedef ompi_registry_buf_t (*mca_gpr_base_module_getbuf_fn_t)(size_t size);
typedef int (*mca_gpr_base_module_packbuf_fn_t)(ompi_registry_buf_t *buf, void *ptr,
					 size_t num_items, ompi_registry_bufdata_t datatype);
typedef int (*mca_gpr_base_module_packstring_fn_t)(ompi_registry_buf_t *buf, char *string);
typedef int (*mca_gpr_base_module_unpackstring_fn_t)(ompi_registry_buf_t *buf, char *string, size_t maxlen);
typedef int (*mca_gpr_base_module_unpackbuf_fn_t)(ompi_registry_buf_t *buf, void *ptr, size_t num_items,
					   ompi_registry_bufdata_t datatype);
typedef int (*mca_gpr_base_module_sendbuf_fn_t)(ompi_process_name_t *target, ompi_registry_buf_t *buf, bool freebuf);


/*
 * Ver 1.0.0
 */
struct mca_gpr_base_component_1_0_0_t {
  mca_base_component_t gprc_version;
  mca_base_component_data_1_0_0_t gprc_data;

  mca_gpr_base_component_init_fn_t gprc_init;
  mca_gpr_base_component_finalize_fn_t gprc_finalize;
};
typedef struct mca_gpr_base_component_1_0_0_t mca_gpr_base_component_1_0_0_t;
typedef mca_gpr_base_component_1_0_0_t mca_gpr_base_component_t;

struct mca_gpr_base_module_1_0_0_t {
    /* non-public utility functions - must be provided */
    mca_gpr_base_module_send_fn_t send;
    mca_gpr_base_module_recv_fn_t recv;
    /* public functions - must be provided */
    mca_gpr_base_module_get_fn_t get;
    mca_gpr_base_module_put_fn_t put;
    mca_gpr_base_module_define_segment_fn_t define_segment;
    mca_gpr_base_module_delete_segment_fn_t delete_segment;
    mca_gpr_base_module_subscribe_fn_t subscribe;
    mca_gpr_base_module_unsubscribe_fn_t unsubscribe;
    mca_gpr_base_module_delete_fn_t delete;
    /* block functions - may be provided */
    mca_gpr_base_module_getbuf_fn_t getbuf;
    mca_gpr_base_module_packbuf_fn_t packbuf;
    mca_gpr_base_module_packstring_fn_t pack_string;
    mca_gpr_base_module_unpackstring_fn_t unpack_string;
    mca_gpr_base_module_unpackbuf_fn_t unpack_buf;
    mca_gpr_base_module_sendbuf_fn_t sendbuf;
};
typedef struct mca_gpr_base_module_1_0_0_t mca_gpr_base_module_1_0_0_t;
typedef mca_gpr_base_module_1_0_0_t mca_gpr_base_module_t;

/*
 * Macro for use in modules that are of type coll v1.0.0
 */
#define MCA_GPR_BASE_VERSION_1_0_0 \
  /* gpr v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* gpr v1.0 */ \
  "gpr", 1, 0, 0

#endif
