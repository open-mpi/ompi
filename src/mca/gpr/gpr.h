/* -*- C -*-
 *
 * $HEADER$
 */
/** @file **/

/** 
 *  \brief General Purpose Registry (GPR)
 *
 * The Open MPI General Purpose Registry (GPR) 
 */

#ifndef MCA_GPR_H_
#define MCA_GPR_H_

#include "ompi_config.h"

#include "include/types.h"
#include "mca/mca.h"
#include "mca/gpr/base/base.h"

/*
 * Global constants / types
 */

/*
 * Component functions
 */

/*
 * public functions that MUST be provided
 */
typedef int (*mca_gpr_base_definesegment_fn_t)(char *segment);
typedef int (*mca_gpr_base_put_fn_t)(ompi_registry_mode_t mode, char *segment,
				     char **tokens, ompi_registry_object_t *object,
				     int size);
typedef ompi_registry_value_t* (*mca_gpr_base_get_fn_t)(ompi_registry_mode_t mode,
							char *segment, char **tokens);
typedef int (*mca_gpr_base_delete_fn_t)(ompi_registry_mode_t mode,
					char *segment, char **tokens);
typedef ompi_keytable_t* (*mca_gpr_base_index_fn_t)(char *segment);
typedef int (*mca_gpr_base_subscribe_fn_t)(ompi_registry_mode_t mode,
					   ompi_registry_action_t action,
					   char *segment, char **tokens);
typedef int (*mca_gpr_base_unsubscribe_fn_t)(ompi_registry_mode_t mode,
					     char *segment, char **tokens);

/*
 * non-public functions that can be provided, or use defaults
 */



/*
 * Ver 1.0.0
 */
struct mca_gpr_base_module_1_0_0_t {
  mca_base_module_t gprc_version;
  mca_base_module_data_1_0_0_t gprc_data;

  mca_gpr_base_init_fn_t gprc_init;
  mca_gpr_base_finalize_fn_t gprc_finalize;
};
typedef struct mca_gpr_base_module_1_0_0_t mca_gpr_base_module_1_0_0_t;

struct mca_gpr_1_0_0_t {
    /* non-public utility functions */
    mca_gpr_base_send_fn_t gpr_send;
    mca_gpr_base_recv_fn_t gpr_recv;
    /* public functions */
    mca_gpr_base_get_fn_t gpr_get;
    mca_gpr_base_put_fn_t gpr_put;
    mca_gpr_base_definesegment_fn_t gpr_definesegment;
    mca_gpr_base_subscribe_fn_t gpr_subscribe;
    mca_gpr_base_unsubscribe_fn_t gpr_unsubscribe;
    mca_gpr_base_delete_fn_t gpr_delete;
};
typedef struct mca_gpr_1_0_0_t mca_gpr_1_0_0_t;

typedef mca_gpr_base_module_1_0_0_t mca_gpr_base_module_t;
typedef mca_gpr_1_0_0_t mca_gpr_t;

/*
 * Macro for use in modules that are of type coll v1.0.0
 */
#define MCA_GPR_BASE_VERSION_1_0_0 \
  /* gpr v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* gpr v1.0 */ \
  "gpr", 1, 0, 0

#endif
