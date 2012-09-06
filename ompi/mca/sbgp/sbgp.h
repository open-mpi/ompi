/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_SBGP_H
#define MCA_SBGP_H

#include "ompi_config.h"
#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "ompi/communicator/communicator.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 *  List of supported network types
 */

typedef int (*mca_sbgp_component_init_query_fn_t)
    (bool enable_progress_threads, bool enable_mpi_threads);

typedef enum {
    OMPI_SBGP_MUMA   = 1 << 0, /* Muma */
    OMPI_SBGP_SOCKET = 1 << 1, /* CPU socket */
    OMPI_SBGP_P2P    = 1 << 2, /* Point 2 point networks */
    OMPI_SBGP_IBCX2  = 1 << 3, /* Infiniband ConnextX2 */
    OMPI_SBGP_IB     = 1 << 4  /* Infiniband */
} mca_sbgp_net_type;

/*
 * Interface function for routine that will extract subgroups
 *
 * @param procs (IN)    List of mpi processes to filter
 * @param n_procs_in (IN)    Number of input processes
 * @param key (IN)  optional key
 * @param output_data (OUT) component specific output
 * @return                module, NULL if one is not created.
 *
 */

struct mca_sbgp_base_module_2_0_0_t {

    /** Collective modules all inherit from opal_object */
    opal_object_t super;
    /* group size */
    int group_size;

    /* largest power of 2 in group */
    int pow_2;

    /* number of levels in the tree */
    int n_levels_pow2;

    /* my index in the group list,
     * pointer to my rank */
    int my_index;
    /* List of ranks.
     * Actually we return to ML array of
     * indexes to ompi_proc.
     * And ML is responsible to replace
     * the indexes to ranks */
    int *group_list;
    /* pointer to *father* communicator,
     * Not sure if we really need it now. I know my rank via my index,
     * and ompi_proc I can cache on sbgp module.
     * For ib I do not need it */
    struct ompi_communicator_t *group_comm;
    /* network supported by this groups */
    mca_sbgp_net_type group_net;

    /*FIXME:
     * I don't know where to add the use_hdl flag since the 
     * mca_bcol_basesmuma_comm_query takes just two input parameters.
     */
    bool use_hdl;

};
typedef struct mca_sbgp_base_module_2_0_0_t mca_sbgp_base_module_2_0_0_t;
typedef struct mca_sbgp_base_module_2_0_0_t mca_sbgp_base_module_t;
/* typedef mca_sbgp_base_module_2_0_0_t mca_sbgp_base_module_t; */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_sbgp_base_module_t);

typedef mca_sbgp_base_module_t *(*mca_sbgp_create_subgroup_fn_t)(
        struct ompi_proc_t ** procs, int n_procs_in,
        struct ompi_communicator_t *comm, char *key,
        void *output_data
        );

/**
 * Subgrouping  component interface
 *
 * Component interface for the sub-gorup  framework.  A public
 * instance of this structure, called
 * mca_sbgp_[component_name]_component, must exist in any sub-group
 * component.
 */
struct mca_sbgp_base_component_2_0_0_t {
    /** Base component description */
    mca_base_component_t sbgp_version;

    /** Sbgp component init query function */
    mca_sbgp_component_init_query_fn_t sbgp_init_query;

    /** process selection function */
    mca_sbgp_create_subgroup_fn_t select_procs;

    /** priority */
    int priority;

};
typedef struct mca_sbgp_base_component_2_0_0_t mca_sbgp_base_component_2_0_0_t;
typedef struct mca_sbgp_base_component_2_0_0_t mca_sbgp_base_component;


/*
* Macro for use in components that are of type coll
*/
#define MCA_SBGP_BASE_VERSION_2_0_0 \
      MCA_BASE_VERSION_2_0_0, \
  "sbgp", 2, 0, 0

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_SBGP_H */
