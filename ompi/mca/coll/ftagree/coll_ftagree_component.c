/*
 * Copyright (c) 2012-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "ompi_config.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/ftagree/coll_ftagree.h"
#include "ompi/mca/coll/ftagree/coll_ftagree_era.h"


/*
 * Public string showing the coll ompi_ftagree component version number
 */
const char *mca_coll_ftagree_component_version_string =
    "Open MPI ftagree collective MCA component version " OMPI_VERSION;

/*
 * Global variables
 */
int mca_coll_ftagree_priority  = 0;
mca_coll_ftagree_algorithm_t mca_coll_ftagree_algorithm = COLL_FTAGREE_EARLY_RETURNING;
int mca_coll_ftagree_cur_era_topology = 1;
int mca_coll_ftagree_era_rebuild = 0;
#if defined(FTAGREE_DEBUG_FAILURE_INJECT)
double mca_coll_ftagree_debug_inject_proba = 0.0;
#endif

/*
 * Local function
 */
static int ftagree_register(void);
static int ftagree_close(void);
/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const mca_coll_base_component_2_4_0_t mca_coll_ftagree_component = {

    /* First, the mca_component_t struct containing meta information
     * about the component itself */

    {
     MCA_COLL_BASE_VERSION_2_4_0,

     /* Component name and version */
     "ftagree",
     OMPI_MAJOR_VERSION,
     OMPI_MINOR_VERSION,
     OMPI_RELEASE_VERSION,

     /* Component open and close functions */
     NULL,
     ftagree_close,
     NULL,
     ftagree_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Initialization / querying functions */

    mca_coll_ftagree_init_query,
    mca_coll_ftagree_comm_query
};

static int
ftagree_close(void)
{
    if( mca_coll_ftagree_algorithm ==  COLL_FTAGREE_EARLY_RETURNING ) {
        return mca_coll_ftagree_era_finalize();
    }
    return OMPI_SUCCESS;
}

static int
ftagree_register(void)
{
    int value;

    /* Use a low priority, but allow other components to be lower */
    mca_coll_ftagree_priority = 30;
    (void) mca_base_component_var_register(&mca_coll_ftagree_component.collm_version,
                                           "priority", "Priority of the ftagree coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_ftagree_priority);

    if( ompi_ftmpi_enabled ) value = 1;
    else value = 0; /* NOFT: do not initialize ERA */
    (void) mca_base_component_var_register(&mca_coll_ftagree_component.collm_version,
                                           "agreement", "Agreement algorithm 0: Allreduce (NOT FAULT TOLERANT); 1: Early Returning Consensus (era); 2: Early Terminating Consensus (eta)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &value);
    switch(value) {
    case 0:
        mca_coll_ftagree_algorithm = COLL_FTAGREE_NOFT;
        opal_output_verbose(6, ompi_ftmpi_output_handle,
                            "%s ftagree:register) Agreement Algorithm - Allreduce (NOT FAULT TOLERANT)",
                            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME) );
        break;
    default:  /* Includes the valid case 1 */
        mca_coll_ftagree_algorithm = COLL_FTAGREE_EARLY_RETURNING;
        opal_output_verbose(6, ompi_ftmpi_output_handle,
                            "%s ftagree:register) Agreement Algorithm - Early Returning Consensus Algorithm",
                            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME) );
        break;
    case 2:
        mca_coll_ftagree_algorithm = COLL_FTAGREE_EARLY_TERMINATION;
        opal_output_verbose(6, ompi_ftmpi_output_handle,
                            "%s ftagree:register) Agreement Algorithm - Early Terminating Consensus Algorithm",
                            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME) );
        break;
    }

    mca_coll_ftagree_cur_era_topology = 1;
    (void) mca_base_component_var_register(&mca_coll_ftagree_component.collm_version,
                                           "era_topology", "ERA topology 1: binary tree; 2: star tree; 3: string tree",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_ftagree_cur_era_topology);

    /* TODO: add an adaptative rebuilding strategy */
    mca_coll_ftagree_era_rebuild = 0; /* by default do not rebuild, master-worker application patterns can benefit greatly from rebuilding... */
    (void) mca_base_component_var_register(&mca_coll_ftagree_component.collm_version,
                                           "era_rebuild", "ERA rebuild/rebalance the tree in a first post-failure agreement 0: no rebalancing; 1: rebalance all the time",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_ftagree_era_rebuild);

#if defined(FTAGREE_DEBUG_FAILURE_INJECT)
    mca_coll_ftagree_rank_fault_proba = 0.0; /* by default, inject no faults */
    (void) mca_base_component_var_register(&mca_coll_ftagree_component.collm_version,
                                           "era_debug_fault_proba", "Inject faults with set probability at points of interest in the Agreement algorithm; For debugging only. (0.: no faults, 1.: always inject; 0.05 is typical)",
                                           MCA_BASE_VAR_TYPE_DOUBLE, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_ftagree_rank_fault_proba);
#endif /* FTAGREE_DEBUG_FAILURE_INJECT */

    return OMPI_SUCCESS;
}


static void
mca_coll_ftagree_module_construct(mca_coll_ftagree_module_t *module)
{
    module->mccb_reqs = NULL;
    module->mccb_num_reqs = 0;

    module->mccb_statuses = NULL;
    module->mccb_num_statuses = 0;

    /* This object is managed by the agreement operation selected */
    module->agreement_info = NULL;
}

static void
mca_coll_ftagree_module_destruct(mca_coll_ftagree_module_t *module)
{

    /* Finalize the agreement function */
    if( ompi_ftmpi_enabled ) {
        switch( mca_coll_ftagree_algorithm ) {
        case COLL_FTAGREE_EARLY_RETURNING:
            mca_coll_ftagree_era_comm_finalize(module);
            break;
        default:
            break;
        }
    }

    /* This object is managed by the agreement operation selected */
    module->agreement_info = NULL;

    if (NULL != module->mccb_reqs) {
        free(module->mccb_reqs);
        module->mccb_reqs = NULL;
    }

    if( NULL != module->mccb_statuses ) {
        free(module->mccb_statuses);
        module->mccb_statuses = NULL;
    }
}


OBJ_CLASS_INSTANCE(mca_coll_ftagree_module_t,
                   mca_coll_base_module_t,
                   mca_coll_ftagree_module_construct,
                   mca_coll_ftagree_module_destruct);
