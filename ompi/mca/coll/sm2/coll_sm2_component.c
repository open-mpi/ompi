/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * Most of the description of the data layout is in the
 * coll_sm_module.c file.
 */

#include "ompi_config.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "opal/util/show_help.h"
#include "coll_sm2.h"


/*
 * Public string showing the coll ompi_sm V2 component version number
 */
const char *mca_coll_sm_component_version_string =
    "Open MPI sm-V2 collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */
static int sm2_module_enable(struct mca_coll_base_module_1_1_0_t *module,
        struct ompi_communicator_t *comm);

/*
 * Local functions
 */

static int sm2_open(void);
static int sm2_close(void);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_coll_sm2_component_t mca_coll_sm2_component = {

    /* First, fill in the super (mca_coll_base_component_1_1_0_t) */

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */
        
        {
            /* Indicate that we are a coll v1.1.0 component (which
               also implies a specific MCA version) */

            MCA_COLL_BASE_VERSION_1_1_0,

            /* Component name and version */

            "sm-v2",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */

            sm2_open,
            sm2_close,
        },
        
        /* Next the MCA v1.1.0 component meta data */

        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */
        
        mca_coll_sm2_init_query,
        mca_coll_sm2_comm_query,
    },

    /* sm-component specifc information */

    /* (default) priority */
    /* JMS temporarily lowered until we can get more testing */
    0,

};


static void                
mca_coll_sm2_module_construct(mca_coll_sm2_module_t *module)
{
    /* debug */
    fprintf(stderr," sm2 constructor called \n");
    fflush(stderr);
    /* end debug */
}

static void                
mca_coll_sm2_module_destruct(mca_coll_sm2_module_t *module)
{
    /* debug */
    fprintf(stderr," sm2 destructor called \n");
    fflush(stderr);
    /* end debug */
}


/*
 * Open the component
 */
static int sm2_open(void)
{
    mca_base_component_t *c = &mca_coll_sm2_component.super.collm_version;
    mca_coll_sm2_component_t *cs = &mca_coll_sm2_component;

    mca_base_param_reg_int(c, "priority", 
                           "Priority of the sm-v2 coll component",
                           false, false, 
                           cs->sm_priority,
                           &cs->sm_priority);

    return OMPI_SUCCESS;
}


/*
 * Close the component
 */
static int sm2_close(void)
{
    return OMPI_SUCCESS;
}

/* query to see if the component is available for use, and can 
 * satisfy the thread and progress requirements 
 */
int mca_coll_sm2_init_query(bool enable_progress_threads,
        bool enable_mpi_threads)
{

    /* done */
    return OMPI_SUCCESS;
}


/* query to see if the module is available for use on the given
 * communicator, and if so, what it's priority is.
 */
struct mca_coll_base_module_1_1_0_t *
mca_coll_sm2_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    /* local variables */
    mca_coll_sm2_module_t *sm_module;

    /* Get our priority */
    *priority = mca_coll_sm2_component.sm_priority;

    /* allocate and initialize an sm-v2  module */
    sm_module = OBJ_NEW(mca_coll_sm2_module_t);

    sm_module->super.coll_module_enable = sm2_module_enable;
    sm_module->super.ft_event        = NULL;
    sm_module->super.coll_allgather  = NULL;
    sm_module->super.coll_allgatherv = NULL;
    sm_module->super.coll_allreduce  = NULL;
    sm_module->super.coll_alltoall   = NULL;
    sm_module->super.coll_alltoallv  = NULL;
    sm_module->super.coll_alltoallw  = NULL;
    sm_module->super.coll_barrier    = NULL;
    sm_module->super.coll_bcast      = NULL;
    sm_module->super.coll_exscan     = NULL;
    sm_module->super.coll_gather     = NULL;
    sm_module->super.coll_gatherv    = NULL;
    sm_module->super.coll_reduce     = NULL;
    sm_module->super.coll_reduce_scatter = NULL;
    sm_module->super.coll_scan       = NULL;
    sm_module->super.coll_scatter    = NULL;
    sm_module->super.coll_scatterv   = NULL;
    return &(sm_module->super);

}

/*
    * Init module on the communicator
     */
static int
sm2_module_enable(struct mca_coll_base_module_1_1_0_t *module,
                         struct ompi_communicator_t *comm)
{
    /* All done */
    return OMPI_SUCCESS;
}


OBJ_CLASS_INSTANCE(mca_coll_sm2_module_t,
                   mca_coll_base_module_1_1_0_t,
                   mca_coll_sm2_module_construct,
                   mca_coll_sm2_module_destruct);
