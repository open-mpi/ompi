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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "coll_libnbc.h"

#include "mpi.h"
#include "ompi/mca/coll/coll.h"


/*
 * Public string showing the coll ompi_libnbc component version number
 */
const char *mca_coll_libnbc_component_version_string =
    "Open MPI libnbc collective MCA component version " OMPI_VERSION;


static int libnbc_priority = 10;


static int libnbc_register(void);
static int libnbc_init_query(bool, bool);
static mca_coll_base_module_t *libnbc_comm_query(struct ompi_communicator_t *, int *);
static int libnbc_module_enable(mca_coll_base_module_t *, struct ompi_communicator_t *);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const mca_coll_base_component_2_0_0_t mca_coll_libnbc_component = {

    /* First, the mca_component_t struct containing meta information
     * about the component itself */

    {
     MCA_COLL_BASE_VERSION_2_0_0,

     /* Component name and version */
     "libnbc",
     OMPI_MAJOR_VERSION,
     OMPI_MINOR_VERSION,
     OMPI_RELEASE_VERSION,

     /* Component open and close functions */
     NULL,
     NULL,
     NULL,
     libnbc_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Initialization / querying functions */

    libnbc_init_query,
    libnbc_comm_query
};


static int
libnbc_register(void)
{
    /* Use a low priority, but allow other components to be lower */

    mca_base_param_reg_int(&mca_coll_libnbc_component.collm_version,
                           "priority",
                           "Priority of the libnbc coll component",
                           false, false, libnbc_priority,
                           &libnbc_priority);

    return OMPI_SUCCESS;
}



/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
static int
libnbc_init_query(bool enable_progress_threads,
                  bool enable_mpi_threads)
{
    /* Nothing to do */
    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
libnbc_comm_query(struct ompi_communicator_t *comm, 
                  int *priority)
{
    ompi_coll_libnbc_module_t *module;

    module = OBJ_NEW(ompi_coll_libnbc_module_t);
    if (NULL == module) return NULL;

    *priority = libnbc_priority;

    module->super.coll_module_enable = libnbc_module_enable;

    module->super.coll_ibarrier = ompi_coll_libnbc_ibarrier;

    module->super.ft_event = NULL;

    return &(module->super);
}


/*
 * Init module on the communicator
 */
static int
libnbc_module_enable(mca_coll_base_module_t *module,
                     struct ompi_communicator_t *comm)
{
    /* All done */
    return OMPI_SUCCESS;
}


static void
libnbc_module_construct(ompi_coll_libnbc_module_t *module)
{
}

static void
libnbc_module_destruct(ompi_coll_libnbc_module_t *module)
{
}


OBJ_CLASS_INSTANCE(ompi_coll_libnbc_module_t,
                   mca_coll_base_module_t,
                   libnbc_module_construct,
                   libnbc_module_destruct);
