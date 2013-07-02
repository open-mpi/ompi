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
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "coll_portals4.h"

#include "mpi.h"
#include "ompi/mca/coll/coll.h"

const char *mca_coll_portals4_component_version_string =
    "Open MPI Portals 4 collective MCA component version " OMPI_VERSION;

int mca_coll_portals4_priority = 10;

static int portals4_register(void);
static int portals4_init_query(bool enable_progress_threads,
                               bool enable_mpi_threads);
static mca_coll_base_module_t* portals4_comm_query(struct ompi_communicator_t *comm,
                                                   int *priority);
static int portals4_module_enable(mca_coll_base_module_t *module,
                                  struct ompi_communicator_t *comm);

const mca_coll_base_component_2_0_0_t mca_coll_portals4_component = {

    /* First, the mca_component_t struct containing meta information
     * about the component itself */

    {
     MCA_COLL_BASE_VERSION_2_0_0,

     /* Component name and version */
     "portals4",
     OMPI_MAJOR_VERSION,
     OMPI_MINOR_VERSION,
     OMPI_RELEASE_VERSION,

     /* Component open and close functions */
     NULL,
     NULL,
     NULL,
     portals4_register
    },
    {
    },

    /* Initialization / querying functions */
    mca_coll_portals4_init_query,
    mca_coll_portals4_comm_query
};


static int
portals4_register(void)
{
    mca_coll_portals4_priority = 100;
    (void) mca_base_component_var_register(&mca_coll_portals4_component.collm_version, "priority",
                                           "Priority of the portals4 coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_portals4_priority);

    return OMPI_SUCCESS;
}



/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
static int
portals4_init_query(bool enable_progress_threads,
                    bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
portals4_comm_query(struct ompi_communicator_t *comm, 
                    int *priority)
{
    int size;
    mca_coll_portals4_module_t *portals4_module;

    portals4_module = OBJ_NEW(mca_coll_portals4_module_t);
    if (NULL == portals4_module) return NULL;

    *priority = mca_coll_portals4_priority;
    portals4_module->super.coll_module_enable = mca_coll_portals4_module_enable;
    portals4_module->super.ft_event = NULL;

    return &(portals4_module->super);
}


/*
 * Init module on the communicator
 */
int
portals4_module_enable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm)
{
    return OMPI_SUCCESS;
}

OBJ_CLASS_INSTANCE(mca_coll_portals4_module_t,
                   mca_coll_base_module_t,
                   NULL, NULL);
