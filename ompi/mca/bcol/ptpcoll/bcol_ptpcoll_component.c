/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#include "ompi_config.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/bcol/bcol.h"
#include "bcol_ptpcoll.h"
#include "ompi/mca/bcol/base/base.h"

#include "bcol_ptpcoll_mca.h"
#include "bcol_ptpcoll_utils.h"

/*
 * Public string showing the bcol ptpcoll V2 component version number
 */
const char *mca_bcol_ptpcoll_component_version_string =
    "Open MPI bcol - ptpcoll collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */

static int ptpcoll_open(void);
static int ptpcoll_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_bcol_ptpcoll_component_t mca_bcol_ptpcoll_component = {

    /* First, fill in the super */

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */
        
        {
            MCA_BCOL_BASE_VERSION_2_0_0,

            /* Component name and version */

            "ptpcoll",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */

            ptpcoll_open,
            ptpcoll_close,
	    .mca_register_component_params = mca_bcol_ptpcoll_register_mca_params
        },

        /* Initialization / querying functions */
        
        mca_bcol_ptpcoll_init_query,
        mca_bcol_ptpcoll_comm_query,
        NULL,
        NULL,
        false,
        false,
    },

    /* component specific */

};

static void
collreq_construct(mca_bcol_ptpcoll_collreq_t *collreq)
{
    collreq->requests = NULL;
}

static void
collreq_destruct(mca_bcol_ptpcoll_collreq_t *collreq)
{
    if (NULL != collreq->requests) {
        free(collreq->requests);
    }
}

OBJ_CLASS_INSTANCE(mca_bcol_ptpcoll_collreq_t,
        opal_free_list_item_t,
        collreq_construct,
        collreq_destruct);

/*
 * Open the component
 */
static int ptpcoll_open(void)
{
    return OMPI_SUCCESS;
}

/*
 * Close the component
 */
static int ptpcoll_close(void)
{
    return OMPI_SUCCESS;
}

/* query to see if the component is available for use, and can 
 * satisfy the thread and progress requirements 
 */
int mca_bcol_ptpcoll_init_query(bool enable_progress_threads,
        bool enable_mpi_threads)
{
    /* at this stage there is no reason to disaulify this component */

    /* done */
    return OMPI_SUCCESS;
}

/* memory management routines */

/* allocte memory - this is a no-op function intended to work with
 * mpool2, which will use malloc for allocation, if no other allocator
 * is available.
 */
void * bcol_ptpcoll_allocate_memory(size_t length, size_t alignment, 
 struct mca_bcol_base_module_t *bcol_module)
{
   /* do nothing */
   return NULL;
}

/*
 * register memory - nothing to do
 */
int bcol_ptpcoll_register_memory(void * in_ptr, size_t length, size_t alignment,
     struct mca_bcol_base_module_t *bcol_module)
{
   /* nothing to do */
   return OMPI_SUCCESS;
}

/* deregister memory - nothing to do 
 */
int bcol_ptpcoll_deregister_memory( void * in_ptr,
     struct mca_bcol_base_module_t *bcol_module)
{
   /* nothing to do */
   return OMPI_SUCCESS;
}

/* free memory - since we don't allocate, we also don't free */
int bcol_ptpcoll_free_memory(void *ptr,
        struct mca_bcol_base_module_t *bcol_module)
{
   /* nnthing to do */
   return OMPI_SUCCESS;
}
