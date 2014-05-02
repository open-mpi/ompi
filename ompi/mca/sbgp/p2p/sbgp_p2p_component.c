/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#include <fcntl.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "sbgp_p2p.h"
#include "ompi/mca/bml/bml.h"


/*
 * Public string showing the coll ompi_sm V2 component version number
 */
const char *mca_sbgp_p2p_component_version_string =
    "Open MPI sbgp - p2p collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */

static int p2p_register(void);
static int p2p_open(void);
static int p2p_close(void);
static mca_sbgp_base_module_t * mca_sbgp_p2p_select_procs(struct ompi_proc_t ** procs,
        int n_procs_in, struct ompi_communicator_t *comm, char *key, void *output_data);

static int mca_sbgp_p2p_init_query(bool enable_progress_threads,
        bool enable_mpi_threads);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_sbgp_p2p_component_t mca_sbgp_p2p_component = {


    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */

        {
            MCA_SBGP_BASE_VERSION_2_0_0,
            /* Component name and version */

            "p2p",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */

            p2p_open,
            p2p_close,
            NULL,
            p2p_register
        },

    mca_sbgp_p2p_init_query,
    /* select function */
    mca_sbgp_p2p_select_procs,

    /* (default) priority */
    0
    }

};

static int p2p_register(void)
{
    mca_sbgp_p2p_component_t *cs = &mca_sbgp_p2p_component;
    cs->super.priority = 90;
    (void) mca_base_component_var_register(&cs->super.sbgp_version,
                                           "priority", "Priority for the sbgp p2p component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->super.priority);

    return OMPI_SUCCESS;
}

/*
 * Open the component
 */
static int p2p_open(void)
{
    return OMPI_SUCCESS;
}


/*
 * Close the component
 */
static int p2p_close(void)
{
    return OMPI_SUCCESS;
}

/* query to see if the component is available for use, and can
 * satisfy the thread and progress requirements
 */
int mca_sbgp_p2p_init_query(bool enable_progress_threads,
        bool enable_mpi_threads)
{
    /* at this stage there is no reason to disaulify this component */

    /* done */
    return OMPI_SUCCESS;
}
/* This routine is used to find the list of procs that run on the
** same host as the calling process.
*/
static mca_sbgp_base_module_t * mca_sbgp_p2p_select_procs(struct ompi_proc_t ** procs,
        int n_procs_in,
        struct ompi_communicator_t *comm,
        char *key,
        void *output_data
        )
{
    /* local variables */
    int cnt, proc, my_rank;
    mca_sbgp_p2p_module_t *module;

    /* find my rank in the group */
    for (my_rank = -1, proc = 0 ; proc < n_procs_in ; ++proc) {
        if (ompi_proc_local() == procs[proc]) {
            my_rank = proc;
        }
    }

    /* I am not in the list - so will form no local subgroup */
    if (0 > my_rank) {
        return NULL;
    }

    module = OBJ_NEW(mca_sbgp_p2p_module_t);
    if (!module ) {
        return NULL;
    }

    module->super.group_size = 0;
    module->super.group_comm = comm;
    module->super.group_net = OMPI_SBGP_P2P;

    /* allocate resources */
    module->super.group_list = (int *) calloc (n_procs_in, sizeof (int));
    if (NULL == module->super.group_list) {
        goto Error;
    }

    for (cnt = 0, proc = 0 ; proc < n_procs_in ; ++proc) {
#if defined(OMPI_PROC_ENDPOINT_TAG_BML)
        mca_bml_base_endpoint_t* endpoint =
            (mca_bml_base_endpoint_t*) procs[proc]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];
#endif

        if (my_rank == proc || !key) {
            module->super.group_list[cnt++] = proc;
            continue;
        }

#if defined(OMPI_PROC_ENDPOINT_TAG_BML)
        if (NULL != endpoint) {
            int num_btls = mca_bml_base_btl_array_get_size(&(endpoint->btl_eager));
            /* loop over btls */

            for (int i_btl = 0 ; i_btl < num_btls ; ++i_btl) {
                /* I am checking for specific btl */
                if (strcmp(endpoint->btl_eager.bml_btls[i_btl].btl->
                           btl_component->btl_version.mca_component_name, key)) {
                    module->super.group_list[cnt++] = proc;
                    break;
                }
            }
        }
#endif
    }

    if (0 == cnt) {
	goto Error;
    }

    module->super.group_size = cnt;
    module->super.group_list = (int *) realloc (module->super.group_list, sizeof (int) * cnt);
    if (NULL == module->super.group_list) {
        /* Shouldn't ever happen */
        goto Error;
    }

    /* successful return */
    return (mca_sbgp_base_module_t *)module;

    /* return with error */
Error:
    /* clean up */
    if (NULL != module->super.group_list) {
        free (module->super.group_list);
        module->super.group_list = NULL;
    }
    OBJ_RELEASE(module);

    return NULL;
}
