/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
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
#include "sbgp_basesmuma.h"


/*
 * Public string showing the coll ompi_sm V2 component version number
 */
const char *mca_sbgp_basesmuma_component_version_string =
    "Open MPI sbgp - basesmuma collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */

static int basesmuma_register(void);
static int basesmuma_open(void);
static int basesmuma_close(void);
static mca_sbgp_base_module_t *mca_sbgp_basesmuma_select_procs(struct ompi_proc_t ** procs,
        int n_procs_in, struct ompi_communicator_t *comm, char *key, void *output_data);

static int mca_sbgp_basesmuma_init_query(bool enable_progress_threads,
        bool enable_mpi_threads);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_sbgp_basesmuma_component_t mca_sbgp_basesmuma_component = {

    /* First, fill in the super */

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */

        {
            MCA_SBGP_BASE_VERSION_2_0_0,

            /* Component name and version */

            "basesmuma",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open, close, and register functions */

            basesmuma_open,
            basesmuma_close,
            NULL,
            basesmuma_register
        },
    mca_sbgp_basesmuma_init_query,
    mca_sbgp_basesmuma_select_procs,

    /* (default) priority */
    0

    }

};

/*
 * Register the component
 */
static int basesmuma_register(void)
{
    mca_sbgp_basesmuma_component_t *cs = &mca_sbgp_basesmuma_component;

    /* set component priority */
    cs->super.priority = 90;
    (void) mca_base_component_var_register(&cs->super.sbgp_version,
                                           "priority", "Priority of the sbgp basesmuma",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &cs->super.priority);
    return OMPI_SUCCESS;
}

/*
 * Open the component
 */
static int basesmuma_open(void)
{
    return OMPI_SUCCESS;
}


/*
 * Close the component
 */
static int basesmuma_close(void)
{
    return OMPI_SUCCESS;
}

/* query to see if the component is available for use, and can
 * satisfy the thread and progress requirements
 */
int mca_sbgp_basesmuma_init_query(bool enable_progress_threads,
        bool enable_mpi_threads)
{
    /* at this stage there is no reason to disaulify this component */

    /* done */
    return OMPI_SUCCESS;
}

/* This routine is used to find the list of procs that run on the
** same host as the calling process.
*/
static mca_sbgp_base_module_t *mca_sbgp_basesmuma_select_procs(struct ompi_proc_t ** procs,
        int n_procs_in,
        struct ompi_communicator_t *comm,
        char *key,
        void *output_data
        )
{
    /* local variables */
    int cnt,proc,local,last_local_proc;
    mca_sbgp_basesmuma_module_t *module;

    module=OBJ_NEW(mca_sbgp_basesmuma_module_t);
    if (!module ) {
        return NULL;
    }
    module->super.group_size=0;
    module->super.group_comm = comm;
    module->super.group_list = NULL;
    module->super.group_net = OMPI_SBGP_MUMA;
    for (proc = 0, cnt = 0, last_local_proc = 0 ; proc < n_procs_in ; ++proc) {
        local = OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags);
        if (local) {
            last_local_proc = proc;
            cnt++;
        }
    }
    /* if no other local procs found skip to end */

    if( 2 > cnt ) {
        /* There's always at least one - namely myself */
        assert(1 == cnt);
        module->super.group_size = 1;
        module->super.group_list = (int *) malloc (sizeof (int));
        module->super.group_list[0] = last_local_proc;
        /* let ml handle this case */
        goto OneLocalPeer;
    }

    /* generate list of local ranks */
    module->super.group_size=cnt;
    if( cnt > 0 ) {
        module->super.group_list=(int *)malloc(sizeof(int)*cnt);
        if(NULL == module->super.group_list){
            goto Error;
        }
    }

    for (proc = 0, cnt = 0 ; proc < n_procs_in ; ++proc) {
        local = OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags);
        if( local ) {
            module->super.group_list[cnt++] = proc;
        }
    }
OneLocalPeer:
    /* successful completion */
    return (mca_sbgp_base_module_t *)module;

    /* return with error */

Error:

    /* clean up */
    if( NULL != module->super.group_list ) {
        free(module->super.group_list);
        module->super.group_list=NULL;
    }

    OBJ_RELEASE(module);

    return NULL;
}
