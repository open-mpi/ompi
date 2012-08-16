/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
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
#include "sbgp_basesmuma.h"
#include "orte/mca/rml/rml.h"


/*
 * Public string showing the coll ompi_sm V2 component version number
 */
const char *mca_sbgp_basesmuma_component_version_string =
    "Open MPI sbgp - basesmuma collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */

static int basesmuma_open(void);
static int basesmuma_close(void);
struct mca_sbgp_base_module_t;
static mca_sbgp_base_module_t *mca_sbgp_basesmuma_select_procs(struct ompi_proc_t ** procs,
        int n_procs_in, struct ompi_communicator_t *comm, char *key, void *output_data);

static int mca_sbgp_basesmuma_init_query(bool enable_progress_threads,
        bool enable_mpi_threads);


static inline int mca_sbgp_basesmuma_param_register_int(
        const char* param_name, int default_value)
{
    int id = mca_base_param_register_int("sbgp","basesmuma",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

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

            /* Component open and close functions */

            basesmuma_open,
            basesmuma_close,
        },
    mca_sbgp_basesmuma_init_query,
    mca_sbgp_basesmuma_select_procs,

    /* (default) priority */
    0

    }

};

/*
 * Open the component
 */
static int basesmuma_open(void)
{

    /* local variables */
    mca_sbgp_basesmuma_component_t *cs = &mca_sbgp_basesmuma_component;

    /* set component priority */
    cs->super.priority=
        mca_sbgp_basesmuma_param_register_int("priority",90);

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
    int cnt,proc,local;
    mca_sbgp_basesmuma_module_t *module;

    module=OBJ_NEW(mca_sbgp_basesmuma_module_t);
    if (!module ) {
        return NULL;
    }
    module->super.group_size=0;
    module->super.group_comm = comm;
    module->super.group_list = NULL;
    module->super.group_net = OMPI_SBGP_MUMA;
    cnt=0;
    for( proc=0 ; proc < n_procs_in ; proc++) {
        /* debug
        if( odd ) {
            if( !(1&proc) )
                continue;
        } else {
            if( (1&proc) )
                continue;
        }
         end debug */
        local=OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags);
        if( local ) {
            cnt++;
        }
    }
    /* if no other local procs found skip to end */
    if( 2 > cnt ) {
        /* There's always at least one - namely myself */
        assert( 1 == cnt);
        module->super.group_size=1;
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
    cnt=0;
    for( proc=0 ; proc < n_procs_in ; proc++) {
        /* debug
        if( odd ) {
            if( !(1&proc) )
                continue;
        } else {
            if( (1&proc) )
                continue;
        }
         end debug */
        local=OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags);
        if( local ) {
            module->super.group_list[cnt]=proc;
            cnt++;
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
