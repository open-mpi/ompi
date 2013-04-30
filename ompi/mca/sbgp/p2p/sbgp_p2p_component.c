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
#include "orte/mca/rml/rml.h"
#include "ompi/mca/bml/bml.h"


/*
 * Public string showing the coll ompi_sm V2 component version number
 */
const char *mca_sbgp_p2p_component_version_string =
    "Open MPI sbgp - p2p collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */

static int p2p_open(void);
static int p2p_close(void);
static mca_sbgp_base_module_t * mca_sbgp_p2p_select_procs(struct ompi_proc_t ** procs,
        int n_procs_in, struct ompi_communicator_t *comm, char *key, void *output_data);

static int mca_sbgp_p2p_init_query(bool enable_progress_threads,
        bool enable_mpi_threads);

static inline int mca_sbgp_p2p_param_register_int(
        const char* param_name, int default_value)
{
    int param_value = default_value;

    (void) mca_base_param_reg_int (&mca_sbgp_p2p_component.super.sbgp_version,
                                   param_name, NULL, false, false, default_value,
                                   &param_value);

    return param_value;
}

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
            NULL
        },

    mca_sbgp_p2p_init_query,
    /* select function */
    mca_sbgp_p2p_select_procs,

    /* (default) priority */
    0
    }

};

/*
 * Open the component
 */
static int p2p_open(void)
{

    /* local variables */
    mca_sbgp_p2p_component_t *cs = &mca_sbgp_p2p_component;

    /* set component priority */
    cs->super.priority=
        mca_sbgp_p2p_param_register_int("priority",90);

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
    int cnt,proc;
    mca_sbgp_p2p_module_t *module;
    int my_rank,i_btl;

    module=OBJ_NEW(mca_sbgp_p2p_module_t);
    if (!module ) {
        return NULL;
    }
    module->super.group_size=0;
    module->super.group_comm = comm;
    module->super.group_net = OMPI_SBGP_P2P;

    /* find my rank in the group */
    my_rank=-1;
    for( proc=0 ; proc < n_procs_in ; proc++) {
        if(ompi_proc_local() == procs[proc]) {
            my_rank=proc;
        }
    }

    /* I am not in the list - so will form no local subgroup */
   if( 0 > my_rank ){
       return NULL;
   }

   /* count the number of ranks in the group */
   cnt=0;
    for( proc=0 ; proc < n_procs_in ; proc++) {
        if(my_rank == proc ) {
            cnt++;
            continue;
        }
        /* loop over btls */
        for( i_btl=0 ; i_btl < (int) mca_bml_base_btl_array_get_size(&(procs[proc]->proc_bml->btl_eager)) ; i_btl++ ) {
            if(key) {
                /* I am checking for specific btl */
                if( strcmp(procs[proc]->proc_bml->btl_eager.bml_btls[i_btl].btl->btl_component->btl_version.mca_component_name,key)) {
                    cnt++;
                    break;

                }
            } else {
                /* I will take any btl */
                cnt++;
                break;
            }
        }
    }
/* debug
fprintf(stderr," AAA cnt %d n_procs_in %d \n",cnt,n_procs_in);
fflush(stderr);
 end debug */

    /* allocate resources */
    module->super.group_size=cnt;
    if( cnt > 0 ) {
        module->super.group_list=(int *)malloc(sizeof(int)*
                module->super.group_size);
        if( NULL == module->super.group_list ) {
            goto Error;
        }
    }

   cnt=0;
    for( proc=0 ; proc < n_procs_in ; proc++) {
        if(my_rank == proc ) {
            module->super.group_list[cnt]=proc;
            cnt++;
            continue;
        }
        /* loop over btls */

        for( i_btl=0 ;
                i_btl < (int) mca_bml_base_btl_array_get_size(&(procs[proc]->proc_bml->btl_eager)) ;
                i_btl++ ) {
            if(key) {
                /* I am checking for specific btl */
                if( strcmp(procs[proc]->proc_bml->btl_eager.bml_btls[i_btl].btl->
                            btl_component->btl_version.mca_component_name,key)) {
                    module->super.group_list[cnt]=proc;
                    cnt++;
                    break;

                }
            } else {
                /* I will take any btl */
                module->super.group_list[cnt]=proc;
                cnt++;
                break;
            }
        }
    }

    /* successful return */
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
