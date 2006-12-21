/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "ompi_config.h"
#include "coll_tuned.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "coll_tuned.h"
#include "coll_tuned_topo.h"
#include "coll_tuned_dynamic_rules.h"
#include "coll_tuned_dynamic_file.h"
#include "coll_tuned_forced.h"

/*
 * Which set are we using?
 */
static const mca_coll_base_module_1_0_0_t *to_use = NULL;

/*
 * Intra communicator decision functions
 * 
 * Two prototypes, one for fixed rules and one for dynamic rules
 *
 */
static const mca_coll_base_module_1_0_0_t intra_fixed = {

    /* Initialization / finalization functions */

    ompi_coll_tuned_module_init,
    ompi_coll_tuned_module_finalize,

    /* Collective function pointers */

    ompi_coll_tuned_allgather_intra_dec_fixed,
    /*     NULL, */
    /*   ompi_coll_tuned_allgatherv_intra_dec_fixed, */
    NULL,
    ompi_coll_tuned_allreduce_intra_dec_fixed,
    /*     NULL, */
    ompi_coll_tuned_alltoall_intra_dec_fixed,
    /*     NULL, */
    /*   ompi_coll_tuned_alltoallv_intra_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_alltoallw_intra_dec_fixed, */
    NULL,
    ompi_coll_tuned_barrier_intra_dec_fixed,
    /*     NULL, */
    ompi_coll_tuned_bcast_intra_dec_fixed,
    /*     NULL, */
    /*   ompi_coll_tuned_exscan_intra_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_gather_intra_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_gatherv_intra_dec_fixed, */
    NULL,
    ompi_coll_tuned_reduce_intra_dec_fixed,
    /*     NULL, */
    /*   ompi_coll_tuned_reduce_scatter_intra_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_scan_intra_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_scatter_intra_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_scatterv_intra_dec_fixed */
    NULL
};

static const mca_coll_base_module_1_0_0_t intra_dynamic = {

    /* Initialization / finalization functions */

    ompi_coll_tuned_module_init,
    ompi_coll_tuned_module_finalize,

    /* Collective function pointers */

    ompi_coll_tuned_allgather_intra_dec_dynamic,
    /*     NULL, */
    /*   ompi_coll_tuned_allgatherv_intra_dec_dynamic, */
    NULL,
    ompi_coll_tuned_allreduce_intra_dec_dynamic,
    /*     NULL, */
    ompi_coll_tuned_alltoall_intra_dec_dynamic,
    /*     NULL, */
    /*   ompi_coll_tuned_alltoallv_intra_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_alltoallw_intra_dec_dynamic, */
    NULL,
    ompi_coll_tuned_barrier_intra_dec_dynamic,
    /*     NULL, */
    ompi_coll_tuned_bcast_intra_dec_dynamic,
    /*     NULL, */
    /*   ompi_coll_tuned_exscan_intra_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_gather_intra_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_gatherv_intra_dec_dynamic, */
    NULL,
    ompi_coll_tuned_reduce_intra_dec_dynamic,
    /*     NULL, */
    /*   ompi_coll_tuned_reduce_scatter_intra_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_scan_intra_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_scatter_intra_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_scatterv_intra_dec_dynamic */
    NULL
};

/*
 * collective decision functions for intercommunicators
 * 
 * Two prototypes, one for fixed rules and one for dynamic rules
 *
 */
static const mca_coll_base_module_1_0_0_t inter_fixed = {

    /* Initialization / finalization functions */

    ompi_coll_tuned_module_init,
    ompi_coll_tuned_module_finalize,

    /* Collective function pointers */

    /*     ompi_coll_tuned_allgather_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_allgatherv_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_allreduce_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_alltoall_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_alltoallv_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_alltoallw_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_barrier_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_bcast_inter_dec_fixed, */
    NULL,
    /* ompi_coll_tuned_exscan_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_gather_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_gatherv_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_reduce_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_reduce_scatter_inter_dec_fixed, */
    NULL,
    /* ompi_coll_tuned_scan_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_scatter_inter_dec_fixed, */
    NULL,
    /*   ompi_coll_tuned_scatterv_inter_dec_fixed */
    NULL
};

static const mca_coll_base_module_1_0_0_t inter_dynamic = {

    /* Initialization / finalization functions */

    ompi_coll_tuned_module_init,
    ompi_coll_tuned_module_finalize,

    /* Collective function pointers */

    /*   ompi_coll_tuned_allgather_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_allgatherv_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_allreduce_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_alltoall_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_alltoallv_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_alltoallw_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_barrier_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_bcast_inter_dec_dynamic, */
    NULL,
    /* ompi_coll_tuned_exscan_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_gather_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_gatherv_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_reduce_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_reduce_scatter_inter_dec_dynamic, */
    NULL,
    /* ompi_coll_tuned_scan_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_scatter_inter_dec_dynamic, */
    NULL,
    /*   ompi_coll_tuned_scatterv_inter_dec_dynamic */
    NULL
};

/* 
 * Note I keep the names here as place markers until all the functions
 * are implemented
 */


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int ompi_coll_tuned_init_query(bool enable_progress_threads,
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
const mca_coll_base_module_1_0_0_t *
ompi_coll_tuned_comm_query(struct ompi_communicator_t *comm, int *priority,
                           struct mca_coll_base_comm_t **data)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream, "coll:tuned:module_tuned query called"));

    *priority = ompi_coll_tuned_priority;

    /* 
     * Choose whether to use [intra|inter] decision functions 
     * and if using fixed OR dynamic rule sets.
     * Right now you cannot mix them, maybe later on it can be changed
     * but this would probably add an extra if and funct call to the path
     */

    if (OMPI_COMM_IS_INTER(comm)) {
        if (ompi_coll_tuned_use_dynamic_rules) {
            OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_query using inter_dynamic"));
            to_use = &inter_dynamic;
        } else {
            OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_query using inter_fixed"));
            to_use = &inter_fixed;
        }
    } else { /* is an intra comm */
        /**
         * If the communicator size is less than 2 we have specialized modules
         * to handle the intra collective communications.
         */
        if( ompi_comm_size(comm) < 2) {
            *priority = 0;
            return NULL;
        }
        if (ompi_coll_tuned_use_dynamic_rules) {
            OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_query using intra_dynamic"));
            to_use = &intra_dynamic;
        } else {
            OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_query using intra_fixed"));
            to_use = &intra_fixed;
        }
    }
    return to_use;
}


/*
 * Init module on the communicator
 */
const struct mca_coll_base_module_1_0_0_t *
ompi_coll_tuned_module_init(struct ompi_communicator_t *comm)
{
    int size, rank;
    struct mca_coll_base_comm_t *data;
    /* fanout parameters */
    int rc=0;
    int i;


    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init called."));

    /* This routine will become more complex and might have to be */
    /* broken into more sections/function calls */

    /* Order of operations:
     * alloc memory for nb reqs (in case we fall through) 
     * add decision rules if using dynamic rules
     *     compact rules using communicator size info etc
     * build first guess cached topologies (might depend on the rules from above)
     *
     * then attach all to the communicator and return base module funct ptrs 
     */

    /* Allocate the data that hangs off the communicator */

    if (OMPI_COMM_IS_INTER(comm)) {
        size = ompi_comm_remote_size(comm);
    } else {
        size = ompi_comm_size(comm);
    }


    /* 
     * we still malloc data as it is used by the TUNED modules
     * if we don't allocate it and fall back to a BASIC module routine then confuses debuggers 
     * we place any special info after the default data
     *
     * BUT on very large systems we might not be able to allocate all this memory so
     * we do check a MCA parameter to see if if we should allocate this memory
     *
     * The default is set very high  
     *
     */

    /* if we within the memory/size limit, allow preallocated data */


    if (size<=ompi_coll_tuned_preallocate_memory_comm_size_limit) {
        data = (mca_coll_base_comm_t*)malloc(sizeof(struct mca_coll_base_comm_t) +
                                             (sizeof(ompi_request_t *) * size * 2));
  
        if (NULL == data) {
            return NULL;
        }
        data->mcct_reqs = (ompi_request_t **) (data + 1);
        data->mcct_num_reqs = size * 2;
    }
    else {
        data = (mca_coll_base_comm_t*)malloc(sizeof(struct mca_coll_base_comm_t)); 
  
        if (NULL == data) {
            return NULL;
        }
        data->mcct_reqs = (ompi_request_t **) NULL;
        data->mcct_num_reqs = 0;
    }


    /*
     * If using dynamic and you are MPI_COMM_WORLD and you want to use a parameter file..
     * then this effects how much storage space you need
     * (This is a basic version of what will go into V2)
     *
     */


    size = ompi_comm_size(comm);  /* find size so we can (A) decide if to access the file directly */
    /* (B) so we can get our very own customised ompi_coll_com_rule_t ptr */
    /* which only has rules in it for our com size */

    rank = ompi_comm_rank(comm);    /* find rank as only MCW:0 opens any tuned conf files */
    /* actually if they are below a threadhold, they all open it */
    /* have to build a collective in here.. but just for MCW.. */
    /* but we have to make sure we have the same rules everywhere :( */

    /* if using dynamic rules make sure all overrides are NULL before we start override anything accidently */
    if (ompi_coll_tuned_use_dynamic_rules) {
        /* base rules */
        data->all_base_rules = (ompi_coll_alg_rule_t*) NULL;

        /* each collective rule for my com size */
        for (i=0;i<COLLCOUNT;i++) {
            data->com_rules[i] = (ompi_coll_com_rule_t*) NULL;
        }
    }

    /* next dynamic state, recheck all forced rules as well */
    /* warning, we should check to make sure this is really an INTRA comm here... */
    if (ompi_coll_tuned_use_dynamic_rules) {
        ompi_coll_tuned_forced_getvalues         (ompi_coll_tuned_forced_params[ALLREDUCE], &(data->user_forced[ALLREDUCE]));
        ompi_coll_tuned_forced_getvalues         (ompi_coll_tuned_forced_params[ALLTOALL],  &(data->user_forced[ALLTOALL]));
        ompi_coll_tuned_forced_getvalues         (ompi_coll_tuned_forced_params[ALLGATHER],  &(data->user_forced[ALLGATHER]));
        /*         ompi_coll_tuned_forced_getvalues (ompi_coll_tuned_forced_params[ALLTOALLV], &(data->user_forced[ALLTOALLV])); */
        ompi_coll_tuned_forced_getvalues_barrier (ompi_coll_tuned_forced_params[BARRIER],   &(data->user_forced[BARRIER]));
        ompi_coll_tuned_forced_getvalues         (ompi_coll_tuned_forced_params[BCAST],     &(data->user_forced[BCAST]));
        ompi_coll_tuned_forced_getvalues         (ompi_coll_tuned_forced_params[REDUCE],    &(data->user_forced[REDUCE]));
    }


    if (&ompi_mpi_comm_world==comm) {

        if (ompi_coll_tuned_use_dynamic_rules) {

            OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init MCW & Dynamic"));

            if (ompi_coll_tuned_dynamic_rules_filename) {
                OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init Opening [%s]", 
                             ompi_coll_tuned_dynamic_rules_filename));
                rc = ompi_coll_tuned_read_rules_config_file (ompi_coll_tuned_dynamic_rules_filename,
                                                             &(data->all_base_rules), COLLCOUNT);
                if (rc>=0) {
                    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init Read %d valid rules\n", rc));
                    /* at this point we all have a base set of rules */
                    /* now we can get our customized communicator sized rule set, for each collective */
                    for (i=0;i<COLLCOUNT;i++) {
                        data->com_rules[i] = ompi_coll_tuned_get_com_rule_ptr (data->all_base_rules, i, size);
                    }
                }
                else { /* failed to read config file, thus make sure its a NULL... */
                    data->all_base_rules = (ompi_coll_alg_rule_t*) NULL;
                }


            } /* end if a config filename exists */

        } /* end if dynamic_rules */

    } /* end if MCW */
  
    /* ok, if using dynamic rules, not MCW and we are just any rank and a base set of rules exist.. ref them */
    /* order of eval is important here, if we are MCW ompi_mpi_comm_world.c_coll_selected_data is NULL still.. */
    if ((ompi_coll_tuned_use_dynamic_rules)&&(!(&ompi_mpi_comm_world==comm))&&
        ((ompi_mpi_comm_world.c_coll_selected_data)->all_base_rules)) {

        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init NOT MCW & Dynamic"));

        /* this will, erm fail if MCW doesn't exist which it should! */
        data->all_base_rules = (ompi_mpi_comm_world.c_coll_selected_data)->all_base_rules;

        /* at this point we all have a base set of rules if they exist atall */
        /* now we can get our customized communicator sized rule set, for each collective */
        for (i=0;i<COLLCOUNT;i++) {
            data->com_rules[i] = ompi_coll_tuned_get_com_rule_ptr (data->all_base_rules, i, size);
        }
    }

    /* 
     * now for the cached topo functions 
     * guess the initial topologies to use rank 0 as root 
     */

    /* general n fan out tree */
    data->cached_ntree = ompi_coll_tuned_topo_build_tree (ompi_coll_tuned_init_tree_fanout, comm, 0); 
    data->cached_ntree_root = 0;
    data->cached_ntree_fanout = ompi_coll_tuned_init_tree_fanout;

    /* binary tree */
    data->cached_bintree = ompi_coll_tuned_topo_build_tree (2, comm, 0); 
    data->cached_bintree_root = 0;

    /* binomial tree */
    data->cached_bmtree = ompi_coll_tuned_topo_build_bmtree (comm, 0);
    data->cached_bmtree_root = 0;

    /* 
     * chains (fanout followed by pipelines)
     * are more difficuilt as the fan out really really depends on message size [sometimes].. 
     * as size gets larger fan-out gets smaller [usually]
     * 
     * will probably change how we cache this later, for now a midsize
     * GEF
     */
    data->cached_chain = ompi_coll_tuned_topo_build_chain (ompi_coll_tuned_init_chain_fanout, comm, 0);
    data->cached_chain_root = 0;
    data->cached_chain_fanout = ompi_coll_tuned_init_chain_fanout;

    /* standard pipeline */
    data->cached_pipeline = ompi_coll_tuned_topo_build_chain (1, comm, 0);
    data->cached_pipeline_root = 0;

    /* All done */

    comm->c_coll_selected_data = data;

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init Tuned is in use"));
    return to_use;
}


/*
 * Finalize module on the communicator
 */
int ompi_coll_tuned_module_finalize(struct ompi_communicator_t *comm)
{
    if (NULL == comm->c_coll_selected_module) {
        return OMPI_SUCCESS;
    }

#if OMPI_ENABLE_DEBUG
    /* Reset the reqs to NULL/0 -- they'll be freed as part of freeing
       the generel c_coll_selected_data */

    comm->c_coll_selected_data->mcct_reqs = NULL;
    comm->c_coll_selected_data->mcct_num_reqs = 0;
#endif

    /* free any cached information that has been allocated */
    if (comm->c_coll_selected_data->cached_ntree) { /* destroy general tree if defined */
        ompi_coll_tuned_topo_destroy_tree (&comm->c_coll_selected_data->cached_ntree);
    }
    if (comm->c_coll_selected_data->cached_bintree) { /* destroy bintree if defined */
        ompi_coll_tuned_topo_destroy_tree (&comm->c_coll_selected_data->cached_bintree);
    }
    if (comm->c_coll_selected_data->cached_bmtree) { /* destroy bmtree if defined */
        ompi_coll_tuned_topo_destroy_tree (&comm->c_coll_selected_data->cached_bmtree);
    }
    if (comm->c_coll_selected_data->cached_chain) { /* destroy general chain if defined */
        ompi_coll_tuned_topo_destroy_tree (&comm->c_coll_selected_data->cached_chain);
    }
    if (comm->c_coll_selected_data->cached_pipeline) { /* destroy pipeline if defined */
        ompi_coll_tuned_topo_destroy_tree (&comm->c_coll_selected_data->cached_pipeline);
    }

    /* if any algorithm rules are cached on the communicator, only free them if its MCW */
    /* as this is the only place they are allocated by reading the decision configure file */
    if ((ompi_coll_tuned_use_dynamic_rules)&&(&ompi_mpi_comm_world==comm)) {
        if (comm->c_coll_selected_data->all_base_rules) {
            ompi_coll_tuned_free_all_rules (comm->c_coll_selected_data->all_base_rules, COLLCOUNT);
        }
    }

    /* if allocated memory free it */
    if (comm->c_coll_selected_data) {
        free(comm->c_coll_selected_data);
        comm->c_coll_selected_data = NULL;
    }
    return OMPI_SUCCESS;
}

