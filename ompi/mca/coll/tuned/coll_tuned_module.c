/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2016      Intel, Inc.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_base_topo.h"
#include "coll_tuned.h"
#include "coll_tuned_dynamic_rules.h"
#include "coll_tuned_dynamic_file.h"

static int tuned_module_enable(mca_coll_base_module_t *module,
                   struct ompi_communicator_t *comm);
/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int ompi_coll_tuned_init_query(bool enable_progress_threads,
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
ompi_coll_tuned_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    mca_coll_tuned_module_t *tuned_module;

    OPAL_OUTPUT((ompi_coll_tuned_stream, "coll:tuned:module_tuned query called"));

    /**
     * No support for inter-communicator yet.
     */
    if (OMPI_COMM_IS_INTER(comm)) {
        *priority = 0;
        return NULL;
    }

    /**
     * If it is inter-communicator and size is less than 2 we have specialized modules
     * to handle the intra collective communications.
     */
    if (OMPI_COMM_IS_INTRA(comm) && ompi_comm_size(comm) < 2) {
        *priority = 0;
        return NULL;
    }

    tuned_module = OBJ_NEW(mca_coll_tuned_module_t);
    if (NULL == tuned_module) return NULL;

    *priority = ompi_coll_tuned_priority;

    /*
     * Choose whether to use [intra|inter] decision functions
     * and if using fixed OR dynamic rule sets.
     * Right now you cannot mix them, maybe later on it can be changed
     * but this would probably add an extra if and funct call to the path
     */
    tuned_module->super.coll_module_enable = tuned_module_enable;
    tuned_module->super.ft_event = mca_coll_tuned_ft_event;

    /* By default stick with the fixed version of the tuned collectives. Later on,
     * when the module get enabled, set the correct version based on the availability
     * of the dynamic rules.
     */
    tuned_module->super.coll_allgather  = ompi_coll_tuned_allgather_intra_dec_fixed;
    tuned_module->super.coll_allgatherv = ompi_coll_tuned_allgatherv_intra_dec_fixed;
    tuned_module->super.coll_allreduce  = ompi_coll_tuned_allreduce_intra_dec_fixed;
    tuned_module->super.coll_alltoall   = ompi_coll_tuned_alltoall_intra_dec_fixed;
    tuned_module->super.coll_alltoallv  = ompi_coll_tuned_alltoallv_intra_dec_fixed;
    tuned_module->super.coll_alltoallw  = NULL;
    tuned_module->super.coll_barrier    = ompi_coll_tuned_barrier_intra_dec_fixed;
    tuned_module->super.coll_bcast      = ompi_coll_tuned_bcast_intra_dec_fixed;
    tuned_module->super.coll_exscan     = NULL;
    tuned_module->super.coll_gather     = ompi_coll_tuned_gather_intra_dec_fixed;
    tuned_module->super.coll_gatherv    = NULL;
    tuned_module->super.coll_reduce     = ompi_coll_tuned_reduce_intra_dec_fixed;
    tuned_module->super.coll_reduce_scatter = ompi_coll_tuned_reduce_scatter_intra_dec_fixed;
    tuned_module->super.coll_reduce_scatter_block = ompi_coll_tuned_reduce_scatter_block_intra_dec_fixed;
    tuned_module->super.coll_scan       = NULL;
    tuned_module->super.coll_scatter    = ompi_coll_tuned_scatter_intra_dec_fixed;
    tuned_module->super.coll_scatterv   = NULL;

    return &(tuned_module->super);
}

/* We put all routines that handle the MCA user forced algorithm and parameter choices here */
/* recheck the setting of forced, called on module create (i.e. for each new comm) */

static int
ompi_coll_tuned_forced_getvalues( enum COLLTYPE type,
                                  coll_tuned_force_algorithm_params_t *forced_values )
{
    coll_tuned_force_algorithm_mca_param_indices_t* mca_params;
    const int *tmp = NULL;

    mca_params = &(ompi_coll_tuned_forced_params[type]);

    /**
     * Set the selected algorithm to 0 by default. Later on we can check this against 0
     * to see if it was setted explicitly (if we suppose that setting it to 0 enable the
     * default behavior) or not.
     */
    mca_base_var_get_value(mca_params->algorithm_param_index, &tmp, NULL, NULL);
    forced_values->algorithm = tmp ? tmp[0] : 0;

    if( BARRIER != type ) {
        mca_base_var_get_value(mca_params->segsize_param_index, &tmp, NULL, NULL);
        if (tmp) forced_values->segsize = tmp[0];
        mca_base_var_get_value(mca_params->tree_fanout_param_index, &tmp, NULL, NULL);
        if (tmp) forced_values->tree_fanout = tmp[0];
        mca_base_var_get_value(mca_params->chain_fanout_param_index, &tmp, NULL, NULL);
        if (tmp) forced_values->chain_fanout = tmp[0];
        mca_base_var_get_value(mca_params->max_requests_param_index, &tmp, NULL, NULL);
        if (tmp) forced_values->max_requests = tmp[0];
    }
    return (MPI_SUCCESS);
}

#define COLL_TUNED_EXECUTE_IF_DYNAMIC(TMOD, TYPE, EXECUTE)              \
    {                                                                   \
        int need_dynamic_decision = 0;                                  \
        ompi_coll_tuned_forced_getvalues( (TYPE), &((TMOD)->user_forced[(TYPE)]) ); \
        (TMOD)->com_rules[(TYPE)] = NULL;                               \
        if( 0 != (TMOD)->user_forced[(TYPE)].algorithm ) {              \
            need_dynamic_decision = 1;                                  \
        }                                                               \
        if( NULL != mca_coll_tuned_component.all_base_rules ) {         \
            (TMOD)->com_rules[(TYPE)]                                   \
                = ompi_coll_tuned_get_com_rule_ptr( mca_coll_tuned_component.all_base_rules, \
                                                    (TYPE), nnodes, size );                  \
            if( NULL != (TMOD)->com_rules[(TYPE)] ) {                   \
                need_dynamic_decision = 1;                              \
            }                                                           \
        }                                                               \
        if( 1 == need_dynamic_decision ) {                              \
            OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned: enable dynamic selection for "#TYPE)); \
            EXECUTE;                                                    \
        }                                                               \
    }

OBJ_CLASS_INSTANCE(ompi_coll_tuned_hostname_item_t, opal_list_item_t, NULL, NULL);

static int
ompi_coll_tuned_get_nnodes(struct ompi_communicator_t *comm)
{
    ompi_group_t* group;
    ompi_proc_t *proc;
    opal_list_t hostname_list;
    ompi_coll_tuned_hostname_item_t *host_item, *next, *new_item;
    bool already_in_the_list;
    char* hostname;
    int i, cmp, group_size, nodes_nb;

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init get_nnodes called."));
    OBJ_CONSTRUCT(&hostname_list, opal_list_t);
    /* Is inter communicator ? */
    if (OMPI_COMM_IS_INTER(comm)) {
        group = comm->c_remote_group;
    } else {
        group = comm->c_local_group;
    }
    /* allocate an array of node id */
    group_size = ompi_group_size(group);
    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init get_nnodes group size %d.", group_size));
    /* For each rank */
    for (i=0 ; i<group_size ; i++) {
        proc = ompi_group_get_proc_ptr (group, i, true);
        hostname = opal_get_proc_hostname(&proc->super);
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init get_nnodes host %s.", hostname));
        /* Check if hostname is OK. */
        if(0 == strcmp(hostname,"unknown")) {
            /* Do not consider this rank */
            OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init get_nnodes ignoring unknown host."));
            continue;
        }
        already_in_the_list = false;
        /* Try to find eleme in the list */
        /* List is sorted we stop if current elem is "higher" than the one we expect to find */
        /* In this case this element is inserted at this position. */
        /* If element is found, no insertion is performed, element is inserted at the end otherwise. */
        OPAL_LIST_FOREACH_SAFE(host_item, next, &hostname_list, ompi_coll_tuned_hostname_item_t) {
            cmp = strcmp(host_item->hostname, hostname);
            if(cmp > 0) {
                /* no match found, insert at this position */
                break;
            }
            else if(0 == cmp) {
                /* Found, do not insert this hostname */
                already_in_the_list = true;
                break;
            }
            /* continue comparing elements otherwise */
        }
        if (false == already_in_the_list) {
            /* Insert a new element at current position */
            new_item = OBJ_NEW(ompi_coll_tuned_hostname_item_t);
            new_item->hostname = hostname;
            OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init get_nnodes insert host %s", new_item->hostname));
            opal_list_insert_pos(&hostname_list, &host_item->super, &new_item->super);
        }
    }
    nodes_nb = opal_list_get_size(&hostname_list);
    OPAL_LIST_DESTRUCT(&hostname_list);
    return nodes_nb;
}

/*
 * Init module on the communicator
 */
static int
tuned_module_enable( mca_coll_base_module_t *module,
                     struct ompi_communicator_t *comm )
{
    /* Variables used in COLL_TUNED_EXECUTE_IF_DYNAMIC macro */
    int size, nnodes;

    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t *) module;
    mca_coll_base_comm_t *data = NULL;

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init called."));

    /* Allocate the data that hangs off the communicator */
    if (OMPI_COMM_IS_INTER(comm)) {
        size = ompi_comm_remote_size(comm);
    } else {
        size = ompi_comm_size(comm);
    }

    /* Get the number of nodes in communicator */
    nnodes = ompi_coll_tuned_get_nnodes(comm);
    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init nnodes %d.", nnodes));
    /**
     * we still malloc data as it is used by the TUNED modules
     * if we don't allocate it and fall back to a BASIC module routine then confuses debuggers
     * we place any special info after the default data
     *
     * BUT on very large systems we might not be able to allocate all this memory so
     * we do check a MCA parameter to see if if we should allocate this memory
     *
     * The default is set very high
     */

    /* prepare the placeholder for the array of request* */
    data = OBJ_NEW(mca_coll_base_comm_t);
    if (NULL == data) {
        return OMPI_ERROR;
    }

    if (ompi_coll_tuned_use_dynamic_rules) {
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init MCW & Dynamic"));

        /**
         * next dynamic state, recheck all forced rules as well
         * warning, we should check to make sure this is really an INTRA comm here...
         */
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, ALLGATHER,
                                      tuned_module->super.coll_allgather  = ompi_coll_tuned_allgather_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, ALLGATHERV,
                                      tuned_module->super.coll_allgatherv = ompi_coll_tuned_allgatherv_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, ALLREDUCE,
                                      tuned_module->super.coll_allreduce  = ompi_coll_tuned_allreduce_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, ALLTOALL,
                                      tuned_module->super.coll_alltoall   = ompi_coll_tuned_alltoall_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, ALLTOALLV,
                                      tuned_module->super.coll_alltoallv  = ompi_coll_tuned_alltoallv_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, ALLTOALLW,
                                      tuned_module->super.coll_alltoallw  = NULL);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, BARRIER,
                                      tuned_module->super.coll_barrier    = ompi_coll_tuned_barrier_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, BCAST,
                                      tuned_module->super.coll_bcast      = ompi_coll_tuned_bcast_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, EXSCAN,
                                      tuned_module->super.coll_exscan     = ompi_coll_tuned_exscan_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, GATHER,
                                      tuned_module->super.coll_gather     = ompi_coll_tuned_gather_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, GATHERV,
                                      tuned_module->super.coll_gatherv    = NULL);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, REDUCE,
                                      tuned_module->super.coll_reduce     = ompi_coll_tuned_reduce_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, REDUCESCATTER,
                                      tuned_module->super.coll_reduce_scatter = ompi_coll_tuned_reduce_scatter_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, REDUCESCATTERBLOCK,
                                      tuned_module->super.coll_reduce_scatter_block = ompi_coll_tuned_reduce_scatter_block_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, SCAN,
                                      tuned_module->super.coll_scan       = ompi_coll_tuned_scan_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, SCATTER,
                                      tuned_module->super.coll_scatter    = ompi_coll_tuned_scatter_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(tuned_module, SCATTERV,
                                      tuned_module->super.coll_scatterv   = NULL);
    }

    /* general n fan out tree */
    data->cached_ntree = NULL;
    /* binary tree */
    data->cached_bintree = NULL;
    /* binomial tree */
    data->cached_bmtree = NULL;
    /* binomial tree */
    data->cached_in_order_bmtree = NULL;
    /* k-nomial tree */
    data->cached_kmtree = NULL;
    /* chains (fanout followed by pipelines) */
    data->cached_chain = NULL;
    /* standard pipeline */
    data->cached_pipeline = NULL;
    /* in-order binary tree */
    data->cached_in_order_bintree = NULL;

    /* All done */
    tuned_module->super.base_data = data;

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init Tuned is in use"));
    return OMPI_SUCCESS;
}

int mca_coll_tuned_ft_event(int state) {
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}
