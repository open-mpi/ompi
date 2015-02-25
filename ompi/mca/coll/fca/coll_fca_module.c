/*
 * Copyright (c) 2011      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "coll_fca.h"

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */

int modular_pow(uint64_t base, uint64_t exponent, uint64_t modulus)
{
    int result = 1;
    while(exponent>0)
    {
        if((exponent % 2) == 1)
            result = (result * base) % modulus;
        exponent = exponent >> 1;
        base = (base * base) % modulus;
    }
    return result;
}

int mca_coll_fca_init_query(bool enable_progress_threads,
                            bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}

static int have_remote_peers(ompi_group_t *group, size_t size, int *local_peers)
{
    ompi_proc_t *proc;
    size_t i;
    int ret;

    *local_peers = 0;
    ret = 0;
    for (i = 0; i < size; ++i) {
        proc = ompi_group_peer_lookup(group, i);
        if (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)) {
            ++*local_peers;
        } else {
            ret = 1;
        }
    }
    return ret;
}

static inline ompi_proc_t* __local_rank_lookup(ompi_communicator_t *comm, int rank)
{
    return ompi_group_peer_lookup(comm->c_local_group, rank);
}

/**
 * Fills local rank information in fca_module.
 */
static int __get_local_ranks(mca_coll_fca_module_t *fca_module)
{
    ompi_communicator_t *comm = fca_module->comm;
    ompi_proc_t* proc;
    int i, rank;

    /* Count the local ranks */
    fca_module->num_local_procs = 0;
    for (rank = 0; rank < ompi_comm_size(comm); ++rank) {
        proc = __local_rank_lookup(comm, rank);
        if (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)) {
            if (rank == fca_module->rank) {
                fca_module->local_proc_idx = fca_module->num_local_procs;
            }
            ++fca_module->num_local_procs;
        }
    }

    /* Make a list of local ranks */
    fca_module->local_ranks = calloc(fca_module->num_local_procs,
                                     sizeof *fca_module->local_ranks);
    if (!fca_module->local_ranks) {
        FCA_ERROR("Failed to allocate memory for %d local ranks",
                  fca_module->num_local_procs);
        return OMPI_ERROR;
    }

    i = 0;
    for (rank = 0; rank < ompi_comm_size(comm); ++rank) {
        proc = __local_rank_lookup(comm, rank);
        if (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)) {
            fca_module->local_ranks[i++] = rank;
        }
    }

    FCA_MODULE_VERBOSE(fca_module, 3, "i am %d/%d", fca_module->local_proc_idx,
                       fca_module->num_local_procs);
    return OMPI_SUCCESS;
}

static int __fca_comm_new(mca_coll_fca_module_t *fca_module)
{
    ompi_communicator_t *comm = fca_module->comm;
    fca_comm_new_spec_t spec = {0,};
    int info_size, all_info_size;
    void *all_info = NULL;
    void *my_info = NULL;
    int *rcounts = NULL;
    int *displs = NULL;
    int i, rc, ret, comm_size = ompi_comm_size(fca_module->comm);

    /* call fca_get_rank_info() on node managers only*/
    if (fca_module->local_proc_idx == 0) {
#if OMPI_FCA_VERSION >= 30
        my_info = fca_get_rank_info(mca_coll_fca_component.fca_context,
                                    fca_module->rank, &info_size);
#else
        my_info = fca_get_rank_info(mca_coll_fca_component.fca_context,
                                    &info_size);
#endif
        if (!my_info) {
            FCA_ERROR("fca_get_rank_info returned NULL");
            return OMPI_ERROR;
        }
    } else {
        info_size = 0;
    }
    FCA_MODULE_VERBOSE(fca_module, 1, "Info size: %d", info_size);

    /* Allocate gather buffer on the root rank */
    if (fca_module->rank == 0) {
        rcounts = calloc(comm_size, sizeof *rcounts);
    }

    /* Get all rank info sizes using MPI_Gather */
    rc = comm->c_coll.coll_gather(&info_size, 1, MPI_INT, rcounts, 1, MPI_INT, 0,
                                  comm, comm->c_coll.coll_gather_module);
    if (rc != OMPI_SUCCESS)
        return rc;

    /* Allocate buffer for gathering rank information on rank0 */
    if (fca_module->rank == 0) {
        all_info_size = 0;
        displs = calloc(comm_size, sizeof *displs);

        for (i = 0; i < comm_size; ++i) {
            displs[i] = all_info_size;
            all_info_size += rcounts[i];

            if (rcounts[i] > 0)
                ++spec.rank_count;

            FCA_MODULE_VERBOSE(fca_module, 1, "gatherv: rcounts[%d]=%d displs[%d]=%d",
                               i, rcounts[i], i, displs[i]);
        }

        FCA_MODULE_VERBOSE(fca_module, 1, "Total rank_info size: %d", all_info_size);
        all_info = calloc(all_info_size, 1);
    }

    /* Send all node managers information to rank0 using MPI_Gatherv */
    rc = comm->c_coll.coll_gatherv(my_info, info_size, MPI_BYTE,
                                   all_info, rcounts, displs, MPI_BYTE, 0,
                                   comm, comm->c_coll.coll_gather_module);
    if (rc != OMPI_SUCCESS) {
        FCA_ERROR("Failed to gather rank information to rank0: %d", rc);
        return rc;
    }

    /* Rank0 calls fca_comm_new() and fills fca_comm_spec filed */
    if (fca_module->rank == 0) {
        spec.rank_info  = all_info;
        spec.is_comm_world = comm == MPI_COMM_WORLD;

        free(displs);
        free(rcounts);
#if OMPI_FCA_VERSION >= 30
        spec.comm_size = comm_size;

        spec.comm_init_data = NULL;
        spec.comm_init_data_size = 0;
#endif
        FCA_MODULE_VERBOSE(fca_module, 1, "starting fca_comm_new(), rank_count: %d",
                           spec.rank_count);

        ret = fca_comm_new(mca_coll_fca_component.fca_context,
                           &spec, &fca_module->fca_comm_desc);

        free(all_info);
    }

    /* Broadcast return value from rank0 to all other ranks */
    rc = fca_module->previous_bcast(&ret, 1, MPI_INT, 0, comm,
                                    fca_module->previous_bcast_module);
    if (rc != OMPI_SUCCESS) {
        FCA_ERROR("Failed to broadcast comm_new return value from rank0: %d", rc);
        return rc;
    }

    /* Examine comm_new return value */
    if (ret < 0) {
        FCA_ERROR("COMM_NEW failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }
#if OMPI_FCA_VERSION >= 30
    /* Send comm_ini_data_size to all ranks in comm */
    rc = fca_module->previous_bcast(&spec.comm_init_data_size, 1, MPI_INT,
                                    0, comm, fca_module->previous_bcast_module);
    if (OMPI_SUCCESS != rc) {
        FCA_ERROR("Failed to broadcast comm init data size value from rank0: %d", rc);
        return rc;
    }

    if (0 != fca_module->rank &&
        NULL == (spec.comm_init_data = calloc(1, spec.comm_init_data_size))) {
        FCA_ERROR("Failed to allocate memory for comm init data.");
        return OMPI_ERROR;
    }

    /* Send comm_ini_data to all ranks in comm */
    rc = fca_module->previous_scatter(spec.comm_init_data, spec.comm_init_data_size, MPI_BYTE,
                                      spec.comm_init_data, spec.comm_init_data_size, MPI_BYTE,
                                      0, comm, fca_module->previous_scatter_module);
    if (OMPI_SUCCESS != rc) {
        FCA_ERROR("Failed to scatter comm_init sizes return value from rank0: %d", rc);
        return rc;
    }
#endif
    /* Release allocate rank_info on node managers */
    if (fca_module->local_proc_idx == 0) {
        fca_free_rank_info(my_info);
    }

    /* Pass fca_comm_desc to all ranks using MPI_Bcast */
    rc = fca_module->previous_bcast(&fca_module->fca_comm_desc,
                                    sizeof(fca_module->fca_comm_desc), MPI_BYTE, 0,
                                    comm, fca_module->previous_bcast_module);
    if (rc != OMPI_SUCCESS) {
        FCA_ERROR("Failed to broadcast comm_desc from rank0: %d", rc);
        return rc;
    }

    FCA_MODULE_VERBOSE(fca_module, 1, "Received FCA communicator spec, comm_id %d",
                       fca_module->fca_comm_desc.comm_id);

#if OMPI_FCA_VERSION >= 30
    /* allocate comm_init_spec */
    FCA_MODULE_VERBOSE(fca_module, 1, "Starting COMM_INIT comm_id %d proc_idx %d num_procs %d",
                       fca_module->fca_comm_desc.comm_id, fca_module->local_proc_idx,
                       fca_module->num_local_procs);

    ret = mca_coll_fca_comm_init(mca_coll_fca_component.fca_context,
                                 fca_module->rank, comm_size,
                                 fca_module->local_proc_idx, fca_module->num_local_procs,
                                 &fca_module->fca_comm_desc, &fca_module->fca_comm,
                                 spec.comm_init_data);
    if (ret < 0) {
        FCA_ERROR("COMM_INIT failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }

    if (0 != fca_module->rank) {
        free(spec.comm_init_data);
    }
#endif
    return OMPI_SUCCESS;
}

static int __create_fca_comm(mca_coll_fca_module_t *fca_module)
{
    int rc, ret;
    int result = MPI_UNEQUAL;
    mca_coll_fca_c_cache_item_t *c_item = NULL, *c_item_new = NULL;
    mca_coll_fca_component_t *cm = &mca_coll_fca_component;
    int comm_size = ompi_comm_size(fca_module->comm);
    ompi_communicator_t *comm = fca_module->comm;
    opal_list_t *c_cache;
    struct timeval start, end, seq_start, seq_end, par_start, par_end;
    int act_deep;
    

    if(mca_coll_fca_component.fca_verbose == 10) {
        gettimeofday(&start, NULL);
    }

    if(mca_coll_fca_component.fca_enable_cache) {

        c_cache = &cm->c_cache;

        if(mca_coll_fca_component.fca_enable_hash){

            int  grank = ORTE_PROC_MY_NAME->vpid;
            int hash_index, part_of_hash_index;

            if(mca_coll_fca_component.fca_parallel_hash_calc == 1) {
                
                if(mca_coll_fca_component.fca_verbose == 10){
                    gettimeofday(&par_start, NULL);
                }

                part_of_hash_index = modular_pow(cm->fca_primes[grank % cm->fca_number_of_primes], grank, cm->fca_hash_size);
                rc = comm->c_coll.coll_allreduce(&part_of_hash_index, &hash_index, 1, MPI_INT, MPI_SUM, comm, comm->c_coll.coll_allreduce_module);
                if (rc != OMPI_SUCCESS) {
                    FCA_ERROR("Failed to reduce hash_index: %d", rc);
                    return rc;
                }

                if(mca_coll_fca_component.fca_verbose == 10){
                    gettimeofday(&par_end, NULL);
                    mca_coll_fca_component.fca_work_time_parallel =+
                        par_end.tv_sec - par_start.tv_sec + 1e-6 * (par_end.tv_usec - par_start.tv_usec);
                }
            }else{

                if(mca_coll_fca_component.fca_verbose == 10){
                    gettimeofday(&seq_start, NULL);
                }

                hash_index = 0;
                int q_counter = 0;
                int q_comm_size = ompi_comm_size (comm);

                for(q_counter = 0; q_counter < q_comm_size; q_counter++)
                {
                    hash_index += modular_pow(cm->fca_primes[q_counter % cm->fca_number_of_primes], q_counter, cm->fca_hash_size);
                }

                if(mca_coll_fca_component.fca_verbose == 10){
                    gettimeofday(&seq_end, NULL);
                    mca_coll_fca_component.fca_work_time_sequency =+
                        seq_end.tv_sec - seq_start.tv_sec + 1e-6 * (seq_end.tv_usec - seq_start.tv_usec);
                }

            }

            if(cm->fca_hash[hash_index % cm->fca_hash_size] != NULL)
            {
                c_cache = cm->fca_hash[hash_index % cm->fca_hash_size];
                if(mca_coll_fca_component.fca_verbose == 10) {
                    gettimeofday(&end, NULL);
                    mca_coll_fca_component.fca_total_work_time =+
                        end.tv_sec - start.tv_sec + 1e-6 * (end.tv_usec - start.tv_usec);
                    mca_coll_fca_component.fca_hash_hit += 1;
                }
            }else
            {
                if(mca_coll_fca_component.fca_verbose == 10) {
                    mca_coll_fca_component.fca_hash_miss += 1;
                }
                c_cache = OBJ_NEW(opal_list_t);
                cm->fca_hash[hash_index % cm->fca_hash_size] = c_cache;
            }

        }

        act_deep = 0;
        for( c_item = (mca_coll_fca_c_cache_item_t *)opal_list_get_first(c_cache);
                c_item != (mca_coll_fca_c_cache_item_t *)opal_list_get_end(c_cache);
                c_item = (mca_coll_fca_c_cache_item_t *)opal_list_get_next((opal_list_item_t *) c_item)){
            act_deep++;
            /* first check the size */
            if( c_item && (comm_size == c_item->size)) {
                /* then we have a potential cache hit */
                ompi_comm_compare(comm, c_item->comm, &result); 
                if( MPI_CONGRUENT == result) {
                    /* cache hit! Return the context and be done with it */
                    /* first bump the score */
                    ret = fca_comm_get_caps(c_item->fca_comm_wrap->fca_comm, &fca_module->fca_comm_caps);
                    if (ret < 0) {
                        FCA_ERROR("GET_COMM_CAPS failed: %s", fca_strerror(ret));
                        return OMPI_ERROR;
                    }
                    fca_module->fca_comm = c_item->fca_comm_wrap->fca_comm;

                    if(mca_coll_fca_component.fca_verbose == 10) {
                        gettimeofday(&end, NULL);
                        
                        mca_coll_fca_component.fca_total_work_time =+
                            end.tv_sec - start.tv_sec + 1e-6 * (end.tv_usec - start.tv_usec);
                        
                        mca_coll_fca_component.fca_cache_hit += 1;
                        
                        if(act_deep>mca_coll_fca_component.fca_max_deep_in_cache)
                            mca_coll_fca_component.fca_max_deep_in_cache = act_deep;
                    }
                    return OMPI_SUCCESS;
                }
            }
        }
    }
    rc = __fca_comm_new(fca_module);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }
#if OMPI_FCA_VERSION < 30
    /* allocate comm_init_spec */
    FCA_MODULE_VERBOSE(fca_module, 1, "Starting COMM_INIT comm_id %d proc_idx %d num_procs %d",
                       fca_module->fca_comm_desc.comm_id, fca_module->local_proc_idx,
                       fca_module->num_local_procs);

    ret = mca_coll_fca_comm_init(mca_coll_fca_component.fca_context,
                                 fca_module->rank, ompi_comm_size(fca_module->comm),
                                 fca_module->local_proc_idx, fca_module->num_local_procs,
                                 &fca_module->fca_comm_desc, &fca_module->fca_comm);
    if (ret < 0) {
        FCA_ERROR("COMM_INIT failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
     }
#endif
    /* get communicator capabilities */
    ret = fca_comm_get_caps(fca_module->fca_comm, &fca_module->fca_comm_caps);
    if (ret < 0) {
        FCA_ERROR("GET_COMM_CAPS failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }

    /* by this point every rank in the communicator is set up */
    FCA_MODULE_VERBOSE(fca_module, 1, "Initialized FCA communicator, comm_id %d",
            fca_module->fca_comm_desc.comm_id);
    if(mca_coll_fca_component.fca_enable_cache) {

        c_item_new = OBJ_NEW(mca_coll_fca_c_cache_item_t);
        c_item_new->fca_comm_wrap = OBJ_NEW(mca_coll_fca_comm_wrap_t);

        OBJ_RETAIN(comm); 

        c_item_new->size = comm_size;
        c_item_new->comm = comm;
        c_item_new->fca_comm_wrap->fca_comm = fca_module->fca_comm;
        c_item_new->fca_comm_wrap->rank = fca_module->rank;
        c_item_new->fca_comm_wrap->comm_id = fca_module->fca_comm_desc.comm_id;

        opal_list_append(c_cache,(opal_list_item_t *) c_item_new);
    }

    if(mca_coll_fca_component.fca_verbose == 10) {
        
        gettimeofday(&end, NULL);
        
        mca_coll_fca_component.fca_total_work_time =+
            end.tv_sec - start.tv_sec + 1e-6 * (end.tv_usec - start.tv_usec);

        mca_coll_fca_component.fca_cache_miss += 1;
    }
    return OMPI_SUCCESS;
}

static void __destroy_fca_comm(mca_coll_fca_module_t *fca_module)
{
    int ret;

    fca_comm_destroy(fca_module->fca_comm);
    if (fca_module->rank == 0) {
        ret = fca_comm_end(mca_coll_fca_component.fca_context,
                                                      fca_module->fca_comm_desc.comm_id);
        if (ret < 0) {
            FCA_ERROR("COMM_END failed: %s", fca_strerror(ret));
        }
    }

    FCA_MODULE_VERBOSE(fca_module, 1, "Destroyed FCA communicator, comm_id %d",
                       fca_module->fca_comm_desc.comm_id);
}

#define FCA_SAVE_PREV_COLL_API(__api) do {\
    fca_module->previous_ ## __api            = comm->c_coll.coll_ ## __api;\
    fca_module->previous_ ## __api ## _module = comm->c_coll.coll_ ## __api ## _module;\
    if (!comm->c_coll.coll_ ## __api || !comm->c_coll.coll_ ## __api ## _module) {\
        FCA_VERBOSE(1, "(%d/%s): no underlying " # __api"; disqualifying myself",\
                    comm->c_contextid, comm->c_name);\
        return OMPI_ERROR;\
    }\
    OBJ_RETAIN(fca_module->previous_ ## __api ## _module);\
} while(0)

static int __save_coll_handlers(mca_coll_fca_module_t *fca_module)
{
    ompi_communicator_t *comm = fca_module->comm;

    FCA_SAVE_PREV_COLL_API(barrier);
    FCA_SAVE_PREV_COLL_API(bcast);
    FCA_SAVE_PREV_COLL_API(reduce);
    FCA_SAVE_PREV_COLL_API(allreduce);
    FCA_SAVE_PREV_COLL_API(allgather);
    FCA_SAVE_PREV_COLL_API(allgatherv);
    FCA_SAVE_PREV_COLL_API(gather);
    FCA_SAVE_PREV_COLL_API(gatherv);
    FCA_SAVE_PREV_COLL_API(alltoall);
    FCA_SAVE_PREV_COLL_API(alltoallv);
    FCA_SAVE_PREV_COLL_API(alltoallw);
    FCA_SAVE_PREV_COLL_API(reduce_scatter);
#if OMPI_FCA_VERSION >= 30
    FCA_SAVE_PREV_COLL_API(scatter);
#endif

    return OMPI_SUCCESS;
}

/*
 * Initialize module on the communicator
 */
static int mca_coll_fca_module_enable(mca_coll_base_module_t *module,
                                      struct ompi_communicator_t *comm)
{

    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*) module;
    int rc;

    fca_module->comm = comm;
    fca_module->rank = ompi_comm_rank(comm);

    rc = mca_coll_fca_get_fca_lib(comm);
    if (rc != OMPI_SUCCESS)
        return rc;

    rc = __save_coll_handlers(fca_module);
    if (rc != OMPI_SUCCESS)
        return rc;

    rc = __get_local_ranks(fca_module);
    if (rc != OMPI_SUCCESS)
        return rc;

    rc = __create_fca_comm(fca_module);
    if (rc != OMPI_SUCCESS)
        return rc;

    FCA_MODULE_VERBOSE(fca_module, 1, "FCA Module initialized");
    return OMPI_SUCCESS;
}


static int mca_coll_fca_ft_event(int state)
{    
    return OMPI_SUCCESS;
}

static void mca_coll_fca_module_clear(mca_coll_fca_module_t *fca_module)
{
    fca_module->num_local_procs = 0;
    fca_module->local_ranks = NULL;
    fca_module->fca_comm = NULL;

    fca_module->previous_barrier    = NULL;
    fca_module->previous_bcast      = NULL;
    fca_module->previous_reduce     = NULL;
    fca_module->previous_allreduce  = NULL;
    fca_module->previous_allgather  = NULL;
    fca_module->previous_allgatherv = NULL;
    fca_module->previous_gather     = NULL;
    fca_module->previous_gatherv    = NULL;
    fca_module->previous_alltoall   = NULL;
    fca_module->previous_alltoallv  = NULL;
    fca_module->previous_alltoallw  = NULL;
    fca_module->previous_reduce_scatter  = NULL;
}

static void mca_coll_fca_module_construct(mca_coll_fca_module_t *fca_module)
{
    FCA_VERBOSE(5, "==>");
    mca_coll_fca_module_clear(fca_module);
}

static void mca_coll_fca_module_destruct(mca_coll_fca_module_t *fca_module)
{
    FCA_VERBOSE(5, "==>");
    if(mca_coll_fca_component.fca_enable_cache == 0) {

        if (fca_module->fca_comm) {
            __destroy_fca_comm(fca_module);
        } 
    }

    OBJ_RELEASE(fca_module->previous_barrier_module);
    OBJ_RELEASE(fca_module->previous_bcast_module);
    OBJ_RELEASE(fca_module->previous_reduce_module);
    OBJ_RELEASE(fca_module->previous_allreduce_module);
    OBJ_RELEASE(fca_module->previous_allgather_module);
    OBJ_RELEASE(fca_module->previous_allgatherv_module);
    OBJ_RELEASE(fca_module->previous_gather_module);
    OBJ_RELEASE(fca_module->previous_gatherv_module);
    OBJ_RELEASE(fca_module->previous_alltoall_module);
    OBJ_RELEASE(fca_module->previous_alltoallv_module);
    OBJ_RELEASE(fca_module->previous_alltoallw_module);
    OBJ_RELEASE(fca_module->previous_reduce_scatter_module);
#if OMPI_FCA_VERSION >= 30
    OBJ_RELEASE(fca_module->previous_scatter_module);
#endif

    free(fca_module->local_ranks);
    mca_coll_fca_module_clear(fca_module);
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_fca_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    mca_coll_base_module_t *module;
    int size = ompi_comm_size(comm);
    int local_peers = 0;
    mca_coll_fca_module_t *fca_module;

    *priority = 0;
    module = NULL;

    if (!mca_coll_fca_component.fca_enable)
        goto exit;

    if (size < mca_coll_fca_component.fca_np)
        goto exit;

    if (!have_remote_peers(comm->c_local_group, size, &local_peers) || OMPI_COMM_IS_INTER(comm))
        goto exit;

    fca_module = OBJ_NEW(mca_coll_fca_module_t);
    if (!fca_module)
        goto exit;

    fca_module->super.coll_module_enable = mca_coll_fca_module_enable;
    fca_module->super.ft_event        = mca_coll_fca_ft_event;
    fca_module->super.coll_allgather  = mca_coll_fca_component.fca_enable_allgather?  mca_coll_fca_allgather  : NULL;
    fca_module->super.coll_allgatherv = mca_coll_fca_component.fca_enable_allgatherv? mca_coll_fca_allgatherv : NULL;
    fca_module->super.coll_allreduce  = mca_coll_fca_component.fca_enable_allreduce?  mca_coll_fca_allreduce  : NULL;
    fca_module->super.coll_alltoall   = mca_coll_fca_component.fca_enable_alltoall?   mca_coll_fca_alltoall   : NULL;
    fca_module->super.coll_alltoallv  = mca_coll_fca_component.fca_enable_alltoallv?  mca_coll_fca_alltoallv  : NULL;
    fca_module->super.coll_alltoallw  = mca_coll_fca_component.fca_enable_alltoallw?  mca_coll_fca_alltoallw  : NULL;
    fca_module->super.coll_barrier    = mca_coll_fca_component.fca_enable_barrier?    mca_coll_fca_barrier    : NULL;
    fca_module->super.coll_bcast      = mca_coll_fca_component.fca_enable_bcast?      mca_coll_fca_bcast      : NULL;
    fca_module->super.coll_exscan     = NULL;
    fca_module->super.coll_gather     = mca_coll_fca_component.fca_enable_gather?     mca_coll_fca_gather     : NULL;
    fca_module->super.coll_gatherv    = mca_coll_fca_component.fca_enable_gatherv?    mca_coll_fca_gatherv    : NULL;
    fca_module->super.coll_reduce     = mca_coll_fca_component.fca_enable_reduce?     mca_coll_fca_reduce     : NULL;
    fca_module->super.coll_reduce_scatter = mca_coll_fca_component.fca_enable_reduce_scatter? mca_coll_fca_reduce_scatter  : NULL;
    fca_module->super.coll_scan       = NULL;
    fca_module->super.coll_scatter    = NULL;
    fca_module->super.coll_scatterv   = NULL;

    *priority = mca_coll_fca_component.fca_priority;
    module = &fca_module->super;

exit:
    FCA_VERBOSE(4, "Query FCA module for comm %p size %d rank %d local_peers=%d: priority=%d %s",
                (void *)comm, size, ompi_comm_rank(comm), local_peers,
                *priority, module ? "enabled" : "disabled");
    return module;
}

OBJ_CLASS_INSTANCE(mca_coll_fca_module_t,
                   mca_coll_base_module_t,
                   mca_coll_fca_module_construct,
                   mca_coll_fca_module_destruct);


static void mca_coll_fca_comm_wrap_constructor(mca_coll_fca_comm_wrap_t *item) {
    item->fca_comm = NULL;
    item->comm_id = -1;
    item->rank = -1;
}

static void mca_coll_fca_comm_wrap_destruct(mca_coll_fca_comm_wrap_t *item) {

    int ret;

    if(item->fca_comm != NULL)
    {
        fca_comm_destroy(item->fca_comm);
        if (item->rank == 0) {
            ret = fca_comm_end(mca_coll_fca_component.fca_context,
                    item->comm_id);
            if (ret < 0) {
                FCA_ERROR("COMM_END failed: %s", fca_strerror(ret));
            }
        }

    }

}

OBJ_CLASS_INSTANCE(mca_coll_fca_comm_wrap_t,
        opal_object_t,
        mca_coll_fca_comm_wrap_constructor,
        mca_coll_fca_comm_wrap_destruct);


static void mca_coll_fca_c_cache_item_construct(mca_coll_fca_c_cache_item_t *item) {
        item->comm = NULL;
        item->size = -1;
        /* item->fca_comm_wrap = OBJ_NEW(mca_coll_fca_comm_wrap_t); */
        item->fca_comm_wrap = NULL;
}

static void mca_coll_fca_c_cache_item_destruct(mca_coll_fca_c_cache_item_t *item) {
    if(item->fca_comm_wrap != NULL) {
        OBJ_RELEASE(item->fca_comm_wrap);
        /* OBJ_RELEASE(item->comm); */
    }
} 

OBJ_CLASS_INSTANCE(mca_coll_fca_c_cache_item_t,
        opal_list_item_t,
        mca_coll_fca_c_cache_item_construct,
        mca_coll_fca_c_cache_item_destruct);
