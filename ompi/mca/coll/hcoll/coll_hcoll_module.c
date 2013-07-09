/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

 $HEADER$
 */
#include "ompi_config.h"
#include "coll_hcoll.h"


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_hcoll_init_query(bool enable_progress_threads, bool enable_mpi_threads)
{

    return OMPI_SUCCESS;
}

static void mca_coll_hcoll_module_clear(mca_coll_hcoll_module_t *hcoll_module)
{
    hcoll_module->hcoll_context = NULL;
    hcoll_module->previous_barrier    = NULL;
    hcoll_module->previous_bcast      = NULL;
    hcoll_module->previous_reduce     = NULL;
    hcoll_module->previous_allreduce  = NULL;
    hcoll_module->previous_allgather  = NULL;
    hcoll_module->previous_allgatherv = NULL;
    hcoll_module->previous_gather     = NULL;
    hcoll_module->previous_gatherv    = NULL;
    hcoll_module->previous_alltoall   = NULL;
    hcoll_module->previous_alltoallv  = NULL;
    hcoll_module->previous_alltoallw  = NULL;
    hcoll_module->previous_reduce_scatter  = NULL;
}

static void mca_coll_hcoll_module_construct(mca_coll_hcoll_module_t *hcoll_module)
{
    mca_coll_hcoll_module_clear(hcoll_module);
}

static void mca_coll_hcoll_module_destruct(mca_coll_hcoll_module_t *hcoll_module)
{
    OBJ_RELEASE(hcoll_module->previous_barrier_module);
    OBJ_RELEASE(hcoll_module->previous_bcast_module);
    OBJ_RELEASE(hcoll_module->previous_reduce_module);
    OBJ_RELEASE(hcoll_module->previous_allreduce_module);
    OBJ_RELEASE(hcoll_module->previous_allgather_module);
    OBJ_RELEASE(hcoll_module->previous_allgatherv_module);
    OBJ_RELEASE(hcoll_module->previous_gather_module);
    OBJ_RELEASE(hcoll_module->previous_gatherv_module);
    OBJ_RELEASE(hcoll_module->previous_alltoall_module);
    OBJ_RELEASE(hcoll_module->previous_alltoallv_module);
    OBJ_RELEASE(hcoll_module->previous_alltoallw_module);
    OBJ_RELEASE(hcoll_module->previous_reduce_scatter_module);
    hcoll_destroy_context(hcoll_module->hcoll_context);
    mca_coll_hcoll_module_clear(hcoll_module);
}

#define HCOL_SAVE_PREV_COLL_API(__api) do {\
    hcoll_module->previous_ ## __api            = comm->c_coll.coll_ ## __api;\
    hcoll_module->previous_ ## __api ## _module = comm->c_coll.coll_ ## __api ## _module;\
    if (!comm->c_coll.coll_ ## __api || !comm->c_coll.coll_ ## __api ## _module) {\
        return OMPI_ERROR;\
    }\
    OBJ_RETAIN(hcoll_module->previous_ ## __api ## _module);\
} while(0)


static int __save_coll_handlers(mca_coll_hcoll_module_t *hcoll_module)
{
    ompi_communicator_t *comm = hcoll_module->comm;

    HCOL_SAVE_PREV_COLL_API(barrier);
    HCOL_SAVE_PREV_COLL_API(bcast);
    HCOL_SAVE_PREV_COLL_API(reduce);
    HCOL_SAVE_PREV_COLL_API(allreduce);
    HCOL_SAVE_PREV_COLL_API(allgather);
    HCOL_SAVE_PREV_COLL_API(allgatherv);
    HCOL_SAVE_PREV_COLL_API(gather);
    HCOL_SAVE_PREV_COLL_API(gatherv);
    HCOL_SAVE_PREV_COLL_API(alltoall);
    HCOL_SAVE_PREV_COLL_API(alltoallv);
    HCOL_SAVE_PREV_COLL_API(alltoallw);
    HCOL_SAVE_PREV_COLL_API(reduce_scatter);

    return OMPI_SUCCESS;
}


/*
 * Initialize module on the communicator
 */
static int mca_coll_hcoll_module_enable(mca_coll_base_module_t *module,
                                      struct ompi_communicator_t *comm)
{
    mca_coll_hcoll_module_t *hcoll_module = (mca_coll_hcoll_module_t*) module;
    hcoll_module->comm = comm;
    if (OMPI_SUCCESS != __save_coll_handlers(hcoll_module)){
        HCOL_ERROR("coll_hcol: __save_coll_handlers failed");
        return OMPI_ERROR;
    }

    hcoll_set_runtime_tag_offset(-100,mca_pml.pml_max_tag);

    hcoll_module->hcoll_context = 
            hcoll_create_context((rte_grp_handle_t)comm);
    if (NULL == hcoll_module->hcoll_context){
        HCOL_VERBOSE(1,"hcoll_create_context returned NULL");
        return OMPI_ERROR;
    }


#if 0
    {
        printf("\033[33mrank %d: DOING EXTRA TEST\033[0m\n",ompi_comm_rank(comm));
        fflush(stdout);
        sleep(1);
        rte_ec_handle_t handle;
        rte_grp_handle_t world_group = hcoll_rte_functions.rte_world_group_fn();
        int peer;
        const int max_count = 10000000;
        const int step = max_count/100;
        int buf = 0;
        int i;
        rte_request_handle_t req;
        peer = (ompi_comm_rank(comm)+1)%2;
        hcoll_rte_functions.get_ec_handles_fn(1,&peer,world_group,&handle);

        for (i=1; i<max_count+1; i++){
            if (0 == ompi_comm_rank(comm)){
                if (i/step*step == i){
                    printf("%d %% done...\n",i/step);fflush(stdout);
                }
                buf = 1;
                hcoll_rte_functions.send_fn(DTE_INT32,1,&buf,handle,world_group,0,&req);
            } else {
                hcoll_rte_functions.recv_fn(DTE_INT32,1,&buf,handle,world_group,0,&req);
            }
            int completed = 0;
            hcoll_rte_functions.test_fn(&req,&completed);
            while(!completed){
                hcoll_rte_functions.test_fn(&req,&completed);
                /*hcoll_rte_functions.rte_progress_fn();*/
                opal_progress();
            }
        }
        printf("\033[32mrank %d: EXTRA TEST PASS\033[0m\n",ompi_comm_rank(comm));
        fflush(stdout);
        sleep(1);
    }
#endif
    hcoll_module->super.coll_barrier(comm,module);
    return OMPI_SUCCESS;
}





/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_hcoll_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    mca_coll_base_module_t *module;
    mca_coll_hcoll_module_t *hcoll_module;

    *priority = 0;
    module = NULL;

    if (!mca_coll_hcoll_component.hcoll_enable){
        goto exit;
    }

    hcoll_module = OBJ_NEW(mca_coll_hcoll_module_t);
    if (!hcoll_module){
        goto exit;
    }

    if (ompi_comm_size(comm) < 2 || OMPI_COMM_IS_INTER(comm)){
        goto exit;
    }



    hcoll_module->super.coll_module_enable = mca_coll_hcoll_module_enable;
    hcoll_module->super.coll_barrier = hcoll_collectives.coll_barrier ? mca_coll_hcoll_barrier : NULL;
    hcoll_module->super.coll_bcast = hcoll_collectives.coll_bcast ? mca_coll_hcoll_bcast : NULL;
    hcoll_module->super.coll_allgather = hcoll_collectives.coll_allgather ? mca_coll_hcoll_allgather : NULL;
    hcoll_module->super.coll_allreduce = hcoll_collectives.coll_allreduce ? mca_coll_hcoll_allreduce : NULL;
    hcoll_module->super.coll_alltoall = /*hcoll_collectives.coll_alltoall ? mca_coll_hcoll_alltoall : */  NULL;

    *priority = mca_coll_hcoll_component.hcoll_priority;
    module = &hcoll_module->super;


exit:
    return module;
}


OBJ_CLASS_INSTANCE(mca_coll_hcoll_module_t,
        mca_coll_base_module_t,
        mca_coll_hcoll_module_construct,
        mca_coll_hcoll_module_destruct);



