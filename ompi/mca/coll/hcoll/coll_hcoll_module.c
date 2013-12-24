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
    hcoll_module->previous_ibarrier    = NULL;
    hcoll_module->previous_ibcast      = NULL;
    hcoll_module->previous_iallreduce  = NULL;
    hcoll_module->previous_iallgather  = NULL;
}

static void mca_coll_hcoll_module_construct(mca_coll_hcoll_module_t *hcoll_module)
{
    mca_coll_hcoll_module_clear(hcoll_module);
}

static void mca_coll_hcoll_module_destruct(mca_coll_hcoll_module_t *hcoll_module)
{
    opal_list_item_t  *item, *item_next;
    opal_list_t *am;
    mca_coll_hcoll_module_t *module;
    ompi_communicator_t *comm;
    int context_destroyed;

    am = &mca_coll_hcoll_component.active_modules;

    if (hcoll_module->comm == &ompi_mpi_comm_world.comm){
        /* If we get here then we are detroying MPI_COMM_WORLD now. So,
         * it is safe to destory all the other communicators and corresponding
         * hcoll contexts that could still be on the "active_modules" list.
         */
        item = opal_list_get_first(am);
        while (item != opal_list_get_end(am)){
            item_next = opal_list_get_next(item);
            module = ((mca_coll_hcoll_module_list_item_wrapper_t *)item)->module;
            comm = module->comm;
            context_destroyed = 0;
            while(!context_destroyed){
                hcoll_destroy_context(module->hcoll_context,
                                      (rte_grp_handle_t)comm,
                                      &context_destroyed);
            }
            module->hcoll_context = NULL;
            OBJ_RELEASE(comm);
            opal_list_remove_item(am,item);
            OBJ_RELEASE(item);
            item = item_next;
        }

        /* Now destory the comm_world hcoll context as well */
        context_destroyed = 0;
        while(!context_destroyed){
            hcoll_destroy_context(hcoll_module->hcoll_context,
                                  (rte_grp_handle_t)hcoll_module->comm,
                                  &context_destroyed);
        }
    }

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
    OBJ_RELEASE(hcoll_module->previous_ibarrier_module);
    OBJ_RELEASE(hcoll_module->previous_ibcast_module);
    OBJ_RELEASE(hcoll_module->previous_iallreduce_module);
    OBJ_RELEASE(hcoll_module->previous_iallgather_module);

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
    HCOL_SAVE_PREV_COLL_API(ibarrier);
    HCOL_SAVE_PREV_COLL_API(ibcast);
    HCOL_SAVE_PREV_COLL_API(iallreduce);
    HCOL_SAVE_PREV_COLL_API(iallgather);

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

    if (comm != &ompi_mpi_comm_world.comm){
        mca_coll_hcoll_module_list_item_wrapper_t *mw =
            OBJ_NEW(mca_coll_hcoll_module_list_item_wrapper_t);
        mw->module = hcoll_module;
        OBJ_RETAIN(hcoll_module->comm);
        opal_list_append(&mca_coll_hcoll_component.active_modules,
                         (opal_list_item_t*)mw);
    }

    return OMPI_SUCCESS;
}

int mca_coll_hcoll_progress(void)
{
    opal_list_item_t  *item, *item_next;
    opal_list_t *am;
    mca_coll_hcoll_module_t *module;
    ompi_communicator_t *comm;
    int context_destroyed;
    OPAL_THREAD_ADD32(&mca_coll_hcoll_component.progress_lock,1);

    am = &mca_coll_hcoll_component.active_modules;

    if (mca_coll_hcoll_component.progress_lock){
        OPAL_THREAD_ADD32(&mca_coll_hcoll_component.progress_lock,-1);
        (*hcoll_progress_fn)();
        return OMPI_SUCCESS;
    }
    if (ompi_mpi_finalized){
        hcoll_rte_p2p_disabled_notify();
    }

    item = opal_list_get_first(am);
    while (item != opal_list_get_end(am)){
        item_next = opal_list_get_next(item);
        module = ((mca_coll_hcoll_module_list_item_wrapper_t *)item)->module;
        comm = module->comm;
        if (((opal_object_t*)comm)->obj_reference_count == 1){
            /* Ok, if we are here then nobody owns a communicator pointed with comm except
             * for coll_hcoll. Hence, it is safe to remove the hcoll context firstly and
             * call release on the communicator.
             *
             * The call to hcoll_destroy_context is not blocking. The last parameter on the return
             * indicates whether the context has been destroyd (1) or not (0). In the latter
             * case one should call destroy again after some progressing
             */
            context_destroyed = 0;
            hcoll_destroy_context(module->hcoll_context,
                                  (rte_grp_handle_t)comm,
                                  &context_destroyed);
            if (context_destroyed){
                module->hcoll_context = NULL;
                OBJ_RELEASE(comm);
                opal_list_remove_item(am,item);
                OBJ_RELEASE(item);
            }
        }
        item = item_next;
    }

    (*hcoll_progress_fn)();
    OPAL_THREAD_ADD32(&mca_coll_hcoll_component.progress_lock,-1);
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
    static bool libhcoll_initialized = false;
    int rc;

    *priority = 0;
    module = NULL;

    if (!mca_coll_hcoll_component.hcoll_enable){
        goto exit;
    }

    if (!libhcoll_initialized)
    {
        /* libhcoll should be initialized here since current implmentation of
           mxm bcol in libhcoll needs world_group fully functional during init
           world_group, i.e. ompi_comm_world, is not ready at hcoll component open
           call */
        opal_progress_register(mca_coll_hcoll_progress);
        rc = hcoll_init();

        if (HCOLL_SUCCESS != rc){
            mca_coll_hcoll_component.hcoll_enable = 0;
            opal_progress_unregister(hcoll_progress_fn);
            HCOL_VERBOSE(0,"Hcol library init failed");
            return NULL;
        }
        libhcoll_initialized = true;
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
    hcoll_module->super.coll_ibarrier = hcoll_collectives.coll_ibarrier ? mca_coll_hcoll_ibarrier : NULL;
    hcoll_module->super.coll_ibcast = hcoll_collectives.coll_ibcast ? mca_coll_hcoll_ibcast : NULL;
    hcoll_module->super.coll_iallgather = hcoll_collectives.coll_iallgather ? mca_coll_hcoll_iallgather : NULL;
    hcoll_module->super.coll_iallreduce = hcoll_collectives.coll_iallreduce ? mca_coll_hcoll_iallreduce : NULL;
    hcoll_module->super.coll_gather = hcoll_collectives.coll_gather ? mca_coll_hcoll_gather : NULL;

    *priority = mca_coll_hcoll_component.hcoll_priority;
    module = &hcoll_module->super;


exit:
    return module;
}


OBJ_CLASS_INSTANCE(mca_coll_hcoll_module_t,
        mca_coll_base_module_t,
        mca_coll_hcoll_module_construct,
        mca_coll_hcoll_module_destruct);



OBJ_CLASS_INSTANCE(mca_coll_hcoll_module_list_item_wrapper_t,
                   opal_list_item_t,
                   NULL,NULL);

