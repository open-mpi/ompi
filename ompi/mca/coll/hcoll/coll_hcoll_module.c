/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

 $HEADER$
 */

#include "ompi_config.h"
#include "coll_hcoll.h"

int hcoll_comm_attr_keyval;

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
    hcoll_module->previous_igatherv    = NULL;
}

static void mca_coll_hcoll_module_construct(mca_coll_hcoll_module_t *hcoll_module)
{
    mca_coll_hcoll_module_clear(hcoll_module);
}

void mca_coll_hcoll_mem_release_cb(void *buf, size_t length,
                                          void *cbdata, bool from_alloc)
{
#if HCOLL_API > HCOLL_VERSION(3,0)
    hcoll_mem_unmap(buf, length, cbdata, from_alloc);
#endif
}

static void mca_coll_hcoll_module_destruct(mca_coll_hcoll_module_t *hcoll_module)
{
    mca_coll_hcoll_module_t *module;
    ompi_communicator_t *comm;
    int context_destroyed;

    if (hcoll_module->comm == &ompi_mpi_comm_world.comm){
        if (OMPI_SUCCESS != ompi_attr_free_keyval(COMM_ATTR, &hcoll_comm_attr_keyval, 0)) {
            HCOL_VERBOSE(1,"hcoll ompi_attr_free_keyval failed");
        }
    }

    /* If the hcoll_context is null then we are destroying the hcoll_module
       that didn't initialized fallback colls/modules.
       Then just clear and return. Otherwise release module pointers and
       destroy hcoll context*/

    if (hcoll_module->hcoll_context != NULL){
        OBJ_RELEASE(hcoll_module->previous_barrier_module);
        OBJ_RELEASE(hcoll_module->previous_bcast_module);
        OBJ_RELEASE(hcoll_module->previous_allreduce_module);
        OBJ_RELEASE(hcoll_module->previous_allgather_module);
        OBJ_RELEASE(hcoll_module->previous_gatherv_module);

        OBJ_RELEASE(hcoll_module->previous_ibarrier_module);
        OBJ_RELEASE(hcoll_module->previous_ibcast_module);
        OBJ_RELEASE(hcoll_module->previous_iallreduce_module);
        OBJ_RELEASE(hcoll_module->previous_iallgather_module);
        OBJ_RELEASE(hcoll_module->previous_igatherv_module);

        /*
        OBJ_RELEASE(hcoll_module->previous_allgatherv_module);
        OBJ_RELEASE(hcoll_module->previous_gather_module);
        OBJ_RELEASE(hcoll_module->previous_gatherv_module);
        OBJ_RELEASE(hcoll_module->previous_alltoall_module);
        OBJ_RELEASE(hcoll_module->previous_alltoallv_module);
        OBJ_RELEASE(hcoll_module->previous_alltoallw_module);
        OBJ_RELEASE(hcoll_module->previous_reduce_scatter_module);
        OBJ_RELEASE(hcoll_module->previous_reduce_module);
        */

        context_destroyed = 0;
        hcoll_destroy_context(hcoll_module->hcoll_context,
                              (rte_grp_handle_t)hcoll_module->comm,
                              &context_destroyed);
        assert(context_destroyed);
    }
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


static int mca_coll_hcoll_save_coll_handlers(mca_coll_hcoll_module_t *hcoll_module)
{
    ompi_communicator_t *comm;
    comm = hcoll_module->comm;

    HCOL_SAVE_PREV_COLL_API(barrier);
    HCOL_SAVE_PREV_COLL_API(bcast);
    HCOL_SAVE_PREV_COLL_API(allreduce);
    HCOL_SAVE_PREV_COLL_API(allgather);
    HCOL_SAVE_PREV_COLL_API(gatherv);

    HCOL_SAVE_PREV_COLL_API(ibarrier);
    HCOL_SAVE_PREV_COLL_API(ibcast);
    HCOL_SAVE_PREV_COLL_API(iallreduce);
    HCOL_SAVE_PREV_COLL_API(iallgather);
    HCOL_SAVE_PREV_COLL_API(igatherv);

    /*
      These collectives are not yet part of hcoll, so
      don't retain them on hcoll module
    HCOL_SAVE_PREV_COLL_API(reduce_scatter);
    HCOL_SAVE_PREV_COLL_API(gather);
    HCOL_SAVE_PREV_COLL_API(reduce);
    HCOL_SAVE_PREV_COLL_API(allgatherv);
    HCOL_SAVE_PREV_COLL_API(alltoall);
    HCOL_SAVE_PREV_COLL_API(alltoallv);
    HCOL_SAVE_PREV_COLL_API(alltoallw);
    */
    return OMPI_SUCCESS;
}



/*
** Communicator free callback
*/
int hcoll_comm_attr_del_fn(MPI_Comm comm, int keyval, void *attr_val, void *extra)
{

    mca_coll_hcoll_module_t *hcoll_module;
    hcoll_module = (mca_coll_hcoll_module_t*) attr_val;

    hcoll_group_destroy_notify(hcoll_module->hcoll_context);
    return OMPI_SUCCESS;

}
/*
 * Initialize module on the communicator
 */
static int mca_coll_hcoll_module_enable(mca_coll_base_module_t *module,
                                        struct ompi_communicator_t *comm)
{
    int ret;

    if (OMPI_SUCCESS != mca_coll_hcoll_save_coll_handlers((mca_coll_hcoll_module_t *)module)){
        HCOL_ERROR("coll_hcol: mca_coll_hcoll_save_coll_handlers failed");
        return OMPI_ERROR;
    }

    ret = ompi_attr_set_c(COMM_ATTR, comm, &comm->c_keyhash, hcoll_comm_attr_keyval, (void *)module, false);
    if (OMPI_SUCCESS != ret) {
        HCOL_VERBOSE(1,"hcoll ompi_attr_set_c failed");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_coll_hcoll_progress(void)
{
    if (!ompi_mpi_finalized){
        (*hcoll_progress_fn)();
    } else {
        hcoll_rte_p2p_disabled_notify();
    }
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
    ompi_attribute_fn_ptr_union_t del_fn;
    ompi_attribute_fn_ptr_union_t copy_fn;
    mca_coll_hcoll_component_t *cm;
    int err;
    int rc;
    cm = &mca_coll_hcoll_component;
    *priority = 0;
    module = NULL;

    if (!cm->hcoll_enable){
        return NULL;
    }

    if (OMPI_COMM_IS_INTER(comm) || ompi_comm_size(comm) < cm->hcoll_np
        || ompi_comm_size(comm) < 2){
        return NULL;
    }


    if (!cm->libhcoll_initialized)
    {
        /* libhcoll should be initialized here since current implmentation of
           mxm bcol in libhcoll needs world_group fully functional during init
           world_group, i.e. ompi_comm_world, is not ready at hcoll component open
           call */
        opal_progress_register(mca_coll_hcoll_progress);

        hcoll_set_runtime_tag_offset(MCA_COLL_BASE_TAG_HCOLL_BASE, mca_pml.pml_max_tag);

        HCOL_VERBOSE(10,"Calling hcoll_init();");
        rc = hcoll_init();

        if (HCOLL_SUCCESS != rc){
            cm->hcoll_enable = 0;
            opal_progress_unregister(mca_coll_hcoll_progress);
            HCOL_ERROR("Hcol library init failed");
            return NULL;
        }

        if (cm->using_mem_hooks && hcoll_check_mem_release_cb_needed()) {
            opal_mem_hooks_register_release(mca_coll_hcoll_mem_release_cb, NULL);
        } else {
            cm->using_mem_hooks = 0;
        }

        copy_fn.attr_communicator_copy_fn = (MPI_Comm_internal_copy_attr_function*) MPI_COMM_NULL_COPY_FN;
        del_fn.attr_communicator_delete_fn = hcoll_comm_attr_del_fn;
        err = ompi_attr_create_keyval(COMM_ATTR, copy_fn, del_fn, &hcoll_comm_attr_keyval, NULL ,0, NULL);
        if (OMPI_SUCCESS != err) {
            cm->hcoll_enable = 0;
            hcoll_finalize();
            opal_progress_unregister(mca_coll_hcoll_progress);
            HCOL_ERROR("Hcol comm keyval create failed");
            return NULL;
        }

    }
    hcoll_module = OBJ_NEW(mca_coll_hcoll_module_t);
    if (!hcoll_module){
        if (!cm->libhcoll_initialized) {
            cm->hcoll_enable = 0;
            hcoll_finalize();
            opal_progress_unregister(mca_coll_hcoll_progress);
        }
        return NULL;
    }

    hcoll_module->comm = comm;

    HCOL_VERBOSE(10,"Creating hcoll_context for comm %p, comm_id %d, comm_size %d",
                 (void*)comm,comm->c_contextid,ompi_comm_size(comm));

    hcoll_module->hcoll_context =
        hcoll_create_context((rte_grp_handle_t)comm);

    if (NULL == hcoll_module->hcoll_context){
        HCOL_VERBOSE(1,"hcoll_create_context returned NULL");
        OBJ_RELEASE(hcoll_module);
        if (!cm->libhcoll_initialized) {
            cm->hcoll_enable = 0;
            hcoll_finalize();
            opal_progress_unregister(mca_coll_hcoll_progress);
        }
        return NULL;
    }


    hcoll_module->super.coll_module_enable = mca_coll_hcoll_module_enable;
    hcoll_module->super.coll_barrier = hcoll_collectives.coll_barrier ? mca_coll_hcoll_barrier : NULL;
    hcoll_module->super.coll_bcast = hcoll_collectives.coll_bcast ? mca_coll_hcoll_bcast : NULL;
    hcoll_module->super.coll_allgather = hcoll_collectives.coll_allgather ? mca_coll_hcoll_allgather : NULL;
    hcoll_module->super.coll_allreduce = hcoll_collectives.coll_allreduce ? mca_coll_hcoll_allreduce : NULL;
    hcoll_module->super.coll_alltoall = /*hcoll_collectives.coll_alltoall ? mca_coll_hcoll_alltoall : */  NULL;
    hcoll_module->super.coll_gatherv = hcoll_collectives.coll_gatherv ? mca_coll_hcoll_gatherv : NULL;
    hcoll_module->super.coll_ibarrier = hcoll_collectives.coll_ibarrier ? mca_coll_hcoll_ibarrier : NULL;
    hcoll_module->super.coll_ibcast = hcoll_collectives.coll_ibcast ? mca_coll_hcoll_ibcast : NULL;
    hcoll_module->super.coll_iallgather = hcoll_collectives.coll_iallgather ? mca_coll_hcoll_iallgather : NULL;
    hcoll_module->super.coll_iallreduce = hcoll_collectives.coll_iallreduce ? mca_coll_hcoll_iallreduce : NULL;
    hcoll_module->super.coll_gather = /*hcoll_collectives.coll_gather ? mca_coll_hcoll_gather :*/ NULL;
    hcoll_module->super.coll_igatherv = hcoll_collectives.coll_igatherv ? mca_coll_hcoll_igatherv : NULL;

    *priority = cm->hcoll_priority;
    module = &hcoll_module->super;

    if (!cm->libhcoll_initialized) {
        cm->libhcoll_initialized = true;
    }

    return module;
}


OBJ_CLASS_INSTANCE(mca_coll_hcoll_module_t,
        mca_coll_base_module_t,
        mca_coll_hcoll_module_construct,
        mca_coll_hcoll_module_destruct);



