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

#include "ompi/include/ompi/constants.h"
#include "ompi/mca/coll/ml/coll_ml.h"
#include "ompi/mca/coll/ml/coll_ml_inlines.h"
#include "ompi/mca/coll/ml/coll_ml_allocation.h"

/* collective managment descriptor initialization - called right after
 * the constructor by ompi_free_list code
 */
static void mca_coll_ml_collective_operation_progress_init
                             (ompi_free_list_item_t* item, void* ctx)
{
    int i;
    int max_dag_size = ((struct coll_desc_init *)ctx)->max_dag_size;
    size_t max_n_bytes_per_proc_total = 
        ((struct coll_desc_init *)ctx)->max_n_bytes_per_proc_total;
    mca_coll_ml_collective_operation_progress_t *coll_op =
        (mca_coll_ml_collective_operation_progress_t *) item;

    coll_op->dag_description.status_array =
        (mca_coll_ml_task_status_t *)
        calloc(max_dag_size, sizeof(mca_coll_ml_task_status_t));
    assert(coll_op->dag_description.status_array);

    /* initialize individual elements */
    for (i = 0; i < max_dag_size; i++ ) {
        /* Pasha: We assume here index syncronization between
           task indexes and indexes in component_function array
           (mca_coll_ml_collective_operation_description) 
         */
        coll_op->dag_description.status_array[i].
            my_index_in_coll_schedule = i;
        coll_op->dag_description.status_array[i].
            ml_coll_operation = coll_op;

        OBJ_CONSTRUCT(&coll_op->dag_description.status_array[i].item, opal_list_item_t);
    }

    /* set the size per proc of the ML buffer */
    coll_op->full_message.max_n_bytes_per_proc_total=
        max_n_bytes_per_proc_total;

    /* set the pointer to the bcol module */
    coll_op->coll_module =
        ((struct coll_desc_init *)ctx)->bcol_base_module;

}

int ml_coll_schedule_setup(mca_coll_ml_module_t *ml_module)
{
    /* local variables */
    int ret = OMPI_SUCCESS, comm_size;
    mca_coll_ml_component_t *cm = &mca_coll_ml_component;
    size_t ml_per_proc_buffer_size;

    /* Barrier */
    ret = ml_coll_hier_barrier_setup(ml_module);
    if( OMPI_SUCCESS != ret ) {
        return ret;
    }

    /* Broadcast */
    ret = ml_coll_hier_bcast_setup(ml_module);
    if( OMPI_SUCCESS != ret ) {
        return ret;
    }

    /* Allreduce */
    if (!mca_coll_ml_component.use_knomial_allreduce) { 
		ret = ml_coll_hier_allreduce_setup(ml_module); 
	} else {
		ret = ml_coll_hier_allreduce_setup_new(ml_module);
	}

	if( OMPI_SUCCESS != ret ) {
        return ret;
    }


    /* Alltoall */
    /*
    ret = ml_coll_hier_alltoall_setup_new(ml_module);
    
    if( OMPI_SUCCESS != ret ) {
        return ret;
    }
    */

    /* Allgather */
    ret = ml_coll_hier_allgather_setup(ml_module);
    
    if( OMPI_SUCCESS != ret ) {
        return ret;
    }
    
    /* Gather */
    /*
    ret = ml_coll_hier_gather_setup(ml_module);
    
    if( OMPI_SUCCESS != ret ) {
        return ret;
    }
    */

    /* Reduce */
    ret = ml_coll_hier_reduce_setup(ml_module);
    if( OMPI_SUCCESS != ret ) {
        return ret;
    }

    /* Scatter */
    /*
    ret = ml_coll_hier_scatter_setup(ml_module);
    if( OMPI_SUCCESS != ret ) {
        return ret;
    }
    */

    ret = ml_coll_memsync_setup(ml_module);
    if( OMPI_SUCCESS != ret ) {
        return ret;
    }

    /* nonblocking Reduce */

    /* Alltoall */

    /* nonblocking alltoall */

    /* max_dag_size will be set here, so initialize it */

    /* Pasha: Do we have to keep the max_dag_size ? 
       In most generic case, it will be equal to max_fn_calls */
    ml_module->max_dag_size = ml_module->max_fn_calls;

    assert(ml_module->max_dag_size > 0);

    /* initialize the mca_coll_ml_collective_operation_progress_t free list */
    /* NOTE: as part of initialization each routine needs to make sure that
     * the module element max_dag_size is set large enough - space for
     * tracking collective progress is allocated based on this value. */

    /* figure out what the size of the ml buffer is */
    ml_per_proc_buffer_size=ml_module->payload_block->size_buffer;
    comm_size=ompi_comm_size(ml_module->comm);
    ml_per_proc_buffer_size/=comm_size;
    ml_module->coll_desc_init_data.max_dag_size=ml_module->max_dag_size;
    ml_module->coll_desc_init_data.max_n_bytes_per_proc_total=ml_per_proc_buffer_size;
    ml_module->coll_desc_init_data.bcol_base_module=(mca_coll_base_module_t *)
        ml_module;

    ret = ompi_free_list_init_ex_new(
            &(ml_module->coll_ml_collective_descriptors),
            sizeof(mca_coll_ml_collective_operation_progress_t),
            /* no special alignment needed */
            8,
            OBJ_CLASS(mca_coll_ml_collective_operation_progress_t),
            /* no payload data */
            0, 0, 
            /* NOTE: hack - need to parametrize this */
            cm->free_list_init_size, 
            cm->free_list_max_size, 
            cm->free_list_grow_size,
            /* No Mpool */
            NULL,
            mca_coll_ml_collective_operation_progress_init,
            (void *)&(ml_module->coll_desc_init_data)
            );
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* done */
    return ret;
}
