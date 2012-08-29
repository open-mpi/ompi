/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file */

#include "ompi_config.h"

#include "ompi/constants.h"
#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/bcol/bcol.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/coll/ml/coll_ml.h"
#include "ompi/mca/coll/ml/coll_ml_inlines.h"
#include "ompi/mca/coll/ml/coll_ml_allocation.h"
#include "coll_ml_colls.h"
#include <unistd.h>
#include <sys/uio.h>



/* This routine re-orders and packs user data.  The assumption is that
 * there is per-process data, the amount of data is the same for all
 * ranks, and the user data is contigous.
 */
int mca_coll_ml_pack_reorder_contiguous_data(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    int i, rank;
    void *user_buf, *library_buf;
    size_t bytes_per_proc;
    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t *)
        coll_op->coll_module;
    mca_coll_ml_topology_t *topo_info = coll_op->coll_schedule->topo_info;
    ptrdiff_t ptr_dif;

    /* get the offset into each processes data.  The assumption is that
     * we are manipulating the same amount of data for each process.
     */

    /* figure out how much data per-proc to copy */
    bytes_per_proc=coll_op->fragment_data.per_rank_fragment_size;
    
    /* loop over all the ranks in the communicator */
    for( i=0 ; i < ompi_comm_size(ml_module->comm) ; i++ ) {

        /* look up the rank of the i'th element in the sorted list */
        rank = topo_info->sort_list[i];
    
        /* get the pointer to user data */
        user_buf=coll_op->full_message.src_user_addr;
        /* compute offset into the user buffer */

        /* offset for data already processed */
        ptr_dif=rank*coll_op->full_message.n_bytes_per_proc_total+
                            coll_op->fragment_data.offset_into_user_buffer_per_proc;
        user_buf=(void *) ((char *)user_buf+ptr_dif);
                /*
                rank*coll_op->full_message.n_bytes_per_proc_total+
                coll_op->fragment_data.offset_into_user_buffer_per_proc);
                */

        /* get the pointer to the ML buffer */
        library_buf= (void *)
            ((char *)coll_op->variable_fn_params.src_desc->data_addr+i*bytes_per_proc);

        /* copy the data */
        memcpy(library_buf, user_buf, bytes_per_proc);

    }

    return OMPI_SUCCESS;
}

/* This routine re-orders and packs user data.  The assumption is that
 * there is per-process data, the amount of data is the same for all
 * ranks, and the user data is contigous.
 */
int mca_coll_ml_pack_reorder_noncontiguous_data(mca_coll_ml_collective_operation_progress_t *coll_op)
{
    int i, rank;
    void *user_buf, *library_buf;
    size_t bytes_per_proc;
    ptrdiff_t ptr_dif;
    mca_coll_ml_module_t *ml_module = (mca_coll_ml_module_t *)
        coll_op->coll_module;
    mca_coll_ml_topology_t *topo_info = coll_op->coll_schedule->topo_info;

    /* get the offset into each processes data.  The assumption is that
     * we are manipulating the same amount of data for each process.
     */

    /* figure out how much data per-proc to copy */
    bytes_per_proc = coll_op->fragment_data.per_rank_fragment_size;
    
    /* loop over all the ranks in the communicator */
    for(i = 0; i < ompi_comm_size(ml_module->comm); i++ ) {

        /* look up the rank of the i'th element in the sorted list */
        rank = topo_info->sort_list[i];
    
        /* get the pointer to user data */
        user_buf=coll_op->full_message.src_user_addr;
        /* compute offset into the user buffer */

        /* offset for data already processed */
        ptr_dif=rank*coll_op->full_message.send_count*
                coll_op->full_message.send_extent+
                coll_op->fragment_data.offset_into_user_buffer_per_proc;
        user_buf=(void *) ((char *)user_buf+ptr_dif);

        /* get the pointer to the ML buffer */
        library_buf= (void *)
            ((char *)coll_op->variable_fn_params.src_desc->data_addr+i*bytes_per_proc);

        /* copy the data */
        memcpy(library_buf, user_buf, bytes_per_proc);

    }

    return OMPI_SUCCESS;
}

