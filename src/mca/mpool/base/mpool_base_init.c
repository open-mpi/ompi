/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "runtime/runtime.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/mpool/mpool.h"
#include "mca/mpool/base/base.h"
#include "class/ompi_rb_tree.h"
#include "class/ompi_free_list.h"

OBJ_CLASS_INSTANCE(mca_mpool_base_selected_module_t, ompi_list_item_t, NULL, NULL);
static bool mca_mpool_enable_progress_threads = true;
static bool mca_mpool_enable_mpi_threads = true;
         
OBJ_CLASS_INSTANCE(mca_mpool_base_chunk_t, ompi_list_item_t, NULL, NULL);

/**
 * Function for weeding out mpool modules that don't want to run.
 *
 * Call the init function on all available components to find out if they
 * want to run.  Select all components that don't fail.  Failing modules
 * will be closed and unloaded.  The selected modules will be returned
 * to the caller in a ompi_list_t.
 */
int mca_mpool_base_init(bool enable_progress_threads, bool enable_mpi_threads)
{
    mca_mpool_enable_progress_threads = enable_progress_threads;
    mca_mpool_enable_mpi_threads = enable_mpi_threads;
    OBJ_CONSTRUCT(&mca_mpool_base_mem_list, ompi_free_list_t);
    ompi_free_list_init(&mca_mpool_base_mem_list, sizeof(mca_mpool_base_chunk_t),
                        OBJ_CLASS(mca_mpool_base_chunk_t), 0, -1 , 128, NULL);
    OBJ_CONSTRUCT(&mca_mpool_base_tree, ompi_rb_tree_t);
    return ompi_rb_tree_init(&mca_mpool_base_tree, mca_mpool_base_tree_node_compare);
}

