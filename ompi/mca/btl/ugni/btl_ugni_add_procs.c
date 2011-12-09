/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"

#include "ompi_config.h"

#include "btl_ugni.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_endpoint.h"

int mca_btl_ugni_add_procs(struct mca_btl_base_module_t* btl,
                           size_t nprocs,
                           struct ompi_proc_t **procs,
                           struct mca_btl_base_endpoint_t **peers,
                           opal_bitmap_t *reachable) {
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    size_t ntotal_procs;
    size_t i;
    int rc;


    if (NULL == ugni_module->endpoints) {
        (void) ompi_proc_world (&ntotal_procs);

        ugni_module->endpoints = calloc (ntotal_procs, sizeof (mca_btl_base_endpoint_t *));

        if (OPAL_UNLIKELY(NULL == ugni_module->endpoints)) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        rc = ompi_free_list_init_new (&mca_btl_ugni_component.ugni_frags_eager,
                                      sizeof (mca_btl_ugni_base_frag_t),
                                      opal_cache_line_size, OBJ_CLASS(mca_btl_ugni_base_frag_t),
                                      sizeof (mca_btl_ugni_frag_hdr_t) + mca_btl_ugni_component.eager_limit,
                                      opal_cache_line_size,
                                      mca_btl_ugni_component.ugni_free_list_num,
                                      mca_btl_ugni_component.ugni_free_list_max,
                                      mca_btl_ugni_component.ugni_free_list_inc,
                                      NULL);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            return rc;
        }

        rc = ompi_free_list_init_new (&mca_btl_ugni_component.ugni_frags_rdma,
                                      sizeof (mca_btl_ugni_rdma_frag_t),
                                      opal_cache_line_size, OBJ_CLASS(mca_btl_ugni_rdma_frag_t),
                                      0, opal_cache_line_size,
                                      mca_btl_ugni_component.ugni_free_list_num,
                                      mca_btl_ugni_component.ugni_free_list_max,
                                      mca_btl_ugni_component.ugni_free_list_inc,
                                      NULL);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            return rc;
        }
    }

    for (i = 0 ; i < nprocs ; ++i) {
        struct ompi_proc_t *ompi_proc = procs[i];
        uint32_t rem_rank = ompi_proc->proc_name.vpid;

        if (OPAL_PROC_ON_LOCAL_NODE(ompi_proc->proc_flags)) {
            /* ignore local procs */
            peers[i] = NULL;
            continue;
        }

        /*  Create and Init endpoints */
        rc = mca_btl_ugni_init_ep (peers + i, (mca_btl_ugni_module_t *) btl, ompi_proc);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            BTL_ERROR(("btl/ugni error initializing endpoint"));
            return rc;
        }

        /* Set the reachable bit */
        rc = opal_bitmap_set_bit (reachable, i);

        /* Store a reference to this peer */
        ugni_module->endpoints[rem_rank] = peers[i];
    }

    ugni_module->endpoint_count += nprocs;

    return OMPI_SUCCESS;
}

int mca_btl_ugni_del_procs (struct mca_btl_base_module_t *btl,
                            size_t nprocs, struct ompi_proc_t **procs,
                            struct mca_btl_base_endpoint_t **peers) {
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    size_t i;

    /* NTH: this function destroys the endpoint list which will cause bad
       things to happen if the caller only wants to delete a few procs. */

    for (i = 0 ; i < nprocs ; ++i) {
        struct ompi_proc_t *ompi_proc = procs[i];
        uint32_t rem_rank = ompi_proc->proc_name.vpid;

        if (ugni_module->endpoints[rem_rank]) {
            mca_btl_ugni_release_ep (ugni_module->endpoints[rem_rank]);
        }

        ugni_module->endpoints[rem_rank] = NULL;
    }

    ugni_module->endpoint_count -= nprocs;

    if (0 == ugni_module->endpoint_count) {
        free (ugni_module->endpoints);
        ugni_module->endpoints = NULL;
    }

    return OMPI_SUCCESS;
}
