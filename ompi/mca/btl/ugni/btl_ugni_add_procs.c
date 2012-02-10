/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
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
#include "btl_ugni_smsg.h"

static int
mca_btl_ugni_setup_mpools (mca_btl_ugni_module_t *ugni_module);

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

        rc = mca_btl_ugni_setup_mpools (ugni_module);
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

static int ugni_reg_mem (void *reg_data, void *base, size_t size,
                         mca_mpool_base_registration_t *reg)
{
    mca_btl_ugni_module_t *btl = (mca_btl_ugni_module_t *) reg_data;
    mca_btl_ugni_reg_t *ugni_reg = (mca_btl_ugni_reg_t *) reg;
    int rc;

    rc = GNI_MemRegister (btl->device->dev_handle, (uint64_t) base,
                          size, NULL, GNI_MEM_READWRITE |
                          GNI_MEM_RELAXED_PI_ORDERING, -1,
                          &(ugni_reg->memory_hdl));

    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

static int ugni_reg_smsg_mem (void *reg_data, void *base, size_t size,
                              mca_mpool_base_registration_t *reg)
{
    mca_btl_ugni_module_t *btl = (mca_btl_ugni_module_t *) reg_data;
    mca_btl_ugni_reg_t *ugni_reg = (mca_btl_ugni_reg_t *) reg;
    int rc;

    rc = GNI_MemRegister (btl->device->dev_handle, (uint64_t)base,
                          size, btl->smsg_remote_cq, GNI_MEM_READWRITE,
                          -1, &(ugni_reg->memory_hdl));

    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

static int
ugni_dereg_mem (void *reg_data, mca_mpool_base_registration_t *reg)
{
    mca_btl_ugni_module_t *btl = (mca_btl_ugni_module_t *) reg_data;
    mca_btl_ugni_reg_t *ugni_reg = (mca_btl_ugni_reg_t *)reg;
    int rc;

    rc = GNI_MemDeregister (btl->device->dev_handle, &ugni_reg->memory_hdl);
    if (GNI_RC_SUCCESS != rc) {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

static int
mca_btl_ugni_setup_mpools (mca_btl_ugni_module_t *ugni_module)
{
    struct mca_mpool_base_resources_t mpool_resources;
    int mbox_increment, rc;
    size_t nprocs;

    (void) ompi_proc_world (&nprocs);

    rc = ompi_free_list_init_new (&mca_btl_ugni_component.ugni_frags_smsg,
                                  sizeof (mca_btl_ugni_smsg_frag_t),
                                  opal_cache_line_size, OBJ_CLASS(mca_btl_ugni_smsg_frag_t),
                                  mca_btl_ugni_component.ugni_smsg_limit,
                                  opal_cache_line_size,
                                  mca_btl_ugni_component.ugni_free_list_num,
                                  mca_btl_ugni_component.ugni_free_list_max,
                                  mca_btl_ugni_component.ugni_free_list_inc,
                                  NULL);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = ompi_free_list_init_new (&mca_btl_ugni_component.ugni_frags_rdma,
                                  sizeof (mca_btl_ugni_rdma_frag_t), 8,
                                  OBJ_CLASS(mca_btl_ugni_rdma_frag_t),
                                  0, opal_cache_line_size,
                                  mca_btl_ugni_component.ugni_free_list_num,
                                  mca_btl_ugni_component.ugni_free_list_max,
                                  mca_btl_ugni_component.ugni_free_list_inc,
                                  NULL);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    mpool_resources.reg_data       = (void *) ugni_module;
    mpool_resources.sizeof_reg     = sizeof (mca_btl_ugni_reg_t);
    mpool_resources.register_mem   = ugni_reg_mem;
    mpool_resources.deregister_mem = ugni_dereg_mem;
    ugni_module->super.btl_mpool =
        mca_mpool_base_module_create("rdma", ugni_module->device,
                                     &mpool_resources);
    if (NULL == ugni_module->super.btl_mpool) {
        BTL_ERROR(("error creating mpool"));
        return OMPI_ERROR;
    }

    mpool_resources.register_mem   = ugni_reg_smsg_mem;

    ugni_module->smsg_mpool =
        mca_mpool_base_module_create("rdma", ugni_module->device,
                                     &mpool_resources);

    OBJ_CONSTRUCT(&ugni_module->eager_frags_send, ompi_free_list_t);

    rc = ompi_free_list_init_new (&ugni_module->eager_frags_send,
                                  sizeof (mca_btl_ugni_eager_frag_t), 8,
                                  OBJ_CLASS(mca_btl_ugni_eager_frag_t),
                                  ugni_module->super.btl_eager_limit, 64,
                                  mca_btl_ugni_component.ugni_eager_num,
                                  mca_btl_ugni_component.ugni_eager_max,
                                  mca_btl_ugni_component.ugni_eager_inc,
                                  ugni_module->super.btl_mpool);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    OBJ_CONSTRUCT(&ugni_module->eager_frags_recv, ompi_free_list_t);

    rc = ompi_free_list_init_new (&ugni_module->eager_frags_recv,
                                  sizeof (mca_btl_ugni_eager_frag_t), 8,
                                  OBJ_CLASS(mca_btl_ugni_eager_frag_t),
                                  ugni_module->super.btl_eager_limit, 64,
                                  mca_btl_ugni_component.ugni_eager_num,
                                  mca_btl_ugni_component.ugni_eager_max,
                                  mca_btl_ugni_component.ugni_eager_inc,
                                  ugni_module->super.btl_mpool);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    OBJ_CONSTRUCT(&ugni_module->smsg_mboxes, ompi_free_list_t);

    mbox_increment = nprocs;

    /* limit mailbox allocations to at most 2MiB at a time */
    if (nprocs * mca_btl_ugni_component.smsg_mbox_size > 2097152) {
        mbox_increment = (int) (2097152.0 / (float)mca_btl_ugni_component.smsg_mbox_size);
    }

    rc = ompi_free_list_init_new (&ugni_module->smsg_mboxes,
                                  sizeof (mca_btl_ugni_smsg_mbox_t), 8,
                                  OBJ_CLASS(mca_btl_ugni_smsg_mbox_t),
                                  mca_btl_ugni_component.smsg_mbox_size, 64,
                                  0, nprocs, mbox_increment,
                                  ugni_module->smsg_mpool);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    return OMPI_SUCCESS;
}

