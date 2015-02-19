/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "btl_ugni.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_smsg.h"

#include "opal/include/opal/align.h"
#include "opal/mca/dstore/dstore.h"

#define INITIAL_GNI_EPS 10000

static int
mca_btl_ugni_setup_mpools (mca_btl_ugni_module_t *ugni_module);
static void
mca_btl_ugni_module_set_max_reg (mca_btl_ugni_module_t *ugni_module, int nlocal_procs);
static int mca_btl_ugni_smsg_setup (int nprocs);

int mca_btl_ugni_add_procs(struct mca_btl_base_module_t* btl,
                           size_t nprocs,
                           struct opal_proc_t **procs,
                           struct mca_btl_base_endpoint_t **peers,
                           opal_bitmap_t *reachable) {
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    size_t i;
    int rc;
    void *mmap_start_addr;

    if (false == ugni_module->initialized) {

        /* TODO: need to think of something more elegant than this max array */

        rc = opal_pointer_array_init (&ugni_module->endpoints, INITIAL_GNI_EPS, 1 << 24, 512);
        if (OPAL_SUCCESS != rc) {
            BTL_ERROR(("error inializing the endpoint array. rc = %d", rc));
            return rc;
        }


        /* NTH: might want to vary this size based off the universe size (if
         * one exists). the table is only used for connection lookup and
         * endpoint removal. */
        rc = opal_hash_table_init (&ugni_module->id_to_endpoint, 512);
        if (OPAL_SUCCESS != rc) {
            BTL_ERROR(("error initializing the endpoint hash. rc = %d", rc));
            return rc;
        }
    }

    for (i = 0 ; i < nprocs ; ++i) {
        struct opal_proc_t *opal_proc = procs[i];
        uint64_t proc_id = mca_btl_ugni_proc_name_to_id(opal_proc->proc_name);

        if (OPAL_PROC_ON_LOCAL_NODE(opal_proc->proc_flags)) {
            ugni_module->nlocal_procs++;

            /* ugni is allowed on local processes to provide support for network
             * atomic operations */
        }

        /*  Create and Init endpoints */
        rc = mca_btl_ugni_init_ep (ugni_module, peers + i, (mca_btl_ugni_module_t *) btl, opal_proc);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_ERROR(("btl/ugni error initializing endpoint"));
            return rc;
        }

        /* go ahead and connect the local endpoint for RDMA/CQ write */
        if (opal_proc == opal_proc_local_get ()) {
            ugni_module->local_ep = peers[i];
        }

        /* Add this endpoint to the pointer array. */
        BTL_VERBOSE(("initialized uGNI endpoint for proc id: 0x%" PRIx64 " ptr: %p", proc_id, (void *) peers[i]));
        opal_hash_table_set_value_uint64 (&ugni_module->id_to_endpoint, proc_id, peers[i]);

        /* Set the reachable bit */
        rc = opal_bitmap_set_bit (reachable, i);
        ++ugni_module->endpoint_count;
    }

    mca_btl_ugni_module_set_max_reg (ugni_module, ugni_module->nlocal_procs);

    if (false == ugni_module->initialized) {
        OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);
        rc = GNI_CqCreate (ugni_module->device->dev_handle, mca_btl_ugni_component.local_cq_size,
                           0, GNI_CQ_NOBLOCK, NULL, NULL, &ugni_module->rdma_local_cq);
        OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);
        if (GNI_RC_SUCCESS != rc) {
            BTL_ERROR(("error creating local BTE/FMA CQ"));
            return opal_common_rc_ugni_to_opal (rc);
        }

        OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);
        rc = GNI_CqCreate (ugni_module->device->dev_handle, mca_btl_ugni_component.local_cq_size,
                           0, GNI_CQ_NOBLOCK, NULL, NULL, &ugni_module->smsg_local_cq);
        OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);
        if (GNI_RC_SUCCESS != rc) {
            BTL_ERROR(("error creating local SMSG CQ"));
            return opal_common_rc_ugni_to_opal (rc);
        }

        OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);
        rc = GNI_CqCreate (ugni_module->device->dev_handle, mca_btl_ugni_component.remote_cq_size,
                           0, GNI_CQ_NOBLOCK, NULL, NULL, &ugni_module->smsg_remote_cq);
        OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);
        if (GNI_RC_SUCCESS != rc) {
            BTL_ERROR(("error creating remote SMSG CQ"));
            return opal_common_rc_ugni_to_opal (rc);
        }

        if (mca_btl_ugni_component.progress_thread_enabled) {
            OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);
            rc = GNI_CqCreate (ugni_module->device->dev_handle, mca_btl_ugni_component.local_cq_size,
                               0, GNI_CQ_BLOCKING, NULL, NULL, &ugni_module->rdma_local_irq_cq);
            OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);
            if (GNI_RC_SUCCESS != rc) {
                BTL_ERROR(("error creating local BTE/FMA CQ"));
                return opal_common_rc_ugni_to_opal (rc);
            }

            OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);
            rc = GNI_CqCreate (ugni_module->device->dev_handle, mca_btl_ugni_component.remote_cq_size,
                               0, GNI_CQ_BLOCKING, NULL, NULL, &ugni_module->smsg_remote_irq_cq);
            OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);
            if (GNI_RC_SUCCESS != rc) {
                BTL_ERROR(("error creating remote SMSG CQ"));
                return opal_common_rc_ugni_to_opal (rc);
            }
        }

        rc = mca_btl_ugni_setup_mpools (ugni_module);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_ERROR(("btl/ugni error setting up mpools/free lists"));
            return rc;
        }

        rc = mca_btl_ugni_smsg_init (ugni_module);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_ERROR(("btl/ugni error initializing SMSG"));
            return rc;
        }

        /*
         * If progress thread enabled, registered a page of memory
         * with the smsg_remote_irq_cq.  This memory handle is passed
         * to ranks which want to communicate with this rank. A rank which
         * posts a GNI_PostCqWrite targeting this memory handle generates
         * an IRQ at the target node, which ultimately causes the progress
         * thread in the target rank to become schedulable.
         */
        if (mca_btl_ugni_component.progress_thread_enabled) {
            mmap_start_addr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
            if (NULL == mmap_start_addr) {
                BTL_ERROR(("btl/ugni mmap returned error"));
                return OPAL_ERR_OUT_OF_RESOURCE;
            }

            OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);
            rc = GNI_MemRegister(ugni_module->device->dev_handle,
                                     (unsigned long)mmap_start_addr,
                                     4096,
                                     ugni_module->smsg_remote_irq_cq,
                                     GNI_MEM_READWRITE,
                                     -1,
                                     &ugni_module->device->smsg_irq_mhndl);
            OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);

            mca_btl_ugni_spawn_progress_thread(btl);
        }

        ugni_module->initialized = true;
    }

    return OPAL_SUCCESS;
}

int mca_btl_ugni_del_procs (struct mca_btl_base_module_t *btl,
                            size_t nprocs, struct opal_proc_t **procs,
                            struct mca_btl_base_endpoint_t **peers) {
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    size_t i;
    int rc;

    while (ugni_module->active_send_count) {
        /* ensure all sends are complete before removing and procs */
        rc = mca_btl_ugni_progress_local_smsg (ugni_module);
        if (OPAL_SUCCESS != rc) {
            break;
        }
    }

    for (i = 0 ; i < nprocs ; ++i) {
        struct opal_proc_t *opal_proc = procs[i];
        uint64_t proc_id = mca_btl_ugni_proc_name_to_id(opal_proc->proc_name);
        mca_btl_base_endpoint_t *ep = NULL;

        /* lookup this proc in the hash table */
        (void) opal_hash_table_get_value_uint64 (&ugni_module->id_to_endpoint, proc_id, (void **) &ep);

        BTL_VERBOSE(("deleting endpoint with proc id 0x%" PRIx64 ", ptr: %p", proc_id, (void *) ep));

        if (NULL != ep) {
            mca_btl_ugni_release_ep (ep);
            --ugni_module->endpoint_count;
        }

        /* remote the endpoint from the hash table */
        opal_hash_table_set_value_uint64 (&ugni_module->id_to_endpoint, proc_id, NULL);
    }

    return OPAL_SUCCESS;
}

static int ugni_reg_rdma_mem (void *reg_data, void *base, size_t size,
                              mca_mpool_base_registration_t *reg)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) reg_data;
    mca_btl_ugni_reg_t *ugni_reg = (mca_btl_ugni_reg_t *) reg;
    gni_return_t rc;

    if (ugni_module->reg_count >= ugni_module->reg_max) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
 
    OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);
    rc = GNI_MemRegister (ugni_module->device->dev_handle, (uint64_t) base,
                          size, NULL, GNI_MEM_READWRITE | GNI_MEM_RELAXED_PI_ORDERING,
                          -1, &(ugni_reg->handle.gni_handle));
    OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);

    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    opal_atomic_add_32(&ugni_module->reg_count,1);

    return OPAL_SUCCESS;
}


static int ugni_reg_smsg_mem (void *reg_data, void *base, size_t size,
                              mca_mpool_base_registration_t *reg)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) reg_data;
    mca_btl_ugni_reg_t *ugni_reg = (mca_btl_ugni_reg_t *) reg;
    gni_return_t rc;

    OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);
    rc = GNI_MemRegister (ugni_module->device->dev_handle, (uint64_t) base,
                          size, ugni_module->smsg_remote_cq, GNI_MEM_READWRITE, -1,
                          &(ugni_reg->handle.gni_handle));
    OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);
    return opal_common_rc_ugni_to_opal (rc);
}

static int
ugni_dereg_mem (void *reg_data, mca_mpool_base_registration_t *reg)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) reg_data;
    mca_btl_ugni_reg_t *ugni_reg = (mca_btl_ugni_reg_t *)reg;
    gni_return_t rc;

    OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);
    rc = GNI_MemDeregister (ugni_module->device->dev_handle, &ugni_reg->handle.gni_handle);
    OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);
    if (GNI_RC_SUCCESS != rc) {
        return OPAL_ERROR;
    }

    opal_atomic_add_32(&ugni_module->reg_count,-1);

    return OPAL_SUCCESS;
}

static int
mca_btl_ugni_setup_mpools (mca_btl_ugni_module_t *ugni_module)
{
    struct mca_mpool_base_resources_t mpool_resources;
    unsigned int mbox_increment, nprocs;
    const char *mpool_name;
    int rc;
    opal_list_t vals;
    opal_value_t *kv;

    rc = opal_pointer_array_init (&ugni_module->pending_smsg_frags_bb, 0,
                                  1 << 30, 32768);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* determine how many procs are in the job (might want to check universe size here) */
    OBJ_CONSTRUCT(&vals, opal_list_t);
    if (OPAL_SUCCESS == opal_dstore.fetch(opal_dstore_internal, &OPAL_PROC_MY_NAME,
                                          OPAL_DSTORE_UNIV_SIZE, &vals)) {
        /* the number of procs in the job is in the uint32 field */
        kv = (opal_value_t*)opal_list_get_first(&vals);
        nprocs = kv->data.uint32;
    } else {
        nprocs = 512;
    }
    OPAL_LIST_DESTRUCT(&vals);

    rc = mca_btl_ugni_smsg_setup (nprocs);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error setting up smsg"));
        return rc;
    }

    rc = opal_free_list_init (&ugni_module->smsg_frags,
                              sizeof (mca_btl_ugni_smsg_frag_t),
                              opal_cache_line_size, OBJ_CLASS(mca_btl_ugni_smsg_frag_t),
                              mca_btl_ugni_component.ugni_smsg_limit,
                              opal_cache_line_size,
                              mca_btl_ugni_component.ugni_free_list_num,
                              mca_btl_ugni_component.ugni_free_list_max,
                              mca_btl_ugni_component.ugni_free_list_inc,
                              NULL, 0, NULL, (opal_free_list_item_init_fn_t) mca_btl_ugni_frag_init,
                              (void *) ugni_module);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error creating smsg fragment free list"));
        return rc;
    }

    rc = opal_free_list_init (&ugni_module->rdma_frags,
                              sizeof (mca_btl_ugni_rdma_frag_t), 64,
                              OBJ_CLASS(mca_btl_ugni_rdma_frag_t),
                              0, opal_cache_line_size,
                              mca_btl_ugni_component.ugni_free_list_num,
                              mca_btl_ugni_component.ugni_free_list_max,
                              mca_btl_ugni_component.ugni_free_list_inc,
                              NULL, 0, NULL, (opal_free_list_item_init_fn_t) mca_btl_ugni_frag_init,
                              (void *) ugni_module);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    rc = opal_free_list_init (&ugni_module->rdma_int_frags,
                              sizeof (mca_btl_ugni_rdma_frag_t), 8,
                              OBJ_CLASS(mca_btl_ugni_rdma_frag_t),
                              0, opal_cache_line_size, 0, -1, 64,
                              NULL, 0, NULL, (opal_free_list_item_init_fn_t) mca_btl_ugni_frag_init,
                              (void *) ugni_module);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    mpool_resources.pool_name      = "ompi.ugni";
    mpool_resources.reg_data       = (void *) ugni_module;
    mpool_resources.sizeof_reg     = sizeof (mca_btl_ugni_reg_t);
    mpool_resources.register_mem   = ugni_reg_rdma_mem;
    mpool_resources.deregister_mem = ugni_dereg_mem;

    if (MCA_BTL_UGNI_MPOOL_UDREG == mca_btl_ugni_component.mpool_type) {
        /* additional settings for the udreg mpool */
        /* 4k should be large enough for any Gemini/Ares system */
        mpool_resources.max_entries       = 4096;
        mpool_resources.use_kernel_cache  = true;

        /* request a specific page size. this request may not be honored if the
         * page size does not exist. */
        mpool_resources.page_size         = mca_btl_ugni_component.smsg_page_size;

        mpool_resources.use_evict_w_unreg = false;
        mpool_name = "udreg";
    } else {
        mpool_name = "grdma";
    }

    ugni_module->super.btl_mpool =
        mca_mpool_base_module_create(mpool_name, ugni_module->device, &mpool_resources);

    mpool_resources.register_mem   = ugni_reg_smsg_mem;

    ugni_module->smsg_mpool =
        mca_mpool_base_module_create(mpool_name, ugni_module->device, &mpool_resources);

    if (NULL == ugni_module->super.btl_mpool) {
        BTL_ERROR(("error creating rdma mpool"));
        return OPAL_ERROR;
    }

    if (NULL == ugni_module->smsg_mpool) {
        BTL_ERROR(("error creating smsg mpool"));
        return OPAL_ERROR;
    }

    rc = opal_free_list_init (&ugni_module->eager_frags_send,
                              sizeof (mca_btl_ugni_eager_frag_t), 8,
                              OBJ_CLASS(mca_btl_ugni_eager_frag_t),
                              ugni_module->super.btl_eager_limit, 64,
                              mca_btl_ugni_component.ugni_eager_num,
                              mca_btl_ugni_component.ugni_eager_max,
                              mca_btl_ugni_component.ugni_eager_inc,
                              ugni_module->super.btl_mpool, 0, NULL,
                              (opal_free_list_item_init_fn_t) mca_btl_ugni_frag_init,
                              (void *) ugni_module);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error creating eager send fragment free list"));
        return rc;
    }

    rc = opal_free_list_init (&ugni_module->eager_frags_recv,
                              sizeof (mca_btl_ugni_eager_frag_t), 8,
                              OBJ_CLASS(mca_btl_ugni_eager_frag_t),
                              ugni_module->super.btl_eager_limit, 64,
                              mca_btl_ugni_component.ugni_eager_num,
                              mca_btl_ugni_component.ugni_eager_max,
                              mca_btl_ugni_component.ugni_eager_inc,
                              ugni_module->super.btl_mpool, 0, NULL,
                              (opal_free_list_item_init_fn_t) mca_btl_ugni_frag_init,
                              (void *) ugni_module);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error creating eager receive fragment free list"));
        return rc;
    }

    if (0 == mca_btl_ugni_component.mbox_increment) {
        /* limit mailbox allocations to either 12.5% of available registrations
           or 2MiB per allocation */
        mbox_increment = (unsigned int) (2097152.0 / (float)mca_btl_ugni_component.smsg_mbox_size);

        /* we may end up using more */
        if (nprocs/mbox_increment > (unsigned int) ugni_module->reg_max / 8) {
            mbox_increment = nprocs / (ugni_module->reg_max >> 3);
        }
    } else {
        mbox_increment = mca_btl_ugni_component.mbox_increment;
    }

    rc = opal_free_list_init (&ugni_module->smsg_mboxes,
                              sizeof (mca_btl_ugni_smsg_mbox_t), 8,
                              OBJ_CLASS(mca_btl_ugni_smsg_mbox_t),
                              mca_btl_ugni_component.smsg_mbox_size, 128,
                              32, -1, mbox_increment, ugni_module->smsg_mpool,
                              0, NULL, NULL, NULL);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error creating smsg mailbox free list"));
        return rc;
    }

    rc = opal_free_list_init (&ugni_module->post_descriptors,
                              sizeof (mca_btl_ugni_post_descriptor_t),
                              8, OBJ_CLASS(mca_btl_ugni_post_descriptor_t),
                              0, 0, 0, -1, 256, NULL, 0, NULL, NULL, NULL);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error creating post descriptor free list"));
        return rc;
    }

    return OPAL_SUCCESS;
}

static void
mca_btl_ugni_module_set_max_reg (mca_btl_ugni_module_t *ugni_module, int nlocal_procs)
{
    if (0 == mca_btl_ugni_component.max_mem_reg) {
#if defined(HAVE_GNI_GETJOBRESINFO)
        gni_job_res_desc_t res_des;
        gni_return_t grc;
        int fuzz = 20;

        grc = GNI_GetJobResInfo (ugni_module->device->dev_id, opal_common_ugni_module.ptag,
                                 GNI_JOB_RES_MDD, &res_des);
        if (GNI_RC_SUCCESS == grc) {
            ugni_module->reg_max = (res_des.limit - fuzz) / nlocal_procs;
        }
#else
        /* no way to determine the maximum registration count */
        ugni_module->reg_max = 1200 / nlocal_procs;
#endif
    } else if (-1 == mca_btl_ugni_component.max_mem_reg) {
        ugni_module->reg_max = INT_MAX;
    } else {
        ugni_module->reg_max = mca_btl_ugni_component.max_mem_reg;
    }

    ugni_module->reg_count = 0;
}

static int mca_btl_ugni_smsg_setup (int nprocs)
{
    gni_smsg_attr_t tmp_smsg_attrib;
    unsigned int mbox_size;
    gni_return_t grc;

    if (0 == mca_btl_ugni_component.ugni_smsg_limit) {
        /* auto-set the smsg limit based on the number of ranks */
        if (nprocs <= 512) {
            mca_btl_ugni_component.ugni_smsg_limit = 8192;
        } else if (nprocs <= 1024) {
            mca_btl_ugni_component.ugni_smsg_limit = 2048;
        } else if (nprocs <= 8192) {
            mca_btl_ugni_component.ugni_smsg_limit = 1024;
        } else if (nprocs <= 16384) {
            mca_btl_ugni_component.ugni_smsg_limit = 512;
        } else {
            mca_btl_ugni_component.ugni_smsg_limit = 256;
        }
    }

    mca_btl_ugni_component.smsg_max_data = mca_btl_ugni_component.ugni_smsg_limit -
        sizeof (mca_btl_ugni_send_frag_hdr_t);

    if (mca_btl_ugni_component.ugni_smsg_limit == mca_btl_ugni_module.super.btl_eager_limit) {
        mca_btl_ugni_module.super.btl_eager_limit = mca_btl_ugni_component.smsg_max_data;
    }

    /* calculate mailbox size */
    tmp_smsg_attrib.msg_type       = GNI_SMSG_TYPE_MBOX_AUTO_RETRANSMIT;
    tmp_smsg_attrib.msg_maxsize    = mca_btl_ugni_component.ugni_smsg_limit;
    tmp_smsg_attrib.mbox_maxcredit = mca_btl_ugni_component.smsg_max_credits;

    grc = GNI_SmsgBufferSizeNeeded (&tmp_smsg_attrib, &mbox_size);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != grc)) {
        BTL_ERROR(("error in GNI_SmsgBufferSizeNeeded"));
        return opal_common_rc_ugni_to_opal (grc);
    }

    mca_btl_ugni_component.smsg_mbox_size = OPAL_ALIGN(mbox_size, 64, unsigned int);

    return OPAL_SUCCESS;
}
