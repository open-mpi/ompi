/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
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
#include "opal/mca/pmix/pmix.h"

#define INITIAL_GNI_EPS 1024

static int
mca_btl_ugni_setup_mpools (mca_btl_ugni_module_t *ugni_module);
static void
mca_btl_ugni_module_set_max_reg (mca_btl_ugni_module_t *ugni_module, int nlocal_procs);
static int mca_btl_ugni_smsg_setup (int nprocs);

int mca_btl_ugni_add_procs (struct mca_btl_base_module_t* btl, size_t nprocs,
                            struct opal_proc_t **procs,
                            struct mca_btl_base_endpoint_t **peers,
                            opal_bitmap_t *reachable) {
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    int rc;
    void *mmap_start_addr;
    struct timeval tv = {.tv_sec = 0, .tv_usec = MCA_BTL_UGNI_CONNECT_USEC};

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
        rc = opal_hash_table_init (&ugni_module->id_to_endpoint, INITIAL_GNI_EPS);
        if (OPAL_SUCCESS != rc) {
            BTL_ERROR(("error initializing the endpoint hash. rc = %d", rc));
            return rc;
        }
    }

    for (size_t i = 0 ; i < nprocs ; ++i) {
        peers[i] = mca_btl_ugni_get_ep (btl, procs[i]);
        if (NULL == peers[i]) {
            continue;
        }

        if (procs[i] == opal_proc_local_get ()) {
            ugni_module->local_ep = peers[i];
        }

        /* Set the reachable bit if necessary */
        if (reachable) {
            (void) opal_bitmap_set_bit (reachable, i);
        }
    }

    mca_btl_ugni_module_set_max_reg (ugni_module, ugni_module->nlocal_procs);

    if (false == ugni_module->initialized) {
        for (int i = 0 ; i < mca_btl_ugni_component.virtual_device_count ; ++i) {
            mca_btl_ugni_device_t *device = ugni_module->devices + i;
            rc = GNI_CqCreate (device->dev_handle, mca_btl_ugni_component.local_rdma_cq_size, 0,
                               GNI_CQ_NOBLOCK, NULL, NULL, &device->dev_rdma_local_cq.gni_handle);
            if (GNI_RC_SUCCESS != rc) {
                BTL_ERROR(("error creating local BTE/FMA CQ"));
                return mca_btl_rc_ugni_to_opal (rc);
            }

            rc = GNI_CqCreate (device->dev_handle, mca_btl_ugni_component.local_cq_size,
                               0, GNI_CQ_NOBLOCK, NULL, NULL, &device->dev_smsg_local_cq.gni_handle);
            if (GNI_RC_SUCCESS != rc) {
                BTL_ERROR(("error creating local SMSG CQ"));
                return mca_btl_rc_ugni_to_opal (rc);
            }

            if (mca_btl_ugni_component.progress_thread_enabled) {
                rc = GNI_CqCreate (device->dev_handle, mca_btl_ugni_component.local_rdma_cq_size,
                                   0, GNI_CQ_BLOCKING, NULL, NULL, &device->dev_rdma_local_irq_cq.gni_handle);
                if (GNI_RC_SUCCESS != rc) {
                    BTL_ERROR(("error creating local BTE/FMA CQ"));
                    return mca_btl_rc_ugni_to_opal (rc);
                }
            }
        }

        rc = GNI_CqCreate (ugni_module->devices[0].dev_handle, mca_btl_ugni_component.remote_cq_size,
                           0, GNI_CQ_NOBLOCK, NULL, NULL, &ugni_module->smsg_remote_cq);
        if (GNI_RC_SUCCESS != rc) {
            BTL_ERROR(("error creating remote SMSG CQ"));
            return mca_btl_rc_ugni_to_opal (rc);
        }

        if (mca_btl_ugni_component.progress_thread_enabled) {
            rc = GNI_CqCreate (ugni_module->devices[0].dev_handle, mca_btl_ugni_component.remote_cq_size,
                               0, GNI_CQ_BLOCKING, NULL, NULL, &ugni_module->smsg_remote_irq_cq);
            if (GNI_RC_SUCCESS != rc) {
                BTL_ERROR(("error creating remote SMSG CQ"));
                return mca_btl_rc_ugni_to_opal (rc);
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

            rc = GNI_MemRegister(ugni_module->devices[0].dev_handle,
                                     (unsigned long)mmap_start_addr,
                                     4096,
                                     ugni_module->smsg_remote_irq_cq,
                                     GNI_MEM_READWRITE,
                                     -1,
                                     &ugni_module->devices[0].smsg_irq_mhndl);

            mca_btl_ugni_spawn_progress_thread(btl);
        }

        opal_event_evtimer_add (&ugni_module->connection_event, &tv);

        ugni_module->initialized = true;
    }

    return OPAL_SUCCESS;
}

int mca_btl_ugni_del_procs (struct mca_btl_base_module_t *btl,
                            size_t nprocs, struct opal_proc_t **procs,
                            struct mca_btl_base_endpoint_t **peers) {
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;

    OPAL_THREAD_LOCK(&ugni_module->endpoint_lock);

    for (size_t i = 0 ; i < nprocs ; ++i) {
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

        if (OPAL_PROC_ON_LOCAL_NODE(opal_proc->proc_flags)) {
            --ugni_module->nlocal_procs;
        }

        /* remote the endpoint from the hash table */
        opal_hash_table_set_value_uint64 (&ugni_module->id_to_endpoint, proc_id, NULL);
    }

    OPAL_THREAD_UNLOCK(&ugni_module->endpoint_lock);

    mca_btl_ugni_module_set_max_reg (ugni_module, ugni_module->nlocal_procs);

    return OPAL_SUCCESS;
}


struct mca_btl_base_endpoint_t *mca_btl_ugni_get_ep (struct mca_btl_base_module_t *module, opal_proc_t *proc)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) module;
    uint64_t proc_id = mca_btl_ugni_proc_name_to_id(proc->proc_name);
    mca_btl_base_endpoint_t *ep;
    int rc;

    OPAL_THREAD_LOCK(&ugni_module->endpoint_lock);

    do {
        rc = opal_hash_table_get_value_uint64 (&ugni_module->id_to_endpoint, proc_id, (void **) &ep);
        if (OPAL_SUCCESS == rc) {
            BTL_VERBOSE(("returning existing endpoint for proc %s", OPAL_NAME_PRINT(proc->proc_name)));
            break;
        }

        BTL_VERBOSE(("initialized uGNI endpoint for proc id: 0x%" PRIx64 " ptr: %p", proc_id, (void *) proc));

        /*  Create and Init endpoints */
        rc = mca_btl_ugni_init_ep (ugni_module, &ep, ugni_module, proc);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_ERROR(("btl/ugni error initializing endpoint"));
            break;
        }

        /* ugni is allowed on local processes to provide support for network atomic operations */
        if (OPAL_PROC_ON_LOCAL_NODE(proc->proc_flags)) {
            ++ugni_module->nlocal_procs;
        }
        ++ugni_module->endpoint_count;

        /* add this endpoint to the connection lookup table */
        opal_hash_table_set_value_uint64 (&ugni_module->id_to_endpoint, proc_id, ep);
    } while (0);

    OPAL_THREAD_UNLOCK(&ugni_module->endpoint_lock);

    return ep;
}


static int ugni_reg_mem (void *reg_data, void *base, size_t size,
                         mca_rcache_base_registration_t *reg)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) reg_data;
    gni_cq_handle_t cq = 0;
    int flags, rc;

    if (ugni_module->reg_count >= ugni_module->reg_max) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (reg->access_flags & (MCA_RCACHE_ACCESS_REMOTE_WRITE | MCA_RCACHE_ACCESS_LOCAL_WRITE |
                             MCA_RCACHE_ACCESS_REMOTE_ATOMIC)) {
        flags = GNI_MEM_READWRITE;
    } else {
        flags = GNI_MEM_READ_ONLY;
    }

    if (!(reg->flags & MCA_RCACHE_FLAGS_SO_MEM)) {
        flags |= GNI_MEM_RELAXED_PI_ORDERING;
    }

    if (reg->flags & MCA_RCACHE_FLAGS_RESV0) {
        cq = ugni_module->smsg_remote_cq;
    }

    rc = mca_btl_ugni_reg_mem (ugni_module, base, size, (mca_btl_ugni_reg_t *) reg, cq, flags);
    if (OPAL_LIKELY(OPAL_SUCCESS == rc)) {
        opal_atomic_add_fetch_32(&ugni_module->reg_count,1);
    }

    return rc;
}

static int
ugni_dereg_mem (void *reg_data, mca_rcache_base_registration_t *reg)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) reg_data;
    int rc;

    rc = mca_btl_ugni_dereg_mem (ugni_module, (mca_btl_ugni_reg_t *) reg);
    if (OPAL_LIKELY(OPAL_SUCCESS == rc)) {
        opal_atomic_add_fetch_32(&ugni_module->reg_count,-1);
    }

    return rc;
}

static int
mca_btl_ugni_setup_mpools (mca_btl_ugni_module_t *ugni_module)
{
    mca_rcache_udreg_resources_t rcache_resources;
    unsigned int mbox_increment;
    uint32_t nprocs, *u32;
    char *rcache_name;
    int rc;

    rc = opal_pointer_array_init (&ugni_module->pending_smsg_frags_bb, 0,
                                  1 << 30, 32768);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* determine how many procs are in the job (might want to check universe size here) */
    u32 = &nprocs;
    OPAL_MODEX_RECV_VALUE(rc, OPAL_PMIX_UNIV_SIZE, &OPAL_PROC_MY_NAME,
                          &u32, OPAL_UINT32);
    if (OPAL_SUCCESS != rc) {
        /* take a wild conservative guess */
        nprocs = 512;
    }

    rc = mca_btl_ugni_smsg_setup (nprocs);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error setting up smsg"));
        return rc;
    }

    rc = opal_free_list_init (ugni_module->frags_lists + MCA_BTL_UGNI_LIST_SMSG,
                              sizeof (mca_btl_ugni_smsg_frag_t),
                              opal_cache_line_size, OBJ_CLASS(mca_btl_ugni_smsg_frag_t),
                              mca_btl_ugni_component.ugni_smsg_limit,
                              opal_cache_line_size,
                              mca_btl_ugni_component.ugni_free_list_num,
                              mca_btl_ugni_component.ugni_free_list_max,
                              mca_btl_ugni_component.ugni_free_list_inc,
                              NULL, 0, NULL, (opal_free_list_item_init_fn_t) mca_btl_ugni_frag_init,
                              (void *) (intptr_t) MCA_BTL_UGNI_LIST_SMSG);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error creating smsg fragment free list"));
        return rc;
    }

    rc = opal_free_list_init (ugni_module->frags_lists + MCA_BTL_UGNI_LIST_RDMA,
                              sizeof (mca_btl_ugni_rdma_frag_t), 64,
                              OBJ_CLASS(mca_btl_ugni_rdma_frag_t),
                              0, opal_cache_line_size,
                              mca_btl_ugni_component.ugni_free_list_num,
                              mca_btl_ugni_component.ugni_free_list_max,
                              mca_btl_ugni_component.ugni_free_list_inc,
                              NULL, 0, NULL, (opal_free_list_item_init_fn_t) mca_btl_ugni_frag_init,
                              (void *) (intptr_t) MCA_BTL_UGNI_LIST_RDMA);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    rc = opal_free_list_init (ugni_module->frags_lists + MCA_BTL_UGNI_LIST_RDMA_INT,
                              sizeof (mca_btl_ugni_rdma_frag_t), 8,
                              OBJ_CLASS(mca_btl_ugni_rdma_frag_t),
                              0, opal_cache_line_size, 0, -1, 64,
                              NULL, 0, NULL, (opal_free_list_item_init_fn_t) mca_btl_ugni_frag_init,
                              (void *) (intptr_t) MCA_BTL_UGNI_LIST_RDMA_INT);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    ugni_module->super.btl_mpool = mca_mpool_base_module_lookup (mca_btl_ugni_component.mpool_hints);
    if (NULL == ugni_module->super.btl_mpool) {
        BTL_ERROR(("could not find mpool matching hints %s", mca_btl_ugni_component.mpool_hints));
        return OPAL_ERROR;
    }

    rcache_resources.base.cache_name     = "ompi.ugni";
    rcache_resources.base.reg_data       = (void *) ugni_module;
    rcache_resources.base.sizeof_reg     = sizeof (mca_btl_ugni_reg_t);
    rcache_resources.base.register_mem   = ugni_reg_mem;
    rcache_resources.base.deregister_mem = ugni_dereg_mem;

    if (MCA_BTL_UGNI_RCACHE_UDREG == mca_btl_ugni_component.rcache_type) {
        /* additional settings for the udreg mpool */
        /* 4k should be large enough for any Gemini/Ares system */
        rcache_resources.max_entries       = 4096;
        rcache_resources.use_kernel_cache  = true;

        rcache_resources.use_evict_w_unreg = false;
        rcache_name = "udreg";
    } else {
        rcache_name = "grdma";
    }

    ugni_module->rcache =
        mca_rcache_base_module_create (rcache_name, ugni_module->devices, &rcache_resources.base);

    if (NULL == ugni_module->rcache) {
        BTL_ERROR(("error creating registration cache"));
        return OPAL_ERROR;
    }

    rc = opal_free_list_init (ugni_module->frags_lists + MCA_BTL_UGNI_LIST_EAGER_SEND,
                              sizeof (mca_btl_ugni_eager_frag_t), 8,
                              OBJ_CLASS(mca_btl_ugni_eager_frag_t),
                              ugni_module->super.btl_eager_limit, 64,
                              mca_btl_ugni_component.ugni_eager_num,
                              mca_btl_ugni_component.ugni_eager_max,
                              mca_btl_ugni_component.ugni_eager_inc,
                              ugni_module->super.btl_mpool, 0, ugni_module->rcache,
                              (opal_free_list_item_init_fn_t) mca_btl_ugni_frag_init,
                              (void *) (intptr_t) MCA_BTL_UGNI_LIST_EAGER_SEND);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error creating eager send fragment free list"));
        return rc;
    }

    rc = opal_free_list_init (ugni_module->frags_lists + MCA_BTL_UGNI_LIST_EAGER_RECV,
                              sizeof (mca_btl_ugni_eager_frag_t), 8,
                              OBJ_CLASS(mca_btl_ugni_eager_frag_t),
                              ugni_module->super.btl_eager_limit, 64,
                              mca_btl_ugni_component.ugni_eager_num,
                              mca_btl_ugni_component.ugni_eager_max,
                              mca_btl_ugni_component.ugni_eager_inc,
                              ugni_module->super.btl_mpool, 0, ugni_module->rcache,
                              (opal_free_list_item_init_fn_t) mca_btl_ugni_frag_init,
                              (void *) (intptr_t) MCA_BTL_UGNI_LIST_EAGER_RECV);
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

    /* use the MCA_RCACHE_FLAGS_RESV0 to signal this is smsg memory */
    rc = opal_free_list_init (&ugni_module->smsg_mboxes,
                              sizeof (mca_btl_ugni_smsg_mbox_t), 8,
                              OBJ_CLASS(mca_btl_ugni_smsg_mbox_t),
                              mca_btl_ugni_component.smsg_mbox_size, 128,
                              32, -1, mbox_increment, ugni_module->super.btl_mpool,
                              MCA_RCACHE_FLAGS_SO_MEM | MCA_RCACHE_FLAGS_RESV0,
                              ugni_module->rcache, NULL, NULL);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error creating smsg mailbox free list"));
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

        grc = GNI_GetJobResInfo (0, mca_btl_ugni_component.ptag,
                                 GNI_JOB_RES_MDD, &res_des);
        if (GNI_RC_SUCCESS == grc) {
            if (nlocal_procs) {
                ugni_module->reg_max = (res_des.limit - fuzz) / nlocal_procs;
            } else {
                ugni_module->reg_max = 0;
            }
        }
#else
        /* no way to determine the maximum registration count */
        if (nlocal_procs) {
            ugni_module->reg_max = 1200 / nlocal_procs;
        } else {
            ugni_module->reg_max = 0;
        }
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
        return mca_btl_rc_ugni_to_opal (grc);
    }

    mca_btl_ugni_component.smsg_mbox_size = OPAL_ALIGN(mbox_size, 64, unsigned int);

    return OPAL_SUCCESS;
}
