/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
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


static pthread_t mca_btl_ugni_progress_thread_id;
static pthread_mutex_t progress_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t progress_cond = PTHREAD_COND_INITIALIZER;

static volatile int stop_progress_thread = 0;
static volatile int progress_thread_done = 0;

static int thread_wakeups = 0;

static void *mca_btl_ugni_prog_thread_fn(void * data)
{
    int rc,ret = OPAL_SUCCESS;
    uint32_t which;
    gni_return_t status;
    gni_cq_handle_t cq_vec[2];

    struct mca_btl_ugni_module_t *btl = (mca_btl_ugni_module_t *)data;

    /*
     * need to block signals
     */

    cq_vec[0] = btl->smsg_remote_irq_cq;
    cq_vec[1] = btl->rdma_local_irq_cq;


    while (stop_progress_thread == 0) {

        /*
         * this ugni call doesn't need a lock
         */

        status = GNI_CqVectorMonitor(cq_vec,
                                     2,
                                     -1,
                                     &which);

        if (status == GNI_RC_NOT_DONE) continue;

        if ((status == GNI_RC_SUCCESS) && (stop_progress_thread == 0)) {
            thread_wakeups++;
            opal_progress();
        }
    }

    /* Send a signal to the main thread saying we are done */
    rc = pthread_mutex_lock(&progress_mutex);
    if (0 != rc) {
        BTL_ERROR(("btl/ugni pthread_mutex_lock returned %s ",strerror(rc)));
        ret = OPAL_ERROR;
        goto fn_exit;
    }

    progress_thread_done = 1;

    rc = pthread_mutex_unlock(&progress_mutex);
    if (0 != rc) {
        BTL_ERROR(("btl/ugni pthread_mutex_unlock returned %s ",strerror(rc)));
        ret = OPAL_ERROR;
        goto fn_exit;
    }

    rc = pthread_cond_signal(&progress_cond);
    if (0 != rc) {
        BTL_ERROR(("btl/ugni pthread_cond_signal returned %s ",strerror(rc)));
        ret = OPAL_ERROR;
    }

   fn_exit:
    return ret;
}

int mca_btl_ugni_spawn_progress_thread(struct mca_btl_base_module_t *btl)
{
    int rc, ret=OPAL_SUCCESS;
    pthread_attr_t attr;

    pthread_attr_init(&attr);
    rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    if (0 != rc) {
        BTL_ERROR(("btl/ugni pthread_attr_setdetachstate returned %s ",strerror(rc)));
        ret = OPAL_ERROR;
        goto fn_exit;
    }

    rc = pthread_create(&mca_btl_ugni_progress_thread_id, 
                        &attr, mca_btl_ugni_prog_thread_fn, (void *)btl);
    if (0 != rc) {
        BTL_ERROR(("btl/ugni pthread_create returned %s ",strerror(rc)));
        ret = OPAL_ERROR;
        goto fn_exit;
    }

    rc = pthread_attr_destroy(&attr);
    if (0 != rc) {
        BTL_ERROR(("btl/ugni pthread_attr_destory returned %s ",strerror(rc)));
        ret = OPAL_ERROR;
    }

   fn_exit:
    return ret;
}

int mca_btl_ugni_kill_progress_thread(void)
{
    int rc, ret=OPAL_SUCCESS;
    gni_return_t status;
    static mca_btl_ugni_base_frag_t cq_write_frag;

    stop_progress_thread = 1;

    /*
     * post a CQ to myself to wake my thread up
     */

    cq_write_frag.post_desc.base.type = GNI_POST_CQWRITE;
    cq_write_frag.post_desc.base.cqwrite_value = 0xdead;   /* up to 48 bytes here, not used for now */
    cq_write_frag.post_desc.base.cq_mode = GNI_CQMODE_GLOBAL_EVENT;
    cq_write_frag.post_desc.base.dlvr_mode = GNI_DLVMODE_IN_ORDER;
    cq_write_frag.post_desc.base.src_cq_hndl = mca_btl_ugni_component.modules[0].rdma_local_cq;
    cq_write_frag.post_desc.base.remote_mem_hndl = mca_btl_ugni_component.modules[0].device->smsg_irq_mhndl;
    cq_write_frag.post_desc.tries = 0;
    cq_write_frag.cbfunc = NULL;
    OPAL_THREAD_LOCK(&mca_btl_ugni_component.modules[0].device->dev_lock);
    status = GNI_PostCqWrite(mca_btl_ugni_component.modules[0].local_ep,
                             &cq_write_frag.post_desc.base);
    OPAL_THREAD_UNLOCK(&mca_btl_ugni_component.modules[0].device->dev_lock);
    /*
     * TODO: if error returned, need to kill off thread manually
     */
    if (GNI_RC_SUCCESS != status) {
        BTL_ERROR(("GNI_PostCqWrite returned error - %s",gni_err_str[status]));
        ret = opal_common_rc_ugni_to_opal(status);
        goto fn_exit;
    }

    rc = pthread_mutex_lock(&progress_mutex);
    if (0 != rc) {
        BTL_ERROR(("btl/ugni pthread_mutex_lock returned %s ",strerror(rc)));
        ret = OPAL_ERROR;
        goto fn_exit;
    }

    while (!progress_thread_done) {
        pthread_cond_wait(&progress_cond, &progress_mutex);
        if (0 != rc) {
            BTL_ERROR(("btl/ugni pthread_cond_wait returned %s ",strerror(rc)));
            ret = OPAL_ERROR;
            goto fn_exit;
        }
    }

    rc = pthread_mutex_unlock(&progress_mutex);
    if (0 != rc) {
        BTL_ERROR(("btl/ugni pthread_mutex_unlock returned %s ",strerror(rc)));
        ret = OPAL_ERROR;
        goto fn_exit;
    }

    /*
     * destroy the local_ep
     */

    OPAL_THREAD_LOCK(&mca_btl_ugni_component.modules[0].device->dev_lock);
    status =  GNI_EpDestroy (mca_btl_ugni_component.modules[0].local_ep);
    OPAL_THREAD_UNLOCK(&mca_btl_ugni_component.modules[0].device->dev_lock);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != status)) {
        BTL_ERROR(("GNI_EpDestroy returned error - %s", gni_err_str[status]));
        ret = opal_common_rc_ugni_to_opal(status);
        goto fn_exit;
    }

   fn_exit:
    return ret;
}

