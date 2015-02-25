/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
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

static volatile int stop_progress_thread = 0;

unsigned int mca_btl_ugni_progress_thread_wakeups;

static void *mca_btl_ugni_prog_thread_fn(void * data)
{
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
            mca_btl_ugni_progress_thread_wakeups++;
            opal_progress();
        }
    }

    return (void *) (intptr_t) OPAL_SUCCESS;
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
    int ret=OPAL_SUCCESS;
    void *thread_rc;

    stop_progress_thread = 1;

    /*
     * post a CQ to myself to wake my thread up
     */

    ret = mca_btl_ugni_post_cqwrite (mca_btl_ugni_component.modules[0].local_ep,
                                     mca_btl_ugni_component.modules[0].rdma_local_cq,
                                     mca_btl_ugni_component.modules[0].device->smsg_irq_mhndl,
                                     0xdead, NULL, NULL, NULL);
    /*
     * TODO: if error returned, need to kill off thread manually
     */
    if (OPAL_SUCCESS != ret) {
        /* force the thread to exit */
        pthread_cancel (mca_btl_ugni_progress_thread_id);
        goto fn_exit;
    }

    pthread_join (mca_btl_ugni_progress_thread_id, &thread_rc);
    if (0 != (intptr_t) thread_rc) {
        BTL_ERROR(("btl/ugni error returned from progress thread: %d", (int) (intptr_t) thread_rc));
        ret = (int)(intptr_t) thread_rc;
    }

   fn_exit:
    return ret;
}

