/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/util/sys_limits.h"

#include "btl_scif.h"
#include "btl_scif_frag.h"

static int
mca_btl_scif_setup_mpools (mca_btl_scif_module_t *scif_module);
static void *mca_btl_scif_connect_accept (void *arg);

int mca_btl_scif_add_procs(struct mca_btl_base_module_t* btl,
                           size_t nprocs,
                           struct opal_proc_t **procs,
                           struct mca_btl_base_endpoint_t **peers,
                           opal_bitmap_t *reachable) {
    mca_btl_scif_module_t *scif_module = (mca_btl_scif_module_t *) btl;
    size_t procs_on_board, i, board_proc;
    opal_proc_t *my_proc = opal_proc_local_get();
    int rc;

    /* determine how many procs are on this board */
    for (i = 0, procs_on_board = 0 ; i < nprocs ; ++i) {
        struct opal_proc_t *opal_proc = procs[i];

        if (my_proc == opal_proc) {
            continue;
        }

        if (!OPAL_PROC_ON_LOCAL_HOST(opal_proc->proc_flags) ||
            my_proc == opal_proc) {
            /* scif can only be used with procs on this board */
            continue;
        }

        procs_on_board++;
    }

    /* allocate space for the detected peers and setup the mpool */
    if (NULL == scif_module->endpoints) {
        scif_module->endpoints = calloc (procs_on_board, sizeof (mca_btl_base_endpoint_t));
        if (OPAL_UNLIKELY(NULL == scif_module->endpoints)) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        rc = mca_btl_scif_setup_mpools (scif_module);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_ERROR(("btl/scif error setting up mpools/free lists"));
            return rc;
        }
    }

    for (i = 0, board_proc = 0 ; i < nprocs ; ++i) {
        struct opal_proc_t *opal_proc = procs[i];

        if (my_proc == opal_proc) {
            continue;
        }

        if (!OPAL_PROC_ON_LOCAL_HOST(opal_proc->proc_flags) ||
            my_proc == opal_proc) {
            peers[i] = NULL;
            /* scif can only be used with procs on this board */
            continue;
        }

        /* Initialize endpoints */
        rc = mca_btl_scif_ep_init (scif_module->endpoints + board_proc, (mca_btl_scif_module_t *) btl, opal_proc);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_ERROR(("btl/scif error initializing endpoint"));
            return rc;
        }

        scif_module->endpoints[board_proc].id = board_proc;

        /* Set the reachable bit */
        rc = opal_bitmap_set_bit (reachable, i);

        /* Store a reference to this peer */
        peers[i] = scif_module->endpoints + board_proc;

        board_proc++;
    }

    BTL_VERBOSE(("%lu procs on board\n", (unsigned long) procs_on_board));

    scif_module->endpoint_count = procs_on_board;

    if (!mca_btl_scif_module.listening) {
        /* start listening thread */
        rc = pthread_create (&mca_btl_scif_module.listen_thread, NULL, mca_btl_scif_connect_accept, NULL);
        if (0 > rc) {
            return OPAL_ERROR;
        }
        mca_btl_scif_module.listening = true;
    }

    return OPAL_SUCCESS;
}

static void *mca_btl_scif_connect_accept (void *arg)
{
    struct scif_pollepd pollepd = {.epd = mca_btl_scif_module.scif_fd, .events = SCIF_POLLIN, .revents = 0};
    int rc;

    BTL_VERBOSE(("btl/scif: listening for new connections"));

    /* listen for connections */
    while (1) {
        pollepd.revents = 0;

        rc = scif_poll (&pollepd, 1, -1);
        if (1 == rc) {
            if (SCIF_POLLIN != pollepd.revents) {
                break;
            }
            if (mca_btl_scif_module.exiting) {
                /* accept the connection so scif_connect() does not timeout */
                struct scif_portID peer;
                scif_epd_t newepd;
                scif_accept(mca_btl_scif_module.scif_fd, &peer, &newepd, SCIF_ACCEPT_SYNC);
                scif_close(newepd);
                break;
            }

            rc = mca_btl_scif_ep_connect_start_passive ();
            if (OPAL_SUCCESS != rc) {
                BTL_VERBOSE(("btl/scif: error accepting scif connection"));
                continue;
            }
        } else {
            break;
        }
    }

    BTL_VERBOSE(("btl/scif: stopped listening for new connections"));

    return NULL;
}

int mca_btl_scif_del_procs (struct mca_btl_base_module_t *btl,
                            size_t nprocs, struct opal_proc_t **procs,
                            struct mca_btl_base_endpoint_t **peers) {
    /* do nothing for now */
    return OPAL_SUCCESS;
}

static int scif_dereg_mem (void *reg_data, mca_mpool_base_registration_t *reg)
{
    mca_btl_scif_reg_t *scif_reg = (mca_btl_scif_reg_t *)reg;
    size_t size = (size_t)((uintptr_t) reg->bound - (uintptr_t) reg->base);
    int i;

    /* register the fragment with all connected endpoints */
    for (i = 0 ; i < (int) mca_btl_scif_module.endpoint_count ; ++i) {
        if ((off_t)-1 != scif_reg->handles[i].btl_handle.scif_offset &&
            MCA_BTL_SCIF_EP_STATE_CONNECTED == mca_btl_scif_module.endpoints[i].state) {
            (void) scif_unregister(mca_btl_scif_module.endpoints[i].scif_epd,
                                   scif_reg->handles[i].btl_handle.scif_offset, size);
        }
    }

    free (scif_reg->handles);

    return OPAL_SUCCESS;
}

static int scif_reg_mem (void *reg_data, void *base, size_t size,
                         mca_mpool_base_registration_t *reg)
{
    mca_btl_scif_reg_t *scif_reg = (mca_btl_scif_reg_t *)reg;
    int rc = OPAL_SUCCESS;
    unsigned int i;

    scif_reg->handles = calloc (mca_btl_scif_module.endpoint_count, sizeof (scif_reg->handles[0]));

    /* intialize all scif offsets to -1 and initialize the pointer back to the mpool registration */
    for (i = 0 ; i < mca_btl_scif_module.endpoint_count ; ++i) {
        scif_reg->handles[i].btl_handle.scif_offset = -1;
        scif_reg->handles[i].btl_handle.scif_base = (intptr_t) base;
        scif_reg->handles[i].reg = scif_reg;
    }

    /* register the pointer with all connected endpoints */
    for (i = 0 ; i < mca_btl_scif_module.endpoint_count ; ++i) {
        if (MCA_BTL_SCIF_EP_STATE_CONNECTED == mca_btl_scif_module.endpoints[i].state) {
            scif_reg->handles[i].btl_handle.scif_offset = scif_register (mca_btl_scif_module.endpoints[i].scif_epd,
                                                                         base, size, 0, SCIF_PROT_READ |
                                                                         SCIF_PROT_WRITE, 0);
            if (SCIF_REGISTER_FAILED == scif_reg->handles[i].btl_handle.scif_offset) {
                /* cleanup */
                scif_dereg_mem (reg_data, reg);
                rc = OPAL_ERR_OUT_OF_RESOURCE;
                break;
            }
        }
    }

    return rc;
}

static int
mca_btl_scif_setup_mpools (mca_btl_scif_module_t *scif_module)
{
    struct mca_mpool_base_resources_t mpool_resources;
    int rc;

    /* initialize the grdma mpool */
    mpool_resources.pool_name      = "scif";
    mpool_resources.reg_data       = (void *) scif_module;
    mpool_resources.sizeof_reg     = sizeof (mca_btl_scif_reg_t);
    mpool_resources.register_mem   = scif_reg_mem;
    mpool_resources.deregister_mem = scif_dereg_mem;
    scif_module->super.btl_mpool =
        mca_mpool_base_module_create("grdma", scif_module, &mpool_resources);
    if (NULL == scif_module->super.btl_mpool) {
        BTL_ERROR(("error creating grdma mpool"));
        return OPAL_ERROR;
    }

    /* setup free lists for fragments. dma fragments will be used for
     * rma operations and in-place sends. eager frags will be used for
     * buffered sends. */
    rc = opal_free_list_init (&scif_module->dma_frags,
                              sizeof (mca_btl_scif_dma_frag_t), 64,
                              OBJ_CLASS(mca_btl_scif_dma_frag_t),
                              128, opal_getpagesize (),
                              mca_btl_scif_component.scif_free_list_num,
                              mca_btl_scif_component.scif_free_list_max,
                              mca_btl_scif_component.scif_free_list_inc,
                              NULL, 0, NULL, NULL, NULL);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    rc = opal_free_list_init (&scif_module->eager_frags,
                              sizeof (mca_btl_scif_eager_frag_t), 8,
                              OBJ_CLASS(mca_btl_scif_eager_frag_t),
                              128 + scif_module->super.btl_eager_limit, 64,
                              mca_btl_scif_component.scif_free_list_num,
                              mca_btl_scif_component.scif_free_list_max,
                              mca_btl_scif_component.scif_free_list_inc,
                              NULL, 0, NULL, NULL, NULL);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error creating eager receive fragment free list"));
        return rc;
    }

    return OPAL_SUCCESS;
}
