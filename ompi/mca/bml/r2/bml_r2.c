/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2008-2016 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2026 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      Intel, Inc. All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "opal/class/opal_bitmap.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#include "ompi/mca/bml/base/bml_base_btl.h"
#include "bml_r2.h"
#include "ompi/proc/proc.h"
#include "ompi/runtime/ompi_modex.h"

extern mca_bml_base_component_t mca_bml_r2_component;

/* Names of all the BTL components that this BML is aware of */
static char *btl_names = NULL;

static void mca_bml_r2_register_progress(mca_btl_base_module_t *btl, bool hp);

/*
 * Say which pair of processes cannot reach each other, and what was
 * tried. Wire-up used to be collective, so this was reported from the
 * add_procs every rank ran during MPI_Init; built on first use, the same
 * conclusion is instead reached by whoever sends to that peer, and the
 * only thing the caller can do with it is fail the operation with
 * MPI_ERR_UNREACH. That code alone does not say which btls were asked or
 * which hosts were involved, and the peer is not named at all, so it is
 * still worth an explanation here. Telling a final no from an early one
 * is what the per-rank status made possible.
 *
 * Once per process, not once per peer: the default error handler turns
 * the MPI_ERR_UNREACH below into an abort, so a second one is only
 * reachable if the application chose to carry on, where repeating this
 * essay per send would bury whatever it printed itself.
 */
static void mca_bml_r2_show_unreach(ompi_proc_t *proc)
{
    static bool shown = false;
    char *errhost, *localhost;

    if (!mca_bml_r2.show_unreach_errors || shown) {
        return;
    }
    shown = true;

    errhost = opal_get_proc_hostname(&proc->super);
    localhost = opal_get_proc_hostname(&ompi_proc_local_proc->super);
    /* Which of the two conclusions this is. Every btl declining the peer
     * is one thing; every btl reading absent keys off a blob that was
     * never fetched is another, and the list of btls attempted is beside
     * the point in the second -- none of them was ever given anything to
     * decline. Only the fetch knows the difference, and it says so on the
     * proc. */
    if (opal_proc_known(&proc->super, OPAL_PROC_FLAG_FETCH_FAILED)) {
        opal_show_help("help-mca-bml-r2.txt", "peer info unavailable", true,
                       OMPI_NAME_PRINT(&(ompi_proc_local_proc->super.proc_name)),
                       localhost,
                       OMPI_NAME_PRINT(&(proc->super.proc_name)),
                       errhost);
    } else {
        opal_show_help("help-mca-bml-r2.txt", "unreachable proc", true,
                       OMPI_NAME_PRINT(&(ompi_proc_local_proc->super.proc_name)),
                       localhost,
                       OMPI_NAME_PRINT(&(proc->super.proc_name)),
                       errhost,
                       btl_names);
    }
    free(errhost);
    free(localhost);
}

static int btl_exclusivity_compare(const void* arg1, const void* arg2)
{
    mca_btl_base_module_t* btl1 = *(struct mca_btl_base_module_t**)arg1;
    mca_btl_base_module_t* btl2 = *(struct mca_btl_base_module_t**)arg2;
    if( btl1->btl_exclusivity > btl2->btl_exclusivity ) {
        return -1;
    } else if (btl1->btl_exclusivity == btl2->btl_exclusivity ) {
        return 0;
    } else {
        return 1;
    }
}

static int mca_bml_r2_add_btls( void )
{
    int i;
    opal_list_t *btls = NULL;
    mca_btl_base_selected_module_t* selected_btl;
    size_t num_btls = 0;
    char **btl_names_argv = NULL;

    if(true == mca_bml_r2.btls_added) {
        return OMPI_SUCCESS;
    }

    /* build an array of r2s and r2 modules */
    btls = &mca_btl_base_modules_initialized;
    num_btls = opal_list_get_size(btls);

    mca_bml_r2.num_btl_modules = 0;
    mca_bml_r2.num_btl_progress = 0;

    mca_bml_r2.btl_modules = (mca_btl_base_module_t **)malloc(sizeof(mca_btl_base_module_t*) * num_btls);
    mca_bml_r2.btl_progress = (mca_btl_base_component_progress_fn_t*)malloc(sizeof(mca_btl_base_component_progress_fn_t) * num_btls);

    if (NULL == mca_bml_r2.btl_modules ||
        NULL == mca_bml_r2.btl_progress) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    OPAL_LIST_FOREACH(selected_btl, btls, mca_btl_base_selected_module_t) {
        mca_btl_base_module_t *btl = selected_btl->btl_module;
        mca_bml_r2.btl_modules[mca_bml_r2.num_btl_modules++] = btl;
        for (i = 0; NULL != btl_names_argv && NULL != btl_names_argv[i]; ++i) {
            if (0 ==
                strcmp(btl_names_argv[i],
                       btl->btl_component->btl_version.mca_component_name)) {
                break;
            }
        }
        if (NULL == btl_names_argv || NULL == btl_names_argv[i]) {
            opal_argv_append_nosize(&btl_names_argv,
                                    btl->btl_component->btl_version.mca_component_name);
        }
    }
    if (NULL != btl_names_argv) {
        btl_names = opal_argv_join(btl_names_argv, ' ');
        opal_argv_free(btl_names_argv);
    } else {
        btl_names = strdup("no devices available");
    }

    /* sort r2 list by exclusivity */
    qsort(mca_bml_r2.btl_modules,
          mca_bml_r2.num_btl_modules,
          sizeof(struct mca_btl_base_module_t*),
          btl_exclusivity_compare);
    mca_bml_r2.btls_added = true;

    /* Recv-first / ANY_SOURCE must poll BTLs that have a progress
     * function (sm FIFO) without add_proc of every peer. */
    for (size_t p = 0; p < mca_bml_r2.num_btl_modules; ++p) {
        mca_bml_r2_register_progress(mca_bml_r2.btl_modules[p], true);
    }
    return OMPI_SUCCESS;
}

static int btl_bandwidth_compare(const void *v1, const void *v2)
{
    mca_bml_base_btl_t *b1 = (mca_bml_base_btl_t*)v1,
                       *b2 = (mca_bml_base_btl_t*)v2;

    return b2->btl->btl_bandwidth - b1->btl->btl_bandwidth;
}

static void mca_bml_r2_calculate_bandwidth_latency (mca_bml_base_btl_array_t *btl_array, double *total_bandwidth, uint32_t *latency)
{
    const size_t array_length = mca_bml_base_btl_array_get_size (btl_array);

    *latency = UINT_MAX;
    *total_bandwidth = 0.;

    for (size_t i = 0 ; i < array_length ; ++i) {
        mca_bml_base_btl_t *bml_btl = mca_bml_base_btl_array_get_index (btl_array, i);
        mca_btl_base_module_t *btl = bml_btl->btl;
        *total_bandwidth += btl->btl_bandwidth;
        if (btl->btl_latency < *latency) {
            *latency = btl->btl_latency;
        }
    }
}

static mca_bml_base_endpoint_t *mca_bml_r2_allocate_endpoint (ompi_proc_t *proc) {
    mca_bml_base_endpoint_t *bml_endpoint;

    /* allocate bml specific proc data */
    bml_endpoint = OBJ_NEW(mca_bml_base_endpoint_t);
    if (NULL == bml_endpoint) {
        opal_output(0, "%s: unable to allocate resources", __func__);
        return NULL;
    }

    /* preallocate space in array for max number of r2s */
    mca_bml_base_btl_array_reserve(&bml_endpoint->btl_eager, mca_bml_r2.num_btl_modules);
    mca_bml_base_btl_array_reserve(&bml_endpoint->btl_send,  mca_bml_r2.num_btl_modules);
    mca_bml_base_btl_array_reserve(&bml_endpoint->btl_rdma,  mca_bml_r2.num_btl_modules);
    bml_endpoint->btl_max_send_size = -1;
    bml_endpoint->btl_proc = proc;

    bml_endpoint->btl_flags_or = 0;
    return bml_endpoint;
}

static void mca_bml_r2_register_progress (mca_btl_base_module_t *btl, bool hp)
{
    if (NULL != btl->btl_component->btl_progress) {
        bool found = false;
        size_t p;

        for (p = 0 ; p < mca_bml_r2.num_btl_progress ; ++p) {
            if(mca_bml_r2.btl_progress[p] == btl->btl_component->btl_progress) {
                found = true;
                break;
            }
        }

        if (found == false || hp) {
            if (found == false) {
                mca_bml_r2.btl_progress[mca_bml_r2.num_btl_progress++] =
                    btl->btl_component->btl_progress;
            }

            if (hp) {
                opal_progress_register (btl->btl_component->btl_progress);
            } else {
                opal_progress_register_lp (btl->btl_component->btl_progress);
            }
        }
    }
}

static int mca_bml_r2_endpoint_add_btl (struct ompi_proc_t *proc, mca_bml_base_endpoint_t *bml_endpoint,
                                        mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *btl_endpoint)
{
    mca_bml_base_btl_t* bml_btl = NULL;
    int btl_flags = btl->btl_flags;
    bool btl_in_use = false;
    size_t size;

    /* NTH: these flags should have been sanitized by the btl. Once that is verified these
     * checks can be safely removed. */
    if ((btl_flags & MCA_BTL_FLAGS_PUT) && (NULL == btl->btl_put)) {
        opal_output(0, "%s: The PUT flag is specified for"
                    " the %s BTL without any PUT function attached. Discard the flag !",
                    __func__,
                    btl->btl_component->btl_version.mca_component_name);
        btl_flags ^= MCA_BTL_FLAGS_PUT;
    }
    if ((btl_flags & MCA_BTL_FLAGS_GET) && (NULL == btl->btl_get)) {
        opal_output(0, "%s: The GET flag is specified for"
                    " the %s BTL without any GET function attached. Discard the flag !",
                    __func__, btl->btl_component->btl_version.mca_component_name);
        btl_flags ^= MCA_BTL_FLAGS_GET;
    }

    if ((btl_flags & (MCA_BTL_FLAGS_PUT | MCA_BTL_FLAGS_GET | MCA_BTL_FLAGS_SEND)) == 0) {
        /* If no protocol specified, we have 2 choices: we ignore the BTL
         * as we don't know which protocol to use, or we suppose that all
         * BTLs support the send protocol. This is really a btl error as
         * these flags should have been sanitized by the btl. */
        btl_flags |= MCA_BTL_FLAGS_SEND;
    }

    if (btl_flags & MCA_BTL_FLAGS_SEND) {
        /* dont allow an additional BTL with a lower exclusivity ranking */
        size = mca_bml_base_btl_array_get_size (&bml_endpoint->btl_send);
        bml_btl = mca_bml_base_btl_array_get_index (&bml_endpoint->btl_send, size - 1);

        if (!bml_btl || bml_btl->btl->btl_exclusivity <= btl->btl_exclusivity) {
            /* this btl has higher exclusivity than an existing btl or none exists */
            if (0 < opal_output_get_verbosity(opal_btl_base_framework.framework_output)) {
                char *errhost = opal_get_proc_hostname(&proc->super);
                opal_output(0, "mca: bml: Using %s btl for send to %s on node %s",
                            btl->btl_component->btl_version.mca_component_name,
                            OMPI_NAME_PRINT(&proc->super.proc_name),
                            errhost);
                free(errhost);
            }

            /* cache the endpoint on the proc */
            if (NULL == bml_btl || (bml_btl->btl->btl_exclusivity <= btl->btl_exclusivity)) {
                bml_btl = mca_bml_base_btl_array_insert (&bml_endpoint->btl_send);
                bml_btl->btl = btl;
                bml_btl->btl_endpoint = btl_endpoint;
                bml_btl->btl_weight = 0;
                bml_btl->btl_flags = btl_flags;

                /**
                 * calculate the bitwise OR of the btl flags
                 */
                bml_endpoint->btl_flags_or |= bml_btl->btl_flags;
            } else if (19 < opal_output_get_verbosity(opal_btl_base_framework.framework_output)) {
                char *errhost = opal_get_proc_hostname(&proc->super);
                opal_output(0, "mca: bml: Not using %s btl for send to %s on node %s "
                            "because %s btl has higher exclusivity (%d > %d)",
                            btl->btl_component->btl_version.mca_component_name,
                            OMPI_NAME_PRINT(&proc->super.proc_name), errhost,
                            bml_btl->btl->btl_component->btl_version.mca_component_name,
                            bml_btl->btl->btl_exclusivity,
                            btl->btl_exclusivity);
                free(errhost);
            }

            btl_in_use = true;
        }
    }

    /* always add rdma endpoints if they support full rdma */
    if (((btl_in_use && (btl_flags & MCA_BTL_FLAGS_RDMA)) ||
         (btl_flags & (MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_ATOMIC_FOPS)) == (MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_ATOMIC_FOPS)) &&
        !((proc->super.proc_arch != ompi_proc_local_proc->super.proc_arch) &&
          (0 == (btl->btl_flags & MCA_BTL_FLAGS_HETEROGENEOUS_RDMA)))) {
        mca_bml_base_btl_t *bml_btl_rdma = mca_bml_base_btl_array_insert(&bml_endpoint->btl_rdma);

        bml_btl_rdma->btl = btl;
        bml_btl_rdma->btl_endpoint = btl_endpoint;
        bml_btl_rdma->btl_weight = 0;
        bml_btl_rdma->btl_flags = btl_flags;

        if (bml_endpoint->btl_pipeline_send_length < btl->btl_rdma_pipeline_send_length) {
            bml_endpoint->btl_pipeline_send_length = btl->btl_rdma_pipeline_send_length;
        }

        if (bml_endpoint->btl_send_limit < btl->btl_min_rdma_pipeline_size) {
            bml_endpoint->btl_send_limit = btl->btl_min_rdma_pipeline_size;
        }

        btl_in_use = true;
    }

    return btl_in_use ? OMPI_SUCCESS : OMPI_ERR_NOT_AVAILABLE;
}

static void mca_bml_r2_compute_endpoint_metrics (mca_bml_base_endpoint_t *bml_endpoint)
{
    double total_bandwidth = 0;
    uint32_t latency;
    size_t n_send, n_rdma;

    /* (1) determine the total bandwidth available across all btls
     *     note that we need to do this here, as we may already have btls configured
     * (2) determine the highest priority ranking for latency
     * (3) compute the maximum amount of bytes that can be send without any
     *     weighting. Once the left over is smaller than this number we will
     *     start using the weight to compute the correct amount.
     */
    n_send = mca_bml_base_btl_array_get_size (&bml_endpoint->btl_send);
    n_rdma = mca_bml_base_btl_array_get_size (&bml_endpoint->btl_rdma);

    /* sort BTLs in descending order according to bandwidth value */
    qsort (bml_endpoint->btl_send.bml_btls, n_send,
           sizeof(mca_bml_base_btl_t), btl_bandwidth_compare);

    bml_endpoint->btl_rdma_index = 0;

    mca_bml_r2_calculate_bandwidth_latency (&bml_endpoint->btl_send, &total_bandwidth, &latency);

    /* (1) set the weight of each btl as a percentage of overall bandwidth
     * (2) copy all btl instances at the highest priority ranking into the
     *     list of btls used for first fragments
     */
    for (size_t n_index = 0 ; n_index < n_send ; ++n_index) {
        mca_bml_base_btl_t *bml_btl =
            mca_bml_base_btl_array_get_index(&bml_endpoint->btl_send, n_index);
        mca_btl_base_module_t *btl = bml_btl->btl;

        /* compute weighting factor for this r2 */
        if(btl->btl_bandwidth > 0) {
            bml_btl->btl_weight = (float)(btl->btl_bandwidth / total_bandwidth);
        } else {
            bml_btl->btl_weight = (float)(1.0 / n_send);
        }

        /* check to see if this r2 is already in the array of r2s
         * used for first fragments - if not add it.
         */
        if(btl->btl_latency == latency) {
            mca_bml_base_btl_t* bml_btl_new =
                mca_bml_base_btl_array_insert(&bml_endpoint->btl_eager);
            *bml_btl_new = *bml_btl;
        }

        /* set endpoint max send size as min of available btls */
        if (bml_endpoint->btl_max_send_size > btl->btl_max_send_size)
            bml_endpoint->btl_max_send_size = btl->btl_max_send_size;
    }

    /* sort BTLs in descending order according to bandwidth value */
    qsort(bml_endpoint->btl_rdma.bml_btls, n_rdma,
          sizeof(mca_bml_base_btl_t), btl_bandwidth_compare);

    mca_bml_r2_calculate_bandwidth_latency (&bml_endpoint->btl_rdma, &total_bandwidth, &latency);

    /* set rdma btl weights */
    for (size_t n_index = 0 ; n_index < n_rdma ; ++n_index) {
        mca_bml_base_btl_t *bml_btl =
            mca_bml_base_btl_array_get_index(&bml_endpoint->btl_rdma, n_index);

        /* compute weighting factor for this r2 */
        if (bml_btl->btl->btl_bandwidth > 0.0) {
            bml_btl->btl_weight = (float)(bml_btl->btl->btl_bandwidth / total_bandwidth);
        } else {
            bml_btl->btl_weight = (float)(1.0 / n_rdma);
        }
    }
}

static int mca_bml_r2_add_procs(size_t nprocs,
                                struct ompi_proc_t **procs);

/*
 * SM indexes endpoints by local rank and polls one FIFO. A single-proc
 * add_proc leaves the other slots empty; the next fifo_read then
 * faults. When the target is node-local, wire every local proc
 * (including self) the way add_procs(world) used to.
 */
static int mca_bml_r2_add_local_procs(void)
{
    ompi_proc_t **locals;
    size_t nalloc = 0, nlocals = 0;
    int rc;

    /* Compacted in place: the array is ours, and the procs that survive
     * the filter keep their relative order. */
    locals = ompi_proc_get_allocated(&nalloc);
    if (NULL == locals) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (size_t i = 0; i < nalloc; ++i) {
        ompi_proc_t *p = locals[i];
        if (p != ompi_proc_local_proc &&
            !OPAL_PROC_ON_LOCAL_NODE(p->super.proc_flags)) {
            continue;
        }
        if (NULL != p->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML]) {
            continue;
        }
        /* Do not batch-wire a local peer whose connection blob is not
         * local yet. add_proc of that peer falls through to a
         * single-proc attempt, which returns NOT_READY. */
        if (p != ompi_proc_local_proc && !ompi_modex_proc_ready(p)) {
            continue;
        }
        /* Never wire a peer whose architecture could not be read: its
         * convertor would still be the local one. The blob is local at
         * this point, so this only trips on a hard failure. */
        if (OMPI_SUCCESS != ompi_proc_complete_init_single(p)) {
            continue;
        }
        locals[nlocals++] = p;
    }

    if (0 == nlocals) {
        free(locals);
        return OMPI_SUCCESS;
    }

    rc = mca_bml_r2_add_procs(nlocals, locals);
    free(locals);
    return rc;
}

static int mca_bml_r2_add_proc (struct ompi_proc_t *proc)
{
    int rc;

    if (OPAL_UNLIKELY(NULL == proc)) {
        return OMPI_ERR_BAD_PARAM;
    }

    /* Already wired. Take no reference: the one the endpoint owns was
     * taken when it was published. */
    if (NULL != proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML]) {
        return OMPI_SUCCESS;
    }

    /* The batch below walks the whole proc list, and the lazy path
     * retries this call from every progress tick until the peer's blob
     * lands. Nothing can come of the walk while the peer that asked for
     * it is not ready itself, so do not pay for it. */
    if (proc == ompi_proc_local_proc ||
        (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags) && ompi_modex_proc_ready(proc))) {
        rc = mca_bml_r2_add_local_procs();
        if (NULL != proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML]) {
            return OMPI_SUCCESS;
        }
        if (OMPI_SUCCESS != rc && OMPI_ERR_NOT_READY != rc) {
            return rc;
        }
        /* This local peer was not in the ready batch; try it alone
         * so the BTL can return NOT_READY for just this proc. */
    }

    return mca_bml_r2_add_procs (1, &proc);
}

/*
 *   For each proc setup a datastructure that indicates the BTLs
 *   that can be used to reach the destination.
 *
 */

/* Enough for the single-peer lazy wire-up, which runs on first contact
 * from the send path, to stay off the heap. */
#define MCA_BML_R2_ADD_PROCS_STATIC 8
/* new_procs, btl_endpoints, bml_endpoints */
#define MCA_BML_R2_ADD_PROCS_ARRAYS 3
/* One block holds the three pointer arrays and one walk-state byte per
 * proc, the bytes last so the arrays keep the block's alignment. */
#define MCA_BML_R2_ADD_PROCS_WORDS(nprocs)                                              \
    (MCA_BML_R2_ADD_PROCS_ARRAYS * (nprocs)                                             \
     + ((nprocs) + sizeof(void *) - 1) / sizeof(void *))

/** A btl of the tier this proc stopped at claimed it. Sticky: this is
 *  what keeps the proc from descending to the tiers below. */
#define MCA_BML_R2_PROC_CLAIMED 0x1
/** Some btl answered NO_INFO or CONNECTING for this proc, so the set of
 *  btls that will carry its traffic is not final yet. */
#define MCA_BML_R2_PROC_DEFERRED 0x2

/**
 * Exchange two procs, and everything the walk holds about them.
 */
static inline void mca_bml_r2_swap_procs (struct ompi_proc_t **procs,
                                          mca_bml_base_endpoint_t **endpoints,
                                          uint8_t *state, size_t a, size_t b)
{
    struct ompi_proc_t *proc = procs[a];
    mca_bml_base_endpoint_t *endpoint = endpoints[a];
    uint8_t proc_state = state[a];

    procs[a] = procs[b];
    endpoints[a] = endpoints[b];
    state[a] = state[b];

    procs[b] = proc;
    endpoints[b] = endpoint;
    state[b] = proc_state;
}

static int mca_bml_r2_add_procs( size_t nprocs,
                                 struct ompi_proc_t** procs )
{
    void *static_scratch[MCA_BML_R2_ADD_PROCS_WORDS(MCA_BML_R2_ADD_PROCS_STATIC)];
    void **scratch = static_scratch;
    struct ompi_proc_t **new_procs;
    struct mca_btl_base_endpoint_t **btl_endpoints;
    mca_bml_base_endpoint_t **bml_endpoints;
    uint8_t *proc_state;
    size_t n_new_procs = 0, n_walking, n_settled;
    opal_bitmap_t proc_status;
    bool saw_deferred = false, saw_unreach = false;
    int rc, ret = OMPI_SUCCESS;

    if(0 == nprocs) {
        return OMPI_SUCCESS;
    }

    /* The module array this walks is global and is built (and sorted)
     * right below, so two threads adding procs at once -- two
     * MPI_Comm_accept, or a first send racing one -- must not rebuild it
     * while the other iterates it. This is also what makes the
     * publication below the only writer of an endpoint at a time. */
    OPAL_THREAD_LOCK(&mca_bml_lock);

    if(OMPI_SUCCESS != (ret = mca_bml_r2_add_btls()) ) {
        goto release_arrays;
    }

    if (nprocs > MCA_BML_R2_ADD_PROCS_STATIC) {
        scratch = (void **) malloc(MCA_BML_R2_ADD_PROCS_WORDS(nprocs) * sizeof(*scratch));
        if (NULL == scratch) {
            scratch = static_scratch;
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto release_arrays;
        }
    }

    new_procs = (struct ompi_proc_t **) scratch;

    /* Select only the procs that don't yet have the BML proc struct. This prevent
     * us from calling btl->add_procs several times on the same destination proc.
     *
     * The reference taken here keeps the proc alive for the rest of this
     * call and then becomes the one the published endpoint owns, which
     * del_procs releases. Whatever is left unwired hands it back below,
     * so a retried call cannot accumulate references.
     */
    for (size_t p_index = 0 ; p_index < nprocs ; ++p_index) {
        struct ompi_proc_t* proc = procs[p_index];

        if(NULL !=  proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML]) {
            continue;  /* go to the next proc */
        }
        OBJ_RETAIN(proc);
        new_procs[n_new_procs++] = proc;
    }

    if ( 0 == n_new_procs ) {
        goto release_arrays;
    }

    /* Same count, same element size, so the one chunk holds all three.
     * Laying the other two out behind the selection rather than behind
     * nprocs keeps every array exactly as long as the range that was
     * just initialized. btl_endpoints is refilled per btl_add_procs()
     * call, below. */
    btl_endpoints = (struct mca_btl_base_endpoint_t **) (scratch + n_new_procs);
    bml_endpoints = (mca_bml_base_endpoint_t **) (scratch + 2 * n_new_procs);
    proc_state = (uint8_t *) (scratch + MCA_BML_R2_ADD_PROCS_ARRAYS * n_new_procs);
    memset(bml_endpoints, 0, n_new_procs * sizeof(*bml_endpoints));
    memset(proc_state, 0, n_new_procs * sizeof(*proc_state));

    /* What each btl has to say about each proc it was asked about. A btl
     * can hand back an endpoint for a proc it does not want to be
     * selected for, so this is not the same answer as a non-NULL
     * btl_endpoints[i]. */
    OBJ_CONSTRUCT(&proc_status, opal_bitmap_t);
    ret = opal_bitmap_init(&proc_status, MCA_BTL_PROC_STATUS_NBITS(n_new_procs));
    if (OMPI_SUCCESS != ret) {
        for (size_t p = 0 ; p < n_new_procs ; ++p) {
            OBJ_RELEASE(new_procs[p]);
        }
        OBJ_DESTRUCT(&proc_status);
        goto release_arrays;
    }

    /* new_procs is kept partitioned in three, so that every btl_add_procs()
     * call is one leading range of it and no gather is needed:
     *
     *   [0, n_walking)                       still descending the tiers
     *   [n_walking, n_walking + n_settled)   a tier took them
     *   the rest                             withheld, nobody sees them again
     *
     * A settled proc is done choosing who sends for it, but not who
     * fetches for it, which is why the second range is next to the
     * first rather than at the end.
     *
     * TODO: this still walks every module for every peer, including
     * intra-node BTLs for off-node ranks. A BTL-advertised LOCAL scope
     * would let those be skipped. */
    n_walking = n_new_procs;
    n_settled = 0;

    for (size_t p_index = 0 ; p_index < mca_bml_r2.num_btl_modules ; ) {
        uint32_t exclusivity = mca_bml_r2.btl_modules[p_index]->btl_exclusivity;
        size_t lo = 0, mid = 0, hi;
        size_t tier_end = p_index;

        /* Modules are sorted by descending exclusivity, so equal
         * exclusivity is one contiguous run. Exclusivity does not pick a
         * single winner: a whole tier stripes together, so the unit of
         * the walk is the tier, and a claim only closes the selection
         * once every module of the tier has had the proc offered to it. */
        while (tier_end < mca_bml_r2.num_btl_modules
               && mca_bml_r2.btl_modules[tier_end]->btl_exclusivity == exclusivity) {
            ++tier_end;
        }

        for ( ; p_index < tier_end ; ++p_index) {
            mca_btl_base_module_t *btl = mca_bml_r2.btl_modules[p_index];
            size_t n_try = n_walking;
            int btl_inuse = 0;

            /* A module that offers rdma with fetching atomics lands in
             * btl_rdma whichever tier won btl_send, so keep asking it
             * about the procs a higher tier has already settled: that is
             * how osc/rdma gets one accelerated btl for node-local and
             * remote peers alike, and stopping the walk at the winning
             * tier would quietly narrow btl_rdma to it. */
            if ((btl->btl_flags & (MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_ATOMIC_FOPS))
                == (MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_ATOMIC_FOPS)) {
                n_try += n_settled;
            }

            if (0 == n_try) {
                continue;
            }

            /* The btl records what it has to say about each of these
             * procs, and hands back addressing information for the ones
             * it claims, to be passed back to it on data transfer calls.
             */
            opal_bitmap_clear_all_bits(&proc_status);
            memset(btl_endpoints, 0, n_try * sizeof(*btl_endpoints));

            rc = btl->btl_add_procs(btl, n_try, (opal_proc_t**)new_procs, btl_endpoints,
                                    &proc_status);
            if (OMPI_SUCCESS != rc) {
                /* The call failed as a whole, so it has nothing to say
                 * about any of these procs. Carry on: another btl may
                 * well reach them. */
                continue;
            }

            for (size_t i = 0 ; i < n_try ; ++i) {
                int status = MCA_BTL_PROC_STATUS_GET(&proc_status, i);
                ompi_proc_t *proc = new_procs[i];

                if (!MCA_BTL_PROC_DECIDED(status)
                    || (MCA_BTL_PROC_CLAIMED(status) && !MCA_BTL_PROC_USABLE(status))) {
                    /* This btl either cannot answer yet or is still
                     * coming up. Either way it belongs in this proc's
                     * set of btls and is not in it yet, so withhold the
                     * proc: publishing without this btl commits us to a
                     * set the peer, which sees the same modules in the
                     * same order, may not have committed to. */
                    proc_state[i] |= MCA_BML_R2_PROC_DEFERRED;
                }

                if (MCA_BTL_PROC_CLAIMED(status)) {
                    proc_state[i] |= MCA_BML_R2_PROC_CLAIMED;
                }

                if (!MCA_BTL_PROC_USABLE(status)) {
                    continue;
                }

                if (NULL == bml_endpoints[i]) {
                    bml_endpoints[i] = mca_bml_r2_allocate_endpoint (proc);
                    if (NULL == bml_endpoints[i]) {
                        ret = OPAL_ERR_OUT_OF_RESOURCE;
                        goto publish;
                    }
                }

                rc = mca_bml_r2_endpoint_add_btl (proc, bml_endpoints[i], btl, btl_endpoints[i]);
                if (OMPI_SUCCESS != rc) {
                    btl->btl_del_procs(btl, 1, (opal_proc_t**)&proc, &btl_endpoints[i]);
                    continue;
                }

                /* This BTL is in use, allow the progress registration */
                btl_inuse++;
            }

            mca_bml_r2_register_progress (btl, !!(btl_inuse));
        }

        /* End of the tier: re-partition the procs it was asked about.
         * The ones it deferred go to the tail, where no module will see
         * them again; the ones it claimed follow the ones still walking.
         * Deferred wins over claimed, since a claim this incomplete is
         * exactly what has to be settled before anything is published.
         */
        hi = n_walking + n_settled;
        while (mid < hi) {
            if (proc_state[mid] & MCA_BML_R2_PROC_DEFERRED) {
                mca_bml_r2_swap_procs (new_procs, bml_endpoints, proc_state, mid, --hi);
            } else if (proc_state[mid] & MCA_BML_R2_PROC_CLAIMED) {
                ++mid;
            } else {
                mca_bml_r2_swap_procs (new_procs, bml_endpoints, proc_state, lo++, mid++);
            }
        }

        n_walking = lo;
        n_settled = hi - lo;

        if (0 == hi) {
            /* Every proc is either settled with nothing left to fetch
             * for it, or withheld. */
            break;
        }
    }

publish:
    OBJ_DESTRUCT(&proc_status);

    /* compute metrics for registered r2s */
    for (size_t p = 0; p < n_new_procs ; ++p) {
        if (NULL == bml_endpoints[p]) {
            continue;
        }

        if (proc_state[p] & MCA_BML_R2_PROC_DEFERRED) {
            /* Some btls wired this proc and another one is still working
             * on it. Drop the half-built endpoint -- not the btl
             * endpoints inside it, which the btls keep and hand back on
             * the next call -- so this proc is never seen with a set of
             * btls that is missing one. */
            OBJ_RELEASE(bml_endpoints[p]);
            bml_endpoints[p] = NULL;
            continue;
        }

        mca_bml_r2_compute_endpoint_metrics (bml_endpoints[p]);
    }

    /* Nothing above is reachable from the proc yet, so this one barrier
     * covers every endpoint published below: a reader that sees the
     * pointer sees a complete endpoint. */
    opal_atomic_wmb();

    for (size_t p = 0; p < n_new_procs ; ++p) {
        ompi_proc_t *proc = new_procs[p];

        if (NULL == bml_endpoints[p]) {
            /* Never wired, so hand back the reference taken above: no
             * endpoint will be there for del_procs to release it. */
            OBJ_RELEASE(proc);

            /* A btl that has not made up its mind about this proc is
             * worth calling again. So is a proc every btl turned down
             * while its connection info is still in flight: a btl that
             * reads the modex before answering should have said NO_INFO,
             * but one that reports NOT_FOUND as "not mine" -- a LOCAL
             * Get can still race the node GDS even once that peer's data
             * is otherwise local -- would otherwise have this proc
             * declared permanently unreachable. */
            if ((proc_state[p] & MCA_BML_R2_PROC_DEFERRED) || !ompi_modex_proc_ready(proc)) {
                saw_deferred = true;
            } else {
                saw_unreach = true;
                mca_bml_r2_show_unreach(proc);
            }
            continue;
        }

        if (OPAL_UNLIKELY(NULL != proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML])) {
            /* Another thread wired this peer while we were building.
             * Both endpoints wrap the same per-BTL endpoints, so drop
             * ours -- not the BTL's, which theirs is using -- along
             * with the reference it would have owned. */
            OBJ_RELEASE(bml_endpoints[p]);
            OBJ_RELEASE(proc);
            continue;
        }

        proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML] = bml_endpoints[p];
        opal_proc_learned(&proc->super, OPAL_PROC_FLAG_WIRED);
    }

    /* A proc some btl is still working on says come back; one that every
     * btl turned down says do not bother. Retrying wins when the batch
     * has both, since the caller that cares about the second one asks
     * for it by itself and gets the plain answer. */
    if (OMPI_SUCCESS == ret) {
        if (saw_deferred) {
            ret = OMPI_ERR_NOT_READY;
        } else if (saw_unreach) {
            ret = OMPI_ERR_UNREACH;
        }
    }

release_arrays:
    OPAL_THREAD_UNLOCK(&mca_bml_lock);

    if (scratch != static_scratch) {
        free(scratch);
    }

    return ret;
}

/*
 * iterate through each proc and notify any BTLs associated
 * with the proc that it is/has gone away
 */

static int mca_bml_r2_del_procs(size_t nprocs,
                                struct ompi_proc_t** procs)
{
    for (size_t p = 0 ; p < nprocs ; ++p) {
        ompi_proc_t *proc = procs[p];
        mca_bml_base_endpoint_t *bml_endpoint =
            (mca_bml_base_endpoint_t*) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];

        if (!bml_endpoint) {
            /* NTH: I would think this is a developer bug and should not be ignored. */
            continue;
        }

        /* notify each btl that the proc is going away */
        size_t f_size = mca_bml_base_btl_array_get_size (&bml_endpoint->btl_send);
        for (size_t f_index = 0 ; f_index < f_size ; ++f_index) {
            mca_bml_base_btl_t* bml_btl = mca_bml_base_btl_array_get_index(&bml_endpoint->btl_send, f_index);
            mca_btl_base_module_t *btl = bml_btl->btl;

            int rc = btl->btl_del_procs (btl, 1, (opal_proc_t **) &proc, &bml_btl->btl_endpoint);
            if (OPAL_SUCCESS != rc) {
                return rc;
            }

            /* The reference stored in btl_eager and btl_rdma will automatically
             * disappear once the btl_array destructor is called. Thus, there is
             * no need for extra cleaning here.
             */
        }

        /* some btl endpoints may only be in the btl_rdma array. call del_procs on those as well */
        size_t r_size = mca_bml_base_btl_array_get_size (&bml_endpoint->btl_rdma);
        for (size_t r_index = 0 ; r_index < r_size ; ++r_index) {
            mca_bml_base_btl_t *rdma_btl = mca_bml_base_btl_array_get_index (&bml_endpoint->btl_rdma, r_index);
            mca_btl_base_module_t *btl = rdma_btl->btl;
            bool needs_del = true;

            for (size_t f_index = 0 ; f_index < f_size ; ++f_index) {
                mca_bml_base_btl_t *bml_btl = mca_bml_base_btl_array_get_index (&bml_endpoint->btl_send, f_index);
                if (bml_btl->btl_endpoint == rdma_btl->btl_endpoint) {
                    needs_del = false;
                    break;
                }
            }

            if (needs_del) {
                int rc = btl->btl_del_procs (btl, 1, (opal_proc_t **) &proc, &rdma_btl->btl_endpoint);
                if (OPAL_SUCCESS != rc) {
                    return rc;
                }
            }
        }

        proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML] = NULL;
        opal_proc_forget(&proc->super, OPAL_PROC_FLAG_WIRED);

        /* release the bml endpoint's reference to the proc */
        OBJ_RELEASE(proc);

        /* do any required cleanup */
        OBJ_RELEASE(bml_endpoint);
    }

    return OMPI_SUCCESS;
}

static inline int bml_r2_remove_btl_progress(mca_btl_base_module_t* btl)
{
    unsigned int p;

    if(NULL == btl->btl_component->btl_progress) {
        return OMPI_SUCCESS;
    }
    for(p = 0; p < mca_bml_r2.num_btl_progress; p++) {
        if(btl->btl_component->btl_progress != mca_bml_r2.btl_progress[p])
            continue;
        opal_progress_unregister( btl->btl_component->btl_progress );
        if( p < (mca_bml_r2.num_btl_progress-1) ) {
            mca_bml_r2.btl_progress[p] = mca_bml_r2.btl_progress[mca_bml_r2.num_btl_progress-1];
        }
        mca_bml_r2.num_btl_progress--;
        return OMPI_SUCCESS;
    }
    return OMPI_ERR_NOT_FOUND;
}

static int mca_bml_r2_del_proc_btl(ompi_proc_t* proc, mca_btl_base_module_t* btl)
{
    mca_bml_base_endpoint_t* ep = (mca_bml_base_endpoint_t*)proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];
    mca_bml_base_btl_t* bml_btl;
    mca_btl_base_module_t* ep_btl;
    double total_bandwidth = 0;
    size_t b;

    if(NULL == ep)
        return OMPI_SUCCESS;

    /* remove btl from eager list */
    mca_bml_base_btl_array_remove(&ep->btl_eager, btl);

    /* remove btl from send list */
    if(mca_bml_base_btl_array_remove(&ep->btl_send, btl)) {

        /* compute total_bandwidth and
           reset max_send_size to the min of all btl's */
        total_bandwidth = 0;
        ep->btl_max_send_size = -1;
        for(b=0; b< mca_bml_base_btl_array_get_size(&ep->btl_send); b++) {
            bml_btl = mca_bml_base_btl_array_get_index(&ep->btl_send, b);
            ep_btl = bml_btl->btl;

            total_bandwidth += ep_btl->btl_bandwidth;
            if (ep->btl_max_send_size > ep_btl->btl_max_send_size) {
                ep->btl_max_send_size = ep_btl->btl_max_send_size;
            }
        }

        /* compute weighting factor for this btl */
        for(b=0; b< mca_bml_base_btl_array_get_size(&ep->btl_send); b++) {
            bml_btl = mca_bml_base_btl_array_get_index(&ep->btl_send, b);
            ep_btl = bml_btl->btl;

            if(ep_btl->btl_bandwidth > 0) {
                bml_btl->btl_weight = (float)(ep_btl->btl_bandwidth / total_bandwidth);
            } else {
                bml_btl->btl_weight = (float)(1.0 / mca_bml_base_btl_array_get_size(&ep->btl_send));
            }
        }
    }

    /* remove btl from RDMA list */
    if(mca_bml_base_btl_array_remove(&ep->btl_rdma, btl)) {

        /* compute total bandwidth */
        total_bandwidth = 0;
        ep->btl_pipeline_send_length = 0;
        ep->btl_send_limit = 0;
        for(b=0; b< mca_bml_base_btl_array_get_size(&ep->btl_rdma); b++) {
            bml_btl = mca_bml_base_btl_array_get_index(&ep->btl_rdma, b);
            ep_btl = bml_btl->btl;

            /* update aggregate endpoint info */
            total_bandwidth += ep_btl->btl_bandwidth;
            if (ep->btl_pipeline_send_length < ep_btl->btl_rdma_pipeline_send_length) {
                ep->btl_pipeline_send_length = ep_btl->btl_rdma_pipeline_send_length;
            }
            if (ep->btl_send_limit < ep_btl->btl_min_rdma_pipeline_size) {
                ep->btl_send_limit = ep_btl->btl_min_rdma_pipeline_size;
            }
        }

        /* compute weighting factor for this btl */
        for(b=0; b< mca_bml_base_btl_array_get_size(&ep->btl_rdma); b++) {
            bml_btl = mca_bml_base_btl_array_get_index(&ep->btl_rdma, b);
            ep_btl = bml_btl->btl;

            if(ep_btl->btl_bandwidth > 0) {
                bml_btl->btl_weight = (float)(ep_btl->btl_bandwidth / total_bandwidth);
            } else {
                bml_btl->btl_weight = (float)(1.0 / mca_bml_base_btl_array_get_size(&ep->btl_rdma));
            }
        }
    }

    return OMPI_SUCCESS;
}

int mca_bml_r2_finalize( void )
{
    ompi_proc_t** procs;
    size_t p, num_procs;
    opal_list_item_t* w_item;

    if (NULL != btl_names) {
        free(btl_names);
        btl_names = NULL;
    }

    /* Similar to mca_bml_r2_del_btl ... */
    procs = ompi_proc_all(&num_procs);
    if(NULL == procs)
        goto CLEANUP;

    for (w_item =  opal_list_get_first(&mca_btl_base_modules_initialized);
         w_item != opal_list_get_end(&mca_btl_base_modules_initialized);
         w_item =  opal_list_get_next(w_item)) {
        mca_btl_base_selected_module_t *sm = (mca_btl_base_selected_module_t *) w_item;
        mca_btl_base_module_t* btl = sm->btl_module;

        /* unregister the BTL progress function if any */
        bml_r2_remove_btl_progress(btl);

        /* dont use this btl for any peers */
        for( p = 0; p < num_procs; p++ ) {
            ompi_proc_t* proc = procs[p];
            mca_bml_r2_del_proc_btl(proc, sm->btl_module);
        }
    }
    /* Release the procs as the ompi_proc_all increase their ref_count */
    for( p = 0; p < num_procs; p++ ) {
        OBJ_RELEASE(procs[p]);
    }
    free(procs);

 CLEANUP:
    mca_bml_r2.num_btl_modules = 0;
    mca_bml_r2.num_btl_progress = 0;
    /* The btl_modules array being freed below is rebuilt (from
       mca_btl_base_modules_initialized) by mca_bml_r2_add_btls(), which is a
       no-op while btls_added is true -- so the flag must fall with the state
       it guards.  It was previously reset only in mca_bml_r2_component_init(),
       which cannot be relied upon to run again: mca_bml_base_init() skips
       component init while the bml framework is merely *held* rather than
       closed and reopened, and MPI_T's framework registration holds every
       framework across MPI session cycles.  Leaving the flag set while the
       array is empty made the next session's add_procs attach zero BTLs and
       fail (unreachable), even though the BTL modules themselves were still
       alive. */
    mca_bml_r2.btls_added = false;

    if( NULL != mca_bml_r2.btl_modules) {
        free(mca_bml_r2.btl_modules);
        mca_bml_r2.btl_modules = NULL;
    }
    if( NULL != mca_bml_r2.btl_progress ) {
        free(mca_bml_r2.btl_progress);
        mca_bml_r2.btl_progress = NULL;
    }

    /* Do not close the BTL base here; the BML upper layer will take
       care of that. */

    return OMPI_SUCCESS;
}


/*
 *  (1) Remove btl from each bml endpoint
 *  (2) Remove btl from the global list
 */

static int mca_bml_r2_del_btl(mca_btl_base_module_t* btl)
{
    ompi_proc_t** procs;
    size_t i, m, p, num_procs;
    opal_list_item_t* item;
    mca_btl_base_module_t** modules;
    bool found = false;

    if(opal_list_get_size(&mca_btl_base_modules_initialized) == 2) {
        opal_output(0, "only one BTL left, can't failover");
        return OMPI_SUCCESS;
    }

    procs = ompi_proc_all(&num_procs);
    if(NULL == procs)
        return OMPI_SUCCESS;

    /* Get rid of the associated progress function */
    bml_r2_remove_btl_progress(btl);

    /* dont use this btl for any peers */
    for( p = 0; p < num_procs; p++ ) {
        ompi_proc_t* proc = procs[p];
        mca_bml_r2_del_proc_btl(proc, btl);
    }

    /* remove from the btl list */
    for (item =  opal_list_get_first(&mca_btl_base_modules_initialized);
         item != opal_list_get_end(&mca_btl_base_modules_initialized);
         item =  opal_list_get_next(item)) {
        mca_btl_base_selected_module_t *sm = (mca_btl_base_selected_module_t *) item;
        if(sm->btl_module == btl) {
            opal_list_remove_item(&mca_btl_base_modules_initialized, item);
            free(sm);
            found = true;
            break;
        }
    }
    if(!found) {
        /* doesn't even exist */
        goto CLEANUP;
    }
    /* remove from bml list */
    modules = (mca_btl_base_module_t**)malloc(sizeof(mca_btl_base_module_t*) * (mca_bml_r2.num_btl_modules-1));
    for(i=0,m=0; i<mca_bml_r2.num_btl_modules; i++) {
        if(mca_bml_r2.btl_modules[i] != btl) {
            modules[m++] = mca_bml_r2.btl_modules[i];
        }
    }
    free(mca_bml_r2.btl_modules);
    mca_bml_r2.btl_modules = modules;
    mca_bml_r2.num_btl_modules = m;

    /* cleanup */
    btl->btl_finalize(btl);
CLEANUP:
    /* Decrease the ref_count increased by the call to ompi_proc_all */
    for( p = 0; p < num_procs; p++ ) {
        OBJ_RELEASE(procs[p]);
    }
    free(procs);
    return OMPI_SUCCESS;
}

static int mca_bml_r2_add_btl(mca_btl_base_module_t* btl)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


/*
 *  Register callback w/ all active btls
 */
static int mca_bml_r2_register( mca_btl_base_tag_t tag,
                                mca_btl_base_module_recv_cb_fn_t cbfunc,
                                void* data )
{
    int rc;

    /* Builds the global module array and then walks it, so it takes the
     * same lock add_procs does. */
    OPAL_THREAD_LOCK(&mca_bml_lock);

    rc = mca_bml_r2_add_btls();
    if (OMPI_SUCCESS != rc) {
        goto done;
    }

    mca_btl_base_active_message_trigger[tag].cbfunc = cbfunc;
    mca_btl_base_active_message_trigger[tag].cbdata = data;
    /* Give an opportunity to the BTLs to do something special
     * for each registration.
     */
    for (uint32_t i = 0; i < mca_bml_r2.num_btl_modules; i++) {
        mca_btl_base_module_t *btl = mca_bml_r2.btl_modules[i];

        if (NULL == btl->btl_register) {
            continue;
        }
        rc = btl->btl_register(btl, tag, cbfunc, data);
        if (OMPI_SUCCESS != rc) {
            goto done;
        }
    }

done:
    OPAL_THREAD_UNLOCK(&mca_bml_lock);

    return rc;
}


/*
 *  Register an error handler with/ all active btls
 *   if they support error handlers..
 */

static int mca_bml_r2_register_error( mca_btl_base_module_error_cb_fn_t  cbfunc)
{
    uint32_t  i;
    int rc;
    mca_btl_base_module_t *btl;
    uint32_t ver;

    for(i = 0; i < mca_bml_r2.num_btl_modules; i++) {
        btl = mca_bml_r2.btl_modules[i];
        /* this wont work for version numbers greater than 256... seems
           reasonable.. */
        ver = btl->btl_component->btl_version.mca_type_major_version << 16 |
            btl->btl_component->btl_version.mca_type_minor_version << 8 |
            btl->btl_component->btl_version.mca_type_release_version;
        /* is version number greater than or equal to 1.0.1? */
        if(ver >= ((1 << 16) |  (0 << 8) | 1) &&
           NULL != btl->btl_register_error) {
            rc = btl->btl_register_error(btl, cbfunc);
            if(OMPI_SUCCESS != rc) {
                return rc;
            }
        }
    }
    return OMPI_SUCCESS;
}


int mca_bml_r2_component_fini(void)
{
    return OMPI_SUCCESS;
}

mca_bml_r2_module_t mca_bml_r2 = {
    .super = {
        .bml_component = &mca_bml_r2_component,
        .bml_add_proc = mca_bml_r2_add_proc,
        .bml_add_procs = mca_bml_r2_add_procs,
        .bml_del_procs = mca_bml_r2_del_procs,
        .bml_add_btl = mca_bml_r2_add_btl,
        .bml_del_btl = mca_bml_r2_del_btl,
        .bml_del_proc_btl = mca_bml_r2_del_proc_btl,
        .bml_register = mca_bml_r2_register,
        .bml_register_error = mca_bml_r2_register_error,
        .bml_finalize = mca_bml_r2_finalize,
    },
};

