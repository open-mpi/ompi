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

#ifndef MCA_BTL_SCIF_H
#define MCA_BTL_SCIF_H

#include "opal_config.h"

#include "opal/mca/mpool/mpool.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/mpool/grdma/mpool_grdma.h"
#include "opal/util/output.h"
#include "opal_stdint.h"
#include "opal/util/proc.h"

#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/btl/base/btl_base_error.h"

#include <scif.h>
#include <errno.h>
#include <stdint.h>
#include <sys/types.h>
#include <assert.h>
#include <sys/time.h>

/* Turn on timers for debug builds */
#if OPAL_ENABLE_DEBUG
/* #define SCIF_TIMING */
#endif

#if defined(SCIF_TIMING)
#include <sys/time.h>
#include <math.h>

static inline void timerspecsub (struct timespec *end, struct timespec *start,
                                 struct timespec *diff) {
    diff->tv_nsec = end->tv_nsec - start->tv_nsec;
    diff->tv_sec  = end->tv_sec - start->tv_sec;
    if (diff->tv_nsec < 0) {
        --diff->tv_sec;
        diff->tv_nsec += 1000000000;
    }
}

#define SCIF_UPDATE_TIMER(agg, max, start)                              \
    do {                                                                \
        struct timespec _te, _diff;                                     \
        double _tmpd;                                                   \
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &_te);                  \
        timerspecsub(&_te, &(start), &_diff);                           \
        _tmpd = (double) _diff.tv_sec + (double) _diff.tv_nsec / 1000000000.0; \
        (agg) += _tmpd;                                                 \
        (max) = fmax ((max), _tmpd);                                    \
    } while (0)
#endif

typedef struct mca_btl_scif_modex_t {
    struct scif_portID port_id;
} mca_btl_scif_modex_t;

typedef struct mca_btl_scif_module_t {
    mca_btl_base_module_t super;

    /* listening endpoint */
    scif_epd_t scif_fd;

    /* listening port */
    struct scif_portID port_id;

    size_t endpoint_count;
    struct mca_btl_base_endpoint_t *endpoints;

    opal_list_t failed_frags;

    /* fragments for DMA */
    opal_free_list_t dma_frags;

    /* fragments for eager send */
    opal_free_list_t eager_frags;

    pthread_t listen_thread;

    volatile bool exiting;
    bool listening;
} mca_btl_scif_module_t;

typedef struct mca_btl_scif_component_t {
    /* base BTL component */
    mca_btl_base_component_3_0_0_t super;

    /* DMA free list settings */
    int scif_free_list_num;
    int scif_free_list_max;
    int scif_free_list_inc;

    unsigned int segment_size;

    bool rma_use_cpu;
    bool rma_sync;

#if defined(SCIF_TIMING)
    /* performance timers */
    double aquire_buffer_time;
    double aquire_buffer_time_max;

    double send_time;
    double send_time_max;

    double sendi_time;
    double sendi_time_max;

    double get_time;
    double get_time_max;
    unsigned long get_count;

    double put_time;
    double put_time_max;
    unsigned long put_count;
#endif
} mca_btl_scif_component_t;

int mca_btl_scif_module_init (void);

/**
 * BML->BTL notification of change in the process list. 
 *
 * location: btl_scif_add_procs.c
 *
 * @param btl (IN)            BTL module
 * @param nprocs (IN)         Number of processes
 * @param procs (IN)          Array of processes
 * @param endpoint (OUT)      Array of mca_btl_base_endpoint_t structures by BTL.
 * @param reachable (OUT)     Bitmask indicating set of peer processes that are reachable by this BTL.
 * @return                    OPAL_SUCCESS or error status on failure.
 */
int
mca_btl_scif_add_procs (struct mca_btl_base_module_t* btl,
                        size_t nprocs,
                        struct opal_proc_t **procs,
                        struct mca_btl_base_endpoint_t **peers,
                        opal_bitmap_t *reachable);

/**
 * Notification of change to the process list.
 *
 * location: btl_scif_add_procs.c
 *
 * @param btl (IN)     BTL module
 * @param nprocs (IN)  Number of processes
 * @param proc (IN)    Set of processes
 * @param peer (IN)    Set of peer addressing information.
 * @return             Status indicating if cleanup was successful
 */
int
mca_btl_scif_del_procs (struct mca_btl_base_module_t *btl,
                        size_t nprocs,
                        struct opal_proc_t **procs,
                        struct mca_btl_base_endpoint_t **peers);

/**
 * Initiate an asynchronous send.
 *
 * location: btl_scif_send.c
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */
int
mca_btl_scif_send (struct mca_btl_base_module_t *btl,
                   struct mca_btl_base_endpoint_t *btl_peer,
                   struct mca_btl_base_descriptor_t *descriptor,
                   mca_btl_base_tag_t tag);

int mca_btl_scif_sendi (struct mca_btl_base_module_t *btl,
                        struct mca_btl_base_endpoint_t *endpoint,
                        struct opal_convertor_t *convertor,
                        void *header, size_t header_size,
                        size_t payload_size, uint8_t order,
                        uint32_t flags, mca_btl_base_tag_t tag,
                        mca_btl_base_descriptor_t **descriptor);

/**
 * Initiate a get operation.
 *
 * location: btl_scif_get.c
 */
int mca_btl_scif_get (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                      int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

/**
 * Initiate a put operation.
 *
 * location: btl_scif_put.c
 */
int mca_btl_scif_put (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                      int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

mca_btl_base_descriptor_t *
mca_btl_scif_alloc(struct mca_btl_base_module_t *btl,
                   struct mca_btl_base_endpoint_t *endpoint,
                   uint8_t order, size_t size, uint32_t flags);

int mca_btl_scif_progress_send_wait_list (struct mca_btl_base_endpoint_t *endpoint);

struct mca_btl_scif_reg_t;

struct mca_btl_base_registration_handle_t {
    /** scif offset */
    off_t scif_offset;
    /** base address of this scif region */
    uintptr_t scif_base;
};

struct mca_btl_scif_registration_handle_t {
    mca_btl_base_registration_handle_t btl_handle;
    struct mca_btl_scif_reg_t *reg;
};
typedef struct mca_btl_scif_registration_handle_t mca_btl_scif_registration_handle_t;

typedef struct mca_btl_scif_reg_t {
    mca_mpool_base_registration_t base;
    /** per-endpoint btl handles for this registration */
    mca_btl_scif_registration_handle_t *handles;
} mca_btl_scif_reg_t;

/* Global structures */ 

OPAL_MODULE_DECLSPEC extern mca_btl_scif_component_t mca_btl_scif_component;
OPAL_MODULE_DECLSPEC extern mca_btl_scif_module_t mca_btl_scif_module;

#endif
