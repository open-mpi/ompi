/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 *  @file 
 */

#ifndef MCA_SPML_UD_MXM_H
#define MCA_SPML_UD_MXM_H

#include "oshmem_config.h"
#include "oshmem/request/request.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/util/oshmem_util.h"
#include "oshmem/mca/spml/base/spml_base_putreq.h"
#include "oshmem/proc/proc.h"
#include "oshmem/mca/spml/base/spml_base_request.h"
#include "oshmem/mca/spml/base/spml_base_getreq.h"

#include "ompi/mca/bml/base/base.h" 
#include "opal/class/opal_free_list.h"
#include "opal/class/opal_list.h"

#include "orte/runtime/orte_globals.h"

#include <mxm/api/mxm_api.h>

#ifndef MXM_VERSION
#define MXM_VERSION(major, minor) (((major)<<MXM_MAJOR_BIT)|((minor)<<MXM_MINOR_BIT))
#endif

#if MXM_API < MXM_VERSION(2,0)
#include <mxm/api/mxm_addr.h>
#include <mxm/api/mxm_stats.h>
#endif

#define MXM_SHMEM_MQ_ID 0x7119

/* start request explicit ack once our buffer pool is less than watermark */
#define SPML_IKRIT_PUT_LOW_WATER    16
/* request explicit ack (SYNC) per every X put requests per connection */
#define SPML_IKRIT_PACKETS_PER_SYNC  64

BEGIN_C_DECLS

/**
 * UD MXM SPML module
 */
struct mxm_peer {
    opal_list_item_t    super;
    mxm_conn_h          mxm_conn;
    mxm_conn_h          mxm_hw_rdma_conn;
    int                 pe;
    int32_t             n_active_puts;
    int                 need_fence;
};

typedef struct mxm_peer mxm_peer_t;
OBJ_CLASS_DECLARATION(mxm_peer_t);

struct mca_spml_ikrit_t {
    mca_spml_base_module_t super;

    mxm_context_opts_t *mxm_ctx_opts;
    mxm_ep_opts_t *mxm_ep_opts;
    mxm_ep_opts_t *mxm_ep_hw_rdma_opts;
    mxm_h mxm_context;
    mxm_ep_h mxm_ep;
    mxm_ep_h mxm_hw_rdma_ep;
    mxm_mq_h mxm_mq;
    mxm_peer_t **mxm_peers;

    int32_t n_active_puts;
    int32_t n_active_gets;
    int32_t n_mxm_fences;

    int priority; /* component priority */
    int free_list_num; /* initial size of free list */
    int free_list_max; /* maximum size of free list */
    int free_list_inc; /* number of elements to grow free list */
    int bulk_connect; /* use bulk connect */
    int bulk_disconnect; /* use bulk disconnect */

    bool enabled;
    opal_list_t active_peers;
    int n_relays; /* number of procs/node serving as relays */

    char *mxm_tls;
    int   ud_only;  /* only ud transport is used. In this case 
                       it is possible to speedup mkey exchange 
                       and not to register memheap */
    int hw_rdma_channel;  /* true if we provide separate channel that
                       has true one sided capability */
    int np;
#if MXM_API >= MXM_VERSION(2,0)
    int unsync_conn_max;
#endif
};

typedef struct mca_spml_ikrit_t mca_spml_ikrit_t;

#define MXM_MAX_ADDR_LEN 512

#if MXM_API >= MXM_VERSION(2,0)
#define MXM_PTL_SHM  0
#define MXM_PTL_RDMA 1
#define MXM_PTL_LAST 2
#endif

typedef struct spml_ikrit_mxm_ep_conn_info_t {
    union {
        struct sockaddr_storage  ptl_addr[MXM_PTL_LAST];
        char ep_addr[MXM_MAX_ADDR_LEN];
    } addr;
} spml_ikrit_mxm_ep_conn_info_t;

extern mca_spml_ikrit_t mca_spml_ikrit;

extern int mca_spml_ikrit_enable(bool enable);
extern int mca_spml_ikrit_get(void* dst_addr,
                              size_t size,
                              void* src_addr,
                              int src);
/* extension. used 4 fence implementation b4 fence was added to mxm */
extern int mca_spml_ikrit_get_async(void *src_addr,
                                    size_t size,
                                    void *dst_addr,
                                    int src);

extern int mca_spml_ikrit_put(void* dst_addr,
                              size_t size,
                              void* src_addr,
                              int dst);
extern int mca_spml_ikrit_put_nb(void* dst_addr,
                                 size_t size,
                                 void* src_addr,
                                 int dst,
                                 void **handle);

extern int mca_spml_ikrit_recv(void* buf, size_t size, int src);
extern int mca_spml_ikrit_send(void* buf,
                               size_t size,
                               int dst,
                               mca_spml_base_put_mode_t mode);

extern sshmem_mkey_t *mca_spml_ikrit_register(void* addr,
                                                size_t size,
                                                uint64_t shmid,
                                                int *count);
extern int mca_spml_ikrit_deregister(sshmem_mkey_t *mkeys);
extern int mca_spml_ikrit_oob_get_mkeys(int pe,
                                        uint32_t seg,
                                        sshmem_mkey_t *mkeys);

extern int mca_spml_ikrit_add_procs(oshmem_proc_t** procs, size_t nprocs);
extern int mca_spml_ikrit_del_procs(oshmem_proc_t** procs, size_t nprocs);
extern int mca_spml_ikrit_fence(void);
extern int spml_ikrit_progress(void);

static inline oshmem_proc_t *mca_spml_ikrit_proc_find(int dst)
{
    orte_process_name_t name;

    name.jobid = ORTE_PROC_MY_NAME->jobid;
    name.vpid = dst;
    return oshmem_proc_find(&name);
}

END_C_DECLS

#endif

