/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 *  @file
 */

#ifndef MCA_SPML_YODA_H
#define MCA_SPML_YODA_H

#include "oshmem_config.h"
#include "oshmem/request/request.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/util/oshmem_util.h"
#include "oshmem/mca/spml/base/spml_base_putreq.h"
#include "oshmem/proc/proc.h"
#include "oshmem/mca/spml/base/spml_base_request.h"
#include "oshmem/mca/spml/base/spml_base_getreq.h"

#include "orte/runtime/orte_globals.h"

#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/class/ompi_free_list.h"

/* Turn ON/OFF debug output from build (default 0) */
#ifndef OSHMEM_WAIT_COMPLETION_DEBUG
#define OSHMEM_WAIT_COMPLETION_DEBUG    0
#endif

#define MCA_SPML_YODA_PUT (MCA_BTL_TAG_USR + 0x0A)
#define MCA_SPML_YODA_GET (MCA_BTL_TAG_USR + 0x0B)
#define MCA_SPML_YODA_GET_RESPONSE (MCA_BTL_TAG_USR + 0x0C)

#define SPML_YODA_SEND_CONTEXT_SIZE (sizeof(size_t) + 3*sizeof(void*) + sizeof(int))
BEGIN_C_DECLS

/**
 * YODA SPML module
 */

enum {
    YODA_BTL_UNKNOWN = -1,
    YODA_BTL_SELF = 0,
    YODA_BTL_SM,
    YODA_BTL_OPENIB,
    YODA_BTL_MAX
};

struct yoda_btl {
    mca_btl_base_module_t *btl;
    mca_bml_base_btl_t *bml_btl;
    int btl_type;
    int use_cnt;
};

struct mca_spml_yoda_t {
    mca_spml_base_module_t super;

    int priority;
    int free_list_num; /* initial size of free list */
    int free_list_max; /* maximum size of free list */
    int free_list_inc; /* number of elements to grow free list */
    int bml_alloc_threshold; /* number of puts to wait
        in case of put/get temporary buffer allocation failture */

    /* lock queue access */
    opal_mutex_t lock;

    /* free lists */
    ompi_free_list_t rdma_frags;
    /* number of outstanding put requests */
    int32_t n_active_puts;
    int32_t n_active_gets;
    bool enabled;
    struct yoda_btl *btl_type_map;
    int n_btls;
};
typedef struct mca_spml_yoda_t mca_spml_yoda_module_t;

struct mca_spml_yoda_context_t {
    mca_btl_base_descriptor_t* btl_src_descriptor;
    mca_mpool_base_registration_t* registration;
};
typedef struct mca_spml_yoda_context_t mca_spml_yoda_context_t;

extern mca_spml_yoda_module_t mca_spml_yoda;

extern int mca_spml_yoda_enable(bool enable);
extern int mca_spml_yoda_get(void* dst_addr,
                             size_t size,
                             void* src_addr,
                             int src);
extern int mca_spml_yoda_put(void* dst_addr,
                             size_t size,
                             void* src_addr,
                             int dst);
extern int mca_spml_yoda_put_nb(void* dst_addr,
                                size_t size,
                                void* src_addr,
                                int dst,
                                void **handle);
extern int mca_spml_yoda_recv(void* buf, size_t size, int src);
extern int mca_spml_yoda_send(void* buf,
                              size_t size,
                              int dst,
                              mca_spml_base_put_mode_t mode);
extern sshmem_mkey_t *mca_spml_yoda_register(void* addr,
                                               size_t size,
                                               uint64_t shmid,
                                               int *count);
extern int mca_spml_yoda_deregister(sshmem_mkey_t *mkeys);
extern int mca_spml_yoda_add_procs(oshmem_proc_t** procs,
                                   size_t nprocs);
extern int mca_spml_yoda_del_procs(oshmem_proc_t** procs,
                                   size_t nprocs);
extern int mca_spml_yoda_fence(void);
extern void* mca_spml_yoda_get_remote_context(void*);
extern void mca_spml_yoda_set_remote_context(void**, void*);
extern int mca_spml_yoda_get_remote_context_size(void*);
extern void mca_spml_yoda_set_remote_context_size(void**, int);
extern int mca_spml_yoda_wait_gets(void);

#if OSHMEM_WAIT_COMPLETION_DEBUG == 1
extern void condition_dbg_init(void);
extern void condition_dbg_finalize(void);
#endif

END_C_DECLS

#endif

