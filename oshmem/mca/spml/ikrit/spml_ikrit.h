/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
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

#ifndef MCA_SPML_UD_MXM_H
#define MCA_SPML_UD_MXM_H

#include "oshmem_config.h"
#include "oshmem/request/request.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/spml/base/spml_base_putreq.h"
#include "oshmem/proc/proc.h"
#include "oshmem/mca/spml/base/spml_base_request.h"
#include "oshmem/mca/spml/base/spml_base_getreq.h"

#include "ompi/mca/bml/base/base.h" 
#include "ompi/class/ompi_free_list.h" 
#include "opal/class/opal_list.h"

#include "orte/runtime/orte_globals.h"

#include <mxm/api/mxm_api.h>
#include <mxm/api/mxm_addr.h>
#include <mxm/api/mxm_stats.h>

#ifndef MXM_VERSION
#define MXM_VERSION(major, minor) (((major)<<MXM_MAJOR_BIT)|((minor)<<MXM_MINOR_BIT))
#endif


#define MXM_SHMEM_MQ_ID 0x7119
#define MXM_SHMEM_TAG   0x7

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
    int                 pe;
    uint32_t            n_active_puts;
    int                 need_fence;
    /* if >= 0, data will be send to pe_relay which will forward it to destination pe */
    int                 pe_relay;   
    uint64_t            dst_va; /* virtual address on the final destination */
    int                 n_slaves;      
    int                 pe_relays[16];
    int                 n_relays;
};

typedef struct mxm_peer mxm_peer_t;
OBJ_CLASS_DECLARATION(mxm_peer_t);

struct mca_spml_ikrit_t {
    mca_spml_base_module_t super; 

    mxm_h                 mxm_context;
    mxm_ep_h              mxm_ep;
    mxm_mq_h              mxm_mq;
    mxm_peer_t            **mxm_peers;

    uint32_t n_active_puts;
    uint32_t n_active_gets;
    uint32_t n_mxm_fences;

    int priority;           /* component priority */
    int free_list_num;      /* initial size of free list */
    int free_list_max;      /* maximum size of free list */
    int free_list_inc;      /* number of elements to grow free list */

    bool enabled;
    opal_list_t           active_peers;
    int n_relays;           /* number of procs/node serving as relays */
};

typedef struct mca_spml_ikrit_t mca_spml_ikrit_t; 

typedef struct spml_ikrit_mxm_ep_conn_info_t {
        struct sockaddr_storage  ptl_addr[MXM_PTL_LAST];
} spml_ikrit_mxm_ep_conn_info_t;


extern mca_spml_ikrit_t mca_spml_ikrit;

extern int mca_spml_ikrit_enable( bool enable );
extern int mca_spml_ikrit_get(void* dst_addr, size_t size, void* src_addr, int src); 
/* extension. used 4 fence implementation b4 fence was added to mxm */
extern int mca_spml_ikrit_get_async(void *src_addr, size_t size, void *dst_addr, int src);

extern int mca_spml_ikrit_put(void* dst_addr, size_t size, void* src_addr, int dst);
extern int mca_spml_ikrit_put_nb(void* dst_addr, size_t size, void* src_addr, int dst, void **handle);

extern int mca_spml_ikrit_recv(void* buf, size_t size, int src);
extern int mca_spml_ikrit_send(void* buf, size_t size, int dst, mca_spml_base_put_mode_t mode);

extern mca_spml_mkey_t *mca_spml_ikrit_register(void* addr, size_t size, uint64_t shmid, int *count);
extern int mca_spml_ikrit_deregister(mca_spml_mkey_t *mkeys);
extern int mca_spml_ikrit_oob_get_mkeys(int pe, uint32_t seg, mca_spml_mkey_t *mkeys);

extern int mca_spml_ikrit_add_procs(oshmem_proc_t** procs, size_t nprocs);
extern int mca_spml_ikrit_del_procs(oshmem_proc_t** procs, size_t nprocs);
extern int mca_spml_ikrit_fence(void);
extern int spml_ikrit_progress(void);

static inline oshmem_proc_t *mca_spml_ikrit_proc_find(int dst)
{
    orte_process_name_t name;
   
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    name.vpid  = dst;
    return oshmem_proc_find(&name);
}


END_C_DECLS

#endif

