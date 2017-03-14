/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/*
 * The ugni btl is implemented with native Cray Gemini.
 *
 * Known issues with ugni:
 *  -
 */

#ifndef MCA_BTL_UGNI_H
#define MCA_BTL_UGNI_H

#include "opal_config.h"

#include "opal/mca/mpool/mpool.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/mca/rcache/udreg/rcache_udreg.h"
#include "opal/util/output.h"
#include "opal_stdint.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_free_list.h"

#include <errno.h>
#include <stdint.h>
#include <sys/types.h>
#include <assert.h>
#include <sys/time.h>
#include <gni_pub.h>

/* datagram message ids */
#define MCA_BTL_UGNI_CONNECT_WILDCARD_ID 0x0000000000000000ull
#define MCA_BTL_UGNI_CONNECT_DIRECTED_ID 0x8000000000000000ull
#define MCA_BTL_UGNI_DATAGRAM_MASK       0x8000000000000000ull

/** maximum number of supported virtual devices */
#define MCA_BTL_UGNI_MAX_DEV_HANDLES 128

/** number of rdma completion queue items to remove per progress loop */
#define MCA_BTL_UGNI_COMPLETIONS_PER_LOOP 16

/** how often to check for connection requests */
#define MCA_BTL_UGNI_CONNECT_USEC 10

/**
 * Modex data
 */
struct mca_btl_ugni_modex_t {
    /** GNI NIC address */
    uint32_t addr;
    /** CDM identifier (base) */
    int      id;
};
typedef struct mca_btl_ugni_modex_t mca_btl_ugni_modex_t;

/* ompi and smsg endpoint attributes */
typedef struct mca_btl_ugni_endpoint_attr_t {
    opal_process_name_t proc_name;
    uint32_t index;
    gni_smsg_attr_t smsg_attr;
    gni_mem_handle_t rmt_irq_mem_hndl;
} mca_btl_ugni_endpoint_attr_t;

enum {
    MCA_BTL_UGNI_RCACHE_UDREG,
    MCA_BTL_UGNI_RCACHE_GRDMA
};

enum mca_btl_ugni_free_list_id_t {
    /* eager fragment list (registered) */
    MCA_BTL_UGNI_LIST_EAGER_SEND,
    MCA_BTL_UGNI_LIST_EAGER_RECV,
    /* SMSG fragment list (unregistered) */
    MCA_BTL_UGNI_LIST_SMSG,
    /* RDMA fragment list */
    MCA_BTL_UGNI_LIST_RDMA,
    MCA_BTL_UGNI_LIST_RDMA_INT,
    MCA_BTL_UGNI_LIST_MAX,
};

struct mca_btl_ugni_cq_t {
    /** ugni CQ handle */
    gni_cq_handle_t gni_handle;
    /** number of completions expected on the CQ */
    int32_t active_operations;
};
typedef struct mca_btl_ugni_cq_t mca_btl_ugni_cq_t;

/**
 * GNI virtual device
 */
struct mca_btl_ugni_device_t {
    /** Communication domain handle */
    gni_cdm_handle_t dev_cd_handle;

    /** protection for ugni access */
    volatile int32_t lock;

    /** Index of device in module devices array */
    int dev_index;

    /** number of SMSG connections */
    volatile int32_t smsg_connections;

    /** uGNI device handle */
    gni_nic_handle_t dev_handle;

    /** uGNI rdma completion queue */
    mca_btl_ugni_cq_t dev_rdma_local_cq;

    /** local rdma completion queue (async) */
    mca_btl_ugni_cq_t dev_rdma_local_irq_cq;

    /** local SMSG completion queue */
    mca_btl_ugni_cq_t dev_smsg_local_cq;

    /** IRQ memory handle for this device */
    gni_mem_handle_t smsg_irq_mhndl;

    /** RDMA endpoint free list */
    opal_free_list_t endpoints;

    /** post descriptors pending resources */
    opal_list_t pending_post;
};
typedef struct mca_btl_ugni_device_t mca_btl_ugni_device_t;

typedef intptr_t (*mca_btl_ugni_device_serialize_fn_t) (mca_btl_ugni_device_t *device, void *arg);

typedef struct mca_btl_ugni_module_t {
    mca_btl_base_module_t super;

    bool initialized;

    mca_btl_ugni_device_t devices[MCA_BTL_UGNI_MAX_DEV_HANDLES];

    opal_mutex_t endpoint_lock;
    size_t endpoint_count;
    opal_pointer_array_t endpoints;
    opal_hash_table_t id_to_endpoint;

    /* lock for this list */
    opal_mutex_t     failed_frags_lock;
    /** rdma frags waiting to be reposted */
    opal_list_t failed_frags;

    /** lock for the eager_get_pending list */
    opal_mutex_t eager_get_pending_lock;
    opal_list_t eager_get_pending;

    opal_free_list_t post_descriptors;

    mca_mpool_base_module_t *mpool;
    opal_free_list_t         smsg_mboxes;

    gni_ep_handle_t wildcard_ep;
    struct mca_btl_base_endpoint_t *local_ep;

    volatile int32_t active_datagrams;
    opal_event_t connection_event;

    struct mca_btl_ugni_endpoint_attr_t wc_remote_attr, wc_local_attr;

    gni_cq_handle_t smsg_remote_cq;
    gni_cq_handle_t smsg_remote_irq_cq;

    /** fragment free lists (see enum mca_btl_ugni_free_list_id_t) */
    opal_free_list_t frags_lists[MCA_BTL_UGNI_LIST_MAX];

    /* lock for this list */
    opal_mutex_t     ep_wait_list_lock;
    /* endpoints waiting on credits */
    opal_list_t      ep_wait_list;

    /* fragment id bounce buffer (smsg msg ids are only 32 bits) */
    opal_pointer_array_t pending_smsg_frags_bb;

    int32_t reg_max;
    volatile int32_t reg_count;

    /* used to calculate the fraction of registered memory resources
     * this rank should be limited too */
    int nlocal_procs;

    volatile int active_send_count;
    volatile int64_t connected_peer_count;
    volatile int64_t active_rdma_count;

    mca_rcache_base_module_t *rcache;
} mca_btl_ugni_module_t;

typedef struct mca_btl_ugni_component_t {
    /* base BTL component */
    mca_btl_base_component_3_0_0_t super;

    /* maximum supported btls. hardcoded to 1 for now */
    uint32_t ugni_max_btls;
    /* Maximum number of entries a completion queue can hold */
    uint32_t remote_cq_size;
    uint32_t local_cq_size;

    /* number of ugni modules */
    uint32_t ugni_num_btls;
    /* ugni modules */
    mca_btl_ugni_module_t *modules;

    size_t smsg_max_data;

    /* After this message size switch to BTE protocols */
    size_t ugni_fma_limit;
    /* Switch to get when sending above this size */
    size_t ugni_smsg_limit;

    /* RDMA/SMSG free list settings */
    int ugni_free_list_num;
    int ugni_free_list_max;
    int ugni_free_list_inc;

    /* eager free list settings */
    int ugni_eager_num;
    int ugni_eager_max;
    int ugni_eager_inc;

    int smsg_max_retries;
    /* number of times to retry a post */
    int rdma_max_retries;

    /* Maximum number of outstanding eager messages */
    int smsg_max_credits;
    /* mailbox size (computed) */
    int smsg_mbox_size;

    /* Maximum number of memory registrations per process */
    int max_mem_reg;

    /* Page size to use for SMSG allocations (udreg mpool) */
    unsigned int smsg_page_size;

    /* rcache type (grdma or udreg) */
    int rcache_type;

    /* memory pool hints */
    char *mpool_hints;

    /* Number of mailboxes to allocate in each block */
    unsigned int mbox_increment;

    /* Indicate whether progress thread requested */
    bool progress_thread_requested;

    /* Indicate whether progress thread allowed */
    bool progress_thread_enabled;

    /** Number of ugni device contexts to create per GNI device */
    int virtual_device_count;

    /** Protection tag */
    uint8_t ptag;

    /** Unique id for this process assigned by the system */
    uint32_t cookie;

    /** Starting value of communication identifier */
    uint32_t cdm_id_base;

    /** GNI CDM flags */
    uint32_t cdm_flags;

    /** NIC address */
    uint32_t dev_addr;
} mca_btl_ugni_component_t;

/* Global structures */

OPAL_MODULE_DECLSPEC extern mca_btl_ugni_component_t mca_btl_ugni_component;
OPAL_MODULE_DECLSPEC extern mca_btl_ugni_module_t mca_btl_ugni_module;

/**
 * Get a virtual device for communication
 */
static inline mca_btl_ugni_device_t *mca_btl_ugni_ep_get_device (mca_btl_ugni_module_t *ugni_module)
{
    static volatile uint32_t device_index = (uint32_t) 0;
    uint32_t dev_index;

    /* don't really care if the device index is atomically updated */
    dev_index = (device_index++) & (mca_btl_ugni_component.virtual_device_count - 1);

    return ugni_module->devices + dev_index;
}

static inline int mca_btl_rc_ugni_to_opal (gni_return_t rc)
{
    static int codes[] = {OPAL_SUCCESS,
                          OPAL_ERR_RESOURCE_BUSY,
                          OPAL_ERR_BAD_PARAM,
                          OPAL_ERR_OUT_OF_RESOURCE,
                          OPAL_ERR_TIMEOUT,
                          OPAL_ERR_PERM,
                          OPAL_ERROR,
                          OPAL_ERR_BAD_PARAM,
                          OPAL_ERR_BAD_PARAM,
                          OPAL_ERR_NOT_FOUND,
                          OPAL_ERR_VALUE_OUT_OF_BOUNDS,
                          OPAL_ERROR,
                          OPAL_ERR_NOT_SUPPORTED,
                          OPAL_ERR_OUT_OF_RESOURCE};
    return codes[rc];
}

/**
 * BML->BTL notification of change in the process list.
 *
 * location: btl_ugni_add_procs.c
 *
 * @param btl (IN)            BTL module
 * @param nprocs (IN)         Number of processes
 * @param procs (IN)          Array of processes
 * @param endpoint (OUT)      Array of mca_btl_base_endpoint_t structures by BTL.
 * @param reachable (OUT)     Bitmask indicating set of peer processes that are reachable by this BTL.
 * @return                    OPAL_SUCCESS or error status on failure.
 */
int
mca_btl_ugni_add_procs (struct mca_btl_base_module_t* btl,
                        size_t nprocs,
                        struct opal_proc_t **procs,
                        struct mca_btl_base_endpoint_t **peers,
                        opal_bitmap_t *reachable);

/**
 * Notification of change to the process list.
 *
 * location: btl_ugni_add_procs.c
 *
 * @param btl (IN)     BTL module
 * @param nprocs (IN)  Number of processes
 * @param proc (IN)    Set of processes
 * @param peer (IN)    Set of peer addressing information.
 * @return             Status indicating if cleanup was successful
 */
int
mca_btl_ugni_del_procs (struct mca_btl_base_module_t *btl,
                        size_t nprocs,
                        struct opal_proc_t **procs,
                        struct mca_btl_base_endpoint_t **peers);

struct mca_btl_base_endpoint_t *mca_btl_ugni_get_ep (struct mca_btl_base_module_t *module, opal_proc_t *proc);

/**
 * Initiate an asynchronous send.
 *
 * location: btl_ugni_send.c
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */
int
mca_btl_ugni_send (struct mca_btl_base_module_t *btl,
                   struct mca_btl_base_endpoint_t *btl_peer,
                   struct mca_btl_base_descriptor_t *descriptor,
                   mca_btl_base_tag_t tag);

/**
 * Initiate an immediate blocking send.
 *
 * location: btl_ugni_sendi.c
 *
 * @param btl (IN)             BTL module
 * @param endpoint (IN)        BTL addressing information
 * @param convertor (IN)       Data type convertor
 * @param header (IN)          Pointer to header.
 * @param header_size (IN)     Size of header.
 * @param payload_size (IN)    Size of payload (from convertor).
 * @param order (IN)           The ordering tag (may be MCA_BTL_NO_ORDER)
 * @param flags (IN)           Flags.
 * @param tag (IN)             The tag value used to notify the peer.
 * @param descriptor (OUT)     The descriptor to be returned unable to be sent immediately
 */
int
mca_btl_ugni_sendi (struct mca_btl_base_module_t *btl,
                    struct mca_btl_base_endpoint_t *endpoint,
                    struct opal_convertor_t *convertor,
                    void *header, size_t header_size,
                    size_t payload_size, uint8_t order,
                    uint32_t flags, mca_btl_base_tag_t tag,
                    mca_btl_base_descriptor_t **descriptor);

int mca_btl_ugni_get (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                      int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

int mca_btl_ugni_put (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                      int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

int mca_btl_ugni_aop (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *remote_handle,
                      mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                      mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

int mca_btl_ugni_afop (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                       void *local_address, uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                       mca_btl_base_registration_handle_t *remote_handle, mca_btl_base_atomic_op_t op,
                       uint64_t operand, int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                       void *cbcontext, void *cbdata);

int mca_btl_ugni_acswap (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                         void *local_address, uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                         mca_btl_base_registration_handle_t *remote_handle, uint64_t compare, uint64_t value,
                         int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

int mca_btl_ugni_progress_send_wait_list (struct mca_btl_base_endpoint_t *endpoint);
int mca_btl_ugni_progress_datagram (mca_btl_ugni_device_t *device);

mca_btl_base_descriptor_t *
mca_btl_ugni_alloc(struct mca_btl_base_module_t *btl,
                   struct mca_btl_base_endpoint_t *endpoint,
                   uint8_t order, size_t size, uint32_t flags);

struct mca_btl_base_registration_handle_t {
    /** uGNI memory handle */
    gni_mem_handle_t gni_handle;
};

typedef struct mca_btl_ugni_reg_t {
    mca_rcache_base_registration_t base;
    mca_btl_base_registration_handle_t handle;
} mca_btl_ugni_reg_t;

/**
 * Initialize uGNI support.
 */
int mca_btl_ugni_init (void);

/**
 * Finalize uGNI support.
 */
int mca_btl_ugni_fini (void);

int mca_btl_ugni_module_init (mca_btl_ugni_module_t *ugni_module);

/**
 * Intialize a virtual device for device index 0.
 *
 * @param[inout] device         Device to initialize
 * @param[in] virtual_device_id Virtual device identified (up to max handles)
 */
int mca_btl_ugni_device_init (mca_btl_ugni_device_t *device, int virtual_device_id);

/**
 * Finalize a virtual device.
 *
 * @param[in] device Device to finalize
 */
int mca_btl_ugni_device_fini (mca_btl_ugni_device_t *dev);

/* Get a unique 64-bit id for the process name */
static inline uint64_t mca_btl_ugni_proc_name_to_id (opal_process_name_t name) {
    /* Throw away the top bit of the jobid for the datagram type */
    return ((uint64_t) (name.jobid & 0x7fffffff) << 32 | name.vpid);
}

int mca_btl_ugni_spawn_progress_thread(struct mca_btl_base_module_t* btl);
int mca_btl_ugni_kill_progress_thread(void);

/**
 * Try to lock a uGNI device for exclusive access
 */
static inline int mca_btl_ugni_device_trylock (mca_btl_ugni_device_t *device)
{
    /* checking the lock non-atomically first can reduce the number of
     * unnecessary atomic operations. */
    return (device->lock || opal_atomic_swap_32 (&device->lock, 1));
}

/**
 * Lock a uGNI device for exclusive access
 */
static inline void mca_btl_ugni_device_lock (mca_btl_ugni_device_t *device)
{
    while (mca_btl_ugni_device_trylock (device));
}

/**
 * Release exclusive access to the device
 */
static inline void mca_btl_ugni_device_unlock (mca_btl_ugni_device_t *device)
{
    opal_atomic_wmb ();
    device->lock = 0;
}

/**
 * Serialize an operation on a uGNI device
 *
 * @params[in] device ugni device
 * @params[in] fn     function to serialize
 * @params[in] arg    function argument
 */
static inline intptr_t mca_btl_ugni_device_serialize (mca_btl_ugni_device_t *device,
                                                      mca_btl_ugni_device_serialize_fn_t fn, void *arg)
{
    intptr_t rc;

    if (!opal_using_threads ()) {
        return fn (device, arg);
    }

    /* NTH: for now the device is just protected by a spin lock but this will change in the future */
    mca_btl_ugni_device_lock (device);
    rc = fn (device, arg);
    mca_btl_ugni_device_unlock (device);
    return rc;
}


/** Number of times the progress thread has woken up */
extern unsigned int mca_btl_ugni_progress_thread_wakeups;

#endif
