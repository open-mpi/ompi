/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 *
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_SM_TYPES_H
#define MCA_BTL_SM_TYPES_H

#include "opal_config.h"
#include "opal/class/opal_free_list.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/smsc/smsc.h"

/*
 * Modex data
 */
struct mca_btl_sm_modex_t {
    uint64_t segment_base;
    int seg_ds_size;
    /* seg_ds needs to be the last element */
    opal_shmem_ds_t seg_ds;
};

typedef struct mca_btl_sm_modex_t mca_btl_sm_modex_t;

typedef struct mca_btl_base_endpoint_t {
    opal_list_item_t super;

    /* per peer buffers */
    struct {
        unsigned char *buffer; /**< starting address of peer's fast box out */
        uint32_t *startp;
        unsigned int start;
        uint16_t seq;
    } fbox_in;

    struct {
        unsigned char *buffer; /**< starting address of peer's fast box in */
        uint32_t *startp;      /**< pointer to location storing start offset */
        unsigned int start, end;
        uint16_t seq;
        opal_free_list_item_t *fbox; /**< fast-box free list item */
    } fbox_out;

    uint16_t peer_smp_rank;        /**< my peer's SMP process rank.  Used for accessing
                                    *   SMP specific data structures. */
    opal_atomic_size_t send_count; /**< number of fragments sent to this peer */
    char *segment_base;            /**< start of the peer's segment (in the address space
                                    *   of this process) */

    struct sm_fifo_t *fifo; /**< */

    opal_mutex_t lock; /**< lock to protect endpoint structures from concurrent
                        *   access */

    mca_smsc_endpoint_t *smsc_endpoint;
    void *smsc_map_context;
    opal_shmem_ds_t *seg_ds; /**< stored segment information for detach */

    opal_mutex_t pending_frags_lock; /**< protect pending_frags */
    opal_list_t pending_frags;       /**< fragments pending fast box space */
    bool waiting;                    /**< endpoint is on the component wait list */
} mca_btl_base_endpoint_t;

typedef mca_btl_base_endpoint_t mca_btl_sm_endpoint_t;

OBJ_CLASS_DECLARATION(mca_btl_sm_endpoint_t);

/**
 * Shared Memory (SM) BTL module.
 */
struct mca_btl_sm_component_t {
    mca_btl_base_component_3_0_0_t super; /**< base BTL component */
    int sm_free_list_num;                 /**< initial size of free lists */
    int sm_free_list_max;                 /**< maximum size of free lists */
    int sm_free_list_inc; /**< number of elements to alloc when growing free lists */
    opal_shmem_ds_t seg_ds; /**< this rank's shared memory segment (when not using xpmem) */

    opal_mutex_t lock;     /**< lock to protect concurrent updates to this structure's members */
    char *my_segment;      /**< this rank's base pointer */
    size_t segment_size;   /**< size of my_segment */
    int32_t num_smp_procs; /**< current number of smp procs on this host */
    opal_free_list_t sm_frags_eager;    /**< free list of sm send frags */
    opal_free_list_t sm_frags_max_send; /**< free list of sm max send frags (large fragments) */
    opal_free_list_t sm_frags_user;     /**< free list of small inline frags */
    opal_free_list_t sm_fboxes;         /**< free list of available fast-boxes */

    unsigned int
        fbox_threshold; /**< number of sends required before we setup a send fast box for a peer */
    unsigned int fbox_max;  /**< maximum number of send fast boxes to allocate */
    unsigned int fbox_size; /**< size of each peer fast box allocation */

    int single_copy_mechanism; /**< single copy mechanism to use */

    int memcpy_limit;             /**< Limit where we switch from memmove to memcpy */
    unsigned int max_inline_send; /**< Limit for copy-in-copy-out fragments */

    mca_btl_base_endpoint_t
        *endpoints; /**< array of local endpoints (one for each local peer including myself) */
    mca_btl_base_endpoint_t **fbox_in_endpoints; /**< array of fast box in endpoints */
    unsigned int num_fbox_in_endpoints;          /**< number of fast boxes to poll */
    struct sm_fifo_t *my_fifo;                   /**< pointer to the local fifo */

    opal_list_t pending_endpoints; /**< list of endpoints with pending fragments */
    opal_list_t pending_fragments; /**< fragments pending remote completion */

    char *backing_directory; /**< directory to place shared memory backing files */

    mca_mpool_base_module_t *mpool;
};
typedef struct mca_btl_sm_component_t mca_btl_sm_component_t;

/**
 * SM BTL Interface
 */
struct mca_btl_sm_t {
    mca_btl_base_module_t super; /**< base BTL interface */
    bool btl_inited;             /**< flag indicating if btl has been inited */
    mca_btl_base_module_error_cb_fn_t error_cb;
};
typedef struct mca_btl_sm_t mca_btl_sm_t;

enum {
    MCA_BTL_SM_FLAG_INLINE = 0,
    MCA_BTL_SM_FLAG_SINGLE_COPY = 1,
    MCA_BTL_SM_FLAG_COMPLETE = 2,
    MCA_BTL_SM_FLAG_SETUP_FBOX = 4,
};

struct mca_btl_sm_frag_t;
struct mca_btl_sm_fbox_t;

enum mca_btl_sm_sc_emu_type_t {
    MCA_BTL_SM_OP_PUT,
    MCA_BTL_SM_OP_GET,
    MCA_BTL_SM_OP_ATOMIC,
    MCA_BTL_SM_OP_CSWAP,
};
typedef enum mca_btl_sm_sc_emu_type_t mca_btl_sm_sc_emu_type_t;

struct mca_btl_sm_sc_emu_hdr_t {
    mca_btl_sm_sc_emu_type_t type;
    uint64_t addr;
    mca_btl_base_atomic_op_t op;
    int flags;
    int64_t operand[2];
};
typedef struct mca_btl_sm_sc_emu_hdr_t mca_btl_sm_sc_emu_hdr_t;

/**
 * FIFO fragment header
 */
struct mca_btl_sm_hdr_t {
    /** next item in fifo. many peers may touch this */
    volatile intptr_t next;
    /** pointer back the the fragment */
    struct mca_btl_sm_frag_t *frag;
    /** tag associated with this fragment (used to lookup callback) */
    mca_btl_base_tag_t tag;
    /** sm send flags (inline, complete, setup fbox, etc) */
    uint8_t flags;
    /** length of data following this header */
    int32_t len;
    /** io vector containing pointer to single-copy data */
    struct iovec sc_iov;
    /** if the fragment indicates to setup a fast box the base is stored here */
    intptr_t fbox_base;
};
typedef struct mca_btl_sm_hdr_t mca_btl_sm_hdr_t;

/**
 * shared memory send fragment derived type.
 */
struct mca_btl_sm_frag_t {
    /** base object */
    mca_btl_base_descriptor_t base;
    /** storage for segment data (max 2) */
    mca_btl_base_segment_t segments[2];
    /** endpoint this fragment is active on */
    struct mca_btl_base_endpoint_t *endpoint;
    /** fragment header (in the shared memory region) */
    mca_btl_sm_hdr_t *hdr;
    /** free list this fragment was allocated within */
    opal_free_list_t *my_list;
    /** rdma callback data */
    struct mca_btl_sm_rdma_cbdata_t {
        void *local_address;
        uint64_t remote_address;
        mca_btl_base_rdma_completion_fn_t cbfunc;
        void *context;
        void *cbdata;
        size_t remaining;
        size_t sent;
    } rdma;
};
typedef struct mca_btl_sm_frag_t mca_btl_sm_frag_t;

OBJ_CLASS_DECLARATION(mca_btl_sm_frag_t);

/** FIFO types **/
typedef opal_atomic_intptr_t atomic_fifo_value_t;
typedef intptr_t fifo_value_t;

/* lock free fifo */
struct sm_fifo_t {
    atomic_fifo_value_t fifo_head;
    atomic_fifo_value_t fifo_tail;
    opal_atomic_int32_t fbox_available;
};
typedef struct sm_fifo_t sm_fifo_t;

#endif /* MCA_BTL_SM_TYPES_H */
