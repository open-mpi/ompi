/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_PORTALS_H_HAS_BEEN_INCLUDED
#define BTL_PORTALS_H_HAS_BEEN_INCLUDED

#include <portals4.h>
#include <btl_portals4_frag.h>

#include "opal/class/opal_free_list.h"
#include "opal/class/opal_list.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/btl/base/btl_base_error.h"

BEGIN_C_DECLS

/*
 * Portals BTL component.
 */
struct mca_btl_portals4_component_t {
    /* base BTL component */
    mca_btl_base_component_2_0_0_t super;

    unsigned int num_btls;
    unsigned int max_btls; /* Maximum number of accepted Portals4 cards */

    struct mca_btl_portals4_module_t** btls; /* array of available BTL modules */

    /* add_procs() can get called multiple times.  this prevents multiple calls to portals4_init_interface(). */
    int need_init;

    /* Use the logical to physical table to accelerate portals4 adressing: 1 (true) : 0 (false) */
    int use_logical;

    /* initial size of free lists */
    int portals_free_list_init_num;
    /* max size of free lists */
    int portals_free_list_max_num;
    /* numer of elements to grow free lists */
    int portals_free_list_inc_num;

    /* number of eager fragments */
    int portals_free_list_eager_max_num;

    /* do I need a portals ACK? */
    int portals_need_ack;

    /** Length of the receive event queues */
    int recv_queue_size;

    /* number outstanding sends and local rdma */
    int32_t portals_max_outstanding_ops;

    /* incoming send message receive memory descriptors */
    int portals_recv_mds_num;
    int portals_recv_mds_size;

    /** Event queue handles table used in PtlEQPoll */
    ptl_handle_eq_t *eqs_h;

    /** Upper limit for message sizes */
    unsigned long portals_max_msg_size;
};

typedef struct mca_btl_portals4_component_t mca_btl_portals4_component_t;

struct mca_btl_portals4_module_t {
    /* base BTL module interface */
    mca_btl_base_module_t super;

    /* number of processes we're actively connected to.  Needed to
       know when to do activation / shutdown */
    int32_t portals_num_procs;

    /* number of the interface (btl) */
    uint32_t interface_num;

    /* fragment free lists */
    opal_free_list_t portals_frag_eager;
    opal_free_list_t portals_frag_max;
    opal_free_list_t portals_frag_user;

    opal_list_t portals_recv_blocks;

    /** Length of the receive event queues */
    int recv_queue_size;

    /** Event queue handle */
    ptl_handle_eq_t recv_eq_h;

    /* number outstanding sends and local rdma */
    volatile int32_t portals_outstanding_ops;
    int32_t portals_max_outstanding_ops;

    /* key to use for next rdma operation */
    volatile int64_t portals_rdma_key;

    /* our portals network interface */
    ptl_handle_ni_t portals_ni_h;

    /** portals index */
    ptl_pt_index_t recv_idx;

    /** MD handle for sending ACKS */
    ptl_handle_md_t zero_md_h;

    /** Send MD handle */
    ptl_handle_md_t send_md_h;

    /** long message receive overflow ME.  Persistent ME, first in
        overflow list on the recv_idx portal table. */
    ptl_handle_me_t long_overflow_me_h;
};

typedef struct mca_btl_portals4_module_t mca_btl_portals4_module_t;

/* match/ignore bit manipulation
 *
 * 0123 4567 01234567 01234567 01234567 01234567 01234567 01234567 01234567
 *     |             |                 |
 * ^   | context id  |      source     |            message tag
 * |   |             |                 |
 * +---- protocol
 */

#define BTL_PORTALS4_PROTOCOL_MASK  0xF000000000000000ULL
#define BTL_PORTALS4_CONTEXT_MASK   0x0FFF000000000000ULL
#define BTL_PORTALS4_SOURCE_MASK    0x0000FFFF00000000ULL
#define BTL_PORTALS4_TAG_MASK       0x00000000FFFFFFFFULL

#define BTL_PORTALS4_PROTOCOL_IGNR  BTL_PORTALS4_PROTOCOL_MASK
#define BTL_PORTALS4_CONTEXT_IGNR   BTL_PORTALS4_CONTEXT_MASK
#define BTL_PORTALS4_SOURCE_IGNR    BTL_PORTALS4_SOURCE_MASK
#define BTL_PORTALS4_TAG_IGNR       0x000000007FFFFFFFULL

#define BTL_PORTALS4_SHORT_MSG      0x1000000000000000ULL
#define BTL_PORTALS4_LONG_MSG       0x2000000000000000ULL

/* send posting */
#define BTL_PORTALS4_SET_SEND_BITS(match_bits, contextid, source, tag, type) \
    {                                                                   \
        match_bits = contextid;                                         \
        match_bits = (match_bits << 16);                                \
        match_bits |= source;                                           \
        match_bits = (match_bits << 32);                                \
        match_bits |= (BTL_PORTALS4_TAG_MASK & tag) | type;             \
    }

#define BTL_PORTALS4_SET_HDR_DATA(hdr_data, opcount, length, sync)   \
    {                                                                \
        hdr_data = (sync) ? 1 : 0;                                   \
        hdr_data = (hdr_data << 15);                                 \
        hdr_data |= opcount & 0x7FFFULL;                             \
        hdr_data = (hdr_data << 48);                                 \
        hdr_data |= (length & 0xFFFFFFFFFFFFULL);                    \
    }

#define REQ_BTL_TABLE_ID	2

int mca_btl_portals4_component_progress(void);
void mca_btl_portals4_free_module(mca_btl_portals4_module_t *portals4_btl);

/* BTL interface functions */
int mca_btl_portals4_finalize(struct mca_btl_base_module_t* btl_base);


int mca_btl_portals4_add_procs(struct mca_btl_base_module_t* btl_base,
                              size_t nprocs,
                              struct opal_proc_t **procs,
                              struct mca_btl_base_endpoint_t** peers,
                              opal_bitmap_t* reachable);

int mca_btl_portals4_del_procs(struct mca_btl_base_module_t* btl_base,
                              size_t nprocs,
                              struct opal_proc_t **procs,
                              struct mca_btl_base_endpoint_t** peers);

mca_btl_base_descriptor_t*
mca_btl_portals4_alloc(struct mca_btl_base_module_t* btl_base,
                      struct mca_btl_base_endpoint_t* endpoint,
                      uint8_t order,
                      size_t size,
                      uint32_t flags);

int mca_btl_portals4_free(struct mca_btl_base_module_t* btl_base,
                         mca_btl_base_descriptor_t* des);

mca_btl_base_descriptor_t*
mca_btl_portals4_prepare_src(struct mca_btl_base_module_t* btl_base,
                            struct mca_btl_base_endpoint_t* peer,
                            struct opal_convertor_t* convertor,
                            uint8_t order,
                            size_t reserve,
                            size_t* size,
                            uint32_t flags);

int mca_btl_portals4_send(struct mca_btl_base_module_t* btl_base,
                         struct mca_btl_base_endpoint_t* btl_peer,
                         struct mca_btl_base_descriptor_t* descriptor,
                         mca_btl_base_tag_t tag);


int mca_btl_portals4_sendi(struct mca_btl_base_module_t* btl_base,
                          struct mca_btl_base_endpoint_t* endpoint,
                          struct opal_convertor_t* convertor,
                          void* header,
                          size_t header_size,
                          size_t payload_size,
                          uint8_t order,
                          uint32_t flags,
                          mca_btl_base_tag_t tag,
                          mca_btl_base_descriptor_t** des);

int mca_btl_portals4_put(struct mca_btl_base_module_t* btl_base,
                        struct mca_btl_base_endpoint_t* btl_peer,
                        struct mca_btl_base_descriptor_t* decriptor);


int mca_btl_portals4_get(struct mca_btl_base_module_t* btl_base,
                        struct mca_btl_base_endpoint_t* btl_peer,
                        void *local_address,
                        uint64_t remote_address,
                        struct mca_btl_base_registration_handle_t *local_handle,
                        struct mca_btl_base_registration_handle_t *remote_handle,
                        size_t size,
                        int flags,
                        int order,
                        mca_btl_base_rdma_completion_fn_t cbfunc,
                        void *cbcontext,
                        void *cbdata);

int mca_btl_portals4_get_error(int ptl_error);

struct mca_btl_base_registration_handle_t {
    /** Portals4 match bits */
    ptl_match_bits_t key;
    /** Portals4 me_h */
    ptl_handle_me_t me_h;
    /** Remote offset */
    ptl_size_t remote_offset;
};

/*
 * global structures
 */
OPAL_MODULE_DECLSPEC extern mca_btl_portals4_component_t mca_btl_portals4_component;
extern mca_btl_portals4_module_t mca_btl_portals4_module;

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */
struct mca_btl_base_endpoint_t {
    ptl_process_t ptl_proc;
};
typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;

END_C_DECLS

#endif  /* BTL_PORTALS_H_HAS_BEEN_INCLUDED */
