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
 * Copyright (c) 2010-2014 Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_VADER_H
#define MCA_BTL_VADER_H

#include "ompi_config.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif  /* HAVE_STDINT_H */
#ifdef HAVE_SCHED_H
# include <sched.h>
#endif  /* HAVE_SCHED_H */
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif /* HAVE_UNISTD_H */

#if OMPI_BTL_VADER_HAVE_XPMEM
  #if defined(HAVE_XPMEM_H)
    #include <xpmem.h>

    typedef struct xpmem_addr xpmem_addr_t;
  #elif defined(HAVE_SN_XPMEM_H)
    #include <sn/xpmem.h>

    typedef int64_t xpmem_segid_t;
    typedef int64_t xpmem_apid_t;
  #endif
#else
#include "opal/mca/shmem/base/base.h"
#endif

#include "opal/class/opal_free_list.h"
#include "opal/sys/atomic.h"

#include "ompi/mca/rte/rte.h"

#include "ompi/mca/btl/btl.h"

#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/btl/base/base.h"

#include "ompi/runtime/ompi_module_exchange.h"

#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"

#include "btl_vader_endpoint.h"

BEGIN_C_DECLS

#define min(a,b) ((a) < (b) ? (a) : (b))

/*
 * Shared Memory resource managment
 */

struct vader_fifo_t;

/*
 * Modex data
 */
struct vader_modex_t {
#if OMPI_BTL_VADER_HAVE_XPMEM
    xpmem_segid_t seg_id;
    void *segment_base;
#else
    char buffer[8192];
#endif
};

/**
 * Shared Memory (VADER) BTL module.
 */
struct mca_btl_vader_component_t {
    mca_btl_base_component_2_0_0_t super;   /**< base BTL component */
    int vader_free_list_num;                /**< initial size of free lists */
    int vader_free_list_max;                /**< maximum size of free lists */
    int vader_free_list_inc;                /**< number of elements to alloc
                                             * when growing free lists */
#if OMPI_BTL_VADER_HAVE_XPMEM
    xpmem_segid_t my_seg_id;                /* this rank's xpmem segment id */
#else
    opal_shmem_ds_t seg_ds;                 /* this rank's shared memory segment */
#endif

    char *my_segment;                       /* this rank's base pointer */
    size_t segment_size;                    /* size of my_segment */
    size_t segment_offset;                  /* start of unused portion of my_segment */
    int32_t num_smp_procs;                  /**< current number of smp procs on this host */
    ompi_free_list_t vader_frags_eager;     /**< free list of vader send frags */
#if !OMPI_BTL_VADER_HAVE_XPMEM
    ompi_free_list_t vader_frags_max_send;
#endif
    ompi_free_list_t vader_frags_user;      /**< free list of vader put/get frags */

    int memcpy_limit;                       /** Limit where we switch from memmove to memcpy */
    int log_attach_align;                   /** Log of the alignment for xpmem segments */
    unsigned int max_inline_send;           /** Limit for copy-in-copy-out fragments */

    struct mca_btl_base_endpoint_t *endpoints;
    struct vader_fifo_t *my_fifo;
};
typedef struct mca_btl_vader_component_t mca_btl_vader_component_t;
OMPI_MODULE_DECLSPEC extern mca_btl_vader_component_t mca_btl_vader_component;

/**
 * VADER BTL Interface
 */
struct mca_btl_vader_t {
    mca_btl_base_module_t  super;       /**< base BTL interface */
    bool btl_inited;  /**< flag indicating if btl has been inited */
    mca_btl_base_module_error_cb_fn_t error_cb;
};
typedef struct mca_btl_vader_t mca_btl_vader_t;
OMPI_MODULE_DECLSPEC extern mca_btl_vader_t mca_btl_vader;

/***
 * One or more FIFO components may be a pointer that must be
 * accessed by multiple processes.  Since the shared region may
 * be mmapped differently into each process's address space,
 * these pointers will be relative to some base address.  Here,
 * we define macros to translate between relative addresses and
 * virtual addresses.
 */


/* number of peers on the node (not including self) */
#define MCA_BTL_VADER_NUM_LOCAL_PEERS ompi_process_info.num_local_peers

/* local rank in the group */
#define MCA_BTL_VADER_LOCAL_RANK ompi_process_info.my_local_rank

/* memcpy is faster at larger sizes but is undefined if the
   pointers are aliased (TODO -- readd alias check) */
static inline void vader_memmove (void *dst, void *src, size_t size)
{
    if (size >= (size_t) mca_btl_vader_component.memcpy_limit) {
        memcpy (dst, src, size);
    } else {
        memmove (dst, src, size);
    }
}

/**
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
int mca_btl_vader_send(struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *endpoint,
                       struct mca_btl_base_descriptor_t *descriptor,
                       mca_btl_base_tag_t tag);

/**
 * Initiate an inline send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
int mca_btl_vader_sendi (struct mca_btl_base_module_t *btl,
                         struct mca_btl_base_endpoint_t *endpoint,
                         struct opal_convertor_t *convertor,
                         void *header, size_t header_size,
                         size_t payload_size, uint8_t order,
                         uint32_t flags, mca_btl_base_tag_t tag,
                         mca_btl_base_descriptor_t **descriptor);

/**
 * Initiate an synchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
int mca_btl_vader_put (struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *endpoint,
                       struct mca_btl_base_descriptor_t *des);

/**
 * Initiate an synchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
int mca_btl_vader_get (struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *endpoint,
                       struct mca_btl_base_descriptor_t *des);

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
mca_btl_base_descriptor_t* mca_btl_vader_alloc (struct mca_btl_base_module_t* btl,
                                                struct mca_btl_base_endpoint_t* endpoint,
                                                uint8_t order, size_t size, uint32_t flags);

END_C_DECLS

#endif
