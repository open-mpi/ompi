/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_TCP_FRAG_H
#define MCA_BTL_TCP_FRAG_H


#define MCA_BTL_TCP_FRAG_ALIGN (8)
#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif

#include "btl_tcp2.h"
#include "btl_tcp2_hdr.h"

BEGIN_C_DECLS

#define MCA_BTL_TCP_FRAG_IOVEC_NUMBER  4

/**
 * Commands for the threaded version when the fragments must be completed
 * by one of the MPI bounded threads.
 */
#define MCA_BTL_TCP_FRAG_STEP_UNDEFINED       ((uint16_t)0x0000)
#define MCA_BTL_TCP_FRAG_STEP_SEND_COMPLETE   ((uint16_t)0x0001)
#define MCA_BTL_TCP_FRAG_STEP_RECV_COMPLETE   ((uint16_t)0x0002)

/**
 * TCP fragment derived type.
 */
struct mca_btl_tcp2_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segments[2];
    struct mca_btl_base_endpoint_t *endpoint;
    struct mca_btl_tcp2_module_t* btl;
    mca_btl_tcp2_hdr_t hdr;
    struct iovec iov[MCA_BTL_TCP_FRAG_IOVEC_NUMBER + 1];
    struct iovec *iov_ptr;
    size_t iov_cnt;
    size_t iov_idx;
    size_t size;
    int rc;
    ompi_free_list_t* my_list;
};
typedef struct mca_btl_tcp2_frag_t mca_btl_tcp2_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_tcp2_frag_t);

typedef struct mca_btl_tcp2_frag_t mca_btl_tcp2_frag_eager_t;

OBJ_CLASS_DECLARATION(mca_btl_tcp2_frag_eager_t);

typedef struct mca_btl_tcp2_frag_t mca_btl_tcp2_frag_max_t;

OBJ_CLASS_DECLARATION(mca_btl_tcp2_frag_max_t);

typedef struct mca_btl_tcp2_frag_t mca_btl_tcp2_frag_user_t;

OBJ_CLASS_DECLARATION(mca_btl_tcp2_frag_user_t);


/*
 * Macros to allocate/return descriptors from module specific
 * free list(s).
 */

#define MCA_BTL_TCP_FRAG_ALLOC_EAGER(frag)                                 \
{                                                                          \
    ompi_free_list_item_t *item;                                           \
    MCA_BTL_TCP_CRITICAL_SECTION_ENTER(&mca_btl_tcp_component.tcp_frag_eager_mutex); \
    OMPI_FREE_LIST_GET_MT(&mca_btl_tcp_component.tcp_frag_eager, item);    \
    MCA_BTL_TCP_CRITICAL_SECTION_LEAVE(&mca_btl_tcp_component.tcp_frag_eager_mutex); \
    frag = (mca_btl_tcp_frag_t*) item;                                     \
}

#define MCA_BTL_TCP_FRAG_ALLOC_MAX(frag)                                   \
{                                                                          \
    ompi_free_list_item_t *item;                                           \
    MCA_BTL_TCP_CRITICAL_SECTION_ENTER(&mca_btl_tcp_component.tcp_frag_max_mutex); \
    OMPI_FREE_LIST_GET_MT(&mca_btl_tcp_component.tcp_frag_max, item);      \
    MCA_BTL_TCP_CRITICAL_SECTION_LEAVE(&mca_btl_tcp_component.tcp_frag_max_mutex); \
    frag = (mca_btl_tcp_frag_t*) item;                                     \
}

#define MCA_BTL_TCP_FRAG_ALLOC_USER(frag)                                  \
{                                                                          \
    ompi_free_list_item_t *item;                                           \
    MCA_BTL_TCP_CRITICAL_SECTION_ENTER(&mca_btl_tcp_component.tcp_frag_user_mutex); \
    OMPI_FREE_LIST_GET_MT(&mca_btl_tcp_component.tcp_frag_user, item);     \
    MCA_BTL_TCP_CRITICAL_SECTION_LEAVE(&mca_btl_tcp_component.tcp_frag_user_mutex); \
    frag = (mca_btl_tcp_frag_t*) item;                                     \
}

#if MCA_BTL_TCP_USES_PROGRESS_THREAD
#define MCA_BTL_TCP_FRAG_RETURN(frag)                                      \
{                                                                          \
    (frag)->next_step = MCA_BTL_TCP_FRAG_STEP_UNDEFINED;                   \
    if( frag->my_list == &mca_btl_tcp_component.tcp_frag_eager ) {         \
        MCA_BTL_TCP_CRITICAL_SECTION_ENTER(&mca_btl_tcp_component.tcp_frag_eager_mutex); \
        OMPI_FREE_LIST_RETURN_MT(frag->my_list, (ompi_free_list_item_t*)(frag)); \
        MCA_BTL_TCP_CRITICAL_SECTION_LEAVE(&mca_btl_tcp_component.tcp_frag_eager_mutex); \
    } else if( frag->my_list == &mca_btl_tcp_component.tcp_frag_max ) { \
        MCA_BTL_TCP_CRITICAL_SECTION_ENTER(&mca_btl_tcp_component.tcp_frag_max_mutex); \
        OMPI_FREE_LIST_RETURN_MT(frag->my_list, (ompi_free_list_item_t*)(frag)); \
        MCA_BTL_TCP_CRITICAL_SECTION_LEAVE(&mca_btl_tcp_component.tcp_frag_max_mutex); \
    } else {                                                            \
        assert( frag->my_list == &mca_btl_tcp_component.tcp_frag_user ); \
        MCA_BTL_TCP_CRITICAL_SECTION_ENTER(&mca_btl_tcp_component.tcp_frag_user_mutex); \
        OMPI_FREE_LIST_RETURN_MT(frag->my_list, (ompi_free_list_item_t*)(frag)); \
        MCA_BTL_TCP_CRITICAL_SECTION_LEAVE(&mca_btl_tcp_component.tcp_frag_user_mutex); \
    }                                                                   \
}
#else
#define MCA_BTL_TCP_FRAG_RETURN(frag)                                      \
{                                                                          \
    (frag)->next_step = MCA_BTL_TCP_FRAG_STEP_UNDEFINED;                   \
    OMPI_FREE_LIST_RETURN_MT(frag->my_list, (ompi_free_list_item_t*)(frag));  \
}
#endif  /* MCA_BTL_TCP_USES_PROGRESS_THREAD */

#define MCA_BTL_TCP_FRAG_INIT_DST(frag,ep)                                 \
do {                                                                       \
    frag->base.des_src = NULL;                                             \
    frag->base.des_src_cnt = 0;                                            \
    frag->base.des_dst = frag->segments;                                   \
    frag->base.des_dst_cnt = 1;                                            \
    frag->endpoint = ep;                                                   \
    frag->iov[0].iov_len = sizeof(frag->hdr);                              \
    frag->iov[0].iov_base = (IOVBASE_TYPE*)&frag->hdr;                     \
    frag->iov_cnt = 1;                                                     \
    frag->iov_idx = 0;                                                     \
    frag->iov_ptr = frag->iov;                                             \
    frag->rc = 0;                                                          \
} while(0)


bool mca_btl_tcp2_frag_send(mca_btl_tcp2_frag_t*, int sd);
bool mca_btl_tcp2_frag_recv(mca_btl_tcp2_frag_t*, int sd);

void mca_btl_tcp_dump_frag( mca_btl_tcp_frag_t* frag, char* msg );

END_C_DECLS
#endif
