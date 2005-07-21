/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_BTL_PORTALS_RECV_H
#define OMPI_BTL_PORTALS_RECV_H

#include "btl_portals_frag.h"

struct mca_btl_portals_recv_chunk_t {
    opal_list_item_t base;

    mca_btl_portals_module_t *btl;

    void *start;
    size_t length;
    ptl_handle_me_t me_h;
    ptl_handle_md_t md_h;

    volatile bool full;
    volatile int32_t pending;
};
typedef struct mca_btl_portals_recv_chunk_t mca_btl_portals_recv_chunk_t;
OBJ_CLASS_DECLARATION(mca_btl_portals_recv_chunk_t);


int mca_btl_portals_recv_enable(mca_btl_portals_module_t *btl);

int mca_btl_portals_recv_disable(mca_btl_portals_module_t *btl);

int mca_btl_portals_process_recv(mca_btl_portals_module_t *btl, 
                                 ptl_event_t *ev);

/**
 * Create a chunk of memory for receiving send messages.  Must call
 * activate_chunk on the returned chunk of memory before it will be
 * active with the POrtals library 
 *
 * Module lock must be held before calling this function
 */
mca_btl_portals_recv_chunk_t* 
mca_btl_portals_recv_chunk_init(mca_btl_portals_module_t *btl);


/**
 * Free a chunk of memory.  Will remove the match entry, then progress
 * Portals until the pending count is returned to 0.  Will then free
 * all resources associated with chunk.
 *
 * Module lock must be held before calling this function
 */
int mca_btl_portals_recv_chunk_free(mca_btl_portals_recv_chunk_t *chunk);


/**
 * activate a chunk.  Chunks that are full (have gone inactive) can be
 * re-activated with this call.  There is no need to hold the lock
 * before calling this function
 */
static inline int
mca_btl_portals_activate_chunk(mca_btl_portals_recv_chunk_t *chunk)
{
    int ret;
    ptl_process_id_t any_proc = { PTL_NID_ANY, PTL_PID_ANY };
    ptl_md_t md;

    /* if we have pending operations, something very, very, very bad
       has happened... */
    assert(chunk->pending == 0);

    if (NULL == chunk->start) return OMPI_ERROR;

    /* create match entry */
    ret = PtlMEInsert(chunk->btl->portals_recv_reject_me_h,
                      any_proc,
                      0, /* match bits */
                      0, /* ignore bits */
                      PTL_UNLINK,
                      PTL_INS_BEFORE,
                      &(chunk->me_h));
    if (PTL_OK != ret) return OMPI_ERROR;

    /* and the memory descriptor */
    md.start = chunk->start;
    md.length = chunk->length;
    md.threshold = PTL_MD_THRESH_INF;
    md.max_size = chunk->btl->super.btl_max_send_size;
    md.options = PTL_MD_OP_PUT | PTL_MD_MAX_SIZE;
    md.user_ptr = chunk;
    md.eq_handle = chunk->btl->portals_eq_handles[OMPI_BTL_PORTALS_EQ_RECV];

    chunk->pending = 0;
    chunk->full = false;
    /* make sure that everyone sees the update on full value */
    opal_atomic_mb();

    ret = PtlMDAttach(chunk->me_h,
                      md,
                      PTL_UNLINK,
                      &(chunk->md_h));
    if (PTL_OK != ret) {
        PtlMEUnlink(chunk->me_h);
        return OMPI_ERROR;
    }

    OPAL_OUTPUT_VERBOSE((100, mca_btl_portals_component.portals_output,
                         "*** new receive buffer posted ***"));

    return OMPI_SUCCESS;
}


static inline void
mca_btl_portals_return_chunk_part(mca_btl_portals_module_t *btl,
                                  mca_btl_portals_recv_chunk_t *chunk)
{
    int ret;

    OPAL_OUTPUT_VERBOSE((100, mca_btl_portals_component.portals_output,
                         "*** return chunk called %d %d ***", 
                         chunk->full, chunk->pending));
    OPAL_THREAD_ADD32(&(chunk->pending), -1);
    if (chunk->full == true) {
        if (chunk->pending == 0) {
            ret = mca_btl_portals_activate_chunk(chunk);
            if (OMPI_SUCCESS != ret) {
                /* BWB - now what? */
            }
        }
    }    
}

#endif /* OMPI_BTL_PORTALS_RECV_H */
