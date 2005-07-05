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

#ifndef MCA_BTL_PORTALS_RECV_H
#define MCA_BTL_PORTALS_RECV_H

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


int mca_btl_portals_recv_enable(mca_btl_portals_module_t *module);

int mca_btl_portals_recv_disable(mca_btl_portals_module_t *module);

int mca_btl_portals_process_recv(mca_btl_portals_module_t *module, 
                                 ptl_event_t *ev);

/**
 * Create a chunk of memory for receiving send messages.  Must call
 * activate_chunk on the returned chunk of memory before it will be
 * active with the POrtals library */
mca_btl_portals_recv_chunk_t* 
mca_btl_portals_recv_chunk_init(mca_btl_portals_module_t *module);

/**
 * Free a chunk of memory.  Will remove the match entry, then progress
 * Portals until the pending count is returned to 0.  Will then free
 * all resources associated with chunk.
 */
int mca_btl_portals_recv_chunk_free(mca_btl_portals_recv_chunk_t *chunk);

/*
 * activate a chunk.  Chunks that are full (have gone inactive) can be
 * re-activated with this call
 */
static inline int
mca_btl_portals_activate_chunk(mca_btl_portals_recv_chunk_t *chunk)
{
    int ret;
    ptl_process_id_t proc = { PTL_NID_ANY, PTL_PID_ANY };
    ptl_md_t md;

    /* if we have pending operations, something very, very, very bad
       has happened... */
    assert(chunk->pending == 0);

    if (NULL == chunk->start) return OMPI_ERROR;

    /* create match entry */
    ret = PtlMEAttach(chunk->btl->portals_ni_h,
                      BTL_PORTALS_SEND_TABLE_ID,
                      proc,
                      0, /* match bits */
                      0, /* ignore bits */
                      PTL_UNLINK,
                      PTL_INS_AFTER,
                      &(chunk->me_h));
    if (PTL_OK != ret) return OMPI_ERROR;

    /* and the memory descriptor */
    md.start = chunk->start;
    md.length = chunk->length;
    md.threshold = PTL_MD_THRESH_INF;
    md.max_size = chunk->btl->super.btl_max_send_size;
    md.options = PTL_MD_OP_PUT | PTL_MD_MAX_SIZE;
    md.user_ptr = chunk;
    md.eq_handle = chunk->btl->portals_eq_handles[MCA_BTL_PORTALS_EQ_RECV];

    chunk->full = false;

    ret = PtlMDAttach(chunk->me_h,
                      md,
                      PTL_UNLINK,
                      &(chunk->md_h));
    if (PTL_OK != ret) {
        PtlMEUnlink(chunk->me_h);
        return OMPI_ERROR;
    }

    OPAL_OUTPUT_VERBOSE((100, mca_btl_portals_component.portals_output,
                         "new receive buffer posted"));

    return OMPI_SUCCESS;
}

#endif /* MCA_BTL_PORTALS_RECV_H */
