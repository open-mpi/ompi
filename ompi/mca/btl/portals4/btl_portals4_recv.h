/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#ifndef OMPI_BTL_PORTALS4_RECV_H
#define OMPI_BTL_PORTALS4_RECV_H

#include "btl_portals4_frag.h"
    
struct mca_btl_portals4_recv_block_t {
    opal_list_item_t base;
    
    mca_btl_portals4_module_t *btl;
    
    void *start; 
    size_t length;
    ptl_handle_me_t me_h;

    volatile bool full;
    volatile int32_t pending;
};  
typedef struct mca_btl_portals4_recv_block_t mca_btl_portals4_recv_block_t;
OBJ_CLASS_DECLARATION(mca_btl_portals4_recv_block_t);


int mca_btl_portals4_recv_enable(mca_btl_portals4_module_t *btl);

int mca_btl_portals4_recv_disable(mca_btl_portals4_module_t *btl);

/**
 * Free a block of memory.
 *
 */
int mca_btl_portals4_recv_block_free(mca_btl_portals4_recv_block_t *block);

/**
 * Create a block of memory for receiving send messages.  Must call
 * activate_block on the returned block of memory before it will be
 * active with the Portals library
 *
 * Module lock must be held before calling this function
 */
mca_btl_portals4_recv_block_t*
mca_btl_portals4_recv_block_init(mca_btl_portals4_module_t *btl);

/**
 * activate a block.  Blocks that are full (have gone inactive) can be
 * re-activated with this call.  There is no need to hold the lock
 * before calling this function
 */
static inline int
mca_btl_portals4_activate_block(mca_btl_portals4_recv_block_t *block)
{
    int ret;
    ptl_me_t me;
    ptl_process_t remote_proc;
    ptl_match_bits_t match_bits, ignore_bits;
    mca_btl_portals4_module_t *btl = block->btl;

    if (NULL == block->start) return OMPI_ERROR;

    ignore_bits = BTL_PORTALS4_CONTEXT_MASK | BTL_PORTALS4_SOURCE_MASK | BTL_PORTALS4_TAG_MASK;
    match_bits = BTL_PORTALS4_SHORT_MSG;

    me.start = block->start;
    me.length = block->length;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = btl->super.btl_eager_limit;
    me.uid = PTL_UID_ANY;
    me.options =
        PTL_ME_OP_PUT |
        PTL_ME_MANAGE_LOCAL |
        PTL_ME_EVENT_LINK_DISABLE  | 
        PTL_ME_MAY_ALIGN;

    remote_proc.phys.nid = PTL_NID_ANY;
    remote_proc.phys.pid = PTL_PID_ANY;

    me.match_id = remote_proc;
    me.match_bits = match_bits;
    me.ignore_bits = ignore_bits;

    block->pending = 0;
    block->full = false;
    opal_atomic_mb();

    ret = PtlMEAppend(btl->portals_ni_h,
                      btl->recv_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      block,
                      &block->me_h);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed on NI %d: %d",
                            __FILE__, __LINE__, btl->interface_num, ret);
        return OMPI_ERROR;
    }
    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "PtlMEAppend (recv) block=%p me_h=%d start=%p len=%x NI=%d\n",
        (void *)block, block->me_h, block->start, (unsigned int) block->length, btl->interface_num));

    return OMPI_SUCCESS;
}

#endif /* OMPI_BTL_PORTALS4_RECV_H */
