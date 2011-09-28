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
 * Copyright (c) 2010      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"

#include "ompi/constants.h"

#include "mtl_portals4.h"
#include "mtl_portals4_recv_short.h"


OBJ_CLASS_INSTANCE(ompi_mtl_portals4_recv_short_block_t,
                   opal_list_item_t,
                   NULL, NULL);

static ompi_mtl_portals4_recv_short_block_t* 
ompi_mtl_portals4_recv_short_block_init(mca_mtl_portals4_module_t *mtl)
{
    ompi_mtl_portals4_recv_short_block_t *block;

    block = OBJ_NEW(ompi_mtl_portals4_recv_short_block_t);
    block->mtl = mtl;
    block->start = malloc(mtl->recv_short_size);
    if (block->start == NULL) return NULL;

    block->me_h = PTL_INVALID_HANDLE;

    return block;
}


static int
ompi_mtl_portals4_recv_short_block_free(ompi_mtl_portals4_recv_short_block_t *block)
{
    if (PTL_INVALID_HANDLE != block->me_h) {
        PtlMEUnlink(block->me_h);
        block->me_h = PTL_INVALID_HANDLE;
    }

    if (NULL != block->start) {
        free(block->start);
        block->start = NULL;
    }

    OBJ_RELEASE(block);

    return OMPI_SUCCESS;
}


static inline int
ompi_mtl_portals4_activate_block(ompi_mtl_portals4_recv_short_block_t *block)
{
    ptl_match_bits_t match_bits = PTL_SHORT_MSG;
    ptl_match_bits_t ignore_bits;
    ptl_me_t me;
    int ret;

    ignore_bits = PTL_CONTEXT_MASK | PTL_SOURCE_MASK | PTL_TAG_MASK;

    me.start = block->start;
    me.length = block->mtl->recv_short_size;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = block->mtl->eager_limit;
    me.uid = PTL_UID_ANY;
    me.options = PTL_ME_OP_PUT | PTL_ME_MANAGE_LOCAL | PTL_ME_NO_TRUNCATE | 
        PTL_ME_MAY_ALIGN | PTL_ME_ACK_DISABLE | PTL_ME_EVENT_COMM_DISABLE;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = match_bits;
    me.ignore_bits = ignore_bits;

    ret = PtlMEAppend(block->mtl->ni_h,
                      ompi_mtl_portals4.send_idx,
                      &me,
                      PTL_OVERFLOW,
                      block,
                      &block->me_h);
    return (ret == PTL_OK) ? OMPI_SUCCESS : ompi_mtl_portals4_get_error(ret);
}


int
ompi_mtl_portals4_recv_short_block_repost(ptl_event_t *ev)
{
    return ompi_mtl_portals4_activate_block(ev->user_ptr);
}


int
ompi_mtl_portals4_recv_short_init(mca_mtl_portals4_module_t *mtl)
{
    int i;

    OBJ_CONSTRUCT(&(mtl->recv_short_blocks), opal_list_t);

    /* create the recv blocks */
    for (i = 0 ; i < mtl->recv_short_num ; ++i) {
        ompi_mtl_portals4_recv_short_block_t *block = 
            ompi_mtl_portals4_recv_short_block_init(mtl);
        if (NULL == block) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        opal_list_append(&(mtl->recv_short_blocks),
                         (opal_list_item_t*) block);
        ompi_mtl_portals4_activate_block(block);
    }

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_recv_short_fini(mca_mtl_portals4_module_t *mtl)
{
    opal_list_item_t *item;

    while (NULL !=  (item = opal_list_remove_first(&mtl->recv_short_blocks))) {
        ompi_mtl_portals4_recv_short_block_t *block = 
            (ompi_mtl_portals4_recv_short_block_t*) item;
        ompi_mtl_portals4_recv_short_block_free(block);
    }

    return OMPI_SUCCESS;
}


