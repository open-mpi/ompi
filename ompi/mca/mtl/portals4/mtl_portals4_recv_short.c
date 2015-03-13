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

static inline int ompi_mtl_portals4_activate_block(ompi_mtl_portals4_recv_short_block_t *block);
static int ompi_mtl_portals4_recv_short_block_free(ompi_mtl_portals4_recv_short_block_t *block);

static int
ompi_mtl_portals4_recv_block_progress(ptl_event_t *ev,
                                     ompi_mtl_portals4_base_request_t* ptl_base_request)
{
    ompi_mtl_portals4_recv_short_request_t *ptl_request = 
        (ompi_mtl_portals4_recv_short_request_t*) ptl_base_request;
    ompi_mtl_portals4_recv_short_block_t *block = ptl_request->block;

    if (PTL_EVENT_AUTO_FREE == ev->type) {
        if (OPAL_UNLIKELY(block->release_on_free)) {
            opal_list_remove_item(&ompi_mtl_portals4.waiting_recv_short_blocks,
                                  &block->base);
            ompi_mtl_portals4_recv_short_block_free(block);
        } else {
            ompi_mtl_portals4_activate_block(block);
        }
    } else if (PTL_EVENT_AUTO_UNLINK == ev->type) {
        opal_list_remove_item(&ompi_mtl_portals4.active_recv_short_blocks,
                              &block->base);
        opal_list_append(&ompi_mtl_portals4.waiting_recv_short_blocks,
                         &block->base);
    } else {
        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                             "OVERFLOW EVENT %d, hdr_data = %lx", ev->type, (long unsigned) ev->hdr_data));
    }

    return OMPI_SUCCESS;
}


static ompi_mtl_portals4_recv_short_block_t* 
ompi_mtl_portals4_recv_short_block_alloc(bool release_on_free)
{
    ompi_mtl_portals4_recv_short_block_t *block;

    block = OBJ_NEW(ompi_mtl_portals4_recv_short_block_t);
    block->start = malloc(ompi_mtl_portals4.recv_short_size);
    if (block->start == NULL) return NULL;

    block->me_h = PTL_INVALID_HANDLE;
    block->request.block = block;
    block->request.super.type = portals4_req_recv_short;
    block->request.super.event_callback = ompi_mtl_portals4_recv_block_progress;
    block->release_on_free = release_on_free;

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
    ptl_match_bits_t match_bits = MTL_PORTALS4_SHORT_MSG;
    ptl_match_bits_t ignore_bits;
    ptl_me_t me;
    int ret;

    opal_list_remove_item(&ompi_mtl_portals4.waiting_recv_short_blocks, &block->base);

    ignore_bits = MTL_PORTALS4_CONTEXT_MASK | MTL_PORTALS4_SOURCE_MASK | MTL_PORTALS4_TAG_MASK;

    me.start = block->start;
    me.length = ompi_mtl_portals4.recv_short_size;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = ompi_mtl_portals4.eager_limit;
    me.uid = ompi_mtl_portals4.uid;
    me.options = 
        PTL_ME_OP_PUT | 
        PTL_ME_EVENT_LINK_DISABLE |
        PTL_ME_MANAGE_LOCAL | 
        PTL_ME_MAY_ALIGN;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = match_bits;
    me.ignore_bits = ignore_bits;

    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.recv_idx,
                      &me,
                      PTL_OVERFLOW_LIST,
                      &block->request,
                      &block->me_h);
    if (OPAL_LIKELY(ret == PTL_OK)) {
        ret = OMPI_SUCCESS;
        opal_list_append(&ompi_mtl_portals4.active_recv_short_blocks,
                         &block->base);
    } else {
        ret = ompi_mtl_portals4_get_error(ret);
    }

    return ret;
}


int
ompi_mtl_portals4_recv_short_init(void)
{
    int i;

    OBJ_CONSTRUCT(&(ompi_mtl_portals4.active_recv_short_blocks), opal_list_t);
    OBJ_CONSTRUCT(&(ompi_mtl_portals4.waiting_recv_short_blocks), opal_list_t);

    /* create the recv blocks */
    for (i = 0 ; i < ompi_mtl_portals4.recv_short_num ; ++i) {
        ompi_mtl_portals4_recv_short_block_t *block = 
            ompi_mtl_portals4_recv_short_block_alloc(false);
        if (OPAL_UNLIKELY(NULL == block)) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        opal_list_append(&ompi_mtl_portals4.waiting_recv_short_blocks,
                         &block->base);
        ompi_mtl_portals4_activate_block(block);
    }

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_recv_short_fini(void)
{
    opal_list_item_t *item;

    while (NULL !=  (item = opal_list_remove_first(&ompi_mtl_portals4.active_recv_short_blocks))) {
        ompi_mtl_portals4_recv_short_block_t *block = 
            (ompi_mtl_portals4_recv_short_block_t*) item;
        ompi_mtl_portals4_recv_short_block_free(block);
    }
    while (NULL !=  (item = opal_list_remove_first(&ompi_mtl_portals4.waiting_recv_short_blocks))) {
        ompi_mtl_portals4_recv_short_block_t *block = 
            (ompi_mtl_portals4_recv_short_block_t*) item;
        ompi_mtl_portals4_recv_short_block_free(block);
    }

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_recv_short_link(int count)
{
    int active = opal_list_get_size(&ompi_mtl_portals4.active_recv_short_blocks);
    int i;

    if (active < count) {
        for (i = 0 ; i < (count - active) ; ++i) {
            ompi_mtl_portals4_recv_short_block_t *block = 
                ompi_mtl_portals4_recv_short_block_alloc(false);
            if (NULL == block) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            opal_list_append(&ompi_mtl_portals4.waiting_recv_short_blocks,
                             &block->base);
            ompi_mtl_portals4_activate_block(block);
        }
    }
    
    return OMPI_SUCCESS;
}
