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


#include "ompi_config.h"

#include "ompi/constants.h"

#include "btl_portals4.h"
#include "btl_portals4_recv.h"
#include "btl_portals4_frag.h"


OBJ_CLASS_INSTANCE(mca_btl_portals4_recv_block_t,
                   opal_list_item_t,
                   NULL, NULL);

int
mca_btl_portals4_recv_enable(mca_btl_portals4_module_t *btl)
{
    int i;

    /* create the recv blocks */
    for (i = 0 ; i < mca_btl_portals4_component.portals_recv_mds_num ; ++i) {
        mca_btl_portals4_recv_block_t *block =
            mca_btl_portals4_recv_block_init(btl);
        if (NULL == block) {
            mca_btl_portals4_recv_disable(btl);
            return OMPI_ERROR;
        }
        opal_list_append(&(btl->portals_recv_blocks),
                         (opal_list_item_t*) block);
        mca_btl_portals4_activate_block(block);
    }
    return OMPI_SUCCESS;
}

int
mca_btl_portals4_recv_disable(mca_btl_portals4_module_t *btl)
{
    opal_list_item_t *item;

    if (opal_list_get_size(&btl->portals_recv_blocks) > 0) {
        while (NULL !=
               (item = opal_list_remove_first(&btl->portals_recv_blocks))) {
            mca_btl_portals4_recv_block_t *block =
                (mca_btl_portals4_recv_block_t*) item;
            mca_btl_portals4_recv_block_free(block);
        }
    }

    return OMPI_SUCCESS;
}

mca_btl_portals4_recv_block_t*
mca_btl_portals4_recv_block_init(mca_btl_portals4_module_t *btl)
{
    mca_btl_portals4_recv_block_t *block;

    block = OBJ_NEW(mca_btl_portals4_recv_block_t);
    block->btl = btl;
    block->length = mca_btl_portals4_component.portals_recv_mds_size;
    block->start = malloc(block->length);
    if (block->start == NULL) return NULL;

    block->me_h = PTL_INVALID_HANDLE;

    block->full = false;
    block->pending = 0;

    return block;
}


int
mca_btl_portals4_recv_block_free(mca_btl_portals4_recv_block_t *block)
{
    if (NULL != block->start) {
        free(block->start);
        block->start = NULL;
    }
    block->length = 0;
    block->full = false;

    return OMPI_SUCCESS;
}
