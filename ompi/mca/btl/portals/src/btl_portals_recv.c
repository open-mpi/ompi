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


#include "ompi_config.h"
#include "portals_config.h"

#include "include/constants.h"

#include "btl_portals.h"
#include "btl_portals_recv.h"


int
mca_btl_portals_recv_enable(mca_btl_portals_module_t *module)
{
    return OMPI_SUCCESS;
}


int
mca_btl_portals_recv_disable(mca_btl_portals_module_t *module)
{
    return OMPI_SUCCESS;
}


int
mca_btl_portals_process_recv(mca_btl_portals_module_t *module, 
                             ptl_event_t *ev)
{
    return OMPI_SUCCESS;
}


mca_btl_portals_recv_chunk_t* 
mca_btl_portals_recv_chunk_init(mca_btl_portals_module_t *module)
{
    mca_btl_portals_recv_chunk_t *chunk;

    chunk = OBJ_NEW(mca_btl_portals_recv_chunk_t);
    chunk->btl = module;
    chunk->length = module->portals_recv_mds_size;
    chunk->start = malloc(chunk->length);
    if (chunk->start == NULL) return NULL;

    chunk->me_h = PTL_INVALID_HANDLE;
    chunk->md_h = PTL_INVALID_HANDLE;

    chunk->full = false;
    chunk->pending = 0;

    return chunk;
}


int
mca_btl_portals_recv_chunk_free(mca_btl_portals_recv_chunk_t *chunk)
{

    if (PTL_INVALID_HANDLE != chunk->me_h) {
        PtlMEUnlink(chunk->me_h);
        chunk->me_h = PTL_INVALID_HANDLE;
    }

    while (chunk->pending != 0) {
        mca_btl_portals_component_progress();
    }

    if (PTL_INVALID_HANDLE != chunk->md_h) {
        PtlMDUnlink(chunk->md_h);
        chunk->md_h = PTL_INVALID_HANDLE;
    }

    if (NULL != chunk->start) {
        free(chunk->start);
        chunk->start = NULL;
    }
    chunk->length = 0;
    chunk->full = false;

    return OMPI_SUCCESS;
}


OBJ_CLASS_INSTANCE(mca_btl_portals_recv_chunk_t,
                   opal_list_item_t,
                   NULL, NULL);
