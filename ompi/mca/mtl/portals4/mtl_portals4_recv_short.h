/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef OMPI_MTL_PORTALS_RECV_SHORT_H
#define OMPI_MTL_PORTALS_RECV_SHORT_H

struct ompi_mtl_portals4_recv_short_block_t {
    opal_list_item_t base;
    mca_mtl_portals4_module_t *mtl;
    void *start;
    ptl_handle_me_t me_h;
};
typedef struct ompi_mtl_portals4_recv_short_block_t ompi_mtl_portals4_recv_short_block_t;
OBJ_CLASS_DECLARATION(ompi_mtl_portals4_recv_short_block_t);

extern int
ompi_mtl_portals4_recv_short_init(mca_mtl_portals4_module_t *mtl);

extern int
ompi_mtl_portals4_recv_short_fini(mca_mtl_portals4_module_t *mtl);

extern int
ompi_mtl_portals4_recv_short_block_repost(ptl_event_t *ev);

extern int
ompi_mtl_portals4_recv_progress(ptl_event_t *ev,
                                ompi_mtl_portals4_base_request_t* ptl_bae_request);

#endif /* OMPI_MTL_PORTALS_RECV_SHORT_H */
