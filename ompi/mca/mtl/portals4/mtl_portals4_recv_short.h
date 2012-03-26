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

#include "mtl_portals4_request.h"

struct ompi_mtl_portals4_recv_short_block_t {
    opal_list_item_t base;
    void *start;
    ptl_handle_me_t me_h;
    struct ompi_mtl_portals4_recv_short_request_t request;
    bool release_on_free;
};
typedef struct ompi_mtl_portals4_recv_short_block_t ompi_mtl_portals4_recv_short_block_t;
OBJ_CLASS_DECLARATION(ompi_mtl_portals4_recv_short_block_t);

/* initialize and post short receive blocks */
extern int ompi_mtl_portals4_recv_short_init(void);

/* clean up all short receive blocks */
extern int ompi_mtl_portals4_recv_short_fini(void);

/* ensure that there's at least N short receive blocks linked */
extern int ompi_mtl_portals4_recv_short_link(int count);

#endif /* OMPI_MTL_PORTALS_RECV_SHORT_H */
