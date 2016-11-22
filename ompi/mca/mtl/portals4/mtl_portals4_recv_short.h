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
    char status;   /* see Note after */
};
/* Note: Even if portals4 may guarantee that PTL_EVENT_AUTO_UNLINK comes before
 *       PTL_EVENT_AUTO_FREE, we are not sure that this is the case in a
 *       multi-threaded environment : A thread catching the PTL_EVENT_AUTO_UNLINK
 *       may be preceded by another catching the PTL_EVENT_AUTO_FREE even if this
 *       event comes after. That is why we introduce the status field with the
 *       following STATUSES. */

#define BLOCK_STATUS_INACTIVE       0    /* The block has just been malloc'ed */
#define BLOCK_STATUS_WAITING_LINK   1    /* The PtlMEAppend has been called. Now wait for PTL_EVENT_LINK */
#define BLOCK_STATUS_ACTIVATED      2    /* PTL_EVENT_LINK has been received, the block is operational */
#define BLOCK_STATUS_WAITING_FREE   3    /* The PTL_EVENT_AUTO_UNLINK has been received, now wait for a PTL_EVENT_AUTO_FREE */
#define BLOCK_STATUS_WAITING_UNLINK 4    /* The PTL_EVENT_AUTO_FREE has been received, now wait for a PTL_EVENT_AUTO_UNLINK (rare) */

typedef struct ompi_mtl_portals4_recv_short_block_t ompi_mtl_portals4_recv_short_block_t;
OBJ_CLASS_DECLARATION(ompi_mtl_portals4_recv_short_block_t);

/* initialize and post short receive blocks */
extern int ompi_mtl_portals4_recv_short_init(void);

/* clean up all short receive blocks */
extern int ompi_mtl_portals4_recv_short_fini(void);

/* ensure that there's at least N short receive blocks linked */
extern int ompi_mtl_portals4_recv_short_link(int count);

#endif /* OMPI_MTL_PORTALS_RECV_SHORT_H */
