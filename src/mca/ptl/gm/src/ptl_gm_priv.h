/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "gm.h"

struct mca_ptl_gm_send_frag_t;
struct mca_ptl_gm_peer_t;

#define  PTL_GM_FIRST_FRAG_SIZE (1<<14)

/*#define DO_DEBUG(inst)  inst*/
#define DO_DEBUG(inst)

int mca_ptl_gm_analyze_recv_event( struct mca_ptl_gm_module_t* ptl, gm_recv_event_t* event );

void mca_ptl_gm_outstanding_recv( struct mca_ptl_gm_module_t *ptl);

int
mca_ptl_gm_peer_send( struct mca_ptl_gm_peer_t *ptl_peer,
		      struct mca_ptl_gm_send_frag_t *fragment,
		      struct mca_pml_base_send_request_t *sendreq,
		      size_t offset,
		      size_t *size,
		      int flags );

int
mca_ptl_gm_peer_put( struct mca_ptl_gm_peer_t *ptl_peer,
                     struct mca_ptl_gm_send_frag_t *fragment,
                     struct mca_pml_base_send_request_t *sendreq,
                     size_t offset,
                     size_t *size,
                     int flags,
                     void *target_buffer,
                     int bytes );



void send_callback(struct gm_port *port,void * context, gm_status_t status);

void put_callback(struct gm_port *port,void * context, gm_status_t status);

