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
#include "ptl_mx.h"
#include "ptl_mx_peer.h"
#include "ptl_mx_sendfrag.h"


static void mca_ptl_mx_send_frag_construct(mca_ptl_mx_send_frag_t* frag);
static void mca_ptl_mx_send_frag_destruct(mca_ptl_mx_send_frag_t* frag);


OBJ_CLASS_INSTANCE(
    mca_ptl_mx_send_frag_t,
    mca_ptl_base_send_frag_t,
    mca_ptl_mx_send_frag_construct,
    mca_ptl_mx_send_frag_destruct);

                                                                                                           
/*
 * Placeholders for send fragment constructor/destructors.
 */

static void mca_ptl_mx_send_frag_construct(mca_ptl_mx_send_frag_t* frag)
{
    /* one time initialization */
    frag->frag_segments[0].segment_ptr = &frag->frag_send.frag_base.frag_header;
    frag->frag_segments[0].segment_length = sizeof(mca_ptl_base_header_t);
}


static void mca_ptl_mx_send_frag_destruct(mca_ptl_mx_send_frag_t* frag)
{
}



