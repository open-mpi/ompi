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

#include "include/constants.h"

#include "btl_portals.h"
#include "btl_portals_send.h"


int
mca_btl_portals_process_send(mca_btl_portals_module_t *module, 
                             ptl_event_t *ev)
{
    opal_output_verbose(99, mca_btl_portals_component.portals_output,
                        "process_send");
    return OMPI_SUCCESS;
}



int
mca_btl_portals_send(struct mca_btl_base_module_t* btl,
                     struct mca_btl_base_endpoint_t* endpoint,
                     struct mca_btl_base_descriptor_t* descriptor, 
                     mca_btl_base_tag_t tag)
{
    mca_btl_portals_module_t *ptl_btl = (mca_btl_portals_module_t*) btl;
    mca_btl_portals_frag_t *frag = (mca_btl_portals_frag_t*) descriptor;
    frag->endpoint = endpoint;
    frag->hdr.tag = tag;
    frag->btl = ptl_btl;

    return mca_btl_portals_send_frag(frag);
}
