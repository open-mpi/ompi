/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "vprotocol_pessimist.h"

int mca_vprotocol_pessimist_progress(void)
{
    int ret = OMPI_ERR_NOT_IMPLEMENTED;
#if 0    
    /* First let the real progress take place */
    ret = mca_pml_v.host_pml.pml_progress();

    for(req = opal_list_head(&mca_vprotocol_pessimist.sender_based.sendprogressreq), 
        req != opal_list_end(&mca_vprotocol_pessimist.sender_based.sendprogressreq),
        req = req->next) {
        preq = VPESSIMIST_SEND_REQ(req);
        conv = req->conv;
        
        
    }
#endif
    return ret;
}
