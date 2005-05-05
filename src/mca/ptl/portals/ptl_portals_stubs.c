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

#include "ptl_portals.h"

/* BWB - README - BWB - README - BWB - README - BWB - README - BWB
 *
 * These are stub functions that return error so that the
 * initialization code can be developed and the whole thing will
 * link.  This file will disappear once all functions are
 * implemented.  Do not implement any functions in this file.
 *
 * BWB - README - BWB - README - BWB - README - BWB - README - BWB */



int
mca_ptl_portals_request_init(struct mca_ptl_base_module_t *ptl,
			     struct mca_pml_base_send_request_t *req)
{
    ompi_output(mca_ptl_portals_component.portals_output,
                "unimplemented function mca_ptl_request_init");
    return OMPI_SUCCESS;
}


void
mca_ptl_portals_request_fini(struct mca_ptl_base_module_t *ptl,
			     struct mca_pml_base_send_request_t *req)
{
    ompi_output(mca_ptl_portals_component.portals_output,
                "unimplemented function mca_ptl_request_fini");
    return;
}

void
mca_ptl_portals_matched(struct mca_ptl_base_module_t *ptl,
			struct mca_ptl_base_recv_frag_t *frag)
{
    ompi_output(mca_ptl_portals_component.portals_output,
                "unimplemented function mca_ptl_portals_matched");
    return;
}
