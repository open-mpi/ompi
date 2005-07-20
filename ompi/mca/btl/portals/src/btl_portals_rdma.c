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
#include "btl_portals_rdma.h"


int
mca_btl_portals_process_rdma(mca_btl_portals_module_t *module, 
                             ptl_event_t *ev)
{
    return OMPI_SUCCESS;
}


int
mca_btl_portals_put(struct mca_btl_base_module_t* btl_base,
                    struct mca_btl_base_endpoint_t* btl_peer,
                    struct mca_btl_base_descriptor_t* decriptor)
{
    opal_output(mca_btl_portals_component.portals_output,
                "Warning: call to unimplemented function put()");
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
mca_btl_portals_get(struct mca_btl_base_module_t* btl_base,
                    struct mca_btl_base_endpoint_t* btl_peer,
                    struct mca_btl_base_descriptor_t* decriptor)
{
    opal_output(mca_btl_portals_component.portals_output,
                "Warning: call to unimplemented function get()");
    return OMPI_ERR_NOT_IMPLEMENTED;
}
