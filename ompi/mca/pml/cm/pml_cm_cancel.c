/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/request/request.h"
#include "ompi/mca/pml/base/pml_base_request.h"

#include "pml_cm.h"
#include "pml_cm_sendreq.h"
#include "pml_cm_recvreq.h"

int
mca_pml_cm_cancel(struct ompi_request_t *request, int flag)
{
    /* Temporarily silence a compiler warning.  Remove this when the
       cancel is fully implemented */
#if 1
    return OMPI_SUCCESS;
#else
    int ret
   /*  mca_pml_cm_request_t *base_request =  */
/*         (mca_pml_cm_request_t*) request; */
/*     ret = OMPI_MTL_CALL(cancel(ompi_mtl, */
/*                                &base_request->req_mtl, */
/*                                flag)); */
    return ret;
#endif
}
