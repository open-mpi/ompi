/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/dss/dss.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/output.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/base/base.h"

int orte_ess_base_build_nidmap(opal_buffer_t *buffer,
                               opal_pointer_array_t *nidmap,
                               orte_pmap_t **pmap, orte_vpid_t *num_procs)
{
    int rc;
    opal_byte_object_t *bo;
    int32_t cnt;
   
    ORTE_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:build:nidmap: received buffer with %ld bytes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)buffer->bytes_used));

    /* it is okay if the buffer is empty - could be a non-MPI proc */
    if (0 == buffer->bytes_used) {
        return ORTE_SUCCESS;
    }
    
    /* extract the byte object holding the daemonmap */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &bo, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the node map */
    if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(bo, nidmap))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* the bytes in the object were free'd by the decode */
    
    /* extract the byte object holding the process map */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &bo, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the process map */
    if (ORTE_SUCCESS != (rc = orte_util_decode_pidmap(bo, num_procs,
                                                      pmap, NULL, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* the bytes in the object were free'd by the decode */
                         
    return ORTE_SUCCESS;
}
