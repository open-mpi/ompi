/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>

#include "include/constants.h"
#include "util/proc_info.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/errmgr/errmgr.h"
#include "mca/soh/soh.h"
#include "runtime/runtime.h"


/**
 *  A "broadcast-like" function over the specified set of peers.
 *  @param  root    The process acting as the root of the broadcast.
 *  @param  peers   The list of processes receiving the broadcast (excluding root).
 *  @param  buffer  The data to broadcast - only significant at root.
 *  @param  cbfunc  Callback function on receipt of data - not significant at root.
 *
 *  Note that the callback function is provided so that the data can be
 *  received and interpreted by the application prior to the broadcast
 *  continuing to forward data along the distribution tree.
 */
                                                                                                  
int mca_oob_xcast(
    orte_process_name_t* root,
    orte_process_name_t* peers,
    size_t num_peers,
    orte_buffer_t* buffer,
    mca_oob_callback_packed_fn_t cbfunc)
{
    size_t i;
    int rc;
    int tag = MCA_OOB_TAG_XCAST;
    int cmpval;
    int status;
    orte_proc_state_t state;

    /* check to see if I am the root process name */
    cmpval = orte_ns.compare(ORTE_NS_CMP_ALL, root, orte_process_info.my_name);
    if(NULL != root && 0 == cmpval) {
        for(i=0; i<num_peers; i++) {
            /* check status of peer to ensure they are alive */
            if (ORTE_SUCCESS != (rc = orte_soh.get_proc_soh(&state, &status, peers+i))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (state != ORTE_PROC_STATE_TERMINATED) {
                rc = mca_oob_send_packed(peers+i, buffer, tag, 0);
                if (rc < 0) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
    } else {
        orte_buffer_t rbuf;
        OBJ_CONSTRUCT(&rbuf, orte_buffer_t);
        rc = mca_oob_recv_packed(MCA_OOB_NAME_ANY, &rbuf, tag);
        if(rc < 0) {
            OBJ_DESTRUCT(&rbuf);
            return rc;
        }
        if(cbfunc != NULL)
            cbfunc(rc, root, &rbuf, tag, NULL);
        OBJ_DESTRUCT(&rbuf);
    }
    return OMPI_SUCCESS;
}


