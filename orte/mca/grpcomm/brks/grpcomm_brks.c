/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"
#include "orte/runtime/orte_wait.h"

#include <string.h>

#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_brks.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_vpid_t *vpids,
                 size_t nprocs,
                 opal_buffer_t *msg);
static int allgather(orte_grpcomm_coll_t *coll,
                     opal_buffer_t *buf);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_brks_module = {
    init,
    finalize,
    xcast,
    allgather
};

/**
 * Initialize the module
 */
static int init(void)
{
    return OPAL_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    return;
}

static int xcast(orte_vpid_t *vpids,
                 size_t nprocs,
                 opal_buffer_t *msg)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int allgather(orte_grpcomm_coll_t *coll,
                     opal_buffer_t *sendbuf)
{
    orte_vpid_t *vpids = coll->dmns;
    size_t np = coll->ndmns;
    size_t total_entries = coll->nreported;
    opal_buffer_t *recvbuf = &coll->bucket;
    orte_rml_recv_cb_t *recv_cb = NULL;

    orte_vpid_t rank, distance, nv;
    orte_process_name_t peer;
    int32_t num_remote, cnt;
    opal_buffer_t collection, buf;
    int rc;

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:bruck algo employed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* if we only have one proc participating, just copy the data across and return */
    if (1 == np) {
        opal_dss.pack(recvbuf, &coll->nreported, 1, OPAL_INT32);
        return opal_dss.copy_payload(recvbuf, sendbuf);
    }

    /* if we only have one proc participating, just copy the data across and return */
    if (np*(np - 1)) {
        OPAL_OUTPUT((orte_grpcomm_base_framework.framework_output,
                     "%s grpcomm:coll:bruck number of participating daemons (%d) is power 2",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)np ));
        return ORTE_ERROR;
    }

    /* start by seeding the collection with our own data */
    OBJ_CONSTRUCT(&collection, opal_buffer_t);
    opal_dss.copy_payload(&collection, sendbuf);

    /* collective is constrained to take place within the specified jobid */
    peer.jobid = ORTE_PROC_MY_NAME->jobid;

    /* Communication step:
     At every step i, rank r:
     - doubles the distance
     - sends message containing all data collected so far to rank r - distance
     - receives message containing all data collected so far from rank (r + distance)
     */
    /* find my position in the group of participants. This
     * value is the "rank" we will use in the algo
     */
    rank = ORTE_VPID_INVALID;
    for (nv=0; nv < np; nv++) {
        if (vpids[nv] == ORTE_PROC_MY_NAME->vpid) {
            rank = nv;
            break;
        }
    }

    /* check for bozo case */
    if (ORTE_VPID_INVALID == rank) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    for (distance = 1; distance < np; distance <<= 1) {

        /* first send my current contents */
        nv = (rank - distance + np) % np;
        peer.vpid = vpids[nv];
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &total_entries, 1, OPAL_INT32);
        opal_dss.copy_payload(&buf, &collection);
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:coll:bruck sending to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&peer)));

        if (0 > (rc = orte_rml.send_buffer_nb(&peer, &buf, ORTE_RML_TAG_ALLGATHER, orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);

        /* now setup to recv from my other partner */
        nv = (rank + distance) % np;
        peer.vpid = vpids[nv];

        recv_cb = OBJ_NEW(orte_rml_recv_cb_t);
        OBJ_CONSTRUCT(recv_cb, orte_rml_recv_cb_t);
        recv_cb->active = true;
        orte_rml.recv_buffer_nb(&peer, ORTE_RML_TAG_ALLGATHER, ORTE_RML_NON_PERSISTENT, orte_rml_recv_callback, recv_cb);
        /* and wait for it to get here */
        ORTE_WAIT_FOR_COMPLETION(recv_cb->active);

        /* extract the number of entries in the remote buffer */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&recv_cb->data, &num_remote, &cnt, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* add it to our running total */
        total_entries += num_remote;

        /* transfer the data to our collection */
        opal_dss.copy_payload(&collection, &recv_cb->data);

        /* cleanup */
        OBJ_RELEASE(recv_cb);
    }

    /* output of a collective begins with the total number of entries */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(recvbuf, &total_entries, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* transfer the collected data */
    opal_dss.copy_payload(recvbuf, &collection);

    /* cleanup */
    OBJ_DESTRUCT(&collection);

    return ORTE_SUCCESS;
}
