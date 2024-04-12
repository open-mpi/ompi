/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"
#include "include/pmix.h"

#include "src/client/pmix_client_ops.h"
#include "src/hwloc/pmix_hwloc.h"
#include "src/include/pmix_globals.h"
#include "src/util/pmix_error.h"

static void _loadtp(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    cb->pstatus = pmix_hwloc_load_topology(cb->topo);
    PMIX_WAKEUP_THREAD(&cb->lock);
}

PMIX_EXPORT pmix_status_t PMIx_Load_topology(pmix_topology_t *topo)
{
    pmix_status_t rc;
    pmix_cb_t cb;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.topo = topo;
    PMIX_THREADSHIFT(&cb, _loadtp);

    /* wait for the result */
    PMIX_WAIT_THREAD(&cb.lock);
    rc = cb.pstatus;
    PMIX_DESTRUCT(&cb);

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Parse_cpuset_string(const char *cpuset_string, pmix_cpuset_t *cpuset)
{
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    rc = pmix_hwloc_parse_cpuset_string(cpuset_string, cpuset);
    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Get_cpuset(pmix_cpuset_t *cpuset, pmix_bind_envelope_t ref)
{
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    rc = pmix_hwloc_get_cpuset(cpuset, ref);
    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Get_relative_locality(const char *locality1, const char *locality2,
                                                     pmix_locality_t *locality)
{
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    rc = pmix_hwloc_get_relative_locality(locality1, locality2, locality);
    return rc;
}

static void icbrelfn(void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t *) cbdata;
    PMIX_RELEASE(cb);
}

static void distcb(pmix_status_t status, pmix_device_distance_t *dist, size_t ndist, void *cbdata,
                   pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t *) cbdata;
    size_t n;

    cb->status = status;
    cb->nvals = ndist;

    if (PMIX_SUCCESS == status && 0 < ndist) {
        PMIX_DEVICE_DIST_CREATE(cb->dist, cb->nvals);
        for (n = 0; n < cb->nvals; n++) {
            if (NULL != dist[n].uuid) {
                cb->dist[n].uuid = strdup(dist[n].uuid);
            }
            if (NULL != dist[n].osname) {
                cb->dist[n].osname = strdup(dist[n].osname);
            }
            cb->dist[n].type = dist[n].type;
            cb->dist[n].mindist = dist[n].mindist;
            cb->dist[n].maxdist = dist[n].maxdist;
        }
    }
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    /* release the caller */
    PMIX_WAKEUP_THREAD(&cb->lock);
}

pmix_status_t PMIx_Compute_distances(pmix_topology_t *topo, pmix_cpuset_t *cpuset,
                                     pmix_info_t info[], size_t ninfo,
                                     pmix_device_distance_t *distances[], size_t *ndist)
{
    pmix_cb_t cb;
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_globals.debug_output, "pmix:compute_distances");

    *distances = NULL;
    *ndist = 0;

    /* create a callback object so we can be notified when
     * the non-blocking operation is complete */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    rc = PMIx_Compute_distances_nb(topo, cpuset, info, ninfo, distcb, &cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_DESTRUCT(&cb);
        return rc;
    }
    /* wait for the data to return */
    PMIX_WAIT_THREAD(&cb.lock);

    rc = cb.status;
    /* xfer the results */
    if (NULL != cb.dist) {
        *distances = cb.dist;
        *ndist = cb.nvals;
        cb.dist = NULL;
        cb.nvals = 0;
    }
    PMIX_DESTRUCT(&cb);

    pmix_output_verbose(2, pmix_globals.debug_output, "pmix:compute_distances completed");

    return rc;
}

static void dcbfunc(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    if (NULL != cb->cbfunc.distfn) {
        cb->cbfunc.distfn(cb->status, cb->dist, cb->nvals, cb->cbdata, icbrelfn, cb);
        return;
    }

    PMIX_RELEASE(cb);
}

static void direcv(struct pmix_peer_t *peer, pmix_ptl_hdr_t *hdr, pmix_buffer_t *buf, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t *) cbdata;
    pmix_status_t rc;
    int cnt;

    PMIX_HIDE_UNUSED_PARAMS(hdr);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:compute_dist recv from server with %d bytes", (int) buf->bytes_used);

    /* a zero-byte buffer indicates that this recv is being
     * completed due to a lost connection */
    if (PMIX_BUFFER_IS_EMPTY(buf)) {
        rc = PMIX_ERR_UNREACH;
        goto complete;
    }

    /* unpack the status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cb->status, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (PMIX_SUCCESS != cb->status) {
        rc = cb->status;
        goto complete;
    }

    /* unpack any returned data */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cb->nvals, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc && PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (0 < cb->nvals) {
        PMIX_DEVICE_DIST_CREATE(cb->dist, cb->nvals);
        cnt = cb->nvals;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cb->dist, &cnt, PMIX_DEVICE_DIST);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto complete;
        }
    }

complete:
    pmix_output_verbose(2, pmix_globals.debug_output, "pmix:compute_dist recv from server releasing");
    /* release the caller */
    cb->cbfunc.distfn(rc, cb->dist, cb->nvals, cb->cbdata, icbrelfn, (void *) cb);
}

pmix_status_t PMIx_Compute_distances_nb(pmix_topology_t *tp, pmix_cpuset_t *cp,
                                        pmix_info_t info[], size_t ninfo,
                                        pmix_device_dist_cbfunc_t cbfunc, void *cbdata)
{
    pmix_cb_t *cb;
    pmix_status_t rc;
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_COMPUTE_DEVICE_DISTANCES_CMD;
    pmix_topology_t *topo = NULL;
    pmix_cpuset_t *cpuset = NULL;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    cb = PMIX_NEW(pmix_cb_t);
    cb->cbfunc.distfn = cbfunc;
    cb->cbdata = cbdata;

    /* if the topology is NULL, then use ours */
    if (NULL == tp) {
        if (NULL == pmix_globals.topology.topology) {
            /* if our topology is NULL, try to get it */
            rc = pmix_hwloc_load_topology(&pmix_globals.topology);
            if (PMIX_SUCCESS != rc) {
                /* try to ask our server if they can do it */
                goto request;
            }
        }
        topo = &pmix_globals.topology;
    } else {
        topo = tp;
    }
    /* same for cpuset */
    if (NULL == cp) {
        /* if our cpuset is NULL, it could be we are unbound or
         * that we haven't yet gotten our cpuset. Try to get it. */
        if (NULL == pmix_globals.cpuset.bitmap) {
            rc = pmix_hwloc_get_cpuset(&pmix_globals.cpuset, PMIX_CPUBIND_PROCESS);
            if (PMIX_SUCCESS != rc) {
                /* try to ask our server if they can do it */
                goto request;
            }
        }
        cpuset = &pmix_globals.cpuset;
    } else {
        cpuset = cp;
    }

    /* see if I can support this myself */
    cb->status = pmix_hwloc_compute_distances(topo, cpuset, info, ninfo, &cb->dist, &cb->nvals);
    if (PMIX_SUCCESS == cb->status) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        /* threadshift to return the result */
        PMIX_THREADSHIFT(cb, dcbfunc);
        return PMIX_SUCCESS;
    }

request:
    /* if I am a server (but not a tool) or I am not connected, there is nothing more I can do */
    if ((PMIX_PEER_IS_SERVER(pmix_globals.mypeer) &&
         !PMIX_PEER_IS_TOOL(pmix_globals.mypeer)) ||
        !pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        PMIX_RELEASE(cb);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* don't send our topology if it's local */
    if (topo == &pmix_globals.topology) {
        topo = NULL;
    }

    /* if we are a tool or client, then relay this request to the server */
    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
        return rc;
    }

    /* pack the topology we want them to use */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, topo, 1, PMIX_TOPO);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
        return rc;
    }
    /* pack the cpuset */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, cpuset, 1, PMIX_PROC_CPUSET);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
        return rc;
    }

    /* pack the directives */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
        return rc;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cb);
            return rc;
        }
    }

    /* push the message into our event base to send to the server */
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, msg, direcv, (void *) cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
    }
    return rc;
}
