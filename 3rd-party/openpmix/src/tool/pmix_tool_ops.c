/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2014      Artem Y. Polyakov <artpol84@gmail.com>.
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

#include "src/client/pmix_client_ops.h"
#include "src/include/pmix_globals.h"
#include "src/mca/bfrops/bfrops.h"
#include "src/mca/ptl/ptl.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"

#include "pmix_tool_ops.h"

static void tool_switchyard(struct pmix_peer_t *pr,
                            pmix_ptl_hdr_t *hdr,
                            pmix_buffer_t *buf,
                            void *cbdata);

/* the following function is used by the PMIx server
 * switchyard to process commands sent to one tool by
 * another tool. Tool processes cannot have PMIx clients
 * as they are tools and NOT servers. Thus, the only
 * case we must handle is that where one tool has
 * connected to another tool as its primary server
 * (for example, a debugger fork/exec'ing a launcher).
 * In this case, the tool must relay the request to
 * a server that can process it since it will itself
 * lack the ability to do so.
 *
 * For example, a "spawn" command must be relayed to
 * the local system server so it can be executed as
 * the tool itself has no mechanism by which it can
 * do so
 *
 * Input params:
 * cmd - the command we received
 *
 * peer - the tool that sent us the request
 *
 * bfr - the buffer containing the request
 */
pmix_status_t pmix_tool_relay_op(pmix_cmd_t cmd, pmix_peer_t *peer,
                                 pmix_buffer_t *bfr, uint32_t tag)
{
    pmix_shift_caddy_t *s;
    pmix_status_t rc;
    pmix_buffer_t *relay;
    pmix_cmd_t relaycmds[] = {PMIX_SPAWNNB_CMD};
    bool found = false;
    size_t nrelaycmds, n;

    /***** IF IT IS THE SPAWN COMMAND, WE SEND IT WITH A
    SEPARATE CBFUNC SO WE CAN INTERCEPT THE RESPONSE *****/

    /* there are some commands we just cannot relay as
     * it makes no sense to do so */
    nrelaycmds = sizeof(relaycmds) / sizeof(pmix_cmd_t);
    for (n = 0; n < nrelaycmds; n++) {
        if (cmd == relaycmds[n]) {
            found = true;
            break;
        }
    }
    if (!found) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    if (!pmix_globals.connected) {
        return PMIX_ERR_UNREACH;
    }

    s = PMIX_NEW(pmix_shift_caddy_t);
    PMIX_RETAIN(peer);
    s->peer = peer;
    s->ncodes = tag;
    /* reset the buffer's pointers so the entire
     * message is resent */
    bfr->unpack_ptr = bfr->base_ptr;
    relay = PMIX_NEW(pmix_buffer_t);
    PMIX_BFROPS_COPY_PAYLOAD(rc, peer, relay, bfr);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(relay);
        PMIX_RELEASE(s);
        return rc;
    }

    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, relay, tool_switchyard, (void *) s);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(relay);
        PMIX_RELEASE(s);
        return rc;
    }
    return PMIX_SUCCESS;
}

static void tool_switchyard(struct pmix_peer_t *pr, pmix_ptl_hdr_t *hdr,
                            pmix_buffer_t *buf, void *cbdata)
{
    pmix_shift_caddy_t *s = (pmix_shift_caddy_t *) cbdata;
    pmix_buffer_t *relay;
    pmix_status_t rc;
    uint32_t tag = (uint32_t) s->ncodes;
    PMIX_HIDE_UNUSED_PARAMS(pr, hdr);

    /* the tag for the original sender was stored in ncodes, and
     * the server would have packed the buffer using my
     * bfrops plugin */

    relay = PMIX_NEW(pmix_buffer_t);
    PMIX_BFROPS_COPY_PAYLOAD(rc, pmix_globals.mypeer, relay, buf);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(relay);
        return;
    }
    PMIX_SERVER_QUEUE_REPLY(rc, s->peer, tag, relay);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(relay);
    }
    PMIX_RELEASE(s);
}
