/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include "opal/opal_socket_errno.h"
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETINET_TCP_H
#include <netinet/tcp.h>
#endif

#include "opal_stdint.h"
#include "opal/types.h"
#include "opal/mca/backtrace/backtrace.h"
#include "opal/util/output.h"
#include "opal/util/net.h"
#include "opal/util/error.h"
#include "opal/class/opal_hash_table.h"
#include "opal/mca/dstore/dstore.h"
#include "opal/mca/event/event.h"
#include "opal/mca/sec/sec.h"
#include "opal/runtime/opal.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/state/state.h"
#include "orte/runtime/orte_wait.h"

#include "pmix_server_internal.h"

/*
 * Dispatch to the appropriate action routine based on the state
 * of the connection with the peer.
 */
void pmix_server_process_message(pmix_server_peer_t *peer)
{
    int rc, ret;
    int32_t cnt;
    pmix_cmd_t cmd;
    opal_buffer_t *reply, xfer, *bptr, buf, save, blocal, bremote;
    opal_buffer_t *data;
    opal_value_t kv, *kvp, *kvp2, *kp;
    opal_process_name_t id, idreq;
    orte_process_name_t name;
    orte_job_t *jdata;
    orte_proc_t *proc;
    orte_proc_t *proc_peer;
    opal_list_t values;
    uint32_t tag;
    opal_pmix_scope_t scope;
    int handle;
    pmix_server_dmx_req_t *req, *nextreq;
    bool found;
    orte_grpcomm_signature_t *sig;
    uint32_t sm_flag;

    /* xfer the message to a buffer for unpacking */
    OBJ_CONSTRUCT(&xfer, opal_buffer_t);
    opal_dss.load(&xfer, peer->recv_msg->data, peer->recv_msg->hdr.nbytes);
    tag = peer->recv_msg->hdr.tag;
    id = peer->recv_msg->hdr.id;
    peer->recv_msg->data = NULL;  // protect the transferred data
    OBJ_RELEASE(peer->recv_msg);

    /* retrieve the cmd */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(&xfer, &cmd, &cnt, PMIX_CMD_T))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&xfer);
        return;
    }
    opal_output_verbose(2, pmix_server_output,
                        "%s recvd pmix cmd %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), cmd);
    switch(cmd) {
    case PMIX_ABORT_CMD:
        opal_output_verbose(2, pmix_server_output,
                            "%s recvd ABORT",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* unpack the status */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&xfer, &ret, &cnt, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        /* don't bother to unpack the message - we ignore this for now as the
         * proc should have emitted it for itself */
        memcpy(&name, &id, sizeof(orte_process_name_t));
        /* go find the proc structure for this process */
        if (NULL == (jdata = orte_get_job_data_object(name.jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        } else {
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, name.vpid))) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            } else {
                proc->exit_code = ret;
                ORTE_FLAG_SET(proc, ORTE_PROC_FLAG_ABORT);
                ORTE_UPDATE_EXIT_STATUS(ret);
            }
        }
        /* we will let the ODLS report this to errmgr when the proc exits, so
         * send the release so the proc can depart */
        reply = OBJ_NEW(opal_buffer_t);
        /* pack the tag */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &tag, 1, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        PMIX_SERVER_QUEUE_SEND(peer, tag, reply);
        OBJ_DESTRUCT(&xfer);
        return;
    case PMIX_FENCE_CMD:
    case PMIX_FENCENB_CMD:
        opal_output_verbose(2, pmix_server_output,
                            "%s recvd %s FROM PROC %s ON TAG %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (PMIX_FENCENB_CMD == cmd) ? "FENCE_NB" : "FENCE",
                            OPAL_NAME_PRINT(id), tag);
        /* get the job and proc objects for the sender */
        memcpy((char*)&name, (char*)&id, sizeof(orte_process_name_t));
        if (NULL == (jdata = orte_get_job_data_object(name.jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, name.vpid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        /* setup a signature object */
        sig = OBJ_NEW(orte_grpcomm_signature_t);
        /* get the number of procs in this fence collective */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&xfer, &sig->sz, &cnt, OPAL_SIZE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(sig);
            goto reply_fence;
        }
        /* if a signature was provided, get it */
        if (0 < sig->sz) {
            sig->signature = (orte_process_name_t*)malloc(sig->sz * sizeof(orte_process_name_t));
            cnt = sig->sz;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&xfer, sig->signature, &cnt, OPAL_NAME))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(sig);
                goto reply_fence;
            }
        }
        if (4 < opal_output_get_verbosity(pmix_server_output)) {
            char *tmp=NULL;
            (void)opal_dss.print(&tmp, NULL, sig, ORTE_SIGNATURE);
            opal_output(0, "%s %s called with procs %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (PMIX_FENCENB_CMD == cmd) ? "FENCE_NB" : "FENCE", tmp);
            free(tmp);
        }
        /* unpack flag if sm dstore is supported by the client */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&xfer, &sm_flag, &cnt, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        /* if we are in a group collective mode, then we need to prep
         * the data as it should be included in the modex */
        OBJ_CONSTRUCT(&save, opal_buffer_t);
        if (orte_process_info.num_procs < orte_direct_modex_cutoff) {
            /* need to include the id of the sender for later unpacking */
            opal_dss.pack(&save, &id, 1, OPAL_NAME);
            opal_dss.copy_payload(&save, &xfer);
        }

        /* mark if peer proc has access to shared memory region*/
        if (1 == sm_flag) {
            ORTE_FLAG_SET(proc, ORTE_PROC_FLAG_SM_ACCESS);
        }

        /* if data was given, unpack and store it in the pmix dstore - it is okay
         * if there was no data, it's just a fence */
        cnt = 1;
        found = false;
        OBJ_CONSTRUCT(&blocal, opal_buffer_t);
        OBJ_CONSTRUCT(&bremote, opal_buffer_t);
        while (OPAL_SUCCESS == (rc = opal_dss.unpack(&xfer, &scope, &cnt, PMIX_SCOPE_T))) {
            found = true;  // at least one block of data is present
            /* unpack the buffer */
            cnt = 1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&xfer, &bptr, &cnt, OPAL_BUFFER))) {
                OPAL_ERROR_LOG(rc);
                OBJ_DESTRUCT(&xfer);
                OBJ_RELEASE(sig);
                return;
            }
            /* prep the value_t */
            OBJ_CONSTRUCT(&kv, opal_value_t);
            kv.key = strdup("modex");
            kv.type = OPAL_BYTE_OBJECT;
            kv.data.bo.bytes = (uint8_t*)bptr->base_ptr;
            kv.data.bo.size = bptr->bytes_used;
            if (PMIX_LOCAL == scope) {
                /* store it in the local-modex dstore handle */
                opal_output_verbose(2, pmix_server_output,
                                    "%s recvd LOCAL modex of size %d for proc %s",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    (int)kv.data.bo.size,
                                    ORTE_NAME_PRINT(&peer->name));
                handle = pmix_server_local_handle;
                /* local procs will want this data */
                if (OPAL_SUCCESS != (rc = opal_dss.pack(&blocal, &bptr, 1, OPAL_BUFFER))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&xfer);
                    OBJ_RELEASE(sig);
                    OBJ_DESTRUCT(&blocal);
                    OBJ_DESTRUCT(&bremote);
                    return;
                }
            } else if (PMIX_REMOTE == scope) {
                /* store it in the remote-modex dstore handle */
                opal_output_verbose(2, pmix_server_output,
                                    "%s recvd REMOTE modex of size %d for proc %s",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    (int)kv.data.bo.size,
                                    ORTE_NAME_PRINT(&peer->name));
                handle = pmix_server_remote_handle;
                /* remote procs will want this data */
                if (OPAL_SUCCESS != (rc = opal_dss.pack(&bremote, &bptr, 1, OPAL_BUFFER))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&xfer);
                    OBJ_RELEASE(sig);
                    OBJ_DESTRUCT(&blocal);
                    OBJ_DESTRUCT(&bremote);
                    return;
                }
            } else {
                /* must be for global dissemination */
                opal_output_verbose(2, pmix_server_output,
                                    "%s recvd GLOBAL modex of size %d for proc %s",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    (int)kv.data.bo.size,
                                    ORTE_NAME_PRINT(&peer->name));
                handle = pmix_server_global_handle;
                /* local procs will want this data */
                if (OPAL_SUCCESS != (rc = opal_dss.pack(&blocal, &bptr, 1, OPAL_BUFFER))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&xfer);
                    OBJ_RELEASE(sig);
                    OBJ_DESTRUCT(&blocal);
                    OBJ_DESTRUCT(&bremote);
                    return;
                }
                /* remote procs will want this data */
                if (OPAL_SUCCESS != (rc = opal_dss.pack(&bremote, &bptr, 1, OPAL_BUFFER))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&xfer);
                    OBJ_RELEASE(sig);
                    OBJ_DESTRUCT(&blocal);
                    OBJ_DESTRUCT(&bremote);
                    return;
                }
            }
            if (OPAL_SUCCESS != (rc = opal_dstore.store(handle, &id, &kv))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&kv);
                OBJ_DESTRUCT(&xfer);
                OBJ_RELEASE(sig);
                return;
            }
            bptr->base_ptr = NULL;  // protect the data region
            OBJ_RELEASE(bptr);
            OBJ_DESTRUCT(&kv);
            cnt = 1;
        }
        if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            OPAL_ERROR_LOG(rc);
        }
        /* mark that we recvd data for this proc */
        ORTE_FLAG_SET(proc, ORTE_PROC_FLAG_DATA_RECVD);
        /* see if anyone is waiting for it - we send a response even if no data
         * was actually provided so we don't hang if no modex data is being given */
        OPAL_LIST_FOREACH_SAFE(req, nextreq, &pmix_server_pending_dmx_reqs, pmix_server_dmx_req_t) {
            if (0 == opal_compare_proc(id, req->target)) {
                /* yes - deliver a copy */
                reply = OBJ_NEW(opal_buffer_t);
                if (NULL == req->proxy) {
                    /* get the proc object for the peer */
                    proc_peer = orte_get_proc_object(&req->peer->name);
                    /* pack the status */
                    ret = OPAL_SUCCESS;
                    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &ret, 1, OPAL_INT))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(reply);
                        OBJ_RELEASE(sig);
                        return;
                    }
                    /* check if the peer has an access to shared memory dstore segment */
                    if (!ORTE_FLAG_TEST(proc_peer, ORTE_PROC_FLAG_SM_ACCESS)) {
                        /* always pass the hostname */
                        OBJ_CONSTRUCT(&buf, opal_buffer_t);
                        OBJ_CONSTRUCT(&kv, opal_value_t);
                        kv.key = strdup(PMIX_HOSTNAME);
                        kv.type = OPAL_STRING;
                        kv.data.string = strdup(orte_process_info.nodename);
                        kp = &kv;
                        if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &kp, 1, OPAL_VALUE))) {
                            ORTE_ERROR_LOG(rc);
                            OBJ_RELEASE(reply);
                            OBJ_DESTRUCT(&buf);
                            OBJ_DESTRUCT(&kv);
                            OBJ_RELEASE(sig);
                            return;
                        }
                        OBJ_DESTRUCT(&kv);
                        /* pack the blob */
                        bptr = &buf;
                        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &bptr, 1, OPAL_BUFFER))) {
                            ORTE_ERROR_LOG(rc);
                            OBJ_RELEASE(reply);
                            OBJ_DESTRUCT(&xfer);
                            OBJ_DESTRUCT(&buf);
                            OBJ_RELEASE(sig);
                            return;
                        }
                        OBJ_DESTRUCT(&buf);
                        /* pass the local blob(s) */
                        opal_dss.copy_payload(reply, &blocal);
                    } else {
                        /* pack reply: info about meta segment for the target process */
                        rc = pack_segment_info(id, reply);
                        if (OPAL_SUCCESS != rc) {
                            OPAL_ERROR_LOG(rc);
                            OBJ_RELEASE(reply);
                            OBJ_DESTRUCT(&xfer);
                            OBJ_RELEASE(sig);
                            return;
                        }
                        if (!ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_DATA_IN_SM)) {
                            data = OBJ_NEW(opal_buffer_t);
                            /* always pass the hostname */
                            OBJ_CONSTRUCT(&buf, opal_buffer_t);
                            OBJ_CONSTRUCT(&kv, opal_value_t);
                            kv.key = strdup(PMIX_HOSTNAME);
                            kv.type = OPAL_STRING;
                            kv.data.string = strdup(orte_process_info.nodename);
                            kp = &kv;
                            if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &kp, 1, OPAL_VALUE))) {
                                ORTE_ERROR_LOG(rc);
                                OBJ_RELEASE(reply);
                                OBJ_RELEASE(data);
                                OBJ_DESTRUCT(&buf);
                                OBJ_DESTRUCT(&kv);
                                OBJ_RELEASE(sig);
                                OBJ_DESTRUCT(&xfer);
                                return;
                            }
                            OBJ_DESTRUCT(&kv);
                            /* pack the blob */
                            bptr = &buf;
                            if (OPAL_SUCCESS != (rc = opal_dss.pack(data, &bptr, 1, OPAL_BUFFER))) {
                                ORTE_ERROR_LOG(rc);
                                OBJ_RELEASE(reply);
                                OBJ_RELEASE(data);
                                OBJ_DESTRUCT(&xfer);
                                OBJ_DESTRUCT(&buf);
                                OBJ_RELEASE(sig);
                                return;
                            }
                            OBJ_DESTRUCT(&buf);
                            /* pass the local blob(s) */
                            opal_dss.copy_payload(data, &blocal);
                            opal_value_t kvp;
                            OBJ_CONSTRUCT(&kvp, opal_value_t);
                            kvp.key = strdup("finalval");
                            kvp.type = OPAL_BYTE_OBJECT;
                            kvp.data.bo.bytes = (uint8_t*)(data->base_ptr);
                            kvp.data.bo.size = data->bytes_used;
                            kvp.data.bo.bytes = NULL;
                            kvp.data.bo.size = 0;
                            /* store data in the shared memory dstore segment */
                            if (OPAL_SUCCESS != (rc = opal_dstore.store(opal_dstore_modex, &id, &kvp))) {
                                ORTE_ERROR_LOG(rc);
                                OBJ_RELEASE(reply);
                                OBJ_RELEASE(data);
                                OBJ_DESTRUCT(&xfer);
                                OBJ_DESTRUCT(&kvp);
                                OBJ_RELEASE(sig);
                                return;
                            }
                            OBJ_DESTRUCT(&kvp);
                            /* mark that we put data for this proc to shared memory region */
                            ORTE_FLAG_SET(proc, ORTE_PROC_FLAG_DATA_IN_SM);
                            OBJ_RELEASE(data);
                        }
                    }
                    /* use the PMIX send to return the data */
                    PMIX_SERVER_QUEUE_SEND(req->peer, req->tag, reply);
                } else {
                    /* pack the id of the requested proc */
                    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &id, 1, OPAL_NAME))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(reply);
                        OBJ_DESTRUCT(&xfer);
                        OBJ_RELEASE(sig);
                        return;
                    }
                    /* pack the status */
                    ret = OPAL_SUCCESS;
                    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &ret, 1, OPAL_INT))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(reply);
                        return;
                    }
                    /* always pass the hostname */
                    OBJ_CONSTRUCT(&buf, opal_buffer_t);
                    OBJ_CONSTRUCT(&kv, opal_value_t);
                    kv.key = strdup(PMIX_HOSTNAME);
                    kv.type = OPAL_STRING;
                    kv.data.string = strdup(orte_process_info.nodename);
                    kp = &kv;
                    if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &kp, 1, OPAL_VALUE))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(reply);
                        OBJ_DESTRUCT(&buf);
                        OBJ_DESTRUCT(&kv);
                        return;
                    }
                    OBJ_DESTRUCT(&kv);
                    /* pack the hostname blob */
                    bptr = &buf;
                    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &bptr, 1, OPAL_BUFFER))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(reply);
                        OBJ_DESTRUCT(&xfer);
                        OBJ_DESTRUCT(&buf);
                        return;
                    }
                    OBJ_DESTRUCT(&buf);
                    /* pass the remote blob(s) */
                    opal_dss.copy_payload(reply, &bremote);
                    /* use RML to send the response */
                    orte_rml.send_buffer_nb(&req->proxy->name, reply,
                                            ORTE_RML_TAG_DIRECT_MODEX_RESP,
                                            orte_rml_send_callback, NULL);
                }
                opal_list_remove_item(&pmix_server_pending_dmx_reqs, &req->super);
                OBJ_RELEASE(req);
            }
        }
        OBJ_DESTRUCT(&blocal);
        OBJ_DESTRUCT(&bremote);

        /* send notification to myself */
        reply = OBJ_NEW(opal_buffer_t);
        /* pack the id of the sender */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &id, 1, OPAL_NAME))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            OBJ_RELEASE(sig);
            goto reply_fence;
        }
        /* pack the socket of the sender */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &peer->sd, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            OBJ_RELEASE(sig);
            goto reply_fence;
        }
        /* pass the tag that this sender is sitting on */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &tag, 1, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            OBJ_RELEASE(sig);
            goto reply_fence;
        }
        /* pack the signature */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &sig, 1, ORTE_SIGNATURE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            OBJ_RELEASE(sig);
            goto reply_fence;
        }
        OBJ_RELEASE(sig);
        /* include any data that is to be globally shared */
        if (found && 0 < save.bytes_used) {
            bptr = &save;
            if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &bptr, 1, OPAL_BUFFER))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(reply);
                goto reply_fence;
            }
        }
        OBJ_DESTRUCT(&save);
        /* send it to myself for processing */
        orte_rml.send_buffer_nb(ORTE_PROC_MY_NAME, reply,
                                ORTE_RML_TAG_DAEMON_COLL,
                                orte_rml_send_callback, NULL);
        OBJ_DESTRUCT(&xfer);
        return;
    reply_fence:
        if (PMIX_FENCE_CMD == cmd) {
            /* send a release message back to the sender so they don't hang */
            reply = OBJ_NEW(opal_buffer_t);
            /* pack the tag */
            if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &tag, 1, OPAL_UINT32))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(reply);
                OBJ_DESTRUCT(&xfer);
                return;
            }
            PMIX_SERVER_QUEUE_SEND(peer, tag, reply);
        }
        OBJ_DESTRUCT(&xfer);
        return;

    case PMIX_GET_CMD:
        opal_output_verbose(2, pmix_server_output,
                            "%s recvd GET",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* get the proc object for the peer */
        memcpy((char*)&name, (char*)&id, sizeof(orte_process_name_t));
        proc_peer = orte_get_proc_object(&name);
        /* unpack the id of the proc whose data is being requested */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&xfer, &idreq, &cnt, OPAL_NAME))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        /* lookup the proc object */
        memcpy((char*)&name, (char*)&idreq, sizeof(orte_process_name_t));
        opal_output_verbose(2, pmix_server_output,
                            "%s recvd GET FROM PROC %s FOR PROC %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT((orte_process_name_t*)&id),
                            ORTE_NAME_PRINT(&name));
        if (NULL == (jdata = orte_get_job_data_object(name.jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, name.vpid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            OBJ_DESTRUCT(&xfer);
            return;
        }

        sm_flag = 0;
        if (ORTE_FLAG_TEST(proc_peer, ORTE_PROC_FLAG_SM_ACCESS)) {
            sm_flag = 1;
        }
        /* if we have already stored data for this proc in shared memory region,
         * then we just need to send a response */
        if (1 == sm_flag && ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_DATA_IN_SM) && ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_LOCAL)) {
            reply = OBJ_NEW(opal_buffer_t);
            ret = OPAL_SUCCESS;
            /* pack the error status */
            if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &ret, 1, OPAL_INT))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&xfer);
                OBJ_RELEASE(reply);
                return;
            }
            /* pack reply: info about meta segment for the target process */
            rc = pack_segment_info(idreq, reply);
            if (OPAL_SUCCESS != rc) {
                OPAL_ERROR_LOG(rc);
                OBJ_DESTRUCT(&xfer);
                OBJ_RELEASE(reply);
                return;
            }
            PMIX_SERVER_QUEUE_SEND(peer, tag, reply);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        /* if we have not yet received data for this proc, then we just
         * need to track the request */
        if (!ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_DATA_RECVD)) {
            /* are we already tracking it? */
            found = false;
            OPAL_LIST_FOREACH(req, &pmix_server_pending_dmx_reqs, pmix_server_dmx_req_t) {
                if (0 == opal_compare_proc(idreq, req->target)) {
                    /* yes, so we don't need to send another request, but
                     * we do need to track that this peer also wants
                     * a copy */
                    found = true;
                    break;
                }
            }
            /* track the request */
            req = OBJ_NEW(pmix_server_dmx_req_t);
            OBJ_RETAIN(peer);  // just to be safe
            req->peer = peer;
            req->target = idreq;
            req->tag = tag;
            opal_list_append(&pmix_server_pending_dmx_reqs, &req->super);
            if (!found) {
                /* this is a new tracker - see if we need to send a data
                 * request to some remote daemon to resolve it */
                if (!ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_LOCAL)) {
                    /* nope - who is hosting this proc */
                    if (NULL == proc->node || NULL == proc->node->daemon) {
                        /* we are hosed - pack an error and return it */
                        reply = OBJ_NEW(opal_buffer_t);
                        ret = ORTE_ERR_NOT_FOUND;
                        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &ret, 1, OPAL_INT))) {
                            ORTE_ERROR_LOG(rc);
                            OBJ_DESTRUCT(&xfer);
                            OBJ_RELEASE(reply);
                            return;
                        }
                        PMIX_SERVER_QUEUE_SEND(peer, tag, reply);
                        OBJ_DESTRUCT(&xfer);
                        return;
                    }
                    /* setup the request */
                    reply = OBJ_NEW(opal_buffer_t);
                    /* pack the proc we want info about */
                    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &idreq, 1, OPAL_NAME))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_DESTRUCT(&xfer);
                        return;
                    }
                    /* send the request - the recv will come back elsewhere
                     * and reply to the original requestor */
                    orte_rml.send_buffer_nb(&proc->node->daemon->name, reply,
                                            ORTE_RML_TAG_DIRECT_MODEX,
                                            orte_rml_send_callback, NULL);
                }
            }
            /* nothing further to do as we are waiting for data */
            OBJ_DESTRUCT(&xfer);
            return;
        }

        /* regardless of where this proc is located, we need to ensure
         * that the hostname it is on is *always* returned. Otherwise,
         * the non-blocking fence operation will cause us to fail if
         * the number of procs is below the cutoff as we will immediately
         * attempt to retrieve the hostname for each proc, but they may
         * not have posted their data by that time */
        if (ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_LOCAL)) {
            opal_output_verbose(2, pmix_server_output,
                                "%s recvd GET PROC %s IS LOCAL",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&name));
            kvp = NULL;
            kvp2 = NULL;
            data = NULL;
            /* retrieve the local blob for that proc */
            OBJ_CONSTRUCT(&values, opal_list_t);
            if (OPAL_SUCCESS == (ret = opal_dstore.fetch(pmix_server_local_handle, &idreq, "modex", &values))) {
                kvp = (opal_value_t*)opal_list_remove_first(&values);
            }
            OPAL_LIST_DESTRUCT(&values);
            /* retrieve the global blob for that proc */
            OBJ_CONSTRUCT(&values, opal_list_t);
            if (OPAL_SUCCESS == (ret = opal_dstore.fetch(pmix_server_global_handle, &idreq, "modex", &values))) {
                kvp2 = (opal_value_t*)opal_list_remove_first(&values);
            }
            OPAL_LIST_DESTRUCT(&values);
            /* return it */
            reply = OBJ_NEW(opal_buffer_t);
            /* pack the status */
            if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &ret, 1, OPAL_INT))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(reply);
                OBJ_DESTRUCT(&xfer);
                return;
            }
            /* pass the hostname */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            OBJ_CONSTRUCT(&kv, opal_value_t);
            kv.key = strdup(PMIX_HOSTNAME);
            kv.type = OPAL_STRING;
            kv.data.string = strdup(orte_process_info.nodename);
            kp = &kv;
            if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &kp, 1, OPAL_VALUE))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(reply);
                OBJ_DESTRUCT(&xfer);
                OBJ_DESTRUCT(&buf);
                OBJ_DESTRUCT(&kv);
                return;
            }
            OBJ_DESTRUCT(&kv);
            /* pack the blob */
            bptr = &buf;
            if (0 == sm_flag) {
                if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &bptr, 1, OPAL_BUFFER))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(reply);
                    OBJ_DESTRUCT(&xfer);
                    OBJ_DESTRUCT(&buf);
                    return;
                }
            } else {
                if (NULL == data) {
                    data = OBJ_NEW(opal_buffer_t);
                }
                if (OPAL_SUCCESS != (rc = opal_dss.pack(data, &bptr, 1, OPAL_BUFFER))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(reply);
                    OBJ_RELEASE(data);
                    OBJ_DESTRUCT(&xfer);
                    OBJ_DESTRUCT(&buf);
                    return;
                }
            }

            OBJ_DESTRUCT(&buf);
            /* local blob */
            if (NULL != kvp) {
                opal_output_verbose(2, pmix_server_output,
                                    "%s passing local blob of size %d from proc %s to proc %s",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    (int)kvp->data.bo.size,
                                    ORTE_NAME_PRINT(&name),
                                    ORTE_NAME_PRINT(&peer->name));
                OBJ_CONSTRUCT(&buf, opal_buffer_t);
                opal_dss.load(&buf, kvp->data.bo.bytes, kvp->data.bo.size);
                /* protect the data */
                kvp->data.bo.bytes = NULL;
                kvp->data.bo.size = 0;
                bptr = &buf;
                if (0 == sm_flag) {
                    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &bptr, 1, OPAL_BUFFER))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(reply);
                        OBJ_RELEASE(data);
                        OBJ_DESTRUCT(&xfer);
                        OBJ_DESTRUCT(&buf);
                        OBJ_RELEASE(kvp);
                        return;
                    }
                } else {
                    if (NULL == data) {
                        data = OBJ_NEW(opal_buffer_t);
                    }
                    if (OPAL_SUCCESS != (rc = opal_dss.pack(data, &bptr, 1, OPAL_BUFFER))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(reply);
                        OBJ_RELEASE(data);
                        OBJ_DESTRUCT(&xfer);
                        OBJ_DESTRUCT(&buf);
                        OBJ_RELEASE(kvp);
                        return;
                    }
                }
                OBJ_DESTRUCT(&buf);
                OBJ_RELEASE(kvp);
            }
            /* global blob */
            if (NULL != kvp2) {
                opal_output_verbose(2, pmix_server_output,
                                    "%s passing global blob of size %d from proc %s to proc %s",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    (int)kvp2->data.bo.size,
                                    ORTE_NAME_PRINT(&name),
                                    ORTE_NAME_PRINT(&peer->name));
                OBJ_CONSTRUCT(&buf, opal_buffer_t);
                opal_dss.load(&buf, kvp2->data.bo.bytes, kvp2->data.bo.size);
                /* protect the data */
                kvp2->data.bo.bytes = NULL;
                kvp2->data.bo.size = 0;
                bptr = &buf;
                if (0 == sm_flag) {
                    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &bptr, 1, OPAL_BUFFER))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(reply);
                        OBJ_RELEASE(data);
                        OBJ_DESTRUCT(&xfer);
                        OBJ_DESTRUCT(&buf);
                        OBJ_RELEASE(kvp2);
                        return;
                    }
                } else {
                    if (NULL == data) {
                        data = OBJ_NEW(opal_buffer_t);
                    }
                    if (OPAL_SUCCESS != (rc = opal_dss.pack(data, &bptr, 1, OPAL_BUFFER))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(reply);
                        OBJ_RELEASE(data);
                        OBJ_DESTRUCT(&xfer);
                        OBJ_DESTRUCT(&buf);
                        OBJ_RELEASE(kvp2);
                        return;
                    }
                }
                OBJ_DESTRUCT(&buf);
                OBJ_RELEASE(kvp2);
            }
            if (1 == sm_flag) {
                /* pack reply: info about meta segment for the target process */
                rc = pack_segment_info(idreq, reply);
                if (OPAL_SUCCESS != rc) {
                    OPAL_ERROR_LOG(rc);
                    OBJ_RELEASE(reply);
                    OBJ_RELEASE(data);
                    OBJ_DESTRUCT(&xfer);
                    return;
                }
                opal_value_t kvf;
                OBJ_CONSTRUCT(&kvf, opal_value_t);
                kvf.key = strdup("finalval");
                kvf.type = OPAL_BYTE_OBJECT;
                kvf.data.bo.bytes = (uint8_t*)(data->base_ptr);
                kvf.data.bo.size = data->bytes_used;
                /* store data in the shared memory dstore segment */
                if (OPAL_SUCCESS != (rc = opal_dstore.store(opal_dstore_modex, &idreq, &kvf))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(reply);
                    OBJ_RELEASE(data);
                    OBJ_DESTRUCT(&xfer);
                    OBJ_DESTRUCT(&kvf);
                    return;
                }
                /* protect the data */
                kvf.data.bo.bytes = NULL;
                kvf.data.bo.size = 0;
                OBJ_DESTRUCT(&kvf);
                /* mark that we put data for this proc to shared memory region */
                ORTE_FLAG_SET(proc, ORTE_PROC_FLAG_DATA_IN_SM);
                OBJ_RELEASE(data);

            }
            PMIX_SERVER_QUEUE_SEND(peer, tag, reply);
            OBJ_DESTRUCT(&xfer);

            return;
        }

        opal_output_verbose(2, pmix_server_output,
                            "%s recvd GET PROC %s IS NON-LOCAL",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&name));
        OBJ_DESTRUCT(&xfer);  // done with this
        /* since we already have this proc's data, we know that the
         * entire blob is stored in the remote handle - so get it */
        OBJ_CONSTRUCT(&values, opal_list_t);
        if (OPAL_SUCCESS == (ret = opal_dstore.fetch(pmix_server_remote_handle, &idreq, "modex", &values))) {
            kvp = (opal_value_t*)opal_list_remove_first(&values);
            OPAL_LIST_DESTRUCT(&values);
            opal_output_verbose(2, pmix_server_output,
                                "%s passing blob of size %d from remote proc %s to proc %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                (int)kvp->data.bo.size,
                                ORTE_NAME_PRINT(&name),
                                ORTE_NAME_PRINT(&peer->name));
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            opal_dss.load(&buf, kvp->data.bo.bytes, kvp->data.bo.size);
            /* protect the data */
            kvp->data.bo.bytes = NULL;
            kvp->data.bo.size = 0;
            OBJ_RELEASE(kvp);
            reply = OBJ_NEW(opal_buffer_t);
            /* pack the status */
            if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &ret, 1, OPAL_INT))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(reply);
                OBJ_DESTRUCT(&buf);
                return;
            }
            /* xfer the data - the blobs are in the buffer,
             * so don't repack them. They will include the remote
             * hostname, so don't add it again */
            if (0 == sm_flag) {
                opal_dss.copy_payload(reply, &buf);
            } else {
                data = OBJ_NEW(opal_buffer_t);
                opal_dss.copy_payload(data, &buf);
            }
            OBJ_DESTRUCT(&buf);
            if (1 == sm_flag) {
                /* pack reply: info about meta segment for the target process */
                rc = pack_segment_info(idreq, reply);
                if (OPAL_SUCCESS != rc) {
                    OPAL_ERROR_LOG(rc);
                    OBJ_RELEASE(reply);
                    OBJ_RELEASE(data);
                    return;
                }
                opal_value_t kvf;
                OBJ_CONSTRUCT(&kvf, opal_value_t);
                kvf.key = strdup("finalval");
                kvf.type = OPAL_BYTE_OBJECT;
                kvf.data.bo.bytes = (uint8_t*)(data->base_ptr);
                kvf.data.bo.size = data->bytes_used;
                /* store data into shared memory dstore segment */
                if (OPAL_SUCCESS != (rc = opal_dstore.store(opal_dstore_modex, &idreq, &kvf))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(reply);
                    OBJ_RELEASE(data);
                    OBJ_DESTRUCT(&kvf);
                    return;
                }
                /* protect the data */
                kvf.data.bo.bytes = NULL;
                kvf.data.bo.size = 0;
                OBJ_DESTRUCT(&kvf);
                /* mark that we put data for this proc to shared memory region */
                ORTE_FLAG_SET(proc, ORTE_PROC_FLAG_DATA_IN_SM);
                OBJ_RELEASE(data);
            }
            PMIX_SERVER_QUEUE_SEND(peer, tag, reply);
            return;
        }
        OPAL_LIST_DESTRUCT(&values);
        /* if we get here, then the data should have been there, but wasn't found
         * for some bizarre reason - pass back an error to ensure we don't block */
        reply = OBJ_NEW(opal_buffer_t);
        /* pack the error status */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &ret, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            return;
        }
        PMIX_SERVER_QUEUE_SEND(peer, tag, reply);
        return;

    case PMIX_GETATTR_CMD:
        opal_output_verbose(2, pmix_server_output,
                            "%s recvd GETATTR",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* create the attrs buffer */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        /* look up this proc */
        memcpy((char*)&name, (char*)&id, sizeof(orte_process_name_t));
        if (NULL == (jdata = orte_get_job_data_object(name.jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            OBJ_DESTRUCT(&buf);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, name.vpid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            OBJ_DESTRUCT(&buf);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        /* mark the proc as having registered */
        ORTE_ACTIVATE_PROC_STATE(&proc->name, ORTE_PROC_STATE_REGISTERED);
        /* stuff the values corresponding to the list of supported attrs */
        if (ORTE_SUCCESS != (ret = pmix_server_fetch_proc_map(&buf, jdata, proc))) {
            ORTE_ERROR_LOG(ret);
            OBJ_DESTRUCT(&buf);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        /* return it */
        reply = OBJ_NEW(opal_buffer_t);
        /* pack the status */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &ret, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        if (OPAL_SUCCESS == ret) {
            /* pack the buffer */
            bptr = &buf;
            if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &bptr, 1, OPAL_BUFFER))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(reply);
                OBJ_DESTRUCT(&xfer);
                OBJ_DESTRUCT(&buf);
                return;
            }
        }
        OBJ_DESTRUCT(&buf);
        PMIX_SERVER_QUEUE_SEND(peer, tag, reply);
        OBJ_DESTRUCT(&xfer);
        return;

    case PMIX_FINALIZE_CMD:
        opal_output_verbose(2, pmix_server_output,
                            "%s recvd FINALIZE",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* look up this proc */
        memcpy((char*)&name, (char*)&id, sizeof(orte_process_name_t));
        if (NULL == (jdata = orte_get_job_data_object(name.jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            OBJ_DESTRUCT(&buf);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, name.vpid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            OBJ_DESTRUCT(&buf);
            OBJ_DESTRUCT(&xfer);
            return;
        }
        /* mark the proc as having deregistered */
        ORTE_FLAG_SET(proc, ORTE_PROC_FLAG_HAS_DEREG);
        /* send the release */
        reply = OBJ_NEW(opal_buffer_t);
        PMIX_SERVER_QUEUE_SEND(peer, tag, reply);
        OBJ_DESTRUCT(&xfer);
        break;

    default:
        ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
        OBJ_DESTRUCT(&xfer);
        return;
    }
}

