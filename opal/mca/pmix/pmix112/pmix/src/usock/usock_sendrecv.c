/*
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include <src/include/pmix_config.h>

#include <src/include/types.h>
#include <pmix/autogen/pmix_stdint.h>
#include <src/include/pmix_socket_errno.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "src/class/pmix_pointer_array.h"
#include "src/include/pmix_globals.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/error.h"

#include "usock.h"

static uint32_t current_tag = 1;  // 0 is reserved for system purposes

static void lost_connection(pmix_peer_t *peer, pmix_status_t err)
{
    pmix_server_trkr_t *trk;
    pmix_rank_info_t *rinfo, *rnext;
    pmix_trkr_caddy_t *tcd;
    pmix_error_reg_info_t *errreg;
    pmix_regevents_info_t *reginfoptr, *regnext;

    /* stop all events */
    if (peer->recv_ev_active) {
        event_del(&peer->recv_event);
        peer->recv_ev_active = false;
    }
    if (peer->send_ev_active) {
        event_del(&peer->send_event);
        peer->send_ev_active = false;
    }
    if (NULL != peer->recv_msg) {
        PMIX_RELEASE(peer->recv_msg);
        peer->recv_msg = NULL;
    }
    CLOSE_THE_SOCKET(peer->sd);
    if (pmix_globals.server) {
        /* if I am a server, then we need to ensure that
         * we properly account for the loss of this client
         * from any local collectives in which it was
         * participating - note that the proc would not
         * have been added to any collective tracker until
         * after it successfully connected */
        PMIX_LIST_FOREACH(trk, &pmix_server_globals.collectives, pmix_server_trkr_t) {
            /* see if this proc is participating in this tracker */
            PMIX_LIST_FOREACH_SAFE(rinfo, rnext, &trk->ranks, pmix_rank_info_t) {
                if (0 != strncmp(rinfo->nptr->nspace, peer->info->nptr->nspace, PMIX_MAX_NSLEN)) {
                    continue;
                }
                if (rinfo->rank != peer->info->rank) {
                    continue;
                }
                /* it is - adjust the count */
                --trk->nlocal;
                /* remove it from the list */
                pmix_list_remove_item(&trk->ranks, &rinfo->super);
                PMIX_RELEASE(rinfo);
                /* check for completion */
                if (pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
                    /* complete, so now we need to process it
                     * we don't want to block someone
                     * here, so kick any completed trackers into a
                     * new event for processing */
                    PMIX_EXECUTE_COLLECTIVE(tcd, trk, pmix_server_execute_collective);
                }
            }
        }
        /* remove this proc from the list of ranks for this nspace */
        pmix_list_remove_item(&(peer->info->nptr->server->ranks), &(peer->info->super));
        PMIX_RELEASE(peer->info);
        /* reduce the number of local procs */
        --peer->info->nptr->server->nlocalprocs;
        /* do some cleanup as the client has left us */
        pmix_pointer_array_set_item(&pmix_server_globals.clients,
                                     peer->index, NULL);
        /* remove all registered event handlers so libevent doesn't complain */
        PMIX_LIST_FOREACH_SAFE(reginfoptr, regnext, &pmix_server_globals.client_eventregs, pmix_regevents_info_t) {
            if (reginfoptr->peer == peer) {
                pmix_list_remove_item(&pmix_server_globals.client_eventregs, &reginfoptr->super);
                PMIX_RELEASE(reginfoptr);
                break;
            }
        }
        PMIX_RELEASE(peer);
     } else {
        /* if I am a client, there is only
         * one connection we can have */
        pmix_globals.connected = false;
    }
    PMIX_REPORT_ERROR(err);
}

static pmix_status_t send_bytes(int sd, char **buf, size_t *remain)
{
    pmix_status_t ret = PMIX_SUCCESS;
    int rc;
    char *ptr = *buf;
    while (0 < *remain) {
        rc = write(sd, ptr, *remain);
        if (rc < 0) {
            if (pmix_socket_errno == EINTR) {
                continue;
            } else if (pmix_socket_errno == EAGAIN) {
                /* tell the caller to keep this message on active,
                 * but let the event lib cycle so other messages
                 * can progress while this socket is busy
                 */
                ret = PMIX_ERR_RESOURCE_BUSY;
                goto exit;
            } else if (pmix_socket_errno == EWOULDBLOCK) {
                /* tell the caller to keep this message on active,
                 * but let the event lib cycle so other messages
                 * can progress while this socket is busy
                 */
                ret = PMIX_ERR_WOULD_BLOCK;
                goto exit;
            }
            /* we hit an error and cannot progress this message */
            pmix_output(0, "pmix_usock_msg_send_bytes: write failed: %s (%d) [sd = %d]",
                        strerror(pmix_socket_errno),
                        pmix_socket_errno, sd);
            ret = PMIX_ERR_COMM_FAILURE;
            goto exit;
        }
        /* update location */
        (*remain) -= rc;
        ptr += rc;
    }
    /* we sent the full data block */
exit:
    *buf = ptr;
    return ret;
}

static pmix_status_t read_bytes(int sd, char **buf, size_t *remain)
{
    pmix_status_t ret = PMIX_SUCCESS;
    int rc;
    char *ptr = *buf;

    /* read until all bytes recvd or error */
    while (0 < *remain) {
        rc = read(sd, ptr, *remain);
        if (rc < 0) {
            if(pmix_socket_errno == EINTR) {
                continue;
            } else if (pmix_socket_errno == EAGAIN) {
                /* tell the caller to keep this message on active,
                 * but let the event lib cycle so other messages
                 * can progress while this socket is busy
                 */
                ret = PMIX_ERR_RESOURCE_BUSY;
                goto exit;
            } else if (pmix_socket_errno == EWOULDBLOCK) {
                /* tell the caller to keep this message on active,
                 * but let the event lib cycle so other messages
                 * can progress while this socket is busy
                 */
                ret = PMIX_ERR_WOULD_BLOCK;
                goto exit;
            }
            /* we hit an error and cannot progress this message - report
             * the error back to the RML and let the caller know
             * to abort this message
             */
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix_usock_msg_recv: readv failed: %s (%d)",
                                strerror(pmix_socket_errno),
                                pmix_socket_errno);
            ret = PMIX_ERR_UNREACH;
            goto exit;
        } else if (0 == rc) {
            /* the remote peer closed the connection */
            ret = PMIX_ERR_UNREACH;
            goto exit;
        }
        /* we were able to read something, so adjust counters and location */
        *remain -= rc;
        ptr += rc;
    }
    /* we read the full data block */
exit:
    *buf = ptr;
    return ret;
}

/*
 * A file descriptor is available/ready for send. Check the state
 * of the socket and take the appropriate action.
 */
void pmix_usock_send_handler(int sd, short flags, void *cbdata)
{
    pmix_peer_t *peer = (pmix_peer_t*)cbdata;
    pmix_usock_send_t *msg = peer->send_msg;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sock:send_handler SENDING TO PEER %s:%d with %s msg",
                        peer->info->nptr->nspace, peer->info->rank,
                        (NULL == msg) ? "NULL" : "NON-NULL");
    if (NULL != msg) {
        if (!msg->hdr_sent) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "usock:send_handler SENDING HEADER");
            if (PMIX_SUCCESS == (rc = send_bytes(peer->sd, &msg->sdptr, &msg->sdbytes))) {
                /* header is completely sent */
                pmix_output_verbose(2, pmix_globals.debug_output,
                                    "usock:send_handler HEADER SENT");
                msg->hdr_sent = true;
                /* setup to send the data */
                if (NULL == msg->data) {
                    /* this was a zero-byte msg - nothing more to do */
                    PMIX_RELEASE(msg);
                    peer->send_msg = NULL;
                    goto next;
                } else {
                    /* send the data as a single block */
                    msg->sdptr = msg->data->base_ptr;
                    msg->sdbytes = msg->hdr.nbytes;
                }
                /* fall thru and let the send progress */
            } else if (PMIX_ERR_RESOURCE_BUSY == rc ||
                       PMIX_ERR_WOULD_BLOCK == rc) {
                /* exit this event and let the event lib progress */
                pmix_output_verbose(2, pmix_globals.debug_output,
                                    "usock:send_handler RES BUSY OR WOULD BLOCK");
                return;
            } else {
                // report the error
                event_del(&peer->send_event);
                peer->send_ev_active = false;
                PMIX_RELEASE(msg);
                peer->send_msg = NULL;
                lost_connection(peer, rc);
                return;
            }
        }

        if (msg->hdr_sent) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "usock:send_handler SENDING BODY OF MSG");
            if (PMIX_SUCCESS == (rc = send_bytes(peer->sd, &msg->sdptr, &msg->sdbytes))) {
                // message is complete
                pmix_output_verbose(2, pmix_globals.debug_output,
                                    "usock:send_handler BODY SENT");
                PMIX_RELEASE(msg);
                peer->send_msg = NULL;
            } else if (PMIX_ERR_RESOURCE_BUSY == rc ||
                       PMIX_ERR_WOULD_BLOCK == rc) {
                /* exit this event and let the event lib progress */
                pmix_output_verbose(2, pmix_globals.debug_output,
                                    "usock:send_handler RES BUSY OR WOULD BLOCK");
                return;
            } else {
                // report the error
                pmix_output(0, "pmix_usock_peer_send_handler: unable to send message ON SOCKET %d",
                            peer->sd);
                event_del(&peer->send_event);
                peer->send_ev_active = false;
                PMIX_RELEASE(msg);
                peer->send_msg = NULL;
                lost_connection(peer, rc);
                return;
            }
        }

    next:
        /* if current message completed - progress any pending sends by
         * moving the next in the queue into the "on-deck" position. Note
         * that this doesn't mean we send the message right now - we will
         * wait for another send_event to fire before doing so. This gives
         * us a chance to service any pending recvs.
         */
        peer->send_msg = (pmix_usock_send_t*)
            pmix_list_remove_first(&peer->send_queue);
    }

    /* if nothing else to do unregister for send event notifications */
    if (NULL == peer->send_msg && peer->send_ev_active) {
        event_del(&peer->send_event);
        peer->send_ev_active = false;
    }
}

/*
 * Dispatch to the appropriate action routine based on the state
 * of the connection with the peer.
 */

void pmix_usock_recv_handler(int sd, short flags, void *cbdata)
{
    pmix_status_t rc;
    pmix_peer_t *peer = (pmix_peer_t*)cbdata;
    pmix_usock_recv_t *msg = NULL;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "usock:recv:handler called with peer %s:%d",
                        (NULL == peer) ? "NULL" : peer->info->nptr->nspace,
                        (NULL == peer) ? -1 : peer->info->rank);

    if (NULL == peer) {
        return;
    }
    /* allocate a new message and setup for recv */
    if (NULL == peer->recv_msg) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "usock:recv:handler allocate new recv msg");
        peer->recv_msg = PMIX_NEW(pmix_usock_recv_t);
        if (NULL == peer->recv_msg) {
            pmix_output(0, "usock_recv_handler: unable to allocate recv message\n");
            goto err_close;
        }
        peer->recv_msg->peer = peer;  // provide a handle back to the peer object
        /* start by reading the header */
        peer->recv_msg->rdptr = (char*)&peer->recv_msg->hdr;
        peer->recv_msg->rdbytes = sizeof(pmix_usock_hdr_t);
    }
    msg = peer->recv_msg;
    msg->sd = sd;
    /* if the header hasn't been completely read, read it */
    if (!msg->hdr_recvd) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "usock:recv:handler read hdr on socket %d", peer->sd);
        if (PMIX_SUCCESS == (rc = read_bytes(peer->sd, &msg->rdptr, &msg->rdbytes))) {
            /* completed reading the header */
            peer->recv_msg->hdr_recvd = true;
            /* if this is a zero-byte message, then we are done */
            if (0 == peer->recv_msg->hdr.nbytes) {
                pmix_output_verbose(2, pmix_globals.debug_output,
                                    "RECVD ZERO-BYTE MESSAGE FROM %s:%d for tag %d",
                                    peer->info->nptr->nspace, peer->info->rank,
                                    peer->recv_msg->hdr.tag);
                peer->recv_msg->data = NULL;  // make sure
                peer->recv_msg->rdptr = NULL;
                peer->recv_msg->rdbytes = 0;
            } else {
                pmix_output_verbose(2, pmix_globals.debug_output,
                                    "usock:recv:handler allocate data region of size %lu",
                                    (unsigned long)peer->recv_msg->hdr.nbytes);
                /* allocate the data region */
                peer->recv_msg->data = (char*)malloc(peer->recv_msg->hdr.nbytes);
                memset(peer->recv_msg->data, 0, peer->recv_msg->hdr.nbytes);
                /* point to it */
                peer->recv_msg->rdptr = peer->recv_msg->data;
                peer->recv_msg->rdbytes = peer->recv_msg->hdr.nbytes;
            }
            /* fall thru and attempt to read the data */
        } else if (PMIX_ERR_RESOURCE_BUSY == rc ||
                   PMIX_ERR_WOULD_BLOCK == rc) {
            /* exit this event and let the event lib progress */
            return;
        } else {
            /* the remote peer closed the connection - report that condition
             * and let the caller know
             */
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix_usock_msg_recv: peer closed connection");
            goto err_close;
        }
    }

    if (peer->recv_msg->hdr_recvd) {
        /* continue to read the data block - we start from
         * wherever we left off, which could be at the
         * beginning or somewhere in the message
         */
        if (PMIX_SUCCESS == (rc = read_bytes(peer->sd, &msg->rdptr, &msg->rdbytes))) {
            /* we recvd all of the message */
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "RECVD COMPLETE MESSAGE FROM SERVER OF %d BYTES FOR TAG %d ON PEER SOCKET %d",
                                (int)peer->recv_msg->hdr.nbytes,
                                peer->recv_msg->hdr.tag, peer->sd);
            /* post it for delivery */
            PMIX_ACTIVATE_POST_MSG(peer->recv_msg);
            peer->recv_msg = NULL;
            return;
        } else if (PMIX_ERR_RESOURCE_BUSY == rc ||
                   PMIX_ERR_WOULD_BLOCK == rc) {
            /* exit this event and let the event lib progress */
            return;
        } else {
            /* the remote peer closed the connection - report that condition
             * and let the caller know
             */
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix_usock_msg_recv: peer closed connection");
            goto err_close;
        }
    }
    /* success */
    return;
 err_close:
    /* stop all events */
    if (peer->recv_ev_active) {
        event_del(&peer->recv_event);
        peer->recv_ev_active = false;
    }
    if (peer->send_ev_active) {
        event_del(&peer->send_event);
        peer->send_ev_active = false;
    }
    if (NULL != peer->recv_msg) {
        PMIX_RELEASE(peer->recv_msg);
        peer->recv_msg = NULL;
    }
    lost_connection(peer, PMIX_ERR_UNREACH);
}

void pmix_usock_send_recv(int fd, short args, void *cbdata)
{
    pmix_usock_sr_t *ms = (pmix_usock_sr_t*)cbdata;
    pmix_usock_posted_recv_t *req;
    pmix_usock_send_t *snd;
    uint32_t tag;

    /* set the tag */
    tag = current_tag++;

    if (NULL != ms->cbfunc) {
        /* if a callback msg is expected, setup a recv for it */
        req = PMIX_NEW(pmix_usock_posted_recv_t);
        /* take the next tag in the sequence */
        if (UINT32_MAX == current_tag ) {
            current_tag = 1;
        }
        req->tag = tag;
        req->cbfunc = ms->cbfunc;
        req->cbdata = ms->cbdata;
        pmix_output_verbose(5, pmix_globals.debug_output,
                            "posting recv on tag %d", req->tag);
        /* add it to the list of recvs - we cannot have unexpected messages
         * in this subsystem as the server never sends us something that
         * we didn't previously request */
        pmix_list_prepend(&pmix_usock_globals.posted_recvs, &req->super);
    }

    snd = PMIX_NEW(pmix_usock_send_t);
    snd->hdr.pindex = pmix_globals.pindex;
    snd->hdr.tag = tag;
    snd->hdr.nbytes = ms->bfr->bytes_used;
    snd->data = ms->bfr;
    /* always start with the header */
    snd->sdptr = (char*)&snd->hdr;
    snd->sdbytes = sizeof(pmix_usock_hdr_t);

    /* if there is no message on-deck, put this one there */
    if (NULL == ms->peer->send_msg) {
        ms->peer->send_msg = snd;
    } else {
        /* add it to the queue */
        pmix_list_append(&ms->peer->send_queue, &snd->super);
    }
    /* ensure the send event is active */
    if (!ms->peer->send_ev_active) {
        event_add(&ms->peer->send_event, 0);
        ms->peer->send_ev_active = true;
    }
    /* cleanup */
    PMIX_RELEASE(ms);
}

void pmix_usock_process_msg(int fd, short flags, void *cbdata)
{
    pmix_usock_recv_t *msg = (pmix_usock_recv_t*)cbdata;
    pmix_usock_posted_recv_t *rcv;
    pmix_buffer_t buf;

    pmix_output_verbose(5, pmix_globals.debug_output,
                        "message received %d bytes for tag %u on socket %d",
                        (int)msg->hdr.nbytes, msg->hdr.tag, msg->sd);

    /* see if we have a waiting recv for this message */
    PMIX_LIST_FOREACH(rcv, &pmix_usock_globals.posted_recvs, pmix_usock_posted_recv_t) {
        pmix_output_verbose(5, pmix_globals.debug_output,
                            "checking msg on tag %u for tag %u",
                            msg->hdr.tag, rcv->tag);

        if (msg->hdr.tag == rcv->tag || UINT_MAX == rcv->tag) {
            if (NULL != rcv->cbfunc) {
                /* construct and load the buffer */
                PMIX_CONSTRUCT(&buf, pmix_buffer_t);
                if (NULL != msg->data) {
                    buf.base_ptr = (char*)msg->data;
                    buf.bytes_allocated = buf.bytes_used = msg->hdr.nbytes;
                    buf.unpack_ptr = buf.base_ptr;
                    buf.pack_ptr = ((char*)buf.base_ptr) + buf.bytes_used;
                }
                msg->data = NULL;  // protect the data region
                if (NULL != rcv->cbfunc) {
                    rcv->cbfunc(msg->peer, &msg->hdr, &buf, rcv->cbdata);
                }
                PMIX_DESTRUCT(&buf);  // free's the msg data
                /* also done with the recv, if not a wildcard or the error tag */
                if (UINT32_MAX != rcv->tag && 0 != rcv->tag) {
                    pmix_list_remove_item(&pmix_usock_globals.posted_recvs, &rcv->super);
                    PMIX_RELEASE(rcv);
                }
                PMIX_RELEASE(msg);
                return;
            }
        }
    }

    /* we get here if no matching recv was found - this is an error */
    pmix_output(0, "UNEXPECTED MESSAGE tag =%d", msg->hdr.tag);
    PMIX_RELEASE(msg);
    PMIX_REPORT_ERROR(PMIX_ERROR);
}
