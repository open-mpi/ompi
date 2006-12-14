/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * In windows, many of the socket functions return an EWOULDBLOCK
 * instead of \ things like EAGAIN, EINPROGRESS, etc. It has been
 * verified that this will \ not conflict with other error codes that
 * are returned by these functions \ under UNIX/Linux environments 
 */

#include "orte_config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
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
#ifndef __WINDOWS__
#include <signal.h>
#endif
#include "opal/event/event.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/util/proc_info.h"

#include "orte/mca/oob/tcp/oob_tcp.h"

/*
 * Local functions
 */
static void noop(int fd, short event, void *arg);

/*
 * Ping a peer to see if it is alive.
 *
 * @param peer (IN)   Opaque name of peer process.
 * @param tv (IN)     Timeout to wait for a response.
 * @return            OMPI error code (<0) on error number of bytes actually sent.
 */

int mca_oob_tcp_ping(
    const orte_process_name_t* name,
    const char* uri,
    const struct timeval *timeout)
{
    int sd, flags, rc;
    struct sockaddr_in inaddr;
    fd_set fdset;
    mca_oob_tcp_hdr_t hdr;
    struct timeval tv;
    struct iovec iov;
#ifndef __WINDOWS__
    struct opal_event sigpipe_handler;
#endif

    /* parse uri string */
    if(ORTE_SUCCESS != (rc = mca_oob_tcp_parse_uri(uri, &inaddr))) {
       opal_output(0,
            "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_ping: invalid uri: %s\n",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(name),
            uri);
        return rc;
    }

    /* create socket */
    sd = socket(AF_INET, SOCK_STREAM, 0);
    if (sd < 0) {
       opal_output(0,
            "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_ping: socket() failed: %s (%d)\n",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(name),
            strerror(opal_socket_errno),
            opal_socket_errno);
        return ORTE_ERR_UNREACH;
    }

    /* setup the socket as non-blocking */
    if((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        opal_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_ping: fcntl(F_GETFL) failed: %s (%d)\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(name),
            strerror(opal_socket_errno),
            opal_socket_errno);
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(sd, F_SETFL, flags) < 0) {
            opal_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_ping: fcntl(F_SETFL) failed: %s (%d)\n",
                ORTE_NAME_ARGS(orte_process_info.my_name),
                ORTE_NAME_ARGS(name),
                strerror(opal_socket_errno),
                opal_socket_errno);
        }
    }

    /* start the connect - will likely fail with EINPROGRESS */
    FD_ZERO(&fdset);
    if(connect(sd, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
        /* connect failed? */
        if(opal_socket_errno != EINPROGRESS && opal_socket_errno != EWOULDBLOCK) {
            CLOSE_THE_SOCKET(sd);
            return ORTE_ERR_UNREACH;
        }

        /* select with timeout to wait for connect to complete */
        FD_SET(sd, &fdset);
        tv = *timeout;
        rc = select(sd+1, NULL, &fdset, NULL, &tv);
        if(rc <= 0) {
             CLOSE_THE_SOCKET(sd);
             return ORTE_ERR_UNREACH;
        }
    }

    /* set socket back to blocking */
    flags &= ~O_NONBLOCK;
    if(fcntl(sd, F_SETFL, flags) < 0) {
         opal_output(0, "[%lu,%lu,%lu]-[%lu,%lu,%lu] mca_oob_tcp_ping: fcntl(F_SETFL) failed: %s (%d)\n",
             ORTE_NAME_ARGS(orte_process_info.my_name),
             ORTE_NAME_ARGS(name),
             strerror(opal_socket_errno),
             opal_socket_errno);
    }

    /* send a probe message */
    memset(&hdr, 0, sizeof(hdr));
    if(orte_process_info.my_name != NULL) {
        hdr.msg_src = *orte_process_info.my_name;
    } else {
        hdr.msg_src = *ORTE_NAME_INVALID;
    }
    hdr.msg_dst = *name;
    hdr.msg_type = MCA_OOB_TCP_PROBE;
    MCA_OOB_TCP_HDR_HTON(&hdr);

#ifndef __WINDOWS__
    /* Ignore SIGPIPE in the write -- determine success or failure in
       the ping by looking at the return code from write() */
    opal_signal_set(&sigpipe_handler, SIGPIPE,
                    noop, &sigpipe_handler);
    opal_signal_add(&sigpipe_handler, NULL);
#endif
    /* Do the write and see what happens. Use the writev version just to
     * make Windows happy as there the write function is limitted to
     * file operations.
     */
    iov.iov_base = (IOVBASE_TYPE*)&hdr;
    iov.iov_len  = sizeof(hdr);
    rc = writev(sd, &iov, 1 );
#ifndef __WINDOWS__
    /* Now de-register the handler */
    opal_signal_del(&sigpipe_handler);
#endif
    if (rc != sizeof(hdr)) {
        CLOSE_THE_SOCKET(sd);
        return ORTE_ERR_UNREACH;
    }

    /* select with timeout to wait for response */
    FD_SET(sd, &fdset);
    tv = *timeout;
    rc = select(sd+1, &fdset, NULL, NULL, &tv);
    if(rc <= 0) {
        CLOSE_THE_SOCKET(sd);
        return ORTE_ERR_UNREACH;
    }
    if((rc = read(sd, &hdr, sizeof(hdr))) != sizeof(hdr)) {
        CLOSE_THE_SOCKET(sd);
        return ORTE_ERR_UNREACH;
    }
    MCA_OOB_TCP_HDR_NTOH(&hdr);
    if(hdr.msg_type != MCA_OOB_TCP_PROBE) {
        CLOSE_THE_SOCKET(sd);
        return ORTE_ERR_UNREACH;
    }
    CLOSE_THE_SOCKET(sd);
    return ORTE_SUCCESS;
}


static void noop(int fd, short event, void *arg)
{
    /* Nothing */
}
