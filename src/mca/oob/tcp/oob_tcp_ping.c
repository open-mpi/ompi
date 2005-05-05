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
#include "include/ompi_socket_errno.h"
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETINET_TCP_H
#include <netinet/tcp.h>
#endif
#include "mca/ns/ns_types.h"
#include "mca/oob/tcp/oob_tcp.h"


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

    /* parse uri string */
    if(OMPI_SUCCESS != (rc = mca_oob_tcp_parse_uri(uri, &inaddr))) {
       ompi_output(0,
            "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_ping: invalid uri: %s\n",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(name),
            uri);
        return rc;
    }

    /* create socket */
    sd = socket(AF_INET, SOCK_STREAM, 0);
    if (sd < 0) {
       ompi_output(0,
            "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_ping: socket() failed with errno=%d\n",
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(name),
            ompi_socket_errno);
        return OMPI_ERR_UNREACH;
    }

    /* setup the socket as non-blocking */
    if((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_ping: fcntl(F_GETFL) failed with errno=%d\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name),
            ORTE_NAME_ARGS(name),
            ompi_socket_errno);
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(sd, F_SETFL, flags) < 0) {
            ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_ping: fcntl(F_SETFL) failed with errno=%d\n",
                ORTE_NAME_ARGS(orte_process_info.my_name),
                ORTE_NAME_ARGS(name),
                ompi_socket_errno);
        }
    }

    /* start the connect - will likely fail with EINPROGRESS */
    FD_ZERO(&fdset);
    if(connect(sd, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
        /* connect failed? */
        if(ompi_socket_errno != EINPROGRESS && ompi_socket_errno != EWOULDBLOCK) {
            close(sd);
            return OMPI_ERR_UNREACH;
        }

        /* select with timeout to wait for connect to complete */
        FD_SET(sd, &fdset);
        tv = *timeout;
        rc = select(sd+1, NULL, &fdset, NULL, &tv);
        if(rc <= 0) {
             close(sd);
             return OMPI_ERR_UNREACH;
        }
    }

    /* set socket back to blocking */
    flags &= ~O_NONBLOCK;
    if(fcntl(sd, F_SETFL, flags) < 0) {
         ompi_output(0, "[%d,%d,%d]-[%d,%d,%d] mca_oob_tcp_ping: fcntl(F_SETFL) failed with errno=%d\n",
             ORTE_NAME_ARGS(orte_process_info.my_name),
             ORTE_NAME_ARGS(name),
             ompi_socket_errno);
    }

    /* send a probe message */
    memset(&hdr, 0, sizeof(hdr));
    if(orte_process_info.my_name != NULL) {
        hdr.msg_src = *orte_process_info.my_name;
    } else {
        hdr.msg_src = mca_oob_name_any;
    }
    hdr.msg_dst = *name;
    hdr.msg_type = MCA_OOB_TCP_PROBE;
 
    if((rc = write(sd, &hdr, sizeof(hdr))) != sizeof(hdr)) {
        close(sd);
        return OMPI_ERR_UNREACH;
    }

    /* select with timeout to wait for response */
    FD_SET(sd, &fdset);
    tv = *timeout;
    rc = select(sd+1, &fdset, NULL, NULL, &tv);
    if(rc <= 0) {
        close(sd);
        return OMPI_ERR_UNREACH;
    }
    if((rc = read(sd, &hdr, sizeof(hdr))) != sizeof(hdr)) {
        close(sd);
        return OMPI_ERR_UNREACH;
    }
    close(sd);
    return OMPI_SUCCESS;
}


