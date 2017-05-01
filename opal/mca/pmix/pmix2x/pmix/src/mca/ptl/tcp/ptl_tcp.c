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
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <src/include/pmix_config.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "src/include/pmix_globals.h"
#include "src/include/pmix_socket_errno.h"
#include "src/client/pmix_client_ops.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/os_path.h"

#include "src/mca/ptl/base/base.h"
#include "ptl_tcp.h"

static pmix_status_t init(void);
static void finalize(void);
static pmix_status_t connect_to_peer(struct pmix_peer_t *peer,
                                     pmix_info_t *info, size_t ninfo);
static pmix_status_t send_recv(struct pmix_peer_t *peer,
                               pmix_buffer_t *bfr,
                               pmix_ptl_cbfunc_t cbfunc,
                               void *cbdata);
static pmix_status_t send_oneway(struct pmix_peer_t *peer,
                                 pmix_buffer_t *bfr,
                                 pmix_ptl_tag_t tag);

pmix_ptl_module_t pmix_ptl_tcp_module = {
    .init = init,
    .finalize = finalize,
    .send_recv = send_recv,
    .send = send_oneway,
    .connect_to_peer = connect_to_peer
};

static pmix_status_t recv_connect_ack(int sd);
static pmix_status_t send_connect_ack(int sd);


static pmix_status_t init(void)
{
    return PMIX_SUCCESS;
}

static void finalize(void)
{
}

static char *pmix_getline(FILE *fp)
{
    char *ret, *buff;
    char input[1024];

    ret = fgets(input, 1024, fp);
    if (NULL != ret) {
       input[strlen(input)-1] = '\0';  /* remove newline */
       buff = strdup(input);
       return buff;
    }

    return NULL;
}

static pmix_status_t connect_to_peer(struct pmix_peer_t *peer,
                                     pmix_info_t *info, size_t ninfo)
{
    char *evar, **uri;
    char *filename, *host;
    FILE *fp;
    char *srvr, *p, *p2;
    struct sockaddr_in *in;
    struct sockaddr_in6 *in6;
    pmix_socklen_t len;
    int sd, rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "ptl:tcp: connecting to server");

    /* see if the connection info is in the info array - if
     * so, then that overrides all other options */


    /* if I am a client, then we need to look for the appropriate
     * connection info in the environment */
    if (PMIX_PROC_CLIENT == pmix_globals.proc_type) {
        if (NULL == (evar = getenv("PMIX_SERVER_URI2"))) {
            /* not us */
            return PMIX_ERR_NOT_SUPPORTED;
        }

        /* the URI consists of  elements:
        *    - server nspace.rank
        *    - ptl rendezvous URI
        */
        uri = pmix_argv_split(evar, ';');
        if (2 != pmix_argv_count(uri)) {
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            pmix_argv_free(uri);
            return PMIX_ERR_NOT_SUPPORTED;
        }

        /* set the server nspace */
        p = uri[0];
        if (NULL == (p2 = strchr(p, '.'))) {
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            pmix_argv_free(uri);
            return PMIX_ERR_NOT_SUPPORTED;
        }
        *p2 = '\0';
        ++p2;
        pmix_client_globals.myserver.nptr->nspace = strdup(p);
        pmix_client_globals.myserver.info->pname.nspace = strdup(p);

        /* set the server rank */
        pmix_client_globals.myserver.info->pname.rank = strtoull(p2, NULL, 10);

        /* save the URI, but do not overwrite what we may have received from
         * the info-key directives */
        if (NULL == mca_ptl_tcp_component.super.uri) {
            mca_ptl_tcp_component.super.uri = strdup(uri[1]);
        }
        pmix_argv_free(uri);

    } else if (PMIX_PROC_TOOL == pmix_globals.proc_type) {
        /* if we already have a URI, then look no further */
        if (NULL == mca_ptl_tcp_component.super.uri) {
            /* we have to discover the connection info,
             * if possible. Start by looking for the connection
             * info in the expected place - if the server supports
             * tool connections via TCP, then there will be a
             * "contact.txt" file under the system tmpdir */
            filename = pmix_os_path(false, mca_ptl_tcp_component.tmpdir, "pmix-contact.txt", NULL);
            if (NULL == filename) {
                return PMIX_ERR_NOMEM;
            }
            fp = fopen(filename, "r");
            if (NULL == fp) {
                /* if we cannot open the file, then the server must not
                 * be configured to support tool connections - so abort */
                free(filename);
                return PMIX_ERR_UNREACH;
            }
            free(filename);
            /* get the URI */
            srvr = pmix_getline(fp);
            if (NULL == srvr) {
                PMIX_ERROR_LOG(PMIX_ERR_FILE_READ_FAILURE);
                fclose(fp);
                return PMIX_ERR_UNREACH;
            }
            fclose(fp);
            /* up to the first ';' is the server nspace/rank */
            if (NULL == (p = strchr(srvr, ';'))) {
                /* malformed */
                free(srvr);
                return PMIX_ERR_UNREACH;
            }
            *p = '\0';
            ++p;  // move past the semicolon
            /* the nspace is the section up to the '.' */
            if (NULL == (p2 = strchr(srvr, '.'))) {
                /* malformed */
                free(srvr);
                return PMIX_ERR_UNREACH;
            }
            *p2 = '\0';
            ++p2;
            /* set the server nspace */
            pmix_client_globals.myserver.nptr->nspace = strdup(srvr);
            pmix_client_globals.myserver.info->pname.nspace = strdup(srvr);
            pmix_client_globals.myserver.info->pname.rank = strtoull(p2, NULL, 10);
            /* now parse the uri itself */
            mca_ptl_tcp_component.super.uri = strdup(p);
            free(srvr);
        }
    }

    /* mark that we are the active module for this server */
    pmix_client_globals.myserver.nptr->compat.ptl = &pmix_ptl_tcp_module;

    /* setup the path to the daemon rendezvous point */
    memset(&mca_ptl_tcp_component.connection, 0, sizeof(struct sockaddr_storage));
    if (0 == strncmp(mca_ptl_tcp_component.super.uri, "tcp4", 4)) {
        /* separate the IP address from the port */
        p = strdup(mca_ptl_tcp_component.super.uri);
        if (NULL == p) {
            return PMIX_ERR_NOMEM;
        }
        p2 = strchr(&p[7], ':');
        if (NULL == p2) {
            free(p);
            return PMIX_ERR_BAD_PARAM;
        }
        *p2 = '\0';
        ++p2;
        host = &p[7];
        /* load the address */
        in = (struct sockaddr_in*)&mca_ptl_tcp_component.connection;
        in->sin_family = AF_INET;
        in->sin_addr.s_addr = inet_addr(host);
        if (in->sin_addr.s_addr == INADDR_NONE) {
            free(p);
            return PMIX_ERR_BAD_PARAM;
        }
        in->sin_port = htons(atoi(p2));
        len = sizeof(struct sockaddr_in);
    } else {
        /* separate the IP address from the port */
        p = strdup(mca_ptl_tcp_component.super.uri);
        if (NULL == p) {
            return PMIX_ERR_NOMEM;
        }
        p2 = strchr(&p[7], ':');
        if (NULL == p2) {
            free(p);
            return PMIX_ERR_BAD_PARAM;
        }
        *p2 = '\0';
        if (']' == p[strlen(p)-1]) {
            p[strlen(p)-1] = '\0';
        }
        if ('[' == p[7]) {
            host = &p[8];
        } else {
            host = &p[7];
        }
        /* load the address */
        in6 = (struct sockaddr_in6*)&mca_ptl_tcp_component.connection;
        in6->sin6_family = AF_INET6;
        if (0 == inet_pton(AF_INET6, host, (void*)&in6->sin6_addr)) {
            pmix_output (0, "ptl_tcp_parse_uri: Could not convert %s\n", host);
            free(p);
            return PMIX_ERR_BAD_PARAM;
        }
        in6->sin6_port = htons(atoi(p2));
        len = sizeof(struct sockaddr_in6);
    }
    free(p);

    /* establish the connection */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_connect(&mca_ptl_tcp_component.connection, len, &sd))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    pmix_client_globals.myserver.sd = sd;

    /* send our identity and any authentication credentials to the server */
    if (PMIX_SUCCESS != (rc = send_connect_ack(sd))) {
        PMIX_ERROR_LOG(rc);
        CLOSE_THE_SOCKET(sd);
        return rc;
    }

    /* do whatever handshake is required */
    if (PMIX_SUCCESS != (rc = recv_connect_ack(sd))) {
        PMIX_ERROR_LOG(rc);
        CLOSE_THE_SOCKET(sd);
        return rc;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sock_peer_try_connect: Connection across to server succeeded");

    /* mark the connection as made */
    pmix_globals.connected = true;

    pmix_ptl_base_set_nonblocking(sd);

    /* setup recv event */
    pmix_event_assign(&pmix_client_globals.myserver.recv_event,
                      pmix_globals.evbase,
                      pmix_client_globals.myserver.sd,
                      EV_READ | EV_PERSIST,
                      pmix_ptl_base_recv_handler, &pmix_client_globals.myserver);
    pmix_event_add(&pmix_client_globals.myserver.recv_event, 0);
    pmix_client_globals.myserver.recv_ev_active = true;

    /* setup send event */
    pmix_event_assign(&pmix_client_globals.myserver.send_event,
                      pmix_globals.evbase,
                      pmix_client_globals.myserver.sd,
                      EV_WRITE|EV_PERSIST,
                      pmix_ptl_base_send_handler, &pmix_client_globals.myserver);
    pmix_client_globals.myserver.send_ev_active = false;

    return PMIX_SUCCESS;
}

static pmix_status_t send_recv(struct pmix_peer_t *peer,
                               pmix_buffer_t *bfr,
                               pmix_ptl_cbfunc_t cbfunc,
                               void *cbdata)
{
    pmix_ptl_sr_t *ms;

    pmix_output_verbose(5, pmix_globals.debug_output,
                        "[%s:%d] post send to server",
                        __FILE__, __LINE__);

    ms = PMIX_NEW(pmix_ptl_sr_t);
    ms->peer = peer;
    ms->bfr = bfr;
    ms->cbfunc = cbfunc;
    ms->cbdata = cbdata;
    pmix_event_assign(&ms->ev, pmix_globals.evbase, -1,
                      EV_WRITE, pmix_ptl_base_send_recv, ms);
    pmix_event_active(&ms->ev, EV_WRITE, 1);
    return PMIX_SUCCESS;
}

static pmix_status_t send_oneway(struct pmix_peer_t *peer,
                                 pmix_buffer_t *bfr,
                                 pmix_ptl_tag_t tag)
{
    pmix_ptl_queue_t *q;
    pmix_peer_t *pr = (pmix_peer_t*)peer;

    /* we have to transfer this to an event for thread
     * safety as we need to post this message on the
     * peer's send queue */
    q = PMIX_NEW(pmix_ptl_queue_t);
    PMIX_RETAIN(pr);
    q->peer = peer;
    q->buf = bfr;
    q->tag = tag;
    pmix_event_assign(&q->ev, pmix_globals.evbase, -1,
                      EV_WRITE, pmix_ptl_base_send, q);
    pmix_event_active(&q->ev, EV_WRITE, 1);

    return PMIX_SUCCESS;
}


/****    SUPPORTING FUNCTIONS    ****/
static pmix_status_t send_connect_ack(int sd)
{
    char *msg;
    pmix_ptl_hdr_t hdr;
    size_t sdsize=0, csize=0, len;
    char *cred = NULL;
    char *sec, *bfrops, *gds;
    pmix_bfrop_buffer_type_t bftype;
    pmix_status_t rc;
    uint8_t flag;
    uid_t euid;
    gid_t egid;
    uint32_t u32;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:tcp SEND CONNECT ACK");

    /* if we are a server, then we shouldn't be here */
    if (PMIX_PROC_IS_SERVER) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the header */
    memset(&hdr, 0, sizeof(pmix_ptl_hdr_t));
    hdr.pindex = -1;
    hdr.tag = UINT32_MAX;

    /* a security module was assigned to us during rte_init based
     * on a list of available security modules provided by our
     * local PMIx server, if known. Now use that module to
     * get a credential, if the security system provides one. Not
     * every psec module will do so, thus we must first check */
    PMIX_PSEC_CREATE_CRED(rc, &pmix_client_globals.myserver,
                          PMIX_PROTOCOL_V2, &cred, &len);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    /* allow space for a marker indicating client vs tool */
    sdsize = 1;

    if (PMIX_PROC_IS_CLIENT) {
        flag = 0;
        /* reserve space for our nspace and rank info */
        sdsize += strlen(pmix_globals.myid.nspace) + 1 + sizeof(int);
    } else {
        flag = 1;
        /* add space for our uid/gid for ACL purposes */
        sdsize += 2*sizeof(uint32_t);
    }

    /* add the name of our active sec module - we selected it
     * in pmix_client.c prior to entering here */
    sec = pmix_globals.mypeer->nptr->compat.psec->name;

    /* add our active bfrops module name */
    bfrops = pmix_globals.mypeer->nptr->compat.bfrops->version;
    /* and the type of buffer we are using */
    bftype = pmix_globals.mypeer->nptr->compat.type;

    /* add our active gds module for working with the server */
    gds = (char*)pmix_globals.mypeer->nptr->compat.gds->name;

    /* set the number of bytes to be read beyond the header */
    hdr.nbytes = sdsize + strlen(PMIX_VERSION) + 1 + strlen(sec) + 1 \
                + strlen(bfrops) + 1 + sizeof(bftype) \
                + strlen(gds) + 1 + sizeof(uint32_t) + len;  // must NULL terminate the strings!

    /* create a space for our message */
    sdsize = (sizeof(hdr) + hdr.nbytes);
    if (NULL == (msg = (char*)malloc(sdsize))) {
        if (NULL != cred) {
            free(cred);
        }
        free(sec);
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    memset(msg, 0, sdsize);

    /* load the message */
    csize=0;
    memcpy(msg, &hdr, sizeof(pmix_ptl_hdr_t));
    csize += sizeof(pmix_ptl_hdr_t);

    /* provide our active psec module */
    memcpy(msg+csize, sec, strlen(sec));
    csize += strlen(sec)+1;

    /* provide our active bfrops module */
    memcpy(msg+csize, bfrops, strlen(bfrops));
    csize += strlen(bfrops)+1;

    /* provide the bfrops type */
    memcpy(msg+csize, &bftype, sizeof(bftype));
    csize += sizeof(bftype);

    /* provide the gds module */
    memcpy(msg+csize, gds, strlen(gds));
    csize += strlen(gds)+1;

    /* load the length of the credential - we put this in uint32_t
     * format as that is a fixed size, and convert to network
     * byte order for heterogeneity */
    u32 = htonl((uint32_t)len);
    memcpy(msg+csize, &u32, sizeof(uint32_t));
    csize += sizeof(uint32_t);
    /* load the credential */
    if (0 < u32) {
        memcpy(msg+csize, cred, len);
        csize += len;
    }

    /* load our process type - this is a single byte,
     * so no worry about heterogeneity here */
    memcpy(msg+csize, &flag, 1);
    csize += 1;

    if (PMIX_PROC_IS_CLIENT) {
        /* if we are a client, provide our nspace/rank */
        memcpy(msg+csize, pmix_globals.myid.nspace, strlen(pmix_globals.myid.nspace));
        csize += strlen(pmix_globals.myid.nspace)+1;
        /* again, need to convert */
        u32 = htonl((uint32_t)pmix_globals.myid.rank);
        memcpy(msg+csize, &u32, sizeof(uint32_t));
        csize += sizeof(uint32_t);
    } else {
        /* if we are a tool, provide our uid/gid for ACL support - note
         * that we have to convert so we can handle heterogeneity */
        euid = geteuid();
        u32 = htonl(euid);
        memcpy(msg+csize, &u32, sizeof(uint32_t));
        csize += sizeof(uint32_t);
        egid = getegid();
        u32 = htonl(egid);
        memcpy(msg+csize, &u32, sizeof(uint32_t));
        csize += sizeof(uint32_t);
    }

    /* provide our version */
    memcpy(msg+csize, PMIX_VERSION, strlen(PMIX_VERSION));
    csize += strlen(PMIX_VERSION)+1;

    /* send the entire message across */
    if (PMIX_SUCCESS != pmix_ptl_base_send_blocking(sd, msg, sdsize)) {
        free(msg);
        if (NULL != cred) {
            free(cred);
        }
        return PMIX_ERR_UNREACH;
    }
    free(msg);
    if (NULL != cred) {
        free(cred);
    }
    return PMIX_SUCCESS;
}

/* we receive a connection acknowledgement from the server,
 * consisting of nothing more than a status report. If success,
 * then we initiate authentication method */
static pmix_status_t recv_connect_ack(int sd)
{
    pmix_status_t reply;
    pmix_status_t rc;
    struct timeval tv, save;
    pmix_socklen_t sz;
    bool sockopt = true;
    uint32_t u32;
    char nspace[PMIX_MAX_NSLEN+1];

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: RECV CONNECT ACK FROM SERVER");

    /* get the current timeout value so we can reset to it */
    sz = sizeof(save);
    if (0 != getsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, (void*)&save, &sz)) {
        if (ENOPROTOOPT == errno) {
            sockopt = false;
        } else {
           return PMIX_ERR_UNREACH;
       }
   } else {
        /* set a timeout on the blocking recv so we don't hang */
        tv.tv_sec  = 2;
        tv.tv_usec = 0;
        if (0 != setsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv))) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix: recv_connect_ack could not setsockopt SO_RCVTIMEO");
            return PMIX_ERR_UNREACH;
        }
    }

    /* receive the status reply */
    rc = pmix_ptl_base_recv_blocking(sd, (char*)&u32, sizeof(uint32_t));
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    reply = ntohl(u32);

    if (PMIX_PROC_IS_CLIENT) {
        /* see if they want us to do the handshake */
        if (PMIX_ERR_READY_FOR_HANDSHAKE == reply) {
            PMIX_PSEC_CLIENT_HANDSHAKE(rc, &pmix_client_globals.myserver, sd);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
        } else if (PMIX_SUCCESS != reply) {
            return reply;
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix: RECV CONNECT CONFIRMATION");

        /* receive our index into the server's client array */
        rc = pmix_ptl_base_recv_blocking(sd, (char*)&u32, sizeof(uint32_t));
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        pmix_globals.pindex = ntohl(u32);
    } else {  // we are a tool
        /* if the status indicates an error, then we are done */
        if (PMIX_SUCCESS != reply) {
            PMIX_ERROR_LOG(reply);
            return reply;
        }
        /* recv our nspace */
        rc = pmix_ptl_base_recv_blocking(sd, (char*)&pmix_globals.myid.nspace, PMIX_MAX_NSLEN+1);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        /* our rank is always zero */
        pmix_globals.myid.rank = 0;

        /* get the server's nspace and rank so we can send to it */
        pmix_client_globals.myserver.info = PMIX_NEW(pmix_rank_info_t);
        pmix_client_globals.myserver.nptr = PMIX_NEW(pmix_nspace_t);
        pmix_ptl_base_recv_blocking(sd, (char*)nspace, PMIX_MAX_NSLEN+1);
        pmix_client_globals.myserver.nptr->nspace = strdup(nspace);
        pmix_client_globals.myserver.info->pname.nspace = strdup(nspace);
        pmix_ptl_base_recv_blocking(sd, (char*)&(pmix_client_globals.myserver.info->pname.rank), sizeof(int));

        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix: RECV CONNECT CONFIRMATION FOR TOOL %s:%d FROM SERVER %s:%d",
                            pmix_globals.myid.nspace, pmix_globals.myid.rank,
                            pmix_client_globals.myserver.info->pname.nspace,
                            pmix_client_globals.myserver.info->pname.rank);

        /* get the returned status from the security handshake */
        pmix_ptl_base_recv_blocking(sd, (char*)&reply, sizeof(pmix_status_t));
        if (PMIX_SUCCESS != reply) {
            /* see if they want us to do the handshake */
            if (PMIX_ERR_READY_FOR_HANDSHAKE == reply) {
                PMIX_PSEC_CLIENT_HANDSHAKE(reply, &pmix_client_globals.myserver, sd);
                if (PMIX_SUCCESS != reply) {
                    return reply;
                }
                /* if the handshake succeeded, then fall thru to the next step */
            } else {
                return reply;
            }
        }
    }

    if (sockopt) {
        if (0 != setsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, &save, sz)) {
            return PMIX_ERR_UNREACH;
        }
    }

    return PMIX_SUCCESS;
}

#if 0

DIR *cur_dirp = NULL;
struct dirent * dir_entry;
bool connect_to_system_server = false;
bool connect_to_system_first = false;
bool connection_defined = false;

/* scan incoming info for directives */
if (NULL != info) {
    for (n=0; n < ninfo; n++) {
        if (strcmp(info[n].key, PMIX_SERVER_PIDINFO) == 0) {
            server_pid = info[n].value.data.pid;
            server_pid_given = true;
        } else if (strcmp(info[n].key, PMIX_CONNECT_TO_SYSTEM) == 0) {
            connect_to_system_server = info[n].value.data.flag;
            connection_defined = true;
        } else if (strcmp(info[n].key, PMIX_CONNECT_SYSTEM_FIRST) == 0) {
            connect_to_system_first = info[n].value.data.flag;
            connection_defined = true;
        } else if (strcmp(info[n].key, PMIX_SERVER_TMPDIR) == 0 &&
                   NULL == mytmpdir) {
            mytmpdir = strdup(info[n].value.data.string);
        } else if (strcmp(info[n].key, PMIX_SYSTEM_TMPDIR) == 0 &&
                   NULL == systmpdir) {
            systmpdir = strdup(info[n].value.data.string);
        }
    }
}



/* if we are to connect solely to the system-level daemon,
 * or to preferentially connect to the system-level daemon,
 * or nothing was specified at all, then look to see if a
 * rendezvous point in that location exists */
if (connect_to_system_server || connect_to_system_first || !connection_defined) {
    /* find the temp dir */
    if (NULL != systmpdir) {
        tdir = systmpdir;
    } else if (NULL == (tdir = getenv("TMPDIR"))) {
        if (NULL == (tdir = getenv("TEMP"))) {
            if (NULL == (tdir = getenv("TMP"))) {
                tdir = "/tmp";
            }
        }
    }
    snprintf(address.sun_path, sizeof(address.sun_path)-1, "%s/pmix.sys.%s", tdir, hostname);
    /* see if the rendezvous file exists */
    if (0 != access(address.sun_path, R_OK)) {
        /* if it was a requirement, then error out */
        if (connect_to_system_server) {
            return PMIX_ERR_UNREACH;
        }
        /* otherwise, this isn't a fatal error - reset the addr */
        memset(&address, 0, sizeof(struct sockaddr_un));
        address.sun_family = AF_UNIX;
        connection_defined = false;
    } else {
        /* connect to this server */
        connection_defined = true;
    }
}

if (!connection_defined) {
    /* if we get here, then either we are to connect to
     * a non-system daemon, or a system-level daemon was
     * not found - so now look for the session daemon */


    /* find the temp dir */
    if (NULL != mytmpdir) {
        tdir = mytmpdir;
    } else if (NULL == (tdir = getenv("TMPDIR"))) {
        if (NULL == (tdir = getenv("TEMP"))) {
            if (NULL == (tdir = getenv("TMP"))) {
                tdir = "/tmp";
            }
        }
    }

    /* if they gave us a specific pid, then look for that
     * particular server - otherwise, see if there is only
     * one on this node and default to it */
    if (server_pid_given) {
        snprintf(address.sun_path, sizeof(address.sun_path)-1, "%s/pmix.%s.%d", tdir, hostname, server_pid);
        /* if the rendezvous file doesn't exist, that's an error */
        if (0 != access(address.sun_path, R_OK)) {
            return PMIX_ERR_NOT_FOUND;
        }
    } else {
        /* open up the temp directory */
        if (NULL == (cur_dirp = opendir(tdir))) {
            return PMIX_ERR_NOT_FOUND;
        }
        /* search the entries for something that starts with pmix.hostname */
        if (0 > asprintf(&tmp, "pmix.%s", hostname)) {
            closedir(cur_dirp);
            return PMIX_ERR_NOMEM;
        }
        evar = NULL;
        while (NULL != (dir_entry = readdir(cur_dirp))) {
            if (0 == strncmp(dir_entry->d_name, tmp, strlen(tmp))) {
                /* found one - if more than one, then that's an error */
                if (NULL != evar) {
                    closedir(cur_dirp);
                    free(evar);
                    free(tmp);
                    return PMIX_ERR_INIT;
                }
                evar = strdup(dir_entry->d_name);
            }
        }
        free(tmp);
        closedir(cur_dirp);
        if (NULL == evar) {
            /* none found */
            return PMIX_ERR_INIT;
        }
        /* use the found one as our contact point */
        snprintf(address.sun_path, sizeof(address.sun_path)-1, "%s/%s", tdir, evar);
        free(evar);
    }
}

#endif
