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
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved.
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
#include <dirent.h>

#include "src/include/pmix_globals.h"
#include "src/include/pmix_socket_errno.h"
#include "src/client/pmix_client_ops.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/os_path.h"
#include "src/util/show_help.h"
#include "src/mca/bfrops/base/base.h"

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

static pmix_status_t parse_uri_file(char *filename,
                                    char **uri,
                                    char **nspace,
                                    pmix_rank_t *rank);
static pmix_status_t try_connect(char *uri, int *sd);
static pmix_status_t df_search(char *dirname, char *prefix,
                               int *sd, char **nspace,
                               pmix_rank_t *rank);

static pmix_status_t connect_to_peer(struct pmix_peer_t *peer,
                                     pmix_info_t *info, size_t ninfo)
{
    char *evar, **uri, *suri = NULL, *suri2 = NULL;
    char *filename, *nspace=NULL;
    pmix_rank_t rank = PMIX_RANK_WILDCARD;
    char *p, *p2, *server_nspace = NULL;
    int sd, rc;
    size_t n;
    char myhost[PMIX_MAXHOSTNAMELEN];
    bool system_level = false;
    bool system_level_only = false;
    bool reconnect = false;
    pid_t pid = 0;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "ptl:tcp: connecting to server");

    /* see if the connection info is in the info array - if
     * so, then that overrides all other options */


    /* if I am a client, then we need to look for the appropriate
     * connection info in the environment */
    if (PMIX_PROC_IS_CLIENT(pmix_globals.mypeer)) {
        if (NULL != (evar = getenv("PMIX_SERVER_URI3"))) {
            /* we are talking to a v3 server */
            pmix_client_globals.myserver->proc_type = PMIX_PROC_SERVER | PMIX_PROC_V3;
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "V3 SERVER DETECTED");
            /* must use the v3 bfrops module */
            pmix_globals.mypeer->nptr->compat.bfrops = pmix_bfrops_base_assign_module("v3");
            if (NULL == pmix_globals.mypeer->nptr->compat.bfrops) {
                return PMIX_ERR_INIT;
            }
        } else if (NULL != (evar = getenv("PMIX_SERVER_URI21"))) {
            /* we are talking to a v2.1 server */
            pmix_client_globals.myserver->proc_type = PMIX_PROC_SERVER | PMIX_PROC_V21;
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "V21 SERVER DETECTED");
            /* must use the v21 bfrops module */
            pmix_globals.mypeer->nptr->compat.bfrops = pmix_bfrops_base_assign_module("v21");
            if (NULL == pmix_globals.mypeer->nptr->compat.bfrops) {
                return PMIX_ERR_INIT;
            }
        } else if (NULL != (evar = getenv("PMIX_SERVER_URI2"))) {
            /* we are talking to a v2.0 server */
            pmix_client_globals.myserver->proc_type = PMIX_PROC_SERVER | PMIX_PROC_V20;
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "V20 SERVER DETECTED");
            /* must use the v20 bfrops module */
            pmix_globals.mypeer->nptr->compat.bfrops = pmix_bfrops_base_assign_module("v20");
            if (NULL == pmix_globals.mypeer->nptr->compat.bfrops) {
                return PMIX_ERR_INIT;
            }
        } else {
            /* not us */
            return PMIX_ERR_NOT_SUPPORTED;
        }
        /* the server will be using the same bfrops as us */
        pmix_client_globals.myserver->nptr->compat.bfrops = pmix_globals.mypeer->nptr->compat.bfrops;
        /* mark that we are using the V2 (i.e., tcp) protocol */
        pmix_globals.mypeer->protocol = PMIX_PROTOCOL_V2;

        /* the URI consists of the following elements:
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
        nspace = strdup(p);
        rank = strtoull(p2, NULL, 10);

        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "ptl:tcp:client attempt connect to %s", uri[1]);

        /* go ahead and try to connect */
        if (PMIX_SUCCESS != (rc = try_connect(uri[1], &sd))) {
            free(nspace);
            pmix_argv_free(uri);
            return rc;
        }
        pmix_argv_free(uri);
        goto complete;

    }

    /* get here if we are a tool - check any provided directives
     * to see where they want us to connect to */
    suri = NULL;
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (0 == strcmp(info[n].key, PMIX_CONNECT_TO_SYSTEM)) {
                system_level_only = PMIX_INFO_TRUE(&info[n]);
            } else if (0 == strncmp(info[n].key, PMIX_CONNECT_SYSTEM_FIRST, PMIX_MAX_KEYLEN)) {
                /* try the system-level */
                system_level = PMIX_INFO_TRUE(&info[n]);
            } else if (0 == strncmp(info[n].key, PMIX_SERVER_PIDINFO, PMIX_MAX_KEYLEN)) {
                pid = info[n].value.data.pid;
            } else if (0 == strncmp(info[n].key, PMIX_SERVER_NSPACE, PMIX_MAX_KEYLEN)) {
                if (NULL != server_nspace) {
                    /* they included it more than once */
                    if (0 == strcmp(server_nspace, info[n].value.data.string)) {
                        /* same value, so ignore it */
                        continue;
                    }
                    /* otherwise, we don't know which one to use */
                    free(server_nspace);
                    if (NULL != suri) {
                        free(suri);
                    }
                    return PMIX_ERR_BAD_PARAM;
                }
                server_nspace = strdup(info[n].value.data.string);
            } else if (0 == strncmp(info[n].key, PMIX_SERVER_URI, PMIX_MAX_KEYLEN)) {
                if (NULL != suri) {
                    /* they included it more than once */
                    if (0 == strcmp(suri, info[n].value.data.string)) {
                        /* same value, so ignore it */
                        continue;
                    }
                    /* otherwise, we don't know which one to use */
                    free(suri);
                    if (NULL != server_nspace) {
                        free(server_nspace);
                    }
                    return PMIX_ERR_BAD_PARAM;
                }
                suri = strdup(info[n].value.data.string);
            } else if (0 == strncmp(info[n].key, PMIX_CONNECT_RETRY_DELAY, PMIX_MAX_KEYLEN)) {
                mca_ptl_tcp_component.wait_to_connect = info[n].value.data.uint32;
            } else if (0 == strncmp(info[n].key, PMIX_CONNECT_MAX_RETRIES, PMIX_MAX_KEYLEN)) {
                mca_ptl_tcp_component.max_retries = info[n].value.data.uint32;
            } else if (0 == strncmp(info[n].key, PMIX_RECONNECT_SERVER, PMIX_MAX_KEYLEN)) {
                reconnect = true;
            }
        }
    }
    if (NULL == suri && !reconnect && NULL != mca_ptl_tcp_component.super.uri) {
        suri = strdup(mca_ptl_tcp_component.super.uri);
    }

    /* mark that we are using the V2 protocol */
    pmix_globals.mypeer->protocol = PMIX_PROTOCOL_V2;
    gethostname(myhost, sizeof(myhost));
    /* if we were given a URI via MCA param, then look no further */
    if (NULL != suri) {
        if (NULL != server_nspace) {
            free(server_nspace);
            server_nspace = NULL;
        }
        /* if the string starts with "file:", then they are pointing
         * us to a file we need to read to get the URI itself */
        if (0 == strncmp(suri, "file:", 5)) {
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "ptl:tcp:tool getting connection info from %s", suri);
            nspace = NULL;
            rc = parse_uri_file(&suri[5], &suri2, &nspace, &rank);
            if (PMIX_SUCCESS != rc) {
                free(suri);
                return PMIX_ERR_UNREACH;
            }
            free(suri);
            suri = suri2;
        } else {
            /* we need to extract the nspace/rank of the server from the string */
            p = strchr(suri, ';');
            if (NULL == p) {
                free(suri);
                return PMIX_ERR_BAD_PARAM;
            }
            *p = '\0';
            p++;
            suri2 = strdup(p); // save the uri portion
            /* the '.' in the first part of the original string separates
             * nspace from rank */
            p = strchr(suri, '.');
            if (NULL == p) {
                free(suri2);
                free(suri);
                return PMIX_ERR_BAD_PARAM;
            }
            *p = '\0';
            p++;
            nspace = strdup(suri);
            rank = strtoull(p, NULL, 10);
            free(suri);
            suri = suri2;
            /* now update the URI */
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "ptl:tcp:tool attempt connect using given URI %s", suri);
        /* go ahead and try to connect */
        if (PMIX_SUCCESS != (rc = try_connect(suri, &sd))) {
            if (NULL != nspace) {
                free(nspace);
            }
            free(suri);
            return rc;
        }
        free(suri);
        suri = NULL;
        goto complete;
    }

    /* if they gave us a pid, then look for it */
    if (0 != pid) {
        if (NULL != server_nspace) {
            free(server_nspace);
            server_nspace = NULL;
        }
        if (0 > asprintf(&filename, "pmix.%s.tool.%d", myhost, pid)) {
            return PMIX_ERR_NOMEM;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "ptl:tcp:tool searching for given session server %s",
                            filename);
        nspace = NULL;
        rc = df_search(mca_ptl_tcp_component.system_tmpdir,
                       filename, &sd, &nspace, &rank);
        free(filename);
        if (PMIX_SUCCESS == rc) {
            goto complete;
        }
        if (NULL != nspace) {
            free(nspace);
        }
        /* since they gave us a specific pid and we couldn't
         * connect to it, return an error */
        return PMIX_ERR_UNREACH;
    }

    /* if they gave us an nspace, then look for it */
    if (NULL != server_nspace) {
        if (0 > asprintf(&filename, "pmix.%s.tool.%s", myhost, server_nspace)) {
            free(server_nspace);
            return PMIX_ERR_NOMEM;
        }
        free(server_nspace);
        server_nspace = NULL;
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "ptl:tcp:tool searching for given session server %s",
                            filename);
        nspace = NULL;
        rc = df_search(mca_ptl_tcp_component.system_tmpdir,
                       filename, &sd, &nspace, &rank);
        free(filename);
        if (PMIX_SUCCESS == rc) {
            goto complete;
        }
        if (NULL != nspace) {
            free(nspace);
        }
        /* since they gave us a specific nspace and we couldn't
         * connect to it, return an error */
        return PMIX_ERR_UNREACH;
    }

    /* if they asked for system-level, we start there */
    if (system_level || system_level_only) {
        if (0 > asprintf(&filename, "%s/pmix.sys.%s", mca_ptl_tcp_component.system_tmpdir, myhost)) {
            return PMIX_ERR_NOMEM;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "ptl:tcp:tool looking for system server at %s",
                            filename);
        /* try to read the file */
        rc = parse_uri_file(filename, &suri, &nspace, &rank);
        free(filename);
        if (PMIX_SUCCESS == rc) {
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "ptl:tcp:tool attempt connect to system server at %s", suri);
            /* go ahead and try to connect */
            if (PMIX_SUCCESS == try_connect(suri, &sd)) {
                /* don't free nspace - we will use it below */
                goto complete;
            }
            free(nspace);
        }
    }

    /* we get here if they either didn't ask for a system-level connection,
     * or they asked for it and it didn't succeed. If they _only_ wanted
     * a system-level connection, then we are done */
    if (system_level_only) {
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "ptl:tcp: connecting to system failed");
        if (NULL != suri) {
            free(suri);
        }
        return PMIX_ERR_UNREACH;
    }

    /* they didn't give us a pid, so we will search to see what session-level
     * tools are available to this user. We will take the first connection
     * that succeeds - this is based on the likelihood that there is only
     * one session per user on a node */

    if (0 > asprintf(&filename, "pmix.%s.tool", myhost)) {
        if (NULL != suri) {
            free(suri);
        }
        return PMIX_ERR_NOMEM;
    }
    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "ptl:tcp:tool searching for session server %s",
                        filename);
    nspace = NULL;
    rc = df_search(mca_ptl_tcp_component.system_tmpdir,
                   filename, &sd, &nspace, &rank);
    free(filename);
    if (PMIX_SUCCESS != rc) {
        if (NULL != nspace){
            free(nspace);
        }
        if (NULL != suri) {
            free(suri);
        }
        return PMIX_ERR_UNREACH;
    }

  complete:
    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "sock_peer_try_connect: Connection across to server succeeded");

    /* do a final bozo check */
    if (NULL == nspace || PMIX_RANK_WILDCARD == rank) {
        if (NULL != nspace) {
            free(nspace);
        }
        if (NULL != suri) {
            free(suri);
        }
        CLOSE_THE_SOCKET(sd);
        return PMIX_ERR_UNREACH;
    }
    /* mark the connection as made */
    pmix_globals.connected = true;
    pmix_client_globals.myserver->sd = sd;

    /* tools setup their server info in try_connect because they
     * utilize a broader handshake */
    if (PMIX_PROC_IS_CLIENT(pmix_globals.mypeer)) {
        /* setup the server info */
        if (NULL == pmix_client_globals.myserver->info) {
            pmix_client_globals.myserver->info = PMIX_NEW(pmix_rank_info_t);
        }
        if (NULL == pmix_client_globals.myserver->nptr) {
            pmix_client_globals.myserver->nptr = PMIX_NEW(pmix_nspace_t);
        }
        if (NULL != pmix_client_globals.myserver->nptr->nspace) {
            free(pmix_client_globals.myserver->nptr->nspace);
        }
        pmix_client_globals.myserver->nptr->nspace = strdup(nspace);

        if (NULL != pmix_client_globals.myserver->info->pname.nspace) {
            free(pmix_client_globals.myserver->info->pname.nspace);
        }
        pmix_client_globals.myserver->info->pname.nspace = strdup(pmix_client_globals.myserver->nptr->nspace);
        pmix_client_globals.myserver->info->pname.rank = rank;
    }

    pmix_ptl_base_set_nonblocking(sd);

    /* setup recv event */
    pmix_event_assign(&pmix_client_globals.myserver->recv_event,
                      pmix_globals.evbase,
                      pmix_client_globals.myserver->sd,
                      EV_READ | EV_PERSIST,
                      pmix_ptl_base_recv_handler, pmix_client_globals.myserver);
    pmix_client_globals.myserver->recv_ev_active = true;
    PMIX_POST_OBJECT(pmix_client_globals.myserver);
    pmix_event_add(&pmix_client_globals.myserver->recv_event, 0);

    /* setup send event */
    pmix_event_assign(&pmix_client_globals.myserver->send_event,
                      pmix_globals.evbase,
                      pmix_client_globals.myserver->sd,
                      EV_WRITE|EV_PERSIST,
                      pmix_ptl_base_send_handler, pmix_client_globals.myserver);
    pmix_client_globals.myserver->send_ev_active = false;

    free(nspace);
    if (NULL != suri) {
        free(suri);
    }
    return PMIX_SUCCESS;
}

static pmix_status_t send_recv(struct pmix_peer_t *peer,
                               pmix_buffer_t *bfr,
                               pmix_ptl_cbfunc_t cbfunc,
                               void *cbdata)
{
    pmix_ptl_sr_t *ms;
    pmix_peer_t *pr = (pmix_peer_t*)peer;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "[%s:%d] post send to server",
                        __FILE__, __LINE__);

    ms = PMIX_NEW(pmix_ptl_sr_t);
    PMIX_RETAIN(pr);
    ms->peer = pr;
    ms->bfr = bfr;
    ms->cbfunc = cbfunc;
    ms->cbdata = cbdata;
    PMIX_THREADSHIFT(ms, pmix_ptl_base_send_recv);
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
    q->peer = pr;
    q->buf = bfr;
    q->tag = tag;
    PMIX_THREADSHIFT(q, pmix_ptl_base_send);
    return PMIX_SUCCESS;
}

static void timeout(int sd, short args, void *cbdata)
{
    pmix_lock_t *lock = (pmix_lock_t*)cbdata;
    PMIX_WAKEUP_THREAD(lock);
}

/****    SUPPORTING FUNCTIONS    ****/
static pmix_status_t parse_uri_file(char *filename,
                                    char **uri,
                                    char **nspace,
                                    pmix_rank_t *rank)
{
    FILE *fp;
    char *srvr, *p, *p2;
    pmix_lock_t lock;
    pmix_event_t ev;
    struct timeval tv;
    int retries;
    int major;

    fp = fopen(filename, "r");
    if (NULL == fp) {
        /* if we cannot open the file, then the server must not
         * be configured to support tool connections, or this
         * user isn't authorized to access it - or it may just
         * not exist yet! Check for existence */
        if (0 != access(filename, R_OK)) {
            if (ENOENT == errno && 0 < mca_ptl_tcp_component.wait_to_connect) {
                /* the file does not exist, so give it
                 * a little time to see if the server
                 * is still starting up */
                retries = 0;
                do {
                    ++retries;
                    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                        "WAITING FOR CONNECTION FILE");
                    PMIX_CONSTRUCT_LOCK(&lock);
                    tv.tv_sec = mca_ptl_tcp_component.wait_to_connect;
                    tv.tv_usec = 0;
                    pmix_event_evtimer_set(pmix_globals.evbase, &ev,
                                           timeout, &lock);
                    pmix_event_evtimer_add(&ev, &tv);
                    PMIX_WAIT_THREAD(&lock);
                    PMIX_DESTRUCT_LOCK(&lock);
                    fp = fopen(filename, "r");
                    if (NULL != fp) {
                        /* we found it! */
                        goto process;
                    }
                } while (retries < mca_ptl_tcp_component.max_retries);
                /* otherwise, mark it as unreachable */
            }
        }
        return PMIX_ERR_UNREACH;
    }

  process:
    /* get the URI */
    srvr = pmix_getline(fp);
    if (NULL == srvr) {
        PMIX_ERROR_LOG(PMIX_ERR_FILE_READ_FAILURE);
        fclose(fp);
        return PMIX_ERR_UNREACH;
    }
    /* see if this file contains the server's version */
    p2 = pmix_getline(fp);
    if (NULL == p2) {
        pmix_client_globals.myserver->proc_type = PMIX_PROC_SERVER | PMIX_PROC_V20;
        pmix_client_globals.myserver->protocol = PMIX_PROTOCOL_V2;
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "V20 SERVER DETECTED");
    } else {
        /* convert the version to a number */
        if ('v' == p2[0]) {
            major = strtoul(&p2[1], NULL, 10);
        } else {
            major = strtoul(p2, NULL, 10);
        }
        if (2 == major) {
            pmix_client_globals.myserver->proc_type = PMIX_PROC_SERVER | PMIX_PROC_V21;
            pmix_client_globals.myserver->protocol = PMIX_PROTOCOL_V2;
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "V21 SERVER DETECTED");
        } else if (3 <= major) {
            pmix_client_globals.myserver->proc_type = PMIX_PROC_SERVER | PMIX_PROC_V3;
            pmix_client_globals.myserver->protocol = PMIX_PROTOCOL_V2;
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "V3 SERVER DETECTED");
        }
    }
    if (NULL != p2) {
        free(p2);
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
    /* set the server nspace/rank */
    *nspace = strdup(srvr);
    *rank = strtoull(p2, NULL, 10);

    /* now parse the uri itself */
    *uri = strdup(p);
    free(srvr);

    return PMIX_SUCCESS;
}

static pmix_status_t try_connect(char *uri, int *sd)
{
    char *p, *p2, *host;
    struct sockaddr_in *in;
    struct sockaddr_in6 *in6;
    size_t len;
    pmix_status_t rc;
    bool retried = false;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:tcp try connect to %s", uri);

    /* mark that we are the active module for this server */
    pmix_client_globals.myserver->nptr->compat.ptl = &pmix_ptl_tcp_module;

    /* setup the path to the daemon rendezvous point */
    memset(&mca_ptl_tcp_component.connection, 0, sizeof(struct sockaddr_storage));
    if (0 == strncmp(uri, "tcp4", 4)) {
        /* need to skip the tcp4: part */
        p = strdup(&uri[7]);
        if (NULL == p) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            return PMIX_ERR_NOMEM;
        }

        /* separate the IP address from the port */
        p2 = strchr(p, ':');
        if (NULL == p2) {
            free(p);
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            return PMIX_ERR_BAD_PARAM;
        }
        *p2 = '\0';
        p2++;
        host = p;
        /* load the address */
        in = (struct sockaddr_in*)&mca_ptl_tcp_component.connection;
        in->sin_family = AF_INET;
        in->sin_addr.s_addr = inet_addr(host);
        if (in->sin_addr.s_addr == INADDR_NONE) {
            free(p);
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            return PMIX_ERR_BAD_PARAM;
        }
        in->sin_port = htons(atoi(p2));
        len = sizeof(struct sockaddr_in);
    } else {
        /* need to skip the tcp6: part */
        p = strdup(&uri[7]);
        if (NULL == p) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            return PMIX_ERR_NOMEM;
        }

        p2 = strchr(p, ':');
        if (NULL == p2) {
            free(p);
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            return PMIX_ERR_BAD_PARAM;
        }
        *p2 = '\0';
        if (']' == p[strlen(p)-1]) {
            p[strlen(p)-1] = '\0';
        }
        if ('[' == p[0]) {
            host = &p[1];
        } else {
            host = &p[0];
        }
        /* load the address */
        in6 = (struct sockaddr_in6*)&mca_ptl_tcp_component.connection;
        in6->sin6_family = AF_INET6;
        if (0 == inet_pton(AF_INET6, host, (void*)&in6->sin6_addr)) {
            pmix_output (0, "ptl_tcp_parse_uri: Could not convert %s\n", host);
            free(p);
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            return PMIX_ERR_BAD_PARAM;
        }
        in6->sin6_port = htons(atoi(p2));
        len = sizeof(struct sockaddr_in6);
    }
    free(p);

  retry:
    /* establish the connection */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_connect(&mca_ptl_tcp_component.connection, len, sd))) {
        /* do not error log - might just be a stale connection point */
        return rc;
    }

    /* send our identity and any authentication credentials to the server */
    if (PMIX_SUCCESS != (rc = send_connect_ack(*sd))) {
        PMIX_ERROR_LOG(rc);
        CLOSE_THE_SOCKET(*sd);
        return rc;
    }

    /* do whatever handshake is required */
    if (PMIX_SUCCESS != (rc = recv_connect_ack(*sd))) {
        CLOSE_THE_SOCKET(*sd);
        if (PMIX_ERR_TEMP_UNAVAILABLE == rc) {
            /* give it two tries */
            if (!retried) {
                retried = true;
                goto retry;
            }
        }
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    return PMIX_SUCCESS;
}
static pmix_status_t send_connect_ack(int sd)
{
    char *msg;
    pmix_ptl_hdr_t hdr;
    size_t sdsize=0, csize=0;
    pmix_byte_object_t cred;
    char *sec, *bfrops, *gds;
    pmix_bfrop_buffer_type_t bftype;
    pmix_status_t rc;
    uint8_t flag;
    uid_t euid;
    gid_t egid;
    uint32_t u32;
    bool self_defined = false;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:tcp SEND CONNECT ACK");

    /* if we are a server, then we shouldn't be here */
    if (PMIX_PROC_IS_SERVER(pmix_globals.mypeer) &&
        !PMIX_PROC_IS_LAUNCHER(pmix_globals.mypeer)) {
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
    PMIX_BYTE_OBJECT_CONSTRUCT(&cred);
    PMIX_PSEC_CREATE_CRED(rc, pmix_globals.mypeer,
                          NULL, 0, NULL, 0, &cred);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    /* allow space for a marker indicating client vs tool */
    sdsize = 1;

    if (PMIX_PROC_IS_CLIENT(pmix_globals.mypeer)) {
        flag = 0;
        /* reserve space for our nspace and rank info */
        sdsize += strlen(pmix_globals.myid.nspace) + 1 + sizeof(uint32_t);
    } else if (PMIX_PROC_IS_LAUNCHER(pmix_globals.mypeer)) {
        flag = 2;
        /* add space for our uid/gid for ACL purposes */
        sdsize += 2*sizeof(uint32_t);
        /* if we already have an identifier, we need to pass it */
        if (0 < strlen(pmix_globals.myid.nspace) &&
            PMIX_RANK_INVALID != pmix_globals.myid.rank) {
            sdsize += strlen(pmix_globals.myid.nspace) + 1 + sizeof(uint32_t) + 1;
            self_defined = true;
        } else {
            ++sdsize; // need space for the flag indicating if have id
        }
    } else {  // must be a simple tool
        flag = 1;
        /* add space for our uid/gid for ACL purposes */
        sdsize += 2*sizeof(uint32_t);
        /* if we self-defined an identifier, we need to pass it */
        if (0 < strlen(pmix_globals.myid.nspace) &&
            PMIX_RANK_INVALID != pmix_globals.myid.rank) {
            sdsize += 1 + strlen(pmix_globals.myid.nspace) + 1 + sizeof(uint32_t);
            self_defined = true;
        } else {
            ++sdsize; // need space for the flag indicating if have id
        }
    }

    /* add the name of our active sec module - we selected it
     * in pmix_client.c prior to entering here */
    sec = pmix_globals.mypeer->nptr->compat.psec->name;

    /* add our active bfrops module name */
    bfrops = pmix_globals.mypeer->nptr->compat.bfrops->version;
    /* and the type of buffer we are using */
    bftype = pmix_globals.mypeer->nptr->compat.type;

    /* add our active gds module for working with the server */
    gds = (char*)pmix_client_globals.myserver->nptr->compat.gds->name;

    /* set the number of bytes to be read beyond the header */
    hdr.nbytes = sdsize + strlen(PMIX_VERSION) + 1 + strlen(sec) + 1 \
                + strlen(bfrops) + 1 + sizeof(bftype) \
                + strlen(gds) + 1 + sizeof(uint32_t) + cred.size;  // must NULL terminate the strings!

    /* create a space for our message */
    sdsize = (sizeof(hdr) + hdr.nbytes);
    if (NULL == (msg = (char*)malloc(sdsize))) {
        PMIX_BYTE_OBJECT_DESTRUCT(&cred);
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

    /* load the length of the credential - we put this in uint32_t
     * format as that is a fixed size, and convert to network
     * byte order for heterogeneity */
    u32 = htonl((uint32_t)cred.size);
    memcpy(msg+csize, &u32, sizeof(uint32_t));
    csize += sizeof(uint32_t);
    /* load the credential */
    if (0 < u32) {
        memcpy(msg+csize, cred.bytes, cred.size);
        csize += cred.size;
    }
    PMIX_BYTE_OBJECT_DESTRUCT(&cred);

    /* load our process type - this is a single byte,
     * so no worry about heterogeneity here */
    memcpy(msg+csize, &flag, 1);
    csize += 1;

    if (PMIX_PROC_IS_CLIENT(pmix_globals.mypeer)) {
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

    /* provide our active bfrops module */
    memcpy(msg+csize, bfrops, strlen(bfrops));
    csize += strlen(bfrops)+1;

    /* provide the bfrops type */
    memcpy(msg+csize, &bftype, sizeof(bftype));
    csize += sizeof(bftype);

    /* provide the gds module */
    memcpy(msg+csize, gds, strlen(gds));
    csize += strlen(gds)+1;

    /* if we are not a client and self-defined an identifier, we need to pass it */
    if (!PMIX_PROC_IS_CLIENT(pmix_globals.mypeer)) {
        if (self_defined) {
            flag = 1;
            memcpy(msg+csize, &flag, 1);
            ++csize;
            memcpy(msg+csize, pmix_globals.myid.nspace, strlen(pmix_globals.myid.nspace));
            csize += strlen(pmix_globals.myid.nspace)+1;
            /* again, need to convert */
            u32 = htonl((uint32_t)pmix_globals.myid.rank);
            memcpy(msg+csize, &u32, sizeof(uint32_t));
            csize += sizeof(uint32_t);
        } else {
            flag = 0;
            memcpy(msg+csize, &flag, 1);
            ++csize;
        }
    }

    /* send the entire message across */
    if (PMIX_SUCCESS != pmix_ptl_base_send_blocking(sd, msg, sdsize)) {
        free(msg);
        return PMIX_ERR_UNREACH;
    }
    free(msg);
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

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix: RECV CONNECT ACK FROM SERVER");

    /* get the current timeout value so we can reset to it */
    sz = sizeof(save);
    if (0 != getsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, (void*)&save, &sz)) {
        if (ENOPROTOOPT == errno || EOPNOTSUPP == errno) {
            sockopt = false;
        } else {
           return PMIX_ERR_UNREACH;
       }
   } else {
        /* set a timeout on the blocking recv so we don't hang */
        tv.tv_sec  = 2;
        tv.tv_usec = 0;
        if (0 != setsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv))) {
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "pmix: recv_connect_ack could not setsockopt SO_RCVTIMEO");
            return PMIX_ERR_UNREACH;
        }
    }

    /* receive the status reply */
    rc = pmix_ptl_base_recv_blocking(sd, (char*)&u32, sizeof(uint32_t));
    if (PMIX_SUCCESS != rc) {
        if (sockopt) {
            /* return the socket to normal */
            if (0 != setsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, &save, sz)) {
                return PMIX_ERR_UNREACH;
            }
        }
        return rc;
    }
    reply = ntohl(u32);

    if (PMIX_PROC_IS_CLIENT(pmix_globals.mypeer)) {
        /* see if they want us to do the handshake */
        if (PMIX_ERR_READY_FOR_HANDSHAKE == reply) {
            PMIX_PSEC_CLIENT_HANDSHAKE(rc, pmix_client_globals.myserver, sd);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
        } else if (PMIX_SUCCESS != reply) {
            return reply;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "pmix: RECV CONNECT CONFIRMATION");

        /* receive our index into the server's client array */
        rc = pmix_ptl_base_recv_blocking(sd, (char*)&u32, sizeof(uint32_t));
        if (PMIX_SUCCESS != rc) {
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
        rc = pmix_ptl_base_recv_blocking(sd, nspace, PMIX_MAX_NSLEN+1);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
        /* if we already have our nspace, then just verify it matches */
        if (0 < strlen(pmix_globals.myid.nspace)) {
            if (0 != strncmp(pmix_globals.myid.nspace, nspace, PMIX_MAX_NSLEN)) {
                return PMIX_ERR_INIT;
            }
        } else {
            (void)strncpy(pmix_globals.myid.nspace, nspace, PMIX_MAX_NSLEN);
        }
        /* if we already have a rank, then leave it alone */
        if (PMIX_RANK_INVALID == pmix_globals.myid.rank) {
            /* our rank is always zero */
            pmix_globals.myid.rank = 0;
        }

        /* get the server's nspace and rank so we can send to it */
        if (NULL == pmix_client_globals.myserver->info) {
            pmix_client_globals.myserver->info = PMIX_NEW(pmix_rank_info_t);
        }
        if (NULL == pmix_client_globals.myserver->nptr) {
            pmix_client_globals.myserver->nptr = PMIX_NEW(pmix_nspace_t);
        }
        pmix_ptl_base_recv_blocking(sd, (char*)nspace, PMIX_MAX_NSLEN+1);
        if (NULL != pmix_client_globals.myserver->nptr->nspace) {
            free(pmix_client_globals.myserver->nptr->nspace);
        }
        pmix_client_globals.myserver->nptr->nspace = strdup(nspace);
        if (NULL != pmix_client_globals.myserver->info->pname.nspace) {
            free(pmix_client_globals.myserver->info->pname.nspace);
        }
        pmix_client_globals.myserver->info->pname.nspace = strdup(nspace);
        pmix_ptl_base_recv_blocking(sd, (char*)&(pmix_client_globals.myserver->info->pname.rank), sizeof(int));

        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "pmix: RECV CONNECT CONFIRMATION FOR TOOL %s:%d FROM SERVER %s:%d",
                            pmix_globals.myid.nspace, pmix_globals.myid.rank,
                            pmix_client_globals.myserver->info->pname.nspace,
                            pmix_client_globals.myserver->info->pname.rank);

        /* get the returned status from the security handshake */
        pmix_ptl_base_recv_blocking(sd, (char*)&reply, sizeof(pmix_status_t));
        if (PMIX_SUCCESS != reply) {
            /* see if they want us to do the handshake */
            if (PMIX_ERR_READY_FOR_HANDSHAKE == reply) {
                PMIX_PSEC_CLIENT_HANDSHAKE(reply, pmix_client_globals.myserver, sd);
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

static pmix_status_t df_search(char *dirname, char *prefix,
                               int *sd, char **nspace,
                               pmix_rank_t *rank)
{
    char *suri, *nsp, *newdir;
    pmix_rank_t rk;
    pmix_status_t rc;
    struct stat buf;
    DIR *cur_dirp;
    struct dirent *dir_entry;

    if (NULL == (cur_dirp = opendir(dirname))) {
        return PMIX_ERR_NOT_FOUND;
    }

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:tcp: searching directory %s", dirname);

    /* search the entries for something that starts with the provided prefix */
    while (NULL != (dir_entry = readdir(cur_dirp))) {
        /* ignore the . and .. entries */
        if (0 == strcmp(dir_entry->d_name, ".") ||
            0 == strcmp(dir_entry->d_name, "..")) {
            continue;
        }
        newdir = pmix_os_path(false, dirname, dir_entry->d_name, NULL);
        if (-1 == stat(newdir, &buf)) {
            free(newdir);
            continue;
        }
        /* if it is a directory, down search */
        if (S_ISDIR(buf.st_mode)) {
            rc = df_search(newdir, prefix, sd, nspace, rank);
            free(newdir);
            if (PMIX_SUCCESS == rc) {
                closedir(cur_dirp);
                return rc;
            }
            continue;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "pmix:tcp: checking %s vs %s", dir_entry->d_name, prefix);
        /* see if it starts with our prefix */
        if (0 == strncmp(dir_entry->d_name, prefix, strlen(prefix))) {
            /* try to read this file */
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "pmix:tcp: reading file %s", newdir);
            rc = parse_uri_file(newdir, &suri, &nsp, &rk);
            if (PMIX_SUCCESS == rc) {
                /* go ahead and try to connect */
                pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                    "pmix:tcp: attempting to connect to %s", suri);
                if (PMIX_SUCCESS == try_connect(suri, sd)) {
                    (*nspace) = nsp;
                    *rank = rk;
                    closedir(cur_dirp);
                    free(suri);
                    free(newdir);
                    return PMIX_SUCCESS;
                }
                free(suri);
                free(nsp);
            }
        }
        free(newdir);
    }
    closedir(cur_dirp);
    return PMIX_ERR_NOT_FOUND;
}
