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
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_DIRENT_H
#    include <dirent.h>
#endif
#include <ctype.h>

#include "src/include/pmix_globals.h"
#include "src/include/pmix_socket_errno.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"
#include "src/util/pmix_string_copy.h"

#include "src/mca/ptl/base/base.h"
#include "src/mca/ptl/base/ptl_base_handshake.h"

/****    SUPPORTING FUNCTIONS    ****/
static void timeout(int sd, short args, void *cbdata);
static pmix_status_t construct_message(pmix_peer_t *peer, char **msgout, size_t *sz,
                                       pmix_info_t *iptr, size_t niptr);

pmix_status_t pmix_ptl_base_set_peer(pmix_peer_t *peer, char *evar)
{
    pmix_status_t rc;
    char *vrs;

    vrs = getenv("PMIX_VERSION");

    if (0 == strcmp(evar, "PMIX_SERVER_URI41")) {
        /* we are talking to a v4.1 server */
        PMIX_SET_PEER_TYPE(peer, PMIX_PROC_SERVER);
        PMIX_SET_PEER_VERSION(peer, vrs, 4, 1);

        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "V41 SERVER DETECTED");

        /* must use the v41 bfrops module */
        PMIX_BFROPS_SET_MODULE(rc, pmix_globals.mypeer, peer, "v41");
        return rc;
    }

    if (0 == strcmp(evar, "PMIX_SERVER_URI4")) {
        /* we are talking to a v4 server */
        PMIX_SET_PEER_TYPE(peer, PMIX_PROC_SERVER);
        PMIX_SET_PEER_VERSION(peer, vrs, 4, 0);

        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "V4.0 SERVER DETECTED");

        /* must use the V4 bfrops module */
        PMIX_BFROPS_SET_MODULE(rc, pmix_globals.mypeer, peer, "v4");
        return rc;
    }

    if (0 == strcmp(evar, "PMIX_SERVER_URI3")) {
        /* we are talking to a v3 server */
        PMIX_SET_PEER_TYPE(peer, PMIX_PROC_SERVER);
        PMIX_SET_PEER_VERSION(peer, vrs, 3, 0);

        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "V3 SERVER DETECTED");

        /* must use the v3 bfrops module */
        PMIX_BFROPS_SET_MODULE(rc, pmix_globals.mypeer, peer, "v3");
        return rc;
    }

    if (0 == strcmp(evar, "PMIX_SERVER_URI21")) {
        /* we are talking to a v2.1 server */
        PMIX_SET_PEER_TYPE(peer, PMIX_PROC_SERVER);
        PMIX_SET_PEER_VERSION(peer, vrs, 2, 1);

        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "V21 SERVER DETECTED");

        /* must use the v21 bfrops module */
        PMIX_BFROPS_SET_MODULE(rc, pmix_globals.mypeer, peer, "v21");
        return rc;
    }

    if (0 == strcmp(evar, "PMIX_SERVER_URI2")) {
        /* we are talking to a v2.0 server */
        PMIX_SET_PEER_TYPE(peer, PMIX_PROC_SERVER);
        PMIX_SET_PEER_VERSION(peer, vrs, 2, 0);

        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output, "V20 SERVER DETECTED");

        /* must use the v20 bfrops module */
        PMIX_BFROPS_SET_MODULE(rc, pmix_globals.mypeer, peer, "v20");
        return rc;
    }

    return PMIX_ERR_UNREACH;
}

pmix_status_t pmix_ptl_base_check_server_uris(pmix_peer_t *peer, char **ev)
{
    char *evar;
    pmix_status_t rc;

    if (NULL != (evar = getenv("PMIX_SERVER_URI41"))) {
        rc = pmix_ptl_base_set_peer(peer, "PMIX_SERVER_URI41");
        *ev = evar;
        return rc;
    }

    if (NULL != (evar = getenv("PMIX_SERVER_URI4"))) {
        /* we are talking to a v4 server */
        rc = pmix_ptl_base_set_peer(peer, "PMIX_SERVER_URI4");
        *ev = evar;
        return rc;
    }
    if (NULL != (evar = getenv("PMIX_SERVER_URI3"))) {
        rc = pmix_ptl_base_set_peer(peer, "PMIX_SERVER_URI3");
        *ev = evar;
        return rc;
    }

    if (NULL != (evar = getenv("PMIX_SERVER_URI21"))) {
        rc = pmix_ptl_base_set_peer(peer, "PMIX_SERVER_URI21");
        *ev = evar;
        return rc;
    }

    if (NULL != (evar = getenv("PMIX_SERVER_URI2"))) {
        rc = pmix_ptl_base_set_peer(peer, "PMIX_SERVER_URI2");
        *ev = evar;
        return rc;
    }

    return PMIX_ERR_UNREACH;
}

pmix_status_t pmix_ptl_base_check_directives(pmix_info_t *info, size_t ninfo)
{
    size_t n;
    pmix_status_t rc;

    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_IF_INCLUDE)) {
            if (NULL != pmix_ptl_base.if_include) {
                free(pmix_ptl_base.if_include);
            }
            pmix_ptl_base.if_include = strdup(info[n].value.data.string);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_IF_EXCLUDE)) {
            if (NULL != pmix_ptl_base.if_exclude) {
                free(pmix_ptl_base.if_exclude);
            }
            pmix_ptl_base.if_exclude = strdup(info[n].value.data.string);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_IPV4_PORT)) {
            pmix_ptl_base.ipv4_port = info[n].value.data.integer;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_IPV6_PORT)) {
            pmix_ptl_base.ipv6_port = info[n].value.data.integer;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_DISABLE_IPV4)) {
            pmix_ptl_base.disable_ipv4_family = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_DISABLE_IPV6)) {
            pmix_ptl_base.disable_ipv6_family = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_URI)
                   || PMIX_CHECK_KEY(&info[n], PMIX_SERVER_URI)) {
            if (NULL != pmix_ptl_base.uri) {
                free(pmix_ptl_base.uri);
            }
            pmix_ptl_base.uri = strdup(info[n].value.data.string);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_TMPDIR)) {
            if (NULL != pmix_ptl_base.session_tmpdir) {
                free(pmix_ptl_base.session_tmpdir);
            }
            pmix_ptl_base.session_tmpdir = strdup(info[n].value.data.string);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_SYSTEM_TMPDIR)) {
            if (NULL != pmix_ptl_base.system_tmpdir) {
                free(pmix_ptl_base.system_tmpdir);
            }
            pmix_ptl_base.system_tmpdir = strdup(info[n].value.data.string);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_CONNECT_MAX_RETRIES)) {
            PMIX_VALUE_GET_NUMBER(rc, &info[n].value, pmix_ptl_base.max_retries, int);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_CONNECT_RETRY_DELAY)) {
            PMIX_VALUE_GET_NUMBER(rc, &info[n].value, pmix_ptl_base.wait_to_connect, int);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_ptl_base_setup_fork(const pmix_proc_t *proc, char ***env)
{
    PMIX_HIDE_UNUSED_PARAMS(proc);

    PMIx_Setenv("PMIX_SERVER_TMPDIR", pmix_ptl_base.session_tmpdir, true, env);
    PMIx_Setenv("PMIX_SYSTEM_TMPDIR", pmix_ptl_base.system_tmpdir, true, env);

    return PMIX_SUCCESS;
}

pmix_status_t pmix_ptl_base_parse_uri(const char *evar, char **nspace, pmix_rank_t *rank,
                                      char **suri)
{
    char **uri;
    char *p;

    uri = PMIx_Argv_split(evar, ';');
    if (2 != PMIx_Argv_count(uri)) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        PMIx_Argv_free(uri);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* set the server nspace - the rank is appended
     * to the end with a '.' separator. NOTE: we
     * cannot search from the FRONT as that would
     * stop on any FQDN or IPv4 separations */
    if (NULL == (p = strrchr(uri[0], '.'))) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        PMIx_Argv_free(uri);
        return PMIX_ERR_NOT_SUPPORTED;
    }
    *p = '\0';
    ++p;
    *nspace = strdup(uri[0]);
    /* set the server rank */
    *rank = strtoull(p, NULL, 10);
    if (NULL != suri) {
        *suri = strdup(uri[1]);
    }

    PMIx_Argv_free(uri);
    return PMIX_SUCCESS;
}

pmix_status_t pmix_ptl_base_parse_uri_file(char *filename,
                                           bool optional,
                                           pmix_list_t *connections)
{
    FILE *fp;
    char *srvr, *p = NULL;
    pmix_lock_t lock;
    pmix_event_t ev;
    struct timeval tv;
    int retries;
    pmix_status_t rc;
    pmix_connection_t *cn;
    char *nspace = NULL;
    pmix_rank_t rank;
    char *uri = NULL;

    /* if we cannot open the file, then the server must not
     * be configured to support tool connections, or this
     * user isn't authorized to access it - or it may just
     * not exist yet! Check for existence */
    /* coverity[TOCTOU] */
    if (0 != access(filename, R_OK)) {
        if (ENOENT == errno && !optional) {
            /* the file does not exist, so give it
             * a little time to see if the server
             * is still starting up */
            retries = 0;
            do {
                ++retries;
                pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                    "WAITING FOR CONNECTION FILE %s", filename);
                PMIX_CONSTRUCT_LOCK(&lock);
                if (0 < pmix_ptl_base.wait_to_connect) {
                    tv.tv_sec = pmix_ptl_base.wait_to_connect;
                    tv.tv_usec = 0;
                    pmix_event_evtimer_set(pmix_globals.evbase, &ev, timeout, &lock);
                    PMIX_POST_OBJECT(&ev);
                    pmix_event_evtimer_add(&ev, &tv);
                } else {
                    tv.tv_sec = 0;
                    tv.tv_usec = 10000; // use 0.01 sec as default
                    pmix_event_evtimer_set(pmix_globals.evbase, &ev, timeout, &lock);
                    PMIX_POST_OBJECT(&ev);
                    pmix_event_evtimer_add(&ev, &tv);
                }
                PMIX_WAIT_THREAD(&lock);
                PMIX_DESTRUCT_LOCK(&lock);
                /* coverity[TOCTOU] */
                if (0 == access(filename, R_OK)) {
                    goto process;
                }
            } while (retries < pmix_ptl_base.max_retries);
            /* otherwise, mark it as unreachable */
        }
        return PMIX_ERR_UNREACH;
    }

process:
    fp = fopen(filename, "r");
    if (NULL == fp) {
        return PMIX_ERR_UNREACH;
    }
    /* get the URI - might seem crazy, but there is actually
     * a race condition here where the server may have created
     * the file but not yet finished writing into it. So give
     * us a chance to get the required info */
    for (retries = 0; retries < 3; retries++) {
        srvr = pmix_getline(fp);
        if (NULL != srvr) {
            break;
        }
        fclose(fp);
        tv.tv_sec = 0;
        tv.tv_usec = 10000; // use 0.01 sec as default
        pmix_event_evtimer_set(pmix_globals.evbase, &ev, timeout, &lock);
        PMIX_POST_OBJECT(&ev);
        pmix_event_evtimer_add(&ev, &tv);
        PMIX_WAIT_THREAD(&lock);
        PMIX_DESTRUCT_LOCK(&lock);
        fp = fopen(filename, "r");
        if (NULL == fp) {
            return PMIX_ERR_UNREACH;
        }
    }
    if (NULL == srvr) {
        PMIX_ERROR_LOG(PMIX_ERR_FILE_READ_FAILURE);
        fclose(fp);
        return PMIX_ERR_UNREACH;
    }

    /* see if this file contains the server's version */
    p = pmix_getline(fp);
    fclose(fp);

    /* parse the URI */
    rc = pmix_ptl_base_parse_uri(srvr, &nspace, &rank, &uri);
    free(srvr);
    if (PMIX_SUCCESS == rc) {
        cn = PMIX_NEW(pmix_connection_t);
        cn->nspace = nspace;
        cn->rank = rank;
        cn->uri = uri;
        cn->version = p;
        pmix_list_append(connections, &cn->super);
    } else {
        if (NULL != nspace) {
            free(nspace);
        }
        if (NULL != uri) {
            free(uri);
        }
        if (NULL != p) {
            free(p);
        }
    }
    return rc;
}

pmix_status_t pmix_ptl_base_df_search(char *dirname, char *prefix, pmix_info_t info[], size_t ninfo,
                                      bool optional, pmix_list_t *connections)
{
    char *newdir;
    DIR *cur_dirp, *tst;
    struct dirent *dir_entry;
    pmix_status_t rc;

    if (NULL == (cur_dirp = opendir(dirname))) {
        return PMIX_ERR_NOT_FOUND;
    }

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:ptl: searching directory %s", dirname);

    /* search the entries for something that starts with the provided prefix */
    while (NULL != (dir_entry = readdir(cur_dirp))) {
        /* ignore the . and .. entries */
        if (0 == strcmp(dir_entry->d_name, ".") || 0 == strcmp(dir_entry->d_name, "..")) {
            continue;
        }
        newdir = pmix_os_path(false, dirname, dir_entry->d_name, NULL);
        /* if it is a directory, down search */
        tst = opendir(newdir);
        if (NULL != tst) {
            closedir(tst);
            pmix_ptl_base_df_search(newdir, prefix, info, ninfo, optional, connections);
            free(newdir);
            continue;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "pmix:tool: checking %s vs %s", dir_entry->d_name, prefix);
        /* see if it starts with our prefix */
        if (0 == strncmp(dir_entry->d_name, prefix, strlen(prefix))) {
            /* try to read this file */
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "pmix:tool: reading file %s", newdir);
            rc = pmix_ptl_base_parse_uri_file(newdir, optional, connections);
            if (PMIX_SUCCESS != rc) {
                free(newdir);
                closedir(cur_dirp);
                return rc;
            }
        }
        free(newdir);
    }
    closedir(cur_dirp);
    if (0 == pmix_list_get_size(connections)) {
        return PMIX_ERR_NOT_FOUND;
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_ptl_base_setup_connection(char *uri, struct sockaddr_storage *connection,
                                             size_t *len)
{
    char *p = NULL, *p2, *host;
    struct sockaddr_in *in;
    struct sockaddr_in6 *in6;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:base setup connection to %s", uri);

    memset(connection, 0, sizeof(struct sockaddr_storage));
    if (0 == strncmp(uri, "tcp4", 4)) {
        /* need to skip the tcp4: part */
        p = strdup(&uri[7]);
        if (NULL == p) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            return PMIX_ERR_NOMEM;
        }

        /* separate the IP address from the port */
        p2 = strrchr(p, ':');
        if (NULL == p2) {
            free(p);
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            return PMIX_ERR_BAD_PARAM;
        }
        *p2 = '\0';
        p2++;
        host = p;
        /* load the address */
        in = (struct sockaddr_in *) connection;
        in->sin_family = AF_INET;
        in->sin_addr.s_addr = inet_addr(host);
        if (in->sin_addr.s_addr == INADDR_NONE) {
            free(p);
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            return PMIX_ERR_BAD_PARAM;
        }
        in->sin_port = htons(atoi(p2));
        *len = sizeof(struct sockaddr_in);
    } else {
        /* need to skip the tcp6: part */
        p = strdup(&uri[7]);
        if (NULL == p) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            return PMIX_ERR_NOMEM;
        }

        p2 = strrchr(p, ':');
        if (NULL == p2) {
            free(p);
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            return PMIX_ERR_BAD_PARAM;
        }
        *p2 = '\0';
        if (']' == p[strlen(p) - 1]) {
            p[strlen(p) - 1] = '\0';
        }
        if ('[' == p[0]) {
            host = &p[1];
        } else {
            host = &p[0];
        }
        /* load the address */
        in6 = (struct sockaddr_in6 *) connection;
        in6->sin6_family = AF_INET6;
        if (0 == inet_pton(AF_INET6, host, (void *) &in6->sin6_addr)) {
            pmix_output(0, "ptl_tcp_parse_uri: Could not convert %s\n", host);
            free(p);
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            return PMIX_ERR_BAD_PARAM;
        }
        in6->sin6_port = htons(atoi(p2));
        *len = sizeof(struct sockaddr_in6);
    }
    if (NULL != p) {
        free(p);
    }

    return PMIX_SUCCESS;
}

static pmix_status_t send_connect_ack(pmix_peer_t *peer,
                                      pmix_info_t iptr[], size_t niptr)
{
    char *msg;
    size_t sdsize = 0;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:ptl SEND CONNECT ACK");

    /* set our ID flag and compute the required handshake size */
    peer->proc_type.flag = pmix_ptl_base_set_flag(&sdsize);

    /* construct the contact message */
    rc = construct_message(peer, &msg, &sdsize, iptr, niptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* send the entire message across */
    if (PMIX_SUCCESS != pmix_ptl_base_send_blocking(peer->sd, msg, sdsize)) {
        free(msg);
        return PMIX_ERR_UNREACH;
    }
    free(msg);
    return PMIX_SUCCESS;
}

/* we receive a connection acknowledgment from the server,
 * consisting of nothing more than a status report. If success,
 * then we initiate authentication method */
static pmix_status_t recv_connect_ack(pmix_peer_t *peer)
{
    pmix_status_t reply;
    pmix_status_t rc;
    struct timeval save;
    pmix_socklen_t sz = sizeof(save);
    bool sockopt = true;
    uint32_t u32;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix: RECV CONNECT ACK FROM SERVER");

    /* set the socket timeout so we don't hang on blocking recv */
    rc = pmix_ptl_base_set_timeout(peer, &save, &sz, &sockopt);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    /* receive the status reply */
    rc = pmix_ptl_base_recv_blocking(peer->sd, (char *) &u32, sizeof(uint32_t));
    if (PMIX_SUCCESS != rc) {
        if (sockopt) {
            /* return the socket to normal */
            if (0 != setsockopt(peer->sd, SOL_SOCKET, SO_RCVTIMEO, &save, sz)) {
                pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                    "pmix: could not reset setsockopt SO_RCVTIMEO");
            }
        }
        return rc;
    }
    reply = ntohl(u32);

    if (PMIX_PEER_IS_CLIENT(pmix_globals.mypeer) &&
        !PMIX_PEER_IS_TOOL(pmix_globals.mypeer) &&
        !PMIX_PEER_IS_SINGLETON(pmix_globals.mypeer)) {
        rc = pmix_ptl_base_client_handshake(peer, reply);
    } else { // we are a tool
        rc = pmix_ptl_base_tool_handshake(peer, reply);
    }

    if (sockopt) {
        /* return the socket to normal */
        if (0 != setsockopt(peer->sd, SOL_SOCKET, SO_RCVTIMEO, &save, sz)) {
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "pmix: could not reset setsockopt SO_RCVTIMEO");
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_ptl_base_make_connection(pmix_peer_t *peer, char *suri,
                                            pmix_info_t *iptr, size_t niptr)
{
    struct sockaddr_storage myconnection;
    pmix_status_t rc;
    size_t len;
    int retries = 0;

    /* setup the connection */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_setup_connection(suri, &myconnection, &len))) {
        return rc;
    }

retry:
    /* try to connect */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_connect(&myconnection, len, &peer->sd))) {
        /* do not error log - might just be a stale connection point */
        return rc;
    }

    /* send our identity and any authentication credentials to the server */
    if (PMIX_SUCCESS != (rc = send_connect_ack(peer, iptr, niptr))) {
        PMIX_ERROR_LOG(rc);
        CLOSE_THE_SOCKET(peer->sd);
        return rc;
    }

    /* do whatever handshake is required */
    if (PMIX_SUCCESS != (rc = recv_connect_ack(peer))) {
        CLOSE_THE_SOCKET(peer->sd);
        if (PMIX_ERR_TEMP_UNAVAILABLE == rc) {
            ++retries;
            if (retries < pmix_ptl_base.handshake_max_retries) {
                goto retry;
            }
        }
        return rc;
    }

    return PMIX_SUCCESS;
}

void pmix_ptl_base_complete_connection(pmix_peer_t *peer, char *nspace, pmix_rank_t rank,
                                       char *suri)
{
    pmix_kval_t *urikv;
    pmix_status_t rc;

    pmix_globals.connected = true;

    /* setup the server info */
    if (NULL == peer->info) {
        peer->info = PMIX_NEW(pmix_rank_info_t);
    }
    if (NULL == peer->nptr) {
        peer->nptr = PMIX_NEW(pmix_namespace_t);
    }
    if (NULL != peer->nptr->nspace) {
        free(peer->nptr->nspace);
    }
    peer->nptr->nspace = strdup(nspace);

    if (NULL != peer->info->pname.nspace) {
        free(peer->info->pname.nspace);
    }
    peer->info->pname.nspace = strdup(peer->nptr->nspace);
    peer->info->pname.rank = rank;

    /* store the URI for subsequent lookups */
    PMIX_KVAL_NEW(urikv, PMIX_SERVER_URI);
    urikv->value->type = PMIX_STRING;
    pmix_asprintf(&urikv->value->data.string, "%s.%u;%s", nspace, rank, suri);
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, urikv);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }
    PMIX_RELEASE(urikv); // maintain accounting

    pmix_ptl_base_set_nonblocking(peer->sd);

    /* setup recv event */
    pmix_event_assign(&peer->recv_event, pmix_globals.evbase, peer->sd, EV_READ | EV_PERSIST,
                      pmix_ptl_base_recv_handler, peer);
    peer->recv_ev_active = true;
    PMIX_POST_OBJECT(peer);
    pmix_event_add(&peer->recv_event, 0);

    /* setup send event */
    pmix_event_assign(&peer->send_event, pmix_globals.evbase, peer->sd, EV_WRITE | EV_PERSIST,
                      pmix_ptl_base_send_handler, peer);
    peer->send_ev_active = false;
}

pmix_rnd_flag_t pmix_ptl_base_set_flag(size_t *sz)
{
    pmix_rnd_flag_t flag;
    size_t sdsize = 0;

    /* Defined marker values:
     *
     */
    if (PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        if (PMIX_PEER_IS_CLIENT(pmix_globals.mypeer)) {
            /* if we are both launcher and client, then we need
             * to tell the server we are both */
            flag = PMIX_LAUNCHER_CLIENT;
            /* add space for our uid/gid for ACL purposes */
            sdsize += 2 * sizeof(uint32_t);
            /* add space for our identifier */
            sdsize += strlen(pmix_globals.myid.nspace) + 1 + sizeof(uint32_t);
        } else {
            /* add space for our uid/gid for ACL purposes */
            sdsize += 2 * sizeof(uint32_t);
            /* if they gave us an identifier, we need to pass it */
            if (0 < strlen(pmix_globals.myid.nspace)
                && PMIX_RANK_INVALID != pmix_globals.myid.rank) {
                flag = PMIX_LAUNCHER_GIVEN_ID;
                sdsize += strlen(pmix_globals.myid.nspace) + 1 + sizeof(uint32_t);
            } else {
                flag = PMIX_LAUNCHER_NEEDS_ID;
            }
        }

    } else if (PMIX_PEER_IS_SCHEDULER(pmix_globals.mypeer)) {
        /* add space for our uid/gid for ACL purposes */
        sdsize += 2 * sizeof(uint32_t);
        flag = PMIX_SCHEDULER_WITH_ID;
        sdsize += strlen(pmix_globals.myid.nspace) + 1 + sizeof(uint32_t);

    } else if (PMIX_PEER_IS_CLIENT(pmix_globals.mypeer)
               && !PMIX_PEER_IS_TOOL(pmix_globals.mypeer)) {
        if (PMIX_PEER_IS_SINGLETON(pmix_globals.mypeer)) {
            flag = PMIX_SINGLETON_CLIENT;
            /* reserve space for our nspace and rank info */
            sdsize += strlen(pmix_globals.myid.nspace) + 1 + sizeof(uint32_t);
            /* add space for our uid/gid for ACL purposes */
            sdsize += 2 * sizeof(uint32_t);
        } else {
            /* we are a simple client */
            flag = PMIX_SIMPLE_CLIENT;
            /* reserve space for our nspace and rank info */
            sdsize += strlen(pmix_globals.myid.nspace) + 1 + sizeof(uint32_t);
        }
    } else { // must be a tool of some sort
        /* add space for our uid/gid for ACL purposes */
        sdsize += 2 * sizeof(uint32_t);
        if (PMIX_PEER_IS_CLIENT(pmix_globals.mypeer)) {
            /* if we are both tool and client, then we need
             * to tell the server we are both */
            flag = PMIX_TOOL_CLIENT;
            /* add space for our identifier */
            sdsize += strlen(pmix_globals.myid.nspace) + 1 + sizeof(uint32_t);
        } else if (0 < strlen(pmix_globals.myid.nspace)
                   && PMIX_RANK_INVALID != pmix_globals.myid.rank) {
            /* we were given an identifier by the caller, pass it */
            sdsize += strlen(pmix_globals.myid.nspace) + 1 + sizeof(uint32_t);
            flag = PMIX_TOOL_GIVEN_ID;
        } else {
            /* we are a self-started tool that needs an identifier */
            flag = PMIX_TOOL_NEEDS_ID;
        }
    }

    *sz += sdsize;
    return flag;
}

static pmix_status_t construct_message(pmix_peer_t *peer, char **msgout, size_t *sz,
                                       pmix_info_t *iptr, size_t niptr)
{
    char *msg;
    char *sec, *bfrops, *gds;
    pmix_bfrop_buffer_type_t bftype;
    uid_t euid;
    gid_t egid;
    pmix_buffer_t buf;
    pmix_status_t rc;
    pmix_ptl_hdr_t hdr;
    size_t sdsize, csize;
    pmix_byte_object_t cred;

    sdsize = *sz;

    /* setup the header */
    memset(&hdr, 0, sizeof(pmix_ptl_hdr_t));
    hdr.pindex = -1;
    hdr.tag = UINT32_MAX;

    /* add the name of our active sec module - we selected it
     * in pmix_client.c prior to entering here */
    sec = pmix_globals.mypeer->nptr->compat.psec->name;
    sdsize += strlen(sec) + 1;

    /* a security module was assigned to us during rte_init based
     * on a list of available security modules provided by our
     * local PMIx server, if known. Now use that module to
     * get a credential, if the security system provides one. Not
     * every psec module will do so, thus we must first check */
    PMIX_BYTE_OBJECT_CONSTRUCT(&cred);
    PMIX_PSEC_CREATE_CRED(rc, pmix_globals.mypeer, NULL, 0, NULL, 0, &cred);
    if (PMIX_SUCCESS != rc) {
        PMIX_BYTE_OBJECT_DESTRUCT(&cred);
        return rc;
    }
    sdsize += sizeof(uint32_t); // need to pass the number of bytes
    sdsize += cred.size;        // account for the payload itself

    /* add our type flag */
    sdsize += 1;

    /* add our version string */
    sdsize += strlen(PMIX_VERSION) + 1;

    /* add our active bfrops module name */
    bfrops = pmix_globals.mypeer->nptr->compat.bfrops->version;
    sdsize += strlen(bfrops) + 1;
    /* and the type of buffer we are using */
    bftype = pmix_globals.mypeer->nptr->compat.type;
    sdsize += sizeof(bftype);

    /* add our active gds module for working with the server */
    gds = (char *) peer->nptr->compat.gds->name;
    sdsize += strlen(gds) + 1;

    /* if we were given info structs to pass to the server, pack them */
    if (NULL != iptr) {
        PMIX_CONSTRUCT(&buf, pmix_buffer_t);
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &buf, &niptr, 1, PMIX_SIZE);
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &buf, iptr, niptr, PMIX_INFO);
        sdsize += buf.bytes_used;
    }

    /* set the number of bytes to be read beyond the header */
    hdr.nbytes = sdsize;

    /* create a space for our message */
    sdsize = sizeof(hdr) + hdr.nbytes;
    if (NULL == (msg = (char *) malloc(sdsize))) {
        PMIX_BYTE_OBJECT_DESTRUCT(&cred);
        free(sec);
        if (NULL != iptr) {
            PMIX_DESTRUCT(&buf);
        }
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    memset(msg, 0, sdsize);

    /* load the header */
    csize = 0;
    memcpy(msg, &hdr, sizeof(pmix_ptl_hdr_t));
    csize += sizeof(pmix_ptl_hdr_t);

    /* provide our active psec module */
    PMIX_PTL_PUT_STRING(sec);

    /* load the length of the credential */
    PMIX_PTL_PUT_U32(cred.size);

    /* load the credential */
    PMIX_PTL_PUT_BLOB(cred.bytes, cred.size);
    PMIX_BYTE_OBJECT_DESTRUCT(&cred);

    /* load our process type - this is a single byte,
     * so no worry about heterogeneity here */
    PMIX_PTL_PUT_U8(peer->proc_type.flag);

    switch (peer->proc_type.flag) {
    case PMIX_SIMPLE_CLIENT:
        /* simple client process */
        PMIX_PTL_PUT_PROCID(pmix_globals.myid);
        break;

        /* we cannot have cases 1 or 2 because those are only
         * for legacy processes */

    case PMIX_TOOL_NEEDS_ID:
    case PMIX_LAUNCHER_NEEDS_ID:
        /* self-started tool/launcher process that needs an identifier */
        euid = geteuid();
        PMIX_PTL_PUT_U32(euid);
        egid = getegid();
        PMIX_PTL_PUT_U32(egid);
        break;

    case PMIX_TOOL_GIVEN_ID:
    case PMIX_LAUNCHER_GIVEN_ID:
    case PMIX_SCHEDULER_WITH_ID:
    case PMIX_SINGLETON_CLIENT:
        /* self-started tool/launcher/singleton process that was given an identifier by caller */
        euid = geteuid();
        PMIX_PTL_PUT_U32(euid);
        egid = getegid();
        PMIX_PTL_PUT_U32(egid);
        /* add our identifier */
        PMIX_PTL_PUT_PROCID(pmix_globals.myid);
        break;

    case PMIX_TOOL_CLIENT:
    case PMIX_LAUNCHER_CLIENT:
        /* tool/launcher that was started by a PMIx server - identifier specified by server */
        euid = geteuid();
        PMIX_PTL_PUT_U32(euid);
        egid = getegid();
        PMIX_PTL_PUT_U32(egid);
        /* add our identifier */
        PMIX_PTL_PUT_PROCID(pmix_globals.myid);
        break;

    default:
        /* we don't know what they are! */
        if (NULL != iptr) {
            PMIX_DESTRUCT(&buf);
        }
        free(msg);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* provide our version */
    PMIX_PTL_PUT_STRING(PMIX_VERSION);

    /* provide our active bfrops module */
    PMIX_PTL_PUT_STRING(bfrops);

    /* provide the bfrops type */
    PMIX_PTL_PUT_U8(bftype);

    /* provide the gds module */
    PMIX_PTL_PUT_STRING(gds);

    /* provide the info struct bytes */
    if (NULL != iptr) {
        PMIX_PTL_PUT_BLOB(buf.base_ptr, buf.bytes_used);
        PMIX_DESTRUCT(&buf);
    }

    *msgout = msg;
    *sz = sdsize;
    return PMIX_SUCCESS;
}

pmix_status_t pmix_ptl_base_set_timeout(pmix_peer_t *peer, struct timeval *save,
                                        pmix_socklen_t *sz, bool *sockopt)
{
    struct timeval tv;

    /* get the current timeout value so we can reset to it */
    if (0 != getsockopt(peer->sd, SOL_SOCKET, SO_RCVTIMEO, (void *) save, sz)) {
        *sockopt = false;
    } else {
        /* set a timeout on the blocking recv so we don't hang */
        tv.tv_sec = pmix_ptl_base.handshake_wait_time;
        tv.tv_usec = 0;
        if (0 != setsockopt(peer->sd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv))) {
            *sockopt = false;
        }
    }

    return PMIX_SUCCESS;
}

void pmix_ptl_base_setup_socket(pmix_peer_t *peer)
{
    PMIX_HIDE_UNUSED_PARAMS(peer);
#if defined(TCP_NODELAY)
    int optval;
    optval = 1;
    if (setsockopt(peer->sd, IPPROTO_TCP, TCP_NODELAY, (char *) &optval, sizeof(optval)) < 0) {
        opal_backtrace_print(stderr, NULL, 1);
        pmix_output_verbose(5, pmix_ptl_base_framework.framework_output,
                            "[%s:%d] setsockopt(TCP_NODELAY) failed: %s (%d)", __FILE__, __LINE__,
                            strerror(pmix_socket_errno), pmix_socket_errno);
    }
#endif
#if defined(SO_NOSIGPIPE)
    /* Some BSD flavors generate EPIPE when we write to a disconnected peer. We need
     * the prevent this signal to be able to trap socket shutdown and cleanly release
     * the endpoint.
     */
    int optval2 = 1;
    if (setsockopt(peer->sd, SOL_SOCKET, SO_NOSIGPIPE, (char *) &optval2, sizeof(optval2)) < 0) {
        pmix_output_verbose(5, pmix_ptl_base_framework.framework_output,
                            "[%s:%d] setsockopt(SO_NOSIGPIPE) failed: %s (%d)", __FILE__, __LINE__,
                            strerror(pmix_socket_errno), pmix_socket_errno);
    }
#endif
}

pmix_status_t pmix_ptl_base_client_handshake(pmix_peer_t *peer, pmix_status_t reply)
{
    pmix_status_t rc;

    /* see if they want us to do the handshake */
    if (PMIX_ERR_READY_FOR_HANDSHAKE == reply) {
        PMIX_PSEC_CLIENT_HANDSHAKE(rc, peer, peer->sd);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
    } else if (PMIX_SUCCESS != reply) {
        return reply;
    }
    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix: RECV CONNECT CONFIRMATION");

    /* receive our index into the peer's client array */
    PMIX_PTL_RECV_U32(peer->sd, pmix_globals.pindex);
    return PMIX_SUCCESS;
}

/* the "peer" object passed into this function is that of the SERVER
 * to which the tool is connecting - it is NOT the peer of the tool itself*/
pmix_status_t pmix_ptl_base_tool_handshake(pmix_peer_t *peer, pmix_status_t rp)
{
    pmix_nspace_t nspace;
    pmix_rank_t rank;
    pmix_status_t reply;

    /* if the status indicates an error, then we are done */
    if (PMIX_SUCCESS != rp) {
        return rp;
    }

    /* if we need an identifier, it comes next */
    if (PMIX_TOOL_NEEDS_ID == peer->proc_type.flag ||
        PMIX_LAUNCHER_NEEDS_ID == peer->proc_type.flag) {
        PMIX_PTL_RECV_NSPACE(peer->sd, pmix_globals.myid.nspace);
        PMIX_PTL_RECV_U32(peer->sd, pmix_globals.myid.rank);
    }

    /* get the server's nspace and rank so we can send to it */
    if (NULL == peer->info) {
        peer->info = PMIX_NEW(pmix_rank_info_t);
    }
    if (NULL == peer->nptr) {
        peer->nptr = PMIX_NEW(pmix_namespace_t);
    }
    PMIX_PTL_RECV_NSPACE(peer->sd, nspace);
    PMIX_PTL_RECV_U32(peer->sd, rank);

    if (NULL != peer->nptr->nspace) {
        free(peer->nptr->nspace);
    }
    peer->nptr->nspace = strdup(nspace);
    if (NULL != peer->info->pname.nspace) {
        free(peer->info->pname.nspace);
    }
    peer->info->pname.nspace = strdup(nspace);
    peer->info->pname.rank = rank;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix: RECV CONNECT CONFIRMATION FOR TOOL %s:%d FROM SERVER %s:%d",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank, peer->info->pname.nspace,
                        peer->info->pname.rank);

    /* get the returned status from the security handshake */
    PMIX_PTL_RECV_U32(peer->sd, reply);
    if (PMIX_SUCCESS != reply) {
        /* see if they want us to do the handshake */
        if (PMIX_ERR_READY_FOR_HANDSHAKE == reply) {
            PMIX_PSEC_CLIENT_HANDSHAKE(reply, peer, peer->sd);
            if (PMIX_SUCCESS != reply) {
                return reply;
            }
            /* if the handshake succeeded, then fall thru to the next step */
        } else {
            return reply;
        }
    }

    return PMIX_SUCCESS;
}

static void check_server(char *filename, pmix_list_t *servers)
{
    FILE *fp;
    char *srvr, *p, *p2;
    pmix_lock_t lock;
    pmix_event_t ev;
    struct timeval tv;
    int retries;
    pmix_info_t *sdata;
    size_t ndata, n;
    pmix_infolist_t *iptr, *ians;
    char *nspace = NULL, *version = NULL;
    pmix_rank_t rank;
    pmix_list_t mylist;
    uint32_t u32;
    pmix_status_t rc;

    /* if we cannot open the file, then the server must not
     * be configured to support tool connections, or this
     * user isn't authorized to access it - or it may just
     * not exist yet! Check for existence */
    /* coverity[TOCTOU] */
    if (0 == access(filename, R_OK)) {
        goto process;
    } else {
        if (ENOENT == errno) {
            /* the file does not exist, so give it
             * a little time to see if the server
             * is still starting up */
            retries = 0;
            do {
                ++retries;
                pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                    "WAITING FOR CONNECTION FILE %s", filename);
                PMIX_CONSTRUCT_LOCK(&lock);
                if (0 < pmix_ptl_base.wait_to_connect) {
                    tv.tv_sec = pmix_ptl_base.wait_to_connect;
                    tv.tv_usec = 0;
                    pmix_event_evtimer_set(pmix_globals.evbase, &ev, timeout, &lock);
                    PMIX_POST_OBJECT(&ev);
                    pmix_event_evtimer_add(&ev, &tv);
                } else {
                    tv.tv_sec = 0;
                    tv.tv_usec = 10000; // use 0.01 sec as default
                    pmix_event_evtimer_set(pmix_globals.evbase, &ev, timeout, &lock);
                    PMIX_POST_OBJECT(&ev);
                    pmix_event_evtimer_add(&ev, &tv);
                }
                PMIX_WAIT_THREAD(&lock);
                PMIX_DESTRUCT_LOCK(&lock);
                /* coverity[TOCTOU] */
                if (0 == access(filename, R_OK)) {
                    goto process;
                }
            } while (retries < pmix_ptl_base.max_retries);
            /* otherwise, it is unreachable */
        }
    }
    return;

process:
    fp = fopen(filename, "r");
    if (NULL == fp) {
        return;
    }
    /* get the URI - might seem crazy, but there is actually
     * a race condition here where the server may have created
     * the file but not yet finished writing into it. So give
     * us a chance to get the required info */
    for (retries = 0; retries < 3; retries++) {
        srvr = pmix_getline(fp);
        if (NULL != srvr) {
            break;
        }
        fclose(fp);
        tv.tv_sec = 0;
        tv.tv_usec = 10000; // use 0.01 sec as default
        pmix_event_evtimer_set(pmix_globals.evbase, &ev, timeout, &lock);
        PMIX_POST_OBJECT(&ev);
        pmix_event_evtimer_add(&ev, &tv);
        PMIX_WAIT_THREAD(&lock);
        PMIX_DESTRUCT_LOCK(&lock);
        fp = fopen(filename, "r");
        if (NULL == fp) {
            return;
        }
    }
    if (NULL == srvr) {
        PMIX_ERROR_LOG(PMIX_ERR_FILE_READ_FAILURE);
        fclose(fp);
        return;
    }
    rc = pmix_ptl_base_parse_uri(srvr, &nspace, &rank, NULL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        fclose(fp);
        if (NULL != nspace) {
            free(nspace);
        }
        free(srvr);
        return;
    }

    /* see if we already have this server in our list */
    PMIX_LIST_FOREACH (iptr, servers, pmix_infolist_t) {
        /* each item contains an array starting with the server nspace */
        sdata = (pmix_info_t *) iptr->info.value.data.darray->array;
        ndata = iptr->info.value.data.darray->size;
        if (0 == strcmp(sdata[0].value.data.string, nspace) && sdata[1].value.data.rank == rank) {
            /* already have this one */
            fclose(fp);
            free(srvr);
            free(nspace);
            return;
        }
    }

    /* begin collecting data for the new entry */
    PMIX_CONSTRUCT(&mylist, pmix_list_t);
    iptr = PMIX_NEW(pmix_infolist_t);
    PMIX_INFO_LOAD(&iptr->info, PMIX_SERVER_NSPACE, nspace, PMIX_STRING);
    pmix_list_append(&mylist, &iptr->super);
    iptr = PMIX_NEW(pmix_infolist_t);
    PMIX_INFO_LOAD(&iptr->info, PMIX_SERVER_RANK, &rank, PMIX_PROC_RANK);
    pmix_list_append(&mylist, &iptr->super);

    free(srvr);
    free(nspace);

    /* see if this file contains the server's version */
    p2 = pmix_getline(fp);
    if (NULL == p2) {
        version = strdup("v2.0");
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output, "V20 SERVER DETECTED");
    } else {
        version = p2;
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "VERSION %s SERVER DETECTED", p2);
    }
    iptr = PMIX_NEW(pmix_infolist_t);
    PMIX_INFO_LOAD(&iptr->info, PMIX_VERSION_INFO, version, PMIX_STRING);
    pmix_list_append(&mylist, &iptr->super);
    free(p2);

    /* see if the file contains the pid */
    p2 = pmix_getline(fp);
    if (NULL == p2) {
        goto complete;
    }
    u32 = strtoul(p2, NULL, 10);
    iptr = PMIX_NEW(pmix_infolist_t);
    PMIX_INFO_LOAD(&iptr->info, PMIX_SERVER_PIDINFO, &u32, PMIX_UINT32);
    pmix_list_append(&mylist, &iptr->super);
    free(p2);

    /* check for uid:gid */
    p2 = pmix_getline(fp);
    if (NULL == p2) {
        goto complete;
    }
    /* find the colon */
    if (NULL == (p = strchr(p2, ':'))) {
        /* bad format */
        free(p2);
        goto complete;
    }
    *p = '\0';
    ++p;
    u32 = strtoul(p2, NULL, 10);
    iptr = PMIX_NEW(pmix_infolist_t);
    PMIX_INFO_LOAD(&iptr->info, PMIX_USERID, &u32, PMIX_UINT32);
    pmix_list_append(&mylist, &iptr->super);
    u32 = strtoul(p, NULL, 10);
    iptr = PMIX_NEW(pmix_infolist_t);
    PMIX_INFO_LOAD(&iptr->info, PMIX_GRPID, &u32, PMIX_UINT32);
    pmix_list_append(&mylist, &iptr->super);
    free(p2);

    /* check for timestamp */
    p2 = pmix_getline(fp);
    if (NULL == p2) {
        goto complete;
    }
    iptr = PMIX_NEW(pmix_infolist_t);
    PMIX_INFO_LOAD(&iptr->info, PMIX_SERVER_START_TIME, p2, PMIX_STRING);
    pmix_list_append(&mylist, &iptr->super);
    free(p2);

complete:
    fclose(fp);

    /* convert the list to an array */
    if (0 < (ndata = pmix_list_get_size(&mylist))) {
        ians = PMIX_NEW(pmix_infolist_t);
        PMIX_LOAD_KEY(ians->info.key, PMIX_SERVER_INFO_ARRAY);
        ians->info.value.type = PMIX_DATA_ARRAY;
        PMIX_DATA_ARRAY_CREATE(ians->info.value.data.darray, ndata, PMIX_INFO);
        sdata = (pmix_info_t *) ians->info.value.data.darray->array;
        n = 0;
        PMIX_LIST_FOREACH (iptr, &mylist, pmix_infolist_t) {
            PMIX_INFO_XFER(&sdata[n], &iptr->info);
            ++n;
        }
        PMIX_LIST_DESTRUCT(&mylist);
        pmix_list_append(servers, &ians->super);
    }
}

static void query_servers(char *dirname, pmix_list_t *servers)
{
    char *newdir, *dname;
    DIR *cur_dirp, *tst;
    struct dirent *dir_entry;

    /* search the system tmpdir directory tree for files
     * beginning with "pmix." as these can be potential
     * servers */

    if (NULL == dirname) {
        /* we first check the system tmpdir to see if a system-level
         * server is present */
        dname = pmix_ptl_base.system_tmpdir;
    } else {
        dname = dirname;
    }
    cur_dirp = opendir(dname);
    if (NULL == cur_dirp) {
        return;
    }

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:tcp: searching directory %s",
                        (NULL == dirname) ? pmix_ptl_base.system_tmpdir : dirname);

    /* search the entries for something that starts with the "pmix." prefix */
    while (NULL != (dir_entry = readdir(cur_dirp))) {
        /* ignore the . and .. entries */
        if (0 == strcmp(dir_entry->d_name, ".") || 0 == strcmp(dir_entry->d_name, "..")) {
            continue;
        }
        newdir = pmix_os_path(false, dname, dir_entry->d_name, NULL);
        /* if it is a directory, down search */
        tst = opendir(newdir);
        if (NULL != tst) {
            closedir(tst);
            query_servers(newdir, servers);
            free(newdir);
            continue;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "pmix:tcp: checking %s",
                            dir_entry->d_name);
        /* see if it starts with our prefix */
        if (0 == strncmp(dir_entry->d_name, "pmix.", strlen("pmix."))) {
            /* try to read this file */
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "pmix:tcp: reading file %s", newdir);
            check_server(newdir, servers);
        }
        free(newdir);
    }
    closedir(cur_dirp);
}

static void _local_relcb(void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t *) cbdata;

    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    PMIX_RELEASE(cd);
}

void pmix_ptl_base_query_servers(int sd, short args, void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t *) cbdata;
    pmix_list_t servers;
    size_t n;
    pmix_infolist_t *iptr;
    pmix_status_t rc;

    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_CONSTRUCT(&servers, pmix_list_t);

    query_servers(NULL, &servers);

    /* convert the list to an array of pmix_info_t */
    cd->ninfo = pmix_list_get_size(&servers);
    if (0 == cd->ninfo) {
        rc = PMIX_ERR_NOT_FOUND;
    } else {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        n = 0;
        PMIX_LIST_FOREACH (iptr, &servers, pmix_infolist_t) {
            PMIX_INFO_XFER(&cd->info[n], &iptr->info);
            ++n;
        }
        rc = PMIX_SUCCESS;
    }
    PMIX_LIST_DESTRUCT(&servers);

    /* execute the callback function */
    cd->cbfunc(rc, cd->info, cd->ninfo, cd->cbdata, _local_relcb, cd);
}

static void timeout(int sd, short args, void *cbdata)
{
    pmix_lock_t *lock = (pmix_lock_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_WAKEUP_THREAD(lock);
}

/*
 * Go through a list of argv; if there are any subnet specifications
 * (a.b.c.d/e), resolve them to an interface name (Currently only
 * supporting IPv4).  If unresolvable, warn and remove.
 */
char **pmix_ptl_base_split_and_resolve(const char *orig_str,
                                       const char *name)
{
    int i, ret, if_index;
    char **argv, **interfaces, *str;
    char if_name[PMIX_IF_NAMESIZE];
    struct sockaddr_storage argv_inaddr, if_inaddr;
    uint32_t argv_prefix;
    bool found;

    /* Sanity check */
    if (NULL == orig_str) {
        return NULL;
    }

    argv = PMIx_Argv_split(orig_str, ',');
    interfaces = NULL;
    for (i = 0; NULL != argv[i]; ++i) {
        if (isalpha(argv[i][0])) {
            /* This is an interface name. If not already in the interfaces array, add it */
            PMIx_Argv_append_unique_nosize(&interfaces, argv[i]);
            pmix_output_verbose(20,
                                pmix_ptl_base_framework.framework_output,
                                "ptl:tool: Using interface: %s ", argv[i]);
            continue;
        }

        /* Found a subnet notation.  Convert it to an IP
           address/netmask.  Get the prefix first. */
        argv_prefix = 0;
        str = strchr(argv[i], '/');
        if (NULL == str) {
            pmix_show_help("help-ptl-base.txt", "invalid if_inexclude", true,
                           name, pmix_globals.hostname, argv[i],
                           "Invalid specification (missing \"/\")");
            continue;
        }
        *str = '\0';
        argv_prefix = atoi(str + 1);

        /* Now convert the IPv4 address */
        ((struct sockaddr *) &argv_inaddr)->sa_family = AF_INET;
        ret = inet_pton(AF_INET, argv[i], &((struct sockaddr_in *) &argv_inaddr)->sin_addr);
        *str = '/';

        if (1 != ret) {
            pmix_show_help("help-ptl-base.txt", "invalid if_inexclude", true,
                           name, pmix_globals.hostname, argv[i],
                           "Invalid specification (inet_pton() failed)");
            continue;
        }
        pmix_output_verbose(20, pmix_ptl_base_framework.framework_output,
                            "ptl:base: Searching for %s address+prefix: %s / %u", name,
                            pmix_net_get_hostname((struct sockaddr *) &argv_inaddr), argv_prefix);

        /* Go through all interfaces and see if we can find a match */
        found = false;
        for (if_index = pmix_ifbegin(); if_index >= 0; if_index = pmix_ifnext(if_index)) {
            pmix_ifindextoaddr(if_index,
                               (struct sockaddr*) &if_inaddr,
                               sizeof(if_inaddr));
            if (pmix_net_samenetwork(&argv_inaddr, &if_inaddr, argv_prefix)) {
                /* We found a match. If it's not already in the interfaces array,
                   add it. If it's already in the array, treat it as a match */
                found = true;
                pmix_ifindextoname(if_index, if_name, sizeof(if_name));
                PMIx_Argv_append_unique_nosize(&interfaces, if_name);
                pmix_output_verbose(20,
                                    pmix_ptl_base_framework.framework_output,
                                    "ptl:base: Found match: %s (%s)",
                                    pmix_net_get_hostname((struct sockaddr*) &if_inaddr),
                                    if_name);
            }

        }
        /* If we didn't find a match, report it but keep trying */
        if (!found) {
            pmix_show_help("help-ptl-base.txt", "invalid if_inexclude", true,
                           name, pmix_globals.hostname, argv[i],
                           "Did not find interface matching this subnet");
        }
    }

    PMIx_Argv_free(argv);
    return interfaces;
}
