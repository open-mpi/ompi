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
#include "include/pmix_stdint.h"

#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UIO_H
#    include <sys/uio.h>
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
#if OAC_HAVE_APPLE && defined(HAVE_SYS_SYSCTL_H)
#    include <sys/sysctl.h>
#endif

#include "src/client/pmix_client_ops.h"
#include "src/common/pmix_attributes.h"
#include "src/include/pmix_globals.h"
#include "src/include/pmix_socket_errno.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/gds.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_getid.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"
#include "src/util/pmix_strnlen.h"

#include "src/mca/ptl/base/base.h"

pmix_status_t pmix_ptl_base_set_nonblocking(int sd)
{
    int flags;
    /* setup the socket as non-blocking */
    if ((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        pmix_output(0, "ptl:base:set_nonblocking: fcntl(F_GETFL) failed: %s (%d)\n",
                    strerror(pmix_socket_errno), pmix_socket_errno);
    } else {
        flags |= O_NONBLOCK;
        if (fcntl(sd, F_SETFL, flags) < 0)
            pmix_output(0, "ptl:base:set_nonblocking: fcntl(F_SETFL) failed: %s (%d)\n",
                        strerror(pmix_socket_errno), pmix_socket_errno);
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_ptl_base_set_blocking(int sd)
{
    int flags;
    /* setup the socket as non-blocking */
    if ((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        pmix_output(0, "ptl:base:set_blocking: fcntl(F_GETFL) failed: %s (%d)\n",
                    strerror(pmix_socket_errno), pmix_socket_errno);
    } else {
        flags &= ~(O_NONBLOCK);
        if (fcntl(sd, F_SETFL, flags) < 0)
            pmix_output(0, "ptl:base:set_blocking: fcntl(F_SETFL) failed: %s (%d)\n",
                        strerror(pmix_socket_errno), pmix_socket_errno);
    }
    return PMIX_SUCCESS;
}

/*
 * A blocking send on a non-blocking socket. Used to send the small amount of connection
 * information that identifies the peers endpoint.
 */
pmix_status_t pmix_ptl_base_send_blocking(int sd, char *ptr, size_t size)
{
    size_t cnt = 0;
    int retval;

    pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                        "send blocking of %" PRIsize_t " bytes to socket %d", size, sd);
    while (cnt < size) {
        retval = send(sd, (char *) ptr + cnt, size - cnt, 0);
        if (retval < 0) {
            if (EAGAIN == pmix_socket_errno || EWOULDBLOCK == pmix_socket_errno) {
                /* just cycle and let it try again */
                pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                                    "blocking_send received error %d:%s from remote - cycling",
                                    pmix_socket_errno, strerror(pmix_socket_errno));
                continue;
            }
            if (pmix_socket_errno != EINTR) {
                pmix_output_verbose(
                    8, pmix_ptl_base_framework.framework_output,
                    "ptl:base:peer_send_blocking: send() to socket %d failed: %s (%d)\n", sd,
                    strerror(pmix_socket_errno), pmix_socket_errno);
                return PMIX_ERR_UNREACH;
            }
            continue;
        }
        cnt += retval;
    }

    pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                        "blocking send complete to socket %d", sd);
    return PMIX_SUCCESS;
}

/*
 * A blocking recv on a non-blocking socket. Used to receive the small amount of connection
 * information that identifies the peers endpoint.
 */
pmix_status_t pmix_ptl_base_recv_blocking(int sd, char *data, size_t size)
{
    size_t cnt = 0;

    pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                        "waiting for blocking recv of %" PRIsize_t " bytes", size);

    while (cnt < size) {
        int retval = recv(sd, (char *) data + cnt, size - cnt, MSG_WAITALL);

        /* remote closed connection */
        if (retval == 0) {
            pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                                "ptl:base:recv_blocking: remote closed connection");
            return PMIX_ERR_UNREACH;
        }

        /* handle errors */
        if (retval < 0) {
            if (EAGAIN == pmix_socket_errno || EWOULDBLOCK == pmix_socket_errno) {
                /* just cycle and let it try again */
                pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                                    "blocking_recv received error %d:%s from remote - cycling",
                                    pmix_socket_errno, strerror(pmix_socket_errno));
		continue;
            }
            if (pmix_socket_errno != EINTR) {
                /* If we overflow the listen backlog, it's
                   possible that even though we finished the three
                   way handshake, the remote host was unable to
                   transition the connection from half connected
                   (received the initial SYN) to fully connected
                   (in the listen backlog).  We likely won't see
                   the failure until we try to receive, due to
                   timing and the like.  The first thing we'll get
                   in that case is a RST packet, which receive
                   will turn into a connection reset by peer
                   errno.  In that case, leave the socket in
                   CONNECT_ACK and propagate the error up to
                   recv_connect_ack, who will try to establish the
                   connection again */
                pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                                    "blocking_recv received error %d:%s from remote - aborting",
                                    pmix_socket_errno, strerror(pmix_socket_errno));
                return PMIX_ERR_UNREACH;
            }
            continue;
        }
        cnt += retval;
    }

    pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                        "blocking receive complete from remote");
    return PMIX_SUCCESS;
}

#define PMIX_MAX_RETRIES 10

pmix_status_t pmix_ptl_base_connect(struct sockaddr_storage *addr,
                                    pmix_socklen_t addrlen, int *fd)
{
    int sd = -1, sd2;
    int retries = -1;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "ptl_base_connect: attempting to connect to server");

    /* Create the new socket */
    sd = socket(addr->ss_family, SOCK_STREAM, 0);

    while (retries < PMIX_MAX_RETRIES) {
        retries++;
        if (sd < 0) {
            pmix_output(0, "pmix:create_socket: socket() failed: %s (%d)\n",
                        strerror(pmix_socket_errno), pmix_socket_errno);
            sd = socket(addr->ss_family, SOCK_STREAM, 0);
            continue;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "pmix_ptl_base_connect: attempting to connect to server on socket %d",
                            sd);
        /* try to connect */
        if (connect(sd, (struct sockaddr *) addr, addrlen) < 0) {
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "Connect failed: %s (%d)", strerror(pmix_socket_errno),
                                pmix_socket_errno);
            /* get a different socket, but do that BEFORE we release the current
             * one so we don't just get the same socket handed back to us */
            sd2 = socket(addr->ss_family, SOCK_STREAM, 0);
            CLOSE_THE_SOCKET(sd);
            sd = sd2;
            continue;
        } else {
            /* otherwise, the connect succeeded - so break out of the loop */
            break;
        }
    }

    if (retries == PMIX_MAX_RETRIES || sd < 0) {
        /* We were unsuccessful in establishing this connection, and are
         * not likely to suddenly become successful */
        if (0 <= sd) {
            CLOSE_THE_SOCKET(sd);
        }
        return PMIX_ERR_UNREACH;
    }
    *fd = sd;

    return PMIX_SUCCESS;
}

char *pmix_ptl_base_get_cmd_line(void)
{
    char *p = NULL;

#if OAC_HAVE_APPLE
    int mib[3], argmax, nargs, num;
    size_t size;
    char *procargs = NULL, *cp, *cptr;
    char **stack = NULL;

    /* Get the maximum process arguments size. */
    mib[0] = CTL_KERN;
    mib[1] = KERN_ARGMAX;
    size = sizeof(argmax);

    if (sysctl(mib, 2, &argmax, &size, NULL, 0) == -1) {
        fprintf(stderr, "sysctl() argmax failed\n");
        return NULL;
    }

    /* Allocate space for the arguments. */
    procargs = (char *) malloc(argmax);
    if (procargs == NULL) {
        return NULL;
    }

    /* Make a sysctl() call to get the raw argument space of the process. */
    mib[0] = CTL_KERN;
    mib[1] = KERN_PROCARGS2;
    mib[2] = getpid();

    size = (size_t) argmax;

    if (sysctl(mib, 3, procargs, &size, NULL, 0) == -1) {
        fprintf(stderr, "Lacked permissions\n");
        ;
        free(procargs);
        return NULL;
    }

    memcpy(&nargs, procargs, sizeof(nargs));
    /* this points to the executable - skip over that to get the rest */
    cp = procargs + sizeof(nargs);
    cp += strlen(cp);
    /* this is the first argv */
    PMIx_Argv_append_nosize(&stack, cp);
    /* skip any embedded NULLs */
    while (cp < &procargs[size] && '\0' == *cp) {
        ++cp;
    }
    if (cp != &procargs[size]) {
        /* from this point, we have the argv separated by NULLs - split them out */
        cptr = cp;
        num = 0;
        while (cp < &procargs[size] && num < nargs) {
            if ('\0' == *cp) {
                PMIx_Argv_append_nosize(&stack, cptr);
                ++cp; // skip over the NULL
                cptr = cp;
                ++num;
            } else {
                ++cp;
            }
        }
    }

    p = PMIx_Argv_join(stack, ' ');
    PMIx_Argv_free(stack);
    free(procargs);
#else
    char tmp[512];
    FILE *fp;
    pid_t mypid;

    /* open the pid's info file */
    mypid = getpid();
    pmix_snprintf(tmp, 512, "/proc/%lu/cmdline", (unsigned long) mypid);
    fp = fopen(tmp, "r");
    if (NULL != fp) {
        /* read the cmd line */
        if (NULL == fgets(tmp, 512, fp)) {
            fclose(fp);
            return NULL;
        }
        if (0 != fclose(fp)) {
            ;
        }
        p = strdup(tmp);
    }
#endif
    return p;
}

static pmix_status_t check_connections(pmix_list_t *connections)
{
    size_t len;
    pmix_connection_t *cn, *cnbase;

    len = pmix_list_get_size(connections);
    if (0 == len) {
        return PMIX_ERR_NOT_FOUND;
    }
    if (1 == len) {
        return PMIX_SUCCESS;
    }
    /* check to see if all the connections are to the same target */
    cnbase = (pmix_connection_t *) pmix_list_get_first(connections);
    PMIX_LIST_FOREACH (cn, connections, pmix_connection_t) {
        if (cn == cnbase) {
            continue;
        }
        if (0 != strcmp(cn->uri, cnbase->uri)) { // contains nspace and rank plus port
            pmix_show_help("help-ptl-base.txt", "too-many-conns", true);
            return PMIX_ERR_UNREACH;
        }
    }
    /* they are all to the same server */
    return PMIX_SUCCESS;
}

static pmix_status_t tryfile(pmix_peer_t *peer, char **nspace,
                             pmix_rank_t *rank, char **suri,
                             bool optional, char *filename)
{
    pmix_list_t connections;
    pmix_status_t rc;
    pmix_connection_t *cn;

    /* try to read the file */
    PMIX_CONSTRUCT(&connections, pmix_list_t);
    rc = pmix_ptl_base_parse_uri_file(filename, optional, &connections);
    if (PMIX_SUCCESS == rc) {
        rc = check_connections(&connections);
        if (PMIX_SUCCESS != rc) {
            PMIX_LIST_DESTRUCT(&connections);
            return rc;
        }
        cn = (pmix_connection_t *) pmix_list_get_first(&connections);
        *nspace = cn->nspace;
        cn->nspace = NULL;
        *rank = cn->rank;
        *suri = cn->uri;
        cn->uri = NULL;
        peer->protocol = PMIX_PROTOCOL_V2;
        PMIX_SET_PEER_VERSION(peer, cn->version, 2, 0);
    }
    PMIX_LIST_DESTRUCT(&connections);
    return rc;
}

static pmix_status_t trysearch(pmix_peer_t *peer, char **nspace,
                               pmix_rank_t *rank, char **suri,
                               char *filename, pmix_info_t *iptr, size_t niptr,
                               bool optional)
{
    pmix_list_t connections;
    pmix_status_t rc;
    pmix_connection_t *cn;

    PMIX_CONSTRUCT(&connections, pmix_list_t);
    rc = pmix_ptl_base_df_search(pmix_ptl_base.system_tmpdir, filename, iptr, niptr,
                                 optional, &connections);
    if (PMIX_SUCCESS == rc) {
        rc = check_connections(&connections);
        if (PMIX_SUCCESS != rc) {
            PMIX_LIST_DESTRUCT(&connections);
            return rc;
        }
        cn = (pmix_connection_t *) pmix_list_get_first(&connections);
        peer->protocol = PMIX_PROTOCOL_V2;
        PMIX_SET_PEER_VERSION(peer, cn->version, 2, 0);
        *nspace = cn->nspace;
        cn->nspace = NULL;
        *rank = cn->rank;
        *suri = cn->uri;
        cn->uri = NULL;
        PMIX_LIST_DESTRUCT(&connections);
        return rc;
    } else if (1 < pmix_list_get_size(&connections)) {
        pmix_show_help("help-ptl-base.txt", "too-many-conns", true);
    }
    PMIX_LIST_DESTRUCT(&connections);
    return rc;
}

pmix_status_t pmix_ptl_base_connect_to_peer(struct pmix_peer_t *pr, pmix_info_t *info, size_t ninfo)
{
    char *suri = NULL, *st, *evar;
    char *filename, *nspace = NULL;
    char **order = NULL;
    const char* tmp;
    pmix_rank_t rank = PMIX_RANK_WILDCARD;
    char *p = NULL, *server_nspace = NULL, *rendfile = NULL;
    int rc;
    size_t n, m;
    pid_t pid = 0, mypid;
    pmix_list_t ilist;
    pmix_info_caddy_t *kv;
    pmix_info_t *iptr = NULL, mypidinfo, mycmdlineinfo, launcher;
    size_t niptr = 0;
    pmix_peer_t *peer = (pmix_peer_t *) pr;
    bool optional = false;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "ptl:base: connecting to server");

    /* check for common directives */
    rc = pmix_ptl_base_check_directives(info, ninfo);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    /* check any provided directives
     * to see where they want us to connect to */
    PMIX_CONSTRUCT(&ilist, pmix_list_t);
    if (NULL != info) {
        for (n = 0; n < ninfo; n++) {
            if (PMIX_CHECK_KEY(&info[n], PMIX_CONNECT_TO_SYSTEM)) {
                if (PMIX_INFO_TRUE(&info[n])) {
                    if (NULL != order) {
                        // overrides all prior specs
                        PMIx_Argv_free(order);
                        order = NULL;
                    }
                    PMIx_Argv_append_nosize(&order, PMIX_CONNECT_TO_SYSTEM);
                }

            } else if (PMIX_CHECK_KEY(&info[n], PMIX_CONNECT_SYSTEM_FIRST)) {
                /* try the system-level */
                if (PMIX_INFO_TRUE(&info[n])) {
                    PMIx_Argv_prepend_nosize(&order, PMIX_CONNECT_SYSTEM_FIRST);
                }

            } else if (PMIX_CHECK_KEY(&info[n], PMIX_CONNECT_TO_SCHEDULER)) {
                /* find the scheduler */
                if (PMIX_INFO_TRUE(&info[n])) {
                    PMIx_Argv_append_nosize(&order, PMIX_CONNECT_TO_SCHEDULER);
                }

            } else if (PMIX_CHECK_KEY(&info[n], PMIX_CONNECT_TO_SYS_CONTROLLER)) {
                /* find the system controller */
                if (PMIX_INFO_TRUE(&info[n])) {
                    PMIx_Argv_append_nosize(&order, PMIX_CONNECT_TO_SYS_CONTROLLER);
                }

            } else if (PMIX_CHECK_KEY(&info[n], PMIX_CONNECTION_ORDER)) {
                if (NULL != order) {
                    // overrides all prior specs
                    PMIx_Argv_free(order);
                    order = NULL;
                }
                order = PMIx_Argv_split(info[n].value.data.string, ',');
                // the strings will just be the name of the attribute, so we
                // must convert them to the attribute values
                for (m=0; NULL != order[m]; m++) {
                    tmp = pmix_attributes_lookup(order[m]);
                    free(order[m]);
                    order[m] = strdup(tmp);
                }

            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_PIDINFO)) {
                pid = info[n].value.data.pid;

            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_NSPACE)) {
                // if this is my nspace, then ignore it
                if (0 == strcmp(pmix_globals.myid.nspace, info[n].value.data.string)) {
                    continue;
                }
                if (NULL != server_nspace) {
                    /* they included it more than once */
                    if (0 == strcmp(server_nspace, info[n].value.data.string)) {
                        /* same value, so ignore it */
                        continue;
                    }
                    /* otherwise, we don't know which one to use */
                    rc = PMIX_ERR_BAD_PARAM;
                    goto cleanup;
                }
                server_nspace = strdup(info[n].value.data.string);

            } else if (PMIX_CHECK_KEY(&info[n], PMIX_TOOL_ATTACHMENT_FILE)) {
                if (NULL != rendfile) {
                    free(rendfile);
                }
                rendfile = strdup(info[n].value.data.string);

            } else if (PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)
                       && PMIX_CHECK_KEY(&info[n], PMIX_LAUNCHER_RENDEZVOUS_FILE)) {
                if (NULL != pmix_ptl_base.rendezvous_filename) {
                    free(pmix_ptl_base.rendezvous_filename);
                }
                pmix_ptl_base.rendezvous_filename = strdup(info[n].value.data.string);

            } else if (PMIX_CHECK_KEY(&info[n], PMIX_TOOL_CONNECT_OPTIONAL)) {
                optional = PMIX_INFO_TRUE(&info[n]);

            } else {
                /* need to pass this to server */
                kv = PMIX_NEW(pmix_info_caddy_t);
                kv->info = &info[n];
                pmix_list_append(&ilist, &kv->super);
            }
        }
    }

    /* add our pid to the array */
    kv = PMIX_NEW(pmix_info_caddy_t);
    mypid = getpid();
    PMIX_INFO_LOAD(&mypidinfo, PMIX_PROC_PID, &mypid, PMIX_PID);
    kv->info = &mypidinfo;
    pmix_list_append(&ilist, &kv->super);

    /* if I am a launcher, tell them so */
    if (PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        kv = PMIX_NEW(pmix_info_caddy_t);
        PMIX_INFO_LOAD(&launcher, PMIX_LAUNCHER, NULL, PMIX_BOOL);
        kv->info = &launcher;
        pmix_list_append(&ilist, &kv->super);
    }

    /* add our cmd line to the array */
    p = pmix_ptl_base_get_cmd_line();
    if (NULL != p) {
        /* pass it along */
        kv = PMIX_NEW(pmix_info_caddy_t);
        PMIX_INFO_LOAD(&mycmdlineinfo, PMIX_CMD_LINE, p, PMIX_STRING);
        kv->info = &mycmdlineinfo;
        pmix_list_append(&ilist, &kv->super);
        free(p);
    }

    /* if we need to pass anything, setup an array */
    if (0 < (niptr = pmix_list_get_size(&ilist))) {
        PMIX_INFO_CREATE(iptr, niptr);
        n = 0;
        while (NULL != (kv = (pmix_info_caddy_t *) pmix_list_remove_first(&ilist))) {
            PMIX_INFO_XFER(&iptr[n], kv->info);
            PMIX_RELEASE(kv);
            ++n;
        }
    }
    PMIX_LIST_DESTRUCT(&ilist);

    /* mark that we are using the V2 protocol */
    pmix_globals.mypeer->protocol = PMIX_PROTOCOL_V2;
    /* if we were given a URI, then look no further */
    if (NULL != pmix_ptl_base.uri) {
        /* if the string starts with "file:", then they are pointing
         * us to a file we need to read to get the URI itself */
        if (0 == strncmp(pmix_ptl_base.uri, "file:", 5)) {
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "ptl:tool:tool getting connection info from %s", pmix_ptl_base.uri);
            rc = tryfile(peer, &nspace, &rank, &suri, optional, &pmix_ptl_base.uri[5]);
            if (PMIX_SUCCESS != rc) {
                goto cleanup;
            }
            goto complete;
        } else {
            st = strdup(pmix_ptl_base.uri);
            /* we need to extract the nspace/rank of the server from the string */
            p = strchr(st, ';');
            if (NULL == p) {
                free(st);
                rc = PMIX_ERR_BAD_PARAM;
                goto cleanup;
            }
            *p = '\0';
            p++;
            suri = strdup(p); // save the uri portion
            /* the '.' in the first part of the original string separates
             * nspace from rank */
            p = strchr(st, '.');
            if (NULL == p) {
                free(st);
                rc = PMIX_ERR_BAD_PARAM;
                goto cleanup;
            }
            *p = '\0';
            p++;
            nspace = strdup(st);
            rank = strtoull(p, NULL, 10);
            /* now update the URI */
            free(st);
        }
        goto complete;
    }

    /* if they gave us a rendezvous file, use it */
    if (NULL != rendfile) {
        rc = tryfile(peer, &nspace, &rank, &suri, optional, rendfile);
        if (PMIX_SUCCESS != rc && !optional) {
            /* since they gave us a specific rendfile and we couldn't
             * connect to it, return an error */
            goto cleanup;
        }
        goto complete;
    }

    if (NULL != order) {
        // cycle thru the requested order
        for (n=0; NULL != order[n]; n++) {
            if (0 == strcmp(order[n], PMIX_CONNECT_TO_SYSTEM) ||
                0 == strcmp(order[n], PMIX_CONNECT_SYSTEM_FIRST)) {
                if (0 > asprintf(&filename, "%s/pmix.sys.%s", pmix_ptl_base.system_tmpdir,
                                 pmix_globals.hostname)) {
                    rc = PMIX_ERR_NOMEM;
                    goto cleanup;
                }
                pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                    "ptl:tool:tool looking for system server at %s", filename);
                rc = tryfile(peer, &nspace, &rank, &suri, optional, filename);
                free(filename);
                if (PMIX_SUCCESS == rc) {
                    PMIX_SET_PEER_TYPE(peer, PMIX_PROC_SERVER);
                    goto complete;
                }
                if (0 == strcmp(order[n], PMIX_CONNECT_TO_SYSTEM)) {
                    if (!optional) {
                        // not optional, so report error
                        goto cleanup;
                    }
                }

            } else if (0 == strcmp(order[n], PMIX_CONNECT_TO_SCHEDULER)) {
                if (0 > asprintf(&filename, "%s/pmix.sched.%s",
                                 pmix_ptl_base.system_tmpdir,
                                 pmix_globals.hostname)) {
                    rc = PMIX_ERR_NOMEM;
                    goto cleanup;
                }
                pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                    "ptl:tool:tool looking for scheduler at %s", filename);
                rc = tryfile(peer, &nspace, &rank, &suri, optional, filename);
                free(filename);
                if (PMIX_SUCCESS == rc) {
                    PMIX_SET_PEER_TYPE(peer, PMIX_PROC_SCHEDULER);
                    goto complete;
                }
                if (!optional) {
                    // not optional, so report error
                    goto cleanup;
                }

            } else if (0 == strcmp(order[n], PMIX_CONNECT_TO_SYS_CONTROLLER)) {
                if (0 > asprintf(&filename, "%s/pmix.sysctrlr.%s",
                                 pmix_ptl_base.system_tmpdir,
                                 pmix_globals.hostname)) {
                    rc = PMIX_ERR_NOMEM;
                    goto cleanup;
                }
                pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                    "ptl:tool:tool looking for system controller at %s", filename);
                rc = tryfile(peer, &nspace, &rank, &suri, optional, filename);
                free(filename);
                if (PMIX_SUCCESS == rc) {
                    PMIX_SET_PEER_TYPE(peer, PMIX_PROC_SYS_CTRLR);
                    goto complete;
                }
                if (!optional) {
                    // not optional, so report error
                    goto cleanup;
                }
            }
        }
    }

    /* if they gave us a pid, then look for it */
    if (0 != pid) {
        if (0 > asprintf(&filename, "pmix.%s.tool.%d", pmix_globals.hostname, pid)) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "ptl:tool:tool searching for given session server %s", filename);
        rc = trysearch(peer, &nspace, &rank, &suri, filename, iptr, niptr, optional);
        free(filename);
        if (PMIX_SUCCESS != rc) {
            /* since they gave us a specific pid and we couldn't
             * connect to it, return an error */
            goto cleanup;
        }
        PMIX_SET_PEER_TYPE(peer, PMIX_PROC_SERVER);
        goto complete;
    }

    /* if they gave us an nspace, then look for it */
    if (NULL != server_nspace) {
        if (0 > asprintf(&filename, "pmix.%s.tool.%s", pmix_globals.hostname, server_nspace)) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "ptl:tool:tool searching for given nspace server %s", filename);
        rc = trysearch(peer, &nspace, &rank, &suri, filename, iptr, niptr, optional);
        free(filename);
        if (PMIX_SUCCESS != rc) {
            /* since they gave us a specific nspace and we couldn't
             * connect to it, return an error */
            goto cleanup;
        }
        PMIX_SET_PEER_TYPE(peer, PMIX_PROC_SERVER);
        goto complete;
    }

    /* see if we are a client of some server */
    rc = pmix_ptl_base_check_server_uris(peer, &evar);
    if (PMIX_SUCCESS == rc) {
        PMIX_SET_PEER_TYPE(pmix_globals.mypeer, PMIX_PROC_CLIENT_TOOL);
        rc = pmix_ptl_base_parse_uri(evar, &nspace, &rank, &suri);
        if (PMIX_SUCCESS != rc) {
            goto cleanup;
        }
        goto complete;
    } else if (!PMIX_PEER_IS_SERVER(pmix_globals.mypeer) ||
               PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        /* we aren't a client, so we will search to see what session-level
         * tools are available to this user. We will take the first connection
         * that succeeds - this is based on the likelihood that there is only
         * one session per user on a node */

        if (0 > asprintf(&filename, "pmix.%s.tool", pmix_globals.hostname)) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "ptl:tool:tool searching for session server %s", filename);
        rc = trysearch(peer, &nspace, &rank, &suri, filename, iptr, niptr, optional);
        free(filename);
        if (PMIX_SUCCESS == rc) {
            PMIX_SET_PEER_TYPE(peer, PMIX_PROC_SERVER);
            goto complete;
        }
        if (!optional) {
            goto cleanup;
        }
    }
    rc = PMIX_ERR_UNREACH;
    goto cleanup;

complete:
    rc = pmix_ptl_base_make_connection(peer, suri, iptr, niptr);
    if (PMIX_SUCCESS != rc) {
        goto cleanup;
    }

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "tool_peer_try_connect: Connection across to server succeeded");

    pmix_ptl_base_complete_connection(peer, nspace, rank, suri);

cleanup:
    if (NULL != nspace) {
        free(nspace);
    }
    if (NULL != iptr) {
        PMIX_INFO_FREE(iptr, niptr);
    }
    if (NULL != rendfile) {
        free(rendfile);
    }
    if (NULL != suri) {
        free(suri);
    }
    if (NULL != server_nspace) {
        free(server_nspace);
    }
    return rc;
}
