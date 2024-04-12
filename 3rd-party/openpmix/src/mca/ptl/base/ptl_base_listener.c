/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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

#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_stdint.h"

#include "include/pmix_server.h"
#include "src/include/pmix_globals.h"

#ifdef HAVE_STRING_H
#    include <string.h>
#endif
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#    include <sys/un.h>
#endif
#ifdef HAVE_SYS_UIO_H
#    include <sys/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#include <ctype.h>
#include <sys/stat.h>
#include <event.h>
#include <pthread.h>

#include "src/class/pmix_list.h"
#include "src/include/pmix_socket_errno.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_fd.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/ptl/base/base.h"

// local connection handler
static void connection_event_handler(int incoming_sd, short flags, void *cbdata);

// local value for connection support
static bool setup_complete = false;

/*
 * start listening event
 */
pmix_status_t pmix_ptl_base_start_listening(pmix_info_t info[], size_t ninfo)
{
    pmix_status_t rc;

    /* setup the listener */
    if (!setup_complete) {
        rc = pmix_ptl.setup_listener(info, ninfo);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
    }
    setup_complete = true;

    pmix_event_set(pmix_globals.evbase, &pmix_ptl_base.listener.ev,
               pmix_ptl_base.listener.socket,
               PMIX_EV_READ|PMIX_EV_PERSIST,
               connection_event_handler, 0);
    pmix_ptl_base.listener.active = true;
    pmix_event_add(&pmix_ptl_base.listener.ev, 0);

    return PMIX_SUCCESS;
}

void pmix_ptl_base_stop_listening(void)
{
    pmix_listener_t *lt = &pmix_ptl_base.listener;

    pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                        "listen_thread: shutdown");

    if (!lt->active) {
        /* nothing we need do */
        return;
    }

    /* mark it as inactive */
    lt->active = false;
    pmix_event_del(&lt->ev);
    /* close the socket to remove the connection points */
    CLOSE_THE_SOCKET(lt->socket);
    lt->socket = -1;
}

/*
 * Handler for accepting connections from the event library
 */
static void connection_event_handler(int incoming_sd, short flags, void *cbdata)
{
    struct sockaddr addr;
    pmix_socklen_t addrlen = sizeof(struct sockaddr);
    int sd;
    pmix_pending_connection_t *pending_connection;
    pmix_listener_t *lt = &pmix_ptl_base.listener;
    PMIX_HIDE_UNUSED_PARAMS(flags, cbdata);

    sd = accept(incoming_sd, (struct sockaddr *) &addr, &addrlen);
    pmix_output_verbose(5, pmix_ptl_base_framework.framework_output,
                        "connection_event_handler: working connection "
                        "(%d, %d) %s:%d\n",
                        sd, pmix_socket_errno,
                        pmix_net_get_hostname((struct sockaddr *) &addr),
                        pmix_net_get_port((struct sockaddr *) &addr));
    if (sd < 0) {
        /* Non-fatal errors */
        if (EINTR == pmix_socket_errno ||
            EAGAIN == pmix_socket_errno ||
            EWOULDBLOCK == pmix_socket_errno) {
            return;
        }

        /* If we run out of file descriptors, log an extra warning (so
           that the user can know to fix this problem) and abandon all
           hope. */
        else if (EMFILE == pmix_socket_errno) {
            CLOSE_THE_SOCKET(incoming_sd);
            PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
            pmix_show_help("help-ptl-base.txt", "accept failed", true,
                           pmix_globals.hostname,
                           pmix_socket_errno, strerror(pmix_socket_errno),
                           "Out of file descriptors");
            return;
        }

        /* For all other cases, close the socket, print a warning but
           try to continue */
        else {
            CLOSE_THE_SOCKET(incoming_sd);
            pmix_show_help("help-ptl-base.txt", "accept failed", true,
                           pmix_globals.hostname,
                           pmix_socket_errno, strerror(pmix_socket_errno),
                           "Unknown cause; job will try to continue");
            return;
        }
    }

    /* this descriptor is ready to be read, which means a connection
     * request has been received - so harvest it. All we want to do
     * here is accept the connection and push the info onto the event
     * library for subsequent processing - we don't want to actually
     * process the connection here as it takes too long, and so the
     * OS might start rejecting connections due to timeout.
     */
    pending_connection = PMIX_NEW(pmix_pending_connection_t);
    pending_connection->protocol = lt->protocol;
    pmix_event_assign(&pending_connection->ev, pmix_globals.evbase,
                      -1, EV_WRITE,
                      lt->cbfunc, pending_connection);
    pending_connection->sd = sd;

    pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                        "connection_event_handler: new connection: (%d, %d)", pending_connection->sd,
                        pmix_socket_errno);
    /* post the object */
    PMIX_POST_OBJECT(pending_connection);
    /* activate the event */
    pmix_event_active(&pending_connection->ev, EV_WRITE, 1);
}


pmix_status_t pmix_base_write_rndz_file(char *filename, char *uri, bool *created)
{
    FILE *fp;
    char *dirname;
    time_t mytime;

    dirname = pmix_dirname(filename);
    if (NULL != dirname) {
        if (0 != pmix_os_dirpath_create(dirname, 0755)) {
            pmix_output(0, "System tmpdir %s could not be created\n", dirname);
            PMIX_ERROR_LOG(PMIX_ERR_FILE_OPEN_FAILURE);
            free(dirname);
            return PMIX_ERR_FILE_OPEN_FAILURE;
        }
        *created = true;
        free(dirname);
    }

    fp = fopen(filename, "w");
    if (NULL == fp) {
        pmix_output(0, "Impossible to open the file %s in write mode\n", filename);
        PMIX_ERROR_LOG(PMIX_ERR_FILE_OPEN_FAILURE);
        return PMIX_ERR_FILE_OPEN_FAILURE;
    }

    /* output the URI */
    fprintf(fp, "%s\n", uri);
    /* add the version */
    fprintf(fp, "%s\n", PMIX_VERSION);
    /* output our pid */
    fprintf(fp, "%lu\n", (unsigned long) getpid());
    /* output our effective uid and gid */
    fprintf(fp, "%lu:%lu\n", (unsigned long) geteuid(), (unsigned long) getegid());
    /* output the time */
    mytime = time(NULL);
    fprintf(fp, "%s\n", ctime(&mytime));
    fclose(fp);
    /* set the file mode */
    if (0 != chmod(filename, S_IRUSR | S_IWUSR | S_IRGRP)) {
        PMIX_ERROR_LOG(PMIX_ERR_FILE_OPEN_FAILURE);
        return PMIX_ERR_FILE_OPEN_FAILURE;
    }
    return PMIX_SUCCESS;
}

/* discover the available
 * interfaces, filter them thru any given directives, and select
 * the one we will listen on for connection requests. This will
 * be a loopback device by default, unless we are asked to support
 * tool connections - in that case, we will take a non-loopback
 * device by default, if one is available after filtering directives
 *
 * If we are a tool and were give a rendezvous file, then we first
 * check to see if it already exists. If it does, then this is the
 * connection info we are to use. If it doesn't, then this is the
 * name of the file we are to use to store our listener info.
 *
 * If we are a server and are given a rendezvous file, then that is
 * is the name of the file we are to use to store our listener info.
 */
pmix_status_t pmix_ptl_base_setup_listener(pmix_info_t info[], size_t ninfo)
{
    int flags = 0;
    pmix_listener_t *lt;
    int i, rc = 0, saveindex = -1, savelpbk = -1;
    char **interfaces = NULL;
    bool including = false;
    char name[32];
    struct sockaddr_storage my_ss;
    int kindex;
    pmix_socklen_t addrlen;
    char *prefix;
    char myconnhost[PMIX_MAXHOSTNAMELEN] = {0};
    int myport;
    pmix_kval_t *urikv;
    pid_t mypid;
    int outpipe;
    char *leftover;
    size_t n;
    FILE *fptst;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "ptl:tool setup_listener");

    for (n = 0; n < ninfo; n++) {
        if (0 == strcmp(info[n].key, PMIX_SERVER_SESSION_SUPPORT)) {
            pmix_ptl_base.session_tool = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_SYSTEM_SUPPORT)) {
            pmix_ptl_base.system_tool = PMIX_INFO_TRUE(&info[n]);
        } else if (0 == strcmp(info[n].key, PMIX_SERVER_TOOL_SUPPORT)) {
            pmix_ptl_base.tool_support = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_REMOTE_CONNECTIONS)) {
            pmix_ptl_base.remote_connections = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_IF_INCLUDE)) {
            pmix_ptl_base.if_include = strdup(info[n].value.data.string);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_IF_EXCLUDE)) {
            pmix_ptl_base.if_exclude = strdup(info[n].value.data.string);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_IPV4_PORT)) {
            pmix_ptl_base.ipv4_port = info[n].value.data.integer;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_IPV6_PORT)) {
            pmix_ptl_base.ipv6_port = info[n].value.data.integer;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_DISABLE_IPV4)) {
            pmix_ptl_base.disable_ipv4_family = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_DISABLE_IPV6)) {
            pmix_ptl_base.disable_ipv6_family = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TCP_REPORT_URI)) {
            if (NULL != pmix_ptl_base.report_uri) {
                free(pmix_ptl_base.report_uri);
            }
            pmix_ptl_base.report_uri = strdup(info[n].value.data.string);
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
        }
    }

    if (NULL != pmix_ptl_base.if_include && NULL != pmix_ptl_base.if_exclude) {
        pmix_show_help("help-ptl-base.txt", "include-exclude", true, pmix_ptl_base.if_include,
                       pmix_ptl_base.if_exclude);
        return PMIX_ERR_SILENT;
    }

    lt = &pmix_ptl_base.listener;

    /* if interface include was given, construct a list
     * of those interfaces which match the specifications - remember,
     * the includes could be given as named interfaces, IP addrs, or
     * subnet+mask
     */
    if (NULL != pmix_ptl_base.if_include) {
        interfaces = pmix_ptl_base_split_and_resolve(pmix_ptl_base.if_include, "include");
        including = true;
    } else if (NULL != pmix_ptl_base.if_exclude) {
        interfaces = pmix_ptl_base_split_and_resolve(pmix_ptl_base.if_exclude, "exclude");
        including = false;
    }

    /* look at all available interfaces and pick one - we default to a
     * loopback interface if available, but otherwise pick the first
     * available interface since we are only talking locally */
    for (i = pmix_ifbegin(); i >= 0; i = pmix_ifnext(i)) {
        if (PMIX_SUCCESS != pmix_ifindextoaddr(i, (struct sockaddr *) &my_ss, sizeof(my_ss))) {
            pmix_output(0, "ptl_tool: problems getting address for index %i (kernel index %i)\n", i,
                        pmix_ifindextokindex(i));
            continue;
        }
        /* ignore non-ip4/6 interfaces */
        if (AF_INET != my_ss.ss_family && AF_INET6 != my_ss.ss_family) {
            continue;
        }
        /* get the name for diagnostic purposes */
        pmix_ifindextoname(i, name, sizeof(name));

        /* ignore any virtual interfaces */
        if (0 == strncmp(name, "vir", 3)) {
            continue;
        }
        /* ignore any interfaces in a disabled family */
        if (AF_INET == my_ss.ss_family) {
            if (pmix_ptl_base.disable_ipv4_family) {
                continue;
            }
        } else if (AF_INET6 == my_ss.ss_family) {
            if (pmix_ptl_base.disable_ipv6_family) {
                continue;
            }
        } else {
            /* ignore any other type */
            continue;
        }

        /* get the kernel index */
        kindex = pmix_ifindextokindex(i);
        if (kindex <= 0) {
            continue;
        }
        pmix_output_verbose(10, pmix_ptl_base_framework.framework_output,
                            "WORKING INTERFACE %d KERNEL INDEX %d FAMILY: %s", i, kindex,
                            (AF_INET == my_ss.ss_family) ? "V4" : "V6");
        /* handle include/exclude directives */
        if (NULL != interfaces) {
            /* check for match */
            rc = pmix_ifmatches(kindex, interfaces);
            /* if one of the network specifications isn't parseable, then
             * error out as we can't do what was requested
             */
            if (PMIX_ERR_FABRIC_NOT_PARSEABLE == rc) {
                pmix_show_help("help-ptl-base.txt", "not-parseable", true);
                PMIx_Argv_free(interfaces);
                return PMIX_ERR_BAD_PARAM;
            }
            /* if we are including, then ignore this if not present */
            if (including) {
                if (PMIX_SUCCESS != rc) {
                    pmix_output_verbose(
                        10, pmix_ptl_base_framework.framework_output,
                        "ptl:tool:init rejecting interface %s (not in include list)", name);
                    continue;
                }
            } else {
                /* we are excluding, so ignore if present */
                if (PMIX_SUCCESS == rc) {
                    pmix_output_verbose(10, pmix_ptl_base_framework.framework_output,
                                        "ptl:tool:init rejecting interface %s (in exclude list)",
                                        name);
                    continue;
                }
            }
        }

        /* if this is the loopback device and they didn't enable
         * remote connections, then we are done */
        if (pmix_ifisloopback(i)) {
            pmix_output_verbose(5, pmix_ptl_base_framework.framework_output,
                                "ptl:tool:init loopback interface %s found", name);
            savelpbk = i;
            if (!pmix_ptl_base.remote_connections) {
                saveindex = savelpbk;
                break;
            }
        } else {
            /* if this is the first one we found, then hang on to it - we
             * will use it if a loopback device is not found */
            if (saveindex < 0) {
                saveindex = i;
            }
        }
    }
    /* cleanup */
    if (NULL != interfaces) {
        PMIx_Argv_free(interfaces);
    }

    /* if we didn't find anything, that could be a problem */
    if (saveindex < 0) {
        /* if ONLY a loopback is available, then even if they
         * enabled remote connections, it's the best we can do */
        if (savelpbk < 0) {
            /* if NOTHING is available, then neither are we */
            return PMIX_ERR_NOT_AVAILABLE;
        } else {
            saveindex = savelpbk;
        }
    }

    /* save the connection */
    if (PMIX_SUCCESS
        != pmix_ifindextoaddr(saveindex, (struct sockaddr *) pmix_ptl_base.connection,
                              sizeof(struct sockaddr))) {
        pmix_output(0, "ptl:base: problems getting address for kernel index %i\n",
                    pmix_ifindextokindex(saveindex));
        return PMIX_ERR_NOT_AVAILABLE;
    }

    /* set the port */
    if (AF_INET == pmix_ptl_base.connection->ss_family) {
        ((struct sockaddr_in *) pmix_ptl_base.connection)->sin_port = htons(pmix_ptl_base.ipv4_port);
        addrlen = sizeof(struct sockaddr_in);
        if (0 != pmix_ptl_base.ipv4_port) {
            flags = 1;
        }
    } else if (AF_INET6 == pmix_ptl_base.connection->ss_family) {
        ((struct sockaddr_in6 *) pmix_ptl_base.connection)->sin6_port = htons(pmix_ptl_base.ipv6_port);
        addrlen = sizeof(struct sockaddr_in6);
        if (0 != pmix_ptl_base.ipv6_port) {
            flags = 1;
        }
    } else {
        /* unrecognized family type - shouldn't be possible as we only
         * included IPv4 and IPv6 interfaces, but this is needed to
         * silence warnings */
        return PMIX_ERR_NOT_SUPPORTED;
    }

    lt->varname = strdup("PMIX_SERVER_URI41:PMIX_SERVER_URI4:PMIX_SERVER_URI3:"
                         "PMIX_SERVER_URI2:PMIX_SERVER_URI21");
    lt->protocol = PMIX_PROTOCOL_V2;
    lt->cbfunc = pmix_ptl_base_connection_handler;

    /* create a listen socket for incoming connection attempts */
    lt->socket = socket(pmix_ptl_base.connection->ss_family, SOCK_STREAM, 0);
    if (lt->socket < 0) {
        printf("%s:%d socket() failed\n", __FILE__, __LINE__);
        goto sockerror;
    }

    /* set reusing ports flag */
    if (setsockopt(lt->socket, SOL_SOCKET, SO_REUSEADDR, (const char *) &flags, sizeof(flags))
        < 0) {
        pmix_output(0,
                    "ptl:base:create_listen: unable to set the "
                    "SO_REUSEADDR option (%s:%d)\n",
                    strerror(pmix_socket_errno), pmix_socket_errno);
        goto sockerror;
    }

    /* Set the socket to close-on-exec so that no children inherit
     * this FD */
    if (pmix_fd_set_cloexec(lt->socket) != PMIX_SUCCESS) {
        goto sockerror;
    }

    if (bind(lt->socket, (struct sockaddr *) pmix_ptl_base.connection, addrlen) < 0) {
        printf("[%u] %s:%d bind() failed for socket %d storage size %u: %s\n", (unsigned) getpid(),
               __FILE__, __LINE__, lt->socket, (unsigned) addrlen, strerror(errno));
        goto sockerror;
    }

    /* resolve assigned port */
    if (getsockname(lt->socket, (struct sockaddr *) pmix_ptl_base.connection, &addrlen) < 0) {
        pmix_output(0, "ptl:tool:create_listen: getsockname(): %s (%d)",
                    strerror(pmix_socket_errno), pmix_socket_errno);
        goto sockerror;
    }

    /* setup listen backlog to maximum allowed by kernel */
    if (listen(lt->socket, SOMAXCONN) < 0) {
        printf("%s:%d listen() failed\n", __FILE__, __LINE__);
        goto sockerror;
    }

    /* set socket up to be non-blocking, otherwise accept could block */
    if ((flags = fcntl(lt->socket, F_GETFL, 0)) < 0) {
        printf("%s:%d fcntl(F_GETFL) failed\n", __FILE__, __LINE__);
        goto sockerror;
    }
    flags |= O_NONBLOCK;
    if (fcntl(lt->socket, F_SETFL, flags) < 0) {
        printf("%s:%d fcntl(F_SETFL) failed\n", __FILE__, __LINE__);
        goto sockerror;
    }

    if (AF_INET == pmix_ptl_base.connection->ss_family) {
        prefix = "tcp4://";
        myport = ntohs(((struct sockaddr_in *) pmix_ptl_base.connection)->sin_port);
        inet_ntop(AF_INET, &((struct sockaddr_in *) pmix_ptl_base.connection)->sin_addr,
                  myconnhost, PMIX_MAXHOSTNAMELEN - 1);
    } else if (AF_INET6 == pmix_ptl_base.connection->ss_family) {
        prefix = "tcp6://";
        myport = ntohs(((struct sockaddr_in6 *) pmix_ptl_base.connection)->sin6_port);
        inet_ntop(AF_INET6, &((struct sockaddr_in6 *) pmix_ptl_base.connection)->sin6_addr,
                  myconnhost, PMIX_MAXHOSTNAMELEN - 1);
    } else {
        goto sockerror;
    }

    rc = asprintf(&lt->uri, "%s.%u;%s%s:%d", pmix_globals.myid.nspace, pmix_globals.myid.rank,
                  prefix, myconnhost, myport);
    if (0 > rc || NULL == lt->uri) {
        goto sockerror;
    }
    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "ptl:base URI %s", lt->uri);

    /* save the URI internally so we can report it */
    urikv = PMIX_NEW(pmix_kval_t);
    urikv->key = strdup(PMIX_MYSERVER_URI);
    PMIX_VALUE_CREATE(urikv->value, 1);
    PMIX_VALUE_LOAD(urikv->value, lt->uri, PMIX_STRING);
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, urikv);
    PMIX_RELEASE(urikv); // maintain accounting

    /* save a legacy URI internally so we can report it
     * to older tools */
    urikv = PMIX_NEW(pmix_kval_t);
    urikv->key = strdup(PMIX_SERVER_URI);
    PMIX_VALUE_CREATE(urikv->value, 1);
    PMIX_VALUE_LOAD(urikv->value, lt->uri, PMIX_STRING);
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, urikv);
    PMIX_RELEASE(urikv); // maintain accounting

    if (NULL != pmix_ptl_base.report_uri) {
        /* if the string is a "-", then output to stdout */
        if (0 == strcmp(pmix_ptl_base.report_uri, "-")) {
            fprintf(stdout, "%s\n", lt->uri);
        } else if (0 == strcmp(pmix_ptl_base.report_uri, "+")) {
            /* output to stderr */
            fprintf(stderr, "%s\n", lt->uri);
        } else {
            /* see if it is an integer pipe */
            leftover = NULL;
            outpipe = strtol(pmix_ptl_base.report_uri, &leftover, 10);
            if (NULL == leftover || 0 == strlen(leftover)) {
                /* stitch together the var names and URI */
                pmix_asprintf(&leftover, "%s;%s", lt->varname, lt->uri);
                /* output to the pipe */
                rc = pmix_fd_write(outpipe, strlen(leftover) + 1, leftover);
                free(leftover);
                close(outpipe);
            } else {
                /* must be a file */
                FILE *fp;
                fp = fopen(pmix_ptl_base.report_uri, "w");
                if (NULL == fp) {
                    pmix_output(0, "Impossible to open the file %s in write mode\n",
                                pmix_ptl_base.report_uri);
                    PMIX_ERROR_LOG(PMIX_ERR_FILE_OPEN_FAILURE);
                    goto sockerror;
                }
                /* output my nspace and rank plus the URI */
                fprintf(fp, "%s\n", lt->uri);
                /* add a flag that indicates we accept v2.1 protocols */
                fprintf(fp, "v%s\n", PMIX_VERSION);
                fclose(fp);
                pmix_ptl_base.created_urifile = true;
            }
        }
    }

    /* if we were given a rendezvous file, then drop it */
    if (NULL != pmix_ptl_base.rendezvous_filename) {
        /* if we are a tool and the file already exists, then we
         * just use it as providing the rendezvous info for our
         * server */
        if (PMIX_PEER_IS_TOOL(pmix_globals.mypeer)) {
            fptst = fopen(pmix_ptl_base.rendezvous_filename, "r");
            if (NULL != fptst) {
                fclose(fptst);
                goto nextstep;
            }
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "WRITING RENDEZVOUS FILE %s", pmix_ptl_base.rendezvous_filename);
        rc = pmix_base_write_rndz_file(pmix_ptl_base.rendezvous_filename, lt->uri,
                                       &pmix_ptl_base.created_rendezvous_file);
        if (PMIX_SUCCESS != rc) {
            goto sockerror;
        }
    }

nextstep:
    /* if we are the scheduler, then drop an appropriately named
     * contact file so the system's resource manager can find us */
    if (PMIX_PEER_IS_SCHEDULER(pmix_globals.mypeer)) {
        if (0 > asprintf(&pmix_ptl_base.scheduler_filename, "%s/pmix.sched.%s",
                         pmix_ptl_base.system_tmpdir, pmix_globals.hostname)) {
            goto sockerror;
        }
        rc = pmix_base_write_rndz_file(pmix_ptl_base.scheduler_filename, lt->uri,
                                       &pmix_ptl_base.created_system_tmpdir);
        if (PMIX_SUCCESS != rc) {
            goto sockerror;
        }
        pmix_ptl_base.created_scheduler_filename = true;
    }

    /* if we are the system controller, then drop an appropriately named
     * contact file so others can find us */
    if (PMIX_PEER_IS_SYS_CTRLR(pmix_globals.mypeer)) {
        if (0 > asprintf(&pmix_ptl_base.sysctrlr_filename, "%s/pmix.sysctrlr.%s",
                         pmix_ptl_base.system_tmpdir, pmix_globals.hostname)) {
            goto sockerror;
        }
        rc = pmix_base_write_rndz_file(pmix_ptl_base.sysctrlr_filename, lt->uri,
                                       &pmix_ptl_base.created_system_tmpdir);
        if (PMIX_SUCCESS != rc) {
            goto sockerror;
        }
        pmix_ptl_base.created_sysctrlr_filename = true;
    }

    /* if we are going to support tools, then drop contact file(s) */
    if (pmix_ptl_base.system_tool) {
        if (0 > asprintf(&pmix_ptl_base.system_filename, "%s/pmix.sys.%s",
                         pmix_ptl_base.system_tmpdir, pmix_globals.hostname)) {
            goto sockerror;
        }
        rc = pmix_base_write_rndz_file(pmix_ptl_base.system_filename, lt->uri,
                                       &pmix_ptl_base.created_system_tmpdir);
        if (PMIX_SUCCESS != rc) {
            goto sockerror;
        }
        pmix_ptl_base.created_system_filename = true;
    }

    if (pmix_ptl_base.session_tool) {
        /* first output to a std file */
        if (0 > asprintf(&pmix_ptl_base.session_filename, "%s/pmix.%s.tool",
                         pmix_ptl_base.session_tmpdir, pmix_globals.hostname)) {
            goto sockerror;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "WRITING SESSION TOOL FILE %s", pmix_ptl_base.session_filename);
        rc = pmix_base_write_rndz_file(pmix_ptl_base.session_filename, lt->uri,
                                       &pmix_ptl_base.created_session_tmpdir);
        if (PMIX_SUCCESS != rc) {
            goto sockerror;
        }
        pmix_ptl_base.created_session_filename = true;
    }

    if (pmix_ptl_base.tool_support) {
        /* now output to a file based on pid */
        mypid = getpid();
        if (0 > asprintf(&pmix_ptl_base.pid_filename, "%s/pmix.%s.tool.%d",
                         pmix_ptl_base.session_tmpdir, pmix_globals.hostname, mypid)) {
            goto sockerror;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output, "WRITING PID TOOL FILE %s",
                            pmix_ptl_base.pid_filename);
        rc = pmix_base_write_rndz_file(pmix_ptl_base.pid_filename, lt->uri,
                                       &pmix_ptl_base.created_session_tmpdir);
        if (PMIX_SUCCESS != rc) {
            goto sockerror;
        }
        pmix_ptl_base.created_pid_filename = true;

        /* now output it into a file based on my nspace */
        if (0 > asprintf(&pmix_ptl_base.nspace_filename, "%s/pmix.%s.tool.%s",
                         pmix_ptl_base.session_tmpdir, pmix_globals.hostname,
                         pmix_globals.myid.nspace)) {
            goto sockerror;
        }
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "WRITING NSPACE TOOL FILE %s", pmix_ptl_base.nspace_filename);
        rc = pmix_base_write_rndz_file(pmix_ptl_base.nspace_filename, lt->uri,
                                       &pmix_ptl_base.created_session_tmpdir);
        if (PMIX_SUCCESS != rc) {
            goto sockerror;
        }
        pmix_ptl_base.created_nspace_filename = true;
    }

    return PMIX_SUCCESS;

sockerror:
    CLOSE_THE_SOCKET(lt->socket);
    return rc;
}
