/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennptlee and The University
 *                         of Tennptlee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2017 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include <src/include/pmix_config.h>
#include <pmix_common.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <fcntl.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#include <ctype.h>

#include "src/include/pmix_socket_errno.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/fd.h"
#include "src/util/net.h"
#include "src/util/os_path.h"
#include "src/util/parse_options.h"
#include "src/util/pif.h"
#include "src/util/show_help.h"
#include "src/util/strnlen.h"
#include "src/server/pmix_server_ops.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/base/base.h"
#include "src/mca/psec/base/base.h"

#include "src/mca/ptl/base/base.h"
#include "src/mca/ptl/tcp/ptl_tcp.h"

static pmix_status_t component_open(void);
static pmix_status_t component_close(void);
static int component_register(void);
static int component_query(pmix_mca_base_module_t **module, int *priority);
static pmix_status_t setup_listener(pmix_info_t info[], size_t ninfo,
                                    bool *need_listener);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
 PMIX_EXPORT pmix_ptl_tcp_component_t mca_ptl_tcp_component = {
    .super = {
        .base = {
            PMIX_PTL_BASE_VERSION_1_0_0,

            /* Component name and version */
            .pmix_mca_component_name = "tcp",
            PMIX_MCA_BASE_MAKE_VERSION(component,
                                       PMIX_MAJOR_VERSION,
                                       PMIX_MINOR_VERSION,
                                       PMIX_RELEASE_VERSION),

            /* Component open and close functions */
            .pmix_mca_open_component = component_open,
            .pmix_mca_close_component = component_close,
            .pmix_mca_register_component_params = component_register,
            .pmix_mca_query_component = component_query
        },
        .priority = 30,
        .uri = NULL,
        .setup_listener = setup_listener
    },
    .tmpdir = NULL,
    .if_include = NULL,
    .if_exclude = NULL,
    .ipv4_port = 0,
    .ipv6_port = 0,
    .disable_ipv4_family = false,
    .disable_ipv6_family = true,
    .filename = NULL
};

static char **split_and_resolve(char **orig_str, char *name);
static void connection_handler(int sd, short args, void *cbdata);
static void cnct_cbfunc(pmix_status_t status,
                        pmix_proc_t *proc, void *cbdata);

static int component_register(void)
{
    pmix_mca_base_component_t *component = &mca_ptl_tcp_component.super.base;

    (void)pmix_mca_base_component_var_register(component, "server_uri",
                                               "URI of a server a tool wishes to connect to",
                                               PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                               PMIX_INFO_LVL_2,
                                               PMIX_MCA_BASE_VAR_SCOPE_LOCAL,
                                               &mca_ptl_tcp_component.super.uri);

    (void)pmix_mca_base_component_var_register(component, "if_include",
                                               "Comma-delimited list of devices and/or CIDR notation of TCP networks (e.g., \"eth0,192.168.0.0/16\").  Mutually exclusive with ptl_tcp_if_exclude.",
                                               PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                               PMIX_INFO_LVL_2,
                                               PMIX_MCA_BASE_VAR_SCOPE_LOCAL,
                                               &mca_ptl_tcp_component.if_include);

    (void)pmix_mca_base_component_var_register(component, "if_exclude",
                                               "Comma-delimited list of devices and/or CIDR notation of TCP networks to NOT use -- all devices not matching these specifications will be used (e.g., \"eth0,192.168.0.0/16\").  If set to a non-default value, it is mutually exclusive with ptl_tcp_if_include.",
                                               PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                               PMIX_INFO_LVL_2,
                                               PMIX_MCA_BASE_VAR_SCOPE_LOCAL,
                                               &mca_ptl_tcp_component.if_exclude);

    /* if_include and if_exclude need to be mutually exclusive */
    if (NULL != mca_ptl_tcp_component.if_include &&
        NULL != mca_ptl_tcp_component.if_exclude) {
        /* Return ERR_NOT_AVAILABLE so that a warning message about
           "open" failing is not printed */
        pmix_show_help("help-ptl-tcp.txt", "include-exclude", true,
                       mca_ptl_tcp_component.if_include,
                       mca_ptl_tcp_component.if_exclude);
        return PMIX_ERR_NOT_AVAILABLE;
    }

    (void)pmix_mca_base_component_var_register(component, "ipv4_port",
                                          "IPv4 port to be used",
                                          PMIX_MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          PMIX_INFO_LVL_4,
                                          PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_ptl_tcp_component.ipv4_port);

    (void)pmix_mca_base_component_var_register(component, "ipv6_port",
                                          "IPv6 port to be used",
                                          PMIX_MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          PMIX_INFO_LVL_4,
                                          PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_ptl_tcp_component.ipv6_port);

    (void)pmix_mca_base_component_var_register(component, "disable_ipv4_family",
                                          "Disable the IPv4 interfaces",
                                          PMIX_MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                          PMIX_INFO_LVL_4,
                                          PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_ptl_tcp_component.disable_ipv4_family);

    (void)pmix_mca_base_component_var_register(component, "disable_ipv6_family",
                                          "Disable the IPv6 interfaces",
                                          PMIX_MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                          PMIX_INFO_LVL_4,
                                          PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_ptl_tcp_component.disable_ipv6_family);

    return PMIX_SUCCESS;
}

static pmix_status_t component_open(void)
{
    char *tdir;

    memset(&mca_ptl_tcp_component.connection, 0, sizeof(mca_ptl_tcp_component.connection));

    /* check for environ-based directives
     * on system tmpdir to use */
    if (NULL == (tdir = getenv("PMIX_SERVER_TMPDIR"))) {
        if (NULL == (tdir = getenv("PMIX_SYSTEM_TMPDIR"))) {
            if (NULL == (tdir = getenv("TMPDIR"))) {
                if (NULL == (tdir = getenv("TEMP"))) {
                    if (NULL == (tdir = getenv("TMP"))) {
                        tdir = "/tmp";
                    }
                }
            }
        }
    }
    if (NULL != tdir) {
        mca_ptl_tcp_component.tmpdir = strdup(tdir);
    }
    return PMIX_SUCCESS;
}


pmix_status_t component_close(void)
{
    if (NULL != mca_ptl_tcp_component.filename) {
        /* remove the file */
        unlink(mca_ptl_tcp_component.filename);
    }
    return PMIX_SUCCESS;
}

static int component_query(pmix_mca_base_module_t **module, int *priority)
{
    *module = (pmix_mca_base_module_t*)&pmix_ptl_tcp_module;
    return PMIX_SUCCESS;
}

/* if we are the server, then we need to discover the available
 * interfaces, filter them thru any given directives, and select
 * the one we will listen on for connection requests. This will
 * be a loopback device by default, unless we are asked to support
 * tool connections - in that case, we will take a non-loopback
 * device by default, if one is available after filtering directives
 *
 * NOTE: we accept MCA parameters, but info keys override them
 */
static pmix_status_t setup_listener(pmix_info_t info[], size_t ninfo,
                                    bool *need_listener)
{
    int flags = 0;
    pmix_listener_t *lt;
    int i, rc, saveindex = -1;
    char **interfaces = NULL;
    bool including = false;
    char name[32];
    struct sockaddr_storage my_ss;
    int kindex;
    size_t n;
    bool remote_connections = false;
    bool tool_support = false;
    pmix_socklen_t addrlen;
    char *prefix, myhost[NI_MAXHOST+1];
    int myport;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "ptl:tcp setup_listener");

    /* if we are not a server, then we shouldn't be doing this */
    if (PMIX_PROC_SERVER != pmix_globals.proc_type) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* scan the info keys and process any override instructions */
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (0 == strcmp(info[n].key, PMIX_TCP_IF_INCLUDE)) {
                if (NULL != mca_ptl_tcp_component.if_include) {
                    free(mca_ptl_tcp_component.if_include);
                }
                mca_ptl_tcp_component.if_include = strdup(info[n].value.data.string);
            } else if (0 == strcmp(info[n].key, PMIX_TCP_IF_EXCLUDE)) {
                if (NULL != mca_ptl_tcp_component.if_exclude) {
                    free(mca_ptl_tcp_component.if_exclude);
                }
                mca_ptl_tcp_component.if_exclude = strdup(info[n].value.data.string);
            } else if (0 == strcmp(info[n].key, PMIX_TCP_IPV4_PORT)) {
                mca_ptl_tcp_component.ipv4_port = info[n].value.data.integer;
            } else if (0 == strcmp(info[n].key, PMIX_TCP_IPV6_PORT)) {
                mca_ptl_tcp_component.ipv6_port = info[n].value.data.integer;
            } else if (0 == strcmp(info[n].key, PMIX_TCP_DISABLE_IPV4)) {
                if (PMIX_UNDEF == info[n].value.type) {
                    mca_ptl_tcp_component.disable_ipv4_family = true;
                } else {
                    mca_ptl_tcp_component.disable_ipv4_family = info[n].value.data.flag;
                }
            } else if (0 == strcmp(info[n].key, PMIX_TCP_DISABLE_IPV6)) {
                if (PMIX_UNDEF == info[n].value.type) {
                    mca_ptl_tcp_component.disable_ipv6_family = true;
                } else {
                    mca_ptl_tcp_component.disable_ipv6_family = info[n].value.data.flag;
                }
            } else if (0 == strcmp(info[n].key, PMIX_SERVER_REMOTE_CONNECTIONS)) {
                if (PMIX_UNDEF == info[n].value.type) {
                    remote_connections = true;
                } else {
                    remote_connections = info[n].value.data.flag;
                }
            } else if (0 == strcmp(info[n].key, PMIX_TCP_URI)) {
                if (NULL != mca_ptl_tcp_component.super.uri) {
                    free(mca_ptl_tcp_component.super.uri);
                }
                mca_ptl_tcp_component.super.uri = strdup(info[n].value.data.string);
            } else if (0 == strcmp(info[n].key, PMIX_SERVER_TMPDIR)) {
                if (NULL != mca_ptl_tcp_component.tmpdir) {
                    free(mca_ptl_tcp_component.tmpdir);
                }
                mca_ptl_tcp_component.tmpdir = strdup(info[n].value.data.string);
            } else if (0 == strcmp(info[n].key, PMIX_SERVER_TOOL_SUPPORT)) {
                if (PMIX_UNDEF == info[n].value.type) {
                    tool_support = true;
                } else {
                    tool_support = info[n].value.data.flag;
                }
            }
        }
    }

    /* if interface include was given, construct a list
     * of those interfaces which match the specifications - remember,
     * the includes could be given as named interfaces, IP addrs, or
     * subnet+mask
     */
    if (NULL != mca_ptl_tcp_component.if_include) {
        interfaces = split_and_resolve(&mca_ptl_tcp_component.if_include,
                                       "include");
        including = true;
    } else if (NULL != mca_ptl_tcp_component.if_exclude) {
        interfaces = split_and_resolve(&mca_ptl_tcp_component.if_exclude,
                                       "exclude");
        including = false;
    }

    /* look at all available interfaces and pick one - we default to a
     * loopback interface if available, but otherwise pick the first
     * available interface since we are only talking locally */
    for (i = pmix_ifbegin(); i >= 0; i = pmix_ifnext(i)) {
        if (PMIX_SUCCESS != pmix_ifindextoaddr(i, (struct sockaddr*)&my_ss, sizeof(my_ss))) {
            pmix_output (0, "ptl_tcp: problems getting address for index %i (kernel index %i)\n",
                         i, pmix_ifindextokindex(i));
            continue;
        }
        /* ignore non-ip4/6 interfaces */
        if (AF_INET != my_ss.ss_family &&
            AF_INET6 != my_ss.ss_family) {
            continue;
        }
        /* get the name for diagnostic purposes */
        pmix_ifindextoname(i, name, sizeof(name));

        /* ignore any virtual interfaces */
        if (0 == strncmp(name, "vir", 3)) {
            continue;
        }
        /* ignore any interfaces in a disabled family */
        if (AF_INET == my_ss.ss_family &&
            mca_ptl_tcp_component.disable_ipv4_family) {
            continue;
        } else if (AF_INET6 == my_ss.ss_family &&
                   mca_ptl_tcp_component.disable_ipv6_family) {
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
            if (PMIX_ERR_NETWORK_NOT_PARSEABLE == rc) {
                pmix_show_help("help-ptl-tcp.txt", "not-parseable", true);
                pmix_argv_free(interfaces);
                return PMIX_ERR_BAD_PARAM;
            }
            /* if we are including, then ignore this if not present */
            if (including) {
                if (PMIX_SUCCESS != rc) {
                    pmix_output_verbose(10, pmix_ptl_base_framework.framework_output,
                                        "ptl:tcp:init rejecting interface %s (not in include list)", name);
                    continue;
                }
            } else {
                /* we are excluding, so ignore if present */
                if (PMIX_SUCCESS == rc) {
                    pmix_output_verbose(10, pmix_ptl_base_framework.framework_output,
                                        "ptl:tcp:init rejecting interface %s (in exclude list)", name);
                    continue;
                }
            }
        }

        /* if this is the loopback device and they didn't enable
         * remote connections, then we are done */
        if (pmix_ifisloopback(i)) {
            if (remote_connections) {
                /* ignore loopback */
                continue;
            } else {
                pmix_output_verbose(5, pmix_ptl_base_framework.framework_output,
                                    "ptl:tcp:init loopback interface %s selected", name);
                saveindex = i;
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
        pmix_argv_free(interfaces);
    }

    /* if we didn't find anything, then we cannot operate */
    if (saveindex < 0) {
        return PMIX_ERR_NOT_AVAILABLE;
    }

    /* save the connection */
    if (PMIX_SUCCESS != pmix_ifindextoaddr(saveindex,
                                           (struct sockaddr*)&mca_ptl_tcp_component.connection,
                                           sizeof(struct sockaddr))) {
        pmix_output (0, "ptl:tcp: problems getting address for kernel index %i\n",
                     pmix_ifindextokindex(saveindex));
        return PMIX_ERR_NOT_AVAILABLE;
    }

    /* set the port */
    if (AF_INET == mca_ptl_tcp_component.connection.ss_family) {
        ((struct sockaddr_in*) &mca_ptl_tcp_component.connection)->sin_port = htons(mca_ptl_tcp_component.ipv4_port);
        if (0 != mca_ptl_tcp_component.ipv4_port) {
            flags = 1;
        }
    } else if (AF_INET6 == mca_ptl_tcp_component.connection.ss_family) {
        ((struct sockaddr_in6*) &mca_ptl_tcp_component.connection)->sin6_port = htons(mca_ptl_tcp_component.ipv6_port);
        if (0 != mca_ptl_tcp_component.ipv6_port) {
            flags = 1;
        }
    }

    lt = PMIX_NEW(pmix_listener_t);
    lt->varname = strdup("PMIX_SERVER_URI2");
    lt->protocol = PMIX_PROTOCOL_V2;
    lt->ptl = (struct pmix_ptl_module_t*)&pmix_ptl_tcp_module;
    lt->cbfunc = connection_handler;

    addrlen = sizeof(struct sockaddr_storage);
    /* create a listen socket for incoming connection attempts */
    lt->socket = socket(mca_ptl_tcp_component.connection.ss_family, SOCK_STREAM, 0);
    if (lt->socket < 0) {
        printf("%s:%d socket() failed\n", __FILE__, __LINE__);
        goto sockerror;
    }
    /* set reusing ports flag */
    if (setsockopt (lt->socket, SOL_SOCKET, SO_REUSEADDR, (const char *)&flags, sizeof(flags)) < 0) {
        pmix_output(0, "ptl:tcp:create_listen: unable to set the "
                    "SO_REUSEADDR option (%s:%d)\n",
                    strerror(pmix_socket_errno), pmix_socket_errno);
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }

    /* Set the socket to close-on-exec so that no children inherit
     * this FD */
    if (pmix_fd_set_cloexec(lt->socket) != PMIX_SUCCESS) {
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }

    if (bind(lt->socket, (struct sockaddr*)&mca_ptl_tcp_component.connection, sizeof(struct sockaddr)) < 0) {
        printf("%s:%d bind() failed: %s\n", __FILE__, __LINE__, strerror(errno));
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }

    /* resolve assigned port */
    if (getsockname(lt->socket, (struct sockaddr*)&mca_ptl_tcp_component.connection, &addrlen) < 0) {
        pmix_output(0, "ptl:tcp:create_listen: getsockname(): %s (%d)",
                    strerror(pmix_socket_errno), pmix_socket_errno);
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }

    /* setup listen backlog to maximum allowed by kernel */
    if (listen(lt->socket, SOMAXCONN) < 0) {
        printf("%s:%d listen() failed\n", __FILE__, __LINE__);
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }

    /* set socket up to be non-blocking, otherwise accept could block */
    if ((flags = fcntl(lt->socket, F_GETFL, 0)) < 0) {
        printf("%s:%d fcntl(F_GETFL) failed\n", __FILE__, __LINE__);
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }
    flags |= O_NONBLOCK;
    if (fcntl(lt->socket, F_SETFL, flags) < 0) {
        printf("%s:%d fcntl(F_SETFL) failed\n", __FILE__, __LINE__);
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }

    if (AF_INET == mca_ptl_tcp_component.connection.ss_family) {
        prefix = "tcp4://";
        myport = ntohs(((struct sockaddr_in*) &mca_ptl_tcp_component.connection)->sin_port);
        inet_ntop(AF_INET, &((struct sockaddr_in*) &mca_ptl_tcp_component.connection)->sin_addr,
                                     myhost, NI_MAXHOST);
    } else if (AF_INET6 == mca_ptl_tcp_component.connection.ss_family) {
        prefix = "tcp6://";
        myport = ntohs(((struct sockaddr_in6*) &mca_ptl_tcp_component.connection)->sin6_port);
        inet_ntop(AF_INET6, &((struct sockaddr_in6*) &mca_ptl_tcp_component.connection)->sin6_addr,
                                     myhost, NI_MAXHOST);
    } else {
        goto sockerror;
    }

    asprintf(&lt->uri, "%s.%d;%s%s:%d", pmix_globals.myid.nspace, pmix_globals.myid.rank, prefix, myhost, myport);
    if (NULL == lt->uri) {
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }
    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "ptl:tcp URI %s", lt->uri);

    /* if we are going to support tools, then drop the contact file */
    if (tool_support) {
        FILE *fp;

        mca_ptl_tcp_component.filename = pmix_os_path(false, mca_ptl_tcp_component.tmpdir, "pmix-contact.txt", NULL);
        if (NULL == mca_ptl_tcp_component.filename) {
            CLOSE_THE_SOCKET(lt->socket);
            goto sockerror;
        }
        fp = fopen(mca_ptl_tcp_component.filename, "w");
        if (NULL == fp) {
            pmix_output( 0, "Impossible to open the file %s in write mode\n", mca_ptl_tcp_component.filename );
            PMIX_ERROR_LOG(PMIX_ERR_FILE_OPEN_FAILURE);
            CLOSE_THE_SOCKET(lt->socket);
            free(mca_ptl_tcp_component.filename);
            mca_ptl_tcp_component.filename = NULL;
            goto sockerror;
        }

        /* output my nspace and rank plus the URI */
        fprintf(fp, "%s.%d:%s\n", pmix_globals.myid.nspace, pmix_globals.myid.rank, lt->uri);
        fclose(fp);
    }

    /* we need listener thread support */
    *need_listener = true;
    pmix_list_append(&pmix_ptl_globals.listeners, &lt->super);

    return PMIX_SUCCESS;

  sockerror:
    PMIX_RELEASE(lt);
    return PMIX_ERROR;
}

/*
 * Go through a list of argv; if there are any subnet specifications
 * (a.b.c.d/e), resolve them to an interface name (Currently only
 * supporting IPv4).  If unresolvable, warn and remove.
 */
static char **split_and_resolve(char **orig_str, char *name)
{
    int i, ret, save, if_index;
    char **argv, *str, *tmp;
    char if_name[IF_NAMESIZE];
    struct sockaddr_storage argv_inaddr, if_inaddr;
    uint32_t argv_prefix;

    /* Sanity check */
    if (NULL == orig_str || NULL == *orig_str) {
        return NULL;
    }

    argv = pmix_argv_split(*orig_str, ',');
    if (NULL == argv) {
        return NULL;
    }
    for (save = i = 0; NULL != argv[i]; ++i) {
        if (isalpha(argv[i][0])) {
            argv[save++] = argv[i];
            continue;
        }

        /* Found a subnet notation.  Convert it to an IP
           address/netmask.  Get the prefix first. */
        argv_prefix = 0;
        tmp = strdup(argv[i]);
        str = strchr(argv[i], '/');
        if (NULL == str) {
            pmix_show_help("help-ptl-tcp.txt", "invalid if_inexclude",
                           true, name, tmp, "Invalid specification (missing \"/\")");
            free(argv[i]);
            free(tmp);
            continue;
        }
        *str = '\0';
        argv_prefix = atoi(str + 1);

        /* Now convert the IPv4 address */
        ((struct sockaddr*) &argv_inaddr)->sa_family = AF_INET;
        ret = inet_pton(AF_INET, argv[i],
                        &((struct sockaddr_in*) &argv_inaddr)->sin_addr);
        free(argv[i]);

        if (1 != ret) {
            pmix_show_help("help-ptl-tcp.txt", "invalid if_inexclude",
                           true, name, tmp,
                           "Invalid specification (inet_pton() failed)");
            free(tmp);
            continue;
        }
        pmix_output_verbose(20, pmix_ptl_base_framework.framework_output,
                            "ptl:tcp: Searching for %s address+prefix: %s / %u",
                            name,
                            pmix_net_get_hostname((struct sockaddr*) &argv_inaddr),
                            argv_prefix);

        /* Go through all interfaces and see if we can find a match */
        for (if_index = pmix_ifbegin(); if_index >= 0;
                           if_index = pmix_ifnext(if_index)) {
            pmix_ifindextoaddr(if_index,
                               (struct sockaddr*) &if_inaddr,
                               sizeof(if_inaddr));
            if (pmix_net_samenetwork((struct sockaddr*) &argv_inaddr,
                                     (struct sockaddr*) &if_inaddr,
                                     argv_prefix)) {
                break;
            }
        }
        /* If we didn't find a match, keep trying */
        if (if_index < 0) {
            pmix_show_help("help-ptl-tcp.txt", "invalid if_inexclude",
                           true, name, tmp,
                           "Did not find interface matching this subnet");
            free(tmp);
            continue;
        }

        /* We found a match; get the name and replace it in the
           argv */
        pmix_ifindextoname(if_index, if_name, sizeof(if_name));
        pmix_output_verbose(20, pmix_ptl_base_framework.framework_output,
                            "ptl:tcp: Found match: %s (%s)",
                            pmix_net_get_hostname((struct sockaddr*) &if_inaddr),
                            if_name);
        argv[save++] = strdup(if_name);
        free(tmp);
    }

    /* The list may have been compressed if there were invalid
       entries, so ensure we end it with a NULL entry */
    argv[save] = NULL;
    free(*orig_str);
    *orig_str = pmix_argv_join(argv, ',');
    return argv;
}

static void connection_handler(int sd, short args, void *cbdata)
{
    pmix_pending_connection_t *pnd = (pmix_pending_connection_t*)cbdata;
    pmix_ptl_hdr_t hdr;
    pmix_peer_t *peer;
    pmix_rank_t rank;
    pmix_status_t rc;
    char *msg, *mg;
    char *sec, *bfrops, *gds;
    pmix_bfrop_buffer_type_t bftype;
    char *nspace;
    uint32_t len, u32;
    size_t cnt, msglen, n;
    uint8_t flag;
    pmix_nspace_t *nptr, *tmp;
    bool found;
    pmix_rank_info_t *info;
    pmix_proc_t proc;
    pmix_info_t ginfo;

    pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                        "ptl:tcp:connection_handler: new connection: %d",
                        pnd->sd);

    /* ensure the socket is in blocking mode */
    pmix_ptl_base_set_blocking(pnd->sd);

    /* ensure all is zero'd */
    memset(&hdr, 0, sizeof(pmix_ptl_hdr_t));

    /* get the header */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_recv_blocking(pnd->sd, (char*)&hdr, sizeof(pmix_ptl_hdr_t)))) {
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }

    /* get the id, authentication and version payload (and possibly
     * security credential) - to guard against potential attacks,
     * we'll set an arbitrary limit per a define */
    if (PMIX_MAX_CRED_SIZE < hdr.nbytes) {
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }
    if (NULL == (msg = (char*)malloc(hdr.nbytes))) {
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }
    if (PMIX_SUCCESS != pmix_ptl_base_recv_blocking(pnd->sd, msg, hdr.nbytes)) {
        /* unable to complete the recv */
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "ptl:tcp:connection_handler unable to complete recv of connect-ack with client ON SOCKET %d",
                            pnd->sd);
        free(msg);
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }

    cnt = hdr.nbytes;
    mg = msg;
    /* extract the name of the sec module they used */
    PMIX_STRNLEN(msglen, mg, cnt);
    if (msglen < cnt) {
        sec = mg;
        mg += strlen(sec) + 1;
        cnt -= strlen(sec) + 1;
    } else {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        free(msg);
        /* send an error reply to the client */
        rc = PMIX_ERR_BAD_PARAM;
        goto error;
    }

    /* extract the name of the bfrops module they used */
    PMIX_STRNLEN(msglen, mg, cnt);
    if (msglen < cnt) {
        bfrops = mg;
        mg += strlen(bfrops) + 1;
        cnt -= strlen(bfrops) + 1;
    } else {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        free(msg);
        /* send an error reply to the client */
        rc = PMIX_ERR_BAD_PARAM;
        goto error;
    }

    /* extract the type of buffer they used */
    if (sizeof(bftype) < cnt) {
        memcpy(&bftype, mg, sizeof(bftype));
        mg += sizeof(bftype);
        cnt -= sizeof(bftype);
    } else {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        free(msg);
        /* send an error reply to the client */
        rc = PMIX_ERR_BAD_PARAM;
        goto error;
    }

    /* extract the name of the gds module they used */
    PMIX_STRNLEN(msglen, mg, cnt);
    if (msglen < cnt) {
        gds = mg;
        mg += strlen(gds) + 1;
        cnt -= strlen(gds) + 1;
    } else {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        free(msg);
        /* send an error reply to the client */
        rc = PMIX_ERR_BAD_PARAM;
        goto error;
    }

    /* extract any credential so we can validate this connection
     * before doing anything else */
    if (sizeof(uint32_t) <= cnt) {
        memcpy(&len, mg, sizeof(uint32_t));
        mg += sizeof(uint32_t);
        cnt -= sizeof(uint32_t);
    } else {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        free(msg);
        /* send an error reply to the client */
        rc = PMIX_ERR_BAD_PARAM;
        goto error;
    }
    /* convert it to host byte order */
    pnd->len = ntohl(len);
    /* if a credential is present, then create space and
     * extract it for processing */
    if (0 < pnd->len) {
        pnd->cred = (char*)malloc(pnd->len);
        if (NULL == pnd->cred) {
            /* probably cannot send an error reply if we are out of memory */
            free(msg);
            CLOSE_THE_SOCKET(pnd->sd);
            PMIX_RELEASE(pnd);
            return;
        }
        memcpy(pnd->cred, mg, pnd->len);
        mg += pnd->len;
        cnt -= pnd->len;
    }

    /* get the request type */
    if (1 <= cnt) {
        memcpy(&flag, mg, 1);
        ++mg;
        --cnt;
    } else {
        free(msg);
        /* send an error reply to the client */
        rc = PMIX_ERR_BAD_PARAM;
        goto error;
    }

    /* see if this is a tool connection request */
    if (1 == flag) {
        /* does the server support tool connections? */
        if (NULL == pmix_host_server.tool_connected) {
            /* send an error reply to the client */
            rc = PMIX_ERR_NOT_SUPPORTED;
            goto error;
        }
        /* extract the uid/gid */
        if (sizeof(uint32_t) <= cnt) {
            memcpy(&u32, mg, sizeof(uint32_t));
            mg += sizeof(uint32_t);
            cnt -= sizeof(uint32_t);
            pnd->uid = ntohl(u32);
        } else {
           free(msg);
           /* send an error reply to the client */
           rc = PMIX_ERR_BAD_PARAM;
           goto error;
        }
        if (sizeof(uint32_t) <= cnt) {
            memcpy(&u32, mg, sizeof(uint32_t));
            mg += sizeof(uint32_t);
            cnt -= sizeof(uint32_t);
            pnd->gid = ntohl(u32);
        } else {
           free(msg);
           /* send an error reply to the client */
           rc = PMIX_ERR_BAD_PARAM;
           goto error;
        }
        /* setup the info array to pass the relevant info
         * to the server - starting with the version, if present */
        n = 0;
        PMIX_STRNLEN(msglen, mg, cnt);
        if (msglen < cnt) {
            pnd->ninfo = 4;
            PMIX_INFO_CREATE(pnd->info, pnd->ninfo);
            (void)strncpy(pnd->info[n].key, PMIX_VERSION_INFO, PMIX_MAX_KEYLEN);
            pnd->info[n].value.type = PMIX_STRING;
            pnd->info[n].value.data.string = strdup(mg);
            ++n;
        } else {
            pnd->ninfo = 3;
            PMIX_INFO_CREATE(pnd->info, pnd->ninfo);
        }
        /* provide the user id */
        (void)strncpy(pnd->info[n].key, PMIX_USERID, PMIX_MAX_KEYLEN);
        pnd->info[n].value.type = PMIX_UINT32;
        pnd->info[n].value.data.uint32 = pnd->uid;
        ++n;
        /* and the group id */
        (void)strncpy(pnd->info[n].key, PMIX_GRPID, PMIX_MAX_KEYLEN);
        pnd->info[n].value.type = PMIX_UINT32;
        pnd->info[n].value.data.uint32 = pnd->gid;
        ++n;
        /* pass along the bfrop, buffer_type, and sec fields so
         * we can assign them once we create a peer object */
        pnd->psec = strdup(sec);
        pnd->bfrops = strdup(bfrops);
        pnd->buffer_type = bftype;
        pnd->gds = strdup(gds);
        /* release the msg */
        free(msg);
        /* request an nspace for this requestor - it will
         * automatically be assigned rank=0 */
        pmix_host_server.tool_connected(pnd->info, pnd->ninfo, cnct_cbfunc, pnd);
        return;
    }

    /* they must be a client, so get their nspace/rank */
    PMIX_STRNLEN(msglen, mg, cnt);
    if (msglen < cnt) {
        nspace = mg;
        mg += strlen(nspace) + 1;
        cnt -= strlen(nspace) + 1;
    } else {
        free(msg);
        /* send an error reply to the client */
        rc = PMIX_ERR_BAD_PARAM;
        goto error;
    }

    if (sizeof(pmix_rank_t) <= cnt) {
        /* have to convert this to host order */
        memcpy(&u32, mg, sizeof(uint32_t));
        rank = ntohl(u32);
        mg += sizeof(uint32_t);
        cnt -= sizeof(uint32_t);
    } else {
        free(msg);
        /* send an error reply to the client */
        rc = PMIX_ERR_BAD_PARAM;
        goto error;
    }

    /* see if we know this nspace */
    nptr = NULL;
    PMIX_LIST_FOREACH(tmp, &pmix_server_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(tmp->nspace, nspace)) {
            nptr = tmp;
            break;
        }
    }
    if (NULL == nptr) {
        /* we don't know this namespace, reject it */
        free(msg);
        /* send an error reply to the client */
        rc = PMIX_ERR_NOT_FOUND;
        goto error;
    }

    /* see if we have this peer in our list */
    info = NULL;
    found = false;
    PMIX_LIST_FOREACH(info, &nptr->ranks, pmix_rank_info_t) {
        if (info->pname.rank == rank) {
            found = true;
            break;
        }
    }
    if (!found) {
        /* rank unknown, reject it */
        free(msg);
        /* send an error reply to the client */
        rc = PMIX_ERR_NOT_FOUND;
        goto error;
    }

    /* a peer can connect on multiple sockets since it can fork/exec
     * a child that also calls PMIX_Init, so add it here if necessary.
     * Create the tracker for this peer */
    peer = PMIX_NEW(pmix_peer_t);
    if (NULL == peer) {
        /* probably cannot send an error reply if we are out of memory */
        free(msg);
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }
    PMIX_RETAIN(nptr);
    peer->nptr = nptr;
    PMIX_RETAIN(info);
    peer->info = info;
    info->proc_cnt++; /* increase number of processes on this rank */
    peer->sd = pnd->sd;
    if (0 > (peer->index = pmix_pointer_array_add(&pmix_server_globals.clients, peer))) {
        free(msg);
        info->proc_cnt--;
        PMIX_RELEASE(info);
        PMIX_RELEASE(peer);
        /* probably cannot send an error reply if we are out of memory */
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }
    info->peerid = peer->index;

    /* set the sec module to match this peer */
    peer->nptr->compat.psec = pmix_psec_base_assign_module(sec);
    if (NULL == peer->nptr->compat.psec) {
        free(msg);
        info->proc_cnt--;
        PMIX_RELEASE(info);
        pmix_pointer_array_set_item(&pmix_server_globals.clients, peer->index, NULL);
        PMIX_RELEASE(peer);
        /* send an error reply to the client */
        goto error;
    }

    /* set the bfrops module to match this peer */
    peer->nptr->compat.bfrops = pmix_bfrops_base_assign_module(bfrops);
    if (NULL == peer->nptr->compat.bfrops) {
        free(msg);
        info->proc_cnt--;
        PMIX_RELEASE(info);
        pmix_pointer_array_set_item(&pmix_server_globals.clients, peer->index, NULL);
        PMIX_RELEASE(peer);
        /* send an error reply to the client */
        goto error;
    }
    free(msg);
    /* and the buffer type to match */
    peer->nptr->compat.type = bftype;

    /* set the gds module to match this peer */
    PMIX_INFO_LOAD(&ginfo, PMIX_GDS_MODULE, gds, PMIX_STRING);
    peer->nptr->compat.gds = pmix_gds_base_assign_module(&ginfo, 1);
    if (NULL == peer->nptr->compat.gds) {
        free(msg);
        info->proc_cnt--;
        PMIX_RELEASE(info);
        pmix_pointer_array_set_item(&pmix_server_globals.clients, peer->index, NULL);
        PMIX_RELEASE(peer);
        /* send an error reply to the client */
        goto error;
    }

    /* the choice of PTL module is obviously us */
    peer->nptr->compat.ptl = &pmix_ptl_tcp_module;

    /* validate the connection */
    PMIX_PSEC_VALIDATE_CONNECTION(rc, peer,
                                  PMIX_PROTOCOL_V2,
                                  pnd->cred, pnd->len);
    if (PMIX_SUCCESS != rc) {
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "validation of client connection failed");
        info->proc_cnt--;
        PMIX_RELEASE(info);
        pmix_pointer_array_set_item(&pmix_server_globals.clients, peer->index, NULL);
        PMIX_RELEASE(peer);
        /* send an error reply to the client */
        goto error;
    }

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "client connection validated");

    /* tell the client all is good */
    u32 = htonl(PMIX_SUCCESS);
    if (PMIX_SUCCESS != pmix_ptl_base_send_blocking(pnd->sd, (char*)&u32, sizeof(uint32_t))) {
        PMIX_ERROR_LOG(rc);
        info->proc_cnt--;
        PMIX_RELEASE(info);
        pmix_pointer_array_set_item(&pmix_server_globals.clients, peer->index, NULL);
        PMIX_RELEASE(peer);
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }
      /* send the client's array index */
    u32 = htonl(peer->index);
      if (PMIX_SUCCESS != (rc = pmix_ptl_base_send_blocking(pnd->sd, (char*)&u32, sizeof(uint32_t)))) {
          PMIX_ERROR_LOG(rc);
          info->proc_cnt--;
          PMIX_RELEASE(info);
          pmix_pointer_array_set_item(&pmix_server_globals.clients, peer->index, NULL);
          PMIX_RELEASE(peer);
          goto error;
      }

      pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                          "connect-ack from client completed");

      /* let the host server know that this client has connected */
      if (NULL != pmix_host_server.client_connected) {
          (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
          proc.rank = peer->info->pname.rank;
          rc = pmix_host_server.client_connected(&proc, peer->info->server_object,
                                                 NULL, NULL);
          if (PMIX_SUCCESS != rc) {
              PMIX_ERROR_LOG(rc);
          }
      }

    pmix_ptl_base_set_nonblocking(pnd->sd);

    /* start the events for this client */
    pmix_event_assign(&peer->recv_event, pmix_globals.evbase, pnd->sd,
                      EV_READ|EV_PERSIST, pmix_ptl_base_recv_handler, peer);
    pmix_event_add(&peer->recv_event, NULL);
    peer->recv_ev_active = true;
    pmix_event_assign(&peer->send_event, pmix_globals.evbase, pnd->sd,
                      EV_WRITE|EV_PERSIST, pmix_ptl_base_send_handler, peer);
    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:server client %s:%u has connected on socket %d",
                        peer->info->pname.nspace, peer->info->pname.rank, peer->sd);
    PMIX_RELEASE(pnd);
    return;

  error:
    /* send an error reply to the client */
    if (PMIX_SUCCESS != pmix_ptl_base_send_blocking(pnd->sd, (char*)&rc, sizeof(int))) {
        PMIX_ERROR_LOG(rc);
        CLOSE_THE_SOCKET(pnd->sd);
    }
    PMIX_RELEASE(pnd);
    return;
}

/* process the callback with tool connection info */
static void process_cbfunc(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_pending_connection_t *pnd = (pmix_pending_connection_t*)cd->cbdata;
    pmix_nspace_t *nptr;
    pmix_rank_info_t *info;
    int rc;
    uint32_t u32;

    /* send this status so they don't hang */
    u32 = ntohl(cd->status);
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_send_blocking(pnd->sd, (char*)&u32, sizeof(uint32_t)))) {
        PMIX_ERROR_LOG(rc);
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        PMIX_RELEASE(cd);
        return;
    }

    /* if the request failed, then we are done */
    if (PMIX_SUCCESS != cd->status) {
        PMIX_RELEASE(pnd);
        PMIX_RELEASE(cd);
        return;
    }

    /* send the nspace back to the tool */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_send_blocking(pnd->sd, cd->proc.nspace, PMIX_MAX_NSLEN+1))) {
        PMIX_ERROR_LOG(rc);
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        PMIX_RELEASE(cd);
        return;
    }

    /* send my nspace back to the tool */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_send_blocking(pnd->sd, pmix_globals.myid.nspace, PMIX_MAX_NSLEN+1))) {
        PMIX_ERROR_LOG(rc);
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        PMIX_RELEASE(cd);
        return;
    }

    /* send my rank back to the tool */
    u32 = ntohl(pmix_globals.myid.rank);
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_send_blocking(pnd->sd, (char*)&u32, sizeof(uint32_t)))) {
        PMIX_ERROR_LOG(rc);
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        PMIX_RELEASE(cd);
        return;
    }

    /* add this nspace to our pool */
    nptr = PMIX_NEW(pmix_nspace_t);
    (void)strncpy(nptr->nspace, cd->proc.nspace, PMIX_MAX_NSLEN);
    pmix_list_append(&pmix_server_globals.nspaces, &nptr->super);
    /* add this tool rank to the nspace */
    info = PMIX_NEW(pmix_rank_info_t);
    info->pname.nspace = strdup(cd->proc.nspace);
    info->pname.rank = 0;
    /* need to include the uid/gid for validation */
    info->uid = pnd->uid;
    info->gid = pnd->gid;
    pmix_list_append(&nptr->ranks, &info->super);

    /* setup a peer object for this tool */
    pmix_peer_t *peer = PMIX_NEW(pmix_peer_t);
    PMIX_RETAIN(nptr);
    peer->nptr = nptr;
    PMIX_RETAIN(info);
    peer->info = info;
    peer->proc_cnt = 1;
    peer->sd = pnd->sd;

    /* get the appropriate compatibility modules based on the
     * info provided by the tool during the initial connection request */
    peer->nptr->compat.psec = pmix_psec_base_assign_module(pnd->psec);
    if (NULL == peer->nptr->compat.psec) {
        PMIX_RELEASE(peer);
        pmix_list_remove_item(&pmix_server_globals.nspaces, &nptr->super);
        PMIX_RELEASE(nptr);  // will release the info object
        CLOSE_THE_SOCKET(pnd->sd);
        goto done;
    }
    /* the choice of PTL module was obviously made by the connecting
     * tool as we received this request via that channel, so simply
     * record it here for future use */
    peer->nptr->compat.ptl = &pmix_ptl_tcp_module;

    /* validate the connection */
    PMIX_PSEC_VALIDATE_CONNECTION(rc, peer,
                                  PMIX_PROTOCOL_V2,
                                  pnd->cred, pnd->len);
    if (PMIX_SUCCESS != rc) {
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "validation of tool credentials failed: %s",
                            PMIx_Error_string(rc));
        PMIX_RELEASE(peer);
        pmix_list_remove_item(&pmix_server_globals.nspaces, &nptr->super);
        PMIX_RELEASE(nptr);  // will release the info object
        CLOSE_THE_SOCKET(pnd->sd);
        goto done;
    }

    /* set the socket non-blocking for all further operations */
    pmix_ptl_base_set_nonblocking(pnd->sd);

    if (0 > (peer->index = pmix_pointer_array_add(&pmix_server_globals.clients, peer))) {
        PMIX_RELEASE(pnd);
        PMIX_RELEASE(cd);
        PMIX_RELEASE(peer);
        pmix_list_remove_item(&pmix_server_globals.nspaces, &nptr->super);
        PMIX_RELEASE(nptr);  // will release the info object
        /* probably cannot send an error reply if we are out of memory */
        return;
    }

    /* start the events for this tool */
    pmix_event_assign(&peer->recv_event, pmix_globals.evbase, pnd->sd,
                      EV_READ|EV_PERSIST, pmix_ptl_base_recv_handler, peer);
    pmix_event_add(&peer->recv_event, NULL);
    peer->recv_ev_active = true;
    pmix_event_assign(&peer->send_event, pmix_globals.evbase, pnd->sd,
                      EV_WRITE|EV_PERSIST, pmix_ptl_base_send_handler, peer);
    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:server tool %s:%d has connected on socket %d",
                        peer->info->pname.nspace, peer->info->pname.rank, peer->sd);

  done:
    PMIX_RELEASE(pnd);
    PMIX_RELEASE(cd);
}

/* receive a callback from the host RM with an nspace
 * for a connecting tool */
static void cnct_cbfunc(pmix_status_t status,
                        pmix_proc_t *proc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:tcp:cnct_cbfunc returning %s:%d",
                        proc->nspace, proc->rank);

    /* need to thread-shift this into our context */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    cd->status = status;
    (void)strncpy(cd->proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
    cd->cbdata = cbdata;
    PMIX_THREADSHIFT(cd, process_cbfunc);
}
