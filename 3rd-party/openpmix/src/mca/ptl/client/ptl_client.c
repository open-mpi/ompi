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
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "src/include/pmix_config.h"
#include "src/include/pmix_globals.h"

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

#include "src/client/pmix_client_ops.h"
#include "src/include/pmix_socket_errno.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/gds.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_show_help.h"

#include "ptl_client.h"
#include "src/mca/ptl/base/base.h"

static pmix_status_t connect_to_peer(struct pmix_peer_t *peer, pmix_info_t *info, size_t ninfo);

pmix_ptl_module_t pmix_ptl_client_module = {
    .name = "client",
    .connect_to_peer = connect_to_peer
};

static pmix_status_t connect_to_peer(struct pmix_peer_t *pr, pmix_info_t *info, size_t ninfo)
{
    char *evar = NULL, *suri = NULL;
    char *nspace = NULL;
    pmix_rank_t rank = PMIX_RANK_WILDCARD;
    char *rendfile = NULL;
    pmix_status_t rc;
    pmix_peer_t *peer = (pmix_peer_t *) pr;
    size_t m, n;
    char **tmp, *mycmd;
    void *ilist;
    pid_t mypid;
    pmix_info_t *iptr;
    size_t niptr;
    pmix_data_array_t darray;
    pmix_list_t connections;
    pmix_connection_t *cn;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "ptl:tcp: connecting to server");

    /* see if we were given one */
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_URI)) {
            /* separate out the server URI version(s) */
            suri = strchr(info[n].value.data.string, ';');
            if (NULL == suri) {
                return PMIX_ERR_BAD_PARAM;
            }
            *suri = '\0';
            ++suri;
            evar = info[n].value.data.string;
            /* set the peer's module and type */
            tmp = PMIx_Argv_split(evar, ':');
            rc = PMIX_ERR_BAD_PARAM;
            for (m = 0; NULL != tmp[m]; m++) {
                rc = pmix_ptl_base_set_peer(peer, tmp[m]);
                if (PMIX_SUCCESS == rc) {
                    break;
                }
            }
            PMIx_Argv_free(tmp);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
            /* setup to process the URI */
            evar = suri;
            break;
        }
    }
    if (NULL == evar) {
        /* check the environment */
        rc = pmix_ptl_base_check_server_uris(peer, &evar);
        if (PMIX_SUCCESS != rc) {
            /* we must be a singleton */
            PMIX_SET_PEER_TYPE(pmix_globals.mypeer, PMIX_PROC_SINGLETON);
            /* if we weren't given one and don't have one
             * in the environment, we are allowed to check
             * for a system server */
            pmix_globals.mypeer->nptr->compat.bfrops = pmix_bfrops_base_assign_module(NULL);
            pmix_client_globals.myserver->nptr->compat.bfrops = pmix_bfrops_base_assign_module(NULL);
            /* setup the system rendezvous file name */
            if (0 > asprintf(&rendfile, "%s/pmix.sys.%s", pmix_ptl_base.system_tmpdir,
                             pmix_globals.hostname)) {
                return PMIX_ERR_NOMEM;
            }
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "ptl:client looking for system server at %s", rendfile);
            /* try to read the file */
            PMIX_CONSTRUCT(&connections, pmix_list_t);
            rc = pmix_ptl_base_parse_uri_file(rendfile, true, &connections);
            free(rendfile);
            rendfile = NULL;
            if (PMIX_SUCCESS == rc && 0 < pmix_list_get_size(&connections)) {
                cn = (pmix_connection_t *) pmix_list_get_first(&connections);
                /* provide our cmd line and PID */
                PMIX_INFO_LIST_START(ilist);
                mypid = getpid();
                PMIX_INFO_LIST_ADD(rc, ilist, PMIX_PROC_PID, &mypid, PMIX_PID);
                mycmd = pmix_ptl_base_get_cmd_line();
                if (NULL != mycmd) {
                    PMIX_INFO_LIST_ADD(rc, ilist, PMIX_CMD_LINE, mycmd, PMIX_STRING);
                }
                PMIX_INFO_LIST_CONVERT(rc, ilist, &darray);
                if (PMIX_ERR_EMPTY == rc) {
                    iptr = NULL;
                    niptr = 0;
                } else if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_INFO_LIST_RELEASE(ilist);
                    PMIX_LIST_DESTRUCT(&connections);
                    return rc;
                } else {
                    iptr = (pmix_info_t *) darray.array;
                    niptr = darray.size;
                }
                PMIX_INFO_LIST_RELEASE(ilist);
                /* set our protocol to V2 as that is all we support */
                pmix_globals.mypeer->protocol = PMIX_PROTOCOL_V2;
                peer->protocol = PMIX_PROTOCOL_V2;
                PMIX_SET_PEER_VERSION(peer, cn->version, 2, 0);
                /* go ahead and try to connect */
                rc = pmix_ptl_base_make_connection(peer, cn->uri, iptr, niptr);
                if (PMIX_SUCCESS == rc) {
                    /* don't free nspace - we will use it below */
                    nspace = cn->nspace;
                    rank = cn->rank;
                    suri = cn->uri;
                    cn->nspace = NULL;
                    cn->uri = NULL;
                    PMIX_LIST_DESTRUCT(&connections);
                    goto complete;
                }
            }
            pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                                "ptl:tcp:client is singleton");
            PMIX_LIST_DESTRUCT(&connections);
            return PMIX_ERR_UNREACH;
        }
    }

    /* the URI consists of the following elements:
     *    - server nspace.rank
     *    - ptl rendezvous URI
     */
    rc = pmix_ptl_base_parse_uri(evar, &nspace, &rank, &suri);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "ptl:tcp:client attempt connect to %s:%u at %s", nspace, rank, suri);

    rc = pmix_ptl_base_make_connection(peer, suri, NULL, 0);
    if (PMIX_SUCCESS != rc) {
        free(nspace);
        free(suri);
        return rc;
    }

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "tcp_peer_try_connect: Connection across to peer %s:%u succeeded", nspace,
                        rank);

complete:
    /* mark the connection as made */
    pmix_ptl_base_complete_connection(peer, nspace, rank, suri);

    if (NULL != nspace) {
        free(nspace);
    }
    if (NULL != suri) {
        free(suri);
    }
    return rc;
}
