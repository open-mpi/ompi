/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
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
#include <src/include/pmix_stdint.h>
#include <src/include/pmix_socket_errno.h>

#include <pmix_server.h>
#include <pmix_common.h>
#include <pmix_rename.h>

#include "src/include/pmix_globals.h"

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
#include <ctype.h>
#include <sys/stat.h>
#include PMIX_EVENT_HEADER
#include PMIX_EVENT2_THREAD_HEADER

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"
#include "src/util/show_help.h"
#include "src/mca/base/base.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/pinstalldirs/base/base.h"
#include "src/mca/pnet/pnet.h"
#include "src/runtime/pmix_progress_threads.h"
#include "src/runtime/pmix_rte.h"
#include "src/mca/ptl/base/base.h"
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
#include "src/dstore/pmix_dstore.h"
#endif /* PMIX_ENABLE_DSTORE */
#include "src/include/pmix_jobdata.h"

#include "pmix_server_ops.h"

// global variables
pmix_server_globals_t pmix_server_globals = {{{0}}};

// local variables
static char *security_mode = NULL;
static char *ptl_mode = NULL;
static pid_t mypid;

// local functions for connection support
static void server_message_handler(struct pmix_peer_t *pr,
                                   pmix_ptl_hdr_t *hdr,
                                   pmix_buffer_t *buf, void *cbdata);

static inline int _my_client(const char *nspace, pmix_rank_t rank);

static pmix_status_t initialize_server_base(pmix_server_module_t *module)
{
    char *evar;

    /* look for our namespace, if one was given */
    if (NULL == (evar = getenv("PMIX_SERVER_NAMESPACE"))) {
        /* use a fake namespace */
        (void)strncpy(pmix_globals.myid.nspace, "pmix-server", PMIX_MAX_NSLEN);
    } else {
        (void)strncpy(pmix_globals.myid.nspace, evar, PMIX_MAX_NSLEN);
    }
    /* look for our rank, if one was given */
    mypid = getpid();
    if (NULL == (evar = getenv("PMIX_SERVER_RANK"))) {
        /* use our pid */
        pmix_globals.myid.rank = mypid;
    } else {
        pmix_globals.myid.rank = strtol(evar, NULL, 10);
    }

    /* setup the server-specific globals */
    PMIX_CONSTRUCT(&pmix_server_globals.clients, pmix_pointer_array_t);
    pmix_pointer_array_init(&pmix_server_globals.clients, 1, INT_MAX, 1);
    PMIX_CONSTRUCT(&pmix_server_globals.collectives, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.remote_pnd, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.gdata, pmix_buffer_t);
    PMIX_CONSTRUCT(&pmix_server_globals.events, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.local_reqs, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.notifications, pmix_ring_buffer_t);
    pmix_ring_buffer_init(&pmix_server_globals.notifications, 256);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server init called");

    /* setup the function pointers */
    memset(&pmix_host_server, 0, sizeof(pmix_server_module_t));
    pmix_host_server = *module;

    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_server_init(pmix_server_module_t *module,
                                           pmix_info_t info[], size_t ninfo)
{
    pmix_ptl_posted_recv_t *req;
    pmix_status_t rc;
    size_t n, m;
    pmix_kval_t kv;
    bool protect;
    char *protected[] = {
        PMIX_USERID,
        PMIX_GRPID,
        PMIX_SOCKET_MODE,
        PMIX_SERVER_TOOL_SUPPORT,
        PMIX_SERVER_SYSTEM_SUPPORT,
        NULL
    };

    if (0 < pmix_globals.init_cntr) {
        return PMIX_SUCCESS;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server init called");

    /* setup the runtime - this init's the globals,
     * opens and initializes the required frameworks */
    if (PMIX_SUCCESS != (rc = pmix_rte_init(PMIX_PROC_SERVER, info, ninfo, NULL))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    if (0 != (rc = initialize_server_base(module))) {
        return rc;
    }

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    if (PMIX_SUCCESS != (rc = pmix_dstore_init(info, ninfo))) {
        return rc;
    }
#endif /* PMIX_ENABLE_DSTORE */

    /* setup the wildcard recv for inbound messages from clients */
    req = PMIX_NEW(pmix_ptl_posted_recv_t);
    req->tag = UINT32_MAX;
    req->cbfunc = server_message_handler;
    /* add it to the end of the list of recvs */
    pmix_list_append(&pmix_ptl_globals.posted_recvs, &req->super);

    if (PMIX_SUCCESS != pmix_ptl_base_start_listening(info, ninfo)) {
        pmix_show_help("help-pmix-server.txt", "listener-thread-start", true);
        PMIx_server_finalize();
        return PMIX_ERR_INIT;
    }

    /* check the info keys for info we
     * need to provide to every client */
    if (NULL != info) {
        PMIX_CONSTRUCT(&kv, pmix_kval_t);
        for (n=0; n < ninfo; n++) {
            /* check the list of protected keys */
            protect = false;
            for (m=0; NULL != protected[m]; m++) {
                if (0 == strcmp(info[n].key, protected[m])) {
                    protect = true;
                    break;
                }
            }
            if (protect) {
                continue;
            }
            /* store and pass along to every client */
            kv.key = info[n].key;
            kv.value = &info[n].value;
            if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&pmix_server_globals.gdata, &kv, 1, PMIX_KVAL))) {
                PMIX_ERROR_LOG(rc);
                pmix_show_help("help-pmix-server.txt", "data-store-failed", true, kv.key);
                /* protect the incoming data */
                kv.key = NULL;
                kv.value = NULL;
                PMIX_DESTRUCT(&kv);
                PMIx_server_finalize();
                return rc;
            }
        }
        /* protect the incoming data */
        kv.key = NULL;
        kv.value = NULL;
        PMIX_DESTRUCT(&kv);
    }

    /* get our available security modules */
    security_mode = pmix_psec.get_available_modules();

    /* get our available ptl modules */
    ptl_mode = pmix_ptl.get_available_modules();

    ++pmix_globals.init_cntr;

    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_server_finalize(void)
{
    int i;
    pmix_peer_t *peer;

    if (1 != pmix_globals.init_cntr) {
        --pmix_globals.init_cntr;
        return PMIX_SUCCESS;
    }
    pmix_globals.init_cntr = 0;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server finalize called");

    if (!pmix_globals.external_evbase) {
        /* stop the progress thread, but leave the event base
         * still constructed. This will allow us to safely
         * tear down the infrastructure, including removal
         * of any events objects may be holding */
        (void)pmix_progress_thread_pause(NULL);
    }

    pmix_ptl_base_stop_listening();

    for (i=0; i < pmix_server_globals.clients.size; i++) {
        if (NULL != (peer = (pmix_peer_t*)pmix_pointer_array_get_item(&pmix_server_globals.clients, i))) {
            PMIX_RELEASE(peer);
        }
    }
    PMIX_DESTRUCT(&pmix_server_globals.clients);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.collectives);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.remote_pnd);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.local_reqs);
    PMIX_DESTRUCT(&pmix_server_globals.gdata);
    PMIX_DESTRUCT(&pmix_server_globals.notifications);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.events);

    if (NULL != security_mode) {
        free(security_mode);
    }

    if (NULL != ptl_mode) {
        free(ptl_mode);
    }

    pmix_bfrop_close();
    pmix_rte_finalize();

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server finalize complete");

    return PMIX_SUCCESS;
}

static void _register_nspace(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_nspace_t *nptr, *tmp;
    pmix_status_t rc;
    size_t i, j, size;
    int rank;
    pmix_kval_t kv;
    char **nodes=NULL, **procs=NULL;
    pmix_buffer_t buf2;
    pmix_info_t *iptr;
    pmix_value_t val;
    char *msg;
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    bool nodata = false;
    pmix_buffer_t *jobdata = PMIX_NEW(pmix_buffer_t);
    char *nspace = NULL;
    int32_t cnt;
#endif

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server _register_nspace %s", cd->proc.nspace);

    /* see if we already have this nspace */
    nptr = NULL;
    PMIX_LIST_FOREACH(tmp, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(tmp->nspace, cd->proc.nspace)) {
            nptr = tmp;
            /* release any existing packed data - we will replace it */
            if (0 < nptr->server->job_info.bytes_used) {
                PMIX_DESTRUCT(&nptr->server->job_info);
                PMIX_CONSTRUCT(&nptr->server->job_info, pmix_buffer_t);
            }
            break;
        }
    }
    if (NULL == nptr) {
        nptr = PMIX_NEW(pmix_nspace_t);
        (void)strncpy(nptr->nspace, cd->proc.nspace, PMIX_MAX_NSLEN);
        nptr->server = PMIX_NEW(pmix_server_nspace_t);
        pmix_list_append(&pmix_globals.nspaces, &nptr->super);
    }
    nptr->server->nlocalprocs = cd->nlocalprocs;
    /* see if we have everyone */
    if (nptr->server->nlocalprocs == pmix_list_get_size(&nptr->server->ranks)) {
        nptr->server->all_registered = true;
    }
    /* pack the name of the nspace */
    msg = nptr->nspace;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&nptr->server->job_info, &msg, 1, PMIX_STRING))) {
        PMIX_ERROR_LOG(rc);
        pmix_list_remove_item(&pmix_globals.nspaces, &nptr->super);
        PMIX_RELEASE(nptr);
        goto release;
    }

    /* pack the provided info */
    PMIX_CONSTRUCT(&kv, pmix_kval_t);
    for (i=0; i < cd->ninfo; i++) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:server _register_nspace recording %s",
                            cd->info[i].key);

        if (0 == strcmp(cd->info[i].key, PMIX_REGISTER_NODATA)) {
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
            /* we don't want to save any job data for this nspace */
            nodata = true;
#endif
            /* free anything that was previously stored */
            PMIX_DESTRUCT(&nptr->server->job_info);
            PMIX_CONSTRUCT(&nptr->server->job_info, pmix_buffer_t);
            break;
        }
        if (0 == strcmp(cd->info[i].key, PMIX_NODE_MAP)) {
            /* parse the regex to get the argv array of node names */
            if (PMIX_SUCCESS != (rc = pmix_regex_parse_nodes(cd->info[i].value.data.string, &nodes))) {
                PMIX_ERROR_LOG(rc);
                continue;
            }
            /* if we have already found the proc map, then pass
             * the detailed map */
            if (NULL != procs) {
                pmix_pack_proc_map(&nptr->server->job_info, nodes, procs);
                pmix_argv_free(nodes);
                nodes = NULL;
                pmix_argv_free(procs);
                procs = NULL;
            }
        } else if (0 == strcmp(cd->info[i].key, PMIX_PROC_MAP)) {
            /* parse the regex to get the argv array containg proc ranks on each node */
            if (PMIX_SUCCESS != (rc = pmix_regex_parse_procs(cd->info[i].value.data.string, &procs))) {
                PMIX_ERROR_LOG(rc);
                continue;
            }
            /* if we have already recv'd the node map, then record
             * the detailed map */
            if (NULL != nodes) {
                pmix_pack_proc_map(&nptr->server->job_info, nodes, procs);
                pmix_argv_free(nodes);
                nodes = NULL;
                pmix_argv_free(procs);
                procs = NULL;
            }
        } else if (0 == strcmp(cd->info[i].key, PMIX_PROC_DATA)) {
            /* an array of data pertaining to a specific proc */
            if (PMIX_DATA_ARRAY != cd->info[i].value.type ||
                PMIX_INFO != cd->info[i].value.data.darray->type) {
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                goto release;
            }
            size = cd->info[i].value.data.darray->size;
            iptr = (pmix_info_t*)cd->info[i].value.data.darray->array;
            PMIX_CONSTRUCT(&buf2, pmix_buffer_t);
            /* first element of the array must be the rank */
            if (0 != strcmp(iptr[0].key, PMIX_RANK)) {
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                PMIX_DESTRUCT(&buf2);
                goto release;
            }
            /* pack it separately */
            rank = iptr[0].value.data.rank;
            if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&buf2, &rank, 1, PMIX_PROC_RANK))) {
                PMIX_ERROR_LOG(rc);
                pmix_list_remove_item(&pmix_globals.nspaces, &nptr->super);
                PMIX_RELEASE(nptr);
                PMIX_DESTRUCT(&buf2);
                goto release;
            }
            /* cycle thru the values for this rank and pack them */
            for (j=1; j < size; j++) {
                kv.key = iptr[j].key;
                kv.value = &iptr[j].value;
                if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&buf2, &kv, 1, PMIX_KVAL))) {
                    PMIX_ERROR_LOG(rc);
                    pmix_list_remove_item(&pmix_globals.nspaces, &nptr->super);
                    PMIX_RELEASE(nptr);
                    PMIX_DESTRUCT(&buf2);
                    goto release;
                }
            }
            /* now add the blob */
            kv.key = PMIX_PROC_BLOB;
            kv.value = &val;
            val.type = PMIX_BYTE_OBJECT;
            val.data.bo.bytes = buf2.base_ptr;
            val.data.bo.size = buf2.bytes_used;
            if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&nptr->server->job_info, &kv, 1, PMIX_KVAL))) {
                PMIX_ERROR_LOG(rc);
                pmix_list_remove_item(&pmix_globals.nspaces, &nptr->super);
                PMIX_RELEASE(nptr);
                PMIX_DESTRUCT(&buf2);
                goto release;
            }
            PMIX_DESTRUCT(&buf2);
        } else {
            /* just a value relating to the entire job */
            kv.key = cd->info[i].key;
            kv.value = &cd->info[i].value;

            if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&nptr->server->job_info, &kv, 1, PMIX_KVAL))) {
                PMIX_ERROR_LOG(rc);
                pmix_list_remove_item(&pmix_globals.nspaces, &nptr->super);
                PMIX_RELEASE(nptr);
                goto release;
            }
        }
    }
    /* do not destruct the kv object - no memory leak will result */

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    if (PMIX_SUCCESS != (rc = pmix_dstore_nspace_add(cd->proc.nspace, cd->info, cd->ninfo))) {
        PMIX_ERROR_LOG(rc);
        goto release;
    }
    if (!nodata) {
        pmix_bfrop.copy_payload(jobdata, &nptr->server->job_info);
        pmix_bfrop.copy_payload(jobdata, &pmix_server_globals.gdata);

        /* unpack the nspace - we don't really need it, but have to
        * unpack it to maintain sequence */
        cnt = 1;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(jobdata, &nspace, &cnt, PMIX_STRING))) {
            PMIX_ERROR_LOG(rc);
            goto release;
        }
        if (PMIX_SUCCESS != (rc = pmix_job_data_dstore_store(cd->proc.nspace, jobdata))) {
            PMIX_ERROR_LOG(rc);
            goto release;
        }
    }
#endif

 release:
    if (NULL != nodes) {
        pmix_argv_free(nodes);
    }
    if (NULL != procs) {
        pmix_argv_free(procs);
    }
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(rc, cd->cbdata);
    }
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    if (NULL != nspace) {
        free(nspace);
    }
    if (NULL != jobdata) {
        PMIX_RELEASE(jobdata);
    }
#endif
    PMIX_RELEASE(cd);
}

/* setup the data for a job */
PMIX_EXPORT pmix_status_t PMIx_server_register_nspace(const char nspace[], int nlocalprocs,
                                                      pmix_info_t info[], size_t ninfo,
                                                      pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    cd = PMIX_NEW(pmix_setup_caddy_t);
    (void)strncpy(cd->proc.nspace, nspace, PMIX_MAX_NSLEN);
    cd->nlocalprocs = nlocalprocs;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;
    /* copy across the info array, if given */
    if (0 < ninfo) {
        cd->ninfo = ninfo;
        cd->info = info;
    }

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _register_nspace);
    return PMIX_SUCCESS;
}

static void _deregister_nspace(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_nspace_t *tmp;
    pmix_status_t rc = PMIX_SUCCESS;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server _deregister_nspace %s",
                        cd->proc.nspace);

    /* see if we already have this nspace */
    PMIX_LIST_FOREACH(tmp, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(tmp->nspace, cd->proc.nspace)) {
            pmix_list_remove_item(&pmix_globals.nspaces, &tmp->super);
            PMIX_RELEASE(tmp);
            break;
        }
    }

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    rc = pmix_dstore_nspace_del(cd->proc.nspace);
#endif

    /* release any job-level resources */
    pmix_pnet.local_app_finalized(cd->proc.nspace);

    /* release the caller */
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(rc, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT void PMIx_server_deregister_nspace(const char nspace[],
                                               pmix_op_cbfunc_t cbfunc,
                                               void *cbdata)
{
    pmix_setup_caddy_t *cd;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server deregister nspace %s",
                        nspace);

     cd = PMIX_NEW(pmix_setup_caddy_t);
    (void)strncpy(cd->proc.nspace, nspace, PMIX_MAX_NSLEN);
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _deregister_nspace);
}

void pmix_server_execute_collective(int sd, short args, void *cbdata)
{
    pmix_trkr_caddy_t *tcd = (pmix_trkr_caddy_t*)cbdata;
    pmix_server_trkr_t *trk = tcd->trk;
    char *data = NULL;
    size_t sz = 0;
    pmix_buffer_t bucket, xfer;
    pmix_rank_info_t *info;
    pmix_value_t *val;

    /* we don't need to check for non-NULL APIs here as
     * that was already done when the tracker was created */
    if (PMIX_FENCENB_CMD == trk->type) {
        /* if the user asked us to collect data, then we have
         * to provide any locally collected data to the host
         * server so they can circulate it - only take data
         * from the specified procs as not everyone is necessarily
         * participating! And only take data intended for remote
         * distribution as local data will be added when we send
         * the result to our local clients */
        PMIX_CONSTRUCT(&bucket, pmix_buffer_t);

        assert( PMIX_COLLECT_MAX < UCHAR_MAX );
        unsigned char tmp = (unsigned char)trk->collect_type;
        pmix_bfrop.pack(&bucket, &tmp, 1, PMIX_BYTE);

        if (PMIX_COLLECT_YES == trk->collect_type) {
            pmix_buffer_t databuf;
            PMIX_CONSTRUCT(&databuf, pmix_buffer_t);
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "fence - assembling data");
            PMIX_LIST_FOREACH(info, &trk->ranks, pmix_rank_info_t) {
                pmix_buffer_t rankbuf;
                PMIX_CONSTRUCT(&rankbuf, pmix_buffer_t);
                /* get any remote contribution - note that there
                 * may not be a contribution */
                if (PMIX_SUCCESS == pmix_hash_fetch(&info->nptr->server->myremote, info->rank, "modex", &val) &&
                    NULL != val) {
                    /* pack the proc so we know the source */
                    char *foobar = info->nptr->nspace;
                    pmix_bfrop.pack(&rankbuf, &foobar, 1, PMIX_STRING);
                    pmix_bfrop.pack(&rankbuf, &info->rank, 1, PMIX_PROC_RANK);
                    PMIX_CONSTRUCT(&xfer, pmix_buffer_t);
                    PMIX_LOAD_BUFFER(&xfer, val->data.bo.bytes, val->data.bo.size);
                    PMIX_VALUE_RELEASE(val);
                    pmix_buffer_t *pxfer = &xfer;
                    pmix_bfrop.pack(&rankbuf, &pxfer, 1, PMIX_BUFFER);
                    PMIX_DESTRUCT(&xfer);
                    /* now pack this proc's contribution into the bucket */
                    pmix_buffer_t *pdatabuf = &rankbuf;
                    pmix_bfrop.pack(&databuf, &pdatabuf, 1, PMIX_BUFFER);
                }
                PMIX_DESTRUCT(&rankbuf);
            }
            // TODO: we have multiple data movings while only one is actually need
            pmix_buffer_t *pbkt = &databuf;
            pmix_bfrop.pack(&bucket, &pbkt, 1, PMIX_BUFFER);
            PMIX_DESTRUCT(&databuf);
        }
        PMIX_UNLOAD_BUFFER(&bucket, data, sz);
        PMIX_DESTRUCT(&bucket);
        pmix_host_server.fence_nb(trk->pcs, trk->npcs,
                                  trk->info, trk->ninfo,
                                  data, sz, trk->modexcbfunc, trk);
    } else if (PMIX_CONNECTNB_CMD == trk->type) {
        pmix_host_server.connect(trk->pcs, trk->npcs,
                                 trk->info, trk->ninfo,
                                 trk->op_cbfunc, trk);
    } else if (PMIX_DISCONNECTNB_CMD == trk->type) {
        pmix_host_server.disconnect(trk->pcs, trk->npcs,
                                    trk->info, trk->ninfo,
                                    trk->op_cbfunc, trk);
    } else {
        /* unknown type */
        PMIX_ERROR_LOG(PMIX_ERR_NOT_FOUND);
        pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
        PMIX_RELEASE(trk);
    }
    PMIX_RELEASE(tcd);
}

static void _register_client(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_rank_info_t *info, *iptr, *iptr2;
    pmix_nspace_t *nptr, *tmp;
    pmix_server_trkr_t *trk;
    pmix_trkr_caddy_t *tcd;
    bool all_def;
    size_t i;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server _register_client for nspace %s rank %d",
                        cd->proc.nspace, cd->proc.rank);

    /* see if we already have this nspace */
    nptr = NULL;
    PMIX_LIST_FOREACH(tmp, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(tmp->nspace, cd->proc.nspace)) {
            nptr = tmp;
            break;
        }
    }
    if (NULL == nptr) {
        nptr = PMIX_NEW(pmix_nspace_t);
        (void)strncpy(nptr->nspace, cd->proc.nspace, PMIX_MAX_NSLEN);
        /* add the server object */
        nptr->server = PMIX_NEW(pmix_server_nspace_t);
        pmix_list_append(&pmix_globals.nspaces, &nptr->super);
    }
    /* setup a peer object for this client - since the host server
     * only deals with the original processes and not any clones,
     * we know this function will be called only once per rank */
    info = PMIX_NEW(pmix_rank_info_t);
    PMIX_RETAIN(nptr);
    info->nptr = nptr;
    info->rank = cd->proc.rank;
    info->uid = cd->uid;
    info->gid = cd->gid;
    info->server_object = cd->server_object;
    pmix_list_append(&nptr->server->ranks, &info->super);
    /* see if we have everyone */
    if (nptr->server->nlocalprocs == pmix_list_get_size(&nptr->server->ranks)) {
        nptr->server->all_registered = true;
        /* check any pending trackers to see if they are
         * waiting for us. There is a slight race condition whereby
         * the host server could have spawned the local client and
         * it called back into the collective -before- our local event
         * would fire the register_client callback. Deal with that here. */
        PMIX_LIST_FOREACH(trk, &pmix_server_globals.collectives, pmix_server_trkr_t) {
            /* if this tracker is already complete, then we
             * don't need to update it */
            if (trk->def_complete) {
                continue;
            }
            /* see if any of our procs are involved - the tracker will
             * have been created because a callback was received, but
             * no rank info will have been entered since the clients
             * had not yet been registered. Thus, we couldn't enter rank
             * objects into the tracker as we didn't know which
             * of the ranks were local */
            for (i=0; i < trk->npcs; i++) {
                if (0 != strncmp(cd->proc.nspace, trk->pcs[i].nspace, PMIX_MAX_NSLEN)) {
                    continue;
                }
                /* need to check if this rank is one of mine */
                PMIX_LIST_FOREACH(iptr, &nptr->server->ranks, pmix_rank_info_t) {
                    if (PMIX_RANK_WILDCARD == trk->pcs[i].rank ||
                        iptr->rank == trk->pcs[i].rank) {
                        /* add a tracker for this proc - don't need more than
                         * the nspace pointer and rank */
                        iptr2 = PMIX_NEW(pmix_rank_info_t);
                        PMIX_RETAIN(info->nptr);
                        iptr2->nptr = info->nptr;
                        iptr2->rank = info->rank;
                        pmix_list_append(&trk->ranks, &iptr2->super);
                        /* track the count */
                        ++trk->nlocal;
                    }
                }
            }
            /* we need to know if this tracker is now complete - the only
             * way to do this is to check if all participating
             * nspaces are fully registered */
            all_def = true;
            /* search all the involved procs - fortunately, this
             * list is usually very small */
            PMIX_LIST_FOREACH(iptr, &trk->ranks, pmix_rank_info_t) {
                if (!iptr->nptr->server->all_registered) {
                    /* nope */
                    all_def = false;
                    break;
                }
            }
            /* update this tracker's status */
            trk->def_complete = all_def;
            /* is this now completed? */
            if (trk->def_complete && pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
                /* it did, so now we need to process it
                 * we don't want to block someone
                 * here, so kick any completed trackers into a
                 * new event for processing */
                PMIX_EXECUTE_COLLECTIVE(tcd, trk, pmix_server_execute_collective);
            }
        }
        /* also check any pending local modex requests to see if
         * someone has been waiting for a request on a remote proc
         * in one of our nspaces, but we didn't know all the local procs
         * and so couldn't determine the proc was remote */
        pmix_pending_nspace_requests(nptr);
    }
    /* let the caller know we are done */
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(PMIX_SUCCESS, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT pmix_status_t PMIx_server_register_client(const pmix_proc_t *proc,
                                                      uid_t uid, gid_t gid, void *server_object,
                                                      pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server register client %s:%d",
                        proc->nspace, proc->rank);

     cd = PMIX_NEW(pmix_setup_caddy_t);
    (void)strncpy(cd->proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
    cd->proc.rank = proc->rank;
    cd->uid = uid;
    cd->gid = gid;
    cd->server_object = server_object;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _register_client);
    return PMIX_SUCCESS;
}

static void _deregister_client(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_rank_info_t *info;
    pmix_nspace_t *nptr, *tmp;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server _deregister_client for nspace %s rank %d",
                        cd->proc.nspace, cd->proc.rank);

    /* see if we already have this nspace */
    nptr = NULL;
    PMIX_LIST_FOREACH(tmp, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(tmp->nspace, cd->proc.nspace)) {
            nptr = tmp;
            break;
        }
    }
    if (NULL == nptr) {
        /* nothing to do */
        goto cleanup;
    }
    /* find and remove this client */
    PMIX_LIST_FOREACH(info, &nptr->server->ranks, pmix_rank_info_t) {
        if (info->rank == cd->proc.rank) {
            pmix_list_remove_item(&nptr->server->ranks, &info->super);
            PMIX_RELEASE(info);
            break;
        }
    }

  cleanup:
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(PMIX_SUCCESS, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT void PMIx_server_deregister_client(const pmix_proc_t *proc,
                                               pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server deregister client %s:%d",
                        proc->nspace, proc->rank);

     cd = PMIX_NEW(pmix_setup_caddy_t);
    (void)strncpy(cd->proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
    cd->proc.rank = proc->rank;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _deregister_client);
}

/* setup the envars for a child process */
PMIX_EXPORT pmix_status_t PMIx_server_setup_fork(const pmix_proc_t *proc, char ***env)
{
    char rankstr[128];
    pmix_listener_t *lt;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server setup_fork for nspace %s rank %d",
                        proc->nspace, proc->rank);

    /* pass the nspace */
    pmix_setenv("PMIX_NAMESPACE", proc->nspace, true, env);
    /* pass the rank */
    (void)snprintf(rankstr, 127, "%d", proc->rank);
    pmix_setenv("PMIX_RANK", rankstr, true, env);
    /* pass our rendezvous info */
    PMIX_LIST_FOREACH(lt, &pmix_ptl_globals.listeners, pmix_listener_t) {
        if (NULL != lt->uri && NULL != lt->varname) {
            pmix_setenv(lt->varname, lt->uri, true, env);
        }
    }
    /* pass our active security modules */
    pmix_setenv("PMIX_SECURITY_MODE", security_mode, true, env);
    /* pass our available ptl modules */
    pmix_setenv("PMIX_PTL_MODULE", ptl_mode, true, env);

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    /* pass dstore path to files */
    if (PMIX_SUCCESS != (rc = pmix_dstore_patch_env(proc->nspace, env))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
#endif

    /* get any network contribution */
    if (PMIX_SUCCESS != (rc = pmix_pnet.setup_fork(proc, env))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    return PMIX_SUCCESS;
}

/***************************************************************************************************
 *  Support calls from the host server down to us requesting direct modex data provided by one     *
 *  of our local clients                                                                           *
 ***************************************************************************************************/

static void _dmodex_req(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_rank_info_t *info, *iptr;
    pmix_nspace_t *nptr, *ns;
    pmix_buffer_t pbkt;
    pmix_value_t *val;
    char *data = NULL;
    size_t sz = 0;
    pmix_dmdx_remote_t *dcd;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "DMODX LOOKING FOR %s:%d",
                        cd->proc.nspace, cd->proc.rank);
    /* this should be one of my clients, but a race condition
     * could cause this request to arrive prior to us having
     * been informed of it - so first check to see if we know
     * about this nspace yet */
    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(ns->nspace, cd->proc.nspace)) {
            nptr = ns;
            break;
        }
    }
    if (NULL == nptr) {
        /* we don't know this namespace yet, and so we obviously
         * haven't received the data from this proc yet - defer
         * the request until we do */
        dcd = PMIX_NEW(pmix_dmdx_remote_t);
        PMIX_RETAIN(cd);
        dcd->cd = cd;
        pmix_list_append(&pmix_server_globals.remote_pnd, &dcd->super);
        cd->active = false;  // ensure the request doesn't hang
        return;
    }

    /* They are asking for job level data for this process */
    if (cd->proc.rank == PMIX_RANK_WILDCARD) {

       data = nptr->server->job_info.base_ptr;
       sz = nptr->server->job_info.bytes_used;

       /* execute the callback */
       cd->cbfunc(PMIX_SUCCESS, data, sz, cd->cbdata);
       cd->active = false;

       return;
    }

    /* see if we have this peer in our list */
    info = NULL;
    PMIX_LIST_FOREACH(iptr, &nptr->server->ranks, pmix_rank_info_t) {
        if (iptr->rank == cd->proc.rank) {
            info = iptr;
            break;
        }
    }
    if (NULL == info) {
        /* rank isn't known yet - defer
         * the request until we do */
        dcd = PMIX_NEW(pmix_dmdx_remote_t);
        PMIX_RETAIN(cd);
        dcd->cd = cd;
        pmix_list_append(&pmix_server_globals.remote_pnd, &dcd->super);
        cd->active = false;  // ensure the request doesn't hang
        return;
    }

    /* have we received the modex from this proc yet - if
     * not, then defer */
    if (!info->modex_recvd) {
        /* track the request so we can fulfill it once
         * data is recvd */
        dcd = PMIX_NEW(pmix_dmdx_remote_t);
        PMIX_RETAIN(cd);
        dcd->cd = cd;
        pmix_list_append(&pmix_server_globals.remote_pnd, &dcd->super);
        cd->active = false;  // ensure the request doesn't hang
        return;
    }

    /* collect the remote/global data from this proc */
    PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
    /* get any remote contribution - note that there
     * may not be a contribution */
    if (PMIX_SUCCESS == (rc = pmix_hash_fetch(&nptr->server->myremote, info->rank, "modex", &val)) &&
        NULL != val) {
    data = val->data.bo.bytes;
    sz = val->data.bo.size;
    /* protect the data */
    val->data.bo.bytes = NULL;
    val->data.bo.size = 0;
        PMIX_VALUE_RELEASE(val);
    }

    /* execute the callback */
    cd->cbfunc(rc, data, sz, cd->cbdata);
    if (NULL != data) {
        free(data);
    }
    cd->active = false;
}

PMIX_EXPORT pmix_status_t PMIx_server_dmodex_request(const pmix_proc_t *proc,
                                                     pmix_dmodex_response_fn_t cbfunc,
                                                     void *cbdata)
{
    pmix_setup_caddy_t *cd;

    /* protect against bozo */
    if (NULL == cbfunc || NULL == proc) {
        return PMIX_ERR_BAD_PARAM;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server register client %s:%d",
                        proc->nspace, proc->rank);

    cd = PMIX_NEW(pmix_setup_caddy_t);
    (void)strncpy(cd->proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
    cd->proc.rank = proc->rank;
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _dmodex_req);

    PMIX_WAIT_FOR_COMPLETION(cd->active);
    PMIX_RELEASE(cd);
    return PMIX_SUCCESS;
}

static void _store_internal(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    pmix_nspace_t *ns, *nsptr;

    ns = NULL;
    PMIX_LIST_FOREACH(nsptr, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strncmp(cd->nspace, nsptr->nspace, PMIX_MAX_NSLEN)) {
            ns = nsptr;
            break;
        }
    }
    if (NULL == ns) {
        /* shouldn't be possible */
        cd->status = PMIX_ERR_NOT_FOUND;
    } else {
        cd->status = pmix_hash_store(&ns->internal, cd->rank, cd->kv);
    }
    cd->active = false;
 }

PMIX_EXPORT pmix_status_t PMIx_Store_internal(const pmix_proc_t *proc,
                                              const char *key, pmix_value_t *val)
{
    pmix_shift_caddy_t *cd;
    pmix_status_t rc;

    /* setup to thread shift this request */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    cd->nspace = proc->nspace;
    cd->rank = proc->rank;

    cd->kv = PMIX_NEW(pmix_kval_t);
    cd->kv->key = strdup((char*)key);
    cd->kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
    rc = pmix_value_xfer(cd->kv->value, val);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cd);
        return rc;
    }

    if (PMIX_PROC_SERVER == pmix_globals.proc_type) {
        PMIX_THREADSHIFT(cd, _store_internal);
        PMIX_WAIT_FOR_COMPLETION(cd->active);
    } else {
        _store_internal(0, 0, cd);
    }
    rc = cd->status;
    PMIX_RELEASE(cd);

    return rc;
}

#define PMIX_MAX_NODE_PREFIX        50

PMIX_EXPORT pmix_status_t PMIx_generate_regex(const char *input, char **regexp)
{
    char *vptr, *vsave;
    char prefix[PMIX_MAX_NODE_PREFIX];
    int i, j, len, startnum, vnum, numdigits;
    bool found, fullval;
    char *suffix, *sfx;
    pmix_regex_value_t *vreg;
    pmix_regex_range_t *range;
    pmix_list_t vids;
    char **regexargs = NULL, *tmp, *tmp2;
    char *cptr;

    /* define the default */
    *regexp = NULL;

    /* setup the list of results */
    PMIX_CONSTRUCT(&vids, pmix_list_t);

    /* cycle thru the array of input values - first copy
     * it so we don't overwrite what we were given*/
    vsave = strdup(input);
    vptr = vsave;
    while (NULL != (cptr = strchr(vptr, ',')) || 0 < strlen(vptr)) {
        if (NULL != cptr) {
            *cptr = '\0';
        }
        /* determine this node's prefix by looking for first non-alpha char */
        fullval = false;
        len = strlen(vptr);
        startnum = -1;
        memset(prefix, 0, PMIX_MAX_NODE_PREFIX);
        numdigits = 0;
        for (i=0, j=0; i < len; i++) {
            if (!isalpha(vptr[i])) {
                /* found a non-alpha char */
                if (!isdigit(vptr[i])) {
                    /* if it is anything but a digit, we just use
                     * the entire name
                     */
                    fullval = true;
                    break;
                }
                /* count the size of the numeric field - but don't
                 * add the digits to the prefix
                 */
                numdigits++;
                if (startnum < 0) {
                    /* okay, this defines end of the prefix */
                    startnum = i;
                }
                continue;
            }
            if (startnum < 0) {
                prefix[j++] = vptr[i];
            }
        }
        if (fullval || startnum < 0) {
            /* can't compress this name - just add it to the list */
            vreg = PMIX_NEW(pmix_regex_value_t);
            vreg->prefix = strdup(vptr);
            pmix_list_append(&vids, &vreg->super);
            /* move to the next posn */
            if (NULL == cptr) {
                break;
            }
            vptr = cptr + 1;
            continue;
        }
        /* convert the digits and get any suffix */
        vnum = strtol(&vptr[startnum], &sfx, 10);
        if (NULL != sfx) {
            suffix = strdup(sfx);
        } else {
            suffix = NULL;
        }
        /* is this value already on our list? */
        found = false;
        PMIX_LIST_FOREACH(vreg, &vids, pmix_regex_value_t) {
            if (0 < strlen(prefix) && NULL == vreg->prefix) {
                continue;
            }
            if (0 == strlen(prefix) && NULL != vreg->prefix) {
                continue;
            }
            if (0 < strlen(prefix) && NULL != vreg->prefix
                && 0 != strcmp(prefix, vreg->prefix)) {
                continue;
            }
            if (NULL == suffix && NULL != vreg->suffix) {
                continue;
            }
            if (NULL != suffix && NULL == vreg->suffix) {
                continue;
            }
            if (NULL != suffix && NULL != vreg->suffix &&
                0 != strcmp(suffix, vreg->suffix)) {
                continue;
            }
            if (numdigits != vreg->num_digits) {
                continue;
            }
            /* found a match - flag it */
            found = true;
            /* get the last range on this nodeid - we do this
             * to preserve order
             */
            range = (pmix_regex_range_t*)pmix_list_get_last(&vreg->ranges);
            if (NULL == range) {
                /* first range for this value */
                range = PMIX_NEW(pmix_regex_range_t);
                range->start = vnum;
                range->cnt = 1;
                pmix_list_append(&vreg->ranges, &range->super);
                break;
            }
            /* see if the value is out of sequence */
            if (vnum != (range->start + range->cnt)) {
                /* start a new range */
                range = PMIX_NEW(pmix_regex_range_t);
                range->start = vnum;
                range->cnt = 1;
                pmix_list_append(&vreg->ranges, &range->super);
                break;
            }
            /* everything matches - just increment the cnt */
            range->cnt++;
            break;
        }
        if (!found) {
            /* need to add it */
            vreg = PMIX_NEW(pmix_regex_value_t);
            if (0 < strlen(prefix)) {
                vreg->prefix = strdup(prefix);
            }
            if (NULL != suffix) {
                vreg->suffix = strdup(suffix);
            }
            vreg->num_digits = numdigits;
            pmix_list_append(&vids, &vreg->super);
            /* record the first range for this value - we took
             * care of values we can't compress above
             */
            range = PMIX_NEW(pmix_regex_range_t);
            range->start = vnum;
            range->cnt = 1;
            pmix_list_append(&vreg->ranges, &range->super);
        }
        if (NULL != suffix) {
            free(suffix);
        }
        /* move to the next posn */
        if (NULL == cptr) {
            break;
        }
        vptr = cptr + 1;
    }
    free(vsave);

    /* begin constructing the regular expression */
    while (NULL != (vreg = (pmix_regex_value_t*)pmix_list_remove_first(&vids))) {
        /* if no ranges, then just add the name */
        if (0 == pmix_list_get_size(&vreg->ranges)) {
            if (NULL != vreg->prefix) {
                /* solitary value */
                if (0 > asprintf(&tmp, "%s", vreg->prefix)) {
                    return PMIX_ERR_NOMEM;
                }
                pmix_argv_append_nosize(&regexargs, tmp);
                free(tmp);
            }
            PMIX_RELEASE(vreg);
            continue;
        }
        /* start the regex for this value with the prefix */
        if (NULL != vreg->prefix) {
            if (0 > asprintf(&tmp, "%s[%d:", vreg->prefix, vreg->num_digits)) {
                return PMIX_ERR_NOMEM;
            }
        } else {
            if (0 > asprintf(&tmp, "[%d:", vreg->num_digits)) {
                return PMIX_ERR_NOMEM;
            }
        }
        /* add the ranges */
        while (NULL != (range = (pmix_regex_range_t*)pmix_list_remove_first(&vreg->ranges))) {
            if (1 == range->cnt) {
                if (0 > asprintf(&tmp2, "%s%d,", tmp, range->start)) {
                    return PMIX_ERR_NOMEM;
                }
            } else {
                if (0 > asprintf(&tmp2, "%s%d-%d,", tmp, range->start, range->start + range->cnt - 1)) {
                    return PMIX_ERR_NOMEM;
                }
            }
            free(tmp);
            tmp = tmp2;
            PMIX_RELEASE(range);
        }
        /* replace the final comma */
        tmp[strlen(tmp)-1] = ']';
        if (NULL != vreg->suffix) {
            /* add in the suffix, if provided */
            if (0 > asprintf(&tmp2, "%s%s", tmp, vreg->suffix)) {
                return PMIX_ERR_NOMEM;
            }
            free(tmp);
            tmp = tmp2;
        }
        pmix_argv_append_nosize(&regexargs, tmp);
        free(tmp);
        PMIX_RELEASE(vreg);
    }

    /* assemble final result */
    tmp = pmix_argv_join(regexargs, ',');
    if (0 > asprintf(regexp, "pmix[%s]", tmp)) {
        return PMIX_ERR_NOMEM;
    }
    free(tmp);

    /* cleanup */
    pmix_argv_free(regexargs);

    PMIX_DESTRUCT(&vids);
    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_generate_ppn(const char *input, char **regexp)
{
    char **ppn, **npn;
    int i, j, start, end;
    pmix_regex_value_t *vreg;
    pmix_regex_range_t *rng;
    pmix_list_t nodes;
    char *tmp, *tmp2;
    char *cptr;

    /* define the default */
    *regexp = NULL;

    /* setup the list of results */
    PMIX_CONSTRUCT(&nodes, pmix_list_t);

    /* split the input by node */
    ppn = pmix_argv_split(input, ';');

    /* for each node, split the input by comma */
    for (i=0; NULL != ppn[i]; i++) {
        rng = NULL;
        /* create a record for this node */
        vreg = PMIX_NEW(pmix_regex_value_t);
        pmix_list_append(&nodes, &vreg->super);
        /* split the input for this node */
        npn = pmix_argv_split(ppn[i], ',');
        /* look at each element */
        for (j=0; NULL != npn[j]; j++) {
            /* is this a range? */
            if (NULL != (cptr = strchr(npn[j], '-'))) {
                /* terminate the string */
                *cptr = '\0';
                ++cptr;
                start = strtol(npn[j], NULL, 10);
                end = strtol(cptr, NULL, 10);
                /* are we collecting a range? */
                if (NULL == rng) {
                    /* no - better start one */
                    rng = PMIX_NEW(pmix_regex_range_t);
                    rng->start = start;
                    rng->cnt = end - start + 1;
                    pmix_list_append(&vreg->ranges, &rng->super);
                } else {
                    /* is this a continuation of the current range? */
                    if (start == (rng->start + rng->cnt)) {
                        /* just add it to the end of this range */
                        rng->cnt++;
                    } else {
                        /* nope, there is a break - create new range */
                        rng = PMIX_NEW(pmix_regex_range_t);
                        rng->start = start;
                        rng->cnt = end - start + 1;
                        pmix_list_append(&vreg->ranges, &rng->super);
                    }
                }
            } else {
                /* single rank given */
                start = strtol(npn[j], NULL, 10);
                /* are we collecting a range? */
                if (NULL == rng) {
                    /* no - better start one */
                    rng = PMIX_NEW(pmix_regex_range_t);
                    rng->start = start;
                    rng->cnt = 1;
                    pmix_list_append(&vreg->ranges, &rng->super);
                } else {
                    /* is this a continuation of the current range? */
                    if (start == (rng->start + rng->cnt)) {
                        /* just add it to the end of this range */
                        rng->cnt++;
                    } else {
                        /* nope, there is a break - create new range */
                        rng = PMIX_NEW(pmix_regex_range_t);
                        rng->start = start;
                        rng->cnt = 1;
                        pmix_list_append(&vreg->ranges, &rng->super);
                    }
                }
            }
        }
        pmix_argv_free(npn);
    }
    pmix_argv_free(ppn);


    /* begin constructing the regular expression */
    tmp = strdup("pmix[");
    PMIX_LIST_FOREACH(vreg, &nodes, pmix_regex_value_t) {
        while (NULL != (rng = (pmix_regex_range_t*)pmix_list_remove_first(&vreg->ranges))) {
            if (1 == rng->cnt) {
                if (0 > asprintf(&tmp2, "%s%d,", tmp, rng->start)) {
                    return PMIX_ERR_NOMEM;
                }
            } else {
                if (0 > asprintf(&tmp2, "%s%d-%d,", tmp, rng->start, rng->start + rng->cnt - 1)) {
                    return PMIX_ERR_NOMEM;
                }
            }
            free(tmp);
            tmp = tmp2;
            PMIX_RELEASE(rng);
        }
        /* replace the final comma */
        tmp[strlen(tmp)-1] = ';';
    }

    /* replace the final semi-colon */
    tmp[strlen(tmp)-1] = ']';

    /* assemble final result */
    *regexp = tmp;

    PMIX_LIST_DESTRUCT(&nodes);
    return PMIX_SUCCESS;
}

static void _setup_op(pmix_status_t rc, void *cbdata)
{
    pmix_setup_caddy_t *fcd = (pmix_setup_caddy_t*)cbdata;

    if (NULL != fcd->info) {
        PMIX_INFO_FREE(fcd->info, fcd->ninfo);
    }
    PMIX_RELEASE(fcd);
}

static void _setup_app(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_setup_caddy_t *fcd = NULL;
    pmix_status_t rc;
    pmix_list_t ilist;
    pmix_kval_t *kv;
    size_t n;

    PMIX_CONSTRUCT(&ilist, pmix_list_t);

    /* pass to the network libraries */
    if (PMIX_SUCCESS != (rc = pmix_pnet.setup_app(cd->nspace, &ilist))) {
        goto depart;
    }

    /* setup the return callback */
    fcd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == fcd) {
        rc = PMIX_ERR_NOMEM;
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        goto depart;
    }

    /* if anything came back, construct the info array */
    if (0 < (fcd->ninfo = pmix_list_get_size(&ilist))) {
        PMIX_INFO_CREATE(fcd->info, fcd->ninfo);
        n = 0;
        PMIX_LIST_FOREACH(kv, &ilist, pmix_kval_t) {
            (void)strncpy(fcd->info[n].key, kv->key, PMIX_MAX_KEYLEN);
            if (PMIX_SUCCESS != (rc = pmix_value_xfer(&fcd->info[n].value, kv->value))) {
                PMIX_INFO_FREE(fcd->info, fcd->ninfo);
                PMIX_RELEASE(fcd);
                fcd = NULL;
                goto depart;
            }
        }
    }

  depart:
    /* always execute the callback to avoid hanging */
    if (NULL != cd->setupcbfunc) {
        if (NULL == fcd) {
            cd->setupcbfunc(rc, NULL, 0, cd->cbdata, NULL, NULL);
        } else {
            cd->setupcbfunc(rc, fcd->info, fcd->ninfo, cd->cbdata, _setup_op, fcd);
        }
    }

    /* cleanup memory */
    PMIX_LIST_DESTRUCT(&ilist);
    if (NULL != cd->nspace) {
        free(cd->nspace);
    }
    PMIX_RELEASE(cd);
}

pmix_status_t PMIx_server_setup_application(const char nspace[],
                                            pmix_info_t info[], size_t ninfo,
                                            pmix_setup_application_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    /* need to threadshift this request */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    if (NULL != nspace) {
        cd->nspace = strdup(nspace);
    }
    cd->info = info;
    cd->ninfo = ninfo;
    cd->setupcbfunc = cbfunc;
    cd->cbdata = cbdata;
    PMIX_THREADSHIFT(cd, _setup_app);

    return PMIX_SUCCESS;
}

static void _setup_local_support(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_status_t rc;

    /* pass to the network libraries */
    rc = pmix_pnet.setup_local_network(cd->nspace, cd->info, cd->ninfo);

    /* pass the info back */
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(rc, cd->cbdata);
    }
    /* cleanup memory */
    if (NULL != cd->nspace) {
        free(cd->nspace);
    }
    PMIX_RELEASE(cd);
}

pmix_status_t PMIx_server_setup_local_support(const char nspace[],
                                              pmix_info_t info[], size_t ninfo,
                                              pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    /* need to threadshift this request */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    if (NULL != nspace) {
        cd->nspace = strdup(nspace);
    }
    cd->info = info;
    cd->ninfo = ninfo;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;
    PMIX_THREADSHIFT(cd, _setup_local_support);

    return PMIX_SUCCESS;
}


/****    THE FOLLOWING CALLBACK FUNCTIONS ARE USED BY THE HOST SERVER    ****
 ****    THEY THEREFORE CAN OCCUR IN EITHER THE HOST SERVER'S THREAD     ****
 ****    CONTEXT, OR IN OUR OWN THREAD CONTEXT IF THE CALLBACK OCCURS    ****
 ****    IMMEDIATELY. THUS ANYTHING THAT ACCESSES A GLOBAL ENTITY        ****
 ****    MUST BE PUSHED INTO AN EVENT FOR PROTECTION                     ****/

static void op_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    /* no need to thread-shift here as no global data is
     * being accessed */

    /* setup the reply with the returned status */
    reply = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &status, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        PMIX_RELEASE(cd);
        return;
    }

    /* the function that created the server_caddy did a
     * retain on the peer, so we don't have to worry about
     * it still being present - send a copy to the originator */
    if (PMIX_SUCCESS != (rc = pmix_ptl.send_oneway(cd->peer, reply, cd->hdr.tag))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
    }

    /* cleanup */
    PMIX_RELEASE(cd);
}

static void _spcb(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    pmix_nspace_t *nptr, *ns;
    pmix_buffer_t *reply;
    pmix_status_t rc;
    char          *msg;

    /* setup the reply with the returned status */
    reply = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &cd->status, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cd->cd);
        cd->active = false;
        return;
    }
    if (PMIX_SUCCESS == cd->status) {
        /* add any job-related info we have on that nspace - this will
         * include the name of the nspace */
        nptr = NULL;
        PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_nspace_t) {
            if (0 == strcmp(ns->nspace, cd->nspace)) {
                nptr = ns;
                break;
            }
        }
        if (NULL == nptr) {
            /* This can happen if there are no processes from this
             * namespace running on this host.  In this case just
             * pack the name of the namespace because we need that. */
            msg = (char*)cd->nspace;
            pmix_bfrop.pack(reply, &msg, 1, PMIX_STRING);
        } else {
            pmix_bfrop.copy_payload(reply, &nptr->server->job_info);
        }
    }

    /* the function that created the server_caddy did a
     * retain on the peer, so we don't have to worry about
     * it still being present - tell the originator the result */
    PMIX_SERVER_QUEUE_REPLY(cd->cd->peer, cd->cd->hdr.tag, reply);
    /* cleanup */
    PMIX_RELEASE(cd->cd);
    cd->active = false;
}

static void spawn_cbfunc(pmix_status_t status, char *nspace, void *cbdata)
{
    pmix_shift_caddy_t *cd;

    /* need to thread-shift this request */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    cd->status = status;
    cd->nspace = nspace;
    cd->cd = (pmix_server_caddy_t*)cbdata;;

    PMIX_THREADSHIFT(cd, _spcb);
    PMIX_WAIT_FOR_COMPLETION(cd->active);
    PMIX_RELEASE(cd);
}

static void lookup_cbfunc(pmix_status_t status, pmix_pdata_t pdata[], size_t ndata,
                          void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    /* no need to thread-shift as no global data is accessed */

    /* setup the reply with the returned status */
    reply = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &status, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        return;
    }
    if (PMIX_SUCCESS == status) {
        /* pack the returned data objects */
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &ndata, 1, PMIX_SIZE))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(reply);
            return;
        }
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, pdata, ndata, PMIX_PDATA))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(reply);
            return;
        }
    }

    /* the function that created the server_caddy did a
     * retain on the peer, so we don't have to worry about
     * it still being present - tell the originator the result */
    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    /* cleanup */
    PMIX_RELEASE(cd);
}

static void _mdxcbfunc(int sd, short argc, void *cbdata)
{
    pmix_shift_caddy_t *scd = (pmix_shift_caddy_t*)cbdata;
    pmix_server_trkr_t *tracker = scd->tracker;
    pmix_buffer_t xfer, *bptr, *databuf=NULL, *bpscope, *reply;
    pmix_nspace_t *nptr, *ns;
    pmix_server_caddy_t *cd;
    char *nspace;
    int rank;
    pmix_status_t rc = PMIX_SUCCESS;
    int32_t cnt = 1;
    char byte;

    /* pass the blobs being returned */
    PMIX_CONSTRUCT(&xfer, pmix_buffer_t);

    if (PMIX_SUCCESS != scd->status) {
        rc = scd->status;
        goto finish_collective;
    }

    if (PMIX_COLLECT_INVALID == tracker->collect_type) {
        rc = PMIX_ERR_INVALID_ARG;
        goto finish_collective;
    }

    PMIX_LOAD_BUFFER(&xfer, scd->data, scd->ndata);

    /* if data was returned, unpack and store it */
    while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(&xfer, &byte, &cnt, PMIX_BYTE))) {
        pmix_collect_t ctype = (pmix_collect_t)byte;

        // Check that this blob was accumulated with the same data collection setting
        if (ctype != tracker->collect_type) {
            rc = PMIX_ERR_INVALID_ARG;
            goto finish_collective;
        }

        // Skip the rest of the iteration if there is no data
        if (PMIX_COLLECT_YES != tracker->collect_type) {
            continue;
        }

        // Extract the node-wise blob containing rank data
        cnt = 1;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(&xfer, &databuf, &cnt, PMIX_BUFFER))) {
            rc = PMIX_ERR_DATA_VALUE_NOT_FOUND;
            goto finish_collective;
        }

        // Loop over rank blobs
        cnt = 1;
        while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(databuf, &bptr, &cnt, PMIX_BUFFER))) {
            /* unpack the nspace */
            cnt = 1;
            if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(bptr, &nspace, &cnt, PMIX_STRING))) {
                PMIX_ERROR_LOG(rc);
                goto finish_collective;
            }
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "server:modex_cbfunc unpacked blob for npsace %s", nspace);
            /* find the nspace object */
            nptr = NULL;
            PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_nspace_t) {
                if (0 == strcmp(nspace, ns->nspace)) {
                    nptr = ns;
                    break;
                }
            }

            if (NULL == nptr) {
                /* Shouldn't happen. The Fence is performed among well-known
                 * set of processes in known namespaces. Consider this as
                 * unrecoverable fault.
                 */
                pmix_output_verbose(8, pmix_globals.debug_output,
                                    "modex_cbfunc: unknown nspace %s, Fence ", nspace);
                free(nspace);
                /*
                 * TODO: if some namespaces are OK and the bad one is not the first
                 * the server is in inconsistent state. Should we rely on the client to abort
                 * computation or this is our task?
                 */
                rc = PMIX_ERR_INVALID_NAMESPACE;
                goto finish_collective;
            }
            free(nspace);

            /* unpack the rank */
            cnt = 1;
            if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(bptr, &rank, &cnt, PMIX_PROC_RANK))) {
                PMIX_ERROR_LOG(rc);
                goto finish_collective;
            }
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "client:unpack fence received blob for rank %d", rank);
            /* there may be multiple blobs for this rank, each from a different scope */
            cnt = 1;
            while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(bptr, &bpscope, &cnt, PMIX_BUFFER))) {
                /* don't store blobs to the sm dstore from local clients */
                if (_my_client(nptr->nspace, rank)) {
                    continue;
                }
                pmix_kval_t *kp = PMIX_NEW(pmix_kval_t);
                kp->key = strdup("modex");
                PMIX_VALUE_CREATE(kp->value, 1);
                kp->value->type = PMIX_BYTE_OBJECT;
                PMIX_UNLOAD_BUFFER(bpscope, kp->value->data.bo.bytes, kp->value->data.bo.size);
                /* store it in the appropriate hash */
                if (PMIX_SUCCESS != (rc = pmix_hash_store(&nptr->server->remote, rank, kp))) {
                    PMIX_ERROR_LOG(rc);
                }
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
                if (PMIX_SUCCESS != (rc = pmix_dstore_store(nptr->nspace, rank, kp))) {
                    PMIX_ERROR_LOG(rc);
                }
#endif /* PMIX_ENABLE_DSTORE */
                PMIX_RELEASE(kp);  // maintain acctg
            }  // while bpscope
            if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
                PMIX_ERROR_LOG(rc);
                /*
                 * TODO: if some buffers are OK and the bad one is not the first
                 * the server is in inconsistent state. Should we rely on the client to abort
                 * computation or this is our task?
                 */
                goto finish_collective;
            }
            PMIX_RELEASE(bpscope);
            PMIX_RELEASE(bptr);
            cnt = 1;
        }
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            goto finish_collective;
        } else {
            rc = PMIX_SUCCESS;
        }
        cnt = 1;
    } // while bptr

    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
        rc = PMIX_SUCCESS;
    }

  finish_collective:
    if(NULL != databuf) {
        PMIX_RELEASE(databuf);
    }
    /* setup the reply, starting with the returned status */
    reply = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &rc, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* loop across all procs in the tracker, sending them the reply */
    PMIX_LIST_FOREACH(cd, &tracker->local_cbs, pmix_server_caddy_t) {
        PMIX_RETAIN(reply);
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "server:modex_cbfunc reply being sent to %s:%d",
                            cd->peer->info->nptr->nspace, cd->peer->info->rank);
        PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    }

  cleanup:
    /* Protect data from being free'd because RM pass
     * the pointer that is set to the middle of some
     * buffer (the case with SLURM).
     * RM is responsible on the release of the buffer
     */
    xfer.base_ptr = NULL;
    xfer.bytes_used = 0;
    PMIX_DESTRUCT(&xfer);

    PMIX_RELEASE(reply);  // maintain accounting
    pmix_list_remove_item(&pmix_server_globals.collectives, &tracker->super);
    PMIX_RELEASE(tracker);

    /* we are done */
    if (NULL != scd->cbfunc.relfn) {
        scd->cbfunc.relfn(scd->cbdata);
    }
    PMIX_RELEASE(scd);
}
static void modex_cbfunc(pmix_status_t status, const char *data, size_t ndata, void *cbdata,
                         pmix_release_cbfunc_t relfn, void *relcbd)
{
    pmix_server_trkr_t *tracker = (pmix_server_trkr_t*)cbdata;
    pmix_shift_caddy_t *scd;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:modex_cbfunc called with %d bytes", (int)ndata);

    if (NULL == tracker) {
        /* nothing to do - but be sure to give them
         * a release if they want it */
        if (NULL != relfn) {
            relfn(relcbd);
        }
        return;
    }

    /* need to thread-shift this callback as it accesses global data */
    scd = PMIX_NEW(pmix_shift_caddy_t);
    scd->status = status;
    scd->data = data;
    scd->ndata = ndata;
    scd->tracker = tracker;
    scd->cbfunc.relfn = relfn;
    scd->cbdata = relcbd;
    PMIX_THREADSHIFT(scd, _mdxcbfunc);
}

static void get_cbfunc(pmix_status_t status, const char *data, size_t ndata, void *cbdata,
                       pmix_release_cbfunc_t relfn, void *relcbd)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)cbdata;
    pmix_buffer_t *reply, buf;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:get_cbfunc called with %d elements", (int)ndata);

    /* no need to thread-shift here as no global data is accessed */

    if (NULL == cd) {
        /* nothing to do - but be sure to give them
         * a release if they want it */
        if (NULL != relfn) {
            relfn(relcbd);
        }
        return;
    }

    /* setup the reply, starting with the returned status */
    reply = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &status, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    /* pack the blob being returned */
    PMIX_CONSTRUCT(&buf, pmix_buffer_t);
    PMIX_LOAD_BUFFER(&buf, data, ndata);
    pmix_bfrop.copy_payload(reply, &buf);
    buf.base_ptr = NULL;
    buf.bytes_used = 0;
    PMIX_DESTRUCT(&buf);
    /* send the data to the requestor */
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:get_cbfunc reply being sent to %s:%d",
                        cd->peer->info->nptr->nspace, cd->peer->info->rank);
    pmix_output_hexdump(5, pmix_globals.debug_output,
            reply->base_ptr, (reply->bytes_used < 256 ? reply->bytes_used : 256));

    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);

 cleanup:
    /* if someone wants a release, give it to them */
    if (NULL != relfn) {
        relfn(relcbd);
    }
    PMIX_RELEASE(cd);
}

static void _cnct(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *scd = (pmix_shift_caddy_t*)cbdata;
    pmix_server_trkr_t *tracker = scd->tracker;
    pmix_buffer_t *reply;
    pmix_status_t rc;
    int i;
    pmix_server_caddy_t *cd;
    char **nspaces=NULL;
    pmix_nspace_t *nptr;
    pmix_buffer_t *job_info_ptr;

    /* setup the reply, starting with the returned status */
    reply = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &scd->status, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    if (PMIX_CONNECTNB_CMD == tracker->type) {
        /* find the unique nspaces that are participating */
        PMIX_LIST_FOREACH(cd, &tracker->local_cbs, pmix_server_caddy_t) {
            pmix_argv_append_unique_nosize(&nspaces, cd->peer->info->nptr->nspace, false);
        }

        /* loop across all participating nspaces and include their
         * job-related info */
        for (i=0; NULL != nspaces[i]; i++) {
            PMIX_LIST_FOREACH(nptr, &pmix_globals.nspaces, pmix_nspace_t) {
                if (0 != strcmp(nspaces[i], nptr->nspace)) {
                    continue;
                }
                job_info_ptr = &nptr->server->job_info;
                if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &job_info_ptr, 1, PMIX_BUFFER))) {
                    PMIX_ERROR_LOG(rc);
                    pmix_argv_free(nspaces);
                    goto cleanup;
                }
            }
        }
        pmix_argv_free(nspaces);
    }

    /* loop across all procs in the tracker, sending them the reply */
    PMIX_LIST_FOREACH(cd, &tracker->local_cbs, pmix_server_caddy_t) {
        PMIX_RETAIN(reply);
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "server:cnct_cbfunc reply being sent to %s:%d",
                            cd->peer->info->nptr->nspace, cd->peer->info->rank);
        PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    }

  cleanup:
    PMIX_RELEASE(reply);  // maintain accounting
    pmix_list_remove_item(&pmix_server_globals.collectives, &tracker->super);
    PMIX_RELEASE(tracker);

    /* we are done */
    PMIX_RELEASE(scd);
}

static void cnct_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_server_trkr_t *tracker = (pmix_server_trkr_t*)cbdata;
    pmix_shift_caddy_t *scd;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:cnct_cbfunc called");

    if (NULL == tracker) {
        /* nothing to do */
        return;
    }

    /* need to thread-shift this callback as it accesses global data */
    scd = PMIX_NEW(pmix_shift_caddy_t);
    scd->status = status;
    scd->tracker = tracker;
    PMIX_THREADSHIFT(scd, _cnct);
}

static void regevents_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_status_t rc;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*) cbdata;
    pmix_buffer_t *reply;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:regevents_cbfunc called status = %d", status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &status, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
    }
    // send reply
    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    PMIX_RELEASE(cd);
}

static void notifyerror_cbfunc (pmix_status_t status, void *cbdata)
{
    pmix_status_t rc;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*) cbdata;
    pmix_buffer_t *reply = PMIX_NEW(pmix_buffer_t);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:notifyerror_cbfunc called status = %d", status);

    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &status, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
    }
    // send reply
    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    PMIX_RELEASE(cd);
}


static void query_cbfunc(pmix_status_t status,
                         pmix_info_t *info, size_t ninfo,
                         void *cbdata,
                         pmix_release_cbfunc_t release_fn,
                         void *release_cbdata)
{
    pmix_query_caddy_t *qcd = (pmix_query_caddy_t*)cbdata;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)qcd->cbdata;
    pmix_buffer_t *reply = PMIX_NEW(pmix_buffer_t);
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query callback with status %d", status);

    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &status, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    /* pack the returned data */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &ninfo, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (0 < ninfo) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, info, ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
        }
    }

  complete:
    // send reply
    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    // cleanup
    if (NULL != qcd->queries) {
        PMIX_QUERY_FREE(qcd->queries, qcd->nqueries);
    }
    if (NULL != qcd->info) {
        PMIX_INFO_FREE(qcd->info, qcd->ninfo);
    }
    PMIX_RELEASE(qcd);
    PMIX_RELEASE(cd);
}

/* the switchyard is the primary message handling function. It's purpose
 * is to take incoming commands (packed into a buffer), unpack them,
 * and then call the corresponding host server's function to execute
 * them. Some commands involve only a single proc (i.e., the one
 * sending the command) and can be executed while we wait. In these cases,
 * the switchyard will construct and pack a reply buffer to be returned
 * to the sender.
 *
 * Other cases (either multi-process collective or cmds that require
 * an async reply) cannot generate an immediate reply. In these cases,
 * the reply buffer will be NULL. An appropriate callback function will
 * be called that will be responsible for eventually replying to the
 * calling processes.
 *
 * Should an error be encountered at any time within the switchyard, an
 * error reply buffer will be returned so that the caller can be notified,
 * thereby preventing the process from hanging. */
static pmix_status_t server_switchyard(pmix_peer_t *peer, uint32_t tag,
                                       pmix_buffer_t *buf)
{
    pmix_status_t rc=PMIX_ERR_NOT_SUPPORTED;
    int32_t cnt;
    pmix_cmd_t cmd;
    pmix_server_caddy_t *cd;
    pmix_proc_t proc;
    pmix_buffer_t *reply;
    pmix_regevents_info_t *reginfo;
    pmix_peer_events_info_t *prev;

    /* retrieve the cmd */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &cmd, &cnt, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd pmix cmd %d from %s:%d",
                        cmd, peer->info->nptr->nspace, peer->info->rank);

    if (PMIX_REQ_CMD == cmd) {
        reply = PMIX_NEW(pmix_buffer_t);

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
        char *msg = peer->info->nptr->nspace;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &msg, 1, PMIX_STRING))) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
#else
        pmix_bfrop.copy_payload(reply, &(peer->info->nptr->server->job_info));
        pmix_bfrop.copy_payload(reply, &(pmix_server_globals.gdata));
#endif
        PMIX_SERVER_QUEUE_REPLY(peer, tag, reply);
        return PMIX_SUCCESS; // don't reply twice
    }

    if (PMIX_ABORT_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_abort(peer, buf, op_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_COMMIT_CMD == cmd) {
        rc = pmix_server_commit(peer, buf);
        reply = PMIX_NEW(pmix_buffer_t);
        pmix_bfrop.pack(reply, &rc, 1, PMIX_STATUS);
        PMIX_SERVER_QUEUE_REPLY(peer, tag, reply);
        return PMIX_SUCCESS; // don't reply twice
    }

    if (PMIX_FENCENB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_fence(cd, buf, modex_cbfunc, op_cbfunc))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_GETNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_get(buf, get_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_FINALIZE_CMD == cmd) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "recvd FINALIZE");
        /* call the local server, if supported */
        if (NULL != pmix_host_server.client_finalized) {
            PMIX_PEER_CADDY(cd, peer, tag);
            (void)strncpy(proc.nspace, peer->info->nptr->nspace, PMIX_MAX_NSLEN);
            proc.rank = peer->info->rank;
            /* since the client is finalizing, remove them from any event
             * registrations they may still have on our list */
            PMIX_LIST_FOREACH(reginfo, &pmix_server_globals.events, pmix_regevents_info_t) {
                PMIX_LIST_FOREACH(prev, &reginfo->peers, pmix_peer_events_info_t) {
                    if (prev->peer == peer) {
                        pmix_list_remove_item(&reginfo->peers, &prev->super);
                        PMIX_RELEASE(prev);
                        break;
                    }
                }
            }
            /* now tell the host server */
            if (PMIX_SUCCESS != (rc = pmix_host_server.client_finalized(&proc, peer->info->server_object,
                                                                        op_cbfunc, cd))) {
                PMIX_RELEASE(cd);
            } else {
                /* don't reply to them ourselves - we will do so when the host
                 * server calls us back */
                return rc;
            }
        }
        /* turn off the recv event - we shouldn't hear anything
         * more from this proc */
        if (peer->recv_ev_active) {
            pmix_event_del(&peer->recv_event);
            peer->recv_ev_active = false;
        }
        /* let the network libraries cleanup */
        pmix_pnet.child_finalized(peer);
        return rc;
    }


    if (PMIX_PUBLISHNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_publish(peer, buf, op_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }


    if (PMIX_LOOKUPNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_lookup(peer, buf, lookup_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }


    if (PMIX_UNPUBLISHNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_unpublish(peer, buf, op_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }


    if (PMIX_SPAWNNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_spawn(peer, buf, spawn_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }


    if (PMIX_CONNECTNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        rc = pmix_server_connect(cd, buf, false, cnct_cbfunc);
        PMIX_RELEASE(cd);
        return rc;
    }

    if (PMIX_DISCONNECTNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        rc = pmix_server_connect(cd, buf, true, cnct_cbfunc);
        PMIX_RELEASE(cd);
        return rc;
    }

    if (PMIX_REGEVENTS_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_register_events(peer, buf, regevents_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_DEREGEVENTS_CMD == cmd) {
        pmix_server_deregister_events(peer, buf);
        return PMIX_SUCCESS;
    }

    if (PMIX_NOTIFY_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        rc = pmix_server_event_recvd_from_client(peer, buf, notifyerror_cbfunc, cd);
        return rc;
    }

    if (PMIX_QUERY_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        rc = pmix_server_query(peer, buf, query_cbfunc, cd);
        return rc;
    }

    if (PMIX_LOG_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        rc = pmix_server_log(peer, buf, op_cbfunc, cd);
        return rc;
    }

    if (PMIX_ALLOC_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        rc = pmix_server_alloc(peer, buf, query_cbfunc, cd);
        return rc;
    }

    if (PMIX_JOB_CONTROL_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        rc = pmix_server_job_ctrl(peer, buf, query_cbfunc, cd);
        return rc;
    }

    if (PMIX_MONITOR_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        rc = pmix_server_monitor(peer, buf, query_cbfunc, cd);
        return rc;
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

static void server_message_handler(struct pmix_peer_t *pr,
                                   pmix_ptl_hdr_t *hdr,
                                   pmix_buffer_t *buf, void *cbdata)
{
    pmix_peer_t *peer = (pmix_peer_t*)pr;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "SWITCHYARD for %s:%d:%d",
                        peer->info->nptr->nspace,
                        peer->info->rank, peer->sd);

    rc = server_switchyard(peer, hdr->tag, buf);
    /* send the return, if there was an error returned */
    if (PMIX_SUCCESS != rc) {
        reply = PMIX_NEW(pmix_buffer_t);
        pmix_bfrop.pack(reply, &rc, 1, PMIX_STATUS);
        PMIX_SERVER_QUEUE_REPLY(peer, hdr->tag, reply);
    }
}

static inline int _my_client(const char *nspace, pmix_rank_t rank)
{
    pmix_peer_t *peer;
    int i;
    int local = 0;

    for (i = 0; i < pmix_server_globals.clients.size; i++) {
        if (NULL != (peer = (pmix_peer_t *)pmix_pointer_array_get_item(&pmix_server_globals.clients, i))) {
            if (0 == strcmp(peer->info->nptr->nspace, nspace) && peer->info->rank == rank) {
                local = 1;
                break;
            }
        }
    }

    return local;
}
