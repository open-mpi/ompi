/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>
#include <private/pmix_stdint.h>
#include <private/pmix_socket_errno.h>

#include <pmix_server.h>
#include <pmix/pmix_common.h>
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

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"
#include "src/util/progress_threads.h"
#include "src/usock/usock.h"
#include "src/sec/pmix_sec.h"

#include "pmix_server_ops.h"

// global variables
pmix_server_globals_t pmix_server_globals = {{{0}}};

// local variables
static char *myuri = NULL;
static struct sockaddr_un myaddress;
static char *security_mode = NULL;

// local functions for connection support
static void server_message_handler(struct pmix_peer_t *pr, pmix_usock_hdr_t *hdr,
                                   pmix_buffer_t *buf, void *cbdata);

typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    pmix_peer_t *peer;
    pmix_buffer_t *buf;
    uint32_t tag;
} pmix_usock_queue_t;
PMIX_CLASS_INSTANCE(pmix_usock_queue_t,
                   pmix_object_t,
                   NULL, NULL);

/* define a caddy for thread-shifting operations when
 * the host server executes a callback to us */
 typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    volatile bool active;
    pmix_status_t status;
    const char *nspace;
    int rank;
    const char *data;
    size_t ndata;
    const char *key;
    pmix_info_t *info;
    size_t ninfo;
    pmix_notification_fn_t err;
    pmix_kval_t *kv;
    pmix_value_t *vptr;
    pmix_server_caddy_t *cd;
    pmix_server_trkr_t *tracker;
    union {
       pmix_release_cbfunc_t relfn;
       pmix_errhandler_reg_cbfunc_t errregcbfn;
       pmix_op_cbfunc_t opcbfn;
    }cbfunc;
    void *cbdata;
    int ref;
 } pmix_shift_caddy_t;
static void scon(pmix_shift_caddy_t *p)
{
    p->active = false;
    p->kv = NULL;
    p->cbfunc.relfn = NULL;
    p->cbfunc.errregcbfn = NULL;
    p->cbfunc.opcbfn = NULL;
    p->cbdata = NULL;
}
static void scdes(pmix_shift_caddy_t *p)
{
    if (NULL != p->kv) {
        PMIX_RELEASE(p->kv);
    }
}
PMIX_CLASS_INSTANCE(pmix_shift_caddy_t,
                    pmix_object_t,
                    scon, scdes);


 #define PMIX_THREADSHIFT(r, c)                       \
 do {                                                 \
    (r)->active = true;                               \
    event_assign(&((r)->ev), pmix_globals.evbase,     \
                 -1, EV_WRITE, (c), (r));             \
    event_priority_set(&((r)->ev), 0);                \
    event_active(&((r)->ev), EV_WRITE, 1);            \
} while(0);


/* queue a message to be sent to one of our procs - must
 * provide the following params:
 *
 * p - the peer object of the process
 * t - tag to be sent to
 * b - buffer to be sent
 */
static void _queue_message(int fd, short args, void *cbdata)
{
    pmix_usock_queue_t *queue = (pmix_usock_queue_t*)cbdata;
    pmix_usock_send_t *snd;
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "[%s:%d] queue callback called: reply to %s:%d on tag %d",
                        __FILE__, __LINE__,
                        (queue->peer)->info->nptr->nspace,
                        (queue->peer)->info->rank, (queue->tag));
    snd = PMIX_NEW(pmix_usock_send_t);
    snd->hdr.pindex = pmix_globals.pindex;
    snd->hdr.tag = (queue->tag);
    snd->hdr.nbytes = (queue->buf)->bytes_used;
    snd->data = (queue->buf);
    /* always start with the header */
    snd->sdptr = (char*)&snd->hdr;
    snd->sdbytes = sizeof(pmix_usock_hdr_t);

    /* if there is no message on-deck, put this one there */
    if (NULL == (queue->peer)->send_msg) {
        (queue->peer)->send_msg = snd;
    } else {
        /* add it to the queue */
        pmix_list_append(&(queue->peer)->send_queue, &snd->super);
    }
    /* ensure the send event is active */
    if (!(queue->peer)->send_ev_active) {
        event_add(&(queue->peer)->send_event, 0);
        (queue->peer)->send_ev_active = true;
    }
    PMIX_RELEASE(queue);
}

#define PMIX_SERVER_QUEUE_REPLY(p, t, b)                                \
    do {                                                                \
        pmix_usock_queue_t *queue;                                      \
        queue = PMIX_NEW(pmix_usock_queue_t);                           \
        queue->peer = (p);                                              \
        queue->buf  = (b);                                              \
        queue->tag  = (t);                                              \
        pmix_output_verbose(2, pmix_globals.debug_output,               \
                        "[%s:%d] queue reply to %s:%d on tag %d",       \
                        __FILE__, __LINE__,                             \
                        (queue->peer)->info->nptr->nspace,              \
                        (queue->peer)->info->rank, (queue->tag));       \
        event_assign(&queue->ev, pmix_globals.evbase, -1,               \
                       EV_WRITE, _queue_message, queue);                \
        event_priority_set(&queue->ev, 0);                              \
        event_active(&queue->ev, EV_WRITE, 1);                          \
    } while(0);


static pmix_status_t initialize_server_base(pmix_server_module_t *module)
{
    int debug_level;
    char *tdir, *evar;
    pid_t pid;

    /* initialize the output system */
    if (!pmix_output_init()) {
        return PMIX_ERR_INIT;
    }
    /* setup the globals */
    pmix_globals_init();
    memset(&pmix_server_globals, 0, sizeof(pmix_server_globals));
    pmix_server_globals.listen_socket = -1;

    /* mark that I am a server */
    pmix_globals.server = true;

    /* look for our namespace, if one was given */
    if (NULL == (evar = getenv("PMIX_SERVER_NAMESPACE"))) {
        /* use a fake namespace */
        (void)strncpy(pmix_globals.myid.nspace, "pmix-server", PMIX_MAX_NSLEN);
    } else {
        (void)strncpy(pmix_globals.myid.nspace, evar, PMIX_MAX_NSLEN);
    }
    /* look for our rank, if one was given */
    pid = getpid();
    if (NULL == (evar = getenv("PMIX_SERVER_RANK"))) {
        /* use our pid */
        pmix_globals.myid.rank = pid;
    } else {
        pmix_globals.myid.rank = strtol(evar, NULL, 10);
    }

    /* initialize the datatype support */
    pmix_bfrop_open();

    /* setup the server-specific globals */
    PMIX_CONSTRUCT(&pmix_server_globals.clients, pmix_pointer_array_t);
    pmix_pointer_array_init(&pmix_server_globals.clients, 1, INT_MAX, 1);
    PMIX_CONSTRUCT(&pmix_server_globals.collectives, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.remote_pnd, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.local_reqs, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.client_eventregs, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.gdata, pmix_buffer_t);
    PMIX_CONSTRUCT(&pmix_server_globals.notifications, pmix_ring_buffer_t);
    pmix_ring_buffer_init(&pmix_server_globals.notifications, 256);

    /* see if debug is requested */
    if (NULL != (evar = getenv("PMIX_DEBUG"))) {
        debug_level = strtol(evar, NULL, 10);
        pmix_globals.debug_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_globals.debug_output, debug_level);
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server init called");

    /* setup the function pointers */
    memset(&pmix_host_server, 0, sizeof(pmix_server_module_t));
    pmix_host_server = *module;

    /* init security */
    pmix_sec_init();
    security_mode = strdup(pmix_sec.name);

    /* find the temp dir */
    if (NULL == (tdir = getenv("PMIX_SERVER_TMPDIR"))) {
        if (NULL == (tdir = getenv("TMPDIR"))) {
            if (NULL == (tdir = getenv("TEMP"))) {
                if (NULL == (tdir = getenv("TMP"))) {
                    tdir = "/tmp";
                }
            }
        }
    }

    /* now set the address - we use the pid here to reduce collisions */
    memset(&myaddress, 0, sizeof(struct sockaddr_un));
    myaddress.sun_family = AF_UNIX;
    snprintf(myaddress.sun_path, sizeof(myaddress.sun_path)-1, "%s/pmix-%d", tdir, pid);
    asprintf(&myuri, "%s:%lu:%s", pmix_globals.myid.nspace, (unsigned long)pmix_globals.myid.rank, myaddress.sun_path);


    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server constructed uri %s", myuri);

    return PMIX_SUCCESS;
}

pmix_status_t PMIx_server_init(pmix_server_module_t *module,
                               pmix_info_t info[], size_t ninfo)
{
    pmix_usock_posted_recv_t *req;
    pmix_status_t rc;
    size_t n;
    pmix_kval_t kv;

    ++pmix_globals.init_cntr;
    if (1 < pmix_globals.init_cntr) {
        return PMIX_SUCCESS;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server init called");

    if (0 != (rc = initialize_server_base(module))) {
        return rc;
    }

    /* and the usock system */
    pmix_usock_init(NULL);

    /* create an event base and progress thread for us */
    if (NULL == (pmix_globals.evbase = pmix_start_progress_thread())) {
        return PMIX_ERR_INIT;
    }

    /* check the info keys for a directive about the uid/gid
     * to be set for the rendezvous file */
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (0 == strcmp(info[n].key, PMIX_USERID)) {
                /* the userid is in the uint32_t storage */
                chown(myaddress.sun_path, info[n].value.data.uint32, -1);
            } else if (0 == strcmp(info[n].key, PMIX_GRPID)) {
               /* the grpid is in the uint32_t storage */
                chown(myaddress.sun_path, -1, info[n].value.data.uint32);
            }
        }
    }

    /* setup the wildcard recv for inbound messages from clients */
    req = PMIX_NEW(pmix_usock_posted_recv_t);
    req->tag = UINT32_MAX;
    req->cbfunc = server_message_handler;
    /* add it to the end of the list of recvs */
    pmix_list_append(&pmix_usock_globals.posted_recvs, &req->super);

    /* start listening */
    if (PMIX_SUCCESS != pmix_start_listening(&myaddress)) {
        PMIx_server_finalize();
        return PMIX_ERR_INIT;
    }

    /* check the info keys for a directive about the uid/gid
     * to be set for the rendezvous file, and any info we
     * need to provide to every client */
    if (NULL != info) {
        PMIX_CONSTRUCT(&kv, pmix_kval_t);
        for (n=0; n < ninfo; n++) {
            if (0 == strcmp(info[n].key, PMIX_USERID)) {
                /* the userid is in the uint32_t storage */
                chown(myaddress.sun_path, info[n].value.data.uint32, -1);
            } else if (0 == strcmp(info[n].key, PMIX_GRPID)) {
                /* the grpid is in the uint32_t storage */
                chown(myaddress.sun_path, -1, info[n].value.data.uint32);
            } else {
                /* store and pass along to every client */
                kv.key = info[n].key;
                kv.value = &info[n].value;
                if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&pmix_server_globals.gdata, &kv, 1, PMIX_KVAL))) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DESTRUCT(&kv);
                    return rc;
                }
            }
        }
        /* protect the incoming data */
        kv.key = NULL;
        kv.value = NULL;
        PMIX_DESTRUCT(&kv);
    }

    return PMIX_SUCCESS;
}

static void cleanup_server_state(void)
{
    int i;
    pmix_peer_t *peer;

    for (i=0; i < pmix_server_globals.clients.size; i++) {
        if (NULL != (peer = (pmix_peer_t*)pmix_pointer_array_get_item(&pmix_server_globals.clients, i))) {
            PMIX_RELEASE(peer);
        }
    }
    PMIX_DESTRUCT(&pmix_server_globals.clients);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.collectives);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.remote_pnd);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.local_reqs);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.client_eventregs);
    PMIX_DESTRUCT(&pmix_server_globals.gdata);

    if (NULL != myuri) {
        free(myuri);
    }
    if (NULL != security_mode) {
        free(security_mode);
    }

    pmix_bfrop_close();
    pmix_sec_finalize();
    pmix_globals_finalize();
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server finalize complete");

    pmix_output_close(pmix_globals.debug_output);
    pmix_output_finalize();
    pmix_class_finalize();
}

pmix_status_t PMIx_server_finalize(void)
{
    if (1 != pmix_globals.init_cntr) {
        --pmix_globals.init_cntr;
        return PMIX_SUCCESS;
    }
    pmix_globals.init_cntr = 0;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server finalize called");

    if (pmix_server_globals.listen_thread_active) {
        pmix_stop_listening();
    }

    pmix_stop_progress_thread(pmix_globals.evbase);
    event_base_free(pmix_globals.evbase);
#ifdef HAVE_LIBEVENT_GLOBAL_SHUTDOWN
    libevent_global_shutdown();
#endif

    if (0 <= pmix_server_globals.listen_socket) {
        CLOSE_THE_SOCKET(pmix_server_globals.listen_socket);
    }

    pmix_usock_finalize();

    /* cleanup the rendezvous file */
    unlink(myaddress.sun_path);

    cleanup_server_state();
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

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server _register_nspace");

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
            if (PMIX_INFO_ARRAY != cd->info[i].value.type) {
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                goto release;
            }
            size = cd->info[i].value.data.array.size;
            iptr = (pmix_info_t*)cd->info[i].value.data.array.array;
            PMIX_CONSTRUCT(&buf2, pmix_buffer_t);
            /* first element of the array must be the rank */
            if (0 != strcmp(iptr[0].key, PMIX_RANK)) {
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                PMIX_DESTRUCT(&buf2);
                goto release;
            }
            /* pack it separately */
            rank = iptr[0].value.data.integer;
            if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&buf2, &rank, 1, PMIX_INT))) {
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
    PMIX_RELEASE(cd);
}

/* setup the data for a job */
pmix_status_t PMIx_server_register_nspace(const char nspace[], int nlocalprocs,
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

    PMIX_RELEASE(cd);
}

void PMIx_server_deregister_nspace(const char nspace[])
{
    pmix_setup_caddy_t *cd;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server deregister nspace %s",
                        nspace);

     cd = PMIX_NEW(pmix_setup_caddy_t);
    (void)strncpy(cd->proc.nspace, nspace, PMIX_MAX_NSLEN);

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _deregister_nspace);
}

static void _execute_collective(int sd, short args, void *cbdata)
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
                    pmix_bfrop.pack(&rankbuf, &info->rank, 1, PMIX_INT);
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
    pmix_rank_info_t *info;
    pmix_nspace_t *nptr, *tmp;
    pmix_server_trkr_t *trk;
    pmix_trkr_caddy_t *tcd;

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
            /* is this now completed? */
            if (pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
                /* it did, so now we need to process it
                 * we don't want to block someone
                 * here, so kick any completed trackers into a
                 * new event for processing */
                PMIX_EXECUTE_COLLECTIVE(tcd, trk, _execute_collective);
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

pmix_status_t PMIx_server_register_client(const pmix_proc_t *proc,
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
    /* find an remove this client */
    PMIX_LIST_FOREACH(info, &nptr->server->ranks, pmix_rank_info_t) {
        if (info->rank == cd->proc.rank) {
            pmix_list_remove_item(&nptr->server->ranks, &info->super);
            PMIX_RELEASE(info);
            break;
        }
    }

  cleanup:
    PMIX_RELEASE(cd);
}

void PMIx_server_deregister_client(const pmix_proc_t *proc)
{
    pmix_setup_caddy_t *cd;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server deregister client %s:%d",
                        proc->nspace, proc->rank);

     cd = PMIX_NEW(pmix_setup_caddy_t);
    (void)strncpy(cd->proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
    cd->proc.rank = proc->rank;

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _deregister_client);
}

/* setup the envars for a child process */
pmix_status_t PMIx_server_setup_fork(const pmix_proc_t *proc, char ***env)
{
    char rankstr[128];

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server setup_fork for nspace %s rank %d",
                        proc->nspace, proc->rank);

    /* pass the nspace */
    pmix_setenv("PMIX_NAMESPACE", proc->nspace, true, env);
    /* pass the rank */
    (void)snprintf(rankstr, 127, "%d", proc->rank);
    pmix_setenv("PMIX_RANK", rankstr, true, env);
    /* pass our rendezvous info */
    pmix_setenv("PMIX_SERVER_URI", myuri, true, env);
    /* pass our active security mode */
    pmix_setenv("PMIX_SECURITY_MODE", security_mode, true, env);

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

pmix_status_t PMIx_server_dmodex_request(const pmix_proc_t *proc,
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

static bool match_error_registration(pmix_regevents_info_t *reginfoptr, pmix_notify_caddy_t *cd)
{
    unsigned int i, j;
    char errgroup[PMIX_MAX_KEYLEN];
    pmix_info_t *info = reginfoptr->info;
    size_t ninfo = reginfoptr->ninfo;
    pmix_status_t error = cd->status;

    if (NULL == info || ninfo <= 0) {
        /* this is a general errhandler, and so it always matches.
         * however, here we are looking for an exact match, and
         * so we ignore general errhandlers unless the incoming
         * one is also general */
         if (NULL == cd->info || 0 == cd->ninfo) {
            return true;
        } else {
            return false;
        }
    }

    /* since this errhandler has info keys, it is not a general errhandler.
     * If the incoming errhandler *is* a general one, then we must not
     * match so we can store the general case */
    if (NULL == cd->info || 0 == cd->ninfo) {
        return false;
    }

    /* try to match using error name or error group keys - this indicates
     * a request for a specific error state */
    pmix_get_errorgroup(error, errgroup);
    for (i=0; i < ninfo; i++) {
        // if we get a match on any key then we abort the search and return true.
        if ((0 == strncmp(info[i].key, PMIX_ERROR_NAME, PMIX_MAX_KEYLEN)) &&
            (error == info[i].value.data.int32)) {
            return true;
        } else if ((0 == strncmp(info[i].key, errgroup, PMIX_MAX_KEYLEN)) &&
                   (true == info[i].value.data.flag)) {
            return true;
        }
    }

    /* if we get here, then they haven't asked for a specific error state.
     * It is possible, however, that they are asking for all errors from a
     * specific node, so search by node (error location) key if it is
     * specified in the notify info list */
    for (i=0; i < cd->ninfo ; i++) {
        if (0 == strncmp(cd->info[i].key, PMIX_ERROR_NODE_NAME, PMIX_MAX_KEYLEN)) {
            for (j=0; j < ninfo; j++) {
                if ((0 == strncmp(info[j].key, PMIX_ERROR_NODE_NAME, PMIX_MAX_KEYLEN)) &&
                    (0 == strcmp(info[j].value.data.string, cd->info[i].value.data.string))) {
                    return true;
                }
            }
        }
    }

    /* end of search and nothing matched, so return false */
    return false;
}

static void _notify_error(int sd, short args, void *cbdata)
{
    pmix_notify_caddy_t *cd = (pmix_notify_caddy_t*)cbdata;
    pmix_notify_caddy_t *rbout;
    pmix_status_t rc;
    pmix_cmd_t cmd = PMIX_NOTIFY_CMD;
    int i;
    size_t j;
    pmix_peer_t *peer;
    pmix_regevents_info_t *reginfoptr;
    bool notify, notifyall;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix_server: _notify_error notifying client of error %d",
                        cd->status);
    /* pack the command */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(cd->buf, &cmd, 1, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* pack the status */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(cd->buf, &cd->status, 1, PMIX_INT))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* pack the error procs */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(cd->buf, &cd->error_nprocs, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    if (0 < cd->error_nprocs) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(cd->buf, cd->error_procs, cd->error_nprocs, PMIX_PROC))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }


    /* pack the info */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(cd->buf, &cd->ninfo, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

   if (0 < cd->ninfo) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(cd->buf, cd->info, cd->ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }


    /* we cannot know if everyone who wants this notice has had a chance
     * to register for it - the notice may be coming too early. So cache
     * the message until all local procs have received it, or it ages to
     * the point where it gets pushed out by more recent events */
    PMIX_RETAIN(cd);
    rbout = pmix_ring_buffer_push(&pmix_server_globals.notifications, cd);

   /* if an older event was bumped, release it */
    if (NULL != rbout) {
        PMIX_RELEASE(rbout);
    }

    /* cycle across our registered events and send the message to
     * any within the specified proc array */
    PMIX_LIST_FOREACH(reginfoptr, &pmix_server_globals.client_eventregs, pmix_regevents_info_t) {
       pmix_server_check_notifications(reginfoptr, cd);
    }

  cleanup:
    /* notify the caller */
    if (NULL != cd->cbfunc) {
        cd->cbfunc(rc, cd->cbdata);
    }
   PMIX_RELEASE(cd);
}

pmix_status_t pmix_server_notify_error(pmix_status_t status,
                                pmix_proc_t procs[], size_t nprocs,
                                pmix_proc_t error_procs[], size_t error_nprocs,
                                pmix_info_t info[], size_t ninfo,
                                pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_notify_caddy_t *cd;
    size_t n;

    cd = PMIX_NEW(pmix_notify_caddy_t);
    cd->status = status;
    /* have to copy the info here as we may have to cache this
     * notification until procs have a chance to register for it */
    if (NULL != procs) {
        cd->nprocs = nprocs;
        PMIX_PROC_CREATE(cd->procs, cd->nprocs);
        for (n=0; n < cd->nprocs; n++) {
            (void)strncpy(cd->procs[n].nspace, procs[n].nspace, PMIX_MAX_NSLEN);
            cd->procs[n].rank = procs[n].rank;
        }
    }
    if (NULL != error_procs) {
        cd->error_nprocs = error_nprocs;
        PMIX_PROC_CREATE(cd->error_procs, cd->error_nprocs);
        for (n=0; n < cd->error_nprocs; n++) {
            (void)strncpy(cd->error_procs[n].nspace, error_procs[n].nspace, PMIX_MAX_NSLEN);
            cd->error_procs[n].rank = error_procs[n].rank;
        }
    }
    if (NULL != info) {
        cd->ninfo = ninfo;
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        for (n=0; n < cd->ninfo; n++) {
            PMIX_INFO_LOAD(&cd->info[n], info[n].key,
                           &info[n].value.data, info[n].value.type);
        }
    }
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix_server_notify_error status =%d, nprocs = %lu, ninfo =%lu",
                         status, nprocs, ninfo);

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _notify_error);
    return PMIX_SUCCESS;
}

void pmix_server_check_notifications(pmix_regevents_info_t *reginfo,
                                     pmix_notify_caddy_t *cd)
{
    bool notify;
    size_t j;

    /* if the RM gave us a NULL proc list, then we are notifying everyone */
    if (NULL != cd->procs) {
         /* check to see if this proc matches that of one in the specified array */
        notify = false;
        for (j=0; j < cd->nprocs; j++) {
            if (0 != strncmp(reginfo->peer->info->nptr->nspace, cd->procs[j].nspace, PMIX_MAX_NSLEN)) {
                continue;
            }
            if (PMIX_RANK_WILDCARD == cd->procs[j].rank ||
                cd->procs[j].rank == reginfo->peer->info->rank) {
                notify = true;
                break;
            }
        }
        if (!notify) {
            /* if we are not notifying everyone, and this proc isn't to
             * be notified, so just return */
            return;
        }
    }
    /* check if the client has registered for this error
     * by parsing the info keys */
    if (match_error_registration(reginfo, cd)) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix_server: check notifications - notifying process rank %d error %d",
                             reginfo->peer->info->rank, cd->status);
        PMIX_RETAIN(cd->buf);
        PMIX_SERVER_QUEUE_REPLY(reginfo->peer, 0, cd->buf);
    }

}
static void reg_errhandler(int sd, short args, void *cbdata)
{
    int index = 0;
    pmix_status_t rc;
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    pmix_notify_caddy_t *rb;

    /* check if this handler is already registered if so return error */
    if (PMIX_EXISTS == (rc = pmix_lookup_errhandler(cd->info, cd->ninfo, &index))) {
        /* complete request with error status and return its original reference */
        pmix_output_verbose(2, pmix_globals.debug_output,
                           "pmix_server_register_errhandler error - hdlr already registered index = %d",
                           index);
    } else {
         rc = pmix_add_errhandler(cd->err, cd->info, cd->ninfo, &index);
         pmix_output_verbose(2, pmix_globals.debug_output,
                             "pmix_server_register_errhandler - success index =%d", index);
    }
    /* cycle across any cached notifications and see if any are
     * pending for us and match this description */

    /* acknowledge the registration so the caller can release
     * their data */
    cd->cbfunc.errregcbfn(rc, index, cd->cbdata);

    PMIX_RELEASE(cd);
}

void pmix_server_register_errhandler(pmix_info_t info[], size_t ninfo,
                                     pmix_notification_fn_t errhandler,
                                     pmix_errhandler_reg_cbfunc_t cbfunc,
                                     void *cbdata)
{
    pmix_shift_caddy_t *cd;

    /* need to thread shift this request */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    cd->info = info;
    cd->ninfo = ninfo;
    cd->err = errhandler;
    cd->cbfunc.errregcbfn = cbfunc;
    cd->cbdata = cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix_server_register_errhandler shifting to server thread");

    PMIX_THREADSHIFT(cd, reg_errhandler);
}

static void dereg_errhandler(int sd, short args, void *cbdata)
{
    pmix_status_t rc;
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;

    rc = pmix_remove_errhandler(cd->ref);
    if (NULL != cd->cbfunc.opcbfn) {
        cd->cbfunc.opcbfn(rc, cd->cbdata);
    }
    cd->active = false;
}

void pmix_server_deregister_errhandler(int errhandler_ref,
                                pmix_op_cbfunc_t cbfunc,
                                void *cbdata)
{
    pmix_shift_caddy_t *cd;

    /* need to thread shift this request */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    cd->cbfunc.opcbfn = cbfunc;
    cd->cbdata = cbdata;
    cd->ref = errhandler_ref;
    PMIX_THREADSHIFT(cd, dereg_errhandler);

    PMIX_WAIT_FOR_COMPLETION(cd->active);
    PMIX_RELEASE(cd);
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

pmix_status_t PMIx_Store_internal(const pmix_proc_t *proc,
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

    if (pmix_globals.server) {
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

pmix_status_t PMIx_generate_regex(const char *input, char **regexp)
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
                asprintf(&tmp, "%s", vreg->prefix);
                pmix_argv_append_nosize(&regexargs, tmp);
                free(tmp);
            }
            PMIX_RELEASE(vreg);
            continue;
        }
        /* start the regex for this value with the prefix */
        if (NULL != vreg->prefix) {
            asprintf(&tmp, "%s[%d:", vreg->prefix, vreg->num_digits);
        } else {
            asprintf(&tmp, "[%d:", vreg->num_digits);
        }
        /* add the ranges */
        while (NULL != (range = (pmix_regex_range_t*)pmix_list_remove_first(&vreg->ranges))) {
            if (1 == range->cnt) {
                asprintf(&tmp2, "%s%d,", tmp, range->start);
            } else {
                asprintf(&tmp2, "%s%d-%d,", tmp, range->start, range->start + range->cnt - 1);
            }
            free(tmp);
            tmp = tmp2;
            PMIX_RELEASE(range);
        }
        /* replace the final comma */
        tmp[strlen(tmp)-1] = ']';
        if (NULL != vreg->suffix) {
            /* add in the suffix, if provided */
            asprintf(&tmp2, "%s%s", tmp, vreg->suffix);
            free(tmp);
            tmp = tmp2;
        }
        pmix_argv_append_nosize(&regexargs, tmp);
        free(tmp);
        PMIX_RELEASE(vreg);
    }

    /* assemble final result */
    tmp = pmix_argv_join(regexargs, ',');
    asprintf(regexp, "pmix[%s]", tmp);
    free(tmp);

    /* cleanup */
    pmix_argv_free(regexargs);

    PMIX_DESTRUCT(&vids);
    return PMIX_SUCCESS;
}

pmix_status_t PMIx_generate_ppn(const char *input, char **regexp)
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
                asprintf(&tmp2, "%s%d,", tmp, rng->start);
            } else {
                asprintf(&tmp2, "%s%d-%d,", tmp, rng->start, rng->start + rng->cnt - 1);
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

/****    THE FOLLOWING CALLBACK FUNCTIONS ARE USED BY THE HOST SERVER    ****
 ****    THEY THEREFORE CAN OCCUR IN EITHER THE HOST SERVER'S THREAD     ****
 ****    CONTEXT, OR IN OUR OWN THREAD CONTEXT IF THE CALLBACK OCCURS    ****
 ****    IMMEDIATELY. THUS ANYTHING THAT ACCESSES A GLOBAL ENTITY        ****
 ****    MUST BE PUSHED INTO AN EVENT FOR PROTECTION                     ****/

static void op_cbfunc(int status, void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    /* no need to thread-shift here as no global data is
     * being accessed */

    /* setup the reply with the returned status */
    reply = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &status, 1, PMIX_INT))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        return;
    }
    /* the function that created the server_caddy did a
     * retain on the peer, so we don't have to worry about
     * it still being present - send a copy to the originator */
    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    /* cleanup */
    PMIX_RELEASE(cd);
}

static void _spcb(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    pmix_nspace_t *nptr, *ns;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    /* setup the reply with the returned status */
    reply = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &cd->status, 1, PMIX_INT))) {
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
            /* shouldn't happen */
            PMIX_ERROR_LOG(PMIX_ERR_NOT_FOUND);
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
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &status, 1, PMIX_INT))) {
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
    pmix_buffer_t xfer, *bptr, *databuf, *bpscope, *reply;
    pmix_nspace_t *nptr, *ns;
    pmix_server_caddy_t *cd;
    char *nspace;
    int rank;
    pmix_status_t rc;
    int32_t cnt = 1;
    char byte;

    PMIX_CONSTRUCT(&xfer, pmix_buffer_t);
    if (PMIX_SUCCESS != scd->status) {
        rc = scd->status;
        goto finish_collective;
    }

    /* pass the blobs being returned */
    PMIX_LOAD_BUFFER(&xfer, scd->data, scd->ndata);

    if (PMIX_COLLECT_INVALID == tracker->collect_type) {
        rc = PMIX_ERR_INVALID_ARG;
        goto finish_collective;
    }

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
                /*
                 * TODO: if some namespaces are OK and the bad one is not the first
                 * the server is in inconsistent state. Should we rely on the client to abort
                 * computation or this is our task?
                 */
                rc = PMIX_ERR_INVALID_NAMESPACE;
                goto finish_collective;
            }

            /* unpack the rank */
            cnt = 1;
            if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(bptr, &rank, &cnt, PMIX_INT))) {
                PMIX_ERROR_LOG(rc);
                goto finish_collective;
            }
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "client:unpack fence received blob for rank %d", rank);
            /* there may be multiple blobs for this rank, each from a different scope */
            cnt = 1;
            while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(bptr, &bpscope, &cnt, PMIX_BUFFER))) {
                pmix_kval_t *kp = PMIX_NEW(pmix_kval_t);
                kp->key = strdup("modex");
                PMIX_VALUE_CREATE(kp->value, 1);
                kp->value->type = PMIX_BYTE_OBJECT;
                PMIX_UNLOAD_BUFFER(bpscope, kp->value->data.bo.bytes, kp->value->data.bo.size);
                /* store it in the appropriate hash */
               if (PMIX_SUCCESS != (rc = pmix_hash_store(&nptr->server->remote, rank, kp))) {
                    PMIX_ERROR_LOG(rc);
                }
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
    /* Protect data from being free'd because RM pass
     * the pointer that is set to the middle of some
     * buffer (the case with SLURM).
     * RM is responsible on the release of the buffer
     */
    xfer.base_ptr = NULL;
    xfer.bytes_used = 0;
    PMIX_DESTRUCT(&xfer);

    /* setup the reply, starting with the returned status */
    reply = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &rc, 1, PMIX_INT))) {
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
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &status, 1, PMIX_INT))) {
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
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &scd->status, 1, PMIX_INT))) {
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

void regevents_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_status_t rc;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*) cbdata;
    pmix_regevents_info_t *reginfo, *reginfo_next;
    pmix_buffer_t *reply;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:regevents_cbfunc called status = %d", status);

    if (PMIX_SUCCESS != status) {
        /* need to delete the stored event reg info when server
         * nacks reg events request */
        PMIX_LIST_FOREACH_SAFE(reginfo, reginfo_next, &pmix_server_globals.client_eventregs,
                               pmix_regevents_info_t) {
            if (reginfo->peer == cd->peer) {
                pmix_list_remove_item(&pmix_server_globals.client_eventregs,
                                      &reginfo->super);
                PMIX_RELEASE(reginfo);
                break;
            }
        }
    }
    reply = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &status, 1, PMIX_INT))) {
        PMIX_ERROR_LOG(rc);
    }
    // send reply
    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    PMIX_RELEASE(cd);
}

static void deregevents_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_status_t rc;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*) cbdata;
    pmix_buffer_t *reply = PMIX_NEW(pmix_buffer_t);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:deregevents_cbfunc called status = %d", status);

    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &status, 1, PMIX_INT))) {
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

    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(reply, &status, 1, PMIX_INT))) {
        PMIX_ERROR_LOG(rc);
    }

    // send reply
    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
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
    pmix_status_t rc;
    int32_t cnt;
    pmix_cmd_t cmd;
    pmix_server_caddy_t *cd;
    pmix_proc_t proc;
    pmix_buffer_t *reply;

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
        pmix_bfrop.copy_payload(reply, &(peer->info->nptr->server->job_info));
        pmix_bfrop.copy_payload(reply, &(pmix_server_globals.gdata));
        PMIX_SERVER_QUEUE_REPLY(peer, tag, reply);
        return PMIX_SUCCESS;
    }

    if (PMIX_ABORT_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_abort(peer, buf, op_cbfunc, cd))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_COMMIT_CMD == cmd) {
        if (PMIX_SUCCESS != (rc = pmix_server_commit(peer, buf))) {
            PMIX_ERROR_LOG(rc);
        }
        return rc;
    }

    if (PMIX_FENCENB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_fence(cd, buf, modex_cbfunc, op_cbfunc))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_GETNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_get(buf, get_cbfunc, cd))) {
            PMIX_ERROR_LOG(rc);
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
            if (PMIX_SUCCESS != (rc = pmix_host_server.client_finalized(&proc, peer->info->server_object,
                                                                        op_cbfunc, cd))) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(cd);
            }
        }
        /* turn off the recv event - we shouldn't hear anything
         * more from this proc */
        if (peer->recv_ev_active) {
            event_del(&peer->recv_event);
            peer->recv_ev_active = false;
        }
        return rc;
    }


    if (PMIX_PUBLISHNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_publish(peer, buf, op_cbfunc, cd))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
        }
        return rc;
    }


    if (PMIX_LOOKUPNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_lookup(peer, buf, lookup_cbfunc, cd))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
        }
        return rc;
    }


    if (PMIX_UNPUBLISHNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_unpublish(peer, buf, op_cbfunc, cd))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
        }
        return rc;
    }


    if (PMIX_SPAWNNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_spawn(peer, buf, spawn_cbfunc, cd))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
        }
        return rc;
    }


    if (PMIX_CONNECTNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_connect(cd, buf, false, cnct_cbfunc))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_DISCONNECTNB_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_connect(cd, buf, true, cnct_cbfunc))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_REGEVENTS_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_register_events(peer, buf, regevents_cbfunc, cd))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_DEREGEVENTS_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_deregister_events(peer, buf, deregevents_cbfunc, cd))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_NOTIFY_CMD == cmd) {
        PMIX_PEER_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_notify_error_client(peer, buf, notifyerror_cbfunc, cd))) {
            PMIX_ERROR_LOG(rc);
        }
        return rc;
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

static void server_message_handler(struct pmix_peer_t *pr, pmix_usock_hdr_t *hdr,
                                   pmix_buffer_t *buf, void *cbdata)
{
    pmix_peer_t *peer = (pmix_peer_t*)pr;
    pmix_buffer_t *reply;
    int rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "SWITCHYARD for %s:%d:%d",
                        peer->info->nptr->nspace,
                        peer->info->rank, peer->sd);

    rc = server_switchyard(peer, hdr->tag, buf);
    /* send the return, if there was an error returned */
    if (PMIX_SUCCESS != rc) {
        reply = PMIX_NEW(pmix_buffer_t);
        pmix_bfrop.pack(reply, &rc, 1, PMIX_INT);
        PMIX_SERVER_QUEUE_REPLY(peer, hdr->tag, reply);
    }
}
