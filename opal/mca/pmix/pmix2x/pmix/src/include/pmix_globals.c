/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* THIS FILE IS INCLUDED SOLELY TO INSTANTIATE AND INIT/FINALIZE THE GLOBAL CLASSES */

#include <src/include/pmix_config.h>

#include <src/include/types.h>
#include <src/include/pmix_stdint.h>
#include <src/include/pmix_socket_errno.h>

#include "src/include/pmix_globals.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <ctype.h>
#include PMIX_EVENT_HEADER

#include "src/buffer_ops/types.h"
#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_list.h"

static void cbcon(pmix_cb_t *p)
{
    p->active = false;
    p->checked = false;
    PMIX_CONSTRUCT(&p->data, pmix_buffer_t);
    p->cbfunc = NULL;
    p->op_cbfunc = NULL;
    p->value_cbfunc = NULL;
    p->lookup_cbfunc = NULL;
    p->spawn_cbfunc = NULL;
    p->cbdata = NULL;
    memset(p->nspace, 0, PMIX_MAX_NSLEN+1);
    p->rank = -1;
    p->key = NULL;
    p->value = NULL;
    p->procs = NULL;
    p->info = NULL;
    p->ninfo = 0;
    p->nvals = 0;
}
static void cbdes(pmix_cb_t *p)
{
    PMIX_DESTRUCT(&p->data);
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_cb_t,
                                pmix_list_item_t,
                                cbcon, cbdes);

static void pcon(pmix_peer_t *p)
{
    p->info = NULL;
    p->proc_cnt = 0;
    p->server_object = NULL;
    p->index = 0;
    p->sd = -1;
    p->send_ev_active = false;
    p->recv_ev_active = false;
    PMIX_CONSTRUCT(&p->send_queue, pmix_list_t);
    p->send_msg = NULL;
    p->recv_msg = NULL;
    memset(&p->compat, 0, sizeof(p->compat));
}
static void pdes(pmix_peer_t *p)
{
    if (0 <= p->sd) {
        CLOSE_THE_SOCKET(p->sd);
    }
    if (p->send_ev_active) {
        pmix_event_del(&p->send_event);
    }
    if (p->recv_ev_active) {
        pmix_event_del(&p->recv_event);
    }

    if (NULL != p->info) {
        PMIX_RELEASE(p->info);
    }

    PMIX_LIST_DESTRUCT(&p->send_queue);
    if (NULL != p->send_msg) {
        PMIX_RELEASE(p->send_msg);
    }
    if (NULL != p->recv_msg) {
        PMIX_RELEASE(p->recv_msg);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_peer_t,
                                pmix_object_t,
                                pcon, pdes);

static void nscon(pmix_nspace_t *p)
{
    memset(p->nspace, 0, PMIX_MAX_NSLEN);
    PMIX_CONSTRUCT(&p->nodes, pmix_list_t);
    PMIX_CONSTRUCT(&p->internal, pmix_hash_table_t);
    pmix_hash_table_init(&p->internal, 16);
    PMIX_CONSTRUCT(&p->modex, pmix_hash_table_t);
    pmix_hash_table_init(&p->modex, 256);
    p->server = NULL;
}
static void nsdes(pmix_nspace_t *p)
{
    uint64_t key;
    pmix_object_t *obj;

    PMIX_LIST_DESTRUCT(&p->nodes);
    PMIX_HASH_TABLE_FOREACH(key, uint64, obj, &p->internal) {
        if (NULL != obj) {
            PMIX_RELEASE(obj);
        }
    }
    PMIX_DESTRUCT(&p->internal);
    PMIX_HASH_TABLE_FOREACH(key, uint64, obj, &p->modex) {
        if (NULL != obj) {
            PMIX_RELEASE(obj);
        }
    }
    PMIX_DESTRUCT(&p->modex);
    if (NULL != p->server) {
        PMIX_RELEASE(p->server);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_nspace_t,
                                pmix_list_item_t,
                                nscon, nsdes);

static void ncon(pmix_nrec_t *p)
{
    p->name = NULL;
    p->procs = NULL;
}
static void ndes(pmix_nrec_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
    if (NULL != p->procs) {
        free(p->procs);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_nrec_t,
                                pmix_list_item_t,
                                ncon, ndes);

static void sncon(pmix_server_nspace_t *p)
{
    p->nlocalprocs = 0;
    p->all_registered = false;
    PMIX_CONSTRUCT(&p->job_info, pmix_buffer_t);
    PMIX_CONSTRUCT(&p->ranks, pmix_list_t);
    PMIX_CONSTRUCT(&p->mylocal, pmix_hash_table_t);
    pmix_hash_table_init(&p->mylocal, 16);
    PMIX_CONSTRUCT(&p->myremote, pmix_hash_table_t);
    pmix_hash_table_init(&p->myremote, 16);
    PMIX_CONSTRUCT(&p->remote, pmix_hash_table_t);
    pmix_hash_table_init(&p->remote, 256);
}
static void sndes(pmix_server_nspace_t *p)
{
    uint64_t key;
    pmix_peer_t * peer;
    PMIX_DESTRUCT(&p->job_info);
    PMIX_LIST_DESTRUCT(&p->ranks);
    PMIX_HASH_TABLE_FOREACH(key, uint64, peer, &p->mylocal) {
        PMIX_RELEASE(peer);
    }
    PMIX_DESTRUCT(&p->mylocal);
    PMIX_HASH_TABLE_FOREACH(key, uint64, peer, &p->myremote) {
        PMIX_RELEASE(peer);
    }
    PMIX_DESTRUCT(&p->myremote);
    PMIX_DESTRUCT(&p->remote);
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_server_nspace_t,
                                pmix_object_t,
                                sncon, sndes);

static void info_con(pmix_rank_info_t *info)
{
    info->gid = info->uid = 0;
    info->nptr = NULL;
    info->rank = PMIX_RANK_WILDCARD;
    info->modex_recvd = false;
    info->proc_cnt = 0;
    info->server_object = NULL;
}
static void info_des(pmix_rank_info_t *info)
{
    if (NULL!= info->nptr) {
        PMIX_RELEASE(info->nptr);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_rank_info_t,
                                pmix_list_item_t,
                                info_con, info_des);

static void scon(pmix_shift_caddy_t *p)
{
    p->active = false;
    p->codes = NULL;
    p->ncodes = 0;
    p->nspace = NULL;
    p->data = NULL;
    p->ndata = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->directives = NULL;
    p->ndirs = 0;
    p->evhdlr = NULL;
    p->kv = NULL;
    p->vptr = NULL;
    p->cd = NULL;
    p->tracker = NULL;
    p->enviro = false;
    p->cbfunc.relfn = NULL;
    p->cbdata = NULL;
    p->ref = 0;
}
static void scdes(pmix_shift_caddy_t *p)
{
    if (NULL != p->kv) {
        PMIX_RELEASE(p->kv);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_shift_caddy_t,
                                pmix_object_t,
                                scon, scdes);

PMIX_CLASS_INSTANCE(pmix_info_caddy_t,
                    pmix_list_item_t,
                    NULL, NULL);

static void qcon(pmix_query_caddy_t *p)
{
    p->queries = NULL;
    p->nqueries = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->cbfunc = NULL;
    p->cbdata = NULL;
    p->relcbfunc = NULL;
}
PMIX_CLASS_INSTANCE(pmix_query_caddy_t,
                    pmix_object_t,
                    qcon, NULL);

static void jdcon(pmix_job_data_caddy_t *p)
{
    p->nsptr = NULL;
    p->job_data = NULL;
    p->dstore_fn = NULL;
    p->hstore_fn = NULL;
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    p->bufs = NULL;
#endif
}

PMIX_CLASS_INSTANCE(pmix_job_data_caddy_t,
                    pmix_object_t,
                    jdcon, NULL);
