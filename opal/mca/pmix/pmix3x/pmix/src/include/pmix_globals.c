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

#include "src/mca/bfrops/bfrops_types.h"
#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_list.h"
#include "src/threads/threads.h"

PMIX_EXPORT pmix_lock_t pmix_global_lock = {
    .mutex = PMIX_MUTEX_STATIC_INIT,
    .cond = PMIX_CONDITION_STATIC_INIT,
    .active = false
};

PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_namelist_t,
                                pmix_list_item_t,
                                NULL, NULL);

static void nscon(pmix_nspace_t *p)
{
    p->nspace = NULL;
    p->nlocalprocs = 0;
    p->all_registered = false;
    p->jobbkt = NULL;
    p->ndelivered = 0;
    PMIX_CONSTRUCT(&p->ranks, pmix_list_t);
    memset(&p->compat, 0, sizeof(p->compat));
}
static void nsdes(pmix_nspace_t *p)
{
    if (NULL != p->nspace) {
        free(p->nspace);
    }
    if (NULL != p->jobbkt) {
        PMIX_RELEASE(p->jobbkt);
    }
    PMIX_LIST_DESTRUCT(&p->ranks);
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_nspace_t,
                                pmix_list_item_t,
                                nscon, nsdes);

static void ncdcon(pmix_nspace_caddy_t *p)
{
    p->ns = NULL;
}
static void ncddes(pmix_nspace_caddy_t *p)
{
    if (NULL != p->ns) {
        PMIX_RELEASE(p->ns);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_nspace_caddy_t,
                                pmix_list_item_t,
                                ncdcon, ncddes);

static void info_con(pmix_rank_info_t *info)
{
    info->peerid = -1;
    info->gid = info->uid = 0;
    info->pname.nspace = NULL;
    info->pname.rank = PMIX_RANK_UNDEF;
    info->modex_recvd = false;
    info->proc_cnt = 0;
    info->server_object = NULL;
}
static void info_des(pmix_rank_info_t *info)
{
    if (NULL != info->pname.nspace) {
        free(info->pname.nspace);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_rank_info_t,
                                pmix_list_item_t,
                                info_con, info_des);

static void pcon(pmix_peer_t *p)
{
    p->proc_type = PMIX_PROC_UNDEF;
    p->finalized = false;
    p->info = NULL;
    p->proc_cnt = 0;
    p->index = 0;
    p->sd = -1;
    p->finalized = false;
    p->send_ev_active = false;
    p->recv_ev_active = false;
    PMIX_CONSTRUCT(&p->send_queue, pmix_list_t);
    p->send_msg = NULL;
    p->recv_msg = NULL;
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

static void scon(pmix_shift_caddy_t *p)
{
    PMIX_CONSTRUCT_LOCK(&p->lock);
    p->codes = NULL;
    p->ncodes = 0;
    p->pname.nspace = NULL;
    p->pname.rank = PMIX_RANK_UNDEF;
    p->data = NULL;
    p->ndata = 0;
    p->key = NULL;
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
    PMIX_DESTRUCT_LOCK(&p->lock);
    if (NULL != p->pname.nspace) {
        free(p->pname.nspace);
    }
    if (NULL != p->kv) {
        PMIX_RELEASE(p->kv);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_shift_caddy_t,
                                pmix_object_t,
                                scon, scdes);

static void cbcon(pmix_cb_t *p)
{
    PMIX_CONSTRUCT_LOCK(&p->lock);
    p->checked = false;
    PMIX_CONSTRUCT(&p->data, pmix_buffer_t);
    p->cbfunc.ptlfn = NULL;
    p->cbdata = NULL;
    p->pname.nspace = NULL;
    p->pname.rank = PMIX_RANK_UNDEF;
    p->scope = PMIX_SCOPE_UNDEF;
    p->key = NULL;
    p->value = NULL;
    p->procs = NULL;
    p->nprocs = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->nvals = 0;
    PMIX_CONSTRUCT(&p->kvs, pmix_list_t);
    p->copy = false;
    p->timer_running = false;
}
static void cbdes(pmix_cb_t *p)
{
    if (p->timer_running) {
        pmix_event_del(&p->ev);
    }
    if (NULL != p->pname.nspace) {
        free(p->pname.nspace);
    }
    PMIX_DESTRUCT(&p->data);
    PMIX_LIST_DESTRUCT(&p->kvs);
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_cb_t,
                                pmix_list_item_t,
                                cbcon, cbdes);

PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_info_caddy_t,
                                pmix_list_item_t,
                                NULL, NULL);

static void qcon(pmix_query_caddy_t *p)
{
    PMIX_CONSTRUCT_LOCK(&p->lock);
    p->queries = NULL;
    p->nqueries = 0;
    p->targets = NULL;
    p->ntargets = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->cbfunc = NULL;
    p->valcbfunc = NULL;
    p->cbdata = NULL;
    p->relcbfunc = NULL;
}
static void qdes(pmix_query_caddy_t *p)
{
    PMIX_DESTRUCT_LOCK(&p->lock);
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_query_caddy_t,
                                pmix_object_t,
                                qcon, qdes);
