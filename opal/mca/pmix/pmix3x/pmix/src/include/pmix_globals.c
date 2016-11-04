/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
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


void pmix_globals_init(void)
{
    memset(&pmix_globals.myid, 0, sizeof(pmix_proc_t));
    PMIX_CONSTRUCT(&pmix_globals.nspaces, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_globals.events, pmix_events_t);
}

void pmix_globals_finalize(void)
{
    PMIX_LIST_DESTRUCT(&pmix_globals.nspaces);
    if (NULL != pmix_globals.cache_local) {
        PMIX_RELEASE(pmix_globals.cache_local);
    }
    if (NULL != pmix_globals.cache_remote) {
        PMIX_RELEASE(pmix_globals.cache_remote);
    }
    PMIX_DESTRUCT(&pmix_globals.events);
}


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
PMIX_CLASS_INSTANCE(pmix_nspace_t,
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
PMIX_CLASS_INSTANCE(pmix_nrec_t,
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
    PMIX_DESTRUCT(&p->job_info);
    PMIX_LIST_DESTRUCT(&p->ranks);
    PMIX_DESTRUCT(&p->mylocal);
    PMIX_DESTRUCT(&p->myremote);
    PMIX_DESTRUCT(&p->remote);
}
PMIX_CLASS_INSTANCE(pmix_server_nspace_t,
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
PMIX_CLASS_INSTANCE(pmix_rank_info_t,
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
PMIX_CLASS_INSTANCE(pmix_shift_caddy_t,
                    pmix_object_t,
                    scon, scdes);

PMIX_CLASS_INSTANCE(pmix_info_caddy_t,
                    pmix_list_item_t,
                    NULL, NULL);

static void qcon(pmix_query_caddy_t *p)
{
    p->queries = NULL;
    p->nqueries = 0;
    p->cbfunc = NULL;
    p->cbdata = NULL;
    p->relcbfunc = NULL;
}
PMIX_CLASS_INSTANCE(pmix_query_caddy_t,
                    pmix_object_t,
                    qcon, NULL);
