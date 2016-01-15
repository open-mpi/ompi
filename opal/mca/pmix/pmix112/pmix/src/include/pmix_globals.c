/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* THIS FILE IS INCLUDED SOLELY TO INSTANTIATE AND INIT/FINALIZE THE GLOBAL CLASSES */

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>
#include <private/pmix_stdint.h>
#include <private/pmix_socket_errno.h>

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


pmix_globals_t pmix_globals = {
    .init_cntr = 0,
    .pindex = 0,
    .evbase = NULL,
    .debug_output = -1,
    .errhandler = NULL,
    .server = false,
    .connected = false,
    .cache_local = NULL,
    .cache_remote = NULL
};


void pmix_globals_init(void)
{
    memset(&pmix_globals.myid, 0, sizeof(pmix_proc_t));
    PMIX_CONSTRUCT(&pmix_globals.nspaces, pmix_list_t);
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
    PMIX_LIST_DESTRUCT(&p->nodes);
    PMIX_DESTRUCT(&p->internal);
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
