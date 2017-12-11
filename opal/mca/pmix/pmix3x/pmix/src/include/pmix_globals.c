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
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */

#include <pmix_common.h>

#include "src/mca/bfrops/bfrops_types.h"
#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_list.h"
#include "src/threads/threads.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/os_path.h"

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
    PMIX_CONSTRUCT(&p->epilogs, pmix_list_t);
}

static void cleanup(pmix_info_caddy_t *epi);

static void pdes(pmix_peer_t *p)
{
    pmix_info_caddy_t *epi;

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
    PMIX_LIST_FOREACH(epi, &p->epilogs, pmix_info_caddy_t) {
        /* execute the epilog step */
        if (0 == strncmp(epi->info[0].key, PMIX_REGISTER_CLEANUP, PMIX_MAX_KEYLEN)) {
            cleanup(epi);
        }
        /* free the epilog contents as the info_caddy_t
         * destructor does not do so */
        PMIX_INFO_FREE(epi->info, epi->ninfo);
    }
    PMIX_LIST_DESTRUCT(&p->epilogs);
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

static void dirpath_destroy(const char *path,
                            bool recursive, bool empty,
                            bool leave_top, char **ignores);
static bool dirpath_is_empty(const char *path );
static int dirpath_access(const char *path, const mode_t in_mode );

static void cleanup(pmix_info_caddy_t *epi)
{
    char **targets = NULL, **ignores = NULL;
    bool recurse = false, empty = false, leave_top = false;
    size_t n;

    /* the targets for cleanup are in the first info struct */
    targets = pmix_argv_split(epi->info[0].value.data.string, ',');
    /* cycle over any modifiers */
    for (n=1; n < epi->ninfo; n++) {
        if (0 == strncmp(epi->info[n].key, PMIX_CLEANUP_RECURSIVE, PMIX_MAX_KEYLEN)) {
            recurse = PMIX_INFO_TRUE(&epi->info[n]);
        } else if (0 == strncmp(epi->info[n].key, PMIX_CLEANUP_EMPTY, PMIX_MAX_KEYLEN)) {
            empty = PMIX_INFO_TRUE(&epi->info[n]);
        } else if (0 == strncmp(epi->info[n].key, PMIX_CLEANUP_IGNORE, PMIX_MAX_KEYLEN)) {
            if (PMIX_STRING != epi->info[n].value.type) {
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                continue;
            }
            ignores = pmix_argv_split(epi->info[n].value.data.string, ',');
        } else if (0 == strncmp(epi->info[n].key, PMIX_CLEANUP_LEAVE_TOPDIR, PMIX_MAX_KEYLEN)) {
            leave_top = PMIX_INFO_TRUE(&epi->info[n]);
        }
    }
    /* cleanup the targets */
    for (n=0; NULL != targets[n]; n++) {
        dirpath_destroy(targets[n], recurse, empty, leave_top, ignores);
    }
    pmix_argv_free(targets);
    if (NULL != ignores) {
        pmix_argv_free(ignores);
    }
}

static void dirpath_destroy(const char *path,
                            bool recursive, bool empty,
                            bool leave_top, char **ignores)
{
    int rc;
    bool is_dir = false, ignore;
    DIR *dp;
    struct dirent *ep;
    char *filenm;
    struct stat buf;
    size_t n, len;

    if (NULL == path) {  /* protect against error */
        return;
    }

    /*
     * Make sure we have access to the the base directory
     */
    if (PMIX_SUCCESS != (rc = dirpath_access(path, 0))) {
        goto cleanup;
    }

    /* Open up the directory */
    dp = opendir(path);
    if (NULL == dp) {
        return;
    }

    while (NULL != (ep = readdir(dp))) {
        /* skip:
         *  - . and ..
         */
        if ((0 == strcmp(ep->d_name, ".")) ||
            (0 == strcmp(ep->d_name, ".."))) {
            continue;
        }

        /* Check to see if it is a directory */
        is_dir = false;

        /* Create a pathname.  This is not always needed, but it makes
         * for cleaner code just to create it here.  Note that we are
         * allocating memory here, so we need to free it later on.
         */
        filenm = pmix_os_path(false, path, ep->d_name, NULL);

        rc = stat(filenm, &buf);
        if (0 > rc) {
            /* Handle a race condition. filenm might have been deleted by an
             * other process running on the same node. That typically occurs
             * when one task is removing the job_session_dir and an other task
             * is still removing its proc_session_dir.
             */
            free(filenm);
            continue;
        }
        if (S_ISDIR(buf.st_mode)) {
            is_dir = true;
        }

        /*
         * If not recursively decending, then if we find a directory then fail
         * since we were not told to remove it.
         */
        if (is_dir && !recursive) {
            /* continue removing files */
            free(filenm);
            continue;
        }

        /* Will the caller allow us to remove this file/directory? */
        if (NULL != ignores) {
            ignore = false;
            for (n=0; NULL != ignores[n]; n++) {
                if ('*' == ignores[n][strlen(ignores[n]-1)]) {
                    len = strlen(ignores[n]) - 1;
                } else {
                    len = strlen(ignores[n]);
                }
                if (0 == strncmp(ep->d_name, ignores[n], len)) {
                    /* ignore this one */
                    ignore = true;
                    break;
                }
            }
            /*
             * Caller does not wish to remove this file/directory,
             * continue with the rest of the entries
             */
            if (ignore) {
                free(filenm);
                continue;
            }
        }
        /* Directories are recursively destroyed */
        if (is_dir && recursive) {
            dirpath_destroy(filenm, recursive, empty, leave_top, ignores);
            free(filenm);
        } else {
            /* Files are removed right here */
            unlink(filenm);
            free(filenm);
        }
    }

    /* Done with this directory */
    closedir(dp);

  cleanup:

    /*
     * If the directory is empty, them remove it
     */
    if(empty && dirpath_is_empty(path)) {
        rmdir(path);
    }
}

static bool dirpath_is_empty(const char *path )
{
    DIR *dp;
    struct dirent *ep;

    if (NULL != path) {  /* protect against error */
        dp = opendir(path);
        if (NULL != dp) {
            while ((ep = readdir(dp))) {
                        if ((0 != strcmp(ep->d_name, ".")) &&
                            (0 != strcmp(ep->d_name, ".."))) {
                            closedir(dp);
                            return false;
                        }
            }
            closedir(dp);
            return true;
        }
        return false;
    }

    return true;
}

static int dirpath_access(const char *path, const mode_t in_mode )
{
    struct stat buf;
    mode_t loc_mode = S_IRWXU;  /* looking for full rights */

    /*
     * If there was no mode specified, use the default mode
     */
    if (0 != in_mode) {
        loc_mode = in_mode;
    }

    if (0 == stat(path, &buf)) { /* exists - check access */
        if ((buf.st_mode & loc_mode) == loc_mode) { /* okay, I can work here */
            return(PMIX_SUCCESS);
        } else {
            /* Don't have access rights to the existing path */
            return(PMIX_ERROR);
        }
    } else {
        /* We could not find the path */
        return( PMIX_ERR_NOT_FOUND );
    }
}
