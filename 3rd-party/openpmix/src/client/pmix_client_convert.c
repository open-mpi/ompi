/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2021 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2014      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2016-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016-2022 IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "include/pmix.h"

#include "src/class/pmix_object.h"
#include "src/class/pmix_list.h"
#include "src/client/pmix_client_ops.h"
#include "src/include/pmix_globals.h"

pmix_status_t pmix_client_convert_group_procs(const pmix_proc_t *inprocs, size_t insize,
                                              pmix_proc_t **outprocs, size_t *outsize)
{
    pmix_list_t cache;
    pmix_proclist_t *nm;
    pmix_group_t *grp;
    size_t n, i, cnt, sz;
    bool match;
    uint32_t jsize;
    pmix_status_t rc;
    pmix_kval_t *kv;
    pmix_proc_t *procs;
    pmix_cb_t cb2;

    PMIX_CONSTRUCT(&cache, pmix_list_t);

    /* cycle thru the procs and check to see if any reference
     * a PMIx group */
    for (n = 0; n < insize; n++) {
        match = false;
        PMIX_LIST_FOREACH(grp, &pmix_client_globals.groups, pmix_group_t) {

            if (PMIX_CHECK_NSPACE(grp->grpid, inprocs[n].nspace)) {
                match = true;
                /* the nspace matches this group ID */

                if (PMIX_RANK_WILDCARD == inprocs[n].rank) {
                    /* we need to replace this proc with the grp members */
                    for (i=0; i < grp->nmbrs; i++) {
                        nm = PMIX_NEW(pmix_proclist_t);
                        memcpy(&nm->proc, &grp->members[i], sizeof(pmix_proc_t));
                        pmix_list_append(&cache, &nm->super);
                    }
                    continue;
                }

                /* if the rank isn't wildcard, then we want a specific
                 * proc from within the group. The group might include
                 * members that have rank=wildcard for their nspace,
                 * and so we have to count from the beginning to find
                 * the proc of the specified group rank */
                cnt = 0;
                for (i = 0; i < grp->nmbrs; i++) {
                    /* we are looking for the cnt=inprocs[n].rank proc
                     * within the group. so count our way across */

                    if (PMIX_RANK_WILDCARD == grp->members[i].rank) {
                        /* We must get the number of procs in this nspace so
                         * we can check to see if the specified rank actually
                         * falls within it */
                        PMIX_CONSTRUCT(&cb2, pmix_cb_t);
                        cb2.proc = (pmix_proc_t*)&grp->members[i];
                        cb2.key = PMIX_JOB_SIZE;
                        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb2);
                        if (PMIX_SUCCESS != rc && PMIX_OPERATION_SUCCEEDED != rc) {
                            /* couldn't get the job size, so have to abort */
                            PMIX_LIST_DESTRUCT(&cache);
                            PMIX_DESTRUCT(&cb2);
                            return rc;
                        }
                        kv = (pmix_kval_t*)pmix_list_remove_first(&cb2.kvs);
                        PMIX_DESTRUCT(&cb2);
                        if (NULL == kv) {  // should never be NULL
                            /* couldn't retrieve the size, so we have
                             * to abort */
                            PMIX_LIST_DESTRUCT(&cache);
                            return PMIX_ERR_NOT_FOUND;
                        }
                        PMIX_VALUE_GET_NUMBER(rc, kv->value, jsize, uint32_t);
                        PMIX_RELEASE(kv);
                        if (PMIX_SUCCESS != rc) {
                            PMIX_LIST_DESTRUCT(&cache);
                            return PMIX_ERR_BAD_PARAM;
                        }
                        if (cnt + jsize > inprocs[n].rank) {
                            /* the specified rank is within this job */
                            nm = PMIX_NEW(pmix_proclist_t);
                            PMIX_LOAD_NSPACE(nm->proc.nspace, grp->members[i].nspace);
                            nm->proc.rank = inprocs[n].rank - cnt;
                            break;
                        } else {
                            /* increment the count */
                            cnt += jsize;
                            /* continue to the next group member */
                        }
                    } else {
                        /* this is a single proc entry, so just see if
                         * it matches the one they asked for */
                        if (cnt == inprocs[n].rank) {
                            nm = PMIX_NEW(pmix_proclist_t);
                            memcpy(&nm->proc, &grp->members[i], sizeof(pmix_proc_t));
                            pmix_list_append(&cache, &nm->super);
                            break;
                        } else {
                            /* increment the count */
                            ++cnt;
                            /* continue to the next group member */
                        }
                    }
                }
            }
            if (match) {
                break;
            }
        }
        if (!match) {
            /* xfer the incoming proc across to the cache */
            nm = PMIX_NEW(pmix_proclist_t);
            memcpy(&nm->proc, &inprocs[n], sizeof(pmix_proc_t));
            pmix_list_append(&cache, &nm->super);
        }
    }

    /* we have to return the cached array because
     * we might have replaced some of the entries */
    sz = pmix_list_get_size(&cache);
    PMIX_PROC_CREATE(procs, sz);
    n = 0;
    PMIX_LIST_FOREACH(nm, &cache, pmix_proclist_t) {
        memcpy(&procs[n], &nm->proc, sizeof(pmix_proc_t));
        ++n;
    }
    PMIX_LIST_DESTRUCT(&cache);
    *outprocs = procs;
    *outsize = sz;
    return PMIX_SUCCESS;
}
