/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2018 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018-2020 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#include <time.h>

#include "pmix_common.h"

#include "src/class/pmix_list.h"
#include "src/client/pmix_client_ops.h"
#include "src/include/pmix_globals.h"
#include "src/mca/pcompress/base/base.h"
#include "src/mca/pmdl/pmdl.h"
#include "src/mca/preg/preg.h"
#include "src/mca/ptl/base/base.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_hash.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"

#include "gds_hash.h"
#include "src/mca/gds/base/base.h"

pmix_job_t *pmix_gds_hash_get_tracker(const pmix_nspace_t nspace, bool create)
{
    pmix_job_t *trk, *t;
    pmix_namespace_t *ns, *nptr;

    /* find the hash table for this nspace */
    trk = NULL;
    PMIX_LIST_FOREACH (t, &pmix_mca_gds_hash_component.myjobs, pmix_job_t) {
        if (0 == strcmp(nspace, t->ns)) {
            trk = t;
            break;
        }
    }
    if (NULL == trk && create) {
        /* create one */
        trk = PMIX_NEW(pmix_job_t);
        trk->ns = strdup(nspace);
        /* see if we already have this nspace */
        nptr = NULL;
        PMIX_LIST_FOREACH (ns, &pmix_globals.nspaces, pmix_namespace_t) {
            if (0 == strcmp(ns->nspace, nspace)) {
                nptr = ns;
                break;
            }
        }
        if (NULL == nptr) {
            nptr = PMIX_NEW(pmix_namespace_t);
            if (NULL == nptr) {
                PMIX_RELEASE(trk);
                return NULL;
            }
            nptr->nspace = strdup(nspace);
            pmix_list_append(&pmix_globals.nspaces, &nptr->super);
        }
        PMIX_RETAIN(nptr);
        trk->nptr = nptr;
        pmix_list_append(&pmix_mca_gds_hash_component.myjobs, &trk->super);
    }
    return trk;
}

bool pmix_gds_hash_check_hostname(char *h1, char *h2)
{
    if (0 == strcmp(h1, h2)) {
        return true;
    }
    return false;
}

bool pmix_gds_hash_check_node(pmix_nodeinfo_t *n1, pmix_nodeinfo_t *n2)
{
    int i, j;

    if (UINT32_MAX != n1->nodeid && UINT32_MAX != n2->nodeid && n1->nodeid == n2->nodeid) {
        return true;
    }

    if (NULL == n1->hostname || NULL == n2->hostname) {
        return false;
    }

    if (pmix_gds_hash_check_hostname(n1->hostname, n2->hostname)) {
        return true;
    }

    if (NULL != n1->aliases) {
        for (i = 0; NULL != n1->aliases[i]; i++) {
            if (pmix_gds_hash_check_hostname(n1->aliases[i], n2->hostname)) {
                return true;
            }
            if (NULL != n2->aliases) {
                for (j = 0; NULL != n2->aliases[j]; j++) {
                    if (pmix_gds_hash_check_hostname(n1->hostname, n2->aliases[j])) {
                        return true;
                    }
                    if (pmix_gds_hash_check_hostname(n1->aliases[i], n2->aliases[j])) {
                        return true;
                    }
                }
            }
        }
    } else if (NULL != n2->aliases) {
        for (j = 0; NULL != n2->aliases[j]; j++) {
            if (pmix_gds_hash_check_hostname(n1->hostname, n2->aliases[j])) {
                return true;
            }
        }
    }

    return false;
}

pmix_session_t* pmix_gds_hash_check_session(pmix_job_t *trk,
                                            uint32_t sid,
                                            bool create)
{
    pmix_session_t *sptr;
    bool found;

    /* if the tracker is NULL, then they are asking for the
     * session tracker for a specific sid (which can be UINT32_MAX) */
    if (NULL == trk) {
        PMIX_LIST_FOREACH(sptr, &pmix_mca_gds_hash_component.mysessions, pmix_session_t) {
            if (sptr->session == sid) {
                return sptr;
            }
        }
        /* if it wasn't found, then add it if permitted */
        if (create) {
            sptr = PMIX_NEW(pmix_session_t);
            sptr->session = sid;
            pmix_list_append(&pmix_mca_gds_hash_component.mysessions, &sptr->super);
            return sptr;
        } else {
            /* we didn't find it */
            return NULL;
        }
    }

    if (NULL == trk->session) {
        /* no session has been assigned to this job - see
         * if the given ID has already been registered */
        found = false;
        PMIX_LIST_FOREACH(sptr, &pmix_mca_gds_hash_component.mysessions, pmix_session_t) {
            if (sptr->session == sid) {
                found = true;
                break;
            }
        }
        if (found) {
            /* point the job tracker at this session */
            PMIX_RETAIN(sptr);
            trk->session = sptr;
            return sptr;
        }
        /* if it wasn't found, then create it if permitted */
        if (create) {
            sptr = PMIX_NEW(pmix_session_t);
            sptr->session = sid;
            PMIX_RETAIN(sptr);
            trk->session = sptr;
            pmix_list_append(&pmix_mca_gds_hash_component.mysessions, &sptr->super);
            return sptr;
        } else {
            return NULL;
        }
    }

    /* if the current session object is pointing to the default
     * global session and we were given a specific session ID,
     * then update it */
    if (UINT32_MAX == trk->session->session) {
        if (UINT32_MAX == sid) {
            /* if the given sid is also UINT32_MAX, then we just add to it */
            return trk->session;
        }
        /* see if the given ID has already been registered */
        found = false;
        PMIX_LIST_FOREACH(sptr, &pmix_mca_gds_hash_component.mysessions, pmix_session_t) {
            if (sptr->session == sid) {
                found = true;
                break;
            }
        }
        if (found) {
            /* update the refcount on the current session object */
            PMIX_RELEASE(trk->session);
            /* point the job tracker at the new place */
            PMIX_RETAIN(sptr);
            trk->session = sptr;
            return sptr;
        }
        /* if it wasn't found, then create it */
        if (create) {
            sptr = PMIX_NEW(pmix_session_t);
            sptr->session = sid;
            PMIX_RETAIN(sptr);
            trk->session = sptr;
            pmix_list_append(&pmix_mca_gds_hash_component.mysessions, &sptr->super);
            return sptr;
        }
    } else if (UINT32_MAX == sid) {
        /* it's a wildcard request, so return the job-tracker session */
        return trk->session;
    }

    /* the job tracker already was assigned a session ID - check
     * if the new one matches */
    if (trk->session->session != sid) {
        /* this is an error - you cannot assign a given job
         * to multiple sessions */
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return NULL;
    }
    /* the two must match, so return it */
    sptr = trk->session;
    return sptr;
}

pmix_nodeinfo_t* pmix_gds_hash_check_nodename(pmix_list_t *nodes, char *hostname)
{
    int i;
    pmix_nodeinfo_t *nd;
    bool aliases_exist = false;

    if (NULL == hostname) {
        return NULL;
    }

    /* first, just check all the node names as this is the
     * most likely match */
    PMIX_LIST_FOREACH (nd, nodes, pmix_nodeinfo_t) {
        if (0 == strcmp(nd->hostname, hostname)) {
            return nd;
        }
        if (NULL != nd->aliases) {
            aliases_exist = true;
        }
    }

    if (!aliases_exist) {
        return NULL;
    }

    /* if a match wasn't found, then we have to try the aliases */
    PMIX_LIST_FOREACH (nd, nodes, pmix_nodeinfo_t) {
        if (NULL != nd->aliases) {
            for (i = 0; NULL != nd->aliases[i]; i++) {
                if (0 == strcmp(nd->aliases[i], hostname)) {
                    return nd;
                }
            }
        }
    }
    /* no match was found */
    return NULL;
}

pmix_status_t pmix_gds_hash_store_map(pmix_job_t *trk, char **nodes, char **ppn, uint32_t flags)
{
    pmix_status_t rc;
    size_t m, n;
    pmix_rank_t rank;
    pmix_kval_t *kp1, *kp2;
    char **procs;
    uint32_t totalprocs = 0;
    pmix_hash_table_t *ht = &trk->internal;
    pmix_nodeinfo_t *nd;

    pmix_output_verbose(2, pmix_gds_base_framework.framework_output, "[%s:%d] gds:hash:store_map",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank);

    /* if the lists don't match, then that's wrong */
    if (PMIx_Argv_count(nodes) != PMIx_Argv_count(ppn)) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }

    /* if they didn't provide the number of nodes, then
     * compute it from the list of nodes */
    if (!(PMIX_HASH_NUM_NODES & flags)) {
        kp2 = PMIX_NEW(pmix_kval_t);
        kp2->key = strdup(PMIX_NUM_NODES);
        kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        kp2->value->type = PMIX_UINT32;
        kp2->value->data.uint32 = PMIx_Argv_count(nodes);
        pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                            "[%s:%d] gds:hash:store_map adding key %s to job info",
                            pmix_globals.myid.nspace, pmix_globals.myid.rank, kp2->key);
        if (PMIX_SUCCESS != (rc = pmix_hash_store(ht, PMIX_RANK_WILDCARD, kp2, NULL, 0, NULL))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(kp2);
            return rc;
        }
        PMIX_RELEASE(kp2); // maintain acctg
    }

    for (n = 0; NULL != nodes[n]; n++) {
        /* check and see if we already have this node */
        nd = pmix_gds_hash_check_nodename(&trk->nodeinfo, nodes[n]);
        if (NULL == nd) {
            nd = PMIX_NEW(pmix_nodeinfo_t);
            nd->hostname = strdup(nodes[n]);
            nd->nodeid = n;
            pmix_list_append(&trk->nodeinfo, &nd->super);
        }
        /* store the proc list as-is */
        kp2 = PMIX_NEW(pmix_kval_t);
        if (NULL == kp2) {
            return PMIX_ERR_NOMEM;
        }
        kp2->key = strdup(PMIX_LOCAL_PEERS);
        kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        if (NULL == kp2->value) {
            PMIX_RELEASE(kp2);
            return PMIX_ERR_NOMEM;
        }
        kp2->value->type = PMIX_STRING;
        kp2->value->data.string = strdup(ppn[n]);
        pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                            "[%s:%d] gds:hash:store_map adding key %s to node %s info",
                            pmix_globals.myid.nspace, pmix_globals.myid.rank, kp2->key, nodes[n]);
        /* ensure this item only appears once on the list */
        PMIX_LIST_FOREACH (kp1, &nd->info, pmix_kval_t) {
            if (PMIX_CHECK_KEY(kp1, kp2->key)) {
                pmix_list_remove_item(&nd->info, &kp1->super);
                PMIX_RELEASE(kp1);
                break;
            }
        }
        pmix_list_append(&nd->info, &kp2->super);

        /* save the local leader */
        rank = strtoul(ppn[n], NULL, 10);
        kp2 = PMIX_NEW(pmix_kval_t);
        if (NULL == kp2) {
            return PMIX_ERR_NOMEM;
        }
        kp2->key = strdup(PMIX_LOCALLDR);
        kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        if (NULL == kp2->value) {
            PMIX_RELEASE(kp2);
            return PMIX_ERR_NOMEM;
        }
        kp2->value->type = PMIX_PROC_RANK;
        kp2->value->data.rank = rank;
        pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                            "[%s:%d] gds:hash:store_map adding key %s to node %s info",
                            pmix_globals.myid.nspace, pmix_globals.myid.rank, kp2->key, nodes[n]);
        /* ensure this item only appears once on the list */
        PMIX_LIST_FOREACH (kp1, &nd->info, pmix_kval_t) {
            if (PMIX_CHECK_KEY(kp1, kp2->key)) {
                pmix_list_remove_item(&nd->info, &kp1->super);
                PMIX_RELEASE(kp1);
                break;
            }
        }
        pmix_list_append(&nd->info, &kp2->super);

        /* split the list of procs so we can store their
         * individual location data */
        procs = PMIx_Argv_split(ppn[n], ',');
        /* save the local size in case they don't
         * give it to us */
        kp2 = PMIX_NEW(pmix_kval_t);
        if (NULL == kp2) {
            PMIx_Argv_free(procs);
            return PMIX_ERR_NOMEM;
        }
        kp2->key = strdup(PMIX_LOCAL_SIZE);
        kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        if (NULL == kp2->value) {
            PMIX_RELEASE(kp2);
            PMIx_Argv_free(procs);
            return PMIX_ERR_NOMEM;
        }
        kp2->value->type = PMIX_UINT32;
        kp2->value->data.uint32 = PMIx_Argv_count(procs);
        pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                            "[%s:%d] gds:hash:store_map adding key %s to node %s info",
                            pmix_globals.myid.nspace, pmix_globals.myid.rank, kp2->key, nodes[n]);
        /* ensure this item only appears once on the list */
        PMIX_LIST_FOREACH (kp1, &nd->info, pmix_kval_t) {
            if (PMIX_CHECK_KEY(kp1, kp2->key)) {
                pmix_list_remove_item(&nd->info, &kp1->super);
                PMIX_RELEASE(kp1);
                break;
            }
        }
        pmix_list_append(&nd->info, &kp2->super);
        /* track total procs in job in case they
         * didn't give it to us */
        totalprocs += PMIx_Argv_count(procs);
        for (m = 0; NULL != procs[m]; m++) {
            /* store the hostname for each proc */
            kp2 = PMIX_NEW(pmix_kval_t);
            kp2->key = strdup(PMIX_HOSTNAME);
            kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
            kp2->value->type = PMIX_STRING;
            kp2->value->data.string = strdup(nodes[n]);
            rank = strtol(procs[m], NULL, 10);
            pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                                "[%s:%d] gds:hash:store_map for [%s:%u]: key %s",
                                pmix_globals.myid.nspace, pmix_globals.myid.rank, trk->ns, rank,
                                kp2->key);
            if (PMIX_SUCCESS != (rc = pmix_hash_store(ht, rank, kp2, NULL, 0, NULL))) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kp2);
                PMIx_Argv_free(procs);
                return rc;
            }
            PMIX_RELEASE(kp2); // maintain acctg
            if (!(PMIX_HASH_PROC_DATA & flags)) {
                /* add an entry for the nodeid */
                kp2 = PMIX_NEW(pmix_kval_t);
                kp2->key = strdup(PMIX_NODEID);
                kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
                kp2->value->type = PMIX_UINT32;
                pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                                    "[%s:%d] gds:hash:store_map for [%s:%u]: key %s",
                                    pmix_globals.myid.nspace, pmix_globals.myid.rank, trk->ns, rank,
                                    kp2->key);
                kp2->value->data.uint32 = n;
                if (PMIX_SUCCESS != (rc = pmix_hash_store(ht, rank, kp2, NULL, 0, NULL))) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(kp2);
                    PMIx_Argv_free(procs);
                    return rc;
                }
                PMIX_RELEASE(kp2); // maintain acctg
                /* add an entry for the local rank */
                kp2 = PMIX_NEW(pmix_kval_t);
                kp2->key = strdup(PMIX_LOCAL_RANK);
                kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
                kp2->value->type = PMIX_UINT16;
                kp2->value->data.uint16 = m;
                pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                                    "[%s:%d] gds:hash:store_map for [%s:%u]: key %s",
                                    pmix_globals.myid.nspace, pmix_globals.myid.rank, trk->ns, rank,
                                    kp2->key);
                if (PMIX_SUCCESS != (rc = pmix_hash_store(ht, rank, kp2, NULL, 0, NULL))) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(kp2);
                    PMIx_Argv_free(procs);
                    return rc;
                }
                PMIX_RELEASE(kp2); // maintain acctg
                /* add an entry for the node rank - for now, we assume
                 * only the one job is running */
                kp2 = PMIX_NEW(pmix_kval_t);
                kp2->key = strdup(PMIX_NODE_RANK);
                kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
                kp2->value->type = PMIX_UINT16;
                kp2->value->data.uint16 = m;
                pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                                    "[%s:%d] gds:hash:store_map for [%s:%u]: key %s",
                                    pmix_globals.myid.nspace, pmix_globals.myid.rank, trk->ns, rank,
                                    kp2->key);
                if (PMIX_SUCCESS != (rc = pmix_hash_store(ht, rank, kp2, NULL, 0, NULL))) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(kp2);
                    PMIx_Argv_free(procs);
                    return rc;
                }
                PMIX_RELEASE(kp2); // maintain acctg
            }
        }
        PMIx_Argv_free(procs);
    }

    /* store the comma-delimited list of nodes hosting
     * procs in this nspace in case someone using PMIx v2
     * requests it */
    kp2 = PMIX_NEW(pmix_kval_t);
    kp2->key = strdup(PMIX_NODE_LIST);
    kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
    kp2->value->type = PMIX_STRING;
    kp2->value->data.string = PMIx_Argv_join(nodes, ',');
    pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                        "[%s:%d] gds:hash:store_map for nspace %s: key %s",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank, trk->ns, kp2->key);
    if (PMIX_SUCCESS != (rc = pmix_hash_store(ht, PMIX_RANK_WILDCARD, kp2, NULL, 0, NULL))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(kp2);
        return rc;
    }
    PMIX_RELEASE(kp2); // maintain acctg

    /* if they didn't provide the job size, compute it as
     * being the number of provided procs (i.e., size of
     * ppn list) */
    if (!(PMIX_HASH_JOB_SIZE & flags)) {
        kp2 = PMIX_NEW(pmix_kval_t);
        kp2->key = strdup(PMIX_JOB_SIZE);
        kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        kp2->value->type = PMIX_UINT32;
        kp2->value->data.uint32 = totalprocs;
        pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                            "[%s:%d] gds:hash:store_map for nspace %s: key %s",
                            pmix_globals.myid.nspace, pmix_globals.myid.rank, trk->ns, kp2->key);
        if (PMIX_SUCCESS != (rc = pmix_hash_store(ht, PMIX_RANK_WILDCARD, kp2, NULL, 0, NULL))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(kp2);
            return rc;
        }
        PMIX_RELEASE(kp2); // maintain acctg
        flags |= PMIX_HASH_JOB_SIZE;
        trk->nptr->nprocs = totalprocs;
    }

    /* if they didn't provide a value for max procs, just
     * assume it is the same as the number of procs in the
     * job and store it */
    if (!(PMIX_HASH_MAX_PROCS & flags)) {
        kp2 = PMIX_NEW(pmix_kval_t);
        kp2->key = strdup(PMIX_MAX_PROCS);
        kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        kp2->value->type = PMIX_UINT32;
        kp2->value->data.uint32 = totalprocs;
        pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                            "[%s:%d] gds:hash:store_map for nspace %s: key %s",
                            pmix_globals.myid.nspace, pmix_globals.myid.rank, trk->ns, kp2->key);
        if (PMIX_SUCCESS != (rc = pmix_hash_store(ht, PMIX_RANK_WILDCARD, kp2, NULL, 0, NULL))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(kp2);
            return rc;
        }
        PMIX_RELEASE(kp2); // maintain acctg
        flags |= PMIX_HASH_MAX_PROCS;
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_gds_hash_store_qualified(pmix_hash_table_t *ht,
                                            pmix_rank_t rank,
                                            pmix_value_t *value)
{
    pmix_info_t *iptr, *quals;
    size_t n, sz, nquals;
    pmix_kval_t kv;
    pmix_status_t rc;

    /* the value contains a pmix_data_array_t whose first position
     * contains the key-value being stored, followed by one or more
     * qualifiers */
    iptr = (pmix_info_t*)value->data.darray->array;
    sz = value->data.darray->size;

    /* extract the primary value */
    PMIX_CONSTRUCT(&kv, pmix_kval_t);
    kv.key = iptr[0].key;
    kv.value = &iptr[0].value;

    nquals = sz - 1;
    PMIX_INFO_CREATE(quals, nquals);
    for (n=1; n < sz; n++) {
        PMIX_INFO_SET_QUALIFIER(&quals[n-1]);
        PMIX_INFO_XFER(&quals[n-1], &iptr[n]);
    }

    /* store the result */
    rc = pmix_hash_store(ht, rank, &kv, quals, nquals, NULL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }
    PMIX_INFO_FREE(quals, nquals);
    return rc;
}
