/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2018 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018-2020 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022-2023 Triad National Security, LLC. All rights reserved.
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

pmix_status_t pmix_gds_hash_xfer_sessioninfo(pmix_session_t *sptr,
                                             pmix_job_t *trk,
                                             const char *key,
                                             pmix_list_t *kvs)
{
    size_t n;
    pmix_kval_t *kv, *kp2;
    pmix_info_t *iptr;
    pmix_list_t *sessionlist = &sptr->sessioninfo;
    uint32_t sid = sptr->session;
    pmix_status_t rc;

    if (NULL == key) {
        if (trk->nptr->version.major < 4 ||
            (4 == trk->nptr->version.major &&
             1 == trk->nptr->version.minor)) {
            /* we can only transfer the data as independent values */
            PMIX_LIST_FOREACH(kv, sessionlist, pmix_kval_t) {
                kp2 = PMIX_NEW(pmix_kval_t);
                kp2->key = strdup(kv->key);
                PMIX_VALUE_XFER(rc, kp2->value, kv->value);
                if (PMIX_SUCCESS != rc) {
                    PMIX_RELEASE(kp2);
                    return rc;
                }
                pmix_list_append(kvs, &kp2->super);
            }
        } else {
            /* we return it as an info array */
            PMIX_KVAL_NEW(kp2, PMIX_SESSION_INFO_ARRAY);
            kp2->value->type = PMIX_DATA_ARRAY;
            n = pmix_list_get_size(sessionlist) + 1;
            PMIX_DATA_ARRAY_CREATE(kp2->value->data.darray, n, PMIX_INFO);
            iptr = (pmix_info_t*)kp2->value->data.darray->array;
            /* first element will be the session id */
            PMIX_INFO_LOAD(&iptr[0], PMIX_SESSION_ID, &sid, PMIX_UINT32);
            /* populate the rest of the array */
            n = 1;
            PMIX_LIST_FOREACH(kv, sessionlist, pmix_kval_t) {
                PMIX_LOAD_KEY(iptr[n].key, kv->key);
                rc = PMIx_Value_xfer(&iptr[n].value, kv->value);
                if (PMIX_SUCCESS != rc) {
                    PMIX_RELEASE(kp2);
                    return rc;
                }
                ++n;
            }
            pmix_list_append(kvs, &kp2->super);
        }
        return PMIX_SUCCESS;
    }

    /* we were given a specific key */
    PMIX_LIST_FOREACH(kv, sessionlist, pmix_kval_t) {
        if (PMIX_CHECK_KEY(kv, key)) {
            kp2 = PMIX_NEW(pmix_kval_t);
            kp2->key = strdup(kv->key);
            PMIX_VALUE_XFER(rc, kp2->value, kv->value);
            if (PMIX_SUCCESS != rc) {
                PMIX_RELEASE(kp2);
                return rc;
            }
            pmix_list_append(kvs, &kp2->super);
            return PMIX_SUCCESS;
        }
    }

    return PMIX_ERR_NOT_FOUND;
}

pmix_status_t pmix_gds_hash_fetch_sessioninfo(const char *key,
                                              pmix_job_t *trk,
                                              pmix_info_t *info, size_t ninfo,
                                              pmix_list_t *kvs)
{
    size_t n;
    pmix_status_t rc;
    uint32_t sid = UINT32_MAX;
    pmix_session_t *sptr;

    pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                        "FETCHING SESSION INFO");

    /* scan for the session ID to identify
     * which session they are asking about */
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_SESSION_ID)) {
            PMIX_VALUE_GET_NUMBER(rc, &info[n].value, sid, uint32_t);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
            break;
        }
    }

    sptr = pmix_gds_hash_check_session(trk, sid, false);
    if (NULL == sptr) {
        return PMIX_ERR_NOT_FOUND;
    }

    /* capture the info */
    rc = pmix_gds_hash_xfer_sessioninfo(sptr, trk, key, kvs);
    return rc;
}

pmix_status_t pmix_gds_hash_fetch_nodeinfo(const char *key, pmix_job_t *trk, pmix_list_t *tgt,
                                           pmix_info_t *info, size_t ninfo, pmix_list_t *kvs)
{
    size_t n, nds;
    pmix_status_t rc;
    uint32_t nid = UINT32_MAX;
    char *hostname = NULL;
    bool found = false;
    pmix_nodeinfo_t *nd, *ndptr;
    pmix_kval_t *kv, *kp2;
    pmix_data_array_t *darray;
    pmix_info_t *iptr;

    pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                        "FETCHING NODE INFO");

    /* scan for the nodeID or hostname to identify
     * which node they are asking about */
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_NODEID)) {
            PMIX_VALUE_GET_NUMBER(rc, &info[n].value, nid, uint32_t);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
            found = true;
            break;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_HOSTNAME)) {
            hostname = info[n].value.data.string;
            found = true;
            break;
        }
    }
    if (!found) {
        /* if the key is NULL, then they want all the info from
         * all nodes */
        if (NULL == key) {
            PMIX_LIST_FOREACH (nd, tgt, pmix_nodeinfo_t) {
                kv = PMIX_NEW(pmix_kval_t);
                /* if the proc's version is earlier than v3.1, then the
                 * info must be provided as a data_array with a key
                 * of the node's name as earlier versions don't understand
                 * node_info arrays */
                if (trk->nptr->version.major < 3 ||
                    (3 == trk->nptr->version.major &&
                     0 == trk->nptr->version.minor)) {
                    if (NULL == nd->hostname) {
                        /* skip this one */
                        continue;
                    }
                    kv->key = strdup(nd->hostname);
                } else {
                    /* everyone else uses a node_info array */
                    kv->key = strdup(PMIX_NODE_INFO_ARRAY);
                }
                kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
                if (NULL == kv->value) {
                    PMIX_RELEASE(kv);
                    return PMIX_ERR_NOMEM;
                }
                nds = pmix_list_get_size(&nd->info);
                if (NULL != nd->hostname) {
                    ++nds;
                }
                if (UINT32_MAX != nd->nodeid) {
                    ++nds;
                }
                PMIX_DATA_ARRAY_CREATE(darray, nds, PMIX_INFO);
                if (NULL == darray) {
                    PMIX_RELEASE(kv);
                    return PMIX_ERR_NOMEM;
                }
                iptr = (pmix_info_t *) darray->array;
                n = 0;
                if (NULL != nd->hostname) {
                    PMIX_INFO_LOAD(&iptr[n], PMIX_HOSTNAME, nd->hostname, PMIX_STRING);
                    ++n;
                }
                if (UINT32_MAX != nd->nodeid) {
                    PMIX_INFO_LOAD(&iptr[n], PMIX_NODEID, &nd->nodeid, PMIX_UINT32);
                    ++n;
                }
                PMIX_LIST_FOREACH (kp2, &nd->info, pmix_kval_t) {
                    pmix_output_verbose(12, pmix_gds_base_framework.framework_output,
                                        "%s gds:hash:fetch_nodearray adding key %s",
                                        PMIX_NAME_PRINT(&pmix_globals.myid), kp2->key);
                    PMIX_LOAD_KEY(iptr[n].key, kp2->key);
                    rc = PMIx_Value_xfer(&iptr[n].value, kp2->value);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DATA_ARRAY_FREE(darray);
                        PMIX_RELEASE(kv);
                        return rc;
                    }
                    ++n;
                }
                kv->value->data.darray = darray;
                kv->value->type = PMIX_DATA_ARRAY;
                pmix_list_append(kvs, &kv->super);
            }
            return PMIX_SUCCESS;
        }
        /* assume they want it from this node */
        hostname = pmix_globals.hostname;
    }

    /* scan the list of nodes to find the matching entry */
    nd = NULL;
    if (UINT32_MAX!= nid) {
        PMIX_LIST_FOREACH (ndptr, tgt, pmix_nodeinfo_t) {
            if (UINT32_MAX != ndptr->nodeid &&
                nid == ndptr->nodeid) {
                nd = ndptr;
                break;
            }
        }
    } else if (NULL != hostname) {
        nd = pmix_gds_hash_check_nodename(tgt, hostname);
    }
    if (NULL == nd) {
        if (!found) {
            /* they didn't specify, so it is optional */
            return PMIX_ERR_DATA_VALUE_NOT_FOUND;
        }
        return PMIX_ERR_NOT_FOUND;
    }

    /* if they want it all, give it to them */
    if (NULL == key) {
        kv = PMIX_NEW(pmix_kval_t);
        /* if the proc's version is earlier than v3.1, then the
         * info must be provided as a data_array with a key
         * of the node's name as earlier versions don't understand
         * node_info arrays */
        if (trk->nptr->version.major < 3 ||
            (3 == trk->nptr->version.major &&
             0 == trk->nptr->version.minor)) {
            if (NULL == nd->hostname) {
                kv->key = strdup(pmix_globals.hostname);
            } else {
                kv->key = strdup(nd->hostname);
            }
        } else {
            /* everyone else uses a node_info array */
            kv->key = strdup(PMIX_NODE_INFO_ARRAY);
        }
        kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            return PMIX_ERR_NOMEM;
        }
        nds = pmix_list_get_size(&nd->info);
        if (NULL != nd->hostname) {
            ++nds;
        }
        if (UINT32_MAX != nd->nodeid) {
            ++nds;
        }
        PMIX_DATA_ARRAY_CREATE(darray, nds, PMIX_INFO);
        if (NULL == darray) {
            PMIX_RELEASE(kv);
            return PMIX_ERR_NOMEM;
        }
        iptr = (pmix_info_t *) darray->array;
        n = 0;
        if (NULL != nd->hostname) {
            PMIX_INFO_LOAD(&iptr[n], PMIX_HOSTNAME, nd->hostname, PMIX_STRING);
            ++n;
        }
        if (UINT32_MAX != nd->nodeid) {
            PMIX_INFO_LOAD(&iptr[n], PMIX_NODEID, &nd->nodeid, PMIX_UINT32);
            ++n;
        }
        PMIX_LIST_FOREACH (kp2, &nd->info, pmix_kval_t) {
            pmix_output_verbose(12, pmix_gds_base_framework.framework_output,
                                "%s gds:hash:fetch_nodearray adding key %s",
                                PMIX_NAME_PRINT(&pmix_globals.myid), kp2->key);
            PMIX_LOAD_KEY(iptr[n].key, kp2->key);
            rc = PMIx_Value_xfer(&iptr[n].value, kp2->value);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_ARRAY_FREE(darray);
                PMIX_RELEASE(kv);
                return rc;
            }
            ++n;
        }
        kv->value->data.darray = darray;
        kv->value->type = PMIX_DATA_ARRAY;
        pmix_list_append(kvs, &kv->super);
        return PMIX_SUCCESS;
    }

    /* scan the info list of this node to find the key they want */
    rc = PMIX_ERR_NOT_FOUND;
    PMIX_LIST_FOREACH (kp2, &nd->info, pmix_kval_t) {
        if (PMIX_CHECK_KEY(kp2, key)) {
            pmix_output_verbose(12, pmix_gds_base_framework.framework_output,
                                "%s gds:hash:fetch_nodearray adding key %s",
                                PMIX_NAME_PRINT(&pmix_globals.myid), kp2->key);
            /* since they only asked for one key, return just that value */
            kv = PMIX_NEW(pmix_kval_t);
            kv->key = strdup(kp2->key);
            kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
            if (NULL == kv->value) {
                PMIX_RELEASE(kv);
                return PMIX_ERR_NOMEM;
            }
            rc = PMIx_Value_xfer(kv->value, kp2->value);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kv);
                return rc;
            }
            pmix_list_append(kvs, &kv->super);
            break;
        }
    }
    return rc;
}

pmix_status_t pmix_gds_hash_fetch_appinfo(const char *key, pmix_job_t *trk, pmix_list_t *tgt,
                                          pmix_info_t *info, size_t ninfo, pmix_list_t *kvs)
{
    size_t n, nds;
    pmix_status_t rc;
    uint32_t appnum;
    bool found = false;
    pmix_apptrkr_t *app, *apptr;
    pmix_kval_t *kv, *kp2;
    pmix_data_array_t *darray;

    pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                        "FETCHING APP INFO WITH %d APPS",
                        (int) pmix_list_get_size(tgt));

    /* scan for the appnum to identify
     * which app they are asking about */
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_APPNUM)) {
            PMIX_VALUE_GET_NUMBER(rc, &info[n].value, appnum, uint32_t);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
            found = true;
            break;
        }
    }
    if (!found) {
        /* if the key is NULL, then they want all the info from
         * all apps */
        if (NULL == key) {
            PMIX_LIST_FOREACH (apptr, tgt, pmix_apptrkr_t) {
                kv = PMIX_NEW(pmix_kval_t);
                kv->key = strdup(PMIX_APP_INFO_ARRAY);
                kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
                if (NULL == kv->value) {
                    PMIX_RELEASE(kv);
                    return PMIX_ERR_NOMEM;
                }
                nds = pmix_list_get_size(&apptr->appinfo) + 1;
                PMIX_DATA_ARRAY_CREATE(darray, nds, PMIX_INFO);
                if (NULL == darray) {
                    PMIX_RELEASE(kv);
                    return PMIX_ERR_NOMEM;
                }
                info = (pmix_info_t *) darray->array;
                n = 0;
                /* put in the appnum */
                PMIX_INFO_LOAD(&info[n], PMIX_APPNUM, &apptr->appnum, PMIX_UINT32);
                ++n;
                PMIX_LIST_FOREACH (kp2, &apptr->appinfo, pmix_kval_t) {
                    PMIX_LOAD_KEY(info[n].key, kp2->key);
                    rc = PMIx_Value_xfer(&info[n].value, kp2->value);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DATA_ARRAY_FREE(darray);
                        PMIX_RELEASE(kv);
                        return rc;
                    }
                    ++n;
                }
                kv->value->data.darray = darray;
                kv->value->type = PMIX_DATA_ARRAY;
                pmix_list_append(kvs, &kv->super);
            }
            return PMIX_SUCCESS;
        }
        /* assume they are asking for our app */
        appnum = pmix_globals.appnum;
    }

    /* scan the list of apps to find the matching entry */
    app = NULL;
    PMIX_LIST_FOREACH (apptr, tgt, pmix_apptrkr_t) {
        if (appnum == apptr->appnum) {
            app = apptr;
            break;
        }
    }
    if (NULL == app) {
        return PMIX_ERR_NOT_FOUND;
    }

    /* see if they wanted to know something about a node that
     * is associated with this app */
    rc = pmix_gds_hash_fetch_nodeinfo(key, trk, &app->nodeinfo, info, ninfo, kvs);
    if (PMIX_ERR_DATA_VALUE_NOT_FOUND != rc) {
        return rc;
    }

    /* scan the info list of this app to generate the results */
    rc = PMIX_ERR_NOT_FOUND;
    PMIX_LIST_FOREACH (kv, &app->appinfo, pmix_kval_t) {
        if (NULL == key || PMIX_CHECK_KEY(kv, key)) {
            kp2 = PMIX_NEW(pmix_kval_t);
            kp2->key = strdup(kv->key);
            kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
            rc = PMIx_Value_xfer(kp2->value, kv->value);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kp2);
                return rc;
            }
            pmix_list_append(kvs, &kp2->super);
            rc = PMIX_SUCCESS;
            if (NULL != key) {
                break;
            }
        }
    }

    return rc;
}

pmix_status_t pmix_gds_hash_fetch(const pmix_proc_t *proc, pmix_scope_t scope, bool copy,
                                  const char *key, pmix_info_t qualifiers[], size_t nqual,
                                  pmix_list_t *kvs)
{
    pmix_job_t *trk;
    pmix_status_t rc;
    pmix_kval_t *kv, *kvptr;
    pmix_info_t *iptr;
    size_t n, ninfo, niptr;
    pmix_hash_table_t *ht;
    pmix_rank_t rnk;
    pmix_list_t rkvs;
    bool sessioninfo = false;
    bool nodeinfo = false;
    bool appinfo = false;
    bool sidgiven = false;
    bool nigiven = false;
    bool apigiven = false;

    pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                        "%s pmix:gds:hash fetch %s for proc %s on scope %s",
                        PMIX_NAME_PRINT(&pmix_globals.myid), (NULL == key) ? "NULL" : key,
                        PMIX_NAME_PRINT(proc), PMIx_Scope_string(scope));


    PMIX_HIDE_UNUSED_PARAMS(copy);

    /* see if we have a tracker for this nspace - we will
     * if we already cached the job info for it. If we
     * didn't then we'll have no idea how to answer any
     * questions */
    trk = pmix_gds_hash_get_tracker(proc->nspace, false);
    if (NULL == trk) {
        /* let the caller know */
        return PMIX_ERR_INVALID_NAMESPACE;
    }

    /* if the rank is wildcard and the key is NULL, then
     * they are asking for a complete copy of the job-level
     * info for this nspace - retrieve it */
    if (NULL == key && PMIX_RANK_WILDCARD == proc->rank) {
        /* fetch all values from the hash table tied to rank=wildcard */
        rc = pmix_hash_fetch(&trk->internal, PMIX_RANK_WILDCARD, NULL, NULL, 0, kvs, NULL);
        if (PMIX_SUCCESS != rc && PMIX_ERR_NOT_FOUND != rc) {
            return rc;
        }
        /* also need to add any job-level info */
        PMIX_LIST_FOREACH (kvptr, &trk->jobinfo, pmix_kval_t) {
            kv = PMIX_NEW(pmix_kval_t);
            kv->key = strdup(kvptr->key);
            kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
            PMIX_VALUE_XFER(rc, kv->value, kvptr->value);
            if (PMIX_SUCCESS != rc) {
                PMIX_RELEASE(kv);
                return rc;
            }
            pmix_list_append(kvs, &kv->super);
        }
        /* collect all the relevant session-level info */
        rc = pmix_gds_hash_fetch_sessioninfo(NULL, trk, qualifiers, nqual, kvs);
        if (PMIX_SUCCESS != rc && PMIX_ERR_NOT_FOUND != rc) {
            return rc;
        }
        /* collect the relevant node-level info */
        rc = pmix_gds_hash_fetch_nodeinfo(NULL, trk, &trk->nodeinfo, qualifiers, nqual, kvs);
        if (PMIX_SUCCESS != rc && PMIX_ERR_NOT_FOUND != rc) {
            return rc;
        }
        /* collect the relevant app-level info */
        rc = pmix_gds_hash_fetch_appinfo(NULL, trk, &trk->apps, qualifiers, nqual, kvs);
        if (PMIX_SUCCESS != rc && PMIX_ERR_NOT_FOUND != rc) {
            return rc;
        }
        /* finally, we need the job-level info for each rank in the job */
        for (rnk = 0; rnk < trk->nptr->nprocs; rnk++) {
            PMIX_CONSTRUCT(&rkvs, pmix_list_t);
            rc = pmix_hash_fetch(&trk->internal, rnk, NULL, NULL, 0, &rkvs, NULL);
            if (PMIX_ERR_NOMEM == rc) {
                PMIX_LIST_DESTRUCT(&rkvs);
                return rc;
            }
            if (0 == pmix_list_get_size(&rkvs)) {
                PMIX_DESTRUCT(&rkvs);
                continue;
            }
            ninfo = pmix_list_get_size(&rkvs);
            /* setup to return the result */
            PMIX_KVAL_NEW(kv, PMIX_PROC_DATA);
            kv->value->type = PMIX_DATA_ARRAY;
            niptr = ninfo + 1; // need space for the rank
            PMIX_DATA_ARRAY_CREATE(kv->value->data.darray, niptr, PMIX_INFO);
            iptr = (pmix_info_t*)kv->value->data.darray->array;
            /* start with the rank */
            PMIX_INFO_LOAD(&iptr[0], PMIX_RANK, &rnk, PMIX_PROC_RANK);
            /* now transfer rest of data across */
            n = 1;
            PMIX_LIST_FOREACH(kvptr, &rkvs, pmix_kval_t) {
                PMIX_LOAD_KEY(iptr[n].key, kvptr->key);
                PMIx_Value_xfer(&iptr[n].value, kvptr->value);
                ++n;
            }
            /* add to the results */
            pmix_list_append(kvs, &kv->super);
            /* release the search result */
            PMIX_LIST_DESTRUCT(&rkvs);
        }
        return PMIX_SUCCESS;
    }

    /* see if they are asking for session, node, or app-level info */
    for (n = 0; n < nqual; n++) {
        if (PMIX_CHECK_KEY(&qualifiers[n], PMIX_SESSION_INFO)) {
            sessioninfo = PMIX_INFO_TRUE(&qualifiers[n]);
            sidgiven = true;
        } else if (PMIX_CHECK_KEY(&qualifiers[n], PMIX_NODE_INFO)) {
            nodeinfo = PMIX_INFO_TRUE(&qualifiers[n]);
            nigiven = true;
        } else if (PMIX_CHECK_KEY(&qualifiers[n], PMIX_APP_INFO)) {
            appinfo = PMIX_INFO_TRUE(&qualifiers[n]);
            apigiven = true;
        }
    }

    /* check for session/node/app keys in the absence of corresponding qualifier */
    if (NULL != key && !sidgiven && !nigiven && !apigiven) {
        if (pmix_check_session_info(key)) {
            sessioninfo = true;
        } else if (pmix_check_node_info(key)) {
            nodeinfo = true;
        } else if (pmix_check_app_info(key)) {
            appinfo = true;
        }
    }

    if (sessioninfo) {
        rc = pmix_gds_hash_fetch_sessioninfo(key, trk, qualifiers, nqual, kvs);
        return rc;
    }

    if (!PMIX_RANK_IS_VALID(proc->rank)) {
        if (nodeinfo) {
            rc = pmix_gds_hash_fetch_nodeinfo(key, trk, &trk->nodeinfo, qualifiers, nqual, kvs);
            if (PMIX_SUCCESS != rc && PMIX_RANK_WILDCARD == proc->rank) {
                /* need to check internal as we might have an older peer */
                ht = &trk->internal;
                goto doover;
            }
            return rc;
        } else if (appinfo) {
            rc = pmix_gds_hash_fetch_appinfo(key, trk, &trk->apps, qualifiers, nqual, kvs);
            if (PMIX_SUCCESS != rc && PMIX_RANK_WILDCARD == proc->rank) {
                /* need to check internal as we might have an older peer */
                ht = &trk->internal;
                goto doover;
            }
            return rc;
        }
    }

    /* fetch from the corresponding hash table - note that
     * we always provide a copy as we don't support
     * shared memory */
    if (PMIX_INTERNAL == scope ||
        PMIX_SCOPE_UNDEF == scope ||
        PMIX_GLOBAL == scope ||
        PMIX_RANK_WILDCARD == proc->rank) {
        ht = &trk->internal;
    } else if (PMIX_LOCAL == scope ||
               PMIX_GLOBAL == scope) {
        ht = &trk->local;
    } else if (PMIX_REMOTE == scope) {
        ht = &trk->remote;
    } else {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }

doover:
    /* if rank=PMIX_RANK_UNDEF, then we need to search all
     * known ranks for this nspace as any one of them could
     * be the source */
    if (PMIX_RANK_UNDEF == proc->rank) {
        for (rnk = 0; rnk < trk->nptr->nprocs; rnk++) {
            rc = pmix_hash_fetch(ht, rnk, key, qualifiers, nqual, kvs, NULL);
            if (PMIX_ERR_NOMEM == rc) {
                return rc;
            }
            if (PMIX_SUCCESS == rc && NULL != key) {
                return rc;
            }
        }
        /* also need to check any job-level info */
        PMIX_LIST_FOREACH (kvptr, &trk->jobinfo, pmix_kval_t) {
            if (NULL == key || PMIX_CHECK_KEY(kvptr, key)) {
                kv = PMIX_NEW(pmix_kval_t);
                kv->key = strdup(kvptr->key);
                kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
                PMIX_VALUE_XFER(rc, kv->value, kvptr->value);
                if (PMIX_SUCCESS != rc) {
                    PMIX_RELEASE(kv);
                    return rc;
                }
                pmix_list_append(kvs, &kv->super);
                if (NULL != key) {
                    break;
                }
            }
        }
        if (NULL == key) {
            /* and need to add all job info just in case that was
             * passed via a different GDS component */
            rc = pmix_hash_fetch(&trk->internal, PMIX_RANK_WILDCARD, NULL, NULL, 0, kvs, NULL);
        } else {
            rc = PMIX_ERR_NOT_FOUND;
        }
    } else {
        rc = pmix_hash_fetch(ht, proc->rank, key, qualifiers, nqual, kvs, NULL);
    }
    if (PMIX_SUCCESS == rc) {
        if (PMIX_GLOBAL == scope) {
            if (ht == &trk->local) {
                /* need to do this again for the remote data */
                ht = &trk->remote;
                goto doover;
            } else if (ht == &trk->internal) {
                /* check local */
                ht = &trk->local;
                goto doover;
            }
        }
    } else {
        if (PMIX_GLOBAL == scope || PMIX_SCOPE_UNDEF == scope) {
            if (ht == &trk->internal) {
                /* need to also try the local data */
                ht = &trk->local;
                goto doover;
            } else if (ht == &trk->local) {
                /* need to also try the remote data */
                ht = &trk->remote;
                goto doover;
            }
        }
    }
    if (0 == pmix_list_get_size(kvs)) {
        /* if we didn't find it and the rank was valid, then
         * check to see if the data exists in a different scope.
         * This is done to avoid having the process go into a
         * timeout wait when the data will never appear within
         * the specified scope */
        if (PMIX_RANK_IS_VALID(proc->rank)) {
            if (PMIX_LOCAL == scope) {
                /* check the remote scope */
                rc = pmix_hash_fetch(&trk->remote, proc->rank, key, qualifiers, nqual, kvs, NULL);
                if (PMIX_SUCCESS == rc || 0 < pmix_list_get_size(kvs)) {
                    while (NULL != (kv = (pmix_kval_t *) pmix_list_remove_first(kvs))) {
                        PMIX_RELEASE(kv);
                    }
                    rc = PMIX_ERR_EXISTS_OUTSIDE_SCOPE;
                } else {
                    rc = PMIX_ERR_NOT_FOUND;
                }
            } else if (PMIX_REMOTE == scope) {
                /* check the local scope */
                rc = pmix_hash_fetch(&trk->local, proc->rank, key, qualifiers, nqual, kvs, NULL);
                if (PMIX_SUCCESS == rc || 0 < pmix_list_get_size(kvs)) {
                    while (NULL != (kv = (pmix_kval_t *) pmix_list_remove_first(kvs))) {
                        PMIX_RELEASE(kv);
                    }
                    rc = PMIX_ERR_EXISTS_OUTSIDE_SCOPE;
                } else {
                    rc = PMIX_ERR_NOT_FOUND;
                }
            }
        } else {
            rc = PMIX_ERR_NOT_FOUND;
        }
    }

    return rc;
}

pmix_status_t pmix_gds_hash_fetch_arrays(struct pmix_peer_t *pr, pmix_buffer_t *reply)
{
    pmix_peer_t *peer = (pmix_peer_t *) pr;
    pmix_namespace_t *ns = peer->nptr;
    pmix_job_t *trk;
    pmix_list_t kvs;
    pmix_kval_t *kv;
    pmix_status_t rc;

    if (!PMIX_PEER_IS_SERVER(pmix_globals.mypeer) &&
        !PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        /* this function is only available on servers */
        PMIX_ERROR_LOG(PMIX_ERR_NOT_SUPPORTED);
        return PMIX_ERR_NOT_SUPPORTED;
    }

     pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                "%s pmix:gds:hash fetch arrays for proc [%s:%u]",
                PMIX_NAME_PRINT(&pmix_globals.myid),
                peer->info->pname.nspace, peer->info->pname.rank);

    /* see if we have a tracker for this nspace - we will
     * if we already cached the job info for it. If we
     * didn't then we'll have no idea how to answer any
     * questions */
    trk = pmix_gds_hash_get_tracker(ns->nspace, false);
    if (NULL == trk) {
        /* let the caller know */
        return PMIX_ERR_INVALID_NAMESPACE;
    }
    PMIX_CONSTRUCT(&kvs, pmix_list_t);

    rc = pmix_gds_hash_fetch_sessioninfo(NULL, trk, NULL, 0, &kvs);
    if (PMIX_SUCCESS != rc && PMIX_ERR_NOT_FOUND != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_LIST_DESTRUCT(&kvs);
        return rc;
    }

    rc = pmix_gds_hash_fetch_nodeinfo(NULL, trk, &trk->nodeinfo, NULL, 0, &kvs);
    if (PMIX_SUCCESS != rc && PMIX_ERR_NOT_FOUND != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_LIST_DESTRUCT(&kvs);
        return rc;
    }

    rc = pmix_gds_hash_fetch_appinfo(NULL, trk, &trk->apps, NULL, 0, &kvs);
    if (PMIX_SUCCESS != rc && PMIX_ERR_NOT_FOUND != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_LIST_DESTRUCT(&kvs);
        return rc;
    }

    /* pack the results */
    while (NULL != (kv = (pmix_kval_t*)pmix_list_remove_first(&kvs))) {
        PMIX_BFROPS_PACK(rc, peer, reply, kv, 1, PMIX_KVAL);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            break;
        }
    }
    PMIX_LIST_DESTRUCT(&kvs);
    return rc;
}
