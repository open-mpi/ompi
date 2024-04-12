/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2018 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018-2020 Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights reserved.
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

/* process a node array - contains an array of
 * node-level info for a single node. Either the
 * nodeid, hostname, or both must be included
 * in the array to identify the node */
pmix_status_t pmix_gds_hash_process_node_array(pmix_value_t *val, pmix_list_t *tgt)
{
    size_t size, j, n;
    pmix_info_t *iptr;
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_kval_t *kp2, *k1;
    pmix_list_t cache;
    pmix_nodeinfo_t *nd = NULL, *ndptr;
    bool update;

    pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                        "PROCESSING NODE ARRAY");

    /* array of node-level info for a specific node */
    if (PMIX_DATA_ARRAY != val->type) {
        PMIX_ERROR_LOG(PMIX_ERR_TYPE_MISMATCH);
        return PMIX_ERR_TYPE_MISMATCH;
    }

    /* setup arrays */
    size = val->data.darray->size;
    iptr = (pmix_info_t *) val->data.darray->array;
    PMIX_CONSTRUCT(&cache, pmix_list_t);

    /* cache the values while searching for the nodeid
     * and/or hostname */
    for (j = 0; j < size; j++) {
        pmix_output_verbose(12, pmix_gds_base_framework.framework_output,
                            "%s gds:hash:node_array for key %s",
                            PMIX_NAME_PRINT(&pmix_globals.myid), iptr[j].key);
        if (PMIX_CHECK_KEY(&iptr[j], PMIX_NODEID)) {
            if (NULL == nd) {
                nd = PMIX_NEW(pmix_nodeinfo_t);
            }
            PMIX_VALUE_GET_NUMBER(rc, &iptr[j].value, nd->nodeid, uint32_t);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(nd);
                PMIX_LIST_DESTRUCT(&cache);
                return rc;
            }
        } else if (PMIX_CHECK_KEY(&iptr[j], PMIX_HOSTNAME)) {
            if (NULL == nd) {
                nd = PMIX_NEW(pmix_nodeinfo_t);
            }
            nd->hostname = strdup(iptr[j].value.data.string);
        } else if (PMIX_CHECK_KEY(&iptr[j], PMIX_HOSTNAME_ALIASES)) {
            if (NULL == nd) {
                nd = PMIX_NEW(pmix_nodeinfo_t);
            }
            nd->aliases = PMIx_Argv_split(iptr[j].value.data.string, ',');
            /* need to cache this value as well */
            kp2 = PMIX_NEW(pmix_kval_t);
            kp2->key = strdup(iptr[j].key);
            kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
            PMIX_VALUE_XFER(rc, kp2->value, &iptr[j].value);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kp2);
                PMIX_RELEASE(nd);
                PMIX_LIST_DESTRUCT(&cache);
                return rc;
            }
            pmix_list_append(&cache, &kp2->super);
        } else {
            kp2 = PMIX_NEW(pmix_kval_t);
            kp2->key = strdup(iptr[j].key);
            kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
            PMIX_VALUE_XFER(rc, kp2->value, &iptr[j].value);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kp2);
                if (NULL != nd) {
                    PMIX_RELEASE(nd);
                }
                PMIX_LIST_DESTRUCT(&cache);
                return rc;
            }
            pmix_list_append(&cache, &kp2->super);
        }
    }

    if (NULL == nd) {
        /* they forgot to pass us the ident for the node */
        PMIX_LIST_DESTRUCT(&cache);
        return PMIX_ERR_BAD_PARAM;
    }

    /* see if we already have this node on the
     * provided list */
    update = false;
    PMIX_LIST_FOREACH (ndptr, tgt, pmix_nodeinfo_t) {
        if (UINT32_MAX != ndptr->nodeid &&
            UINT32_MAX != nd->nodeid) {
            if (ndptr->nodeid == nd->nodeid) {
                if (NULL == ndptr->hostname &&
                    NULL != nd->hostname) {
                    ndptr->hostname = strdup(nd->hostname);
                }
                if (NULL != nd->aliases) {
                    for (n=0; NULL != nd->aliases[n]; n++) {
                        PMIx_Argv_append_unique_nosize(&ndptr->aliases, nd->aliases[n]);
                    }
                }
                PMIX_RELEASE(nd);
                nd = ndptr;
                update = true;
                break;
            }
        } else if (NULL != ndptr->hostname &&
                   NULL != nd->hostname) {
            if (0 == strcmp(ndptr->hostname, nd->hostname)) {
                if (UINT32_MAX == ndptr->nodeid &&
                    UINT32_MAX != nd->nodeid) {
                    ndptr->nodeid = nd->nodeid;
                }
                if (NULL != nd->aliases) {
                    for (n=0; NULL != nd->aliases[n]; n++) {
                        PMIx_Argv_append_unique_nosize(&ndptr->aliases, nd->aliases[n]);
                    }
                }
                PMIX_RELEASE(nd);
                nd = ndptr;
                update = true;
                break;
            }
        }
    }
    if (!update) {
        pmix_list_append(tgt, &nd->super);
    }

    /* transfer the cached items to the nodeinfo list */
    kp2 = (pmix_kval_t *) pmix_list_remove_first(&cache);
    while (NULL != kp2) {
        /* if this is an update, we have to ensure each data
         * item only appears once on the list */
        if (update) {
            PMIX_LIST_FOREACH (k1, &nd->info, pmix_kval_t) {
                if (PMIX_CHECK_KEY(k1, kp2->key)) {
                    pmix_list_remove_item(&nd->info, &k1->super);
                    PMIX_RELEASE(k1);
                    break;
                }
            }
        }
        pmix_list_append(&nd->info, &kp2->super);
        kp2 = (pmix_kval_t *) pmix_list_remove_first(&cache);
    }
    PMIX_LIST_DESTRUCT(&cache);

    return PMIX_SUCCESS;
}

/* process an app array - contains an array of
 * app-level info for a single app. If the
 * appnum is not included in the array, then
 * it is assumed that only one app is in the job.
 * This assumption is checked and generates
 * an error if violated */
pmix_status_t pmix_gds_hash_process_app_array(pmix_value_t *val, pmix_job_t *trk)
{
    pmix_list_t cache, ncache;
    size_t size, j;
    pmix_info_t *iptr;
    pmix_status_t rc = PMIX_SUCCESS;
    uint32_t appnum;
    pmix_apptrkr_t *app = NULL, *apptr;
    pmix_kval_t *kp2, *k1;
    pmix_nodeinfo_t *nd;
    bool update;

    pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                        "PROCESSING APP ARRAY");

    /* apps have to belong to a job */
    if (NULL == trk) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* array of app-level info */
    if (PMIX_DATA_ARRAY != val->type) {
        PMIX_ERROR_LOG(PMIX_ERR_TYPE_MISMATCH);
        return PMIX_ERR_TYPE_MISMATCH;
    }

    /* setup arrays and lists */
    PMIX_CONSTRUCT(&cache, pmix_list_t);
    PMIX_CONSTRUCT(&ncache, pmix_list_t);
    size = val->data.darray->size;
    iptr = (pmix_info_t *) val->data.darray->array;

    for (j = 0; j < size; j++) {
        pmix_output_verbose(12, pmix_gds_base_framework.framework_output,
                            "%s gds:hash:app_array for key %s",
                            PMIX_NAME_PRINT(&pmix_globals.myid),
                            iptr[j].key);
        if (PMIX_CHECK_KEY(&iptr[j], PMIX_APPNUM)) {
            PMIX_VALUE_GET_NUMBER(rc, &iptr[j].value, appnum, uint32_t);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                goto release;
            }
            if (NULL != app) {
                /* this is an error - there can be only one app
                 * described in this array */
                PMIX_RELEASE(app);
                PMIX_LIST_DESTRUCT(&cache);
                PMIX_LIST_DESTRUCT(&ncache);
                return PMIX_ERR_BAD_PARAM;
            }
            app = PMIX_NEW(pmix_apptrkr_t);
            app->appnum = appnum;
        } else if (PMIX_CHECK_KEY(&iptr[j], PMIX_NODE_INFO_ARRAY)) {
            if (PMIX_SUCCESS != (rc = pmix_gds_hash_process_node_array(&iptr[j].value, &ncache))) {
                PMIX_ERROR_LOG(rc);
                goto release;
            }
        } else {
            kp2 = PMIX_NEW(pmix_kval_t);
            kp2->key = strdup(iptr[j].key);
            kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
            PMIX_VALUE_XFER(rc, kp2->value, &iptr[j].value);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kp2);
                goto release;
            }
            pmix_list_append(&cache, &kp2->super);
        }
    }
    if (NULL == app) {
        /* per the standard, they don't have to provide us with
         * an appnum so long as only one app is in the job */
        if (0 == pmix_list_get_size(&trk->apps)) {
            app = PMIX_NEW(pmix_apptrkr_t);
            app->appnum = 0;
        } else {
            /* this is not allowed to happen - they are required
             * to provide us with an app number per the standard */
            rc = PMIX_ERR_BAD_PARAM;
            PMIX_ERROR_LOG(rc);
            goto release;
        }
    }
    /* see if we already have this app on the
     * provided list */
    update = false;
    PMIX_LIST_FOREACH (apptr, &trk->apps, pmix_apptrkr_t) {
        if (apptr->appnum == app->appnum) {
            /* we assume that the data is updating the current
             * values */
            PMIX_RELEASE(app);
            app = apptr;
            update = true;
            break;
        }
    }
    if (!update) {
        pmix_list_append(&trk->apps, &app->super);
    }
    /* point the app at its job */
    if (NULL == app->job) {
        /* do NOT retain the tracker - we will not release
         * it in the app destructor. If we retain the tracker,
         * then we won't release it later because the refcount
         * is wrong */
        app->job = trk;
    }

    /* transfer the app-level data across */
    kp2 = (pmix_kval_t *) pmix_list_remove_first(&cache);
    while (NULL != kp2) {
        /* if this is an update, we have to ensure each data
         * item only appears once on the list */
        if (update) {
            PMIX_LIST_FOREACH (k1, &app->appinfo, pmix_kval_t) {
                if (PMIX_CHECK_KEY(k1, kp2->key)) {
                    pmix_list_remove_item(&app->appinfo, &k1->super);
                    PMIX_RELEASE(k1);
                    break;
                }
            }
        }
        if (PMIX_CHECK_KEY(kp2, PMIX_MODEL_LIBRARY_NAME) ||
            PMIX_CHECK_KEY(kp2, PMIX_PROGRAMMING_MODEL) ||
            PMIX_CHECK_KEY(kp2, PMIX_MODEL_LIBRARY_VERSION) ||
            PMIX_CHECK_KEY(kp2, PMIX_PERSONALITY)) {
            // pass this info to the pmdl framework
            pmix_pmdl.setup_nspace_kv(trk->nptr, kp2);
        }
        pmix_list_append(&app->appinfo, &kp2->super);
        kp2 = (pmix_kval_t *) pmix_list_remove_first(&cache);
    }
    /* transfer the associated node-level data across */
    nd = (pmix_nodeinfo_t *) pmix_list_remove_first(&ncache);
    while (NULL != nd) {
        pmix_list_append(&app->nodeinfo, &nd->super);
        nd = (pmix_nodeinfo_t *) pmix_list_remove_first(&ncache);
    }

release:
    PMIX_LIST_DESTRUCT(&cache);
    PMIX_LIST_DESTRUCT(&ncache);

    return rc;
}

/* process a job array */
pmix_status_t pmix_gds_hash_process_job_array(pmix_info_t *info, pmix_job_t *trk, uint32_t *flags,
                                              char ***procs, char ***nodes)
{
    pmix_list_t cache;
    size_t j, size;
    pmix_info_t *iptr;
    pmix_kval_t *kp2;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_gds_base_framework.framework_output,
                        "PROCESSING JOB ARRAY");

    /* array of job-level info */
    if (PMIX_DATA_ARRAY != info->value.type) {
        PMIX_ERROR_LOG(PMIX_ERR_TYPE_MISMATCH);
        return PMIX_ERR_TYPE_MISMATCH;
    }
    size = info->value.data.darray->size;
    iptr = (pmix_info_t *) info->value.data.darray->array;
    PMIX_CONSTRUCT(&cache, pmix_list_t);
    for (j = 0; j < size; j++) {
        if (PMIX_CHECK_KEY(&iptr[j], PMIX_APP_INFO_ARRAY)) {
            if (PMIX_SUCCESS != (rc = pmix_gds_hash_process_app_array(&iptr[j].value, trk))) {
                return rc;
            }
        } else if (PMIX_CHECK_KEY(&iptr[j], PMIX_NODE_INFO_ARRAY)) {
            if (PMIX_SUCCESS
                != (rc = pmix_gds_hash_process_node_array(&iptr[j].value, &trk->nodeinfo))) {
                PMIX_ERROR_LOG(rc);
                return rc;
            }
        } else if (PMIX_CHECK_KEY(&iptr[j], PMIX_PROC_MAP)) {
            /* not allowed to get this more than once */
            if (*flags & PMIX_HASH_PROC_MAP) {
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                return PMIX_ERR_BAD_PARAM;
            }
            /* parse the regex to get the argv array containing proc ranks on each node */
            if (PMIX_SUCCESS != (rc = pmix_preg.parse_procs(iptr[j].value.data.bo.bytes, procs))) {
                PMIX_ERROR_LOG(rc);
                return rc;
            }
            /* mark that we got the map */
            *flags |= PMIX_HASH_PROC_MAP;
        } else if (PMIX_CHECK_KEY(&iptr[j], PMIX_NODE_MAP)) {
            /* not allowed to get this more than once */
            if (*flags & PMIX_HASH_NODE_MAP) {
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                return PMIX_ERR_BAD_PARAM;
            }
            /* parse the regex to get the argv array of node names */
            if (PMIX_SUCCESS != (rc = pmix_preg.parse_nodes(iptr[j].value.data.bo.bytes, nodes))) {
                PMIX_ERROR_LOG(rc);
                return rc;
            }
            /* mark that we got the map */
            *flags |= PMIX_HASH_NODE_MAP;
        } else if (PMIX_CHECK_KEY(&iptr[j], PMIX_MODEL_LIBRARY_NAME) ||
                   PMIX_CHECK_KEY(&iptr[j], PMIX_PROGRAMMING_MODEL) ||
                   PMIX_CHECK_KEY(&iptr[j], PMIX_MODEL_LIBRARY_VERSION) ||
                   PMIX_CHECK_KEY(&iptr[j], PMIX_PERSONALITY)) {
            // pass this info to the pmdl framework
            pmix_pmdl.setup_nspace(trk->nptr, &iptr[j]);
        } else {
            kp2 = PMIX_NEW(pmix_kval_t);
            kp2->key = strdup(iptr[j].key);
            kp2->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
            PMIX_VALUE_XFER(rc, kp2->value, &iptr[j].value);
            if (PMIX_SUCCESS != rc) {
                PMIX_RELEASE(kp2);
                PMIX_LIST_DESTRUCT(&cache);
                return rc;
            }
            pmix_list_append(&trk->jobinfo, &kp2->super);
            /* check for job size */
            if (PMIX_CHECK_KEY(&iptr[j], PMIX_JOB_SIZE)) {
                if (!(PMIX_HASH_JOB_SIZE & *flags)) {
                    trk->nptr->nprocs = iptr[j].value.data.uint32;
                    *flags |= PMIX_HASH_JOB_SIZE;
                }
            } else if (PMIX_CHECK_KEY(&iptr[j], PMIX_DEBUG_STOP_ON_EXEC) ||
                       PMIX_CHECK_KEY(&iptr[j], PMIX_DEBUG_STOP_IN_INIT) ||
                       PMIX_CHECK_KEY(&iptr[j], PMIX_DEBUG_STOP_IN_APP)) {
                if (PMIX_RANK_WILDCARD == iptr[j].value.data.rank) {
                    trk->nptr->num_waiting = trk->nptr->nlocalprocs;
                } else {
                    trk->nptr->num_waiting = 1;
                }
            } else {
                pmix_iof_check_flags(&iptr[j], &trk->nptr->iof_flags);
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_gds_hash_process_session_array(pmix_value_t *val, pmix_job_t *trk)
{
    pmix_session_t *sptr = NULL;
    size_t j, size;
    pmix_info_t *iptr;
    pmix_list_t ncache, scache;
    pmix_status_t rc;
    pmix_kval_t *kp2;
    pmix_nodeinfo_t *nd;
    uint32_t sid = UINT32_MAX;

    /* array of session-level info */
    if (PMIX_DATA_ARRAY != val->type) {
        PMIX_ERROR_LOG(PMIX_ERR_TYPE_MISMATCH);
        return PMIX_ERR_TYPE_MISMATCH;
    }
    size = val->data.darray->size;
    iptr = (pmix_info_t *) val->data.darray->array;

    PMIX_CONSTRUCT(&ncache, pmix_list_t);
    PMIX_CONSTRUCT(&scache, pmix_list_t);

    for (j = 0; j < size; j++) {
         pmix_output_verbose(12, pmix_gds_base_framework.framework_output,
                    "%s gds:hash:session_array for key %s",
                    PMIX_NAME_PRINT(&pmix_globals.myid),
                    iptr[j].key);
        if (PMIX_CHECK_KEY(&iptr[j], PMIX_SESSION_ID)) {
            PMIX_VALUE_GET_NUMBER(rc, &iptr[j].value, sid, uint32_t);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_LIST_DESTRUCT(&ncache);
                PMIX_LIST_DESTRUCT(&scache);
                return rc;
            }
            sptr = pmix_gds_hash_check_session(trk, sid, true);
        } else if (PMIX_CHECK_KEY(&iptr[j], PMIX_NODE_INFO_ARRAY)) {
            if (PMIX_SUCCESS != (rc = pmix_gds_hash_process_node_array(&iptr[j].value, &ncache))) {
                PMIX_ERROR_LOG(rc);
                PMIX_LIST_DESTRUCT(&ncache);
                PMIX_LIST_DESTRUCT(&scache);
                return rc;
            }
        } else {
            kp2 = PMIX_NEW(pmix_kval_t);
            kp2->key = strdup(iptr[j].key);
            kp2->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
            PMIX_VALUE_XFER(rc, kp2->value, &iptr[j].value);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kp2);
                PMIX_LIST_DESTRUCT(&ncache);
                PMIX_LIST_DESTRUCT(&scache);
                return rc;
            }
            pmix_list_append(&scache, &kp2->super);
        }
    }

    /* if we never got a session ID, then that's an error */
    if (NULL == sptr) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        PMIX_LIST_DESTRUCT(&ncache);
        PMIX_LIST_DESTRUCT(&scache);
        return PMIX_ERR_BAD_PARAM;
    }

    /* transfer across the results */
    kp2 = (pmix_kval_t*)pmix_list_remove_first(&scache);
    while (NULL != kp2) {
        pmix_list_append(&sptr->sessioninfo, &kp2->super);
        kp2 = (pmix_kval_t*)pmix_list_remove_first(&scache);
    }
    PMIX_LIST_DESTRUCT(&scache);

    nd = (pmix_nodeinfo_t*)pmix_list_remove_first(&ncache);
    while (NULL != nd) {
        pmix_list_append(&sptr->nodeinfo, &nd->super);
        nd = (pmix_nodeinfo_t*)pmix_list_remove_first(&ncache);
    }
    PMIX_LIST_DESTRUCT(&ncache);
    return PMIX_SUCCESS;
}
