/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2018 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018-2020 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022-2024 Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "gds_shmem2_store.h"
#include "gds_shmem2_utils.h"

#include "src/util/pmix_hash.h"

#include "src/mca/bfrops/base/bfrop_base_tma.h"

/**
 * Convenience function for creating an initialized pmix_kval_t.
 */
static pmix_kval_t *
newkval(
    const char *key,
    pmix_tma_t *tma
) {
    pmix_kval_t *kv = PMIX_NEW(pmix_kval_t, tma);
    if (PMIX_LIKELY(kv)) {
        kv->key = pmix_tma_strdup(tma, key);
        if (PMIX_UNLIKELY(!kv->key)) {
            PMIX_RELEASE(kv);
            return kv;
        }
        kv->value = pmix_tma_calloc(tma, 1, sizeof(pmix_value_t));
        if (PMIX_UNLIKELY(!kv->value)) {
            PMIX_RELEASE(kv);
            return kv;
        }
    }
    return kv;
}

/**
 * Populates the provided with the elements present in the given comma-delimited
 * string. If the list is not empty, it is first cleared and then set.
 */
static inline pmix_status_t
set_host_aliases_from_cds(
    pmix_list_t *list,
    const char *cds
) {
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_tma_t *const tma = pmix_obj_get_tma(&list->super);

    // If the list isn't empty, release remove and release its items.
    if (!pmix_list_is_empty(list)) {
        pmix_list_item_t *it;
        while (NULL != (it = pmix_list_remove_first(list))) {
            PMIX_RELEASE(it);
        }
    }

    // Now, add each element present in the comma-delimited string list. Not
    // using our TMA because this is a temporary value.
    char **tmp = PMIx_Argv_split(cds, ',');
    if (PMIX_UNLIKELY(!tmp)) {
        rc = PMIX_ERR_NOMEM;
        PMIX_ERROR_LOG(rc);
        goto out;
    }

    for (size_t i = 0; NULL != tmp[i]; i++) {
        pmix_gds_shmem2_host_alias_t *alias;
        alias = PMIX_NEW(pmix_gds_shmem2_host_alias_t, tma);
        if (PMIX_UNLIKELY(!alias)) {
            rc = PMIX_ERR_NOMEM;
            PMIX_ERROR_LOG(rc);
            break;
        }

        alias->name = pmix_tma_strdup(tma, tmp[i]);
        if (PMIX_UNLIKELY(!alias->name)) {
            rc = PMIX_ERR_NOMEM;
            PMIX_ERROR_LOG(rc);
            break;
        }
        pmix_list_append(list, &alias->super);
    }
out:
    PMIx_Argv_free(tmp);
    return rc;
}

static pmix_status_t
cache_node_info(
    pmix_info_t *info,
    size_t ninfo,
    pmix_list_t *cache,
    pmix_gds_shmem2_nodeinfo_t **nodeinfo
) {
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_tma_t *const tma = pmix_obj_get_tma(&cache->super);
    bool have_node_id_info = false;

    pmix_gds_shmem2_nodeinfo_t *iinfo = PMIX_NEW(pmix_gds_shmem2_nodeinfo_t, tma);
    if (PMIX_UNLIKELY(!iinfo)) {
        rc = PMIX_ERR_NOMEM;
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    // Cache the values while searching for the nodeid and/or hostname.
    for (size_t j = 0; j < ninfo; j++) {
        PMIX_GDS_SHMEM2_VVOUT(
            "%s:%s for key=%s", __func__,
            PMIX_NAME_PRINT(&pmix_globals.myid), info[j].key
        );
        if (PMIX_CHECK_KEY(&info[j], PMIX_NODEID)) {
            have_node_id_info = true;
            PMIX_VALUE_GET_NUMBER(
                rc, &info[j].value, iinfo->nodeid, uint32_t
            );
            if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                PMIX_ERROR_LOG(rc);
                goto out;
            }
        }
        else if (PMIX_CHECK_KEY(&info[j], PMIX_HOSTNAME)) {
            have_node_id_info = true;
            iinfo->hostname = pmix_tma_strdup(
                tma, info[j].value.data.string
            );
            if (PMIX_UNLIKELY(!iinfo->hostname)) {
                rc = PMIX_ERR_NOMEM;
                PMIX_ERROR_LOG(rc);
                goto out;
            }
        }
        else if (PMIX_CHECK_KEY(&info[j], PMIX_HOSTNAME_ALIASES)) {
            have_node_id_info = true;
            // info[j].value.data.string is a
            // comma-delimited string of hostnames.
            rc = set_host_aliases_from_cds(
                iinfo->aliases,
                info[j].value.data.string
            );
            if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                PMIX_ERROR_LOG(rc);
                goto out;
            }
            // Need to cache this value as well.
            pmix_kval_t *kv = newkval(info[j].key, tma);
            if (PMIX_UNLIKELY(!kv)) {
                rc = PMIX_ERR_NOMEM;
                PMIX_ERROR_LOG(rc);
                goto out;
            }
            rc = pmix_bfrops_base_tma_value_xfer(kv->value, &info[j].value, tma);
            if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kv);
                goto out;
            }
            pmix_list_append(cache, &kv->super);
        }
        else {
            pmix_kval_t *kv = newkval(info[j].key, tma);
            if (PMIX_UNLIKELY(!kv)) {
                rc = PMIX_ERR_NOMEM;
                PMIX_ERROR_LOG(rc);
                goto out;
            }
            rc = pmix_bfrops_base_tma_value_xfer(kv->value, &info[j].value, tma);
            if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kv);
                break;
            }
            pmix_list_append(cache, &kv->super);
        }
    }
    // Make sure that the caller passed us node identifier info.
    if (PMIX_UNLIKELY(!have_node_id_info)) {
        rc = PMIX_ERR_BAD_PARAM;
    }
out:
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(iinfo);
    }
    *nodeinfo = iinfo;
    return rc;
}

static pmix_status_t
store_node_array(
    pmix_value_t *val,
    pmix_list_t *target
) {
    pmix_status_t rc = PMIX_SUCCESS;

    // We expect an array of node-level info for a specific
    // node. Make sure we were given the correct type.
    if (PMIX_UNLIKELY(PMIX_DATA_ARRAY != val->type)) {
        rc = PMIX_ERR_TYPE_MISMATCH;
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    // Construct node info cache from provided data.
    pmix_tma_t *const tma = pmix_obj_get_tma(&target->super);
    pmix_list_t *cache = PMIX_NEW(pmix_list_t, tma);
    if (PMIX_UNLIKELY(!cache)) {
        rc = PMIX_ERR_NOMEM;
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    pmix_gds_shmem2_nodeinfo_t *nodeinfo;
    rc = cache_node_info(
        (pmix_info_t *)val->data.darray->array,
        val->data.darray->size, cache, &nodeinfo
    );
    if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
        goto out;
    }

    pmix_kval_t *cache_kv;
    while ((cache_kv = (pmix_kval_t *)pmix_list_remove_first(cache))) {
        pmix_list_append(nodeinfo->info, &cache_kv->super);
    }

    pmix_list_append(target, &nodeinfo->super);
out:
    PMIX_LIST_DESTRUCT(cache);
    return rc;
}

static pmix_status_t
store_app_array(
    pmix_gds_shmem2_job_t *job,
    pmix_value_t *val
) {
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_gds_shmem2_app_t *app = NULL;

    // Apps must belong to a job.
    if (PMIX_UNLIKELY(!job)) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }
    // We expect an array of node-level info for a specific
    // node. Make sure we were given the correct type.
    if (PMIX_UNLIKELY(PMIX_DATA_ARRAY != val->type)) {
        PMIX_ERROR_LOG(PMIX_ERR_TYPE_MISMATCH);
        return PMIX_ERR_TYPE_MISMATCH;
    }

    pmix_tma_t *const tma = pmix_gds_shmem2_get_job_tma(job);
    // Setup arrays and lists.
    pmix_list_t *app_cache = PMIX_NEW(pmix_list_t, tma);
    if (PMIX_UNLIKELY(!app_cache)) {
        rc = PMIX_ERR_NOMEM;
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    pmix_list_t *node_cache = PMIX_NEW(pmix_list_t, tma);
    if (PMIX_UNLIKELY(!node_cache)) {
        PMIX_LIST_DESTRUCT(app_cache);
        rc = PMIX_ERR_NOMEM;
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    const size_t size = val->data.darray->size;
    pmix_info_t *info = (pmix_info_t *)val->data.darray->array;
    for (size_t j = 0; j < size; j++) {
        PMIX_GDS_SHMEM2_VVOUT(
            "%s:%s for key=%s", __func__,
            PMIX_NAME_PRINT(&pmix_globals.myid), info[j].key
        );
        if (PMIX_CHECK_KEY(&info[j], PMIX_APPNUM)) {
            uint32_t appnum;
            PMIX_VALUE_GET_NUMBER(rc, &info[j].value, appnum, uint32_t);
            if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                PMIX_ERROR_LOG(rc);
                goto out;
            }
            // This is an error: there can be only
            // one app described in this array.
            if (PMIX_UNLIKELY(NULL != app)) {
                PMIX_RELEASE(app);
                rc = PMIX_ERR_BAD_PARAM;
                PMIX_ERROR_LOG(rc);
                goto out;
            }
            app = PMIX_NEW(pmix_gds_shmem2_app_t, tma);
            if (PMIX_UNLIKELY(!app)) {
                rc = PMIX_ERR_NOMEM;
                PMIX_ERROR_LOG(rc);
                goto out;
            }
            app->appnum = appnum;
        }
        else if (PMIX_CHECK_KEY(&info[j], PMIX_NODE_INFO_ARRAY)) {
            rc = store_node_array(&info[j].value, node_cache);
            if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                PMIX_ERROR_LOG(rc);
                goto out;
            }
        }
        else {
            pmix_kval_t *kv = newkval(info[j].key, tma);
            if (PMIX_UNLIKELY(!kv)) {
                rc = PMIX_ERR_NOMEM;
                PMIX_ERROR_LOG(rc);
                goto out;
            }
            rc = pmix_bfrops_base_tma_value_xfer(kv->value, &info[j].value, tma);
            if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kv);
                goto out;
            }
            pmix_list_append(app_cache, &kv->super);
        }
    }

    if (NULL == app) {
        // Per the standard, they don't have to provide us with
        // an appnum so long as only one app is in the job.
        if (0 == pmix_list_get_size(job->smdata->appinfo)) {
            app = PMIX_NEW(pmix_gds_shmem2_app_t, tma);
            if (PMIX_UNLIKELY(!app)) {
                rc = PMIX_ERR_NOMEM;
                PMIX_ERROR_LOG(rc);
                goto out;
            }
            app->appnum = 0;
        }
        else {
            // This is not allowed to happen: they are required
            // to provide us with an app number per the standard.
            rc = PMIX_ERR_BAD_PARAM;
            PMIX_ERROR_LOG(rc);
            goto out;
        }
    }
    // Add it to our list of apps.
    pmix_list_append(job->smdata->appinfo, &app->super);
    // Point the app at its job.
    if (NULL == app->job) {
        // Do NOT retain the tracker. We will not release it in the app
        // destructor. If we retain the tracker, then we won't release it later
        // because the refcount is wrong.
        app->job = job;
    }
    // Transfer the app-level data across.
    pmix_kval_t *akv;
    while ((akv = (pmix_kval_t *)pmix_list_remove_first(app_cache))) {
        pmix_list_append(app->appinfo, &akv->super);
    }
    // Transfer the associated node-level data across.
    pmix_gds_shmem2_nodeinfo_t *nd;
    while ((nd = (pmix_gds_shmem2_nodeinfo_t *)pmix_list_remove_first(node_cache))) {
        pmix_list_append(app->nodeinfo, &nd->super);
    }
out:
    PMIX_LIST_DESTRUCT(app_cache);
    PMIX_LIST_DESTRUCT(node_cache);
    return rc;
}

static pmix_status_t
store_proc_data(
    pmix_gds_shmem2_job_t *job,
    const pmix_kval_t *kval
) {
    pmix_status_t rc = PMIX_SUCCESS;

    // First, make sure this is proc data.
    if (PMIX_UNLIKELY(!PMIX_CHECK_KEY(kval, PMIX_PROC_DATA))) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }
    // We are expecting an array of data pertaining to a specific proc.
    if (PMIX_UNLIKELY(PMIX_DATA_ARRAY != kval->value->type)) {
        PMIX_ERROR_LOG(PMIX_ERR_TYPE_MISMATCH);
        return PMIX_ERR_TYPE_MISMATCH;
    }

    pmix_info_t *const info = (pmix_info_t *)kval->value->data.darray->array;
    // First element of the array must be the rank.
    if (PMIX_UNLIKELY(!PMIX_CHECK_KEY(&info[0], PMIX_RANK)) ||
        info[0].value.type != PMIX_PROC_RANK) {
        rc = PMIX_ERR_TYPE_MISMATCH;
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    pmix_hash_table_t *const ht = job->smdata->local_hashtab;
    pmix_tma_t *const tma = pmix_obj_get_tma(&ht->super);

    const pmix_rank_t rank = info[0].value.data.rank;
    const size_t size = kval->value->data.darray->size;
    // Cycle through the values for this rank and store them.
    for (size_t j = 1; j < size; j++) {
        pmix_kval_t *kv = PMIX_NEW(pmix_kval_t, tma);
        kv->key = info[j].key;
        kv->value = &info[j].value;
        PMIX_GDS_SHMEM2_VVOUT(
            "%s:%s for nspace=%s rank=%u key=%s",
            __func__, PMIX_NAME_PRINT(&pmix_globals.myid),
            job->nspace_id, rank, kv->key
        );
        // Store it in the hash_table.
        rc = pmix_hash_store(ht, rank, kv, NULL, 0, NULL);
        if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
    }
    return rc;
}

static pmix_status_t
store_session_array(
    pmix_gds_shmem2_job_t *job,
    pmix_value_t *val
) {
    pmix_status_t rc = PMIX_SUCCESS;

    // We expect an array of session-level info.
    if (PMIX_UNLIKELY(PMIX_DATA_ARRAY != val->type)) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_TYPE_MISMATCH;
    }

    // The first value is required to be the session ID.
    pmix_info_t *const info = (pmix_info_t *)val->data.darray->array;
    if (PMIX_UNLIKELY(!PMIX_CHECK_KEY(&info[0], PMIX_SESSION_ID))) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }

    uint32_t sid;
    PMIX_VALUE_GET_NUMBER(rc, &info[0].value, sid, uint32_t);
    if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    pmix_gds_shmem2_session_t *sesh;
    sesh = pmix_gds_shmem2_get_session_tracker(job, sid, false);
    if (PMIX_UNLIKELY(!sesh)) {
        rc = PMIX_ERROR;
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    pmix_tma_t *const tma = pmix_gds_shmem2_get_session_tma(job);
    pmix_list_t *ncache = PMIX_NEW(pmix_list_t, tma);
    if (PMIX_UNLIKELY(!ncache)) {
        rc = PMIX_ERR_NOMEM;
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    const size_t size = val->data.darray->size;
    for (size_t j = 1; j < size; j++) {
        PMIX_GDS_SHMEM2_VVOUT(
            "%s:%s key=%s", __func__,
            PMIX_NAME_PRINT(&pmix_globals.myid),
            info[j].key
        );
        if (PMIX_CHECK_KEY(&info[j], PMIX_NODE_INFO_ARRAY)) {
            rc = store_node_array(&info[j].value, ncache);
            if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                PMIX_ERROR_LOG(rc);
                goto out;
            }
        }
        else {
            pmix_kval_t *kval = newkval(info[j].key, tma);
            if (PMIX_UNLIKELY(!kval)) {
                rc = PMIX_ERR_NOMEM;
                PMIX_ERROR_LOG(rc);
                goto out;
            }
            rc = pmix_bfrops_base_tma_value_xfer(kval->value, &info[j].value, tma);
            if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(kval);
                goto out;
            }
            pmix_list_append(sesh->smdata->sessioninfo, &kval->super);
        }
    }

    pmix_gds_shmem2_nodeinfo_t *ni;
    while ((ni = (pmix_gds_shmem2_nodeinfo_t *)pmix_list_remove_first(ncache))) {
        pmix_list_append(sesh->smdata->nodeinfo, &ni->super);
    }
out:
    PMIX_LIST_DESTRUCT(ncache);
    return rc;
}

pmix_status_t
pmix_gds_shmem2_store_qualified(
    pmix_hash_table_t *ht,
    pmix_rank_t rank,
    pmix_value_t *value
) {
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_tma_t *const tma = pmix_obj_get_tma(&ht->super);
    // The value contains a pmix_data_array_t whose first position contains the
    // key-value being stored, followed by one or more qualifiers.
    pmix_info_t *const info = (pmix_info_t *)value->data.darray->array;
    const size_t ninfo = value->data.darray->size;
    // This does not need to use the TMA since its contents are later copied in
    // hash_store() using a TMA. This is just temporary storage.
    const size_t nquals = ninfo - 1;

    pmix_info_t *quals = pmix_bfrops_base_tma_info_create(nquals, tma);
    for (size_t i = 1; i < ninfo; i++) {
        pmix_bfrops_base_tma_info_qualifier(&quals[i - 1], tma);
        rc = pmix_bfrops_base_tma_info_xfer(&quals[i - 1], &info[i], tma);
        if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
            PMIX_ERROR_LOG(rc);
            goto out;
        }
    }
    // Extract the primary value.
    pmix_kval_t *kv = PMIX_NEW(pmix_kval_t, tma);
    if (PMIX_UNLIKELY(!kv)) {
        rc = PMIX_ERR_NOMEM;
        PMIX_ERROR_LOG(rc);
        goto out;
    }
    kv->key = info[0].key;
    kv->value = &info[0].value;
    // Store the result.
    rc = pmix_hash_store(ht, rank, kv, quals, nquals, NULL);
    if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
        PMIX_ERROR_LOG(rc);
    }
out:
    pmix_bfrops_base_tma_info_free(quals, nquals, tma);
    return rc;
}

pmix_status_t
pmix_gds_shmem2_store_local_job_data_in_shmem2(
    pmix_gds_shmem2_job_t *job,
    pmix_list_t *job_data
) {
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_hash_table_t *const local_ht = job->smdata->local_hashtab;
    pmix_tma_t *const tma = pmix_obj_get_tma(&local_ht->super);

    pmix_kval_t *kvi;
    PMIX_LIST_FOREACH (kvi, job_data, pmix_kval_t) {
        PMIX_GDS_SHMEM2_VVOUT("%s: key=%s", __func__, kvi->key);
        if (PMIX_DATA_ARRAY == kvi->value->type) {
            // We support the following data array keys.
            if (PMIX_CHECK_KEY(kvi, PMIX_APP_INFO_ARRAY)) {
                rc = store_app_array(job, kvi->value);
                if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                    PMIX_ERROR_LOG(rc);
                    break;
                }
            }
            else if (PMIX_CHECK_KEY(kvi, PMIX_NODE_INFO_ARRAY)) {
                rc = store_node_array(
                    kvi->value, job->smdata->nodeinfo
                );
                if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                    PMIX_ERROR_LOG(rc);
                    break;
                }
            }
            else if (PMIX_CHECK_KEY(kvi, PMIX_PROC_INFO_ARRAY)) {
                rc = store_proc_data(job, kvi);
                if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                    PMIX_ERROR_LOG(rc);
                    break;
                }
            }
            else if (PMIX_CHECK_KEY(kvi, PMIX_SESSION_INFO_ARRAY)) {
                rc = store_session_array(job, kvi->value);
                if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                    PMIX_ERROR_LOG(rc);
                    break;
                }
            }
            else {
                PMIX_GDS_SHMEM2_VVOUT(
                    "%s: ERROR unsupported array type=%s", __func__, kvi->key
                );
                rc = PMIX_ERR_NOT_SUPPORTED;
                PMIX_ERROR_LOG(rc);
                break;
            }
        }
        else {
            pmix_kval_t *kv = newkval(kvi->key, tma);
            if (PMIX_UNLIKELY(!kv)) {
                rc = PMIX_ERR_NOMEM;
                PMIX_ERROR_LOG(rc);
                break;
            }

            rc = pmix_bfrops_base_tma_value_xfer(kv->value, kvi->value, tma);
            if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                PMIX_RELEASE(kv);
                PMIX_ERROR_LOG(rc);
                break;
            }
            rc = pmix_hash_store(
                local_ht, PMIX_RANK_WILDCARD, kv, NULL, 0, NULL
            );
            if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                PMIX_RELEASE(kv);
                PMIX_ERROR_LOG(rc);
                break;
            }
        }
    }
    if (PMIX_SUCCESS == rc) {
        // Segments are ready for use.
        pmix_gds_shmem2_set_status(
            job, PMIX_GDS_SHMEM2_JOB_ID,
            PMIX_GDS_SHMEM2_READY_FOR_USE
        );
        pmix_gds_shmem2_set_status(
            job, PMIX_GDS_SHMEM2_SESSION_ID,
            PMIX_GDS_SHMEM2_READY_FOR_USE
        );
    }
    return rc;
}

/*
 * vim: ft=cpp ts=4 sts=4 sw=4 expandtab
 */
