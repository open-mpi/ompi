/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2019 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2018      IBM Corporation.  All rights reserved.
 * Copyright (c) 2018-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "include/pmix_common.h"
#include "src/include/pmix_globals.h"

#include "src/class/pmix_list.h"
#include "src/util/argv.h"
#include "src/util/error.h"

#include "src/mca/gds/base/base.h"
#include "src/server/pmix_server_ops.h"


char* pmix_gds_base_get_available_modules(void)
{
    if (!pmix_gds_globals.initialized) {
        return NULL;
    }

    return strdup(pmix_gds_globals.all_mods);
}

/* Select a gds module per the given directives */
pmix_gds_base_module_t* pmix_gds_base_assign_module(pmix_info_t *info, size_t ninfo)
{
    pmix_gds_base_active_module_t *active;
    pmix_gds_base_module_t *mod = NULL;
    int pri, priority = -1;

    if (!pmix_gds_globals.initialized) {
        return NULL;
    }

    PMIX_LIST_FOREACH(active, &pmix_gds_globals.actives, pmix_gds_base_active_module_t) {
        if (NULL == active->module->assign_module) {
            continue;
        }
        if (PMIX_SUCCESS == active->module->assign_module(info, ninfo, &pri)) {
            if (pri < 0) {
                /* use the default priority from the component */
                pri = active->pri;
            }
            if (priority < pri) {
                mod = active->module;
                priority = pri;
            }
        }
    }

    return mod;
}

pmix_status_t pmix_gds_base_setup_fork(const pmix_proc_t *proc,
                                       char ***env)
{
    pmix_gds_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_gds_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    PMIX_LIST_FOREACH(active, &pmix_gds_globals.actives, pmix_gds_base_active_module_t) {
        if (NULL == active->module->setup_fork) {
            continue;
        }
        rc = active->module->setup_fork(proc, env);
        if (PMIX_SUCCESS != rc && PMIX_ERR_NOT_AVAILABLE != rc) {
            return rc;
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_gds_base_store_modex(struct pmix_namespace_t *nspace,
                                        pmix_buffer_t * buff,
                                        pmix_gds_base_ctx_t ctx,
                                        pmix_gds_base_store_modex_cb_fn_t cb_fn,
                                        void *cbdata)
{
    (void)nspace;
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_buffer_t bkt;
    pmix_byte_object_t bo, bo2;
    int32_t cnt = 1;
    pmix_collect_t ctype;
    pmix_server_trkr_t *trk = (pmix_server_trkr_t*)cbdata;
    pmix_proc_t proc;
    pmix_buffer_t pbkt;
    pmix_rank_t rel_rank;
    pmix_nspace_caddy_t *nm;
    bool found;
    char  **kmap = NULL;
    uint32_t kmap_size;
    pmix_gds_modex_key_fmt_t kmap_type;
    pmix_gds_modex_blob_info_t blob_info_byte = 0;

    /* Loop over the enclosed byte object envelopes and
     * store them in our GDS module */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
            buff, &bo, &cnt, PMIX_BYTE_OBJECT);

    /* If the collect flag is set, we should have some data for unpacking */
    if ((PMIX_COLLECT_YES == trk->collect_type) &&
            (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc)) {
        goto exit;
    }

    while (PMIX_SUCCESS == rc) {
        PMIX_CONSTRUCT(&bkt, pmix_buffer_t);
        PMIX_LOAD_BUFFER(pmix_globals.mypeer, &bkt, bo.bytes, bo.size);
        /* unpack the data collection flag */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                &bkt, &blob_info_byte, &cnt, PMIX_BYTE);
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
            /* no data was returned, so we are done with this blob */
            PMIX_DESTRUCT(&bkt);
            break;
        }
        if (PMIX_SUCCESS != rc) {
            /* we have an error */
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&bkt);
            goto exit;
        }
        /* Check that this blob was accumulated with the same data collection
         * setting */
        ctype = PMIX_GDS_COLLECT_IS_SET(blob_info_byte) ?
                    PMIX_COLLECT_YES : PMIX_COLLECT_NO;
        if (trk->collect_type != ctype) {
            rc = PMIX_ERR_INVALID_ARG;
            PMIX_ERROR_LOG(rc);
            goto exit;
        }

        /* determine the key-map existing flag */
        kmap_type = PMIX_GDS_KEYMAP_IS_SET(blob_info_byte) ?
                    PMIX_MODEX_KEY_KEYMAP_FMT : PMIX_MODEX_KEY_NATIVE_FMT;
        if (PMIX_MODEX_KEY_KEYMAP_FMT == kmap_type) {
            /* unpack the size of uniq keys names in the map */
            cnt = 1;
            PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                               &bkt, &kmap_size, &cnt, PMIX_UINT32);
            if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
                rc = PMIX_SUCCESS;
                PMIX_DESTRUCT(&bkt);
                break;
            } else if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&bkt);
                break;
            }

            /* init and unpack key names map, the position of the key name
             * in the array determines the unique key index */
            kmap = (char**)(calloc(kmap_size + 1, sizeof(char*)));
            if (NULL == kmap) {
                rc = PMIX_ERR_OUT_OF_RESOURCE;
                PMIX_ERROR_LOG(rc);
                goto exit;
            }
            cnt = kmap_size;
            PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &bkt,
                               kmap, &cnt, PMIX_STRING);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&bkt);
                goto exit;
            }
            if (pmix_argv_count(kmap) != (int)kmap_size) {
                rc = PMIX_ERR_UNPACK_FAILURE;
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&bkt);
                goto exit;
            }
        }
        /* unpack the enclosed blobs from the various peers */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                &bkt, &bo2, &cnt, PMIX_BYTE_OBJECT);
        while (PMIX_SUCCESS == rc) {
            /* unpack all the kval's from this peer and store them in
             * our GDS. Note that PMIx by design holds all data at
             * the server level until requested. If our GDS is a
             * shared memory region, then the data may be available
             * right away - but the client still has to be notified
             * of its presence. */

            /* setup the byte object for unpacking */
            PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
            PMIX_LOAD_BUFFER(pmix_globals.mypeer, &pbkt, bo2.bytes, bo2.size);
            /* unload the proc that provided this data */
            cnt = 1;
            PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &pbkt, &rel_rank, &cnt,
                               PMIX_PROC_RANK);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                pbkt.base_ptr = NULL;
                PMIX_DESTRUCT(&pbkt);
                break;
            }
            found = false;
            /* calculate proc form the relative rank */
            if (pmix_list_get_size(&trk->nslist) == 1) {
                found = true;
                nm = (pmix_nspace_caddy_t*)pmix_list_get_first(&trk->nslist);
            } else {
                PMIX_LIST_FOREACH(nm, &trk->nslist, pmix_nspace_caddy_t) {
                    if (rel_rank < nm->ns->nprocs) {
                        found = true;
                        break;
                    }
                    rel_rank -= nm->ns->nprocs;
                }
            }
            if (false == found) {
                rc = PMIX_ERR_NOT_FOUND;
                PMIX_ERROR_LOG(rc);
                pbkt.base_ptr = NULL;
                PMIX_DESTRUCT(&pbkt);
                break;
            }
            PMIX_PROC_LOAD(&proc, nm->ns->nspace, rel_rank);

            /* call a specific GDS function to storing
             * part of the process data */
            rc = cb_fn(ctx, &proc, kmap_type, kmap, &pbkt);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                pbkt.base_ptr = NULL;
                PMIX_DESTRUCT(&pbkt);
                break;
            }
            pbkt.base_ptr = NULL;
            PMIX_DESTRUCT(&pbkt);
            PMIX_BYTE_OBJECT_DESTRUCT(&bo2);
            /* get the next blob */
            cnt = 1;
            PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                    &bkt, &bo2, &cnt, PMIX_BYTE_OBJECT);
        }
        PMIX_DESTRUCT(&bkt);

        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
            rc = PMIX_SUCCESS;
        } else if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
        /* unpack and process the next blob */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                buff, &bo, &cnt, PMIX_BYTE_OBJECT);
    }

    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
        rc = PMIX_SUCCESS;
    } else if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }
exit:
    pmix_argv_free(kmap);
    return rc;
}

/*
 * Pack the key-value as a tuple of key-name index and key-value.
 * The key-name to store replaced by unique key-index that stored
 * to the key-map. So the remote server can determine the key-name
 * by the index from map that packed in modex as well.
 *
 * kmap - key values array by (char*), uses to store unique key
 *        names string and determine their indexes
 *
 * buf - output buffer to pack key-values
 *
 * kv - pmix key-value pair
 */
pmix_status_t pmix_gds_base_modex_pack_kval(pmix_gds_modex_key_fmt_t key_fmt,
                                            pmix_buffer_t *buf, char ***kmap,
                                            pmix_kval_t *kv)
{
    uint32_t key_idx;
    pmix_status_t rc = PMIX_SUCCESS;

    if (PMIX_MODEX_KEY_KEYMAP_FMT == key_fmt) {
        rc = pmix_argv_append_unique_idx((int*)&key_idx, kmap, kv->key);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        /* pack key-index */
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, buf, &key_idx, 1, PMIX_UINT32);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        /* pack key-value */
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, buf, kv->value, 1, PMIX_VALUE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
    } else if (PMIX_MODEX_KEY_NATIVE_FMT == key_fmt) {
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, buf, kv, 1, PMIX_KVAL);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
    } else {
        rc = PMIX_ERR_BAD_PARAM;
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    return PMIX_SUCCESS;
}

/*
 * Unpack the key-value as a tuple of key-name index and key-value.
 *
 * kmap - key values array by (char*), uses to store unique key
 *        names string and determine their indexes
 *
 * buf - input buffer to unpack key-values
 *
 * kv - unpacked pmix key-value pair
 */
pmix_status_t pmix_gds_base_modex_unpack_kval(pmix_gds_modex_key_fmt_t key_fmt,
                                              pmix_buffer_t *buf, char **kmap,
                                              pmix_kval_t *kv)
{
    int32_t cnt;
    uint32_t key_idx;
    pmix_status_t rc = PMIX_SUCCESS;

    if (PMIX_MODEX_KEY_KEYMAP_FMT == key_fmt) {
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, buf, &key_idx, &cnt, PMIX_UINT32);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
        // sanity check
        if (NULL == kmap[key_idx]) {
            rc = PMIX_ERR_BAD_PARAM;
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        kv->key = strdup(kmap[key_idx]);
        cnt = 1;
        PMIX_VALUE_CREATE(kv->value, 1);
        PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, buf, kv->value, &cnt, PMIX_VALUE);
        if (PMIX_SUCCESS != rc) {
            free(kv->key);
            PMIX_VALUE_RELEASE(kv->value);
            PMIX_ERROR_LOG(rc);
            return rc;
        }
    } else if (PMIX_MODEX_KEY_NATIVE_FMT == key_fmt) {
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, buf, kv, &cnt, PMIX_KVAL);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
    } else {
        rc = PMIX_ERR_BAD_PARAM;
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    return PMIX_SUCCESS;
}
