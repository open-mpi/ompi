/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022-2024 Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

// TODO(skg) Assert that the tma from the table and keyindex match in all
// relevant places.

#include "src/include/pmix_config.h"

#include "src/include/pmix_hash_string.h"
#include "src/include/pmix_stdint.h"

#include <string.h>

#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_pointer_array.h"
#include "src/include/pmix_dictionary.h"
#include "src/include/pmix_globals.h"
#include "src/include/pmix_hash_string.h"
#include "src/mca/bfrops/bfrops.h"
#include "src/mca/bfrops/base/bfrop_base_tma.h"
#include "src/mca/gds/base/base.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_output.h"

#include "src/util/pmix_hash.h"

/**
 * Utility function that takes a pointer to a pmix_keyindex_t and returns the
 * global keyindex if NULL, returns the input otherwise.
 */
static inline pmix_keyindex_t *get_keyindex_ptr(pmix_keyindex_t *input)
{
    return input ? input : &pmix_globals.keyindex;
}

/**
 * Data for a particular pmix process
 * The name association is maintained in the
 * proc_data hash table.
 */
typedef struct {
    pmix_object_t super;
    /**
     * Array of pmix_dstor_t structures containing all data received from this
     * process. Note that these are pointers because the shared-memory TMA
     * requires that these structures and their data reside on the heap.
     */
    pmix_pointer_array_t *data;
    pmix_pointer_array_t *quals;
} pmix_proc_data_t;
static void pdcon(pmix_proc_data_t *p)
{
    pmix_tma_t *const tma = pmix_obj_get_tma(&p->super);

    p->data = PMIX_NEW(pmix_pointer_array_t, tma);
    pmix_pointer_array_init(p->data, 128, INT_MAX, 128);
    p->quals = PMIX_NEW(pmix_pointer_array_t, tma);
    pmix_pointer_array_init(p->quals, 1, INT_MAX, 1);
}
static void pddes(pmix_proc_data_t *p)
{
    int n;
    size_t nq;
    pmix_dstor_t *d;
    pmix_qual_t *q;
    pmix_data_array_t *darray;
    pmix_tma_t *const tma = pmix_obj_get_tma(&p->super);

    for (n=0; n < p->data->size; n++) {
        d = (pmix_dstor_t*)pmix_pointer_array_get_item(p->data, n);
        if (NULL != d) {
            pmix_dstor_release_tma(d, tma);
            pmix_pointer_array_set_item(p->data, n, NULL);
        }
    }
    PMIX_RELEASE(p->data);
    for (n=0; n < p->quals->size; n++) {
        darray = (pmix_data_array_t*)pmix_pointer_array_get_item(p->quals, n);
        if (NULL != darray) {
            q = (pmix_qual_t*)darray->array;
            for (nq=0; nq < darray->size; nq++) {
                if (NULL != q[nq].value) {
                    pmix_bfrops_base_tma_value_release(&q[nq].value, tma);
                }
            }
            pmix_tma_free(tma, darray->array);
            pmix_tma_free(tma, darray);
        }
        pmix_pointer_array_set_item(p->quals, n, NULL);
    }
    PMIX_RELEASE(p->quals);
}
static PMIX_CLASS_INSTANCE(pmix_proc_data_t, pmix_object_t, pdcon, pddes);

static pmix_dstor_t *lookup_keyval(pmix_proc_data_t *proc, uint32_t kid,
                                   pmix_info_t *qualifiers, size_t nquals,
                                   pmix_keyindex_t *kidx);
static pmix_proc_data_t *lookup_proc(pmix_hash_table_t *jtable, uint32_t id, bool create);
static void erase_qualifiers(pmix_proc_data_t *proc,
                             uint32_t index);


pmix_status_t pmix_hash_store(pmix_hash_table_t *table,
                              pmix_rank_t rank, pmix_kval_t *kin,
                              pmix_info_t *qualifiers, size_t nquals,
                              pmix_keyindex_t *kidx)
{
    pmix_proc_data_t *proc_data;
    uint32_t kid;
    pmix_dstor_t *hv;
    pmix_regattr_input_t *p;
    pmix_status_t rc;
    pmix_data_array_t *darray;
    pmix_qual_t *qarray;
    size_t n, m = 0;
    pmix_tma_t *const tma = pmix_obj_get_tma(&table->super);
    pmix_keyindex_t *const keyindex = get_keyindex_ptr(kidx);

    pmix_output_verbose(10, pmix_gds_base_framework.framework_output,
                        "%s HASH:STORE:QUAL table %s rank %s key %s",
                        PMIX_NAME_PRINT(&pmix_globals.myid),
                        (NULL == table->ht_label) ? "UNKNOWN" : table->ht_label,
                        PMIX_RANK_PRINT(rank), (NULL == kin) ? "NULL KVAL" : kin->key);

    if (PMIX_UNLIKELY(NULL == kin)) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* lookup the key's corresponding index - this should be
     * moved to the periphery of the PMIx library so we can
     * refer to the key numerically throughout the internals
     */
    p = pmix_hash_lookup_key(UINT32_MAX, kin->key, keyindex);
    if (PMIX_UNLIKELY(NULL == p)) {
        /* we don't know this key */
        pmix_output_verbose(10, pmix_gds_base_framework.framework_output,
                            "%s UNKNOWN KEY: %s",
                            PMIX_NAME_PRINT(&pmix_globals.myid),
                            kin->key);
        return PMIX_ERR_BAD_PARAM;
    }
    kid = p->index;

    /* lookup the proc data object for this proc - create
     * it if we don't already have it */
    if (PMIX_UNLIKELY(NULL == (proc_data = lookup_proc(table, rank, true)))) {
        return PMIX_ERR_NOMEM;
    }

    /* see if we already have this key-value */
    hv = lookup_keyval(proc_data, kid, qualifiers, nquals, keyindex);
    if (NULL != hv) {
        if (PMIX_UNLIKELY(9 < pmix_output_get_verbosity(pmix_gds_base_framework.framework_output))) {
            // Note that this doesn't have to use a TMA because it is just a
            // temporary value.
            char *tmp;
            tmp = PMIx_Value_string(hv->value);
            pmix_output(0, "%s PREEXISTING ENTRY FOR PROC %s KEY %s: %s",
                        PMIX_NAME_PRINT(&pmix_globals.myid),
                        PMIX_RANK_PRINT(rank), kin->key, tmp);
            free(tmp);
        }
        /* yes we do - so just replace the current value if it changed */
        if (NULL != hv->value) {
            if (PMIX_EQUAL == PMIx_Value_compare(hv->value, kin->value)) {
                pmix_output_verbose(10, pmix_gds_base_framework.framework_output,
                                    "EQUAL VALUE - IGNORING");
                return PMIX_SUCCESS;
            }
            if (PMIX_UNLIKELY(9 < pmix_output_get_verbosity(pmix_gds_base_framework.framework_output))) {
                // Note that this doesn't have to use a TMA because it is just a
                // temporary value.
                char *tmp;
                tmp = PMIx_Value_string(kin->value);
                pmix_output(0, "%s KEY %s VALUE UPDATING TO: %s",
                            PMIX_NAME_PRINT(&pmix_globals.myid), kin->key, tmp);
                free(tmp);
            }
            pmix_bfrops_base_tma_value_release(&hv->value, tma);
        }
        /* TODO(skg) eventually, we want to eliminate this copy */
        rc = pmix_bfrops_base_tma_copy_value(&hv->value, kin->value, PMIX_VALUE, tma);
        if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        return PMIX_SUCCESS;
    }

    /* we don't already have it, so create it */
    hv = pmix_dstor_new_tma(kid, tma);
    if (PMIX_UNLIKELY(NULL == hv)) {
        return PMIX_ERR_NOMEM;
    }
    if (NULL != qualifiers) {
        /* count the number of actual qualifiers */
        for (n=0, m=0; n < nquals; n++) {
            if (PMIX_INFO_IS_QUALIFIER(&qualifiers[n])) {
                ++m;
            }
        }
        if (0 < m) {
            darray = (pmix_data_array_t*)pmix_tma_malloc(tma, sizeof(pmix_data_array_t));
            darray->array = (pmix_qual_t*)pmix_tma_malloc(tma, m * sizeof(pmix_qual_t));
            darray->size = m;
            hv->qualindex = pmix_pointer_array_add(proc_data->quals, darray);
            qarray = (pmix_qual_t*)darray->array;
            for (n=0, m=0; n < nquals; n++) {
                if (PMIX_INFO_IS_QUALIFIER(&qualifiers[n])) {
                    p = pmix_hash_lookup_key(UINT32_MAX, qualifiers[n].key, keyindex);
                    if (PMIX_UNLIKELY(NULL == p)) {
                        /* we don't know this key */
                        pmix_output_verbose(10, pmix_gds_base_framework.framework_output,
                                            "%s UNKNOWN KEY: %s",
                                            PMIX_NAME_PRINT(&pmix_globals.myid),
                                            kin->key);
                        erase_qualifiers(proc_data, hv->qualindex);
                        pmix_dstor_release_tma(hv, tma);
                        return PMIX_ERR_BAD_PARAM;
                    }
                    qarray[n].index = p->index;
                    rc = pmix_bfrops_base_tma_copy_value(&qarray[m].value, &qualifiers[n].value, PMIX_VALUE, tma);
                    if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
                        PMIX_ERROR_LOG(rc);
                        erase_qualifiers(proc_data, hv->qualindex);
                        pmix_dstor_release_tma(hv, tma);
                        return rc;
                    }
                    ++m;
                }
            }
        }
    }

    /* TODO(skg) eventually, we want to eliminate this copy */
    rc = pmix_bfrops_base_tma_copy_value(&hv->value, kin->value, PMIX_VALUE, tma);
    if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
        PMIX_ERROR_LOG(rc);
        if (UINT32_MAX != hv->qualindex) {
            /* release the associated qualifiers */
            erase_qualifiers(proc_data, hv->qualindex);
        }
        pmix_dstor_release_tma(hv, tma);
        return rc;
    }
    if (PMIX_UNLIKELY(9 < pmix_output_get_verbosity(pmix_gds_base_framework.framework_output))) {
        // Note that this doesn't have to use a TMA because it is just a
        // temporary value.
        char *v = PMIx_Value_string(kin->value);
        pmix_output(0, "%s ADDING KEY %s VALUE %s FOR RANK %s WITH %u QUALS TO TABLE %s",
                    PMIX_NAME_PRINT(&pmix_globals.myid),
                    kin->key, v,
                    PMIX_RANK_PRINT(rank), (unsigned)m,
                    (NULL == table->ht_label) ? "UNKNOWN" : table->ht_label);
        free(v);
    }
    pmix_pointer_array_add(proc_data->data, hv);
    return PMIX_SUCCESS;
}

// TODO(skg) We may have to provide a different fetch entry point with different
// semantics for gds shmem to further reduce memory usage. For now this seems to
// work for our current use case.
pmix_status_t pmix_hash_fetch(pmix_hash_table_t *table,
                              pmix_rank_t rank,
                              const char *key,
                              pmix_info_t *qualifiers, size_t nquals,
                              pmix_list_t *kvals,
                              pmix_keyindex_t *kidx)
{
    pmix_status_t rc;
    pmix_proc_data_t *proc_data;
    pmix_dstor_t *hv;
    uint32_t id, kid=UINT32_MAX;
    char *node;
    pmix_regattr_input_t *p;
    pmix_info_t *iptr;
    size_t nq, m;
    int n;
    pmix_kval_t *kv;
    bool fullsearch = false;
    pmix_data_array_t *darray;
    pmix_qual_t *quals;
    pmix_keyindex_t *const keyindex = get_keyindex_ptr(kidx);

    pmix_output_verbose(10, pmix_gds_base_framework.framework_output,
                        "%s HASH:FETCH table %s id %s key %s",
                        PMIX_NAME_PRINT(&pmix_globals.myid),
                        (NULL == table->ht_label) ? "UNKNOWN" : table->ht_label,
                        PMIX_RANK_PRINT(rank), (NULL == key) ? "NULL" : key);

    /* - PMIX_RANK_UNDEF should return following statuses
     *     PMIX_ERR_NOT_FOUND | PMIX_SUCCESS
     * - specified rank can return following statuses
     *     PMIX_ERR_NOT_FOUND | PMIX_ERR_NOT_FOUND | PMIX_SUCCESS
     * special logic is basing on these statuses on a client and a server */
    if (PMIX_RANK_UNDEF == rank) {
        rc = pmix_hash_table_get_first_key_uint32(table, &id, (void **) &proc_data,
                                                  (void **) &node);
        if (PMIX_SUCCESS != rc) {
            pmix_output_verbose(10, pmix_gds_base_framework.framework_output,
                                "HASH:FETCH[%s:%d] proc data for rank %s not found",
                                __func__, __LINE__, PMIX_RANK_PRINT(rank));
            return PMIX_ERR_NOT_FOUND;
        }
        fullsearch = true;
    } else {
        id = rank;
    }

    if (NULL != key) {
        /* lookup the key's corresponding index - this should be
         * moved to the periphery of the PMIx library so we can
         * refer to the key numerically throughout the internals
         */
        p = pmix_hash_lookup_key(UINT32_MAX, key, keyindex);
        if (NULL == p) {
            /* we don't know this key */
            return PMIX_ERR_BAD_PARAM;
        }
        kid = p->index;
    }

    rc = PMIX_SUCCESS;
    while (PMIX_SUCCESS == rc) {
        proc_data = lookup_proc(table, id, false);
        if (NULL == proc_data) {
            pmix_output_verbose(10, pmix_gds_base_framework.framework_output,
                        "HASH:FETCH[%s:%d] proc data for rank %s not found - key %s",
                        __func__, __LINE__,
                        PMIX_RANK_PRINT(rank), key);
            return PMIX_ERR_NOT_FOUND;
        }

        /* if the key is NULL, then the user wants -all- data
         * put by the specified rank */
        if (NULL == key) {
            /* copy the data */
            for (n=0; n < proc_data->data->size; n++) {
                hv = (pmix_dstor_t*)pmix_pointer_array_get_item(proc_data->data, n);
                if (NULL != hv) {
                    p = pmix_hash_lookup_key(hv->index, NULL, keyindex);
                    if (NULL == p) {
                        return PMIX_ERR_NOT_FOUND;
                    }
                    pmix_output_verbose(10, pmix_gds_base_framework.framework_output,
                                        "%s FETCH NULL LOOKING AT %s",
                                        PMIX_NAME_PRINT(&pmix_globals.myid), p->name);
                    /* if the rank is UNDEF, we ignore reserved keys */
                    if (PMIX_RANK_UNDEF == rank &&
                        PMIX_CHECK_RESERVED_KEY(p->string)) {
                        continue;
                    }
                    if (UINT32_MAX != hv->qualindex) {
                        pmix_output_verbose(10, pmix_gds_base_framework.framework_output,
                                            "%s INCLUDE %s VALUE %s FROM TABLE %s FOR RANK %s",
                                            PMIX_NAME_PRINT(&pmix_globals.myid), p->name,
                                            PMIx_Value_string(hv->value),
                                            (NULL == table->ht_label) ? "UNKNOWN" : table->ht_label,
                                            PMIX_RANK_PRINT(rank));
                        /* this is a qualified value - need to return it as such */
                        PMIX_KVAL_NEW(kv, PMIX_QUALIFIED_VALUE);
                        darray = (pmix_data_array_t*)pmix_pointer_array_get_item(proc_data->quals, hv->qualindex);
                        quals = (pmix_qual_t*)darray->array;
                        nq = darray->size;
                        PMIX_DATA_ARRAY_CREATE(darray, nq+1, PMIX_INFO);
                        iptr = (pmix_info_t*)darray->array;
                        /* the first location is the actual value */
                        PMIX_LOAD_KEY(iptr[0].key, p->string);
                        PMIx_Value_xfer(&iptr[0].value, hv->value);
                        /* now add the qualifiers */
                        for (m=0; m < nq; m++) {
                            p = pmix_hash_lookup_key(quals[m].index, NULL, keyindex);
                            if (NULL == p) {
                                /* should never happen */
                                PMIX_RELEASE(kv);
                                PMIX_DATA_ARRAY_FREE(darray);
                                return PMIX_ERR_BAD_PARAM;
                            }
                            PMIX_LOAD_KEY(iptr[m+1].key, p->string);
                            PMIx_Value_xfer(&iptr[m+1].value, quals[m].value);
                            PMIX_INFO_SET_QUALIFIER(&iptr[m+1]);
                        }
                        kv->value->type = PMIX_DATA_ARRAY;
                        kv->value->data.darray = darray;
                        pmix_list_append(kvals, &kv->super);
                    } else {
                        PMIX_KVAL_NEW(kv, p->string);
                        PMIx_Value_xfer(kv->value, hv->value);
                        pmix_list_append(kvals, &kv->super);
                    }
                }
            }
            return PMIX_SUCCESS;
        } else {
            /* find the value from within this data object */
            hv = lookup_keyval(proc_data, kid, qualifiers, nquals, keyindex);
            if (NULL != hv) {
                /* create the copy */
                PMIX_KVAL_NEW(kv, key);
                PMIx_Value_xfer(kv->value, hv->value);
                pmix_list_append(kvals, &kv->super);
                break;
            } else if (!fullsearch) {
                pmix_output_verbose(10, pmix_gds_base_framework.framework_output,
                                    "HASH:FETCH data for key %s not found", key);
                return PMIX_ERR_NOT_FOUND;
            }
        }

        rc = pmix_hash_table_get_next_key_uint32(table, &id, (void **) &proc_data, node,
                                                 (void **) &node);
        if (PMIX_SUCCESS != rc) {
            pmix_output_verbose(10, pmix_gds_base_framework.framework_output,
                                "%s:%d HASH:FETCH data for key %s not found",
                                __func__, __LINE__, key);
            return PMIX_ERR_NOT_FOUND;
        }
    }

    return rc;
}

pmix_status_t pmix_hash_remove_data(pmix_hash_table_t *table,
                                    pmix_rank_t rank, const char *key,
                                    pmix_keyindex_t *kidx)
{
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_proc_data_t *proc_data;
    pmix_dstor_t *d;
    uint32_t id, kid=UINT32_MAX;
    int n;
    char *node;
    pmix_regattr_input_t *p;
    pmix_tma_t *const tma = pmix_obj_get_tma(&table->super);
    pmix_keyindex_t *const keyindex = get_keyindex_ptr(kidx);

    if (NULL != key) {
        p = pmix_hash_lookup_key(UINT32_MAX, key, keyindex);
        if (PMIX_UNLIKELY(NULL == p)) {
            /* we don't know this key */
            return PMIX_ERR_BAD_PARAM;
        }
        kid = p->index;
    }

    /* if the rank is wildcard, we want to apply this to
     * all rank entries */
    if (PMIX_RANK_WILDCARD == rank) {
        rc = pmix_hash_table_get_first_key_uint32(table, &id, (void **) &proc_data,
                                                  (void **) &node);
        while (PMIX_SUCCESS == rc) {
            if (NULL != proc_data) {
                if (NULL == key) {
                    PMIX_RELEASE(proc_data);
                } else {
                    for (n=0; n < proc_data->data->size; n++) {
                        d = (pmix_dstor_t*)pmix_pointer_array_get_item(proc_data->data, n);
                        if (NULL != d && kid == d->index) {
                            if (NULL != d->value) {
                                pmix_bfrops_base_tma_value_release(&d->value, tma);
                            }
                            if (UINT32_MAX != d->qualindex) {
                                erase_qualifiers(proc_data, d->qualindex);
                            }
                            pmix_tma_free(tma, d);
                            pmix_pointer_array_set_item(proc_data->data, n, NULL);
                            break;
                        }
                    }
                }
            }
            rc = pmix_hash_table_get_next_key_uint32(table, &id, (void **) &proc_data, node,
                                                     (void **) &node);
        }
        return PMIX_SUCCESS;
    }

    /* lookup the specified proc */
    if (NULL == (proc_data = lookup_proc(table, rank, false))) {
        /* no data for this proc */
        return PMIX_SUCCESS;
    }

    /* if key is NULL, remove all data for this proc */
    if (NULL == key) {
        for (n=0; n < proc_data->data->size; n++) {
            d = (pmix_dstor_t*)pmix_pointer_array_get_item(proc_data->data, n);
            if (NULL != d) {
                if (NULL != d->value) {
                    pmix_bfrops_base_tma_value_release(&d->value, tma);
                }
                if (UINT32_MAX != d->qualindex) {
                    erase_qualifiers(proc_data, d->qualindex);
                }
                pmix_tma_free(tma, d);
                pmix_pointer_array_set_item(proc_data->data, n, NULL);
            }
        }
        /* remove the proc_data object itself from the jtable */
        pmix_hash_table_remove_value_uint32(table, rank);
        /* cleanup */
        PMIX_RELEASE(proc_data);
        return PMIX_SUCCESS;
    }

    /* remove this item */
    for (n=0; n < proc_data->data->size; n++) {
        d = (pmix_dstor_t*)pmix_pointer_array_get_item(proc_data->data, n);
        if (NULL != d && kid == d->index) {
            if (NULL != d->value) {
                pmix_bfrops_base_tma_value_release(&d->value, tma);
            }
            if (UINT32_MAX != d->qualindex) {
                erase_qualifiers(proc_data, d->qualindex);
            }
            pmix_tma_free(tma, d);
            pmix_pointer_array_set_item(proc_data->data, n, NULL);
            break;
        }
    }

    return PMIX_SUCCESS;
}

/**
 * Find data for a given key in a given pmix_list_t.
 */
static pmix_dstor_t *lookup_keyval(pmix_proc_data_t *proc_data, uint32_t kid,
                                   pmix_info_t *qualifiers, size_t nquals,
                                   pmix_keyindex_t *kidx)
{
    pmix_dstor_t *d;
    pmix_data_array_t *darray;
    pmix_qual_t *qarray;
    pmix_regattr_input_t *p;
    size_t m, numquals = 0, nq, nfound;
    int n;
    pmix_keyindex_t *const keyindex = get_keyindex_ptr(kidx);

    p = pmix_hash_lookup_key(kid, NULL, keyindex);

    if (NULL != qualifiers) {
        /* count the qualifiers */
        for (m=0; m < nquals; m++) {
            /* if this isn't marked as a qualifier, skip it */
            if (PMIX_INFO_IS_QUALIFIER(&qualifiers[m])) {
                ++numquals;
            }
        }
    }

    for (n=0; n < proc_data->data->size; n++) {
        d = (pmix_dstor_t*)pmix_pointer_array_get_item(proc_data->data, n);
        if (NULL == d) {
            continue;
        }
        if (kid == d->index) {
            if (0 < numquals) {
                if (UINT32_MAX == d->qualindex) {
                    continue;
                }
                darray = (pmix_data_array_t*)pmix_pointer_array_get_item(proc_data->quals, d->qualindex);
                qarray = (pmix_qual_t*)darray->array;
                nfound = 0;
                /* check the qualifiers */
                for (m=0; m < nquals; m++) {
                    /* if this isn't marked as a qualifier, skip it */
                    if (!PMIX_INFO_IS_QUALIFIER(&qualifiers[m])) {
                        continue;
                    }
                    p = pmix_hash_lookup_key(UINT32_MAX, qualifiers[m].key, keyindex);
                    if (NULL == p) {
                        /* we don't know this key */
                        return NULL;
                    }
                    for (nq=0; nq < darray->size; nq++) {
                        /* see if the keys match */
                        if (qarray[nq].index == p->index) {
                            /* if the values don't match, then we reject
                             * this entry */
                            if (PMIX_EQUAL == PMIx_Value_compare(&qualifiers[m].value, qarray[nq].value)) {
                                /* match! */
                                ++nfound;
                                break;
                            }
                        }
                    }
                }
                /* did we get a complete match? */
                if (nfound == numquals) {
                    return d;
                }
            } else {
                /* if the stored key is also "unqualified",
                 * then return it */
                if (UINT32_MAX == d->qualindex) {
                    return d;
                }
            }
        }
    }

    return NULL;
}

/**
 * Find proc_data_t container associated with given
 * pmix_identifier_t.
 */
static pmix_proc_data_t *lookup_proc(pmix_hash_table_t *jtable, uint32_t id, bool create)
{
    pmix_proc_data_t *proc_data = NULL;
    pmix_tma_t *const tma = pmix_obj_get_tma(&jtable->super);

    pmix_hash_table_get_value_uint32(jtable, id, (void **) &proc_data);
    if (NULL == proc_data && create) {
        /* The proc clearly exists, so create a data structure for it */
        proc_data = PMIX_NEW(pmix_proc_data_t, tma);
        if (PMIX_UNLIKELY(NULL == proc_data)) {
            return NULL;
        }
        pmix_hash_table_set_value_uint32(jtable, id, proc_data);
    }

    return proc_data;
}

void pmix_hash_register_key(uint32_t inid,
                            pmix_regattr_input_t *ptr,
                            pmix_keyindex_t *kidx)
{
    pmix_regattr_input_t *p = NULL;
    pmix_keyindex_t *const keyindex = get_keyindex_ptr(kidx);

    if (UINT32_MAX == inid) {
        /* store the pointer in the array */
        pmix_pointer_array_set_item(keyindex->table, (int)keyindex->next_id, ptr);
        ptr->index = keyindex->next_id;
        keyindex->next_id += 1;
        return;
    }

    /* check to see if this key was already registered */
    p = pmix_pointer_array_get_item(keyindex->table, inid);
    if (NULL != p) {
        /* already have this one */
        return;
    }
    /* store the pointer in the table */
    pmix_pointer_array_set_item(keyindex->table, inid, ptr);
}

// skg: Note that one may have to add a TMA wrapper for this call if changes are
// made to how pmix_hash operates. Something for developers to keep in mind.
pmix_regattr_input_t* pmix_hash_lookup_key(uint32_t inid,
                                           const char *key,
                                           pmix_keyindex_t *kidx)
{
    int id;
    pmix_regattr_input_t *ptr = NULL;
    pmix_keyindex_t *const keyindex = get_keyindex_ptr(kidx);

    if (UINT32_MAX == inid) {
        if (NULL == key) {
            /* they have to give us something! */
            return NULL;
        }
        for (id = 0; id < keyindex->table->size; id++) {
            ptr = pmix_pointer_array_get_item(keyindex->table, id);
            if (NULL != ptr) {
                if (0 == strcmp(key, ptr->string)) {
                    return ptr;
                }
            }
        }

        /* we didn't find it - register it */
        ptr = (pmix_regattr_input_t*)pmix_malloc(sizeof(pmix_regattr_input_t));
        ptr->name = strdup(key);
        ptr->string = strdup(key);
        ptr->type = PMIX_UNDEF; // we don't know what type the user will set
        ptr->description = (char**)pmix_malloc(2 * sizeof(char*));
        ptr->description[0] = strdup("USER DEFINED");
        ptr->description[1] = NULL;
        pmix_hash_register_key(UINT32_MAX, ptr, keyindex);
        return ptr;
    }

    /* get the pointer from the table - if it is a reserved key, then
     * it had to be registered at the beginning of time. If it is a
     * non-reserved key, then it had to be registered or else the caller
     * would not have an index to pass us. Thus, the pointer is either
     * found or not - we don't register it if not found. */
    ptr = pmix_pointer_array_get_item(keyindex->table, inid);
    return ptr;
}

static void erase_qualifiers(pmix_proc_data_t *proc,
                             uint32_t index)
{
    pmix_data_array_t *darray;
    pmix_qual_t *qarray;
    size_t n;
    pmix_tma_t *const tma = pmix_obj_get_tma(&proc->super);

    darray = (pmix_data_array_t*)pmix_pointer_array_get_item(proc->quals, index);
    if (NULL == darray || NULL == darray->array) {
        return;
    }
    qarray = (pmix_qual_t*)darray->array;
    for (n=0; n < darray->size; n++) {
        if (NULL != qarray[n].value) {
            pmix_bfrops_base_tma_value_release(&qarray[n].value, tma);
        }
    }
    pmix_tma_free(tma, qarray);
    pmix_tma_free(tma, darray);
    pmix_pointer_array_set_item(proc->quals, index, NULL);
}
