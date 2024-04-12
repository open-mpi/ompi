/*
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2020      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"
#include "types.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <ctype.h>

#include "src/util/pmix_argv.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/rmaps/base/base.h"
#include "src/rml/rml.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"

#include "src/util/nidmap.h"

int prte_util_nidmap_create(pmix_pointer_array_t *pool, pmix_data_buffer_t *buffer)
{
    char *raw = NULL;
    pmix_rank_t *vpids = NULL;
    uint8_t u8;
    int n, m, ndaemons, nbytes;
    bool compressed;
    char **names = NULL;
    char **aliases = NULL, **als;
    prte_node_t *nptr;
    pmix_byte_object_t bo;
    size_t sz;
    pmix_status_t rc;

    /* pack a flag indicating if the HNP was included in the allocation */
    if (prte_hnp_is_allocated) {
        u8 = 1;
    } else {
        u8 = 0;
    }
    rc = PMIx_Data_pack(PRTE_PROC_MY_NAME, buffer, &u8, 1, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* pack a flag indicating if we are in a managed allocation */
    if (prte_managed_allocation) {
        u8 = 1;
    } else {
        u8 = 0;
    }
    rc = PMIx_Data_pack(PRTE_PROC_MY_NAME, buffer, &u8, 1, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* daemon vpids start from 0 and increase linearly by one
     * up to the number of nodes in the system. The vpid is
     * a 32-bit value. We don't know how many of the nodes
     * in the system have daemons - we may not be using them
     * all just yet. However, even the largest systems won't
     * have more than a million nodes for quite some time,
     * so for now we'll just allocate enough space to hold
     * them all. Someone can optimize this further later */
    nbytes = prte_process_info.num_daemons * sizeof(pmix_rank_t);
    vpids = (pmix_rank_t *) malloc(nbytes);

    ndaemons = 0;
    for (n = 0; n < pool->size; n++) {
        if (NULL == (nptr = (prte_node_t *) pmix_pointer_array_get_item(pool, n))) {
            continue;
        }
        /* add the hostname to the argv */
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&names, nptr->name);
        als = NULL;
        if (NULL != nptr->aliases) {
            for (m=0; NULL != nptr->aliases[m]; m++) {
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&als, nptr->aliases[m]);
            }
            raw = PMIX_ARGV_JOIN_COMPAT(als, ',');
            PMIX_ARGV_FREE_COMPAT(als);
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&aliases, raw);
            free(raw);
        } else {
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&aliases, "PRTENONE");
        }
        /* store the vpid */
        if (NULL == nptr->daemon) {
            vpids[ndaemons] = PMIX_RANK_INVALID;
        } else {
            vpids[ndaemons] = nptr->daemon->name.rank;
        }
        ++ndaemons;
    }

    /* little protection */
    if (NULL == names || NULL == aliases) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        free(vpids);
        return PRTE_ERR_NOT_FOUND;
    }

    /* construct the string of node names for compression */
    raw = PMIX_ARGV_JOIN_COMPAT(names, ',');
    PMIX_ARGV_FREE_COMPAT(names);
    if (PMIx_Data_compress((uint8_t *) raw, strlen(raw) + 1, (uint8_t **) &bo.bytes, &sz)) {
        /* mark that this was compressed */
        compressed = true;
        bo.size = sz;
        free(raw);
    } else {
        /* mark that this was not compressed */
        compressed = false;
        bo.bytes = (char *) raw;
        bo.size = strlen(raw) + 1;
    }
    /* indicate compression */
    rc = PMIx_Data_pack(PRTE_PROC_MY_NAME, buffer, &compressed, 1, PMIX_BOOL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        free(bo.bytes);
        free(vpids);
        return rc;
    }
    /* add the object */
    rc = PMIx_Data_pack(PRTE_PROC_MY_NAME, buffer, &bo, 1, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        free(bo.bytes);
        free(vpids);
        return rc;
    }
    free(bo.bytes);

    /* construct the string of aliases for compression */
    raw = PMIX_ARGV_JOIN_COMPAT(aliases, ';');
    PMIX_ARGV_FREE_COMPAT(aliases);
    if (PMIx_Data_compress((uint8_t *) raw, strlen(raw) + 1, (uint8_t **) &bo.bytes, &sz)) {
        /* mark that this was compressed */
        compressed = true;
        bo.size = sz;
        free(raw);
    } else {
        /* mark that this was not compressed */
        compressed = false;
        bo.bytes = (char *) raw;
        bo.size = strlen(raw) + 1;
    }
    /* indicate compression */
    rc = PMIx_Data_pack(PRTE_PROC_MY_NAME, buffer, &compressed, 1, PMIX_BOOL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        free(bo.bytes);
        free(vpids);
        return rc;
    }
    /* add the object */
    rc = PMIx_Data_pack(PRTE_PROC_MY_NAME, buffer, &bo, 1, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        free(bo.bytes);
        free(vpids);
        return rc;
    }
    free(bo.bytes);

    /* compress the vpids */
    if (PMIx_Data_compress((uint8_t *) vpids, nbytes, (uint8_t **) &bo.bytes, &sz)) {
        /* mark that this was compressed */
        compressed = true;
        bo.size = sz;
        free(vpids);
    } else {
        /* mark that this was not compressed */
        compressed = false;
        bo.bytes = (char *) vpids;
        bo.size = ndaemons * sizeof(pmix_rank_t);
    }
    /* indicate compression */
    rc = PMIx_Data_pack(PRTE_PROC_MY_NAME, buffer, &compressed, 1, PMIX_BOOL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        free(bo.bytes);
        return rc;
    }
    /* add the object */
    rc = PMIx_Data_pack(PRTE_PROC_MY_NAME, buffer, &bo, 1, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        free(bo.bytes);
        return rc;
    }
    free(bo.bytes);

    return rc;
}

int prte_util_decode_nidmap(pmix_data_buffer_t *buf)
{
    uint8_t u8;
    pmix_rank_t *vpid = NULL;
    int cnt, n;
    bool compressed;
    size_t sz;
    pmix_byte_object_t pbo;
    char *raw = NULL, **names = NULL, **aliases = NULL;
    prte_node_t *nd;
    prte_job_t *daemons;
    prte_proc_t *proc;
    prte_topology_t *t = NULL;
    pmix_status_t rc;

    /* unpack the flag indicating if HNP is in allocation */
    cnt = 1;
    rc = PMIx_Data_unpack(PRTE_PROC_MY_NAME, buf, &u8, &cnt, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    if (1 == u8) {
        prte_hnp_is_allocated = true;
    } else {
        prte_hnp_is_allocated = false;
    }

    /* unpack the flag indicating if we are in managed allocation */
    cnt = 1;
    rc = PMIx_Data_unpack(PRTE_PROC_MY_NAME, buf, &u8, &cnt, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    if (1 == u8) {
        prte_managed_allocation = true;
    } else {
        prte_managed_allocation = false;
    }

    /* unpack compression flag for node names */
    cnt = 1;
    rc = PMIx_Data_unpack(PRTE_PROC_MY_NAME, buf, &compressed, &cnt, PMIX_BOOL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* unpack the nodename object */
    cnt = 1;
    rc = PMIx_Data_unpack(PRTE_PROC_MY_NAME, buf, &pbo, &cnt, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if compressed, decompress */
    if (compressed) {
        if (!PMIx_Data_decompress((uint8_t *) pbo.bytes, pbo.size, (uint8_t **) &raw, &sz)) {
            PRTE_ERROR_LOG(PRTE_ERROR);
            PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
            rc = PRTE_ERROR;
            goto cleanup;
        }
    } else {
        raw = (char *) pbo.bytes;
        pbo.bytes = NULL; // protect the data
        pbo.size = 0;
    }
    PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
    names = PMIX_ARGV_SPLIT_COMPAT(raw, ',');
    free(raw);

    /* unpack compression flag for node aliases */
    cnt = 1;
    rc = PMIx_Data_unpack(PRTE_PROC_MY_NAME, buf, &compressed, &cnt, PMIX_BOOL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* unpack the aliases object */
    cnt = 1;
    rc = PMIx_Data_unpack(PRTE_PROC_MY_NAME, buf, &pbo, &cnt, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if compressed, decompress */
    if (compressed) {
        if (!PMIx_Data_decompress((uint8_t *) pbo.bytes, pbo.size, (uint8_t **) &raw, &sz)) {
            PRTE_ERROR_LOG(PRTE_ERROR);
            PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
            rc = PRTE_ERROR;
            goto cleanup;
        }
    } else {
        raw = (char *) pbo.bytes;
        pbo.bytes = NULL; // protect the data
        pbo.size = 0;
    }
    PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
    aliases = PMIX_ARGV_SPLIT_COMPAT(raw, ';');
    free(raw);

    /* unpack compression flag for daemon vpids */
    cnt = 1;
    rc = PMIx_Data_unpack(PRTE_PROC_MY_NAME, buf, &compressed, &cnt, PMIX_BOOL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* unpack the vpid object */
    cnt = 1;
    rc = PMIx_Data_unpack(PRTE_PROC_MY_NAME, buf, &pbo, &cnt, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if compressed, decompress */
    if (compressed) {
        if (!PMIx_Data_decompress((uint8_t *) pbo.bytes, pbo.size, (uint8_t **) &vpid, &sz)) {
            PRTE_ERROR_LOG(PRTE_ERROR);
            PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
            rc = PRTE_ERROR;
            goto cleanup;
        }
    } else {
        vpid = (pmix_rank_t *) pbo.bytes;
        sz = pbo.size;
        pbo.bytes = NULL;
        pbo.size = 0;
    }
    PMIX_BYTE_OBJECT_DESTRUCT(&pbo);

    /* if we are the HNP, we don't need any of this stuff */
    if (PRTE_PROC_IS_MASTER) {
        rc = PRTE_SUCCESS;
        goto cleanup;
    }

    /* get the daemon job object */
    daemons = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);

    /* get our topology */
    t = (prte_topology_t *) pmix_pointer_array_get_item(prte_node_topologies, 0);
    if (NULL == t) {
        /* should never happen */
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        rc = PRTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    /* create the node pool array - this will include
     * _all_ nodes known to the allocation */
    for (n = 0; NULL != names[n]; n++) {
        /* do we already have this node? */
        nd = (prte_node_t*)pmix_pointer_array_get_item(prte_node_pool, n);
        if (NULL != nd) {
            /* check the name */
            if (0 != strcmp(nd->name, names[n])) {
                free(nd->name);
                nd->name = strdup(names[n]);
            }
            if (0 != strcmp(aliases[n], "PRTENONE")) {
                if (NULL != nd->aliases) {
                    PMIX_ARGV_FREE_COMPAT(nd->aliases);
                }
                nd->aliases = PMIX_ARGV_SPLIT_COMPAT(aliases[n], ',');
            }
            continue;
        }
        /* add this name to the pool */
        nd = PMIX_NEW(prte_node_t);
        nd->name = strdup(names[n]);
        nd->index = n;
        pmix_pointer_array_set_item(prte_node_pool, n, nd);
        /* add any aliases */
        if (0 != strcmp(aliases[n], "PRTENONE")) {
            nd->aliases = PMIX_ARGV_SPLIT_COMPAT(aliases[n], ',');
        }
        /* set the topology - always default to homogeneous
         * as that is the most common scenario */
        nd->topology = t;
        /* see if it has a daemon on it */
        if (PMIX_RANK_INVALID != vpid[n]) {
            proc = (prte_proc_t *) pmix_pointer_array_get_item(daemons->procs, vpid[n]);
            if (NULL == proc) {
                proc = PMIX_NEW(prte_proc_t);
                PMIX_LOAD_PROCID(&proc->name, PRTE_PROC_MY_NAME->nspace, vpid[n]);
                proc->state = PRTE_PROC_STATE_RUNNING;
                PRTE_FLAG_SET(proc, PRTE_PROC_FLAG_ALIVE);
                daemons->num_procs++;
                pmix_pointer_array_set_item(daemons->procs, proc->name.rank, proc);
            }
            PMIX_RETAIN(nd);
            proc->node = nd;
            PMIX_RETAIN(proc);
            nd->daemon = proc;
        }
    }

    /* update num procs */
    if (prte_process_info.num_daemons != daemons->num_procs) {
        prte_process_info.num_daemons = daemons->num_procs;
        /* update the routing tree */
        prte_rml_compute_routing_tree();
    }


cleanup:
    if (NULL != vpid) {
        free(vpid);
    }
    if (NULL != names) {
        PMIX_ARGV_FREE_COMPAT(names);
    }
    return rc;
}
