/*
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>

#include "opal/dss/dss_types.h"
#include "opal/mca/compress/compress.h"
#include "opal/util/argv.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/runtime/orte_globals.h"

#include "orte/util/nidmap.h"

int orte_util_nidmap_create(opal_pointer_array_t *pool,
                            opal_buffer_t *buffer)
{
    char *raw = NULL;
    uint8_t *vpids=NULL, *flags=NULL, u8;
    uint16_t u16;
    uint16_t *slots=NULL;
    uint32_t u32;
    int n, ndaemons, rc, nbytes, nbitmap;
    bool compressed;
    char **names = NULL, **ranks = NULL;
    orte_node_t *nptr;
    opal_byte_object_t bo, *boptr;
    size_t sz;

    /* pack a flag indicating if the HNP was included in the allocation */
    if (orte_hnp_is_allocated) {
        u8 = 1;
    } else {
        u8 = 0;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &u8, 1, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* pack a flag indicating if we are in a managed allocation */
    if (orte_managed_allocation) {
        u8 = 1;
    } else {
        u8 = 0;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &u8, 1, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
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
    if (256 >= pool->size) {
        nbytes = 1;
    } else if (65536 >= pool->size) {
        nbytes = 2;
    } else {
        nbytes = 4;
    }
    vpids = (uint8_t*)malloc(nbytes * pool->size);

    /* make room for the number of slots on each node */
    slots = (uint16_t*)malloc(sizeof(uint16_t) * pool->size);

    /* and for the flags for each node - only need one bit/node */
    nbitmap = (pool->size / 8) + 1;
    flags = (uint8_t*)calloc(1, nbitmap);

    ndaemons = 0;
    for (n=0; n < pool->size; n++) {
        if (NULL == (nptr = (orte_node_t*)opal_pointer_array_get_item(pool, n))) {
            continue;
        }
        /* add the hostname to the argv */
        opal_argv_append_nosize(&names, nptr->name);
        /* store the vpid */
        if (1 == nbytes) {
            if (NULL == nptr->daemon) {
                vpids[ndaemons] = UINT8_MAX;
            } else {
                vpids[ndaemons] = nptr->daemon->name.vpid;
            }
        } else if (2 == nbytes) {
            if (NULL == nptr->daemon) {
                u16 = UINT16_MAX;
            } else {
                u16 = nptr->daemon->name.vpid;
            }
            memcpy(&vpids[nbytes*ndaemons], &u16, 2);
        } else {
            if (NULL == nptr->daemon) {
                u32 = UINT32_MAX;
            } else {
                u32 = nptr->daemon->name.vpid;
            }
            memcpy(&vpids[nbytes*ndaemons], &u32, 4);
        }
        /* store the number of slots */
        slots[n] = nptr->slots;
        /* store the flag */
        if (ORTE_FLAG_TEST(nptr, ORTE_NODE_FLAG_SLOTS_GIVEN)) {
            flags[n/8] |= (1 << (7 - (n % 8)));
        }
        ++ndaemons;
    }

    /* construct the string of node names for compression */
    raw = opal_argv_join(names, ',');
    if (opal_compress.compress_block((uint8_t*)raw, strlen(raw)+1,
                                     (uint8_t**)&bo.bytes, &sz)) {
        /* mark that this was compressed */
        compressed = true;
        bo.size = sz;
    } else {
        /* mark that this was not compressed */
        compressed = false;
        bo.bytes = (uint8_t*)raw;
        bo.size = strlen(raw)+1;
    }
    /* indicate compression */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &compressed, 1, OPAL_BOOL))) {
        if (compressed) {
            free(bo.bytes);
        }
        goto cleanup;
    }
    /* if compressed, provide the uncompressed size */
    if (compressed) {
        sz = strlen(raw)+1;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &sz, 1, OPAL_SIZE))) {
            free(bo.bytes);
            goto cleanup;
        }
    }
    /* add the object */
    boptr = &bo;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &boptr, 1, OPAL_BYTE_OBJECT))) {
        if (compressed) {
            free(bo.bytes);
        }
        goto cleanup;
    }
    if (compressed) {
        free(bo.bytes);
    }

    /* compress the vpids */
    if (opal_compress.compress_block(vpids, nbytes*ndaemons,
                                     (uint8_t**)&bo.bytes, &sz)) {
        /* mark that this was compressed */
        compressed = true;
        bo.size = sz;
    } else {
        /* mark that this was not compressed */
        compressed = false;
        bo.bytes = vpids;
        bo.size = nbytes*ndaemons;
    }
    /* indicate compression */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &compressed, 1, OPAL_BOOL))) {
        if (compressed) {
            free(bo.bytes);
        }
        goto cleanup;
    }
    /* provide the #bytes/vpid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &nbytes, 1, OPAL_INT))) {
        if (compressed) {
            free(bo.bytes);
        }
        goto cleanup;
    }
    /* if compressed, provide the uncompressed size */
    if (compressed) {
        sz = nbytes*ndaemons;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &sz, 1, OPAL_SIZE))) {
            free(bo.bytes);
            goto cleanup;
        }
    }
    /* add the object */
    boptr = &bo;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &boptr, 1, OPAL_BYTE_OBJECT))) {
        if (compressed) {
            free(bo.bytes);
        }
        goto cleanup;
    }
    if (compressed) {
        free(bo.bytes);
    }

    /* compress the slots */
    if (opal_compress.compress_block((uint8_t*)slots, sizeof(uint16_t)*ndaemons,
                                     (uint8_t**)&bo.bytes, &sz)) {
        /* mark that this was compressed */
        compressed = true;
        bo.size = sz;
    } else {
        /* mark that this was not compressed */
        compressed = false;
        bo.bytes = (uint8_t*)slots;
        bo.size = sizeof(uint16_t)*ndaemons;
    }
    /* indicate compression */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &compressed, 1, OPAL_BOOL))) {
        if (compressed) {
            free(bo.bytes);
        }
        goto cleanup;
    }
    /* if compressed, provide the uncompressed size */
    if (compressed) {
        sz = sizeof(uint16_t)*ndaemons;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &sz, 1, OPAL_SIZE))) {
            free(bo.bytes);
            goto cleanup;
        }
    }
    /* add the object */
    boptr = &bo;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &boptr, 1, OPAL_BYTE_OBJECT))) {
        if (compressed) {
            free(bo.bytes);
        }
        goto cleanup;
    }
    if (compressed) {
        free(bo.bytes);
    }

    /* compress the flags */
    if (opal_compress.compress_block(flags, nbitmap,
                                     (uint8_t**)&bo.bytes, &sz)) {
        /* mark that this was compressed */
        compressed = true;
        bo.size = sz;
    } else {
        /* mark that this was not compressed */
        compressed = false;
        bo.bytes = flags;
        bo.size = nbitmap;
    }
    /* indicate compression */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &compressed, 1, OPAL_BOOL))) {
        if (compressed) {
            free(bo.bytes);
        }
        goto cleanup;
    }
    /* if compressed, provide the uncompressed size */
    if (compressed) {
        sz = nbitmap;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &sz, 1, OPAL_SIZE))) {
            free(bo.bytes);
            goto cleanup;
        }
    }
    /* add the object */
    boptr = &bo;
    rc = opal_dss.pack(buffer, &boptr, 1, OPAL_BYTE_OBJECT);

  cleanup:
    if (NULL != names) {
        opal_argv_free(names);
    }
    if (NULL != raw) {
        free(raw);
    }
    if (NULL != ranks) {
        opal_argv_free(ranks);
    }
    if (NULL != vpids) {
        free(vpids);
    }
    if (NULL != slots) {
        free(slots);
    }
    if (NULL != flags) {
        free(flags);
    }

    return rc;
}

int orte_util_decode_nidmap(opal_buffer_t *buf)
{
    uint8_t u8, *vp8 = NULL, *flags = NULL;
    uint16_t *vp16 = NULL, *slots = NULL;
    uint32_t *vp32 = NULL, vpid;
    int cnt, rc, nbytes, n;
    bool compressed;
    size_t sz;
    opal_byte_object_t *boptr;
    char *raw = NULL, **names = NULL;
    orte_node_t *nd;
    orte_job_t *daemons;
    orte_proc_t *proc;
    orte_topology_t *t;

    /* unpack the flag indicating if HNP is in allocation */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &u8, &cnt, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (1 == u8) {
        orte_hnp_is_allocated = true;
    } else {
        orte_hnp_is_allocated = false;
    }

    /* unpack the flag indicating if we are in managed allocation */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &u8, &cnt, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (1 == u8) {
        orte_managed_allocation = true;
    } else {
        orte_managed_allocation = false;
    }

    /* unpack compression flag for node names */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &compressed, &cnt, OPAL_BOOL))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if compressed, get the uncompressed size */
    if (compressed) {
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &sz, &cnt, OPAL_SIZE))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* unpack the nodename object */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &boptr, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if compressed, decompress */
    if (compressed) {
        if (!opal_compress.decompress_block((uint8_t**)&raw, sz,
                                            boptr->bytes, boptr->size)) {
            ORTE_ERROR_LOG(ORTE_ERROR);
            if (NULL != boptr->bytes) {
                free(boptr->bytes);
            }
            free(boptr);
            rc = ORTE_ERROR;
            goto cleanup;
        }
    } else {
        raw = (char*)boptr->bytes;
        boptr->bytes = NULL;
        boptr->size = 0;
    }
    if (NULL != boptr->bytes) {
        free(boptr->bytes);
    }
    free(boptr);
    names = opal_argv_split(raw, ',');
    free(raw);


    /* unpack compression flag for daemon vpids */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &compressed, &cnt, OPAL_BOOL))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* unpack the #bytes/vpid */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &nbytes, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if compressed, get the uncompressed size */
    if (compressed) {
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &sz, &cnt, OPAL_SIZE))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* unpack the vpid object */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &boptr, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if compressed, decompress */
    if (compressed) {
        if (!opal_compress.decompress_block((uint8_t**)&vp8, sz,
                                            boptr->bytes, boptr->size)) {
            ORTE_ERROR_LOG(ORTE_ERROR);
            if (NULL != boptr->bytes) {
                free(boptr->bytes);
            }
            free(boptr);
            rc = ORTE_ERROR;
            goto cleanup;
        }
    } else {
        vp8 = (uint8_t*)boptr->bytes;
        boptr->bytes = NULL;
        boptr->size = 0;
    }
    if (NULL != boptr->bytes) {
        free(boptr->bytes);
    }
    free(boptr);
    if (2 == nbytes) {
        vp16 = (uint16_t*)vp8;
        vp8 = NULL;
    } else if (4 == nbytes) {
        vp32 = (uint32_t*)vp8;
        vp8 = NULL;
    }


    /* unpack compression flag for slots */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &compressed, &cnt, OPAL_BOOL))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if compressed, get the uncompressed size */
    if (compressed) {
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &sz, &cnt, OPAL_SIZE))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* unpack the slots object */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &boptr, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if compressed, decompress */
    if (compressed) {
        if (!opal_compress.decompress_block((uint8_t**)&slots, sz,
                                            boptr->bytes, boptr->size)) {
            ORTE_ERROR_LOG(ORTE_ERROR);
            if (NULL != boptr->bytes) {
                free(boptr->bytes);
            }
            free(boptr);
            rc = ORTE_ERROR;
            goto cleanup;
        }
    } else {
        slots = (uint16_t*)boptr->bytes;
        boptr->bytes = NULL;
        boptr->size = 0;
    }
    if (NULL != boptr->bytes) {
        free(boptr->bytes);
    }
    free(boptr);


    /* unpack compression flag for node flags */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &compressed, &cnt, OPAL_BOOL))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if compressed, get the uncompressed size */
    if (compressed) {
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &sz, &cnt, OPAL_SIZE))) {
            ORTE_ERROR_LOG(rc);
        goto cleanup;
        }
    }

    /* unpack the node flags object */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &boptr, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if compressed, decompress */
    if (compressed) {
        if (!opal_compress.decompress_block((uint8_t**)&flags, sz,
                                            boptr->bytes, boptr->size)) {
            ORTE_ERROR_LOG(ORTE_ERROR);
            if (NULL != boptr->bytes) {
                free(boptr->bytes);
            }
            free(boptr);
            rc = ORTE_ERROR;
            goto cleanup;
        }
    } else {
        flags = (uint8_t*)boptr->bytes;
        boptr->bytes = NULL;
        boptr->size = 0;
    }
    if (NULL != boptr->bytes) {
        free(boptr->bytes);
    }
    free(boptr);

    /* if we are the HNP, we don't need any of this stuff */
    if (ORTE_PROC_IS_HNP) {
        goto cleanup;
    }

    /* get the daemon job object */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);

    /* get our topology */
    for (n=0; n < orte_node_topologies->size; n++) {
        if (NULL != (t = (orte_topology_t*)opal_pointer_array_get_item(orte_node_topologies, n))) {
            break;
        }
    }

    /* create the node pool array - this will include
     * _all_ nodes known to the allocation */
    for (n=0; NULL != names[n]; n++) {
        /* add this name to the pool */
        nd = OBJ_NEW(orte_node_t);
        nd->name = names[n];
        opal_pointer_array_set_item(orte_node_pool, n, nd);
        /* set the #slots */
        nd->slots = slots[n];
        /* set the flags */
        if (1 == flags[n]) {
            ORTE_FLAG_SET(nd, ORTE_NODE_FLAG_SLOTS_GIVEN);
        }
        /* set the topology */
#if !OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        nd->topology = t;
#endif
        /* see if it has a daemon on it */
        if (1 == nbytes && UINT8_MAX != vp8[n]) {
            vpid = vp8[n];
        } else if (2 == nbytes && UINT16_MAX != vp16[n]) {
            vpid = vp16[n];
        } else if (4 == nbytes && UINT32_MAX != vp32[n]) {
            vpid = vp32[n];
        } else {
            vpid = UINT32_MAX;
        }
        if (UINT32_MAX != vpid &&
            NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, vpid))) {
            proc = OBJ_NEW(orte_proc_t);
            proc->name.jobid = ORTE_PROC_MY_NAME->jobid;
            proc->name.vpid = vpid;
            proc->state = ORTE_PROC_STATE_RUNNING;
            ORTE_FLAG_SET(proc, ORTE_PROC_FLAG_ALIVE);
            daemons->num_procs++;
            opal_pointer_array_set_item(daemons->procs, proc->name.vpid, proc);
        }
        nd->index = proc->name.vpid;
        OBJ_RETAIN(nd);
        proc->node = nd;
        OBJ_RETAIN(proc);
        nd->daemon = proc;
    }

    /* update num procs */
    if (orte_process_info.num_procs != daemons->num_procs) {
        orte_process_info.num_procs = daemons->num_procs;
        /* need to update the routing plan */
        orte_routed.update_routing_plan(NULL);
    }

    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

  cleanup:
    return rc;
}

int orte_util_generate_ppn(orte_job_t *jdata,
                           opal_buffer_t *buf)
{
    uint16_t *ppn=NULL;
    size_t nbytes;
    int rc = ORTE_SUCCESS;
    orte_app_idx_t i;
    int j, k;
    opal_byte_object_t bo, *boptr;
    bool compressed;
    orte_node_t *nptr;
    orte_proc_t *proc;
    size_t sz;

    /* make room for the number of procs on each node */
    nbytes = sizeof(uint16_t) * orte_node_pool->size;
    ppn = (uint16_t*)malloc(nbytes);

    for (i=0; i < jdata->num_apps; i++) {
        /* reset the #procs */
        memset(ppn, 0, nbytes);
        /* for each app_context, compute the #procs on
         * each node of the allocation */
        for (j=0; j < orte_node_pool->size; j++) {
            if (NULL == (nptr = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, j))) {
                continue;
            }
            if (NULL == nptr->daemon) {
                continue;
            }
            for (k=0; k < nptr->procs->size; k++) {
                if (NULL != (proc = (orte_proc_t*)opal_pointer_array_get_item(nptr->procs, k))) {
                    if (proc->name.jobid == jdata->jobid) {
                        ++ppn[j];
                    }
                }
            }
        }
        if (opal_compress.compress_block((uint8_t*)ppn, nbytes,
                                         (uint8_t**)&bo.bytes, &sz)) {
            /* mark that this was compressed */
            compressed = true;
            bo.size = sz;
        } else {
            /* mark that this was not compressed */
            compressed = false;
            bo.bytes = (uint8_t*)ppn;
            bo.size = nbytes;
        }
        /* indicate compression */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &compressed, 1, OPAL_BOOL))) {
            if (compressed) {
                free(bo.bytes);
            }
            goto cleanup;
        }
        /* if compressed, provide the uncompressed size */
        if (compressed) {
            sz = nbytes;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &sz, 1, OPAL_SIZE))) {
                free(bo.bytes);
                goto cleanup;
            }
        }
        /* add the object */
        boptr = &bo;
        rc = opal_dss.pack(buf, &boptr, 1, OPAL_BYTE_OBJECT);
        if (OPAL_SUCCESS != rc) {
            break;
        }
    }

  cleanup:
    free(ppn);
    return rc;
}

int orte_util_decode_ppn(orte_job_t *jdata,
                         opal_buffer_t *buf)
{
    orte_app_idx_t n;
    int m, cnt, rc;
    opal_byte_object_t *boptr;
    bool compressed;
    size_t sz;
    uint16_t *ppn, k;
    orte_node_t *node;
    orte_proc_t *proc;

    for (n=0; n < jdata->num_apps; n++) {
        /* unpack the compression flag */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &compressed, &cnt, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* if compressed, unpack the raw size */
        if (compressed) {
            cnt = 1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &sz, &cnt, OPAL_SIZE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        /* unpack the byte object describing this app */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &boptr, &cnt, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        if (ORTE_PROC_IS_HNP) {
            /* just discard it */
            free(boptr->bytes);
            free(boptr);
            continue;
        }

        /* decompress if required */
        if (compressed) {
            if (!opal_compress.decompress_block((uint8_t**)&ppn, sz,
                                                boptr->bytes, boptr->size)) {
                ORTE_ERROR_LOG(ORTE_ERROR);
                OBJ_RELEASE(boptr);
                return ORTE_ERROR;
            }
        } else {
            ppn = (uint16_t*)boptr->bytes;
            boptr->bytes = NULL;
            boptr->size = 0;
        }
        if (NULL != boptr->bytes) {
            free(boptr->bytes);
        }
        free(boptr);

        /* cycle thru the node pool */
        for (m=0; m < orte_node_pool->size; m++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, m))) {
                continue;
            }
            if (0 < ppn[m]) {
                if (!ORTE_FLAG_TEST(node, ORTE_NODE_FLAG_MAPPED)) {
                    OBJ_RETAIN(node);
                    ORTE_FLAG_SET(node, ORTE_NODE_FLAG_MAPPED);
                    opal_pointer_array_add(jdata->map->nodes, node);
                }
                /* create a proc object for each one */
                for (k=0; k < ppn[m]; k++) {
                    proc = OBJ_NEW(orte_proc_t);
                    proc->name.jobid = jdata->jobid;
                    /* leave the vpid undefined as this will be determined
                     * later when we do the overall ranking */
                    proc->app_idx = n;
                    proc->parent = node->daemon->name.vpid;
                    OBJ_RETAIN(node);
                    proc->node = node;
                    /* flag the proc as ready for launch */
                    proc->state = ORTE_PROC_STATE_INIT;
                    opal_pointer_array_add(node->procs, proc);
                    /* we will add the proc to the jdata array when we
                     * compute its rank */
                }
                node->num_procs += ppn[m];
            }
        }
        free(ppn);
    }

    return ORTE_SUCCESS;
}
