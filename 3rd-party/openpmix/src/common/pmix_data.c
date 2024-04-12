/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#ifdef HAVE_STRING_H
#    include <string.h>
#endif
#include <errno.h>
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#    include <stdlib.h>
#endif

#include "include/pmix.h"

#include "src/client/pmix_client_ops.h"
#include "src/include/pmix_globals.h"
#include "src/mca/bfrops/bfrops.h"
#include "src/mca/pcompress/pcompress.h"
#include "src/server/pmix_server_ops.h"

#define PMIX_EMBED_DATA_BUFFER(b, db)                       \
    do {                                                    \
        (b)->type = pmix_globals.mypeer->nptr->compat.type; \
        (b)->base_ptr = (db)->base_ptr;                     \
        (b)->pack_ptr = (db)->pack_ptr;                     \
        (b)->unpack_ptr = (db)->unpack_ptr;                 \
        (b)->bytes_allocated = (db)->bytes_allocated;       \
        (b)->bytes_used = (db)->bytes_used;                 \
        (db)->base_ptr = NULL;                              \
        (db)->pack_ptr = NULL;                              \
        (db)->unpack_ptr = NULL;                            \
        (db)->bytes_allocated = 0;                          \
        (db)->bytes_used = 0;                               \
    } while (0)

#define PMIX_EXTRACT_DATA_BUFFER(b, db)               \
    do {                                              \
        (db)->base_ptr = (b)->base_ptr;               \
        (db)->pack_ptr = (b)->pack_ptr;               \
        (db)->unpack_ptr = (b)->unpack_ptr;           \
        (db)->bytes_allocated = (b)->bytes_allocated; \
        (db)->bytes_used = (b)->bytes_used;           \
        (b)->base_ptr = NULL;                         \
        (b)->pack_ptr = NULL;                         \
        (b)->unpack_ptr = NULL;                       \
        (b)->bytes_allocated = 0;                     \
        (b)->bytes_used = 0;                          \
    } while (0)

static pmix_peer_t *find_peer(const pmix_proc_t *proc)
{
    pmix_peer_t *peer;
    pmix_proc_t wildcard;
    pmix_value_t *value;
    int i;

    if (NULL == proc) {
        return pmix_globals.mypeer;
    }

    /* if the target is someone in my nspace, then use my own peer */
    if (0 == strncmp(proc->nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN)) {
        return pmix_globals.mypeer;
    }

    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {
        /* see if we know this proc */
        for (i = 0; i < pmix_server_globals.clients.size; i++) {
            if (NULL
                == (peer = (pmix_peer_t *) pmix_pointer_array_get_item(&pmix_server_globals.clients,
                                                                       i))) {
                continue;
            }
            if (0 == strncmp(proc->nspace, peer->nptr->nspace, PMIX_MAX_NSLEN)) {
                return peer;
            }
        }
        /* didn't find it, so try to get the library version of the target
         * from the host - the result will be cached, so we will only have
         * to retrieve it once */
        pmix_strncpy(wildcard.nspace, proc->nspace, PMIX_MAX_NSLEN);
        wildcard.rank = PMIX_RANK_WILDCARD;
        if (PMIX_SUCCESS != PMIx_Get(&wildcard, PMIX_BFROPS_MODULE, NULL, 0, &value)) {
            /* couldn't get it - nothing we can do */
            return NULL;
        }
        /* setup a peer for this nspace */
        peer = PMIX_NEW(pmix_peer_t);
        if (NULL == peer) {
            PMIX_RELEASE(value);
            return NULL;
        }
        peer->nptr = PMIX_NEW(pmix_namespace_t);
        if (NULL == peer->nptr) {
            PMIX_RELEASE(peer);
            PMIX_RELEASE(value);
            return NULL;
        }
        peer->nptr->nspace = strdup(proc->nspace);
        /* assign a module to it based on the returned version */
        peer->nptr->compat.bfrops = pmix_bfrops_base_assign_module(value->data.string);
        PMIX_RELEASE(value);
        if (NULL == peer->nptr->compat.bfrops) {
            PMIX_RELEASE(peer);
            return NULL;
        }
        /* cache the peer object */
        pmix_pointer_array_add(&pmix_server_globals.clients, peer);
        return peer;
    }

    // we are a client or tool

    /* If the target is for the server, then
     * pack it using that peer. */
    if (0
        == strncmp(proc->nspace, pmix_client_globals.myserver->info->pname.nspace,
                   PMIX_MAX_NSLEN)) {
        return pmix_client_globals.myserver;
    }

    /* try to get the library version of this peer - the result will be
     * cached, so we will only have to retrieve it once */
    pmix_strncpy(wildcard.nspace, proc->nspace, PMIX_MAX_NSLEN);
    wildcard.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != PMIx_Get(&wildcard, PMIX_BFROPS_MODULE, NULL, 0, &value)) {
        /* couldn't get it - nothing we can do */
        return NULL;
    }
    /* setup a peer for this nspace */
    peer = PMIX_NEW(pmix_peer_t);
    if (NULL == peer) {
        PMIX_RELEASE(value);
        return NULL;
    }
    peer->nptr = PMIX_NEW(pmix_namespace_t);
    if (NULL == peer->nptr) {
        PMIX_RELEASE(peer);
        PMIX_RELEASE(value);
        return NULL;
    }
    peer->nptr->nspace = strdup(proc->nspace);
    /* assign a module to it based on the returned version */
    peer->nptr->compat.bfrops = pmix_bfrops_base_assign_module(value->data.string);
    PMIX_RELEASE(value);
    if (NULL == peer->nptr->compat.bfrops) {
        PMIX_RELEASE(peer);
        return NULL;
    }
    /* need to cache the peer someplace so we can clean it
     * up later */
    return peer;
}

PMIX_EXPORT pmix_status_t PMIx_Data_pack(const pmix_proc_t *target, pmix_data_buffer_t *buffer,
                                         void *src, int32_t num_vals, pmix_data_type_t type)
{
    pmix_status_t rc;
    pmix_buffer_t buf;
    pmix_peer_t *peer;

    if (NULL == (peer = find_peer(target))) {
        return PMIX_ERR_NOT_FOUND;
    }

    /* setup the host */
    PMIX_CONSTRUCT(&buf, pmix_buffer_t);

    /* embed the data buffer into a buffer */
    PMIX_EMBED_DATA_BUFFER(&buf, buffer);

    /* pack the value */
    PMIX_BFROPS_PACK(rc, peer, &buf, src, num_vals, type);

    /* extract the data buffer - the pointers may have changed */
    PMIX_EXTRACT_DATA_BUFFER(&buf, buffer);

    /* no need to cleanup as all storage was xfered */
    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Data_unpack(const pmix_proc_t *source, pmix_data_buffer_t *buffer,
                                           void *dest, int32_t *max_num_values,
                                           pmix_data_type_t type)
{
    pmix_status_t rc;
    pmix_buffer_t buf;
    pmix_peer_t *peer;

    if (NULL == (peer = find_peer(source))) {
        return PMIX_ERR_NOT_FOUND;
    }

    /* setup the host */
    PMIX_CONSTRUCT(&buf, pmix_buffer_t);

    /* embed the data buffer into a buffer */
    PMIX_EMBED_DATA_BUFFER(&buf, buffer);

    /* unpack the value */
    PMIX_BFROPS_UNPACK(rc, peer, &buf, dest, max_num_values, type);

    /* extract the data buffer - the pointers may have changed */
    PMIX_EXTRACT_DATA_BUFFER(&buf, buffer);

    /* no need to cleanup as all storage was xfered */
    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Data_copy(void **dest, void *src, pmix_data_type_t type)
{
    pmix_status_t rc;

    /* copy the value */
    PMIX_BFROPS_COPY(rc, pmix_globals.mypeer, dest, src, type);

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Data_print(char **output, char *prefix, void *src,
                                          pmix_data_type_t type)
{
    pmix_status_t rc;

    /* print the value */
    PMIX_BFROPS_PRINT(rc, pmix_globals.mypeer, output, prefix, src, type);

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Data_copy_payload(pmix_data_buffer_t *dest, pmix_data_buffer_t *src)
{
    pmix_status_t rc;
    pmix_buffer_t buf1, buf2;

    /* setup the hosts */
    PMIX_CONSTRUCT(&buf1, pmix_buffer_t);
    PMIX_CONSTRUCT(&buf2, pmix_buffer_t);

    /* embed the data buffer into a buffer */
    PMIX_EMBED_DATA_BUFFER(&buf1, dest);
    PMIX_EMBED_DATA_BUFFER(&buf2, src);

    /* copy payload */
    PMIX_BFROPS_COPY_PAYLOAD(rc, pmix_globals.mypeer, &buf1, &buf2);

    /* extract the dest data buffer - the pointers may have changed */
    PMIX_EXTRACT_DATA_BUFFER(&buf1, dest);
    PMIX_EXTRACT_DATA_BUFFER(&buf2, src);

    /* no need to cleanup as all storage was xfered */
    return rc;
}

pmix_status_t PMIx_Data_unload(pmix_data_buffer_t *buffer, pmix_byte_object_t *payload)
{
    /* check that buffer is not null */
    if (!buffer) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* were we given someplace to point to the payload */
    if (NULL == payload) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* init default response */
    PMIX_BYTE_OBJECT_CONSTRUCT(payload);

    /* anything in the buffer - if not, nothing to do */
    if (NULL == buffer->base_ptr || 0 == buffer->bytes_used) {
        return PMIX_SUCCESS;
    }

    /* if nothing has been unpacked, we can pass the entire
     * region back and protect it - no need to copy. This is
     * an optimization */
    if (buffer->unpack_ptr == buffer->base_ptr) {
        payload->bytes = buffer->base_ptr;
        payload->size = buffer->bytes_used;
        buffer->base_ptr = NULL;
        buffer->bytes_used = 0;
        goto cleanup;
    }

    /* okay, we have something to provide - pass it back */
    payload->size = buffer->bytes_used - (buffer->unpack_ptr - buffer->base_ptr);
    if (0 < payload->size) {
        /* we cannot just set the pointer as it might be
         * partway in a malloc'd region */
        payload->bytes = (char *) malloc(payload->size);
        memcpy(payload->bytes, buffer->unpack_ptr, payload->size);
    }

cleanup:
    /* All done - reset the buffer */
    PMIX_DATA_BUFFER_DESTRUCT(buffer);
    PMIX_DATA_BUFFER_CONSTRUCT(buffer);
    return PMIX_SUCCESS;
}

pmix_status_t PMIx_Data_load(pmix_data_buffer_t *buffer, pmix_byte_object_t *payload)
{
    /* check to see if the buffer has been initialized */
    if (NULL == buffer) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* check if buffer already has payload - free it if so */
    PMIX_DATA_BUFFER_DESTRUCT(buffer);
    PMIX_DATA_BUFFER_CONSTRUCT(buffer);

    /* if it's a NULL payload, just set things and return */
    if (NULL == payload) {
        return PMIX_SUCCESS;
    }

    /* populate the buffer */
    buffer->base_ptr = payload->bytes;

    /* set pack/unpack pointers */
    buffer->pack_ptr = ((char *) buffer->base_ptr) + payload->size;
    buffer->unpack_ptr = buffer->base_ptr;

    /* set counts for size and space */
    buffer->bytes_allocated = buffer->bytes_used = payload->size;

    /* protect the payload */
    payload->bytes = NULL;
    payload->size = 0;

    /* All done */

    return PMIX_SUCCESS;
}

pmix_status_t PMIx_Data_embed(pmix_data_buffer_t *buffer, const pmix_byte_object_t *payload)
{
    pmix_data_buffer_t src;
    pmix_status_t rc;

    /* check to see if the buffer has been initialized */
    if (NULL == buffer) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* check if buffer already has payload - free it if so */
    PMIX_DATA_BUFFER_DESTRUCT(buffer);
    PMIX_DATA_BUFFER_CONSTRUCT(buffer);

    /* if it's a NULL payload, we are done */
    if (NULL == payload) {
        return PMIX_SUCCESS;
    }

    /* setup the source */
    src.base_ptr = payload->bytes;
    src.pack_ptr = ((char *) src.base_ptr) + payload->size;
    src.unpack_ptr = src.base_ptr;
    src.bytes_allocated = src.bytes_used = payload->size;

    /* execute a copy operation */
    rc = PMIx_Data_copy_payload(buffer, &src);
    /* do NOT destruct the source as that would release
     * data in the payload */

    return rc;
}

bool PMIx_Data_compress(const uint8_t *inbytes, size_t size, uint8_t **outbytes, size_t *nbytes)
{
    if (NULL == inbytes) {
        return PMIX_ERR_BAD_PARAM;
    }

    return pmix_compress.compress(inbytes, size, outbytes, nbytes);
}

bool PMIx_Data_decompress(const uint8_t *inbytes, size_t size,
                          uint8_t **outbytes, size_t *nbytes)
{
    if (NULL == inbytes) {
        return PMIX_ERR_BAD_PARAM;
    }

    return pmix_compress.decompress(outbytes, nbytes, inbytes, size);
}
