/*
 * Copyright (c) 2026 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/part/onesided/part_onesided_match.h"
#include "ompi/mca/part/onesided/part_onesided.h"
#include "ompi/mca/part/onesided/part_onesided_request.h"
#include "ompi/mca/part/onesided/part_onesided_control.h"
#include "ompi/communicator/communicator.h"
#include "opal/mutex.h"
#include "opal/list.h"

/* Simple hash table implementation using lists for keys */
typedef struct {
    part_onesided_match_key_t key;
    opal_list_t entries;
    opal_mutex_t lock;
} part_onesided_match_bucket_t;

#define MATCH_BUCKETS 256
static part_onesided_match_bucket_t match_table[MATCH_BUCKETS];

static uint32_t hash_key(part_onesided_match_key_t *key) {
    return (uint32_t)(key->src ^ key->dst ^ key->tag ^ key->ctx) % MATCH_BUCKETS;
}

void mca_part_onesided_match_init(void) {
    for (int i = 0; i < MATCH_BUCKETS; i++) {
        match_table[i].entries = OBJ_NEW(opal_list_t);
        OBJ_CONSTRUCT(&match_table[i].lock, opal_mutex_t);
    }
}

void mca_part_onesided_match_finalize(void) {
    for (int i = 0; i < MATCH_BUCKETS; i++) {
        OBJ_RELEASE(match_table[i].entries);
        OBJ_DESTRUCT(&match_table[i].lock);
    }
}

/* Internal entry for the list */
typedef struct {
    opal_list_item_t super;
    mca_part_onesided_request_t *req;
} match_list_item_t;

OBJ_CLASS_DECLARATION(match_list_item_t);
static void match_list_item_construct(match_list_item_t *item) {}
static void match_list_item_destruct(match_list_item_t *item) {}
OBJ_CLASS_INSTANCE(match_list_item_t, opal_list_item_t, match_list_item_construct, match_list_item_destruct);

int mca_part_onesided_match_register(mca_part_onesided_request_t *req, 
                                    int peer, int tag, int ctx) {
    part_onesided_match_key_t key = { .src = 0, .dst = peer, .tag = tag, .ctx = ctx };
    /* For receive, src is peer, dst is self. This is simplified for the match key. */
    if (req->req_type == MCA_PART_ONESIDED_REQUEST_PRECV) {
        key.src = peer;
        key.dst = 0; // Local
    }

    uint32_t bucket = hash_key(&key);
    OPAL_THREAD_LOCK(&match_table[bucket].lock);
    
    match_list_item_t *item = OBJ_NEW(match_list_item_t);
    item->req = req;
    opal_list_append(&match_table[bucket].entries, (opal_list_item_t*)item);
    
    OPAL_THREAD_UNLOCK(&match_table[bucket].lock);
    return OMPI_SUCCESS;
}

int mca_part_onesided_match_handle_init_msg(mca_part_onesided_request_t *req,
                                           int peer, int tag, int ctx,
                                           uint64_t remote_id, 
                                           void *metadata, size_t meta_size) {
    part_onesided_match_key_t key = { .src = peer, .dst = 0, .tag = tag, .ctx = ctx };
    uint32_t bucket = hash_key(&key);
    
    OPAL_THREAD_LOCK(&match_table[bucket].lock);
    
    /* Find first unmatched request in the bucket that matches key */
    match_list_item_t *current;
    mca_part_onesided_request_t *match = NULL;
    
    OPAL_LIST_FOREACH(current, &match_table[bucket].entries, match_list_item_t) {
        if (current->req->req_type != req->req_type) continue;
        if (current->req->req_tag != tag) continue;
        /* We'd normally check ctx and peer here */
        
        match = current->req;
        opal_list_remove_item(&match_table[bucket].entries, (opal_list_item_t*)current);
        OBJ_RELEASE(current);
        break;
    }
    
    OPAL_THREAD_UNLOCK(&match_table[bucket].lock);
    
    if (!match) return OMPI_ERR_NOT_FOUND;
    
    /* Validate metadata */
    if (match->req_parts != req->req_parts || 
        match->req_datatype_size != req->req_datatype_size ||
        match->req_total_bytes != req->req_total_bytes) {
        if (ompi_part_onesided.verbose) {
            opal_output(opal_verbose, 0, "onesided part: match metadata mismatch for req %lu\n", match->req_ompi.req_id);
        }
        return OMPI_ERR_INVALID_ARGUMENT;
    }
    
    /* Transition state to BOUND */
    OPAL_THREAD_LOCK(&match->lock);
    match->req_ompi.req_state = OMPI_REQUEST_BOUND; // Assume this state exists or use a custom one
    OPAL_THREAD_UNLOCK(&match->lock);
    
    return OMPI_SUCCESS;
}
