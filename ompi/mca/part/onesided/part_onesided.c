/*
 * Copyright (c) 2026 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/part/onesided/part_onesided.h"
#include "ompi/mca/part/onesided/part_onesided_request.h"
#include "ompi/mca/part/onesided/part_onesided_control.h"
#include "ompi/mca/part/onesided/part_onesided_match.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/bml/base/base.h"
#include "opal/mca/btl/btl.h"

/*
 * BTL Completion Semantics:
 * We assume that BTL puts provide a completion callback (mca_btl_base_rdma_completion_fn_t).
 * This callback is invoked when the local buffer can be reused or deregistered.
 * For the purpose of this component, we treat this callback as the signal that 
 * the data has been successfully queued to the network. 
 * We assume the BTL handles the necessary memory registration internally via 
 * the BTL registration cache if the buffer is not already registered.
 */

static void mca_part_onesided_put_completion_updated(struct mca_btl_base_module_t *btl, 
                                           struct mca_btl_base_endpoint_t *endpoint,
                                           void *local_address, 
                                           struct mca_btl_base_registration_handle_t *local_handle, 
                                           void *context, void *cbdata, int status) {
    mca_part_onesided_request_t *req = (mca_part_onesided_request_t *)context;
    
    /* Calculate which partition just completed */
    uintptr_t diff = (uintptr_t)local_address - (uintptr_t)req->req_buf;
    size_t part_idx = diff / req->req_part_bytes;
    
    OPAL_THREAD_LOCK(&req->lock);
    
    req->completed_bitmap[part_idx / 8] |= (1 << (part_idx % 8));
    req->completed_count++;
    
    if (ompi_part_onesided.verbose) {
        opal_output(opal_verbose, 0, "onesided part: partition %zu completed for req %lu\n", part_idx, req->req_ompi.req_id);
    }
    
    /* Send PART_COMPLETE to receiver */
    part_onesided_control_hdr_t hdr = {
        .version = PART_ONESIDED_CTRL_VERSION,
        .msg_type = PART_ONESIDED_CTRL_PART_COMPLETE,
        .request_id = (uint64_t)req
    };
    
    mca_part_onesided_send_ctrl(req->btl_endpoint, btl, hdr.msg_type, req->req_id, NULL, 0);
    
    if (req->completed_count == req->req_parts) {
        req->req_ompi.req_state = OMPI_REQUEST_COMPLETE;
        ompi_request_complete(&(req->req_ompi), true);
        mca_part_onesided_remove_active_request(req);
    }
    
    OPAL_THREAD_UNLOCK(&req->lock);
}

static int mca_part_onesided_issue_puts(mca_part_onesided_request_t *req) {
    if (!req->epoch_open_recv) return OMPI_SUCCESS;
    return OMPI_SUCCESS;
    
    mca_btl_base_module_t *btl = NULL;
    ompi_proc_t *proc = ompi_comm_peer_lookup(req->req_comm, req->req_peer);
    mca_bml_base_endpoint_t *bml_endpoint = mca_bml_base_get_endpoint(proc);
    for (int i = 0; i < bml_endpoint->btl_rdma.len; i++) {
        if (bml_endpoint->btl_rdma[i].btl_endpoint == req->btl_endpoint) {
            btl = bml_endpoint->btl_rdma[i].btl;
            break;
        }
    }
    
    if (!btl) return OMPI_ERROR;

    for (size_t i = 0; i < req->req_parts; i++) {
        /* Check if partition is READY and not yet ISSUED */
        if ((req->ready_bitmap[i / 8] & (1 << (i % 8))) && 
            !(req->issued_bitmap[i / 8] & (1 << (i % 8)))) {
            
            uint64_t remote_addr = (uintptr_t)req->remote_metadata + (i * req->req_part_bytes);
            void *local_addr = (char*)req->req_buf + (i * req->req_part_bytes);
            
            int ret = btl->btl_put(btl, req->btl_endpoint, 
                                  local_addr, remote_addr, 
                                  NULL, NULL, // We assume BTL registration cache
                                  req->req_part_bytes, 0, 0, 
                                  mca_part_onesided_put_completion_updated, req, NULL);
            
            if (ret == OPAL_SUCCESS) {
                req->issued_bitmap[i / 8] |= (1 << (i % 8));
                if (ompi_part_onesided.verbose) {
                    opal_output(opal_verbose, 0, "onesided part: issued put for partition %zu\n", i);
                }
            } else if (ret == OPAL_ERR_RESOURCE_BUSY) {
                /* Temporary exhaustion, stop issuing for this request and try next time */
                break;
            } else {
                /* Permanent error */
                req->req_ompi.req_state = OMPI_REQUEST_ERROR;
                return OMPI_ERROR;
            }
        }
    }
    
        OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(return OMPI_SUCCESS;active_requests_lock);active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;
    return OMPI_SUCCESS;
}

static int mca_part_onesided_progress(void) {
    OPAL_THREAD_LOCK(static int mca_part_onesided_progress(void) {active_requests_lock);
    /* 1. Progress control messages */
    mca_part_onesided_ctrl_progress();
    mca_part_onesided_request_t *req;
    OPAL_LIST_FOREACH(req, &active_requests, mca_part_onesided_request_t) {
        mca_part_onesided_issue_puts(req);
    }
    
    /* 2. Issue pending puts for all active requests. 
     * In a real implementation, we'd have a list of active requests.
     * For now, we'll assume we're only progressing the ones the user is waiting on,
     * but we should actually iterate over all requests that are in the ACTIVE state.
     */
    // This part requires a global list of active onesided requests which we should implement.
    
        OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(return OMPI_SUCCESS;active_requests_lock);active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;
    return OMPI_SUCCESS;
}

/* Helper to serialize registration metadata for the sender */
static int mca_part_onesided_serialize_reg_metadata(mca_part_onesided_request_t *req,
                                                   void *metadata_buf, size_t *metadata_size) {
    if (!req->reg_handle) return OMPI_ERROR;
    
    uint64_t remote_addr = (uintptr_t)req->req_buf;
    *metadata_size = sizeof(uint64_t);
    memcpy(metadata_buf, &remote_addr, sizeof(uint64_t));
    
        OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(return OMPI_SUCCESS;active_requests_lock);active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;
    return OMPI_SUCCESS;
}

static int mca_part_onesided_precv_init(void *buf, size_t parts, size_t count,
                                       struct ompi_datatype_t *datatype, int src, int tag,
                                       struct ompi_communicator_t* comm, struct ompi_info_t * info,
                                       struct ompi_request_t **request) {
    if (ompi_part_onesided.verbose) {
        opal_output(opal_verbose, 0, "onesided part: precv_init (src=%d, tag=%d, parts=%zu)\n", src, tag, parts);
    }

    if (src == MPI_ANY_SOURCE || tag == MPI_ANY_TAG) return OMPI_ERR_INVALID_ARGUMENT;
    if (!ompi_datatype_is_contiguous(datatype)) return OMPI_ERR_INVALID_DATATYPE;

    mca_part_onesided_request_t *req;
    OBJ_NEW(mca_part_onesided_request_t, req);
    if (!req) return OMPI_ERR_OUT_OF_RESOURCE;

    req->req_type = MCA_PART_ONESIDED_REQUEST_PRECV;
    req->req_comm = comm;
    ompi_communicator_increment(req->req_comm);
    req->req_datatype = datatype;
    ompi_datatype_increment(req->req_datatype);
    req->req_buf = buf;
    req->req_parts = parts;
    req->req_count = count;
    req->req_peer = src;
    req->req_tag = tag;

    size_t dt_size;
    ompi_datatype_get_size(datatype, &dt_size);
    req->req_datatype_size = dt_size;
    req->req_total_bytes = parts * count * dt_size;
    req->req_part_bytes = count * dt_size;

    req->bitmap_size = (parts + 7) / 8;
    req->ready_bitmap = calloc(req->bitmap_size, 1);
    req->issued_bitmap = calloc(req->bitmap_size, 1);
    req->completed_bitmap = calloc(req->bitmap_size, 1);
    
    ompi_proc_t *proc = ompi_comm_peer_lookup(req->req_comm, src);
    mca_bml_base_endpoint_t *bml_endpoint = mca_bml_base_get_endpoint(proc);
    struct mca_btl_base_endpoint_t *found_endpoint = NULL;
    mca_btl_base_module_t *found_btl = NULL;

    for (int i = 0; i < bml_endpoint->btl_rdma.len; i++) {
        mca_btl_base_module_t *btl = bml_endpoint->btl_rdma[i].btl;
        if (btl->btl_register) {
            found_btl = btl;
            found_endpoint = bml_endpoint->btl_rdma[i].btl_endpoint;
            break;
        }
    }

    if (!found_endpoint) return OMPI_ERR_UNREACH;
    req->btl_endpoint = found_endpoint;

    mca_btl_base_registration_handle_t *handle = NULL;
    if (found_btl->btl_register(found_btl, found_endpoint, req->req_buf, req->req_total_bytes, 
                                MCA_BTL_REG_FLAG_REMOTE_WRITE, &handle) != OPAL_SUCCESS) {
        return OMPI_ERROR;
    }
    req->reg_handle = handle;

    void *meta_buf = malloc(sizeof(uint64_t));
    size_t meta_size = 0;
    mca_part_onesided_serialize_reg_metadata(req, meta_buf, &meta_size);
    
    mca_part_onesided_send_ctrl(req->btl_endpoint, found_btl, PART_ONESIDED_CTRL_INIT_RECV, 
                                (uint64_t)req, src, meta_buf, meta_size);
    
    mca_part_onesided_assign_id(req);
    mca_part_onesided_match_register(req, src, tag, 0);

    *request = (ompi_request_t*) req;
        OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(return OMPI_SUCCESS;active_requests_lock);active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;
    return OMPI_SUCCESS;
}

static int mca_part_onesided_psend_init(const void *buf, size_t parts, size_t count,
                                       struct ompi_datatype_t *datatype, int dst, int tag,
                                       struct ompi_communicator_t* comm, struct ompi_info_t * info,
                                       struct ompi_request_t **request) {
    if (tag == MPI_ANY_TAG) return OMPI_ERR_INVALID_ARGUMENT;
    if (!ompi_datatype_is_contiguous(datatype)) return OMPI_ERR_INVALID_DATATYPE;

    mca_part_onesided_request_t *req;
    OBJ_NEW(mca_part_onesided_request_t, req);
    if (!req) return OMPI_ERR_OUT_OF_RESOURCE;

    req->req_type = MCA_PART_ONESIDED_REQUEST_PSEND;
    req->req_comm = comm;
    ompi_communicator_increment(req->req_comm);
    req->req_datatype = datatype;
    ompi_datatype_increment(req->req_datatype);
    req->req_buf = (void*)buf;
    req->req_parts = parts;
    req->req_count = count;
    req->req_peer = dst;
    req->req_tag = tag;

    size_t dt_size;
    ompi_datatype_get_size(datatype, &dt_size);
    req->req_datatype_size = dt_size;
    req->req_total_bytes = parts * count * dt_size;
    req->req_part_bytes = count * dt_size;

    req->bitmap_size = (parts + 7) / 8;
    req->ready_bitmap = calloc(req->bitmap_size, 1);
    req->issued_bitmap = calloc(req->bitmap_size, 1);
    req->completed_bitmap = calloc(req->bitmap_size, 1);
    
    ompi_proc_t *proc = ompi_comm_peer_lookup(req->req_comm, dst);
    mca_bml_base_endpoint_t *bml_endpoint = mca_bml_base_get_endpoint(proc);
    struct mca_btl_base_endpoint_t *found_endpoint = NULL;
    mca_btl_base_module_t *found_btl = NULL;

    for (int i = 0; i < bml_endpoint->btl_rdma.len; i++) {
        mca_btl_base_module_t *btl = bml_endpoint->btl_rdma[i].btl;
        if (btl->btl_component->btl_version.mca_component_name) {
            found_btl = btl;
            found_endpoint = bml_endpoint->btl_rdma[i].btl_endpoint;
            break;
        }
    }

    if (!found_endpoint) return OMPI_ERR_UNREACH;
    req->btl_endpoint = found_endpoint;

    mca_part_onesided_send_ctrl(req->btl_endpoint, found_btl, PART_ONESIDED_CTRL_INIT_SEND, 
                                (uint64_t)req, dst, NULL, 0);
    
    mca_part_onesided_assign_id(req);
    mca_part_onesided_match_register(req, dst, tag, 0);

    *request = (ompi_request_t*) req;
        OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(return OMPI_SUCCESS;active_requests_lock);active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;
    return OMPI_SUCCESS;
}

static int mca_part_onesided_start(size_t count, ompi_request_t** requests) {
    if (ompi_part_onesided.verbose) {
        opal_output(opal_verbose, 0, "onesided part: start (%zu requests)\n", count);
    }
    
    int err = OMPI_SUCCESS;
    for (size_t i = 0; i < count; i++) {
        mca_part_onesided_request_t *req = (mca_part_onesided_request_t *)requests[i];
        
        OPAL_THREAD_LOCK(&req->lock);
        
        if (req->req_ompi.req_state == OMPI_REQUEST_ACTIVE) {
            if (ompi_part_onesided.verbose) {
                opal_output(opal_verbose, 0, "onesided part: start called on already active request\n");
            }
            OPAL_THREAD_UNLOCK(&req->lock);
            err = OMPI_ERR_INVALID_ARGUMENT; 
            continue;
        }

        req->epoch++;
        memset(req->ready_bitmap, 0, req->bitmap_size);
        memset(req->issued_bitmap, 0, req->bitmap_size);
        memset(req->completed_bitmap, 0, req->bitmap_size);
        req->completed_count = 0;
        req->epoch_open_sent = true;
        req->epoch_open_recv = false;
        
        mca_btl_base_module_t *btl = NULL;
        ompi_proc_t *proc = ompi_comm_peer_lookup(req->req_comm, req->req_peer);
        mca_bml_base_endpoint_t *bml_endpoint = mca_bml_base_get_endpoint(proc);
        for (int i = 0; i < bml_endpoint->btl_rdma.len; i++) {
            if (bml_endpoint->btl_rdma[i].btl_endpoint == req->btl_endpoint) {
                btl = bml_endpoint->btl_rdma[i].btl;
                break;
            }
        }

        if (btl) {
            part_onesided_epoch_open_t epoch_msg = { .epoch = req->epoch };
            mca_part_onesided_send_ctrl(req->btl_endpoint, btl, PART_ONESIDED_CTRL_EPOCH_OPEN, 
                                        (uint64_t)req, req->req_peer, &epoch_msg, sizeof(epoch_msg));
        }
        
        req->req_ompi.req_state = OMPI_REQUEST_ACTIVE;
        mca_part_onesided_add_active_request(req);
        OPAL_THREAD_UNLOCK(&req->lock);
    }
    
    return err;
}

static int mca_part_onesided_pready(size_t min_part, size_t max_part, struct ompi_request_t* request) {
    mca_part_onesided_request_t *req = (mca_part_onesided_request_t *)request;
    if (min_part >= req->req_parts || max_part >= req->req_parts) return OMPI_ERR_INVALID_ARGUMENT;
    OPAL_THREAD_LOCK(&req->lock);
    if (req->req_ompi.req_state != OMPI_REQUEST_ACTIVE) {
        OPAL_THREAD_UNLOCK(&req->lock);
        return OMPI_ERR_INVALID_ARGUMENT;
    }
    for (size_t i = min_part; i <= max_part; i++) {
        req->ready_bitmap[i / 8] |= (1 << (i % 8));
    }
    OPAL_THREAD_UNLOCK(&req->lock);
        OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(return OMPI_SUCCESS;active_requests_lock);active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;
    return OMPI_SUCCESS;
}

static int mca_part_onesided_parrived(size_t min_part, size_t max_part, int* flag, struct ompi_request_t* request) {
    mca_part_onesided_request_t *req = (mca_part_onesided_request_t *)request;
    OPAL_THREAD_LOCK(&req->lock);
    if (req->req_ompi.req_state != OMPI_REQUEST_ACTIVE) {
        OPAL_THREAD_UNLOCK(&req->lock);
        return OMPI_ERR_INVALID_ARGUMENT;
    }
    *flag = 0;
    OPAL_THREAD_UNLOCK(&req->lock);
        OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(    OPAL_THREAD_UNLOCK(return OMPI_SUCCESS;active_requests_lock);active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;active_requests_lock);
    return OMPI_SUCCESS;
    return OMPI_SUCCESS;
}

ompi_part_onesided_t ompi_part_onesided = {
    .super = {
        .part_progress = mca_part_onesided_progress,
        .part_precv_init = mca_part_onesided_precv_init,
        .part_psend_init = mca_part_onesided_psend_init,
        .part_psend_init = mca_part_onesided_psend_init,
        .part_start = mca_part_onesided_start,
        .part_pready = mca_part_onesided_pready,
        .part_parrived = mca_part_onesided_parrived,
    }
};

/* ID Lookup Table implementation */
static opal_mutex_t id_table_lock = OPAL_MUTEX_INITIALIZER;
static opal_list_t id_table = OPAL_LIST_INITIALIZER;

typedef struct {
    opal_list_item_t super;
    uint64_t id;
    mca_part_onesided_request_t *req;
} id_lookup_entry_t;

OBJ_CLASS_DECLARATION(id_lookup_entry_t);
static void id_lookup_entry_construct(id_lookup_entry_t *item) {}
static void id_lookup_entry_destruct(id_lookup_entry_t *item) {}
OBJ_CLASS_INSTANCE(id_lookup_entry_t, opal_list_item_t,
                   id_lookup_entry_construct, id_lookup_entry_destruct);

static uint64_t next_req_id = 1;

static uint64_t generate_req_id(mca_part_onesided_request_t *req) {
    OPAL_THREAD_LOCK(&id_table_lock);
    uint64_t id = next_req_id++;
    id_lookup_entry_t *entry = OBJ_NEW(id_lookup_entry_t);
    entry->id = id;
    entry->req = req;
    opal_list_append(&id_table, (opal_list_item_t*)entry);
    OPAL_THREAD_UNLOCK(&id_table_lock);
    return id;
}

static mca_part_onesided_request_t* lookup_req_id(uint64_t id) {
    OPAL_THREAD_LOCK(&id_table_lock);
    id_lookup_entry_t *current;
    mca_part_onesided_request_t *req = NULL;
    OPAL_LIST_FOREACH(current, &id_table, id_lookup_entry_t) {
        if (current->id == id) {
            req = current->req;
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&id_table_lock);
    return req;
}

/* Update init functions to assign IDs */
void mca_part_onesided_assign_id(mca_part_onesided_request_t *req) {
    req->req_id = generate_req_id(req);
}
/* Prototype for the assign_id function since it's called in init functions */
/* In a real patch we'd move this to the top or a header, but for minimal we just ensure it's defined. */

/* Helper to update receiver completion status */
void mca_part_onesided_mark_partition_complete(mca_part_onesided_request_t *req, size_t part_idx) {
    OPAL_THREAD_LOCK(&req->lock);
    
    // Only mark complete if not already marked to avoid double counting
    if (!(req->completed_bitmap[part_idx / 8] & (1 << (part_idx % 8)))) {
        req->completed_bitmap[part_idx / 8] |= (1 << (part_idx % 8));
        req->completed_count++;
        
        if (ompi_part_onesided.verbose) {
            opal_output(opal_verbose, 0, "onesided part: partition %zu marked complete for req %lu\n", 
                        part_idx, req->req_id);
        }
        
        if (req->completed_count == req->req_parts) {
            req->req_ompi.req_state = OMPI_REQUEST_COMPLETE;
            ompi_request_complete(&(req->req_ompi), true);
        mca_part_onesided_remove_active_request(req);
        }
    }
    
    OPAL_THREAD_UNLOCK(&req->lock);
}

/* Update put completion to send the partition index in the payload */
static void mca_part_onesided_put_completion_updated_updated(struct mca_btl_base_module_t *btl, 
                                           struct mca_btl_base_endpoint_t *endpoint,
                                           void *local_address, 
                                           struct mca_btl_base_registration_handle_t *local_handle, 
                                           void *context, void *cbdata, int status) {
    mca_part_onesided_request_t *req = (mca_part_onesided_request_t *)context;
    
    uintptr_t diff = (uintptr_t)local_address - (uintptr_t)req->req_buf;
    size_t part_idx = diff / req->req_part_bytes;
    
    OPAL_THREAD_LOCK(&req->lock);
    req->completed_bitmap[part_idx / 8] |= (1 << (part_idx % 8));
    req->completed_count++;
    
    // Send PART_COMPLETE with partition index as payload
    part_onesided_control_hdr_t hdr = {
        .version = PART_ONESIDED_CTRL_VERSION,
        .msg_type = PART_ONESIDED_CTRL_PART_COMPLETE,
        .request_id = req->req_id
    };
    
    mca_part_onesided_send_ctrl(req->btl_endpoint, btl, hdr.msg_type, req->req_id, &part_idx, sizeof(size_t));
    
    if (req->completed_count == req->req_parts) {
        req->req_ompi.req_state = OMPI_REQUEST_COMPLETE;
        ompi_request_complete(&(req->req_ompi), true);
        mca_part_onesided_remove_active_request(req);
    }
    OPAL_THREAD_UNLOCK(&req->lock);
}

/* Global list of active requests for progress engine */
static opal_list_t active_requests = OPAL_LIST_INITIALIZER;
static opal_mutex_t active_requests_lock = OPAL_MUTEX_INITIALIZER;

void mca_part_onesided_add_active_request(mca_part_onesided_request_t *req) {
    OPAL_THREAD_LOCK(&active_requests_lock);
    opal_list_append(&active_requests, (opal_list_item_t*)req);
    OPAL_THREAD_UNLOCK(&active_requests_lock);
}

void mca_part_onesided_remove_active_request(mca_part_onesided_request_t *req) {
    OPAL_THREAD_LOCK(&active_requests_lock);
    opal_list_remove_item(&active_requests, (opal_list_item_t*)req);
    OPAL_THREAD_UNLOCK(&active_requests_lock);
}

/* Handler for EPOCH_OPEN on the sender side */
void mca_part_onesided_handle_epoch_open_sender(uint64_t req_id) {
    mca_part_onesided_request_t *req = lookup_req_id(req_id);
    if (!req) return;
    OPAL_THREAD_LOCK(&req->lock);
    req->epoch_open_recv = true;
    OPAL_THREAD_UNLOCK(&req->lock);
}
