/*
 * Copyright (c) 2026 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/part/onesided/part_onesided_control.h"
#include "ompi/mca/part/onesided/part_onesided.h"
#include "ompi/mca/part/onesided/part_onesided_request.h"
#include "ompi/mca/bml/base/base.h"
#include "opal/mca/btl/btl.h"
#include "opal/mutex.h"

#include "ompi/mca/part/onesided/part_onesided_match.h"
/* Structure to track pending control messages that failed due to resource exhaustion */
typedef struct {
    opal_list_item_t super;
    part_onesided_control_hdr_t hdr;
    void *payload;
    size_t payload_size;
    struct mca_btl_base_endpoint_t *endpoint;
    mca_btl_base_module_t *btl;
    int retries;
} part_onesided_pending_ctrl_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(part_onesided_pending_ctrl_t);
static void part_onesided_pending_ctrl_construct(part_onesided_pending_ctrl_t *item) {
    item->retries = 0;
}
static void part_onesided_pending_ctrl_destruct(part_onesided_pending_ctrl_t *item) {
    if (item->payload) free(item->payload);
}
OBJ_CLASS_INSTANCE(part_onesided_pending_ctrl_t, opal_list_item_t,
                   part_onesided_pending_ctrl_construct, part_onesided_pending_ctrl_destruct);

static opal_list_t pending_ctrl_list = OPAL_LIST_INITIALIZER;
static opal_mutex_t pending_ctrl_lock = OPAL_MUTEX_INITIALIZER;

/**
 * Internal function to send a control message using the BTL.
 */
static int part_onesided_btl_send_ctrl(mca_btl_base_module_t *btl, 
                                     struct mca_btl_base_endpoint_t *endpoint,
                                     part_onesided_control_hdr_t *hdr, 
                                     void *payload, size_t payload_size) {
    size_t total_size = sizeof(part_onesided_control_hdr_t) + payload_size;
    void *buf = malloc(total_size);
    if (!buf) return OPAL_ERR_OUT_OF_RESOURCE;
    
    memcpy(buf, hdr, sizeof(part_onesided_control_hdr_t));
    if (payload && payload_size > 0) {
        memcpy((char*)buf + sizeof(part_onesided_control_hdr_t), payload, payload_size);
    }

    int ret = btl->btl_send(btl, endpoint, NULL, buf, 0, total_size, 0, 0, 0);
    
    if (ret != OPAL_SUCCESS) {
        free(buf);
        return ret;
    }
    
    return OPAL_SUCCESS;
}

/* Public API for nonblocking send of control messages */
int mca_part_onesided_send_ctrl(struct mca_btl_base_endpoint_t *endpoint,
                               mca_btl_base_module_t *btl,
                               part_onesided_ctrl_type_t type,
                               uint64_t req_id,
                               void *payload, size_t payload_size) {
    part_onesided_control_hdr_t hdr = {
        .version = PART_ONESIDED_CTRL_VERSION,
        .msg_type = type,
        .request_id = req_id
    };

    int ret = part_onesided_btl_send_ctrl(btl, endpoint, &hdr, payload, payload_size);
    
    if (ret == OPAL_ERR_RESOURCE_BUSY) {
        if (ompi_part_onesided.verbose) {
            opal_output(opal_verbose, 0, "onesided part: BTL busy, queuing control message type %d\n", type);
        }
        
        part_onesided_pending_ctrl_t *pending = OBJ_NEW(part_onesided_pending_ctrl_t);
        pending->hdr = hdr;
        pending->endpoint = endpoint;
        pending->btl = btl;
        pending->payload_size = payload_size;
        if (payload && payload_size > 0) {
            pending->payload = malloc(payload_size);
            memcpy(pending->payload, payload, payload_size);
        }
        
        OPAL_THREAD_LOCK(&pending_ctrl_lock);
        opal_list_append(&pending_ctrl_list, (opal_list_item_t*)pending);
        OPAL_THREAD_UNLOCK(&pending_ctrl_lock);
        
        return OPAL_SUCCESS;
    }
    
    return ret;
}

/* Dispatcher for received control messages */
void mca_part_onesided_recv_ctrl(void *payload, size_t payload_size, 
                                 struct mca_btl_base_endpoint_t *endpoint) {
    if (payload_size < sizeof(part_onesided_control_hdr_t)) {
        return;
    }
    
    part_onesided_control_hdr_t *hdr = (part_onesided_control_hdr_t *)payload;
    
    if (hdr->version != PART_ONESIDED_CTRL_VERSION) {
        opal_output(opal_verbose, 0, "onesided part: received control message with incompatible version %u\n", hdr->version);
        return;
    }

    if (ompi_part_onesided.verbose) {
        opal_output(opal_verbose, 0, "onesided part: received control message type %u for req %lu\n", 
                    hdr->msg_type, hdr->request_id);
    }

    switch (hdr->msg_type) {
        case PART_ONESIDED_CTRL_INIT_SEND: {
                    mca_part_onesided_request_t *req = lookup_req_id(hdr->request_id);
                    if (req) {
                        mca_part_onesided_match_handle_init_msg(req, req->req_peer, req->req_tag, 0, hdr->request_id, NULL, 0);
                    }
                    break;
}
            /* Handler for INIT_SEND */
            break;
        case PART_ONESIDED_CTRL_INIT_RECV: {
                    mca_part_onesided_request_t *req = lookup_req_id(hdr->request_id);
                    if (req) {
                        mca_part_onesided_match_handle_init_msg(req, req->req_peer, req->req_tag, 0, hdr->request_id, NULL, 0);
                    }
                    break;
}
            /* Handler for INIT_RECV */
            break;
        case PART_ONESIDED_CTRL_MATCH_ACK:
            /* Handler for MATCH_ACK */
            break;
        case PART_ONESIDED_CTRL_EPOCH_OPEN: {
                    mca_part_onesided_handle_epoch_open_sender(hdr->request_id);

            mca_part_onesided_request_t *req = lookup_req_id(hdr->request_id);
            if (!req) break;
            
            part_onesided_epoch_open_t *epoch_msg = (part_onesided_epoch_open_t *)((char*)payload + sizeof(part_onesided_control_hdr_t));
            
            OPAL_THREAD_LOCK(&req->lock);
            if (req->epoch != epoch_msg->epoch) {
                if (ompi_part_onesided.verbose) {
                    opal_output(opal_verbose, 0, "onesided part: received stale EPOCH_OPEN (%lu vs current %lu)\n", 
                                epoch_msg->epoch, req->epoch);
                }
            } else {
                req->epoch_open_recv = true;
                if (ompi_part_onesided.verbose) {
                    opal_output(opal_verbose, 0, "onesided part: EPOCH_OPEN received and matched for epoch %lu\n", req->epoch);
                }
            }
            OPAL_THREAD_UNLOCK(&req->lock);
            break;
        }
        case PART_ONESIDED_CTRL_PART_COMPLETE: {
                    mca_part_onesided_request_t *req = lookup_req_id(hdr->request_id);
                    if (req) {
                        size_t part_idx = 0;
                        if (payload_size >= sizeof(part_onesided_control_hdr_t) + sizeof(size_t)) {
                            memcpy(case PART_ONESIDED_CTRL_PART_COMPLETE:part_idx, (char*)payload + sizeof(part_onesided_control_hdr_t), sizeof(size_t));
                        }
                        mca_part_onesided_mark_partition_complete(req, part_idx);
                    }
                    break;
}
            /* Handler for PART_COMPLETE */
            break;
        case PART_ONESIDED_CTRL_ERROR:
            /* Handler for ERROR */
            break;
        default:
            opal_output(opal_verbose, 0, "onesided part: received unknown control message type %u\n", hdr->msg_type);
            break;
    }
}

/* Progress function to drain pending control messages */
int mca_part_onesided_ctrl_progress(void) {
    part_onesided_pending_ctrl_t *current;
    int ret = OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&pending_ctrl_lock);
    
    OPAL_LIST_FOREACH(current, &pending_ctrl_list, part_onesided_pending_ctrl_t) {
        int send_ret = part_onesided_btl_send_ctrl(current->btl, current->endpoint, 
                                               &current->hdr, current->payload, current->payload_size);
        
        if (send_ret == OPAL_SUCCESS) {
            opal_list_remove_item(&pending_ctrl_list, (opal_list_item_t*)current);
            OBJ_RELEASE(current);
        } else if (send_ret != OPAL_ERR_RESOURCE_BUSY) {
            opal_list_remove_item(&pending_ctrl_list, (opal_list_item_t*)current);
            OBJ_RELEASE(current);
        } else {
            current->retries++;
            if (current->retries > 100) {
                opal_output(opal_verbose, 0, "onesided part: control message failed after 100 retries, dropping\n");
                opal_list_remove_item(&pending_ctrl_list, (opal_list_item_t*)current);
                OBJ_RELEASE(current);
            }
        }
    }
    
    OPAL_THREAD_UNLOCK(&pending_ctrl_lock);
    return ret;
}
