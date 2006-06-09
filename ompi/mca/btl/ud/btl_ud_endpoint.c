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
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include <sys/time.h>
#include <time.h>
#include "ompi/types.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss.h"
#include "btl_ud.h"
#include "btl_ud_endpoint.h"
#include "btl_ud_proc.h"
#include "btl_ud_frag.h"
#include "ompi/class/ompi_free_list.h"
#include <errno.h>
#include <string.h>


static void mca_btl_ud_endpoint_construct(mca_btl_base_endpoint_t* endpoint);
static void mca_btl_ud_endpoint_destruct(mca_btl_base_endpoint_t* endpoint);


/*
 * post a send to the work queue
 */
inline int mca_btl_ud_endpoint_post_send(mca_btl_ud_module_t* ud_btl,
                                         mca_btl_ud_endpoint_t * endpoint,
                                         mca_btl_ud_frag_t * frag)
{
    struct ibv_qp* ib_qp;
    struct ibv_send_wr* bad_wr;

    /* Have to be careful here - UD adds a 40 byte header, but it is not
       included on the sending side. */
    frag->sg_entry.length = frag->segment.seg_len + sizeof(mca_btl_ud_header_t);
    frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED;

    if(frag->size == ud_btl->super.btl_eager_limit) {
        if(OPAL_THREAD_ADD32(&ud_btl->sd_wqe_hp, -1) < 0) {
            OPAL_THREAD_ADD32(&ud_btl->sd_wqe_hp, 1);
            opal_list_append(&ud_btl->pending_frags_hp,
                    (opal_list_item_t*)frag);
            return OMPI_SUCCESS;
        }

        ib_qp = ud_btl->qp_hp;
        frag->wr_desc.sr_desc.wr.ud.ah = endpoint->rmt_ah_hp;
        frag->wr_desc.sr_desc.wr.ud.remote_qpn =
                endpoint->rem_info.rem_qp_num_hp;

        if(frag->sg_entry.length <= ud_btl->ib_inline_max) {
            frag->wr_desc.sr_desc.send_flags =
                IBV_SEND_SIGNALED|IBV_SEND_INLINE;
        }
    } else {
        if(OPAL_THREAD_ADD32(&ud_btl->sd_wqe_lp, -1) < 0) {
            OPAL_THREAD_ADD32(&ud_btl->sd_wqe_lp, 1);
            opal_list_append(&ud_btl->pending_frags_lp,
                    (opal_list_item_t*)frag);
            return OMPI_SUCCESS;
        }

        ib_qp = ud_btl->qp_lp;
        frag->wr_desc.sr_desc.wr.ud.ah = endpoint->rmt_ah_lp;
        frag->wr_desc.sr_desc.wr.ud.remote_qpn =
                endpoint->rem_info.rem_qp_num_lp;
    }

    /*BTL_VERBOSE(("Send to : %d, len : %d %d %d, frag : %p",
                  endpoint->endpoint_proc->proc_guid.vpid,
                  frag->sg_entry.length, frag->segment.seg_len,
                  ud_btl->ib_inline_max, frag)); */

#if MCA_BTL_UD_ENABLE_PROFILE
    frag->tm = opal_sys_timer_get_cycles();
#endif

    MCA_BTL_UD_START_TIME(ibv_post_send);
    if(ibv_post_send(ib_qp, &frag->wr_desc.sr_desc, &bad_wr)) {
        BTL_ERROR(("error posting send request errno says %d %s\n",
                    errno, strerror(errno)));
        return OMPI_ERROR;
    }
    MCA_BTL_UD_END_TIME(ibv_post_send);

#if 0
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    if(mca_btl_ud_component.use_srq) {
        MCA_BTL_UD_POST_SRR_HIGH(ud_btl, 1);
        MCA_BTL_UD_POST_SRR_LOW(ud_btl, 1);
    } else {
#endif
        MCA_BTL_UD_ENDPOINT_POST_RR_HIGH(ud_btl, 1);
        MCA_BTL_UD_ENDPOINT_POST_RR_LOW(ud_btl, 1);
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    }
#endif
#endif
    return OMPI_SUCCESS;
}



OBJ_CLASS_INSTANCE(mca_btl_ud_endpoint_t,
                   opal_list_item_t, mca_btl_ud_endpoint_construct,
                   mca_btl_ud_endpoint_destruct);

/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_btl_ud_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
{
    endpoint->endpoint_btl = 0;
    endpoint->endpoint_proc = 0;
    endpoint->endpoint_state = MCA_BTL_IB_CLOSED;
    OBJ_CONSTRUCT(&endpoint->endpoint_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&endpoint->pending_send_frags, opal_list_t);

    memset(&endpoint->rem_info, 0, sizeof(struct mca_btl_ud_rem_info_t));
}

/*
 * Destroy a endpoint
 *
 */

static void mca_btl_ud_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
}

/*
 * Send connection information to remote endpoint using OOB
 *
 */

static void mca_btl_ud_endpoint_send_cb(int status, orte_process_name_t* endpoint,
        orte_buffer_t* buffer, orte_rml_tag_t tag, void* cbdata)
{
    OBJ_RELEASE(buffer);
}


static int mca_btl_ud_endpoint_send_connect_data(mca_btl_ud_endpoint_t* endpoint)
{
    mca_btl_ud_module_t* ud_btl = endpoint->endpoint_btl;
    orte_buffer_t* buffer = OBJ_NEW(orte_buffer_t);
    int rc;

    if(NULL == buffer) {
         ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
         return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* pack the info in the send buffer */
    rc = orte_dss.pack(buffer, &ud_btl->qp_hp->qp_num, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dss.pack(buffer, &ud_btl->qp_lp->qp_num, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dss.pack(buffer, &ud_btl->psn_hp, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dss.pack(buffer, &ud_btl->psn_lp, 1, ORTE_UINT32);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dss.pack(buffer, &ud_btl->ib_port_attr->lid, 1, ORTE_UINT16);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_dss.pack(buffer,
            &((mca_btl_ud_endpoint_t*)endpoint)->subnet, 1, ORTE_UINT16);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* send to endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid,
            buffer, ORTE_RML_TAG_DYNAMIC-1, 0, mca_btl_ud_endpoint_send_cb, NULL);

    BTL_VERBOSE(("Sending High Priority QP num = %d, Low Priority QP num = %d, LID = %d",
              ud_btl->qp_hp->qp_num,
              ud_btl->qp_lp->qp_num,
              endpoint->endpoint_btl->ib_port_attr->lid));

    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return OMPI_SUCCESS;
}


/*
 * Non blocking OOB recv callback.
 * Read incoming QP and other info, and if this endpoint
 * is trying to connect, reply with our QP info,
 * otherwise try to modify QP's and establish
 * reliable connection
 *
 */

static void mca_btl_ud_endpoint_recv(
    int status,
    orte_process_name_t* endpoint,
    orte_buffer_t* buffer,
    orte_rml_tag_t tag,
    void* cbdata)
{
    struct ibv_ah_attr ah_attr;
    mca_btl_ud_proc_t *ib_proc;
    mca_btl_ud_endpoint_t *ib_endpoint = NULL;
    mca_btl_ud_rem_info_t rem_info;
    mca_btl_ud_module_t* ud_btl;
    int rc;
    uint32_t i;
    size_t cnt = 1;

    /* start by unpacking data first so we know who is knocking at
       our door */

    rc = orte_dss.unpack(buffer, &rem_info.rem_qp_num_hp, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    rc = orte_dss.unpack(buffer, &rem_info.rem_qp_num_lp, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    rc = orte_dss.unpack(buffer, &rem_info.rem_psn_hp, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    rc = orte_dss.unpack(buffer, &rem_info.rem_psn_lp, &cnt, ORTE_UINT32);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    rc = orte_dss.unpack(buffer, &rem_info.rem_lid, &cnt, ORTE_UINT16);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    rc = orte_dss.unpack(buffer, &rem_info.rem_subnet, &cnt, ORTE_UINT16);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /*BTL_VERBOSE(("Received High Priority QP num = %d, Low Priority QP num %d,  LID = %d",
                 rem_info.rem_qp_num_hp,
                 rem_info.rem_qp_num_lp,
                 rem_info.rem_lid));*/

    for(ib_proc = (mca_btl_ud_proc_t*)
            opal_list_get_first(&mca_btl_ud_component.ib_procs);
            ib_proc != (mca_btl_ud_proc_t*)
            opal_list_get_end(&mca_btl_ud_component.ib_procs);
            ib_proc  = (mca_btl_ud_proc_t*)opal_list_get_next(ib_proc)) {

        if(orte_ns.compare(ORTE_NS_CMP_ALL,
                    &ib_proc->proc_guid, endpoint) == 0) {
            bool found = false;

            /* Try to get the endpoint instance of this proc */
            for(i = 0; i < ib_proc->proc_endpoint_count; i++) {
                ib_endpoint = ib_proc->proc_endpoints[i];
                if(ib_endpoint->rem_info.rem_lid &&
                   ib_endpoint->rem_info.rem_lid  == rem_info.rem_lid) {
                    /* we've seen them before! */
                    found = true;
                    break;
                }
            }
            /* If we haven't seen this remote lid before then try to match on
               endpoint */
            for(i = 0; !found && i < ib_proc->proc_endpoint_count; i++) {
                ib_endpoint = ib_proc->proc_endpoints[i];
                if(!ib_endpoint->rem_info.rem_lid &&
                   ib_endpoint->subnet == rem_info.rem_subnet) {
                    /* found a match based on subnet! */
                    found = true;
                    break;
                }
            }
            /* try finding an open port, even if subnets don't match */
            for(i = 0; !found && i < ib_proc->proc_endpoint_count; i++) {
                ib_endpoint = ib_proc->proc_endpoints[i];
                if(!ib_endpoint->rem_info.rem_lid) {
                    /* found an unused end-point */
                    found = true;
                    break;
                }
            }

            if(!found) {
                BTL_ERROR(("can't find suitable endpoint for this peer\n"));
                return;
            }

            OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock);

            /* Update status */
            if(ib_endpoint->endpoint_state == MCA_BTL_IB_CLOSED) {
                if(OMPI_SUCCESS !=
                        mca_btl_ud_endpoint_send_connect_data(ib_endpoint)) {
                    BTL_ERROR(("error sending connect request, error code %d", rc));
                    ib_endpoint->endpoint_state = MCA_BTL_IB_FAILED;
                    OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
                    return;
                }
            }

            /* Always 'CONNECTED' at this point */
            ud_btl = ib_endpoint->endpoint_btl;
            memcpy(&ib_endpoint->rem_info,
                    &rem_info, sizeof(mca_btl_ud_rem_info_t));

            ah_attr.is_global = 0;
            ah_attr.dlid = rem_info.rem_lid;
            ah_attr.sl = mca_btl_ud_component.ib_service_level;
            ah_attr.src_path_bits = mca_btl_ud_component.ib_src_path_bits;
            ah_attr.port_num = ud_btl->port_num;

            ib_endpoint->rmt_ah_hp = ibv_create_ah(ud_btl->ib_pd, &ah_attr);
            if(NULL == ib_endpoint->rmt_ah_hp) {
                BTL_ERROR(("error creating address handle errno says %s\n",
                            strerror(errno)));
                ib_endpoint->endpoint_state = MCA_BTL_IB_FAILED;
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
                return;
            }

            ib_endpoint->rmt_ah_lp = ibv_create_ah(ud_btl->ib_pd, &ah_attr);
            if(NULL == ib_endpoint) {
                BTL_ERROR(("error creating address handle errno says %s\n",
                            strerror(errno)));
                ib_endpoint->endpoint_state = MCA_BTL_IB_FAILED;
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
                return;
            }

            ib_endpoint->endpoint_state = MCA_BTL_IB_CONNECTED;

            /*BTL_VERBOSE(("connected! QP num = %d, Low Priority QP num %d,  LID = %d",
                 ib_endpoint->rem_info.rem_qp_num_hp,
                 ib_endpoint->rem_info.rem_qp_num_lp,
                 ib_endpoint->rem_info.rem_lid));*/

            /* Post our queued sends */
            while(!opal_list_is_empty(&(ib_endpoint->pending_send_frags))) {
                mca_btl_ud_frag_t* frag = (mca_btl_ud_frag_t*)
                    opal_list_remove_first(&(ib_endpoint->pending_send_frags));
                if(OMPI_SUCCESS != mca_btl_ud_endpoint_post_send(
                            ud_btl, ib_endpoint, frag)) {
                    BTL_ERROR(("ERROR posting send"));
                    ib_endpoint->endpoint_state = MCA_BTL_IB_FAILED;
                    break;
                }
            }

            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;
        }
    }
}

/*
 *  Post the OOB recv (for receiving the peers information)
 */
void mca_btl_ud_post_recv()
{
    orte_rml.recv_buffer_nb(
        ORTE_RML_NAME_ANY,
        ORTE_RML_TAG_DYNAMIC-1,
        ORTE_RML_PERSISTENT,
        mca_btl_ud_endpoint_recv,
        NULL);
}

/*
 * Attempt to send a fragment using a given endpoint. If the endpoint is not
 * connected, queue the fragment and start the connection as required.
 */

int mca_btl_ud_endpoint_send(mca_btl_base_endpoint_t* endpoint,
                             mca_btl_ud_frag_t* frag)
{
    int rc = OMPI_SUCCESS;
    bool call_progress = false;

    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
    switch(endpoint->endpoint_state) {
    case MCA_BTL_IB_CONNECTED:
    {
        MCA_BTL_UD_START_TIME(endpoint_send_conn);
        rc = mca_btl_ud_endpoint_post_send(
                endpoint->endpoint_btl, endpoint, frag);
        MCA_BTL_UD_END_TIME(endpoint_send_conn);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        return rc;
    }
    case MCA_BTL_IB_CLOSED:
        /* Send connection info over to remote endpoint */
        endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
        rc = mca_btl_ud_endpoint_send_connect_data(endpoint);
        if(OMPI_SUCCESS != rc) {
            BTL_ERROR(("error sending connect request, error code %d", rc));
            endpoint->endpoint_state = MCA_BTL_IB_FAILED;
            return rc;
        }

        /**
         * As long as we expect a message from the peer (in order to setup
         * the connection) let the event engine pool the OOB events. Note:
         * we increment it once per active connection.
         */
        opal_progress_event_increment();
        call_progress = true;

        /* No break here - fall through */
    case MCA_BTL_IB_CONNECTING:
        opal_list_append(&endpoint->pending_send_frags,
                (opal_list_item_t *)frag);
        break;
    case MCA_BTL_IB_FAILED:
        BTL_ERROR(("endpoint FAILED"));
    default:
        rc = OMPI_ERR_UNREACH;
    }
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
    if(call_progress) opal_progress();
    return rc;
}


/*
 * Create the queue pair note that this is just the initial
 *  queue pair creation and we need to get the remote queue pair
 *  info from the peer before the qp is usable,
 */
/* TODO - maybe start to push this off into its own file? */

int mca_btl_ud_endpoint_init_qp(
                                      mca_btl_base_module_t* btl,
                                      struct ibv_cq* cq,
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                                      struct ibv_srq* srq,
#endif
                                      struct ibv_qp** qp,
                                      uint32_t lcl_psn
                                      )
{
    mca_btl_ud_module_t* ud_btl = (mca_btl_ud_module_t*)btl;
    struct ibv_qp* my_qp;
    struct ibv_qp_attr qp_attr;
    struct ibv_qp_init_attr qp_init_attr;

    memset(&qp_init_attr, 0, sizeof(struct ibv_qp_init_attr));

    qp_init_attr.send_cq = cq;
    qp_init_attr.recv_cq = cq;
    qp_init_attr.cap.max_send_wr = mca_btl_ud_component.rd_num;
    qp_init_attr.cap.max_recv_wr = mca_btl_ud_component.rd_num;
    qp_init_attr.cap.max_send_sge = mca_btl_ud_component.ib_sg_list_size;
    qp_init_attr.cap.max_recv_sge = mca_btl_ud_component.ib_sg_list_size;
    qp_init_attr.qp_type = IBV_QPT_UD;
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    if(mca_btl_ud_component.use_srq) {
        qp_init_attr.srq = srq;
    }
#endif
    my_qp = ibv_create_qp(ud_btl->ib_pd, &qp_init_attr);

    if(NULL == my_qp) {
        BTL_ERROR(("error creating qp errno says %s", strerror(errno)));
        return OMPI_ERROR;
    }

    (*qp) = my_qp;
    if(0 == (ud_btl->ib_inline_max = qp_init_attr.cap.max_inline_data)) {
        BTL_ERROR(("ibv_create_qp: returned 0 byte(s) for max inline data"));
    }

    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = mca_btl_ud_component.ib_pkey_ix;
    qp_attr.qkey = mca_btl_ud_component.ib_qkey;
    qp_attr.port_num = ud_btl->port_num;

    if(ibv_modify_qp(*qp, &qp_attr,
                     IBV_QP_STATE |
                     IBV_QP_PKEY_INDEX |
                     IBV_QP_PORT |
                     IBV_QP_QKEY)) {
        BTL_ERROR(("error modifying qp to INIT errno says %s", strerror(errno)));
        return OMPI_ERROR;
    }

    qp_attr.qp_state = IBV_QPS_RTR;
    if(ibv_modify_qp(*qp, &qp_attr, IBV_QP_STATE)) {
        BTL_ERROR(("error modifing QP to RTR errno says %s",  strerror(errno)));
        return OMPI_ERROR;
    }

    qp_attr.qp_state    = IBV_QPS_RTS;
    qp_attr.sq_psn 	    = lcl_psn;
    if (ibv_modify_qp(*qp, &qp_attr, IBV_QP_STATE | IBV_QP_SQ_PSN)) {
        BTL_ERROR(("error modifying QP to RTS errno says %s", strerror(errno)));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

