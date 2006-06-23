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
#include "opal/prefetch.h"
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
        if(OPAL_UNLIKELY(OPAL_THREAD_ADD32(&ud_btl->sd_wqe_hp, -1) < 0)) {
            OPAL_THREAD_ADD32(&ud_btl->sd_wqe_hp, 1);
            opal_list_append(&ud_btl->pending_frags_hp,
                    (opal_list_item_t*)frag);
            return OMPI_SUCCESS;
        }

        ib_qp = ud_btl->qp_hp;
        frag->wr_desc.sr_desc.wr.ud.ah = endpoint->rmt_ah_hp;
        frag->wr_desc.sr_desc.wr.ud.remote_qpn =
                endpoint->rem_addr.qp_num_hp;

        if(frag->sg_entry.length <= ud_btl->ib_inline_max) {
            frag->wr_desc.sr_desc.send_flags =
                IBV_SEND_SIGNALED|IBV_SEND_INLINE;
        }
    } else {
        if(OPAL_UNLIKELY(OPAL_THREAD_ADD32(&ud_btl->sd_wqe_lp, -1) < 0)) {
            OPAL_THREAD_ADD32(&ud_btl->sd_wqe_lp, 1);
            opal_list_append(&ud_btl->pending_frags_lp,
                    (opal_list_item_t*)frag);
            return OMPI_SUCCESS;
        }

        ib_qp = ud_btl->qp_lp;
        frag->wr_desc.sr_desc.wr.ud.ah = endpoint->rmt_ah_lp;
        frag->wr_desc.sr_desc.wr.ud.remote_qpn =
                endpoint->rem_addr.qp_num_lp;
    }

    /*OPAL_OUTPUT((0, "Send to LID %d QP %d, len: %d %d %d, frag: %p",
                  endpoint->rem_addr.lid,
                  frag->wr_desc.sr_desc.wr.ud.remote_qpn,
                  frag->sg_entry.length, frag->segment.seg_len,
                  ud_btl->ib_inline_max, frag));*/

#if MCA_BTL_UD_ENABLE_PROFILE
    frag->tm = opal_sys_timer_get_cycles();
#endif

    MCA_BTL_UD_START_TIME(ibv_post_send);
    if(OPAL_UNLIKELY(ibv_post_send(ib_qp, &frag->wr_desc.sr_desc, &bad_wr))) {
        BTL_ERROR(("error posting send request errno says %d %s\n",
                    errno, strerror(errno)));
        return OMPI_ERROR;
    }
    MCA_BTL_UD_END_TIME(ibv_post_send);

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
    /*OBJ_CONSTRUCT(&endpoint->endpoint_lock, opal_mutex_t);*/

    memset(&endpoint->rem_addr, 0, sizeof(struct mca_btl_ud_addr_t));
}


/*
 * Destroy a endpoint
 *
 */

static void mca_btl_ud_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
}


/*
 * Create the queue pair note that this is just the initial
 *  queue pair creation and we need to get the remote queue pair
 *  info from the peer before the qp is usable,
 */

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

