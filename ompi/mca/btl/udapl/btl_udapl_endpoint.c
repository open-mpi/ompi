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
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 *
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
#include "orte/mca/ns/base/base.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss.h"
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "btl_udapl.h"
#include "btl_udapl_endpoint.h" 
#include "btl_udapl_proc.h"
#include "btl_udapl_frag.h"


static void mca_btl_udapl_endpoint_send_cb(int status, orte_process_name_t* endpoint, 
        orte_buffer_t* buffer, orte_rml_tag_t tag, void* cbdata);
static int mca_btl_udapl_start_connect(mca_btl_base_endpoint_t* endpoint);
static int mca_btl_udapl_endpoint_post_recv(mca_btl_udapl_endpoint_t* endpoint,
                                            size_t size);
void mca_btl_udapl_endpoint_connect(mca_btl_udapl_endpoint_t* endpoint);
void mca_btl_udapl_endpoint_recv(int status, orte_process_name_t* endpoint, 
        orte_buffer_t* buffer, orte_rml_tag_t tag, void* cbdata);
static int mca_btl_udapl_endpoint_finish_eager(mca_btl_udapl_endpoint_t*);
static int mca_btl_udapl_endpoint_finish_max(mca_btl_udapl_endpoint_t*);


int mca_btl_udapl_endpoint_send(mca_btl_base_endpoint_t* endpoint,
                                mca_btl_udapl_frag_t* frag)
{
    int rc = OMPI_SUCCESS;
    DAT_DTO_COOKIE cookie;

    /* Fix up the segment length before we do anything with the frag */
    frag->triplet.segment_length =
            frag->segment.seg_len + sizeof(mca_btl_udapl_footer_t);

    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
    switch(endpoint->endpoint_state) {
        case MCA_BTL_UDAPL_CONNECTED:
            /* just send it already.. */
            cookie.as_ptr = frag;
            if(frag->size ==
                    mca_btl_udapl_component.udapl_eager_frag_size) {

                if(OPAL_THREAD_ADD32(&endpoint->endpoint_eager_sends, -1) < 0) {
                    OPAL_THREAD_ADD32(&endpoint->endpoint_eager_sends, 1);
                    opal_list_append(&endpoint->endpoint_eager_frags,
                            (opal_list_item_t*)frag);
                } else {
                    rc = dat_ep_post_send(endpoint->endpoint_eager, 1,
                            &frag->triplet, cookie,
                            DAT_COMPLETION_DEFAULT_FLAG);
                }
            } else {
                assert(frag->size ==
                        mca_btl_udapl_component.udapl_max_frag_size);
                if(OPAL_THREAD_ADD32(&endpoint->endpoint_max_sends, -1) < 0) {
                    OPAL_THREAD_ADD32(&endpoint->endpoint_max_sends, 1);
                    opal_list_append(&endpoint->endpoint_max_frags,
                            (opal_list_item_t*)frag);
                } else {
                    rc = dat_ep_post_send(endpoint->endpoint_max, 1,
                            &frag->triplet, cookie,
                            DAT_COMPLETION_DEFAULT_FLAG);
                }
            }

            if(DAT_SUCCESS != rc) {
                MCA_BTL_UDAPL_ERROR(rc, "dat_ep_post_send");
                rc = OMPI_ERROR;
            }

            break;
        case MCA_BTL_UDAPL_CLOSED:
            /* Initiate a new connection, add this send to a queue */
            rc = mca_btl_udapl_start_connect(endpoint);
            if(OMPI_SUCCESS != rc) {
                endpoint->endpoint_state = MCA_BTL_UDAPL_FAILED;
                break;
            }

            /* Fall through on purpose to queue the send */
        case MCA_BTL_UDAPL_CONN_EAGER:
        case MCA_BTL_UDAPL_CONN_MAX:
            /* Add this send to a queue */
            if(frag->size ==
                    mca_btl_udapl_component.udapl_eager_frag_size) {
                opal_list_append(&endpoint->endpoint_eager_frags,
                        (opal_list_item_t*)frag);
            } else {
                assert(frag->size ==
                        mca_btl_udapl_component.udapl_max_frag_size);
                OPAL_THREAD_ADD32(&endpoint->endpoint_max_sends, -1);
                opal_list_append(&endpoint->endpoint_max_frags,
                        (opal_list_item_t*)frag);
            }

            break;
        case MCA_BTL_UDAPL_FAILED:
            rc = OMPI_ERR_UNREACH;
            break;
    }
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);

    return rc;
}


static void mca_btl_udapl_endpoint_send_cb(int status, orte_process_name_t* endpoint, 
        orte_buffer_t* buffer, orte_rml_tag_t tag, void* cbdata)
{
    OBJ_RELEASE(buffer);
}


static int mca_btl_udapl_start_connect(mca_btl_base_endpoint_t* endpoint)
{
    mca_btl_udapl_addr_t* addr = &endpoint->endpoint_btl->udapl_addr;
    orte_buffer_t* buf = OBJ_NEW(orte_buffer_t);
    int rc;

    if(NULL == buf) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* Pack our address information */
    rc = orte_dss.pack(buf, &addr->port, 1, ORTE_UINT64);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    rc = orte_dss.pack(buf, &addr->addr, sizeof(DAT_SOCK_ADDR), ORTE_UINT8);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* Send the buffer */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid, buf,
            ORTE_RML_TAG_DYNAMIC - 1, 0, mca_btl_udapl_endpoint_send_cb, NULL);
    if(0 > rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    endpoint->endpoint_state = MCA_BTL_UDAPL_CONN_EAGER;
    return OMPI_SUCCESS;
}


void mca_btl_udapl_endpoint_recv(int status, orte_process_name_t* endpoint, 
        orte_buffer_t* buffer, orte_rml_tag_t tag, void* cbdata)
{
    mca_btl_udapl_addr_t addr;
    mca_btl_udapl_proc_t* proc;
    mca_btl_base_endpoint_t* ep;
    int32_t cnt = 1;
    size_t i;
    int rc;

    /* Unpack data */
    rc = orte_dss.unpack(buffer, &addr.port, &cnt, ORTE_UINT64);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    cnt = sizeof(mca_btl_udapl_addr_t);
    rc = orte_dss.unpack(buffer, &addr.addr, &cnt, ORTE_UINT8);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* Match the endpoint and handle it */
    OPAL_THREAD_LOCK(&mca_btl_udapl_component.udapl_lock);
    for(proc = (mca_btl_udapl_proc_t*)
                opal_list_get_first(&mca_btl_udapl_component.udapl_procs);
            proc != (mca_btl_udapl_proc_t*)
                opal_list_get_end(&mca_btl_udapl_component.udapl_procs);
            proc  = (mca_btl_udapl_proc_t*)opal_list_get_next(proc)) {

        if(ORTE_EQUAL == orte_ns.compare_fields(ORTE_NS_CMP_ALL, &proc->proc_guid, endpoint)) {
            for(i = 0; i < proc->proc_endpoint_count; i++) {
                ep = proc->proc_endpoints[i];

                /* Does this endpoint match? */
                if(!memcmp(&addr, &ep->endpoint_addr,
                        sizeof(mca_btl_udapl_addr_t))) {
                    OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);
                    mca_btl_udapl_endpoint_connect(ep);
                    return;
                }
            }
        }
    }
    OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);
}


/*
 * Set up OOB recv callback.
 */

void mca_btl_udapl_endpoint_post_oob_recv(void)
{
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DYNAMIC-1,
            ORTE_RML_PERSISTENT, mca_btl_udapl_endpoint_recv, NULL);
}


void mca_btl_udapl_endpoint_connect(mca_btl_udapl_endpoint_t* endpoint)
{
    mca_btl_udapl_module_t* btl = endpoint->endpoint_btl;
    int rc;

    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);

    /* Nasty test to prevent deadlock and unwanted connection attempts */
    /* This right here is the whole point of using the ORTE/RML handshake */
    if((MCA_BTL_UDAPL_CONN_EAGER == endpoint->endpoint_state &&
            0 > orte_ns.compare_fields(ORTE_NS_CMP_ALL,
                    &endpoint->endpoint_proc->proc_guid,
                    &ompi_proc_local()->proc_name)) ||
            (MCA_BTL_UDAPL_CLOSED != endpoint->endpoint_state &&
             MCA_BTL_UDAPL_CONN_EAGER != endpoint->endpoint_state)) {
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        return;
    }

    /* Create a new uDAPL endpoint and start the connection process */
    rc = dat_ep_create(btl->udapl_ia, btl->udapl_pz,
            btl->udapl_evd_dto, btl->udapl_evd_dto, btl->udapl_evd_conn,
            NULL, &endpoint->endpoint_eager);
    if(DAT_SUCCESS != rc) {
        MCA_BTL_UDAPL_ERROR(rc, "dat_ep_create (eager)");
        goto failure_create;
    }

    rc = dat_ep_connect(endpoint->endpoint_eager, &endpoint->endpoint_addr.addr,
            endpoint->endpoint_addr.port, mca_btl_udapl_component.udapl_timeout,
            0, NULL, 0, DAT_CONNECT_DEFAULT_FLAG);
    if(DAT_SUCCESS != rc) {
        MCA_BTL_UDAPL_ERROR(rc, "dat_ep_connect (eager)");
        goto failure;
    }

    endpoint->endpoint_state = MCA_BTL_UDAPL_CONN_EAGER;
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
    return;

failure:
    dat_ep_free(endpoint->endpoint_eager);
failure_create:
    endpoint->endpoint_eager = DAT_HANDLE_NULL;
    endpoint->endpoint_state = MCA_BTL_UDAPL_FAILED;
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
    return;
}


/*
 * Finish establishing a connection
 * Note that this routine expects that the mca_btl_udapl_component.udapl.lock
 * has been acquired by the callee.
 */

int mca_btl_udapl_endpoint_finish_connect(struct mca_btl_udapl_module_t* btl,
                                          mca_btl_udapl_addr_t* addr,
                                          int32_t* connection_seq,
                                          DAT_EP_HANDLE endpoint)
{
    mca_btl_udapl_proc_t* proc;
    mca_btl_base_endpoint_t* ep;
    size_t i;
    int rc;

    /* Search for the matching BTL EP */
    for(proc = (mca_btl_udapl_proc_t*)
                opal_list_get_first(&mca_btl_udapl_component.udapl_procs);
            proc != (mca_btl_udapl_proc_t*)
                opal_list_get_end(&mca_btl_udapl_component.udapl_procs);
            proc  = (mca_btl_udapl_proc_t*)opal_list_get_next(proc)) {
    
        for(i = 0; i < proc->proc_endpoint_count; i++) {
            ep = proc->proc_endpoints[i];

            /* Does this endpoint match? */
            /* TODO - Check that the DAT_CONN_QUAL's match too */
            if(ep->endpoint_btl == btl &&
                    !memcmp(addr, &ep->endpoint_addr, sizeof(DAT_SOCK_ADDR))) {
                OPAL_THREAD_LOCK(&ep->endpoint_lock);
                if(MCA_BTL_UDAPL_CONN_EAGER == ep->endpoint_state) {
                    ep->endpoint_connection_seq = *connection_seq;
                    ep->endpoint_eager = endpoint;
                    rc = mca_btl_udapl_endpoint_finish_eager(ep);
               } else if(MCA_BTL_UDAPL_CONN_MAX == ep->endpoint_state) {
                    /* Check to see order of messages received are in
                     * the same order the actual connections are made.
                     * If they are not we need to swap the eager and
                     * max connections. This inversion is possible due
                     * to a race condition that one process may actually
                     * receive the sendrecv messages from the max connection
                     * before the eager connection.
                     */
                    if (ep->endpoint_connection_seq < *connection_seq) {
                        /* normal order connection matching */
                        ep->endpoint_max = endpoint;
                    } else {
                        /* inverted order connection matching */
                        ep->endpoint_max = ep->endpoint_eager;
                        ep->endpoint_eager = endpoint;
                    }

                    rc = mca_btl_udapl_endpoint_finish_max(ep);
                } else {
                    OPAL_OUTPUT((0, "btl_udapl ERROR invalid EP state %d\n",
                            ep->endpoint_state));
                    return OMPI_ERROR;
                }
                return rc;
            }
        }
    }

    /* If this point is reached, no matching endpoint was found */
    OPAL_OUTPUT((0, "btl_udapl ERROR could not match endpoint\n"));
    return OMPI_ERROR;
}


/*
 * Finish setting up an eager connection, start a max connection
 */

static int mca_btl_udapl_endpoint_finish_eager(
        mca_btl_udapl_endpoint_t* endpoint)
{
    mca_btl_udapl_module_t* btl = endpoint->endpoint_btl;
    int rc;

    endpoint->endpoint_state = MCA_BTL_UDAPL_CONN_MAX;
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);

    /* Only one side does dat_ep_connect() */
    if(0 < orte_ns.compare_fields(ORTE_NS_CMP_ALL,
                &endpoint->endpoint_proc->proc_guid,
                &ompi_proc_local()->proc_name)) {
    
        rc = dat_ep_create(btl->udapl_ia, btl->udapl_pz,
                btl->udapl_evd_dto, btl->udapl_evd_dto, btl->udapl_evd_conn,
                NULL, &endpoint->endpoint_max);
        if(DAT_SUCCESS != rc) {
            MCA_BTL_UDAPL_ERROR(rc, "dat_ep_create (max)");
            return OMPI_ERROR;
        }

        rc = dat_ep_connect(endpoint->endpoint_max,
                &endpoint->endpoint_addr.addr, endpoint->endpoint_addr.port,
                mca_btl_udapl_component.udapl_timeout,
                0, NULL, 0, DAT_CONNECT_DEFAULT_FLAG);
        if(DAT_SUCCESS != rc) {
            MCA_BTL_UDAPL_ERROR(rc, "dat_ep_connect (max)");
            dat_ep_free(endpoint->endpoint_max);
            return OMPI_ERROR;
        }
    }
    
    return OMPI_SUCCESS;
}


static int mca_btl_udapl_endpoint_finish_max(mca_btl_udapl_endpoint_t* endpoint)
{
    mca_btl_udapl_frag_t* frag;
    DAT_DTO_COOKIE cookie;
    int ret = OMPI_SUCCESS;
    int rc;

    endpoint->endpoint_state = MCA_BTL_UDAPL_CONNECTED;

    /* post eager/max recv buffers */
    mca_btl_udapl_endpoint_post_recv(endpoint,
            mca_btl_udapl_component.udapl_eager_frag_size);
    mca_btl_udapl_endpoint_post_recv(endpoint,
            mca_btl_udapl_component.udapl_max_frag_size);

    
    /* post queued sends */
    assert(endpoint->endpoint_eager_sends ==
            mca_btl_udapl_component.udapl_num_sends);
    while(OPAL_THREAD_ADD32(&endpoint->endpoint_eager_sends, -1) >= 0 &&
            NULL != (frag = (mca_btl_udapl_frag_t*)
                opal_list_remove_first(&endpoint->endpoint_eager_frags))) {
        cookie.as_ptr = frag;
            
        assert(frag->triplet.virtual_address == 
	       (DAT_VADDR)frag->segment.seg_addr.pval);
        assert(frag->triplet.segment_length ==
                frag->segment.seg_len + sizeof(mca_btl_udapl_footer_t));
        assert(frag->size ==
                mca_btl_udapl_component.udapl_eager_frag_size);
        rc = dat_ep_post_send(endpoint->endpoint_eager, 1,
            &frag->triplet, cookie, DAT_COMPLETION_DEFAULT_FLAG);
        if(DAT_SUCCESS != rc) {
            MCA_BTL_UDAPL_ERROR(rc, "dat_ep_post_send (eager)");
            endpoint->endpoint_state = MCA_BTL_UDAPL_FAILED;
            ret = OMPI_ERROR;
            break;
        }
    }

    if(endpoint->endpoint_eager_sends < 0) {
        OPAL_THREAD_ADD32(&endpoint->endpoint_eager_sends, 1);
    }

    assert(endpoint->endpoint_max_sends ==
            mca_btl_udapl_component.udapl_num_sends);
    while(OPAL_THREAD_ADD32(&endpoint->endpoint_max_sends, -1) >= 0 &&
            NULL != (frag = (mca_btl_udapl_frag_t*)
                opal_list_remove_first(&endpoint->endpoint_max_frags))) {
        cookie.as_ptr = frag;
            
        assert(frag->triplet.virtual_address == (DAT_VADDR)frag->ftr);
        assert(frag->triplet.segment_length ==
                frag->segment.seg_len + sizeof(mca_btl_udapl_footer_t));
        assert(frag->size ==
                mca_btl_udapl_component.udapl_eager_frag_size);

        rc = dat_ep_post_send(endpoint->endpoint_max, 1,
            &frag->triplet, cookie, DAT_COMPLETION_DEFAULT_FLAG);
        if(DAT_SUCCESS != rc) {
            MCA_BTL_UDAPL_ERROR(rc, "dat_ep_post_send (max)");
            endpoint->endpoint_state = MCA_BTL_UDAPL_FAILED;
            ret = OMPI_ERROR;
            break;
        }
    }

    if(endpoint->endpoint_max_sends < 0) {
        OPAL_THREAD_ADD32(&endpoint->endpoint_max_sends, 1);
    }
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
    return ret;
}


/*
 * Post receive buffers for a newly established endpoint connection.
 */

static int mca_btl_udapl_endpoint_post_recv(mca_btl_udapl_endpoint_t* endpoint,
                                            size_t size)
{
    mca_btl_udapl_frag_t* frag = NULL;
    DAT_DTO_COOKIE cookie;
    DAT_EP_HANDLE ep;
    int rc;
    int i;

    for(i = 0; i < mca_btl_udapl_component.udapl_num_recvs; i++) {
        if(size == mca_btl_udapl_component.udapl_eager_frag_size) {
            MCA_BTL_UDAPL_FRAG_ALLOC_EAGER(endpoint->endpoint_btl, frag, rc);
            ep = endpoint->endpoint_eager;
        } else {
            assert(size == mca_btl_udapl_component.udapl_max_frag_size);
            MCA_BTL_UDAPL_FRAG_ALLOC_MAX(endpoint->endpoint_btl, frag, rc);
            ep = endpoint->endpoint_max;
        } 
    
        assert(size == frag->size);
        /* Set up the LMR triplet from the frag segment */
        /* Note that this triplet defines a sub-region of a registered LMR */
        frag->triplet.virtual_address = (DAT_VADDR)frag->segment.seg_addr.pval;
        frag->triplet.segment_length = frag->size;
    
        frag->btl = endpoint->endpoint_btl;
        frag->endpoint = endpoint;
        frag->base.des_dst = &frag->segment;
        frag->base.des_dst_cnt = 1;
        frag->base.des_src = NULL;
        frag->base.des_src_cnt = 0;
        frag->base.des_flags = 0;
        frag->type = MCA_BTL_UDAPL_RECV;

        cookie.as_ptr = frag;

        rc = dat_ep_post_recv(ep, 1,
                &frag->triplet, cookie, DAT_COMPLETION_DEFAULT_FLAG);
        if(DAT_SUCCESS != rc) {
            MCA_BTL_UDAPL_ERROR(rc, "dat_ep_post_recv");
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}


/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_btl_udapl_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
{
    endpoint->endpoint_btl = 0;
    endpoint->endpoint_proc = 0;

    endpoint->endpoint_connection_seq = 0;
    endpoint->endpoint_eager_sends = mca_btl_udapl_component.udapl_num_sends;
    endpoint->endpoint_max_sends = mca_btl_udapl_component.udapl_num_sends;

    endpoint->endpoint_state = MCA_BTL_UDAPL_CLOSED;
    endpoint->endpoint_eager = DAT_HANDLE_NULL;
    endpoint->endpoint_max = DAT_HANDLE_NULL;

    OBJ_CONSTRUCT(&endpoint->endpoint_eager_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_max_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_lock, opal_mutex_t);
}


/*
 * Destroy a endpoint
 *
 */

static void mca_btl_udapl_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
    OBJ_DESTRUCT(&endpoint->endpoint_eager_frags);
    OBJ_DESTRUCT(&endpoint->endpoint_max_frags);
    OBJ_DESTRUCT(&endpoint->endpoint_lock);
}


OBJ_CLASS_INSTANCE(
    mca_btl_udapl_endpoint_t, 
    opal_list_item_t, 
    mca_btl_udapl_endpoint_construct, 
    mca_btl_udapl_endpoint_destruct);

