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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
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
#include "opal/util/show_help.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss.h"
#include "orte/class/orte_pointer_array.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_udapl.h"
#include "btl_udapl_endpoint.h"
#include "btl_udapl_frag.h"
#include "btl_udapl_mca.h"
#include "btl_udapl_proc.h"

static void mca_btl_udapl_endpoint_send_cb(int status, orte_process_name_t* endpoint, 
                                           orte_buffer_t* buffer, orte_rml_tag_t tag,
                                           void* cbdata);
static int mca_btl_udapl_start_connect(mca_btl_base_endpoint_t* endpoint);
static int mca_btl_udapl_endpoint_post_recv(mca_btl_udapl_endpoint_t* endpoint,
                                            size_t size);
void mca_btl_udapl_endpoint_connect(mca_btl_udapl_endpoint_t* endpoint);
void mca_btl_udapl_endpoint_recv(int status, orte_process_name_t* endpoint, 
                                 orte_buffer_t* buffer, orte_rml_tag_t tag,
                                 void* cbdata);
static int mca_btl_udapl_endpoint_finish_eager(mca_btl_udapl_endpoint_t*);
static int mca_btl_udapl_endpoint_finish_max(mca_btl_udapl_endpoint_t*);
static void mca_btl_udapl_endpoint_connect_eager_rdma(mca_btl_udapl_endpoint_t* endpoint);
static int mca_btl_udapl_endpoint_write_eager(mca_btl_base_endpoint_t* endpoint,
                                              mca_btl_udapl_frag_t* frag);
static void mca_btl_udapl_endpoint_control_send_cb(mca_btl_base_module_t* btl,
                                                   mca_btl_base_endpoint_t* endpoint,
                                                   mca_btl_base_descriptor_t* descriptor,
                                                   int status);
static int mca_btl_udapl_endpoint_send_eager_rdma(mca_btl_base_endpoint_t* endpoint);


/*
 *  Write a fragment
 *
 * @param endpoint (IN)    BTL addressing information
 * @param frag (IN)        Fragment to be transferred
 *
 * @return                 OMPI_SUCCESS or OMPI_ERROR
 */
int mca_btl_udapl_endpoint_write_eager(mca_btl_base_endpoint_t* endpoint,
    mca_btl_udapl_frag_t* frag)
{
    DAT_DTO_COOKIE cookie;
    mca_btl_udapl_frag_eager_rdma_t* remote_frag;        
    char* remote_buf;
    DAT_RMR_TRIPLET remote_buffer;
    int rc = OMPI_SUCCESS;
    int pad = 0;
    uint8_t head = endpoint->endpoint_eager_rdma_remote.head;
        
    /* now that we have the head update it */
    MCA_BTL_UDAPL_RDMA_NEXT_INDEX(endpoint->endpoint_eager_rdma_remote.head);
    
    MCA_BTL_UDAPL_FRAG_CALC_ALIGNMENT_PAD(pad,
        (frag->segment.seg_len + sizeof(mca_btl_udapl_footer_t)));
    
    /* set the rdma footer information */
    frag->rdma_ftr = (mca_btl_udapl_rdma_footer_t *)
        ((char *)frag->segment.seg_addr.pval +
        frag->segment.seg_len +
        sizeof(mca_btl_udapl_footer_t) +
        pad);
    frag->rdma_ftr->active = 1;
    frag->rdma_ftr->size = frag->segment.seg_len; /* this is size PML wants;
                                                   * will have to calc
                                                   * alignment
                                                   * at the other end
                                                   */

    /* find remote fragment to be used */
    remote_frag = (mca_btl_udapl_frag_eager_rdma_t *)
        ((char *)(endpoint->endpoint_eager_rdma_remote.base.pval) +
        (head * mca_btl_udapl_component.udapl_eager_rdma_frag_size));
        
    /* prep the fragment to be written out */
    frag->type = MCA_BTL_UDAPL_RDMA_WRITE;
    frag->triplet.segment_length = frag->segment.seg_len +
        sizeof(mca_btl_udapl_footer_t) +
        pad +
        sizeof(mca_btl_udapl_rdma_footer_t);

    /* set remote_buf to start of the remote write location;
     * compute by first finding the end of the entire fragment
     * and then working way back
     */
    remote_buf = (char *)remote_frag +
        (sizeof(mca_btl_udapl_frag_eager_rdma_t) + frag->size) -
        frag->triplet.segment_length;

    /* execute transfer with one contiguous write */
        
    /* establish remote memory region */
    remote_buffer.rmr_context =
        (DAT_RMR_CONTEXT)endpoint->endpoint_eager_rdma_remote.rkey;
    remote_buffer.target_address = (DAT_VADDR)remote_buf;
    remote_buffer.segment_length = frag->triplet.segment_length;

    /* write the data out */
    cookie.as_ptr = frag;
    rc = dat_ep_post_rdma_write(endpoint->endpoint_eager,        
        1,
        &(frag->triplet),
        cookie,
        &remote_buffer,
        DAT_COMPLETION_DEFAULT_FLAG);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major, (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_post_rdma_write",
            major, minor));
        return OMPI_ERROR;
    } 

    return rc;
}


int mca_btl_udapl_endpoint_send(mca_btl_base_endpoint_t* endpoint,
                                mca_btl_udapl_frag_t* frag)
{
    int rc = OMPI_SUCCESS;
    DAT_RETURN dat_rc;
    DAT_DTO_COOKIE cookie;
    bool call_progress = false;
    
    /* Fix up the segment length before we do anything with the frag */
    frag->triplet.segment_length =
            frag->segment.seg_len + sizeof(mca_btl_udapl_footer_t);

    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
    switch(endpoint->endpoint_state) {
    case MCA_BTL_UDAPL_CONNECTED:
        /* just send it already.. */
        if(frag->size ==
            mca_btl_udapl_component.udapl_eager_frag_size) {
	    
            if(OPAL_THREAD_ADD32(&endpoint->endpoint_eager_rdma_remote.tokens, -1) < 0) {
                /* no rdma segment available so either send or queue */
                OPAL_THREAD_ADD32(&endpoint->endpoint_eager_rdma_remote.tokens, 1);

                if(OPAL_THREAD_ADD32(&endpoint->endpoint_sr_tokens[BTL_UDAPL_EAGER_CONNECTION], -1) < 0) {
                    OPAL_THREAD_ADD32(&endpoint->endpoint_sr_tokens[BTL_UDAPL_EAGER_CONNECTION], 1);
                    opal_list_append(&endpoint->endpoint_eager_frags,
                        (opal_list_item_t*)frag);
                    call_progress = true;

                } else {
                    cookie.as_ptr = frag;

                    dat_rc = dat_ep_post_send(endpoint->endpoint_eager, 1,
                        &frag->triplet, cookie,
                        DAT_COMPLETION_DEFAULT_FLAG);
        
                    if(DAT_SUCCESS != dat_rc) {
                        char* major;
                        char* minor;

                        dat_strerror(rc, (const char**)&major,
                            (const char**)&minor);
                        BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_post_send",
                            major, minor));
                        endpoint->endpoint_state = MCA_BTL_UDAPL_FAILED;
                        rc = OMPI_ERROR;
                    }
                }

            } else {
                rc = mca_btl_udapl_endpoint_write_eager(endpoint, frag);
            }
            
        } else {
            assert(frag->size ==
                mca_btl_udapl_component.udapl_max_frag_size);
            if(OPAL_THREAD_ADD32(&endpoint->endpoint_sr_tokens[BTL_UDAPL_MAX_CONNECTION], -1) < 0) {
                OPAL_THREAD_ADD32(&endpoint->endpoint_sr_tokens[BTL_UDAPL_MAX_CONNECTION], 1);
                opal_list_append(&endpoint->endpoint_max_frags,
                    (opal_list_item_t*)frag);
                call_progress = true;

            } else {
                 cookie.as_ptr = frag;

                 dat_rc = dat_ep_post_send(endpoint->endpoint_max, 1,
                     &frag->triplet, cookie,
                     DAT_COMPLETION_DEFAULT_FLAG);
                 
                 if(DAT_SUCCESS != dat_rc) {
                    char* major;
                    char* minor;

                    dat_strerror(rc, (const char**)&major,
                        (const char**)&minor);
                    BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_post_send",
                        major, minor));
                    rc = OMPI_ERROR;
                 }
            }
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

    if(call_progress) opal_progress();
    
    return rc;
}


static void mca_btl_udapl_endpoint_send_cb(int status, orte_process_name_t* endpoint, 
        orte_buffer_t* buffer, orte_rml_tag_t tag, void* cbdata)
{
    OBJ_RELEASE(buffer);
}


/*
 * Set uDAPL endpoint parameters as required in ep_param. Accomplished
 * by retrieving the default set of parameters from temporary (dummy)
 * endpoint and then setting any other parameters as required by
 * this BTL. 
 *
 * @param btl (IN)         BTL module 
 * @param ep_param (IN/OUT)Pointer to a valid endpoint parameter location
 *
 * @return                 OMPI_SUCCESS or error status on failure
 */
int mca_btl_udapl_endpoint_get_params(mca_btl_udapl_module_t* btl,
    DAT_EP_PARAM* ep_param)
{
    int        rc = OMPI_SUCCESS;
    int request_dtos;
    int max_control_messages;
    DAT_EP_HANDLE dummy_ep;    
    DAT_EP_ATTR* ep_attr = &((*ep_param).ep_attr);

    /* open dummy endpoint, used to find default endpoint parameters */
    rc = dat_ep_create(btl->udapl_ia,
        btl->udapl_pz,
        btl->udapl_evd_dto,
        btl->udapl_evd_dto,
        btl->udapl_evd_conn,
        NULL,
        &dummy_ep);
    if (rc != DAT_SUCCESS) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_create",
            major, minor));
        /* this could be recoverable, by just using defaults */
        ep_attr = NULL;
        return OMPI_ERROR;
    }
    
    rc = dat_ep_query(dummy_ep,
        DAT_EP_FIELD_ALL,
        ep_param);
    if (rc != DAT_SUCCESS) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_query",
            major, minor));

        /* this could be recoverable, by just using defaults */
        ep_attr = NULL;
        return OMPI_ERROR;
    }

    /* Set max_recv_dtos :
     * The max_recv_dtos should be equal to the number of
     * outstanding posted receives, which for this BTL will
     * be mca_btl_udapl_component.udapl_num_recvs.
     */
    if (btl->udapl_max_recv_dtos <
        mca_btl_udapl_component.udapl_num_recvs) {
        
        if (MCA_BTL_UDAPL_MAX_RECV_DTOS_DEFAULT != 
            btl->udapl_max_recv_dtos) {        

            /* user modified, this will fail and is not acceptable */
            opal_show_help("help-mpi-btl-udapl.txt",
                "max_recv_dtos too low", 
                true,
                btl->udapl_max_recv_dtos,
                mca_btl_udapl_component.udapl_num_recvs);

            btl->udapl_max_recv_dtos = 
                mca_btl_udapl_component.udapl_num_recvs;
        }

        if (MCA_BTL_UDAPL_NUM_RECVS_DEFAULT != 
                        mca_btl_udapl_component.udapl_num_recvs) {
            
            /* user modified udapl_num_recvs so adjust max_recv_dtos */
            btl->udapl_max_recv_dtos = 
                mca_btl_udapl_component.udapl_num_recvs;
        }
    } 

    (*ep_attr).max_recv_dtos = btl->udapl_max_recv_dtos;

    /* Set max_request_dtos :
     * The max_request_dtos should equal the max number of
     * outstanding sends plus RDMA operations.
     *
     * Note: Using the same value for both EAGER and MAX
     * connections even though the MAX connection does not
     * have the extra RDMA operations that the EAGER
     * connection does.
     */
    max_control_messages =
        (mca_btl_udapl_component.udapl_num_recvs /
        mca_btl_udapl_component.udapl_sr_win) + 1 +
        (mca_btl_udapl_component.udapl_eager_rdma_num /
        mca_btl_udapl_component.udapl_eager_rdma_win) + 1;
    request_dtos = mca_btl_udapl_component.udapl_num_sends +
        (2*mca_btl_udapl_component.udapl_eager_rdma_num) +
        max_control_messages;

    if (btl->udapl_max_request_dtos < request_dtos) {
        if (MCA_BTL_UDAPL_MAX_REQUEST_DTOS_DEFAULT != 
            mca_btl_udapl_module.udapl_max_request_dtos) {

            /* user has modified */
            opal_show_help("help-mpi-btl-udapl.txt",
                "max_request_dtos too low", 
                true,
                btl->udapl_max_request_dtos, request_dtos);
        } else {
            btl->udapl_max_request_dtos = request_dtos;
        }         
    }

    if (btl->udapl_max_request_dtos > btl->udapl_ia_attr.max_dto_per_ep) {
        /* do not go beyond what is allowed by the system */
        opal_show_help("help-mpi-btl-udapl.txt",
            "max_request_dtos system max", 
            true,
            btl->udapl_max_request_dtos,
            btl->udapl_ia_attr.max_dto_per_ep);
        btl->udapl_max_request_dtos = btl->udapl_ia_attr.max_dto_per_ep;
    }

    (*ep_attr).max_request_dtos = btl->udapl_max_request_dtos;
    
    /* close the dummy endpoint */
    rc = dat_ep_free(dummy_ep);
    if (rc != DAT_SUCCESS) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("WARNING: %s %s %s\n", "dat_ep_free",
            major, minor));
        /* this could be recoverable, by just using defaults */
    }                

    return rc;
}

/*
 * Create a uDAPL endpoint
 *
 * @param btl (IN)         BTL module 
 * @param ep_endpoint (IN) uDAPL endpoint information
 *
 * @return                 OMPI_SUCCESS or error status on failure
 */
int mca_btl_udapl_endpoint_create(mca_btl_udapl_module_t* btl,
    DAT_EP_HANDLE* udapl_endpoint)
{
    int rc = OMPI_SUCCESS;

    /* Create a new uDAPL endpoint and start the connection process */
    rc = dat_ep_create(btl->udapl_ia, btl->udapl_pz,
            btl->udapl_evd_dto, btl->udapl_evd_dto, btl->udapl_evd_conn,
            &(btl->udapl_ep_param.ep_attr), udapl_endpoint);

    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_create",
            major, minor));
        dat_ep_free(udapl_endpoint);
        udapl_endpoint = DAT_HANDLE_NULL;
    }

    return rc;
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

    OPAL_THREAD_ADD32(&(endpoint->endpoint_btl->udapl_connect_inprogress), 1);

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
    OPAL_THREAD_ADD32(&(btl->udapl_connect_inprogress), 1);
    
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
    rc = mca_btl_udapl_endpoint_create(btl,  &endpoint->endpoint_eager);
    if(DAT_SUCCESS != rc) {
        BTL_ERROR(("mca_btl_udapl_endpoint_create"));
        goto failure_create;
    }

    rc = dat_ep_connect(endpoint->endpoint_eager, &endpoint->endpoint_addr.addr,
            endpoint->endpoint_addr.port, mca_btl_udapl_component.udapl_timeout,
            sizeof(mca_btl_udapl_addr_t), &btl->udapl_addr, 0, DAT_CONNECT_DEFAULT_FLAG);
    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_connect",
            major, minor));
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
                    ep->endpoint_connection_seq = (NULL != connection_seq) ?
                        *connection_seq:0;
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
                    if (NULL == connection_seq ||
                        ep->endpoint_connection_seq < *connection_seq) {
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

    /* establish eager rdma connection */
    if (btl->udapl_eager_rdma_endpoint_count <
        mca_btl_udapl_component.udapl_max_eager_rdma_peers) {
        mca_btl_udapl_endpoint_connect_eager_rdma(endpoint);
    }

    /* Only one side does dat_ep_connect() */
    if(0 < orte_ns.compare_fields(ORTE_NS_CMP_ALL,
                &endpoint->endpoint_proc->proc_guid,
                &ompi_proc_local()->proc_name)) {
    
        rc = mca_btl_udapl_endpoint_create(btl, &endpoint->endpoint_max);
        if(DAT_SUCCESS != rc) {
            endpoint->endpoint_state = MCA_BTL_UDAPL_FAILED;
            OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
            return OMPI_ERROR;
        }

        rc = dat_ep_connect(endpoint->endpoint_max,
            &endpoint->endpoint_addr.addr, endpoint->endpoint_addr.port,
            mca_btl_udapl_component.udapl_timeout,
            sizeof(mca_btl_udapl_addr_t),&btl->udapl_addr , 0,
            DAT_CONNECT_DEFAULT_FLAG);
        if(DAT_SUCCESS != rc) {
            char* major;
            char* minor;

            dat_strerror(rc, (const char**)&major,
                (const char**)&minor);
            BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_connect",
                major, minor));
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
    OPAL_THREAD_ADD32(&(endpoint->endpoint_btl->udapl_connect_inprogress), -1);

    /* post eager/max recv buffers */
    mca_btl_udapl_endpoint_post_recv(endpoint,
            mca_btl_udapl_component.udapl_eager_frag_size);
    mca_btl_udapl_endpoint_post_recv(endpoint,
            mca_btl_udapl_component.udapl_max_frag_size);

    /* post queued sends */
    assert(endpoint->endpoint_eager_sends ==
            mca_btl_udapl_component.udapl_num_sends);
    while(OPAL_THREAD_ADD32(&endpoint->endpoint_sr_tokens[BTL_UDAPL_EAGER_CONNECTION], -1) >= 0 &&
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
            char* major;
            char* minor;

            dat_strerror(rc, (const char**)&major,
                (const char**)&minor);
            BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_post_send",
                major, minor));
            endpoint->endpoint_state = MCA_BTL_UDAPL_FAILED;
            ret = OMPI_ERROR;
            break;
        }
    }

    if(endpoint->endpoint_sr_tokens[BTL_UDAPL_EAGER_CONNECTION] < 0) {
        OPAL_THREAD_ADD32(&endpoint->endpoint_sr_tokens[BTL_UDAPL_EAGER_CONNECTION], 1);
    }

    assert(endpoint->endpoint_max_sends ==
            mca_btl_udapl_component.udapl_num_sends);
    while(OPAL_THREAD_ADD32(&endpoint->endpoint_sr_tokens[BTL_UDAPL_MAX_CONNECTION], -1) >= 0 &&
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
            char* major;
            char* minor;

            dat_strerror(rc, (const char**)&major,
                (const char**)&minor);
            BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_post_send",
                major, minor));
            endpoint->endpoint_state = MCA_BTL_UDAPL_FAILED;
            ret = OMPI_ERROR;
            break;
        }
    }

    if(endpoint->endpoint_sr_tokens[BTL_UDAPL_MAX_CONNECTION] < 0) {
        OPAL_THREAD_ADD32(&endpoint->endpoint_sr_tokens[BTL_UDAPL_MAX_CONNECTION], 1);
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
            char* major;
            char* minor;

            dat_strerror(rc, (const char**)&major,
                (const char**)&minor);
            BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_post_recv",
                major, minor));
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

    endpoint->endpoint_sr_tokens[BTL_UDAPL_EAGER_CONNECTION] =
        endpoint->endpoint_eager_sends;
    endpoint->endpoint_sr_tokens[BTL_UDAPL_MAX_CONNECTION] =
        endpoint->endpoint_max_sends;
    endpoint->endpoint_sr_credits[BTL_UDAPL_EAGER_CONNECTION] = 0;
    endpoint->endpoint_sr_credits[BTL_UDAPL_MAX_CONNECTION] = 0;

    OBJ_CONSTRUCT(&endpoint->endpoint_eager_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_max_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_lock, opal_mutex_t);

    /* initialize eager RDMA */
    memset(&endpoint->endpoint_eager_rdma_local, 0,
        sizeof(mca_btl_udapl_eager_rdma_local_t));
    memset (&endpoint->endpoint_eager_rdma_remote, 0,
        sizeof(mca_btl_udapl_eager_rdma_remote_t));
    OBJ_CONSTRUCT(&endpoint->endpoint_eager_rdma_local.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_eager_rdma_remote.lock, opal_mutex_t);  
}

/*
 * Destroy a endpoint
 *
 */

static void mca_btl_udapl_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
    mca_btl_udapl_module_t* udapl_btl = endpoint->endpoint_btl;
    mca_mpool_base_registration_t *reg =
        (mca_mpool_base_registration_t*)endpoint->endpoint_eager_rdma_local.reg;

    OBJ_DESTRUCT(&endpoint->endpoint_eager_frags);
    OBJ_DESTRUCT(&endpoint->endpoint_max_frags);
    OBJ_DESTRUCT(&endpoint->endpoint_lock);

    /* release eager rdma resources */
    udapl_btl->super.btl_mpool->mpool_free(udapl_btl->super.btl_mpool,
        NULL,
        reg);
}


/*
 * Release the fragment used to send the eager rdma control message.
 * Callback to be executed upon receiving local completion event
 * from sending a control message operation. Should essentially do
 * the same thing as mca_btl_udapl_free().
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 * @param status (IN/OUT)  
 */
static void mca_btl_udapl_endpoint_control_send_cb(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    MCA_BTL_UDAPL_FRAG_RETURN_CONTROL(((mca_btl_udapl_module_t*)btl),
        ((mca_btl_udapl_frag_t*)descriptor)); 
}

/*
 * Allocate and initialize descriptor to be used in sending uDAPL BTL
 * control messages. Should essentially accomplish same as would be
 * from calling mca_btl_udapl_alloc().
 *
 * @param btl (IN)         BTL module
 * @param size (IN)        Size of segment required to be transferred
 *
 * @return descriptor (IN)  Description of the data to be transferred
 */
static mca_btl_base_descriptor_t* mca_btl_udapl_endpoint_initialize_control_message(
    struct mca_btl_base_module_t* btl,
    size_t size)
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*) btl; 
    mca_btl_udapl_frag_t* frag;
    int rc;
    int pad = 0;

    /* compute pad as needed */
    MCA_BTL_UDAPL_FRAG_CALC_ALIGNMENT_PAD(pad,
        (size + sizeof(mca_btl_udapl_footer_t)));

    /* control messages size should never be greater than eager message size */
    assert((size+pad) <= btl->btl_eager_limit);

    MCA_BTL_UDAPL_FRAG_ALLOC_CONTROL(udapl_btl, frag, rc); 

    /* Set up the LMR triplet from the frag segment */
    frag->segment.seg_len = (uint32_t)size;
    frag->triplet.virtual_address = (DAT_VADDR)frag->segment.seg_addr.pval;

    /* assume send/recv as default when computing segment_length */
    frag->triplet.segment_length =
        frag->segment.seg_len + sizeof(mca_btl_udapl_footer_t);

    assert(frag->triplet.lmr_context ==
        ((mca_btl_udapl_reg_t*)frag->registration)->lmr_triplet.lmr_context);
    
    frag->btl = udapl_btl;
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;
    frag->base.des_cbfunc = mca_btl_udapl_endpoint_control_send_cb;    
    frag->base.des_cbdata = NULL;

    return &frag->base;
}

/*
 * Transfer the given endpoints rdma segment information. Expects that
 * the endpoints rdma segment has previoulsy been created and
 * registered as required.
 * 
 * @param endpoint (IN)    BTL addressing information
 *
 * @return                 OMPI_SUCCESS or error status on failure
 */
static int mca_btl_udapl_endpoint_send_eager_rdma(
    mca_btl_base_endpoint_t* endpoint)
{
    mca_btl_udapl_eager_rdma_connect_t* rdma_connect;
    mca_btl_base_descriptor_t* des;    
    mca_btl_base_segment_t* segment;
    mca_btl_udapl_module_t* udapl_btl = endpoint->endpoint_btl;
    size_t cntrl_msg_size = sizeof(mca_btl_udapl_eager_rdma_connect_t);
    int rc = OMPI_SUCCESS;

    des = mca_btl_udapl_endpoint_initialize_control_message(
        (mca_btl_base_module_t *)udapl_btl, cntrl_msg_size); 
    
    des->des_flags = 0;
    des->des_cbfunc = mca_btl_udapl_endpoint_control_send_cb;
    des->des_cbdata = NULL;

    /* fill in data */
    segment = des->des_src;
    rdma_connect =
        (mca_btl_udapl_eager_rdma_connect_t*)segment->seg_addr.pval;    
    rdma_connect->control.type =
        MCA_BTL_UDAPL_CONTROL_RDMA_CONNECT;
    rdma_connect->rkey =
        endpoint->endpoint_eager_rdma_local.reg->rmr_context;
    rdma_connect->rdma_start.pval =
        endpoint->endpoint_eager_rdma_local.base.pval;

    /* send fragment */
    rc = mca_btl_udapl_send((mca_btl_base_module_t *)udapl_btl, endpoint,
        des, MCA_BTL_TAG_BTL);

    return rc;
}

/*
 * Endpoint handed in is the local process peer. This routine
 * creates and initializes a local memory region which will be used for
 * reading from locally. This memory region will be made available to peer
 * for writing into by sending a description of the area to the given
 * endpoint.
 *
 * @param endpoint (IN)    BTL addressing information
 */
void mca_btl_udapl_endpoint_connect_eager_rdma(
    mca_btl_udapl_endpoint_t* endpoint)
{
    char* buf;
    size_t size;
    int i;
    mca_btl_udapl_module_t* udapl_btl = endpoint->endpoint_btl;

    OPAL_THREAD_LOCK(&endpoint->endpoint_eager_rdma_local.lock);
    if (endpoint->endpoint_eager_rdma_local.base.pval)
        goto unlock_rdma_local;

    if (mca_btl_udapl_component.udapl_eager_rdma_num <= 0) {
        /* NOTE: Need to find a more generic way to check ranges
         * for all mca parameters.
         */
        opal_show_help("help-mpi-btl-udapl.txt",
            "invalid num rdma segments", 
            true,
            mca_btl_udapl_component.udapl_eager_rdma_num);
        goto unlock_rdma_local;
    } 

    /* determine total size of buffer region */
    size = mca_btl_udapl_component.udapl_eager_rdma_frag_size *
        mca_btl_udapl_component.udapl_eager_rdma_num;

    /* create and register memory */
    buf = udapl_btl->super.btl_mpool->mpool_alloc(udapl_btl->super.btl_mpool,
            size, 0, 0,
            (mca_mpool_base_registration_t**)&endpoint->endpoint_eager_rdma_local.reg);

    if(!buf)
       goto unlock_rdma_local;

    /* initialize the rdma segments */
    for(i = 0; i < mca_btl_udapl_component.udapl_eager_rdma_num; i++) {
         mca_btl_udapl_frag_eager_rdma_t* local_rdma_frag;
         ompi_free_list_item_t *item = (ompi_free_list_item_t *)(buf +
                i*mca_btl_udapl_component.udapl_eager_rdma_frag_size);
         item->user_data = endpoint->endpoint_eager_rdma_local.reg;
         OBJ_CONSTRUCT(item, mca_btl_udapl_frag_eager_rdma_t);

         local_rdma_frag = ((mca_btl_udapl_frag_eager_rdma_t*)item);

         local_rdma_frag->base.des_dst = &local_rdma_frag->segment;
         local_rdma_frag->base.des_dst_cnt = 1;
         local_rdma_frag->base.des_src = NULL;
         local_rdma_frag->base.des_src_cnt = 0;
         local_rdma_frag->btl = endpoint->endpoint_btl;

         
         local_rdma_frag->endpoint = endpoint;
         local_rdma_frag->type = MCA_BTL_UDAPL_FRAG_EAGER_RDMA;
         local_rdma_frag->triplet.segment_length = local_rdma_frag->size; 
     }

    OPAL_THREAD_LOCK(&udapl_btl->udapl_eager_rdma_lock);
    if(orte_pointer_array_add (&endpoint->endpoint_eager_rdma_index,
                udapl_btl->udapl_eager_rdma_endpoints, endpoint) < 0)
           goto cleanup;

    endpoint->endpoint_eager_rdma_local.base.pval = buf;
    udapl_btl->udapl_eager_rdma_endpoint_count++;

    /* send the relevant data describing the registered space to the endpoint */
    if (mca_btl_udapl_endpoint_send_eager_rdma(endpoint) == 0) {
        OPAL_THREAD_UNLOCK(&udapl_btl->udapl_eager_rdma_lock);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_eager_rdma_local.lock);
        return;
    }

    udapl_btl->udapl_eager_rdma_endpoint_count--;
    endpoint->endpoint_eager_rdma_local.base.pval = NULL;
    orte_pointer_array_set_item(udapl_btl->udapl_eager_rdma_endpoints,
            endpoint->endpoint_eager_rdma_index, NULL);

cleanup:
    /* this would fail if we hit the max and can not add anymore to the array
     * and this could happen because we do not lock before checking if max has
     * been reached
     */
    OPAL_THREAD_UNLOCK(&udapl_btl->udapl_eager_rdma_lock);
    udapl_btl->super.btl_mpool->mpool_free(udapl_btl->super.btl_mpool,
        buf,
        (mca_mpool_base_registration_t*)endpoint->endpoint_eager_rdma_local.reg);

  unlock_rdma_local:
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_eager_rdma_local.lock);
    
}

/*
 * Send control message with the number of credits available on the
 * endpoint. Update the credit value accordingly.
 *
 * @param endpoint (IN)    BTL addressing information
 *
 * @return                 OMPI_SUCCESS or error status on failure
 */
int mca_btl_udapl_endpoint_send_eager_rdma_credits(
    mca_btl_base_endpoint_t* endpoint)
{
    mca_btl_udapl_eager_rdma_credit_t *rdma_credit;
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    DAT_DTO_COOKIE cookie;
    mca_btl_udapl_frag_t* frag;
    mca_btl_udapl_module_t* udapl_btl = endpoint->endpoint_btl;
    size_t cntrl_msg_size = sizeof(mca_btl_udapl_eager_rdma_credit_t);
    int rc = OMPI_SUCCESS;

    des = mca_btl_udapl_endpoint_initialize_control_message(
        (mca_btl_base_module_t *)udapl_btl, cntrl_msg_size);

    /* fill in data */
    segment = des->des_src;
    rdma_credit = (mca_btl_udapl_eager_rdma_credit_t*)segment->seg_addr.pval;
    rdma_credit->control.type = MCA_BTL_UDAPL_CONTROL_RDMA_CREDIT;
    rdma_credit->credits = endpoint->endpoint_eager_rdma_local.credits;

    /* reset local credits value */
    OPAL_THREAD_LOCK(&endpoint->endpoint_eager_rdma_local.lock);
    endpoint->endpoint_eager_rdma_local.credits -= rdma_credit->credits;

    /* prep and send fragment : control messages do not count
     * against the token/credit number so do not subtract from tokens
     * with this send
     */
    frag = (mca_btl_udapl_frag_t*)des;
    frag->endpoint = endpoint;
    frag->ftr = (mca_btl_udapl_footer_t *)
        ((char *)frag->segment.seg_addr.pval + frag->segment.seg_len);
    frag->ftr->tag = MCA_BTL_TAG_BTL;
    frag->type = MCA_BTL_UDAPL_SEND;
    cookie.as_ptr = frag;
                    
    rc = dat_ep_post_send(endpoint->endpoint_eager, 1,
        &frag->triplet, cookie,
        DAT_COMPLETION_DEFAULT_FLAG);

    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);

    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_post_send",
            major, minor));
        endpoint->endpoint_state = MCA_BTL_UDAPL_FAILED;
        rc = OMPI_ERROR;
    }
    
    return rc;
}

/*
 * Send control message with the number of credits available on the
 * endpoint. Update the credit value accordingly.
 *
 * @param endpoint (IN)    BTL addressing information
 *
 * @param connection (IN)  0 for eager and 1 for max connection
 *
 * @return                 OMPI_SUCCESS or error status on failure
 */
int mca_btl_udapl_endpoint_send_sr_credits(
    mca_btl_base_endpoint_t* endpoint, const int connection)
{
    mca_btl_udapl_sr_credit_t *sr_credit;
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    DAT_DTO_COOKIE cookie;
    mca_btl_udapl_frag_t* frag;
    mca_btl_udapl_module_t* udapl_btl = endpoint->endpoint_btl;
    size_t cntrl_msg_size = sizeof(mca_btl_udapl_sr_credit_t);
    int rc = OMPI_SUCCESS;

    des = mca_btl_udapl_endpoint_initialize_control_message(
        (mca_btl_base_module_t *)udapl_btl, cntrl_msg_size);

    /* fill in data */
    segment = des->des_src;
    sr_credit = (mca_btl_udapl_sr_credit_t*)segment->seg_addr.pval;
    sr_credit->control.type = MCA_BTL_UDAPL_CONTROL_SR_CREDIT;
    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
    sr_credit->credits = endpoint->endpoint_sr_credits[connection];
    sr_credit->connection = connection;

    /* reset local credits value */
    endpoint->endpoint_sr_credits[connection] = 0;

    /* prep and send fragment : control messages do not count
     * against the token/credit count so do not subtract from tokens
     * with this send
     */
    frag = (mca_btl_udapl_frag_t*)des;
    frag->endpoint = endpoint;
    frag->ftr = (mca_btl_udapl_footer_t *)
        ((char *)frag->segment.seg_addr.pval + frag->segment.seg_len);
    frag->ftr->tag = MCA_BTL_TAG_BTL;
    frag->type = MCA_BTL_UDAPL_SEND;
    cookie.as_ptr = frag;
                    
    if (BTL_UDAPL_EAGER_CONNECTION == connection) {
        rc = dat_ep_post_send(endpoint->endpoint_eager, 1,
            &frag->triplet, cookie,
            DAT_COMPLETION_DEFAULT_FLAG);

    } else {
        assert(BTL_UDAPL_MAX_CONNECTION == connection);
        rc = dat_ep_post_send(endpoint->endpoint_max, 1,
            &frag->triplet, cookie,
            DAT_COMPLETION_DEFAULT_FLAG);
    }

    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);

    if(DAT_SUCCESS != rc) {
        char* major;
        char* minor;

        dat_strerror(rc, (const char**)&major,
            (const char**)&minor);
        BTL_ERROR(("ERROR: %s %s %s\n", "dat_ep_post_send",
            major, minor));
        endpoint->endpoint_state = MCA_BTL_UDAPL_FAILED;
        rc = OMPI_ERROR;
    }
    
    return rc;
}


OBJ_CLASS_INSTANCE(
    mca_btl_udapl_endpoint_t, 
    opal_list_item_t, 
    mca_btl_udapl_endpoint_construct, 
    mca_btl_udapl_endpoint_destruct);

