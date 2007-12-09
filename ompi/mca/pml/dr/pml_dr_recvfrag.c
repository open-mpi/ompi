/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 */

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "opal/threads/mutex.h"
#include "opal/util/crc.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/datatype.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/class/ompi_seq_tracker.h"
#include "pml_dr.h"
#include "pml_dr_comm.h"
#include "pml_dr_recvfrag.h"
#include "pml_dr_recvreq.h"
#include "pml_dr_sendreq.h"
#include "pml_dr_hdr.h"
#include "orte/mca/errmgr/errmgr.h"


    
#define MCA_PML_DR_HDR_VALIDATE_ACK(do_csum, hdr, type)                                        \
do {                                                                                           \
    mca_pml_dr_endpoint_t* ep;                                                                 \
    if(do_csum) {                                                                              \
        uint16_t csum = (uint16_t)opal_csum(hdr, sizeof(type));                                \
        if(hdr->hdr_common.hdr_csum != csum) {                                                 \
            MCA_PML_DR_DEBUG(0, (0, "%s:%d: invalid header checksum: 0x%04x != 0x%04x\n",      \
                                 __FILE__, __LINE__, hdr->hdr_common.hdr_csum, csum));         \
            return;                                                                            \
        }                                                                                      \
    }                                                                                          \
    ep = (mca_pml_dr_endpoint_t*)ompi_pointer_array_get_item(&mca_pml_dr.endpoints, hdr->hdr_common.hdr_src);          \
    assert(ep != NULL);                                                                        \
    if(ompi_seq_tracker_check_duplicate(&ep->seq_sends, hdr->hdr_common.hdr_vid)) {            \
        MCA_PML_DR_DEBUG(0, (0, "%s:%d: dropping duplicate ack, vfrag ID %d", \
                             __FILE__, __LINE__, hdr->hdr_common.hdr_vid)); \
        return;                                                         \
    }                                                                                          \
    MCA_PML_DR_DEBUG(1, (0, "%s:%d: couldn't find vfrag ID %d \n",      \
                         __FILE__, __LINE__, hdr->hdr_common.hdr_vid)); \
} while (0)


OBJ_CLASS_INSTANCE(
    mca_pml_dr_buffer_t,
    ompi_free_list_item_t,
    NULL,
    NULL
);

OBJ_CLASS_INSTANCE(
    mca_pml_dr_recv_frag_t,
    ompi_free_list_item_t,
    NULL,
    NULL
);

/*
 * Release resources.
 */

static void mca_pml_dr_ctl_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    mca_bml_base_free(bml_btl, des);
}


/**
 *  Callback from BTL on receive.
 */

void mca_pml_dr_recv_frag_callback(
                                    mca_btl_base_module_t* btl, 
                                    mca_btl_base_tag_t tag,
                                    mca_btl_base_descriptor_t* des,
                                    void* cbdata)
{
    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_dr_hdr_t* hdr = (mca_pml_dr_hdr_t*)segments->seg_addr.pval;
    mca_pml_dr_comm_t *comm;
    mca_pml_dr_comm_proc_t *proc;
    mca_pml_dr_endpoint_t *ep;
    ompi_communicator_t* ompi_comm;
    uint16_t csum;
    bool do_csum = mca_pml_dr.enable_csum && 
        (btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM); 
    
    if(segments->seg_len < sizeof(mca_pml_dr_common_hdr_t)) {
        MCA_PML_DR_DEBUG(0,(0, "%s:%d: wtf? segments->seg_len:%d < sizeof(mca_pml_dr_common_hdr_t):%lu\n", 
                            __FILE__, __LINE__, segments->seg_len, (unsigned long)sizeof(mca_pml_dr_common_hdr_t)));
    
        return;
    }

    MCA_PML_DR_DEBUG(0,(0, "%s:%d: got a hdr of type %d\n", 
                                    __FILE__, __LINE__, hdr->hdr_common.hdr_type));
                
    switch(hdr->hdr_common.hdr_type) {
    case MCA_PML_DR_HDR_TYPE_MATCH:
        {
            if(do_csum) { 
                csum = (uint16_t)opal_csum(hdr, sizeof(mca_pml_dr_match_hdr_t));                                              
                if(hdr->hdr_common.hdr_csum != csum) {                                                     
                    MCA_PML_DR_DEBUG(0,(0, "%s:%d: invalid header checksum: 0x%04x != 0x%04x\n",                  
                                        __FILE__, __LINE__, hdr->hdr_common.hdr_csum, csum));                              
                    return;                                                                                
                } 
            }
            if(hdr->hdr_common.hdr_dst != mca_pml_dr.my_rank ) {                                       
                MCA_PML_DR_DEBUG(0, (0, "%s:%d: misdelivered packet [rank %d -> rank %d]\n",                   
                                     __FILE__, __LINE__, hdr->hdr_common.hdr_src, hdr->hdr_common.hdr_dst));            
                return;                                                                                
            } 
            ep = (mca_pml_dr_endpoint_t*)ompi_pointer_array_get_item(&mca_pml_dr.endpoints, hdr->hdr_common.hdr_src);              
            assert(ep != NULL);                                                                        

            if(ompi_seq_tracker_check_duplicate(&ep->seq_recvs, hdr->hdr_common.hdr_vid)) {    
                MCA_PML_DR_DEBUG(0,(0, "%s:%d: got a duplicate vfrag vfrag id %d\n", 
                                    __FILE__, __LINE__, hdr->hdr_common.hdr_vid));
                mca_pml_dr_recv_frag_ack(btl, 
                                         ep->bml_endpoint,                                                    
                                         &hdr->hdr_common,                                             
                                         hdr->hdr_match.hdr_src_ptr.pval,                              
                                         1, 0);                                                        
                return;                                                                                
            } 
            ompi_comm = ompi_comm_lookup(hdr->hdr_common.hdr_ctx);                                     
            if(NULL == ompi_comm ) {                                                                   
                OPAL_OUTPUT((0, "%s:%d: invalid communicator %d\n",                                   
                                    __FILE__, __LINE__, hdr->hdr_common.hdr_ctx));                                     
                return;                                                                                
            }                                                                                          
            comm = (mca_pml_dr_comm_t*)ompi_comm->c_pml_comm;                                          
            assert(hdr->hdr_common.hdr_src < ompi_pointer_array_get_size(&comm->sparse_procs));        
            proc = (mca_pml_dr_comm_proc_t*)ompi_pointer_array_get_item(&comm->sparse_procs, hdr->hdr_common.hdr_src);          
            assert(proc != NULL);                                                                      
            assert(ep == proc->pml_endpoint);                                                              
            mca_pml_dr_recv_frag_match(comm,proc,btl,&hdr->hdr_match,segments,des->des_dst_cnt);
            break;
        
        }
    case MCA_PML_DR_HDR_TYPE_MATCH_ACK:
        {
            MCA_PML_DR_HDR_VALIDATE_ACK(do_csum, hdr, mca_pml_dr_ack_hdr_t);
            mca_pml_dr_send_request_match_ack(btl, &hdr->hdr_ack);
            break;
        }
    case MCA_PML_DR_HDR_TYPE_RNDV:
        {
            if(do_csum) {
                csum = (uint16_t)opal_csum(hdr, sizeof(mca_pml_dr_rendezvous_hdr_t));
                
                if(hdr->hdr_common.hdr_csum != csum) {
                    MCA_PML_DR_DEBUG(0, (0, "%s:%d: invalid header checksum: 0x%04x != 0x%04x\n",
                                         __FILE__, __LINE__, hdr->hdr_common.hdr_csum, csum));
                    return;
                } 
            }
            if(hdr->hdr_common.hdr_dst != mca_pml_dr.my_rank ) {
                MCA_PML_DR_DEBUG(0, (0, "%s:%d: misdelivered packet [rank %d -> rank %d]\n",
                                     __FILE__, __LINE__, hdr->hdr_common.hdr_src, hdr->hdr_common.hdr_dst));
                return;
            } 
            ep = (mca_pml_dr_endpoint_t*)ompi_pointer_array_get_item(&mca_pml_dr.endpoints, hdr->hdr_common.hdr_src);
            assert(ep != NULL);
            
            /* seq_recvs protected by matching lock */
            if(ompi_seq_tracker_check_duplicate(&ep->seq_recvs, hdr->hdr_common.hdr_vid)){ 
                mca_pml_dr_recv_request_t* recvreq;
                ompi_comm = ompi_comm_lookup(hdr->hdr_common.hdr_ctx);
                if(NULL == ompi_comm) {
                    if(ompi_seq_tracker_check_duplicate(&ep->seq_recvs_matched, hdr->hdr_common.hdr_vid)) {
                        MCA_PML_DR_DEBUG(0, (0, "%s:%d: acking duplicate matched rendezvous from sequence tracker\n",
											 __FILE__, __LINE__));
                        mca_pml_dr_recv_frag_ack(btl,
                                                 ep->bml_endpoint, 
                                                 &hdr->hdr_common, 
                                                 hdr->hdr_match.hdr_src_ptr.pval, 
                                                 ~(uint64_t) 0, hdr->hdr_rndv.hdr_msg_length);
                        return;
                    } else { 
                        OPAL_OUTPUT((0, "%s:%d: the world as we know it is bad\n", __FILE__, __LINE__));
                        orte_errmgr.abort();
                    }
                }
                comm = (mca_pml_dr_comm_t*)ompi_comm->c_pml_comm;
                assert(hdr->hdr_common.hdr_src < ompi_pointer_array_get_size(&comm->sparse_procs));
                proc = (mca_pml_dr_comm_proc_t*)ompi_pointer_array_get_item(&comm->sparse_procs, hdr->hdr_common.hdr_src);
                assert(proc != NULL);
                assert(ep == proc->pml_endpoint);
                
                /* ack only if the vfrag has been matched */
                 recvreq = 
                    mca_pml_dr_comm_proc_check_matched(proc, hdr->hdr_common.hdr_vid);
                 if(NULL != recvreq) {
                     MCA_PML_DR_DEBUG(0,(0, "%s:%d: acking duplicate matched rendezvous from pending matched vfrag id %d\n",
                                         __FILE__, __LINE__, hdr->hdr_common.hdr_vid));
                     mca_pml_dr_recv_request_ack(btl, recvreq, &hdr->hdr_common, 
                                                 hdr->hdr_match.hdr_src_ptr, recvreq->req_bytes_received, 1);
                } else { 
                     if(ompi_seq_tracker_check_duplicate(&ep->seq_recvs_matched, hdr->hdr_common.hdr_vid)) {
                        MCA_PML_DR_DEBUG(0,(0, "%s:%d: acking duplicate matched rendezvous from sequence tracker\n", __FILE__, __LINE__));
                        mca_pml_dr_recv_frag_ack(btl, 
                                                 ep->bml_endpoint, 
                                                 &hdr->hdr_common, 
                                                 hdr->hdr_match.hdr_src_ptr.pval, 
                                                 ~(uint64_t) 0, hdr->hdr_rndv.hdr_msg_length);
                    } else { 
                        MCA_PML_DR_DEBUG(0,(0, "%s:%d: droping duplicate unmatched rendezvous\n", __FILE__, __LINE__));
                    }
                }
            } else {
                ompi_comm = ompi_comm_lookup(hdr->hdr_common.hdr_ctx);
                if(NULL == ompi_comm) { 
                    OPAL_OUTPUT((0, "%s:%d: the world as we know it is bad\n", __FILE__, __LINE__));
                    orte_errmgr.abort();
                }
                comm = (mca_pml_dr_comm_t*)ompi_comm->c_pml_comm;
                assert(hdr->hdr_common.hdr_src < ompi_pointer_array_get_size(&comm->sparse_procs));
                proc = (mca_pml_dr_comm_proc_t*)ompi_pointer_array_get_item(&comm->sparse_procs, hdr->hdr_common.hdr_src);
                assert(proc != NULL);
                assert(ep == proc->pml_endpoint);
                mca_pml_dr_recv_frag_match(comm,proc,btl,&hdr->hdr_match,segments,des->des_dst_cnt);
            }
            break;
        }
    case MCA_PML_DR_HDR_TYPE_RNDV_ACK:
        {
            MCA_PML_DR_HDR_VALIDATE_ACK(do_csum, hdr, mca_pml_dr_ack_hdr_t);
            mca_pml_dr_send_request_rndv_ack(btl, &hdr->hdr_ack);
            break;
        }
    case MCA_PML_DR_HDR_TYPE_FRAG:
        {
            mca_pml_dr_recv_request_t* recvreq;
            
            if(do_csum) { 
                csum = (uint16_t)opal_csum(hdr, sizeof(mca_pml_dr_frag_hdr_t));
                if(hdr->hdr_common.hdr_csum != csum) {
                    MCA_PML_DR_DEBUG(0,(0, "%s:%d: invalid header checksum: 0x%04x != 0x%04x\n",
                                 __FILE__, __LINE__, hdr->hdr_common.hdr_csum, csum));
                    return;
                } 
            }
            if(hdr->hdr_common.hdr_dst != mca_pml_dr.my_rank ) {                                       
                MCA_PML_DR_DEBUG(0,(0, "%s:%d: misdelivered packet [rank %d -> rank %d]\n",
                             __FILE__, __LINE__, hdr->hdr_common.hdr_src, hdr->hdr_common.hdr_dst));
                return;                                                                                
            } 
            ep = (mca_pml_dr_endpoint_t*)ompi_pointer_array_get_item(&mca_pml_dr.endpoints, hdr->hdr_common.hdr_src);
            assert(ep != NULL);                                                                        
            
            /* seq_recvs protected by matching lock */
            if(ompi_seq_tracker_check_duplicate(&ep->seq_recvs, hdr->hdr_common.hdr_vid)) {
                MCA_PML_DR_DEBUG(0,(0, "%s:%d: acking duplicate fragment\n", __FILE__, __LINE__));
                mca_pml_dr_recv_frag_ack(btl,
                                         ep->bml_endpoint,
                                         &hdr->hdr_common,
                                         hdr->hdr_frag.hdr_src_ptr.pval,
                                         ~(uint64_t) 0, 0);
            } else {
                ompi_comm = ompi_comm_lookup(hdr->hdr_common.hdr_ctx);                                     
                if(NULL == ompi_comm) { 
                    MCA_PML_DR_DEBUG(0,(0, "%s:%d: the world as we know it is bad\n", __FILE__, __LINE__));
                    orte_errmgr.abort();
                }
                comm = (mca_pml_dr_comm_t*)ompi_comm->c_pml_comm;                                          
                assert(hdr->hdr_common.hdr_src < ompi_pointer_array_get_size(&comm->sparse_procs));        
                proc = (mca_pml_dr_comm_proc_t*)ompi_pointer_array_get_item(&comm->sparse_procs, hdr->hdr_common.hdr_src);          
                assert(proc != NULL);                                                                      
                assert(ep == proc->pml_endpoint); 
                
                recvreq = (mca_pml_dr_recv_request_t*)hdr->hdr_frag.hdr_dst_ptr.pval;
                mca_pml_dr_recv_request_progress(recvreq,btl,segments,des->des_dst_cnt);
            }
            break;

        }
    case MCA_PML_DR_HDR_TYPE_FRAG_ACK:
        {
            MCA_PML_DR_HDR_VALIDATE_ACK(do_csum, hdr, mca_pml_dr_ack_hdr_t);
            mca_pml_dr_send_request_frag_ack(btl, &hdr->hdr_ack);
            break;
        }
    default:
        MCA_PML_DR_DEBUG(0,(0, "%s:%d: dropping unknown header type\n", __FILE__,__LINE__));
        break;
    }
}
                                                                                                                      

/**
 * Try and match the incoming message fragment to the list of
 * "wild" receives
 *
 * @param hdr Matching data from recived fragment (IN)
 *
 * @param pml_comm Pointer to the communicator structure used for
 * matching purposes. (IN)
 *
 * @return Matched receive
 *
 * This routine assumes that the appropriate matching locks are
 * set by the upper level routine.
 */

#define MCA_PML_DR_CHECK_WILD_RECEIVES_FOR_MATCH(hdr,comm,proc,return_match) \
do { \
    /* local parameters */ \
    opal_list_t* wild_receives = &comm->wild_receives; \
    mca_pml_dr_recv_request_t *wild_recv; \
    int frag_tag,recv_tag; \
 \
    /* initialization */ \
    frag_tag=hdr->hdr_tag; \
 \
    /* \
     * Loop over the wild irecvs - no need to lock, the upper level \
     * locking is protecting from having other threads trying to \
     * change this list. \
     */ \
    for(wild_recv = (mca_pml_dr_recv_request_t *)  \
            opal_list_get_first(wild_receives); \
            wild_recv != (mca_pml_dr_recv_request_t *) \
            opal_list_get_end(wild_receives); \
            wild_recv = (mca_pml_dr_recv_request_t *)  \
            ((opal_list_item_t *)wild_recv)->opal_list_next) { \
 \
        recv_tag = wild_recv->req_recv.req_base.req_tag; \
        if (  \
                /* exact tag match */ \
                (frag_tag == recv_tag) || \
                /* wild tag match - negative tags (except for \
                 * OMPI_ANY_TAG) are reserved for internal use, and will \
                 * not be matched with OMPI_ANY_TAG */ \
                ( (recv_tag == OMPI_ANY_TAG) && (0 <= frag_tag) )  ) \
 \
        { \
            /* \
             * Mark that this is the matching irecv, and go to process it. \
             */ \
            return_match = wild_recv; \
 \
            /* remove this irecv from the postd wild ireceive list */ \
            opal_list_remove_item(wild_receives, \
                    (opal_list_item_t *)wild_recv); \
\
            /* found match - no need to continue */ \
            break; \
        } \
    } \
} while(0)


/**
 * Try and match the incoming message fragment to the list of
 * "specific" receives
 *
 * @param hdr Matching data from recived fragment (IN)
 *
 * @param comm Pointer to the communicator structure used for
 * matching purposes. (IN)
 *
 * @return Matched receive
 *
 * This routine assumes that the appropriate matching locks are
 * set by the upper level routine.
 */
#define MCA_PML_DR_CHECK_SPECIFIC_RECEIVES_FOR_MATCH(hdr,comm,proc,return_match) \
do { \
    /* local variables */ \
    opal_list_t* specific_receives = &proc->specific_receives; \
    mca_pml_dr_recv_request_t *specific_recv; \
    int recv_tag,frag_tag; \
 \
    /* initialization */ \
    frag_tag=hdr->hdr_tag; \
 \
    /* \
     * Loop over the specific irecvs. \
     */ \
    for(specific_recv = (mca_pml_dr_recv_request_t *)  \
            opal_list_get_first(specific_receives); \
            specific_recv != (mca_pml_dr_recv_request_t *) \
            opal_list_get_end(specific_receives); \
            specific_recv = (mca_pml_dr_recv_request_t *)  \
            ((opal_list_item_t *)specific_recv)->opal_list_next) { \
        /* \
         * Check for a match \
         */ \
        recv_tag = specific_recv->req_recv.req_base.req_tag; \
        if ( (frag_tag == recv_tag) || \
             ( (recv_tag == OMPI_ANY_TAG) && (0 <= frag_tag) ) ) { \
 \
            /* \
             * Match made \
             */ \
            return_match = specific_recv; \
 \
            /* remove descriptor from posted specific ireceive list */ \
            opal_list_remove_item(specific_receives, \
                    (opal_list_item_t *)specific_recv); \
 \
            break; \
        } \
    } \
} while(0)

/**
 * Try and match the incoming message fragment to the list of
 * "wild" receives and "specific" receives.  Used when both types
 * of receives have been posted,  i.e. when we need to coordinate
 * between multiple lists to make sure ordered delivery occurs.
 *
 * @param hdr Matching data from recived fragment (IN)
 *
 * @param comm Pointer to the communicator structure used for
 * matching purposes. (IN)
 *
 * @return Matched receive
 *
 * This routine assumes that the appropriate matching locks are
 * set by the upper level routine.
 */

#define MCA_PML_DR_CHECK_SPECIFIC_AND_WILD_RECEIVES_FOR_MATCH( \
    hdr,comm,proc,return_match) \
do {  \
    /* local variables */  \
    mca_pml_dr_recv_request_t *specific_recv, *wild_recv; \
    mca_pml_sequence_t wild_recv_seq, specific_recv_seq;  \
    int frag_tag, wild_recv_tag, specific_recv_tag;  \
  \
    /* initialization */  \
    frag_tag=hdr->hdr_tag;  \
  \
    /*  \
     * We know that when this is called, both specific and wild irecvs  \
     *  have been posted.  \
     */  \
    specific_recv = (mca_pml_dr_recv_request_t *)   \
            opal_list_get_first(&(proc)->specific_receives);  \
    wild_recv = (mca_pml_dr_recv_request_t *)  \
            opal_list_get_first(&comm->wild_receives);  \
  \
    specific_recv_seq = specific_recv->req_recv.req_base.req_sequence;  \
    wild_recv_seq = wild_recv->req_recv.req_base.req_sequence;  \
  \
    while (true) {  \
        if (wild_recv_seq < specific_recv_seq) {  \
            /*  \
             * wild recv is earlier than the specific one.  \
             */  \
            /*  \
             * try and match  \
             */  \
            wild_recv_tag = wild_recv->req_recv.req_base.req_tag;  \
            if ( (frag_tag == wild_recv_tag) ||  \
                 ( (wild_recv_tag == OMPI_ANY_TAG) && (0 <= frag_tag) ) ) {  \
                /*  \
                 * Match made  \
                 */  \
                return_match=wild_recv;  \
  \
                /* remove this recv from the wild receive queue */  \
                opal_list_remove_item(&comm->wild_receives,  \
                        (opal_list_item_t *)wild_recv);  \
                break;  \
            }  \
  \
            /*  \
             * No match, go to the next.  \
             */  \
            wild_recv=(mca_pml_dr_recv_request_t *)  \
                ((opal_list_item_t *)wild_recv)->opal_list_next;  \
  \
            /*  \
             * If that was the last wild one, just look at the  \
             * rest of the specific ones.  \
             */  \
            if (wild_recv == (mca_pml_dr_recv_request_t *)  \
                    opal_list_get_end(&comm->wild_receives) )   \
            {   \
                MCA_PML_DR_CHECK_SPECIFIC_RECEIVES_FOR_MATCH(hdr, comm, proc, return_match);  \
                break;  \
            }  \
  \
            /*  \
             * Get the sequence number for this recv, and go  \
             * back to the top of the loop.  \
             */  \
            wild_recv_seq = wild_recv->req_recv.req_base.req_sequence;  \
  \
        } else {  \
            /*  \
             * specific recv is earlier than the wild one.  \
             */  \
            specific_recv_tag=specific_recv->req_recv.req_base.req_tag;  \
            if ( (frag_tag == specific_recv_tag) || \
                 ( (specific_recv_tag == OMPI_ANY_TAG) && (0<=frag_tag)) )   \
            {  \
                /*  \
                 * Match made  \
                 */  \
                return_match = specific_recv;  \
                /* remove descriptor from specific receive list */  \
                opal_list_remove_item(&(proc)->specific_receives,  \
                    (opal_list_item_t *)specific_recv);  \
                break; \
            }  \
  \
            /*  \
             * No match, go on to the next specific irecv.  \
             */  \
            specific_recv = (mca_pml_dr_recv_request_t *)  \
                ((opal_list_item_t *)specific_recv)->opal_list_next;  \
  \
            /*  \
             * If that was the last specific irecv, process the  \
             * rest of the wild ones.  \
             */  \
            if (specific_recv == (mca_pml_dr_recv_request_t *)  \
                    opal_list_get_end(&(proc)->specific_receives))  \
            {  \
                MCA_PML_DR_CHECK_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, return_match);  \
                break; \
            }  \
            /*  \
             * Get the sequence number for this recv, and go  \
             * back to the top of the loop.  \
             */ \
            specific_recv_seq = specific_recv->req_recv.req_base.req_sequence;  \
        }  \
    }  \
} while(0)


/*
 * Specialized matching routines for internal use only.
 */

static bool mca_pml_dr_check_cantmatch_for_match(
    opal_list_t *additional_matches,
    mca_pml_dr_comm_t* comm,
    mca_pml_dr_comm_proc_t *proc);


/**
 * RCS/CTS receive side matching
 *
 * @param hdr list of parameters needed for matching
 *                    This list is also embeded in frag,
 *                    but this allows to save a memory copy when
 *                    a match is made in this routine. (IN)
 * @param frag   pointer to receive fragment which we want
 *                    to match (IN/OUT).  If a match is not made,
 *                    hdr is copied to frag.
 * @param match_made  parameter indicating if we matched frag/
 *                    hdr (OUT)
 * @param additional_matches  if a match is made with frag, we
 *                    may be able to match fragments that previously
 *                    have arrived out-of-order.  If this is the
 *                    case, the associated fragment descriptors are
 *                    put on this list for further processing. (OUT)
 *
 * @return OMPI error code
 *
 * This routine is used to try and match a newly arrived message fragment
 *   to pre-posted receives.  The following assumptions are made
 *   - fragments are received out of order
 *   - for long messages, e.g. more than one fragment, a RTS/CTS algorithm
 *       is used.
 *   - 2nd and greater fragments include a receive descriptor pointer
 *   - fragments may be dropped
 *   - fragments may be corrupt
 *   - this routine may be called simultaneously by more than one thread
 */
bool mca_pml_dr_recv_frag_match(
    mca_pml_dr_comm_t* comm,
    mca_pml_dr_comm_proc_t *proc,
    mca_btl_base_module_t *btl, 
    mca_pml_dr_match_hdr_t *hdr,
    mca_btl_base_segment_t* segments,
    size_t num_segments)
{
    /* local variables */
    uint16_t next_msg_seq_expected, frag_msg_seq;
    mca_pml_dr_recv_request_t *match = NULL;
    bool additional_match=false;
    opal_list_t additional_matches;
    ompi_proc_t* ompi_proc = proc->ompi_proc;
    int rc;
    uint32_t csum;
    mca_pml_dr_endpoint_t* ep = (mca_pml_dr_endpoint_t*) proc->pml_endpoint;
    bool do_csum = mca_pml_dr.enable_csum && 
        (btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM);
    
    /* source sequence number */
    frag_msg_seq = hdr->hdr_seq;

    /* get next expected message sequence number - if threaded
     * run, lock to make sure that if another thread is processing 
     * a frag from the same message a match is made only once.
     * Also, this prevents other posted receives (for a pair of
     * end points) from being processed, and potentially "loosing"
     * the fragment.
     */
    OPAL_THREAD_LOCK(&comm->matching_lock);

    /* get sequence number of next message that can be processed */
    next_msg_seq_expected = (uint16_t)proc->expected_sequence;
    if (frag_msg_seq == next_msg_seq_expected) {

        /*
         * This is the sequence number we were expecting,
         * so we can try matching it to already posted
         * receives.
         */

        /* We're now expecting the next sequence number. */
        (proc->expected_sequence)++;
rematch:

        /*
         * figure out what sort of matching logic to use, if need to
         *   look only at "specific" receives, or "wild" receives,
         *   or if we need to traverse both sets at the same time.
         */
        if (opal_list_get_size(&proc->specific_receives) == 0 ){
            /*
             * There are only wild irecvs, so specialize the algorithm.
             */
            MCA_PML_DR_CHECK_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
    
        } else if (opal_list_get_size(&comm->wild_receives) == 0 ) {
            /*
             * There are only specific irecvs, so specialize the algorithm.
             */
            MCA_PML_DR_CHECK_SPECIFIC_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
        } else {
            /*
             * There are some of each.
             */
            MCA_PML_DR_CHECK_SPECIFIC_AND_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
        }

        /* if match found, process data */
        if (match) {

            /*
             * update delivered sequence number information, if needed.
             */
            MCA_PML_DR_RECV_REQUEST_MATCHED(match,comm,proc,hdr);
            if( (match->req_recv.req_base.req_type == MCA_PML_REQUEST_PROBE) ) {

                /* complete the probe */
                mca_pml_dr_recv_request_matched_probe(match,btl,segments,num_segments);

                /* retry the matchh */
                match = NULL;
                goto rematch;
            }
        } else {

            /* if no match found, verify csum, if pass place on unexpected queue */
            mca_pml_dr_recv_frag_t* frag;
            MCA_PML_DR_RECV_FRAG_ALLOC(frag, rc);
            if(OMPI_SUCCESS != rc) {
                OPAL_THREAD_UNLOCK(&comm->matching_lock);
                return false;
            }
            MCA_PML_DR_RECV_FRAG_INIT(frag,ompi_proc,hdr,segments,num_segments,btl,csum);
            if(do_csum && csum != hdr->hdr_csum) { 
                mca_pml_dr_recv_frag_ack(btl, 
                                         (mca_bml_base_endpoint_t*)ompi_proc->proc_bml, 
                    &hdr->hdr_common, hdr->hdr_src_ptr.pval, 0, 0);
                MCA_PML_DR_DEBUG(0,(0, "%s:%d: received corrupted data 0x%08x != 0x%08x (segments %lu length %lu)\n", 
                             __FILE__, __LINE__, csum, hdr->hdr_csum, (unsigned long)num_segments,
                             (unsigned long)(segments[0].seg_len - mca_pml_dr_hdr_size(hdr->hdr_common.hdr_type))));
                MCA_PML_DR_RECV_FRAG_RETURN(frag);
                OPAL_THREAD_UNLOCK(&comm->matching_lock);
                return false;
            }
            opal_list_append( &proc->unexpected_frags, (opal_list_item_t *)frag );
        }

        /* 
         * Now that new message has arrived, check to see if
         *   any fragments on the c_c_frags_cant_match list
         *   may now be used to form new matchs
         */
        if (0 < opal_list_get_size(&proc->frags_cant_match)) {
            additional_match = mca_pml_dr_check_cantmatch_for_match(&additional_matches,comm,proc);
        }
        
    } else {

        /*
         * This message comes after the next expected, so it
         * is ahead of sequence.  If passes csum save it for later.
         */

        mca_pml_dr_recv_frag_t* frag;
        MCA_PML_DR_RECV_FRAG_ALLOC(frag, rc);
        if(OMPI_SUCCESS != rc) {
            OPAL_THREAD_UNLOCK(&comm->matching_lock);
            return false;
        }
        MCA_PML_DR_RECV_FRAG_INIT(frag,ompi_proc,hdr,segments,num_segments,btl,csum);
        if(do_csum && csum != hdr->hdr_csum) { 
            mca_pml_dr_recv_frag_ack(btl, 
                                     (mca_bml_base_endpoint_t*)ompi_proc->proc_bml, 
                                     &hdr->hdr_common, hdr->hdr_src_ptr.pval, 0, 0);
            MCA_PML_DR_DEBUG(0,(0, "%s:%d: received corrupted data 0x%08x != 0x%08x\n", 
                         __FILE__, __LINE__, csum, hdr->hdr_csum));
            MCA_PML_DR_RECV_FRAG_RETURN(frag);
            OPAL_THREAD_UNLOCK(&comm->matching_lock);
            return false;
        }
        opal_list_append(&proc->frags_cant_match, (opal_list_item_t *)frag);
    }

    
    ompi_seq_tracker_insert(&ep->seq_recvs, hdr->hdr_common.hdr_vid);
    OPAL_THREAD_UNLOCK(&comm->matching_lock);
    
    /* release matching lock before processing fragment */
    if(match != NULL) {
        mca_pml_dr_recv_request_progress(match,btl,segments,num_segments);
    } 

    /* if buffered a short message - go ahead and ack */
    else if (hdr->hdr_common.hdr_type == MCA_PML_DR_HDR_TYPE_MATCH) { 
        MCA_PML_DR_DEBUG(1,(0, "%s:%d: received short message, acking now vfrag id: %d\n", 
                            __FILE__, __LINE__, hdr->hdr_common.hdr_vid));
            
        mca_pml_dr_recv_frag_ack(btl,
                                 (mca_bml_base_endpoint_t*)ompi_proc->proc_bml, 
                                 &hdr->hdr_common, hdr->hdr_src_ptr.pval, 1, 0);
    }

    if(additional_match) {
        opal_list_item_t* item;
        while(NULL != (item = opal_list_remove_first(&additional_matches))) {
            mca_pml_dr_recv_frag_t* frag = (mca_pml_dr_recv_frag_t*)item;
            mca_pml_dr_recv_request_progress(frag->request,frag->btl,frag->segments,frag->num_segments);
            MCA_PML_DR_RECV_FRAG_RETURN(frag);
        }
    }
    return (match != NULL);
}



void mca_pml_dr_recv_frag_ack(
    mca_btl_base_module_t* btl,
    mca_bml_base_endpoint_t* endpoint,
    mca_pml_dr_common_hdr_t* hdr,
    void *src_ptr,
    uint64_t mask, 
    uint16_t len)
{
    mca_btl_base_descriptor_t* des;
    mca_bml_base_btl_t* bml_btl;
    mca_pml_dr_recv_frag_t* frag;
    mca_pml_dr_ack_hdr_t* ack;
    int rc;
    bool do_csum;
    
    /* use the same BTL for ACK's makes failover SANE */
    bml_btl = mca_bml_base_btl_array_find(&endpoint->btl_eager,
                                          btl);
    
    do_csum = mca_pml_dr.enable_csum && 
        (bml_btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM); 

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &des, MCA_BTL_NO_ORDER,
            sizeof(mca_pml_dr_ack_hdr_t), MCA_BTL_DES_FLAGS_PRIORITY);
    if(NULL == des) {
        goto retry;
    }

    /* fill out header */
    ack = (mca_pml_dr_ack_hdr_t*)des->des_src->seg_addr.pval;
    ack->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_ACK | hdr->hdr_type;
    ack->hdr_common.hdr_flags = 0;
    ack->hdr_common.hdr_src = hdr->hdr_dst;
    ack->hdr_common.hdr_dst = hdr->hdr_src;
    ack->hdr_common.hdr_vid = hdr->hdr_vid;
    ack->hdr_common.hdr_ctx = hdr->hdr_ctx;
    ack->hdr_vlen = len;
    ack->hdr_vmask = mask;
    ack->hdr_src_ptr.pval = src_ptr;
    assert(ack->hdr_src_ptr.pval);
    ack->hdr_dst_ptr.pval = NULL;
    ack->hdr_common.hdr_csum = (uint16_t)(do_csum ? 
                                opal_csum(ack, sizeof(mca_pml_dr_ack_hdr_t)) :
                                OPAL_CSUM_ZERO);
    
    /* initialize descriptor */
    des->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    des->des_cbfunc = mca_pml_dr_ctl_completion;

    MCA_PML_DR_DEBUG(1,(0, "%s:%d: sending ack, vfrag ID %d",
                        __FILE__, __LINE__, hdr->hdr_vid));
    
    rc = mca_bml_base_send(bml_btl, des, MCA_BTL_TAG_PML);
    
    if(rc != OMPI_SUCCESS) {
        mca_bml_base_free(bml_btl, des);
        goto retry;
    }
    
    return;

    /* queue request to retry later */
retry:
    MCA_PML_DR_RECV_FRAG_ALLOC(frag,rc);
    /* frag->hdr.hdr_match = *hdr; */
    frag->num_segments = 0;
    opal_list_append(&mca_pml_dr.acks_pending, (opal_list_item_t*)frag);
}



/**
 * Scan the list of frags that came in ahead of time to see if any
 * can be processed at this time.  If they can, try and match the
 * frags.
 *
 * @param additional_matches List to hold new matches with fragments
 * from the c_frags_cant_match list. (IN/OUT)
 *
 * @param pml_comm Pointer to the communicator structure used for
 * matching purposes. (IN)
 *
 * This routine assumes that the appropriate matching locks are
 * set by the upper level routine.
 */

static bool mca_pml_dr_check_cantmatch_for_match(
    opal_list_t *additional_matches,
    mca_pml_dr_comm_t* comm,
    mca_pml_dr_comm_proc_t *proc)
{
    /* local parameters */
    int match_found;
    uint16_t next_msg_seq_expected, frag_seq;
    mca_pml_dr_recv_frag_t *frag;
    bool match_made = false;

    /*
     * Loop over all the out of sequence messages.  No ordering is assumed
     * in the c_frags_cant_match list.
     */

    match_found = 1;
    while ((0 < opal_list_get_size(&proc->frags_cant_match)) && match_found) {

        /* initialize match flag for this search */
        match_found = 0;

        /* get sequence number of next message that can be processed */
        next_msg_seq_expected = proc->expected_sequence;

        /* search the list for a fragment from the send with sequence
         * number next_msg_seq_expected
         */
        for(frag = (mca_pml_dr_recv_frag_t *) 
            opal_list_get_first(&proc->frags_cant_match);
            frag != (mca_pml_dr_recv_frag_t *)
            opal_list_get_end(&proc->frags_cant_match);
            frag = (mca_pml_dr_recv_frag_t *) 
            opal_list_get_next(frag))
        {
            /*
             * If the message has the next expected seq from that proc...
             */
            frag_seq=frag->hdr.hdr_match.hdr_seq;
            if (frag_seq == next_msg_seq_expected) {
                mca_pml_dr_recv_request_t *match = NULL;
                mca_pml_dr_match_hdr_t* hdr = &frag->hdr.hdr_match;

                /* We're now expecting the next sequence number. */
                (proc->expected_sequence)++;

                /* signal that match was made */
                match_found = 1;

                /*
                 * remove frag from list
                 */
                opal_list_remove_item(&proc->frags_cant_match,
                        (opal_list_item_t *)frag);

rematch:
                /*
                 * figure out what sort of matching logic to use, if need to
                 *   look only at "specific" receives, or "wild" receives,
                 *   or if we need to traverse both sets at the same time.
                 */
                proc = (mca_pml_dr_comm_proc_t*)ompi_pointer_array_get_item(&comm->sparse_procs, 
                                                                            hdr->hdr_common.hdr_src);
                
                if (opal_list_get_size(&proc->specific_receives) == 0 ) {
                    /*
                     * There are only wild irecvs, so specialize the algorithm.
                     */
                    MCA_PML_DR_CHECK_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
                } else if (opal_list_get_size(&comm->wild_receives) == 0 ) {
                    /*
                     * There are only specific irecvs, so specialize the algorithm.
                     */
                    MCA_PML_DR_CHECK_SPECIFIC_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
                } else {
                    /*
                     * There are some of each.
                     */
                    MCA_PML_DR_CHECK_SPECIFIC_AND_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, match);

                }

                /* if match found, process data */
                if (match) {

                    /*
                     * If this was a probe need to queue fragment on unexpected list
                     */
                    if( (match->req_recv.req_base.req_type == MCA_PML_REQUEST_PROBE) ) {

                        /* complete the probe */
                        mca_pml_dr_recv_request_matched_probe(match,frag->btl,frag->segments,frag->num_segments);

                        /* retry the match */
                        match = NULL;
                        goto rematch;

                    } else {

                        /* associate the receive descriptor with the fragment
                         * descriptor */
                        frag->request=match;
                        match->req_proc = proc;
                        match->req_endpoint = (mca_pml_dr_endpoint_t*)proc->ompi_proc->proc_pml;
                        MCA_PML_DR_DEBUG(10, (0, "%s:%d: adding endpoint %p match %p\n", 
                              __FILE__, __LINE__, (void*)proc->ompi_proc->proc_pml, (void*)match->req_endpoint));
        
                        /* add this fragment descriptor to the list of
                         * descriptors to be processed later
                         */
                        if(match_made == false) {
                            match_made = true;
                            OBJ_CONSTRUCT(additional_matches, opal_list_t);
                        }
                        MCA_PML_DR_RECV_REQUEST_MATCHED(match,comm,proc,&frag->hdr.hdr_match);
                        opal_list_append(additional_matches, (opal_list_item_t *)frag);
                    }

                } else {
    
                    /* if no match found, place on unexpected queue */
                    opal_list_append( &proc->unexpected_frags, (opal_list_item_t *)frag);

                }

                /* c_frags_cant_match is not an ordered list, so exit loop
                 * and re-start search for next sequence number */
                break;

            } /* end if (frag_seq == next_msg_seq_expected) */
            
        } /* end for (frag) loop */
        
    } /* end while loop */

    return match_made;
}


