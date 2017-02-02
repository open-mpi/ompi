/*
 * Copyright (c) 2015-2016 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/dss/dss_types.h"
#include "opal/util/output.h"
#include "opal/mca/event/event.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_errno.h>
#include <rdma/fi_tagged.h>

#include "rml_ofi.h"


static void ofi_req_cons(orte_rml_ofi_request_t *ptr)
{
    OBJ_CONSTRUCT(&ptr->pkt_list, opal_list_t);
}
static void ofi_req_des(orte_rml_ofi_request_t *ptr)
{
    OPAL_LIST_DESTRUCT(&ptr->pkt_list);
}
OBJ_CLASS_INSTANCE(orte_rml_ofi_request_t,
                   opal_object_t,
                   ofi_req_cons, ofi_req_des);


static void ofi_send_req_cons(ofi_send_request_t *ptr)
{
    OBJ_CONSTRUCT(&ptr->send, orte_rml_send_t);
}
OBJ_CLASS_INSTANCE(ofi_send_request_t,
                   opal_object_t,
                   ofi_send_req_cons, NULL);

OBJ_CLASS_INSTANCE(orte_rml_ofi_send_pkt_t,
                    opal_list_item_t,
                    NULL, NULL);

OBJ_CLASS_INSTANCE(orte_rml_ofi_recv_pkt_t,
                    opal_list_item_t,
                    NULL, NULL);


static void ofi_recv_msg_queue_cons(ofi_recv_msg_queue_t *ptr)
{
    ptr->msgid = 0;
    ptr->tot_pkts = 1;
    ptr->pkt_recd = 0;
    OBJ_CONSTRUCT(&ptr->pkt_list, opal_list_t);
}
static void ofi_recv_msg_queue_des(ofi_recv_msg_queue_t *ptr)
{
    OPAL_LIST_DESTRUCT(&ptr->pkt_list);
}
OBJ_CLASS_INSTANCE(ofi_recv_msg_queue_t,
                   opal_list_item_t,
                   ofi_recv_msg_queue_cons, ofi_recv_msg_queue_des);


static void send_self_exe(int fd, short args, void* data)
{
    orte_self_send_xfer_t *xfer = (orte_self_send_xfer_t*)data;

    opal_output_verbose(1, orte_rml_base_framework.framework_output,
        "%s rml_send_to_self ofi callback executing for tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), xfer->tag);

    /* execute the send callback function - note that
     * send-to-self always returns a SUCCESS status
     */
    if (NULL != xfer->iov) {
        if (NULL != xfer->cbfunc.iov) {
            /* non-blocking iovec send */
            xfer->cbfunc.iov(ORTE_SUCCESS, ORTE_PROC_MY_NAME, xfer->iov, xfer->count,
                             xfer->tag, xfer->cbdata);
        }
    } else if (NULL != xfer->buffer) {
        if (NULL != xfer->cbfunc.buffer) {
            /* non-blocking buffer send */
            xfer->cbfunc.buffer(ORTE_SUCCESS, ORTE_PROC_MY_NAME, xfer->buffer,
                                xfer->tag, xfer->cbdata);
        }
    } else {
        /* should never happen */
        abort();
    }

    /* cleanup the memory */
    OBJ_RELEASE(xfer);
}

/** Send callback */
/* [Desc] This is called from the progress fn when a send completion
** is received in the cq
** wc [in]      : the completion queue data entry
** ofi_send_req [in]:  ofi send request with the send msg and callback
*/
int orte_rml_ofi_send_callback(struct fi_cq_data_entry *wc,
                           orte_rml_ofi_request_t* ofi_req)
{
   orte_rml_ofi_send_pkt_t *ofi_send_pkt, *next;
   opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s orte_rml_ofi_send_callback called, completion count = %d, msgid = %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ofi_req->completion_count, ofi_req->hdr.msgid);
    assert(ofi_req->completion_count > 0);
    ofi_req->completion_count--;
    if ( 0 == ofi_req->completion_count )  {
        // call the callback fn of the sender
        ofi_req->send->status = ORTE_SUCCESS;
        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s calling ORTE_RML_SEND_COMPLETE macro for msgid = %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ofi_req->hdr.msgid);
        ORTE_RML_SEND_COMPLETE(ofi_req->send);
        OPAL_LIST_FOREACH_SAFE(ofi_send_pkt, next, &ofi_req->pkt_list, orte_rml_ofi_send_pkt_t) {
                            free( ofi_send_pkt->data);
                            ofi_send_pkt->pkt_size=0;
                            opal_list_remove_item(&ofi_req->pkt_list, &ofi_send_pkt->super);
                              opal_output_verbose(10, orte_rml_base_framework.framework_output,
                             "%s  Removed pkt from list ",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
                            OBJ_RELEASE(ofi_send_pkt);
                              opal_output_verbose(10, orte_rml_base_framework.framework_output,
                             "%s  Released packet ",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
                        }
        free(ofi_req->data_blob);
        OBJ_RELEASE(ofi_req);
    }

    // [TODO]  need to check for error before returning success
    return ORTE_SUCCESS;
}

/** Error callback */
/* [Desc] This is called from the progress fn when a send completion
** is received in the cq
** wc [in]      : the completion queue data entry
** ofi_send_req [in]:  ofi send request with the send msg and callback
*/
int orte_rml_ofi_error_callback(struct fi_cq_err_entry *error,
                           orte_rml_ofi_request_t* ofi_req)
{
    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s orte_rml_ofi_error_callback called ",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
    switch(error->err) {
        default:
            /* call the send-callback fn with error and return, also return failure status */
            ofi_req->send->status = ORTE_ERR_CONDUIT_SEND_FAIL;
            ORTE_RML_SEND_COMPLETE(ofi_req->send);
    }
    return ORTE_SUCCESS;
}

/** Recv handler */
/* [Desc] This is called from the progress fn when a recv completion
** is received in the cq
** wc [in]      : the completion queue data entry */
int orte_rml_ofi_recv_handler(struct fi_cq_data_entry *wc, uint8_t ofi_prov_id)
{
    orte_rml_ofi_msg_header_t msg_hdr;
    uint32_t msglen, datalen = 0;
    char *data, *totdata, *nextpkt;
    ofi_recv_msg_queue_t *recv_msg_queue, *new_msg;
    orte_rml_ofi_recv_pkt_t *ofi_recv_pkt, *new_pkt, *next;
    bool msg_in_queue = false;

    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s orte_rml_ofi_recv_handler called ",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
    /*copy the header and data from buffer and pass it on
    ** since this is the ofi_prov recv buffer don't want it to be released as
    ** considering re-using it, so for now copying to newly allocated *data
    ** the *data will be released by orte_rml_base functions */

    memcpy(&msg_hdr,wc->buf,sizeof(orte_rml_ofi_msg_header_t));
    msglen = wc->len - sizeof(orte_rml_ofi_msg_header_t);
    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s Received packet -> msg id = %d wc->len = %lu, msglen = %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), msg_hdr.msgid, wc->len, msglen );
    data = (char *)malloc(msglen);
    memcpy(data,((char *)wc->buf+sizeof(orte_rml_ofi_msg_header_t)),msglen);
    opal_output_verbose(15, orte_rml_base_framework.framework_output,
                         "%s header info of received packet -> cur_pkt_num = %d, tot_pkts = %d ",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), msg_hdr.cur_pkt_num, msg_hdr.tot_pkts );
     /* To accomodate message bigger than recv buffer size,
       check if current message is in multiple blocks and append them before sending it to RML */
    if ( msg_hdr.tot_pkts == 1) {
        /* Since OFI is point-to-point, no need to check if the intended destination is me
         send to RML */
        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s Posting Recv for msgid %d, from peer - %s , Tag = %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), msg_hdr.msgid, ORTE_NAME_PRINT(&msg_hdr.origin),msg_hdr.tag );
        ORTE_RML_POST_MESSAGE(&msg_hdr.origin, msg_hdr.tag, msg_hdr.seq_num,data,msglen);
    } else {
        msg_in_queue = false;
        new_pkt = OBJ_NEW(orte_rml_ofi_recv_pkt_t);
        new_pkt->cur_pkt_num = msg_hdr.cur_pkt_num;
        new_pkt->pkt_size = msglen;
        new_pkt->data = data;
        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s Just beofe checking if this message-pkt is already in queue. msgid-%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), msg_hdr.msgid );
        /* check if the queue has the [msgid,sender] entry */
        OPAL_LIST_FOREACH(recv_msg_queue, &orte_rml_ofi.recv_msg_queue_list, ofi_recv_msg_queue_t) {
             opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s Checking msgid-%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv_msg_queue->msgid );
            if( (recv_msg_queue->msgid == msg_hdr.msgid) && (recv_msg_queue->sender.jobid == msg_hdr.origin.jobid)
                                                         && (recv_msg_queue->sender.vpid == msg_hdr.origin.vpid) ) {
                opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s Found Msg entry in queue for msgid %d, sender jobid=%d, sender vpid=%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv_msg_queue->msgid, recv_msg_queue->sender.jobid, recv_msg_queue->sender.vpid);
                msg_in_queue = true;

                opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s  msgid %d, tot_pkts=%d, opal_list_get_size()=%lu,total pkt_recd=%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv_msg_queue->msgid, recv_msg_queue->tot_pkts,
                         opal_list_get_size(&recv_msg_queue->pkt_list), recv_msg_queue->pkt_recd );
                if( recv_msg_queue->tot_pkts  == (recv_msg_queue->pkt_recd +1) ) {
                    /* all packets received for this message - post message to rml and remove this from queue */
                    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s All packets recd for msgid %d, tot_pkts=%d, opal_list_get_size()=%lu,total pkt_recd=%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv_msg_queue->msgid, recv_msg_queue->tot_pkts,
                         opal_list_get_size(&recv_msg_queue->pkt_list), recv_msg_queue->pkt_recd );
                    totdata = NULL;
                    datalen = 0;
                    OPAL_LIST_FOREACH(ofi_recv_pkt, &recv_msg_queue->pkt_list, orte_rml_ofi_recv_pkt_t) {
                         opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s Adding data for packet %d, pktlength = %lu, cumulative datalen so far = %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ofi_recv_pkt->cur_pkt_num, ofi_recv_pkt->pkt_size, datalen );
                        if (0 == datalen) {
                            totdata = (char *)malloc(ofi_recv_pkt->pkt_size);
                            if( totdata == NULL) {
                                opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                        "%s Error: malloc failed for msgid %d",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),recv_msg_queue->msgid );
                                    return 1;  //[TODO: error-handling needs to be implemented
                            }
                            memcpy(totdata,ofi_recv_pkt->data,ofi_recv_pkt->pkt_size);

                        } else {
                            totdata = realloc(totdata,datalen+ofi_recv_pkt->pkt_size);
                            if (NULL != totdata ) {
                                memcpy((totdata+datalen),ofi_recv_pkt->data,ofi_recv_pkt->pkt_size);
                            } else {
                                    opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                        "%s Error: realloc failed for msgid %d, from sender jobid=%d, sender vpid=%d",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv_msg_queue->msgid, recv_msg_queue->sender.jobid,
                                        recv_msg_queue->sender.vpid);
                                    return 1;  //[TODO: error-handling needs to be implemented
                            }
                        }
                        datalen += ofi_recv_pkt->pkt_size;
                        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                                     "%s packet %d done, datalen = %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ofi_recv_pkt->cur_pkt_num,datalen);
                    }
                    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                                     "%s Adding leftover data recd, datalen = %d, new_pkt->pkt_size = %lu",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), datalen, new_pkt->pkt_size);
                    //add the last packet
                    totdata =realloc(totdata,datalen+new_pkt->pkt_size);
                    if( NULL != totdata ) {
                        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                                     "%s Realloc completed for leftover data recd, datalen = %d, new->pkt->pkt_size = %lu",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), datalen, new_pkt->pkt_size);
                        nextpkt = totdata+datalen;
                        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                                     "%s totdata = %p,nextpkt = %p ",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), totdata, nextpkt);
                        memcpy(nextpkt,new_pkt->data,new_pkt->pkt_size);
                        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                                     "%s memcpy completed for leftover data recd, datalen = %d, new->pkt->pkt_size = %lu",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), datalen, new_pkt->pkt_size);
                        datalen += new_pkt->pkt_size;
                        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s Posting Recv for msgid %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), msg_hdr.msgid );
                        ORTE_RML_POST_MESSAGE(&msg_hdr.origin, msg_hdr.tag, msg_hdr.seq_num,totdata,datalen);\

                        // free the pkts
                        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s msgid %d - posting recv completed, freeing packets",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), msg_hdr.msgid );
                        OPAL_LIST_FOREACH_SAFE(ofi_recv_pkt, next, &recv_msg_queue->pkt_list, orte_rml_ofi_recv_pkt_t) {
                            free( ofi_recv_pkt->data);
                            opal_output_verbose(10, orte_rml_base_framework.framework_output,
                             "%s  freed data for packet %d",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ofi_recv_pkt->cur_pkt_num );
                            ofi_recv_pkt->pkt_size=0;
                            opal_list_remove_item(&recv_msg_queue->pkt_list, &ofi_recv_pkt->super);
                              opal_output_verbose(10, orte_rml_base_framework.framework_output,
                             "%s  Removed pkt from list ",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
                            OBJ_RELEASE(ofi_recv_pkt);
                              opal_output_verbose(10, orte_rml_base_framework.framework_output,
                             "%s  Released packet ",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
                        }
                        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s  freeing packets completed",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
                        //free the msg from the queue-list
                        opal_list_remove_item(&orte_rml_ofi.recv_msg_queue_list,&recv_msg_queue->super);
                        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s  Successfully removed msg from queue",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
                        OBJ_RELEASE(recv_msg_queue);
                    } else {
                        opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                        "%s Error: realloc failed for msgid %d, from sender jobid=%d, sender vpid=%d",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv_msg_queue->msgid, recv_msg_queue->sender.jobid,
                                        recv_msg_queue->sender.vpid);
                                    return 1;  //[TODO: error-handling needs to be implemented
                    }
                } else {
                    /* add this packet to the msg in the queue ordered by cur_pkt_num */
                    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s Adding packet to list, msgid %d, pkt - %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv_msg_queue->msgid, msg_hdr.cur_pkt_num );

                    bool pkt_added = false;
                    OPAL_LIST_FOREACH(ofi_recv_pkt, &recv_msg_queue->pkt_list, orte_rml_ofi_recv_pkt_t) {
                        if( msg_hdr.cur_pkt_num < ofi_recv_pkt->cur_pkt_num ) {
                            opal_list_insert_pos(&recv_msg_queue->pkt_list, (opal_list_item_t*)ofi_recv_pkt, &new_pkt->super);
                            recv_msg_queue->pkt_recd++;
                            pkt_added = true;
                            break;
                        }
                    }
                    if (!pkt_added) {
                        opal_list_append(&recv_msg_queue->pkt_list,&new_pkt->super);
                        recv_msg_queue->pkt_recd++;
                    }
                }
            }
            break;  //we found the msg or added it so exit out of the msg_queue loop
        }
        if( !msg_in_queue ) {
            /*add to the queue as this is the first packet for [msgid,sender] */
            new_msg = OBJ_NEW(ofi_recv_msg_queue_t);
            new_msg->msgid = msg_hdr.msgid;
            new_msg->sender = msg_hdr.origin;
            new_msg->tot_pkts = msg_hdr.tot_pkts;
            new_msg->pkt_recd = 1;
            opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s Adding first Msg entry in queue for msgid %d, sender jobid=%d, sender vpid=%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), new_msg->msgid, new_msg->sender.jobid, new_msg->sender.vpid);
            opal_list_append(&new_msg->pkt_list, &new_pkt->super);
            opal_list_append(&orte_rml_ofi.recv_msg_queue_list, &new_msg->super);

        }
    }
  return ORTE_SUCCESS;
}


static void send_msg(int fd, short args, void *cbdata)
{
    ofi_send_request_t *req = (ofi_send_request_t*)cbdata;
    orte_process_name_t *peer = &(req->send.dst);
    orte_rml_tag_t tag = req->send.tag;
    char *dest_ep_name, *pmix_key;
    size_t dest_ep_namelen = 0;
    int ret = OPAL_ERROR;
    uint32_t  total_packets;
    fi_addr_t dest_fi_addr;
    orte_rml_send_t *snd;
    orte_rml_recv_t *rcv;
    orte_self_send_xfer_t *xfer;
    orte_rml_ofi_request_t* ofi_send_req = OBJ_NEW( orte_rml_ofi_request_t );
    uint8_t ofi_prov_id = req->ofi_prov_id;
    orte_rml_ofi_send_pkt_t* ofi_msg_pkt;
    size_t datalen_per_pkt, hdrsize, data_in_pkt;  // the length of data in per packet excluding the header size
    orte_rml_ofi_peer_t* pr;
    uint64_t ui64;
    struct sockaddr_in* ep_sockaddr;
    int i, bytes;
    char *ptr;

    snd = OBJ_NEW(orte_rml_send_t);
    snd->dst = *peer;
    snd->origin = *ORTE_PROC_MY_NAME;
    snd->tag = tag;
    if (NULL != req->send.iov) {
        snd->iov = req->send.iov;
        snd->count = req->send.count;
        snd->cbfunc.iov = req->send.cbfunc.iov;
    } else {
        snd->buffer = req->send.buffer;
        snd->cbfunc.buffer = req->send.cbfunc.buffer;
    }
    snd->cbdata = req->send.cbdata;

    opal_output_verbose(1, orte_rml_base_framework.framework_output,
                         "%s send_msg_transport to peer %s at tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag);


    /* get the peer address by doing modex_receive      */
    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s calling OPAL_MODEX_RECV_STRING ", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
    // if dest is same as me then instead of doing lookup just populate the dest_ep_name
    /*if (!ORTE_PROC_IS_APP && peer->jobid == ORTE_PROC_MY_NAME->jobid && peer->vpid == ORTE_PROC_MY_NAME->vpid) {
        dest_ep_namelen = orte_rml_ofi.ofi_prov[ofi_prov_id].epnamelen;
        dest_ep_name = (char *)calloc(dest_ep_namelen,sizeof(char));
        memcpy( dest_ep_name, orte_rml_ofi.ofi_prov[ofi_prov_id].ep_name,dest_ep_namelen);
        opal_output_verbose(1, orte_rml_base_framework.framework_output,
                            "%s rml:ofi: send and dest are same so proceeding with cur provider ep_name ",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        ret = OPAL_SUCCESS;
    } else {*/
    if (ORTE_PROC_IS_APP ) {
            asprintf(&pmix_key,"%s%d",orte_rml_ofi.ofi_prov[ofi_prov_id].fabric_info->fabric_attr->prov_name,ofi_prov_id);
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                     "%s calling OPAL_MODEX_RECV_STRING for ORTE_PROC_APP peer - %s, key - %s ",
                      ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(peer),pmix_key );
            OPAL_MODEX_RECV_STRING(ret, pmix_key, peer , (uint8_t **) &dest_ep_name, &dest_ep_namelen);
            opal_output_verbose(10, orte_rml_base_framework.framework_output, "Returned from MODEX_RECV");
            opal_output_verbose(50, orte_rml_base_framework.framework_output,
                         "%s  Return value from OPAL_MODEX_RECV_STRING - %d, length returned - %lu",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ret, dest_ep_namelen);
            free(pmix_key);
    } else {
        opal_output_verbose(1, orte_rml_base_framework.framework_output,
                  "%s calling OPAL_MODEX_RECV_STRING for DAEMON peer %s",
                  ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(peer));
        if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, peer, ORTE_PROC_MY_NAME)) {
                 opal_output_verbose(1, orte_rml_base_framework.framework_output,
                             "%s rml_ofi_send_to_self at tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), tag);
            /* send to self is a tad tricky - we really don't want
             * to track the send callback function throughout the recv
             * process and execute it upon receipt as this would provide
             * very different timing from a non-self message. Specifically,
             * if we just retain a pointer to the incoming data
             * and then execute the send callback prior to the receive,
             * then the caller will think we are done with the data and
             * can release it. So we have to copy the data in order to
             * execute the send callback prior to receiving the message.
             *
             * In truth, this really is a better mimic of the non-self
             * message behavior. If we actually pushed the message out
             * on the wire and had it loop back, then we would receive
             * a new block of data anyway.
             */
            /* setup the send callback */
            xfer = OBJ_NEW(orte_self_send_xfer_t);
            if (NULL != req->send.iov) {
                xfer->iov = req->send.iov;
                xfer->count = req->send.count;
                xfer->cbfunc.iov = req->send.cbfunc.iov;
            } else {
                xfer->buffer = req->send.buffer;
                xfer->cbfunc.buffer = req->send.cbfunc.buffer;
            }
            xfer->tag = tag;
            xfer->cbdata = req->send.cbdata;
            /* setup the event for the send callback */
            opal_event_set(orte_event_base, &xfer->ev, -1, OPAL_EV_WRITE, send_self_exe, xfer);
            opal_event_set_priority(&xfer->ev, ORTE_MSG_PRI);
            opal_event_active(&xfer->ev, OPAL_EV_WRITE, 1);

            /* copy the message for the recv */
            rcv = OBJ_NEW(orte_rml_recv_t);
            rcv->sender = *peer;
            rcv->tag = tag;
            if (NULL != req->send.iov) {
                /* get the total number of bytes in the iovec array */
                bytes = 0;
                for (i = 0 ; i < req->send.count ; ++i) {
                    bytes += req->send.iov[i].iov_len;
                }
                /* get the required memory allocation */
                if (0 < bytes) {
                    rcv->iov.iov_base = (IOVBASE_TYPE*)malloc(bytes);
                    rcv->iov.iov_len = bytes;
                    /* transfer the bytes */
                    ptr =  (char*)rcv->iov.iov_base;
                    for (i = 0 ; i < req->send.count ; ++i) {
                        memcpy(ptr, req->send.iov[i].iov_base, req->send.iov[i].iov_len);
                        ptr += req->send.iov[i].iov_len;
                    }
                }
            } else if (0 < req->send.buffer->bytes_used) {
                rcv->iov.iov_base = (IOVBASE_TYPE*)malloc(req->send.buffer->bytes_used);
                memcpy(rcv->iov.iov_base, req->send.buffer->base_ptr, req->send.buffer->bytes_used);
                rcv->iov.iov_len = req->send.buffer->bytes_used;
            }
            /* post the message for receipt - since the send callback was posted
             * first and has the same priority, it will execute first
             */
            ORTE_RML_ACTIVATE_MESSAGE(rcv);
            OBJ_RELEASE(req);
            return;
       } else {
             memcpy(&ui64, (char*)peer, sizeof(uint64_t));
             if (OPAL_SUCCESS != opal_hash_table_get_value_uint64(&orte_rml_ofi.peers,
                                                     ui64, (void**)&pr) || NULL == pr) {
                  opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s rml:ofi: Send failed to get peer OFI contact info ",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                  return;
             }
             opal_output_verbose(1, orte_rml_base_framework.framework_output,
                                "%s rml:ofi: OFI peer contact info got from hash table",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
             dest_ep_name = pr->ofi_ep;
             dest_ep_namelen = pr->ofi_ep_len;
             ret = OPAL_SUCCESS;
      }
   }
    if ( OPAL_SUCCESS == ret) {
           //Anandhi added for debug purpose
            switch ( orte_rml_ofi.ofi_prov[ofi_prov_id].fabric_info->addr_format)
            {
                case  FI_SOCKADDR_IN :
                    /*  Address is of type sockaddr_in (IPv4) */
                    /*[debug] - print the sockaddr - port and s_addr */
                    ep_sockaddr = (struct sockaddr_in*)dest_ep_name;
                    opal_output_verbose(1,orte_rml_base_framework.framework_output,
                            "%s peer %s epnamelen is %lu, port = %d (or) 0x%x, InternetAddr = 0x%s  ",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),ORTE_NAME_PRINT(peer),
                            orte_rml_ofi.ofi_prov[ofi_prov_id].epnamelen,ntohs(ep_sockaddr->sin_port),
                            ntohs(ep_sockaddr->sin_port),inet_ntoa(ep_sockaddr->sin_addr));
                    /*[end debug]*/
                    break;
            }
            //Anandhi end debug
        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s OPAL_MODEX_RECV succeded, %s peer ep name obtained. length=%lu",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), dest_ep_namelen);
        ret = fi_av_insert(orte_rml_ofi.ofi_prov[ofi_prov_id].av, dest_ep_name,1,&dest_fi_addr,0,NULL);
        if( ret != 1) {
            opal_output_verbose(1, orte_rml_base_framework.framework_output,
                         "%s fi_av_insert failed in send_msg() returned %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),ret );
            /* call the send-callback fn with error and return, also return failure status */
            snd->status = ORTE_ERR_ADDRESSEE_UNKNOWN;

                ORTE_RML_SEND_COMPLETE(snd);
                return;
        }
    } else {

        opal_output_verbose(1, orte_rml_base_framework.framework_output,
                         "%s OPAL_MODEX_RECV failed to obtain  %s peer ep name ",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer));
            /* call the send-callback fn with error and return, also return failure status */
            snd->status = ORTE_ERR_ADDRESSEE_UNKNOWN;
            ORTE_RML_SEND_COMPLETE(snd);
            //OBJ_RELEASE( ofi_send_req);
            return;
    }

    ofi_send_req->send = snd;
    ofi_send_req->completion_count = 1;

    /* [DESC] we want to send the pid,seqnum,tag in addition to the data
    *   copy all of this to header of message from the ofi_send_t* send
    */
    ofi_send_req->hdr.dst = ofi_send_req->send->dst;
    ofi_send_req->hdr.origin = ofi_send_req->send->origin;
    ofi_send_req->hdr.seq_num = ofi_send_req->send->seq_num;
    ofi_send_req->hdr.tag         = ofi_send_req->send->tag;

    /*
     *  also insert ofi plugin specific header details -
     *  the unique msgid, for now initalise total_packets to 1
     */
    ofi_send_req->hdr.msgid   = orte_rml_ofi.cur_msgid;
    orte_rml_ofi.cur_msgid += 1;
    total_packets = 1;

    /* copy the buffer/iov/data to the ofi_send_req->datablob and update ofi_send_req->length*/
    ofi_send_req->length = 0;
    if( NULL != ofi_send_req->send->buffer) {
         ofi_send_req->length = ofi_send_req->send->buffer->bytes_used;
         ofi_send_req->data_blob = (char *)malloc(ofi_send_req->length);
         memcpy(ofi_send_req->data_blob ,
                ofi_send_req->send->buffer->base_ptr,
                ofi_send_req->send->buffer->bytes_used);
    } else if ( NULL != ofi_send_req->send->iov) {
        for (int i=0; i < ofi_send_req->send->count; i++) {
            ofi_send_req->length += ofi_send_req->send->iov[i].iov_len;
        }
        ofi_send_req->data_blob = (char *)malloc(ofi_send_req->length);
        int iovlen=0;
        for (int i=0; i < ofi_send_req->send->count; i++) {
            memcpy(((char *)ofi_send_req->data_blob + iovlen ),
                    ofi_send_req->send->iov[i].iov_base,
                    ofi_send_req->send->iov[i].iov_len);
            iovlen += ofi_send_req->send->iov[i].iov_len;
        }
    } else {
        //just send the data
        ofi_send_req->length = ofi_send_req->send->count;
        ofi_send_req->data_blob = (char *)malloc(ofi_send_req->length);
        memcpy(ofi_send_req->data_blob ,
               ofi_send_req->send->data,
               ofi_send_req->send->count);
   }


    opal_output_verbose(15, orte_rml_base_framework.framework_output,
                         "%s Completed copying all data into ofi_send_req->data_blob, total data - %lu bytes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ofi_send_req->length );

    /* Each packet will have header information, so the data length in each packet is datalen_per_packet.
     * check if the ofi_send_req->send->buffer->bytes_used is greater than the data per packet datalen_per_packet(recv buffer)
     * if so fragment and add info to header and send it in a loop back-to-back  */
    hdrsize = sizeof(orte_rml_ofi_msg_header_t);
    datalen_per_pkt = MIN_MULTI_BUF_SIZE - hdrsize;
    if (ofi_send_req->length > datalen_per_pkt )
    {
        total_packets = ( ofi_send_req->length / datalen_per_pkt ) + 1 ;
    }
    ofi_send_req->hdr.tot_pkts = total_packets;

    opal_output_verbose(15, orte_rml_base_framework.framework_output,
                         "%s datalen_per_pkt = %lu, ofi_send_req->length= %lu, total packets = %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), datalen_per_pkt, ofi_send_req->length, total_packets );

    /* in a loop send create and send the packets */
    for(size_t pkt_num=1,sent_data=0; sent_data < ofi_send_req->length; pkt_num++) {
        ofi_send_req->hdr.cur_pkt_num = pkt_num;
        /* create the packet */
        ofi_msg_pkt = OBJ_NEW(orte_rml_ofi_send_pkt_t);
        data_in_pkt  = ((ofi_send_req->length - sent_data) >= datalen_per_pkt) ?
                                          datalen_per_pkt : (ofi_send_req->length - sent_data);
        ofi_msg_pkt->pkt_size = hdrsize + data_in_pkt;
        opal_output_verbose(15, orte_rml_base_framework.framework_output,
                         "%s Packet %lu -> data_in_pkt= %lu, header_size= %lu, pkt_size=%lu",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), pkt_num,data_in_pkt,hdrsize,ofi_msg_pkt->pkt_size );
        /* copy the header and data for this pkt */
        ofi_msg_pkt->data = malloc( ofi_msg_pkt->pkt_size);
        memcpy(ofi_msg_pkt->data, &ofi_send_req->hdr, hdrsize );
        memcpy( ( (char *)ofi_msg_pkt->data + hdrsize ),
                ((char*)ofi_send_req->data_blob + sent_data),
                data_in_pkt);
        opal_output_verbose(15, orte_rml_base_framework.framework_output,
                         "%s Copying header, data into packets completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
        /* add it to list */
        opal_list_append(&(ofi_send_req->pkt_list), &ofi_msg_pkt->super);
        opal_output_verbose(15, orte_rml_base_framework.framework_output,
                         "%s adding packet %lu to list done successful",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),pkt_num );
        sent_data += data_in_pkt;
    }

    if( ofi_send_req->hdr.tot_pkts != ofi_send_req->hdr.cur_pkt_num ) {
        opal_output_verbose(1, orte_rml_base_framework.framework_output,
                         "%s Error: Total packets calculated [%d] does not match total created-%d pkts to peer %s with tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ofi_send_req->hdr.tot_pkts, ofi_send_req->hdr.cur_pkt_num,
                         ORTE_NAME_PRINT(peer), tag);
    }
    /*  do the fi_send() for all the pkts */
    ofi_send_req->completion_count= ofi_send_req->hdr.tot_pkts;
    OPAL_LIST_FOREACH(ofi_msg_pkt, &ofi_send_req->pkt_list, orte_rml_ofi_send_pkt_t) {
        /* debug purpose - copying the header from packet to verify if it is correct */
        struct orte_rml_ofi_msg_header_t *cur_hdr;
        cur_hdr = (struct orte_rml_ofi_msg_header_t* ) ofi_msg_pkt->data;
        opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s Sending Pkt[%d] of total %d pkts for msgid:%d to peer %s with tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), cur_hdr->cur_pkt_num, ofi_send_req->completion_count,
                         cur_hdr->msgid, ORTE_NAME_PRINT(peer), tag);
        /* end debug*/

        RML_OFI_RETRY_UNTIL_DONE(fi_send(orte_rml_ofi.ofi_prov[ofi_prov_id].ep,
                                 ofi_msg_pkt->data,
                                 ofi_msg_pkt->pkt_size,
                                 fi_mr_desc(orte_rml_ofi.ofi_prov[ofi_prov_id].mr_multi_recv),
                                 dest_fi_addr,
                                 (void *)&ofi_send_req->ctx));

    }
    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s End of send_msg_transport. fi_send completed to peer %s with tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag);
    OBJ_RELEASE(req);
}

int orte_rml_ofi_send_nb(struct orte_rml_base_module_t* mod,
                                   orte_process_name_t* peer,
                                   struct iovec* iov,
                                   int count,
                                   orte_rml_tag_t tag,
                                   orte_rml_callback_fn_t cbfunc,
                                   void* cbdata)
{
    ofi_send_request_t *req;
    orte_rml_ofi_module_t *ofi_mod = (orte_rml_ofi_module_t*)mod;
    int ofi_prov_id = ofi_mod->cur_transport_id;


    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s rml_ofi_send_transport to peer %s at tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag);


    if( (0 > ofi_prov_id) || ( ofi_prov_id >= orte_rml_ofi.ofi_prov_open_num ) ) {
        /* Invalid ofi_prov ID provided */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    if (ORTE_RML_TAG_INVALID == tag) {
        /* cannot send to an invalid tag */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    if (NULL == peer ||
        OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_NAME_INVALID, peer)) {
        /* cannot send to an invalid peer */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    /* get ourselves into an event to protect against
     * race conditions and threads
     */
    req = OBJ_NEW(ofi_send_request_t);
    req->ofi_prov_id = ofi_prov_id;
    req->send.dst = *peer;
    req->send.iov = iov;
    req->send.count = count;
    req->send.tag = tag;
    req->send.cbfunc.iov = cbfunc;
    req->send.cbdata = cbdata;

    /* setup the event for the send callback */
    opal_event_set(orte_event_base, &req->ev, -1, OPAL_EV_WRITE, send_msg, req);
    opal_event_set_priority(&req->ev, ORTE_MSG_PRI);
    opal_event_active(&req->ev, OPAL_EV_WRITE, 1);

    return ORTE_SUCCESS;
}


int orte_rml_ofi_send_buffer_nb(struct orte_rml_base_module_t *mod,
                                              orte_process_name_t* peer,
                                              struct opal_buffer_t* buffer,
                                              orte_rml_tag_t tag,
                                              orte_rml_buffer_callback_fn_t cbfunc,
                                              void* cbdata)
{
    ofi_send_request_t *req;
    orte_rml_ofi_module_t *ofi_mod = (orte_rml_ofi_module_t*)mod;
    int ofi_prov_id = ofi_mod->cur_transport_id;

    opal_output_verbose(10, orte_rml_base_framework.framework_output,
                         "%s rml_ofi_send_buffer_transport to peer %s at tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag);


    if( (0 > ofi_prov_id) || ( ofi_prov_id >= orte_rml_ofi.ofi_prov_open_num ) ) {
        /* Invalid ofi_prov ID provided */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    if (ORTE_RML_TAG_INVALID == tag) {
        /* cannot send to an invalid tag */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    if (NULL == peer ||
        OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_NAME_INVALID, peer)) {
        /* cannot send to an invalid peer */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    /* get ourselves into an event to protect against
     * race conditions and threads
     */
    req = OBJ_NEW(ofi_send_request_t);
    req->ofi_prov_id = ofi_prov_id;
    req->send.dst = *peer;
    req->send.buffer = buffer;
    req->send.tag = tag;
    req->send.cbfunc.buffer = cbfunc;
    req->send.cbdata = cbdata;

    /* setup the event for the send callback */
    opal_event_set(orte_event_base, &req->ev, -1, OPAL_EV_WRITE, send_msg, req);
    opal_event_set_priority(&req->ev, ORTE_MSG_PRI);
    opal_event_active(&req->ev, OPAL_EV_WRITE, 1);

    return ORTE_SUCCESS;
}
