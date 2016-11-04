/*
 * Copyright (c) 2015      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTE_RML_OFI_REQUEST_H
#define ORTE_RML_OFI_REQUEST_H


#define TO_OFI_REQ(_ptr_ctx) \
    container_of((_ptr_ctx), orte_rml_ofi_request_t, ctx)

typedef enum {
  ORTE_RML_OFI_SEND,
  ORTE_RML_OFI_RECV,
  ORTE_RML_OFI_ACK,
  ORTE_RML_OFI_PROBE
} orte_rml_ofi_request_type_t;
/* orte_rml_ofi_msg_header_t contains the header information for the message being sent.
The header and data is passed on to the destination. The destination will re-construct the
orte_rml_sent_t struct once it receives this header and data.This header has the required information
to construct the orte_rml_sent_t struct and also if the message is split into packets,
then the packet information - total number of packets and the current packet number.
*/
struct orte_rml_ofi_msg_header_t{
	opal_process_name_t	origin;         // originator process id from the send message
	opal_process_name_t	dst;            // Destination process id from the send message
	uint32_t			seq_num;        // seq_num  from the send message
	orte_rml_tag_t		tag;	        // tag  from the send message
    uint32_t            msgid;          // unique msgid added by ofi plugin to keep track of fragmented msgs
    uint32_t            tot_pkts;	    // total packets this msg will be fragmented into by ofi plugin
    uint32_t            cur_pkt_num;    // current packet number
 };
typedef struct orte_rml_ofi_msg_header_t orte_rml_ofi_msg_header_t;

/*
orte_rml_ofi_pkts_t defines the packets in the message. Each packet contains header information
and the data. Create a list of packets to hold the entire message.
*/
typedef struct {
    //list_item_t
    opal_list_item_t super;
    /* header + data size */
    size_t pkt_size;
    //header + data
    void *data;
}orte_rml_ofi_send_pkt_t;
OBJ_CLASS_DECLARATION(orte_rml_ofi_send_pkt_t);

/*
orte_rml_ofi_recv_pkt_t defines the packets in the receiving end of message.
Each packet contains the packet number and the data.
Create a list of packets to hold the entire message.
*/
typedef struct {
    //list_item_t
    opal_list_item_t super;
    /* current packet number */
    uint32_t cur_pkt_num;
    /*data size */
    size_t pkt_size;
    //data
    void *data;
}orte_rml_ofi_recv_pkt_t;
OBJ_CLASS_DECLARATION(orte_rml_ofi_recv_pkt_t);

/*
orte_rml_ofi_request_t holds the send request (orte_rml_send_t)
*/
typedef struct {
	opal_object_t super;

    /** OFI context */
    struct fi_context ctx;

    orte_rml_send_t *send;

    /** OFI provider_id the request will use - this is
    *	the reference to element into the orte_rml_ofi.ofi_prov[] **/
    uint8_t ofi_prov_id;

    /** OFI Request type */
    orte_rml_ofi_request_type_t type;

    /** Completion count used by blocking and/or synchronous operations */
    volatile int completion_count;

    /** Reference to the RML used to lookup */
    /*  source of an ANY_SOURCE Recv        */
    struct orte_rml_base_module_t* rml;

    /** header being sent  **/
	orte_rml_ofi_msg_header_t hdr;

    /** Pack buffer */
    void *data_blob;

    /** Pack buffer size */
    size_t length;

    /** Header and data in a list of Packets orte_rml_ofi_send_pkt_t */
    opal_list_t pkt_list;

} orte_rml_ofi_request_t;
OBJ_CLASS_DECLARATION(orte_rml_ofi_request_t);


/* This will hold all the pckts received at the destination.
Each entry will be indexed by [sender,msgid] and will have
all the packets for that msgid and sender.
*/
typedef struct {

    opal_list_item_t    super;     //list_item_t
    uint32_t            msgid;     // unique msgid added by ofi plugin to keep track of fragmented msgs
    opal_process_name_t	sender;    // originator process id from the send message
    uint32_t            tot_pkts;  // total packets this msg will be fragmented into by ofi plugin
    uint32_t            pkt_recd;  // current packet number
    opal_list_t         pkt_list;  // list holding Packets in this msg of type orte_rml_ofi_recv_pkt_t
} ofi_recv_msg_queue_t;
OBJ_CLASS_DECLARATION( ofi_recv_msg_queue_t);

/* define an object for transferring send requests to the event lib */
typedef struct {
    opal_object_t super;
    opal_event_t ev;
    orte_rml_send_t send;
    /* ofi provider id */
    int ofi_prov_id;
} ofi_send_request_t;
OBJ_CLASS_DECLARATION(ofi_send_request_t);

#endif
