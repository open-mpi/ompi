/*
 * $HEADER$
 */
/** @file:
 *
 *  Contains header used by tcp oob.
 */

#ifndef _MCA_OOB_TCP_HDR_H_
#define _MCA_OOB_TCP_HDR_H_

/**
 * Header used by tcp oob protocol.
 */
struct mca_oob_tcp_hdr_t {
    uint32_t msg_size;                /**< the total size of the message body - excluding header */ 
    int32_t  msg_tag;                 /**< user provided tag */
};
typedef struct mca_oob_tcp_hdr_t mca_oob_tcp_hdr_t;

/**
 * Convert the message header to host byte order
 */
#define MCA_OOB_TCP_HDR_NTOHL(h) \
    ntohl(h->msg_size); \
    ntohl(h->msg_tag);

/**
 * Convert the message header to network byte order
 */
#define MCA_OOB_TCP_HDR_HTONL(h) \
    htonl(h->msg_size); \
    htonl(h->msg_tag);

#endif /* _MCA_OOB_TCP_MESSAGE_H_ */

