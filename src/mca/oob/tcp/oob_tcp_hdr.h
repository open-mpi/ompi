/*
 * $HEADER$
 */
/** @file:
 *
 *  Contains header used by tcp oob.
 */

#ifndef _MCA_OOB_TCP_HDR_H_
#define _MCA_OOB_TCP_HDR_H_

#define MCA_OOB_TCP_IDENT    1
#define MCA_OOB_TCP_MSG      2

/**
 * Header used by tcp oob protocol.
 */
struct mca_oob_tcp_hdr_t {
    ompi_process_name_t msg_src;
    ompi_process_name_t msg_dst;
    uint32_t msg_type;                /**< type of message */
    uint32_t msg_size;                /**< the total size of the message body - excluding header */ 
    int32_t  msg_tag;                 /**< user provided tag */
};
typedef struct mca_oob_tcp_hdr_t mca_oob_tcp_hdr_t;

/**
 * Convert the message header to host byte order
 */
#define MCA_OOB_TCP_HDR_NTOH(h) \
    OMPI_PROCESS_NAME_NTOH((h)->msg_src); \
    OMPI_PROCESS_NAME_NTOH((h)->msg_dst); \
    ntohl((h)->msg_type); \
    ntohl((h)->msg_size); \
    ntohl((h)->msg_tag);

/**
 * Convert the message header to network byte order
 */
#define MCA_OOB_TCP_HDR_HTON(h) \
    OMPI_PROCESS_NAME_HTON((h)->msg_src); \
    OMPI_PROCESS_NAME_HTON((h)->msg_dst); \
    htonl((h)->msg_type); \
    htonl((h)->msg_size); \
    htonl((h)->msg_tag);

#endif /* _MCA_OOB_TCP_MESSAGE_H_ */

