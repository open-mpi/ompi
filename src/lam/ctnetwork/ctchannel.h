/*
 *  ctchannel.h
 *  LAM-MPI
 *
 *  Created by Rob Aulwes on Tue Dec 23 2003.
 *  Copyright (c) 2003 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef CT_CHANNEL_H
#define CT_CHANNEL_H

#include "lam/lfc/object.h"
#include "runtime/ctnetwork/ctmessage.h"


/*
 *      Channel error codes
 */

typedef enum
{
    CT_CHNL_OK = 0,
    CT_CHNL_ERROR,                    /* general channel error. */
    CT_CHNL_MALLOC,                   /* unable to alloc mem. */
    CT_CHNL_CLOSED,                   /* channel is not open */
    CT_CHNL_CONN_LOST,                 /* lost connection */
    CT_CHNL_INVALID_MSG,               /* unable to pack/unpack msg or msg is NULL */
    CT_CHNL_TIMED_OUT                 /* channel operation timed out. */
} lam_ctchnl_status_t;


/*
 *      Channel connection status
 */

enum
{
    CT_CHNL_CLOSED = 0,
    CT_CHNL_CONNECTED,
    CT_CHNL_FAILED
}

/*
 *
 *  Abstract communication channel class
 *  The controllers and clients use these objects to
 *  communicate in the network.
 */

#define CTCHANNEL(obj)     (lam_ctchannel_t *)(obj)

struct lam_ctchannel;

typedef struct lam_ctchannel_class
{
    lam_class_info_t    super;
    /* return: error code args: (channel, data, data length, bytes sent) */
    uint32_t            cth_send(struct lam_ctchannel *, const uint8_t *, uint32_t, uint32_t *);
    
    /* return: error code args: (channel, recv buffer, buffer length, bytes received) */
    uint32_t            cth_recv(struct lam_ctchannel *, const uint8_t *, uint32_t, uint32_t *);
    
    /* return: error code args: (channel, msg ptr) */
    uint32_t            cth_get_msg(struct lam_ctchannel *, lam_ctmsg_t **);
    
    /* return: error code args: (channel, recv buffer ptr, bytes received) */
    uint32_t            cth_get_packed_msg(struct lam_ctchannel *, const uint8_t **, uint32_t *);
    
    /* return: error code args: (channel, msg) */
    uint32_t            cth_send_msg(struct lam_ctchannel *, lam_ctmsg_t *);
    
    /* return: error code args: (channel, msg ptr, msg len) */
    uint32_t            cth_send_packed_msg(struct lam_ctchannel *, const uint8_t *, uint32_t);
    
} lam_ctchannel_class_t;


extern lam_ctchannel_class_t    lam_ctchannel_cls;

typedef struct lam_ctchannel
{
    lam_object_t    super;
    int             cth_status;
    uint32_t        cth_id;
    uint32_t        cth_timeout_secs;
} lam_ctchannel_t;


void lam_cth_init(lam_ctchannel_t *channel);

/*
 *
 *  Accessor functions
 *
 */

inline int lam_cth_is_connected(lam_ctchannel_t *channel)
{
    return (CT_CHNL_CONNECTED == channel->cth_status);
}

inline uint32_t lam_cth_get_id(lam_ctchannel_t *channel) {return channel->cth_id;}
inline void lam_cth_set_id(lam_ctchannel_t *channel, uint32_t cid)
{
    channel->cth_id = cid;
}

inline uint32_t lam_cth_get_timeout(lam_ctchannel_t *channel) {return channel->cth_timeout_secs;}
inline void lam_cth_set_id(lam_ctchannel_t *channel, uint32_t timeout)
{
    channel->cth_timeout_secs = timeout;
}

/*
 *
 *  PURE VIRTUAL functions that must be
 *  implemented by concrete subclasses.
 *
 */


/* return: error code args: (channel, data, data length, bytes sent) */
uint32_t lam_cth_send(lam_ctchannel_t *channel, const uint8_t *data, 
                      uint32_t data_len, uint32_t *bytes_sent);

/* return: error code args: (channel, recv buffer, buffer length, bytes received) */
uint32_t lam_cth_recv(lam_ctchannel_t *channel, const uint8_t *buffer, 
                      uint32_t buff_len, uint32_t *bytes_recvd);

/* return: error code args: (channel, msg ptr) */
uint32_t lam_cth_get_msg(lam_ctchannel_t *channel, lam_ctmsg_t **msg);

/* return: error code args: (channel, recv buffer ptr, bytes received) */
uint32_t lam_cth_get_packed_msg(lam_ctchannel_t *channel, const uint8_t **buffer, 
                                uint32_t *bytes_recvd);

/* return: error code args: (channel, msg) */
uint32_t lam_cth_send_msg(lam_ctchannel_t *channel, lam_ctmsg_t *msg);

/* return: error code args: (channel, msg ptr, msg len) */
uint32_t lam_cth_send_packed_msg(lam_ctchannel_t *channel, const uint8_t *packed_msg, 
                                 uint32_t msg_len);

/*
 *
 *  TCP channel class
 *
 */

typedef struct lam_tcp_channel
{
    lam_ctchannel_t     super;
    int                 tcp_sockfd;
    struct sockaddr_in  tcp_addr;
    int                 tcp_blocking;
} lam_tcp_chnl_t;

extern lam_ctchannel_class_t   lam_tcp_channel_cls;

void lam_tcpch_init(lam_tcp_chnl_t *channel);
void lam_tcpch_destroy(lam_tcp_chnl_t *channel);


uint32_t lam_tcpch_send(lam_tcp_chnl_t *channel, const uint8_t *data, 
                      uint32_t data_len, uint32_t *bytes_sent);

uint32_t lam_tcpch_recv(lam_tcp_chnl_t *channel, const uint8_t *buffer, 
                      uint32_t buff_len, uint32_t *bytes_recvd);

uint32_t lam_tcpch_get_msg(lam_tcp_chnl_t *channel, lam_ctmsg_t **msg);

uint32_t lam_tcpch_get_packed_msg(lam_tcp_chnl_t *channel, const uint8_t **buffer, 
                                uint32_t *bytes_recvd);

uint32_t lam_tcpch_send_msg(lam_tcp_chnl_t *channel, lam_ctmsg_t *msg);

uint32_t lam_tcpch_send_packed_msg(lam_tcp_chnl_t *channel, 
                                   const uint8_t *packed_msg, 
                                   uint32_t msg_len);


#endif  /* CT_CHANNEL_H */


