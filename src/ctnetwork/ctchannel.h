/*
 * $HEADER$
 */

#ifndef OMPI_CT_CHANNEL_H
#define OMPI_CT_CHANNEL_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "class/ompi_object.h"
#include "ctnetwork/ctmessage.h"


/*
 *      Channel error codes
 */

typedef enum
{
    CT_CHNL_ERR_OK = 0,
    CT_CHNL_ERR_GENERAL,                  /* general channel error. */
    CT_CHNL_ERR_MALLOC,                   /* unable to alloc mem. */
    CT_CHNL_ERR_CLOSED,                   /* channel is not open */
    CT_CHNL_ERR_CONN_LOST,                /* lost connection */
    CT_CHNL_ERR_INVALID_MSG,              /* unable to pack/unpack msg or msg is NULL */
    CT_CHNL_ERR_TIMED_OUT                 /* channel operation timed out. */
} ompi_ctchnl_error_t;


/*
 *      Channel connection status
 */

typedef enum
{
    CT_CHNL_CLOSED = 0,
    CT_CHNL_CONNECTED,
    CT_CHNL_FAILED
} ompi_ctchnl_status_t;

/*
 *
 *  Abstract communication channel class
 *  The controllers and clients use these objects to
 *  communicate in the network.
 */

#define CTCHANNEL(obj)     (ompi_ctchannel_t *)(obj)

struct ompi_ctchannel;

/* return: error code args: (channel, data, data length, bytes sent) */
typedef uint32_t (*ompi_cth_send_fn_t)(struct ompi_ctchannel *, 
                                      const uint8_t *, uint32_t, uint32_t *);
    
/* return: error code args: (channel, recv buffer, buffer length, bytes received) */
typedef uint32_t (*ompi_cth_recv_fn_t)(struct ompi_ctchannel *, 
                                      const uint8_t *, uint32_t, uint32_t *);
    
/* return: error code args: (channel, msg ptr) */
typedef uint32_t (*ompi_cth_get_msg_fn_t)(struct ompi_ctchannel *, 
                                         ompi_ctmsg_t **);
    
/* return: error code args: (channel, recv buffer ptr, bytes received) */
typedef uint32_t (*ompi_cth_get_packed_msg_fn_t)(struct ompi_ctchannel *, 
                                                const uint8_t **, uint32_t *);
    
/* return: error code args: (channel, msg) */
typedef uint32_t (*ompi_cth_send_msg_fn_t)(struct ompi_ctchannel *, 
                                          ompi_ctmsg_t *);
    
/* return: error code args: (channel, msg ptr, msg len) */
typedef uint32_t (*ompi_cth_send_packed_msg_fn_t)(struct ompi_ctchannel *, 
                                                 const uint8_t *, uint32_t);

typedef struct ompi_ctchannel_class
{
  ompi_class_t    super;
  /* return: error code args: (channel, data, data length, bytes sent) */
  ompi_cth_send_fn_t *send;
    
  /* return: error code args: (channel, recv buffer, buffer length, bytes received) */
  ompi_cth_recv_fn_t *recv;

  /* return: error code args: (channel, msg ptr) */
  ompi_cth_get_msg_fn_t *get_msg;
    
  /* return: error code args: (channel, recv buffer ptr, bytes received) */
  ompi_cth_get_packed_msg_fn_t *get_packed_msg;
    
  /* return: error code args: (channel, msg) */
  ompi_cth_send_msg_fn_t *send_msg;
    
  /* return: error code args: (channel, msg ptr, msg len) */
  ompi_cth_send_packed_msg_fn_t *send_packed_msg;
} ompi_ctchannel_class_t;


extern ompi_ctchannel_class_t    ompi_ct_channel_t_class;

typedef struct ompi_ctchannel
{
    ompi_object_t    super;
    int             cth_status;
    uint32_t        cth_id;
    uint32_t        cth_timeout_secs;
} ompi_ctchannel_t;


/*
 *
 *  Accessor functions
 *
 */

int ompi_cth_is_connected(ompi_ctchannel_t *channel);
inline int ompi_cth_is_connected(ompi_ctchannel_t *channel)
{
    return (CT_CHNL_CONNECTED == channel->cth_status);
}

uint32_t ompi_cth_get_id(ompi_ctchannel_t *channel);
inline uint32_t ompi_cth_get_id(ompi_ctchannel_t *channel) 
{
  return channel->cth_id;
}
void ompi_cth_set_id(ompi_ctchannel_t *channel, uint32_t cid);
inline void ompi_cth_set_id(ompi_ctchannel_t *channel, uint32_t cid)
{
    channel->cth_id = cid;
}

uint32_t ompi_cth_get_timeout(ompi_ctchannel_t *channel);
inline uint32_t ompi_cth_get_timeout(ompi_ctchannel_t *channel) 
{
  return channel->cth_timeout_secs;
}
void ompi_cth_set_timeout(ompi_ctchannel_t *channel, uint32_t timeout);
inline void ompi_cth_set_timeout(ompi_ctchannel_t *channel, uint32_t timeout)
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
uint32_t ompi_cth_send(ompi_ctchannel_t *channel, const uint8_t *data, 
                      uint32_t data_len, uint32_t *bytes_sent);

/* return: error code args: (channel, recv buffer, buffer length, bytes received) */
uint32_t ompi_cth_recv(ompi_ctchannel_t *channel, const uint8_t *buffer, 
                      uint32_t buff_len, uint32_t *bytes_recvd);

/* return: error code args: (channel, msg ptr) */
uint32_t ompi_cth_get_msg(ompi_ctchannel_t *channel, ompi_ctmsg_t **msg);

/* return: error code args: (channel, recv buffer ptr, bytes received) */
uint32_t ompi_cth_get_packed_msg(ompi_ctchannel_t *channel, const uint8_t **buffer, 
                                uint32_t *bytes_recvd);

/* return: error code args: (channel, msg) */
uint32_t ompi_cth_send_msg(ompi_ctchannel_t *channel, ompi_ctmsg_t *msg);

/* return: error code args: (channel, msg ptr, msg len) */
uint32_t ompi_cth_send_packed_msg(ompi_ctchannel_t *channel, const uint8_t *packed_msg, 
                                 uint32_t msg_len);

/*
 *
 *  TCP channel class
 *
 */

typedef struct ompi_tcp_channel
{
    ompi_ctchannel_t     super;
    int                 tcp_sockfd;
    struct sockaddr_in  tcp_addr;
    int                 tcp_blocking;
} ompi_tcp_chnl_t;

extern ompi_ctchannel_class_t   ompi_tcp_chnl_t_class;

uint32_t ompi_tcpch_send(ompi_tcp_chnl_t *channel, const uint8_t *data, 
                      uint32_t data_len, uint32_t *bytes_sent);

uint32_t ompi_tcpch_recv(ompi_tcp_chnl_t *channel, const uint8_t *buffer, 
                      uint32_t buff_len, uint32_t *bytes_recvd);

uint32_t ompi_tcpch_get_msg(ompi_tcp_chnl_t *channel, ompi_ctmsg_t **msg);

uint32_t ompi_tcpch_get_packed_msg(ompi_tcp_chnl_t *channel, const uint8_t **buffer, 
                                uint32_t *bytes_recvd);

uint32_t ompi_tcpch_send_msg(ompi_tcp_chnl_t *channel, ompi_ctmsg_t *msg);

uint32_t ompi_tcpch_send_packed_msg(ompi_tcp_chnl_t *channel, 
                                   const uint8_t *packed_msg, 
                                   uint32_t msg_len);


#endif  /* OMPI_CT_CHANNEL_H */
