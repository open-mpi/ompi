/*
 * $HEADER$
 */

#include <string.h>

#include "lam/ctnetwork/ctchannel.h"

#define CHANNEL_CLS(chnl)       ((lam_ctchannel_class_t *)(OBJECT(chnl)->obj_class))

lam_ctchannel_class_t   lam_ctchannel_cls = {
{"lam_ct_channel_t", &lam_object_cls,
    (class_init_t)lam_cth_init,
    (class_destroy_t)lam_obj_destroy},
    NULL, NULL, NULL, NULL,
    NULL, NULL
};


void lam_cth_init(lam_ctchannel_t *channel)
{
    SUPER_INIT(channel, lam_ctchannel_cls.cls_parent);
    channel->cth_status = CT_CHNL_CLOSED;
    channel->cth_id = 0;
    channel->cth_timeout_secs = 0;
}

/*
 *
 *  PURE VIRTUAL functions that must be
 *  implemented by concrete subclasses.
 *
 */


/* return: error code args: (channel, data, data length, bytes sent) */
uint32_t lam_cth_send(lam_ctchannel_t *channel, const uint8_t *data, 
                      uint32_t data_len, uint32_t *bytes_sent)
{
    return CHANNEL_CLS(channel)->cth_send(channel, data, data_len, bytes_sent);
}

/* return: error code args: (channel, recv buffer, buffer length, bytes received) */
uint32_t lam_cth_recv(lam_ctchannel_t *channel, const uint8_t *buffer, 
                      uint32_t buff_len, uint32_t *bytes_recvd)
{
    return CHANNEL_CLS(channel)->cth_recv(channel, buffer,
                                          buff_len, bytes_recvd);
}

/* return: error code args: (channel, msg ptr) */
uint32_t lam_cth_get_msg(lam_ctchannel_t *channel, lam_ctmsg_t **msg)
{
    return CHANNEL_CLS(channel)->cth_get_msg(channel, msg);
}

/* return: error code args: (channel, recv buffer ptr, bytes received) */
uint32_t lam_cth_get_packed_msg(lam_ctchannel_t *channel, const uint8_t **buffer, 
                                uint32_t *bytes_recvd)
{
    return CHANNEL_CLS(channel)->cth_get_packed_msg(channel, buffer, bytes_recvd);    
}

/* return: error code args: (channel, msg) */
uint32_t lam_cth_send_msg(lam_ctchannel_t *channel, lam_ctmsg_t *msg)
{
    return CHANNEL_CLS(channel)->cth_send_msg(channel, msg);
}


/* return: error code args: (channel, msg ptr, msg len) */
uint32_t lam_cth_send_packed_msg(lam_ctchannel_t *channel, const uint8_t *packed_msg, 
                                 uint32_t msg_len)
{
    return CHANNEL_CLS(channel)->cth_send_packed_msg(channel, packed_msg,
                                                     msg_len);    
}






/*
 *
 *  TCP channel class
 *
 */


lam_ctchannel_class_t   lam_tcp_channel_cls = {
    {"lam_tcp_chnl_t", &lam_ctchannel_cls,
        (class_init_t)lam_tcpch_init,
        (class_destroy_t)lam_tcpch_destroy},
    lam_tcpch_send,
    lam_tcpch_recv,
    lam_tcpch_get_msg,
    lam_tcpch_get_packed_msg,
    lam_tcpch_send_msg,
    lam_tcpch_send_packed_msg
};

void lam_tcpch_init(lam_tcp_chnl_t *channel)
{
    SUPER_INIT(channel, lam_tcp_channel_cls.cls_parent);
    channel->tcp_sockfd = 0;
    memset(&(channel->tcp_addr), 0, sizeof(channel->tcp_addr));
    channel->tcp_blocking = 0;
}

void lam_tcpch_destroy(lam_tcp_chnl_t *channel)
{
    SUPER_DESTROY(channel, lam_tcp_channel_cls.cls_parent);
}


uint32_t lam_tcpch_send(lam_tcp_chnl_t *channel, const uint8_t *data, 
                        uint32_t data_len, uint32_t *bytes_sent)
{
    uint32_t        ret = CT_CHNL_ERR_OK;
    
    return ret;
}

uint32_t lam_tcpch_recv(lam_tcp_chnl_t *channel, const uint8_t *buffer, 
                        uint32_t buff_len, uint32_t *bytes_recvd)
{
    uint32_t        ret = CT_CHNL_ERR_OK;
    
    return ret;
}


uint32_t lam_tcpch_get_msg(lam_tcp_chnl_t *channel, lam_ctmsg_t **msg)
{
    uint32_t        ret = CT_CHNL_ERR_OK;
    
    return ret;
}


uint32_t lam_tcpch_get_packed_msg(lam_tcp_chnl_t *channel, const uint8_t **buffer, 
                                  uint32_t *bytes_recvd)
{
    uint32_t        ret = CT_CHNL_ERR_OK;
    
    return ret;
}


uint32_t lam_tcpch_send_msg(lam_tcp_chnl_t *channel, lam_ctmsg_t *msg)
{
    uint32_t        ret = CT_CHNL_ERR_OK;
    
    return ret;
}


uint32_t lam_tcpch_send_packed_msg(lam_tcp_chnl_t *channel, 
                                   const uint8_t *packed_msg, 
                                   uint32_t msg_len)
{
    uint32_t        ret = CT_CHNL_ERR_OK;
    
    return ret;
}





