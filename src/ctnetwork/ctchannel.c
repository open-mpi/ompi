/*
 * $HEADER$
 */

#include <string.h>

#include "ctnetwork/ctchannel.h"

#define CHANNEL_CLS(chnl) \
    ((ompi_ctchannel_class_t *)(((ompi_object_t *) chnl)->obj_class))


static void ompi_cth_construct(ompi_ctchannel_t *channel)
{
    channel->cth_status = CT_CHNL_CLOSED;
    channel->cth_id = 0;
    channel->cth_timeout_secs = 0;
}


static void ompi_cth_construct(ompi_ctchannel_t *channel)
{
}


ompi_ctchannel_class_t ompi_ct_channel_t_class = {
    {
        "ompi_ct_channel_t",
        OBJ_CLASS(ompi_object_t),
        (ompi_construct_t) ompi_cth_construct,
        (ompi_destruct_t) ompi_object_destruct
    },
    NULL, NULL, NULL, NULL, NULL, NULL
};


/*
 *
 *  PURE VIRTUAL functions that must be
 *  implemented by concrete subclasses.
 *
 */


/* return: error code args: (channel, data, data length, bytes sent) */
uint32_t ompi_cth_send(ompi_ctchannel_t *channel, const uint8_t *data,
                      uint32_t data_len, uint32_t *bytes_sent)
{
    return CHANNEL_CLS(channel)->cth_send(channel, data, data_len,
                                          bytes_sent);
}

/* return: error code args: (channel, recv buffer, buffer length, bytes received) */
uint32_t ompi_cth_recv(ompi_ctchannel_t *channel, const uint8_t *buffer,
                      uint32_t buff_len, uint32_t *bytes_recvd)
{
    return CHANNEL_CLS(channel)->cth_recv(channel, buffer,
                                          buff_len, bytes_recvd);
}

/* return: error code args: (channel, msg ptr) */
uint32_t ompi_cth_get_msg(ompi_ctchannel_t *channel, ompi_ctmsg_t **msg)
{
    return CHANNEL_CLS(channel)->cth_get_msg(channel, msg);
}

/* return: error code args: (channel, recv buffer ptr, bytes received) */
uint32_t ompi_cth_get_packed_msg(ompi_ctchannel_t *channel,
                                const uint8_t **buffer,
                                uint32_t *bytes_recvd)
{
    return CHANNEL_CLS(channel)->cth_get_packed_msg(channel, buffer,
                                                    bytes_recvd);
}

/* return: error code args: (channel, msg) */
uint32_t ompi_cth_send_msg(ompi_ctchannel_t *channel, ompi_ctmsg_t *msg)
{
    return CHANNEL_CLS(channel)->cth_send_msg(channel, msg);
}


/* return: error code args: (channel, msg ptr, msg len) */
uint32_t ompi_cth_send_packed_msg(ompi_ctchannel_t *channel,
                                 const uint8_t *packed_msg,
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


static void ompi_tcpch_construct(ompi_tcp_chnl_t *channel)
{
    channel->tcp_sockfd = 0;
    memset(&(channel->tcp_addr), 0, sizeof(channel->tcp_addr));
    channel->tcp_blocking = 0;
}


static void ompi_tcpch_destruct(ompi_tcp_chnl_t *channel)
{
}


ompi_ctchannel_class_t ompi_tcp_chnl_t_class = {
    {
        "ompi_tcp_chnl_t",
        OBJ_CLASS(ompi_ctchannel_t),
        (ompi_construct_t) ompi_tcpch_construct,
        (ompi_destruct_t) ompi_tcpch_destruct
    },
    ompi_tcpch_send,
    ompi_tcpch_recv,
    ompi_tcpch_get_msg,
    ompi_tcpch_get_packed_msg,
    ompi_tcpch_send_msg,
    ompi_tcpch_send_packed_msg
};


uint32_t ompi_tcpch_send(ompi_tcp_chnl_t *channel, const uint8_t *data,
                        uint32_t data_len, uint32_t *bytes_sent)
{
    uint32_t ret = CT_CHNL_ERR_OK;

    return ret;
}


uint32_t ompi_tcpch_recv(ompi_tcp_chnl_t *channel, const uint8_t *buffer,
                        uint32_t buff_len, uint32_t *bytes_recvd)
{
    uint32_t ret = CT_CHNL_ERR_OK;

    return ret;
}


uint32_t ompi_tcpch_get_msg(ompi_tcp_chnl_t *channel, ompi_ctmsg_t **msg)
{
    uint32_t ret = CT_CHNL_ERR_OK;

    return ret;
}


uint32_t ompi_tcpch_get_packed_msg(ompi_tcp_chnl_t *channel,
                                  const uint8_t **buffer,
                                  uint32_t *bytes_recvd)
{
    uint32_t ret = CT_CHNL_ERR_OK;

    return ret;
}


uint32_t ompi_tcpch_send_msg(ompi_tcp_chnl_t *channel, ompi_ctmsg_t *msg)
{
    uint32_t ret = CT_CHNL_ERR_OK;

    return ret;
}


uint32_t ompi_tcpch_send_packed_msg(ompi_tcp_chnl_t *channel,
                                   const uint8_t *packed_msg,
                                   uint32_t msg_len)
{
    uint32_t ret = CT_CHNL_ERR_OK;

    return ret;
}
