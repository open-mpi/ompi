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

#include "lam/base/object.h"
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
    uint32_t            cth_get_msg(struct lam_ctchannel *, lam_ctmsg_t **msg);
    
    /* return: error code args: (channel, recv buffer ptr, bytes received) */
    uint32_t            cth_get_packed_msg(struct lam_ctchannel *, const uint8_t **, uint32_t *);
    
    /* return: error code args: (channel, msg) */
    uint32_t            cth_send_msg(struct lam_ctchannel *, lam_ctmsg_t *msg);
    
    /* return: error code args: (channel, msg ptr, msg len) */
    uint32_t            cth_send_packed_msg(struct lam_ctchannel *, const uint8_t *, uint32_t);
    
} lam_ctchannel_class_t;



#endif  /* CT_CHANNEL_H */


