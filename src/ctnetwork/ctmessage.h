/*
 * $HEADER$
 */

#ifndef OMPI_CT_MESSAGE_H
#define OMPI_CT_MESSAGE_H

#include "class/ompi_object.h"


/*
 *
 *      Available Classes
 *
 */

extern ompi_class_t     ompi_ct_ctrl_t_class;
extern ompi_class_t     ompi_ctmsg_t_class;

/*
 *
 *      CT Message interface
 *
 */

/*
 * Message control info for routing msgs.
 */


/*
 * Message routing type
 */
 
enum
{
    OMPI_CT_BCAST = 1,
    OMPI_CT_ALLGATHER,
    OMPI_CT_SCATTER,
    OMPI_CT_PT2PT
};

/*
 * Msg control interface to
 * contain routing information.
 * Can be reused by multiple msgs
 * if routing info is the same.
 *
 */

typedef struct ompi_ct_ctrl
{
    ompi_object_t    super;
    uint16_t        ctc_is_user_msg;    /* 1 -> msg is for user app. */
    uint16_t        ctc_routing_type;   /* broadcast, scatter, pt2pt, etc. */
    uint32_t        ctc_sender;         /* node that initiated send. */
    uint32_t        ctc_dest;           /* destination node. */
    uint32_t        ctc_forwarding;     /* node that is relaying msg. */
    uint32_t        ctc_client_tag;     /* tag if client sent msg. */
    uint32_t        ctc_info_len;
    uint8_t         *ctc_info;
} ompi_ct_ctrl_t;


void ompi_ctc_construct(ompi_ct_ctrl_t *ctrl);
void ompi_ctc_destruct(ompi_ct_ctrl_t *ctrl);

void ompi_ctc_construct_with(ompi_ct_ctrl_t *ctrl, int routing_type,
                       uint32_t sender,
                       uint32_t dest);

uint32_t ompi_ctc_pack_size(ompi_ct_ctrl_t *ctrl);
uint8_t *ompi_ctc_pack(ompi_ct_ctrl_t *ctrl, uint32_t *len);
ompi_ct_ctrl_t *ompi_ctc_unpack(uint8_t *buffer);

int ompi_ctc_pack_buffer(ompi_ct_ctrl_t *ctrl, uint8_t *buffer, uint32_t *len);
int ompi_ctc_unpack_buffer(ompi_ct_ctrl_t *ctrl, uint8_t *buffer, uint32_t *len);

/*
 * Functions for accessing data in packed
 * control struct.
 */

uint16_t ompi_pk_ctc_is_user_msg(uint8_t *buffer);

uint16_t ompi_pk_ctc_get_routing_type(uint8_t *buffer);

uint32_t ompi_pk_ctc_get_sender(uint8_t *buffer);

uint32_t ompi_pk_ctc_get_dest(uint8_t *buffer);

uint32_t ompi_pk_ctc_get_forwarding(uint8_t *buffer);
void ompi_pk_ctc_set_forwarding(uint8_t *buffer, uint32_t node);

uint8_t *ompi_pk_ctc_get_info(uint8_t *buffer, uint32_t *len);
void ompi_pk_ctc_set_info(uint8_t *buffer, uint8_t *info);

/*
 * Accessor functions
 */

bool ompi_ctc_get_is_user_msg(ompi_ct_ctrl_t *ctrl);
inline bool ompi_ctc_get_is_user_msg(ompi_ct_ctrl_t *ctrl)
{
  return ctrl->ctc_is_user_msg;
}
void ompi_ctc_set_is_user_msg(ompi_ct_ctrl_t *ctrl, bool yn);
inline void ompi_ctc_set_is_user_msg(ompi_ct_ctrl_t *ctrl, bool yn)
{
    ctrl->ctc_is_user_msg = yn;
}

uint16_t ompi_ctc_get_routing_type(ompi_ct_ctrl_t *ctrl);
inline uint16_t ompi_ctc_get_routing_type(ompi_ct_ctrl_t *ctrl) 
{
  return ctrl->ctc_routing_type;
}
void ompi_ctc_set_routing_type(ompi_ct_ctrl_t *ctrl, int rtype);
inline void ompi_ctc_set_routing_type(ompi_ct_ctrl_t *ctrl, int rtype)
{
    ctrl->ctc_routing_type = rtype;
}

uint32_t ompi_ctc_get_sender(ompi_ct_ctrl_t *ctrl);
inline uint32_t ompi_ctc_get_sender(ompi_ct_ctrl_t *ctrl) 
{
  return ctrl->ctc_sender;
}
void ompi_ctc_set_sender(ompi_ct_ctrl_t *ctrl, uint32_t sender);
inline void ompi_ctc_set_sender(ompi_ct_ctrl_t *ctrl, uint32_t sender)
{
    ctrl->ctc_sender = sender;
}

uint32_t ompi_ctc_get_dest(ompi_ct_ctrl_t *ctrl);
inline uint32_t ompi_ctc_get_dest(ompi_ct_ctrl_t *ctrl) 
{
  return ctrl->ctc_dest;
}
void ompi_ctc_set_dest(ompi_ct_ctrl_t *ctrl, uint32_t dest);
inline void ompi_ctc_set_dest(ompi_ct_ctrl_t *ctrl, uint32_t dest)
{
    ctrl->ctc_dest = dest;
}

uint32_t ompi_ctc_get_forwarding(ompi_ct_ctrl_t *ctrl);
inline uint32_t ompi_ctc_get_forwarding(ompi_ct_ctrl_t *ctrl) 
{
  return ctrl->ctc_forwarding;
}
void ompi_ctc_set_forwarding(ompi_ct_ctrl_t *ctrl, uint32_t node);
inline void ompi_ctc_set_forwarding(ompi_ct_ctrl_t *ctrl, uint32_t node)
{
    ctrl->ctc_forwarding = node;
}

uint8_t *ompi_ctc_get_info(ompi_ct_ctrl_t *ctrl, uint32_t *len);
inline uint8_t *ompi_ctc_get_info(ompi_ct_ctrl_t *ctrl, uint32_t *len)
{
    *len = ctrl->ctc_info_len;
    return ctrl->ctc_info;
}

void ompi_ctc_set_info(ompi_ct_ctrl_t *ctrl, uint32_t len, uint8_t *info);
inline void ompi_ctc_set_info(ompi_ct_ctrl_t *ctrl, uint32_t len, uint8_t *info)
{
    ctrl->ctc_info_len = len;
    ctrl->ctc_info = info;
}



/*
 *
 *  Message interface
 *
 */


typedef struct ompi_ctmsg
{
    ompi_object_t    super;
    ompi_ct_ctrl_t   *ctm_ctrl;
    uint32_t        ctm_len;
    uint8_t         *ctm_data;
    int             ctm_should_free;
} ompi_ctmsg_t;


void ompi_ctm_construct(ompi_ctmsg_t *msg);
void ompi_ctm_destruct(ompi_ctmsg_t *msg);

ompi_ctmsg_t *ompi_ctm_create_with(int is_user_msg, int routing_type,
                                 uint32_t sender,
                                 uint32_t dest, uint8_t *data,
                                 uint32_t data_len,
                                 int should_free);

uint8_t *ompi_ctm_pack(ompi_ctmsg_t *msg);
ompi_ctmsg_t *ompi_ctm_unpack(uint8_t *buffer);

/*
 * Functions for accessing data in packed
 * msg struct.
 */

uint8_t *ompi_pk_ctm_get_control(uint8_t *buffer);
uint8_t *ompi_pk_ctm_get_data(uint8_t *buffer, uint32_t *len);

/*
 *
 * Accessor functions
 *
 */

ompi_ct_ctrl_t *ompi_ctm_get_control(ompi_ctmsg_t *msg);
inline ompi_ct_ctrl_t *ompi_ctm_get_control(ompi_ctmsg_t *msg) 
{
  return msg->ctm_ctrl;
}
void ompi_ctm_set_control(ompi_ctmsg_t *msg, ompi_ct_ctrl_t *ctrl);
inline void ompi_ctm_set_control(ompi_ctmsg_t *msg, ompi_ct_ctrl_t *ctrl)
{
    msg->ctm_ctrl = ctrl;
}


#endif  /* OMPI_CT_MESSAGE_H */
