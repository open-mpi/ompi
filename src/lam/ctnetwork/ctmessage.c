/*
 * $HEADER$
 */

#include "lam/ctnetwork/ctmessage.h"


lam_class_t lam_ct_ctrl_t_class = {
    "lam_ct_ctrl_t",
    OBJ_CLASS(lam_object_t),
    (lam_construct_t) lam_ctc_construct,
    (lam_destruct_t) lam_ctc_destruct
};


lam_class_t lam_ctmsg_t_class = {
    "lam_ctmsg_t",
    OBJ_CLASS(lam_object_t),
    (lam_construct_t) lam_ctm_construct,
    (lam_destruct_t) lam_ctm_destruct
};


static const uint32_t ctrl_alloc_len = sizeof(lam_ct_ctrl_t) -
    sizeof(lam_object_t) - sizeof(ctrl->ctc_info);

void lam_ctc_construct(lam_ct_ctrl_t *ctrl)
{
    ctrl->ctc_is_user_msg = 0;
    ctrl->ctc_routing_type = LAM_CT_PT2PT;
    ctrl->ctc_sender = 0;
    ctrl->ctc_dest = 0;
    ctrl->ctc_forwarding = 0;
    ctrl->ctc_client_tag = 0;
    ctrl->ctc_info_len = 0;
    ctrl->ctc_info = 0;
}



void lam_ctc_destruct(lam_ct_ctrl_t *ctrl)
{
    lam_free(ctrl->ctc_info);
}



void lam_ctc_construct_with(lam_ct_ctrl_t *ctrl, int routing_type,
                            uint32_t sender, uint32_t dest)
{
    ctrl->ctc_routing_type = routing_type;
    ctrl->ctc_sender = sender;
    ctrl->ctc_dest = dest;
}


uint32_t lam_ctc_pack_size(lam_ct_ctrl_t *ctrl)
{
    return ctrl_alloc_len + ctrl->ctc_info_len;
}

uint8_t *lam_ctc_pack(lam_ct_ctrl_t *ctrl, uint32_t *len)
{
    /* ASSERT: packed control struct looks like
       <ctc_is_user_msg (uint16_t)><ctc_routing_type (uint16_t)>
       <ctc_sender (uint32_t)><ctc_dest (uint32_t)>
       <ctc_forwarding (uint32_t)><ctc_client_tag (uint32_t)>
       <ctc_info_len (uint32_t)><ctc_info (uint8_t *)>
     */
    uint8_t *buffer;

    buffer = (uint8_t *) lam_malloc(ctrl_alloc_len + ctrl->ctc_info_len);
    if (0 == buffer) {
        return 0;
    }
    lam_ctc_pack_buffer(ctrl, buffer, len);

    return buffer;
}



lam_ct_ctrl_t *lam_ctc_unpack(uint8_t *buffer)
{
    lam_ct_ctrl_t *ctrl;

    /* ASSERT: packed control struct looks like
       <ctc_is_user_msg (uint16_t)><ctc_routing_type (uint16_t)>
       <ctc_sender (uint32_t)><ctc_dest (uint32_t)>
       <ctc_forwarding (uint32_t)><ctc_client_tag (uint32_t)>
       <ctc_info_len (uint32_t)><ctc_info (uint8_t *)>
     */
    ctrl = OBJ_NEW(lam_ct_ctrl_t);
    if (0 == ctrl) {
        return 0;
    }

    memcpy(&(ctrl->ctc_is_user_msg), buffer, ctrl_alloc_len);
    ctrl->ctc_info = (uint8_t *) lam_malloc(ctrl->ctc_info_len);
    if (0 == ctrl->ctc_info) {
        OBJ_RELEASE(ctrl);
        return 0;
    }
    memcpy(ctrl->ctc_info, buffer + ctrl_alloc_len, ctrl->ctc_info_len);

    return ctrl;
}



int lam_ctc_pack_buffer(lam_ct_ctrl_t *ctrl, uint8_t *buffer,
                        uint32_t *len)
{
    int ret = 0;

    memcpy(buffer, &(ctrl->ctc_is_user_msg), ctrl_alloc_len);
    memcpy(buffer + ctrl_alloc_len, ctrl->ctc_info);
    *len = ctrl_alloc_len + ctrl->ctc_info_len;

    return ret;
}



int lam_ctc_unpack_buffer(lam_ct_ctrl_t *ctrl, uint8_t *buffer,
                          uint32_t *len)
{
    int ret = 0;

    return ret;
}



/*
 * Functions for accessing data in packed
 * control struct.
 */

uint16_t lam_pk_ctc_is_user_msg(uint8_t *buffer)
{
    return *((uint16_t *) buffer);
}

uint16_t lam_pk_ctc_get_routing_type(uint8_t *buffer)
{
    return *(uint16_t *) (buffer + sizeof(uint16_t));
}

uint32_t lam_pk_ctc_get_sender(uint8_t *buffer)
{
    return *(uint32_t *) (buffer + 2 * sizeof(uint16_t));
}

uint32_t lam_pkctc_get_dest(uint8_t *buffer)
{
    return *(uint32_t *) (buffer + 2 * sizeof(uint16_t) +
                          sizeof(uint32_t));
}

uint32_t lam_pk_ctc_get_forwarding(uint8_t *buffer)
{
    return *(uint32_t *) (buffer + 2 * sizeof(uint16_t)
                          + 2 * sizeof(uint32_t));
}

void lam_pk_ctc_set_forwarding(uint8_t *buffer, uint32_t node)
{
    memcpy(buffer + 2 * sizeof(uint16_t) + 2 * sizeof(uint32_t),
           &node, sizeof(node));
}

uint8_t *lam_pk_ctc_get_info(uint8_t *buffer, uint32_t *len)
{
    memcpy(len, buffer + ctrl_alloc_len - sizeof(uint32_t),
           sizeof(uint32_t));
    return buffer + ctrl_alloc_len;
}

void lam_pk_ctc_set_info(uint8_t *buffer, uint8_t *info)
{
    uint32_t len;

    memcpy(&len, buffer + ctrl_alloc_len - sizeof(uint32_t),
           sizeof(uint32_t));
    memcpy(buffer + ctrl_alloc_len, info, len);
}


/*
 *
 *  Message interface
 *
 */


void lam_ctm_construct(lam_ctmsg_t *msg)
{
    msg->ctm_ctrl = OBJ_NEW(lam_ct_ctrl_t);
    msg->ctm_len = 0;
    msg->ctm_data = 0;
    msg->ctm_should_free = 1;
}

void lam_ctm_destruct(lam_ctmsg_t *msg)
{
    if (msg->ctm_should_free) {
        lam_free(msg->ctm_data);
    }
    OBJECT_RELEASE(msg->ctm_ctrl);
}

lam_ctmsg_t *lam_ctm_create_with(int is_user_msg, int routing_type,
                                 uint32_t sender,
                                 uint32_t dest, uint8_t *data,
                                 uint32_t data_len, int should_free)
{
    lam_ctmsg_t *msg;

    msg = OBJ_NEW(lam_ctmsg_t);
    if (0 == msg) {
        return 0;
    }

    OBJ_CONSTRUCT(&msg->ctm_ctrl, lam_ct_ctrl_t);
    lam_ctc_construct_with(&(msg->ctm_ctrl), sender, dest);
    msg->ctm_should_free = should_free;
    msg->ctm_data = data;
    msg->ctm_len = data_len;

    return msg;
}

uint8_t *lam_ctm_pack(lam_ctmsg_t *msg)
{
    /* packed msg layout
       <msg len (uint32_t)><packed ctrl><data len (uint32_t)>
       <data>
     */
    uint32_t len;


}

lam_ctmsg_t *lam_ctm_unpack(uint8_t *buffer)
{
    /* packed msg layout
       <msg len (uint32_t)><packed ctrl><data len (uint32_t)>
       <data>
     */

}

/*
 * Functions for accessing data in packed
 * msg struct.
 */

uint8_t *lam_pk_ctm_get_control(uint8_t *buffer)
{

}

uint8_t *lam_pk_ctm_get_data(uint8_t *buffer, uint32_t *len)
{

}
