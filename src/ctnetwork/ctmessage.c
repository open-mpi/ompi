/*
 * $HEADER$
 */

#include "ctnetwork/ctmessage.h"


ompi_class_t ompi_ct_ctrl_t_class = {
    "ompi_ct_ctrl_t",
    OBJ_CLASS(ompi_object_t),
    (ompi_construct_t) ompi_ctc_construct,
    (ompi_destruct_t) ompi_ctc_destruct
};


ompi_class_t ompi_ctmsg_t_class = {
    "ompi_ctmsg_t",
    OBJ_CLASS(ompi_object_t),
    (ompi_construct_t) ompi_ctm_construct,
    (ompi_destruct_t) ompi_ctm_destruct
};


static const uint32_t ctrl_alloc_len = sizeof(ompi_ct_ctrl_t) -
    sizeof(ompi_object_t) - sizeof(ctrl->ctc_info);

void ompi_ctc_construct(ompi_ct_ctrl_t *ctrl)
{
    ctrl->ctc_is_user_msg = 0;
    ctrl->ctc_routing_type = OMPI_CT_PT2PT;
    ctrl->ctc_sender = 0;
    ctrl->ctc_dest = 0;
    ctrl->ctc_forwarding = 0;
    ctrl->ctc_client_tag = 0;
    ctrl->ctc_info_len = 0;
    ctrl->ctc_info = 0;
}



void ompi_ctc_destruct(ompi_ct_ctrl_t *ctrl)
{
    ompi_free(ctrl->ctc_info);
}



void ompi_ctc_construct_with(ompi_ct_ctrl_t *ctrl, int routing_type,
                            uint32_t sender, uint32_t dest)
{
    ctrl->ctc_routing_type = routing_type;
    ctrl->ctc_sender = sender;
    ctrl->ctc_dest = dest;
}


uint32_t ompi_ctc_pack_size(ompi_ct_ctrl_t *ctrl)
{
    return ctrl_alloc_len + ctrl->ctc_info_len;
}

uint8_t *ompi_ctc_pack(ompi_ct_ctrl_t *ctrl, uint32_t *len)
{
    /* ASSERT: packed control struct looks like
       <ctc_is_user_msg (uint16_t)><ctc_routing_type (uint16_t)>
       <ctc_sender (uint32_t)><ctc_dest (uint32_t)>
       <ctc_forwarding (uint32_t)><ctc_client_tag (uint32_t)>
       <ctc_info_len (uint32_t)><ctc_info (uint8_t *)>
     */
    uint8_t *buffer;

    buffer = (uint8_t *) ompi_malloc(ctrl_alloc_len + ctrl->ctc_info_len);
    if (0 == buffer) {
        return 0;
    }
    ompi_ctc_pack_buffer(ctrl, buffer, len);

    return buffer;
}



ompi_ct_ctrl_t *ompi_ctc_unpack(uint8_t *buffer)
{
    ompi_ct_ctrl_t *ctrl;

    /* ASSERT: packed control struct looks like
       <ctc_is_user_msg (uint16_t)><ctc_routing_type (uint16_t)>
       <ctc_sender (uint32_t)><ctc_dest (uint32_t)>
       <ctc_forwarding (uint32_t)><ctc_client_tag (uint32_t)>
       <ctc_info_len (uint32_t)><ctc_info (uint8_t *)>
     */
    ctrl = OBJ_NEW(ompi_ct_ctrl_t);
    if (0 == ctrl) {
        return 0;
    }

    memcpy(&(ctrl->ctc_is_user_msg), buffer, ctrl_alloc_len);
    ctrl->ctc_info = (uint8_t *) ompi_malloc(ctrl->ctc_info_len);
    if (0 == ctrl->ctc_info) {
        OBJ_RELEASE(ctrl);
        return 0;
    }
    memcpy(ctrl->ctc_info, buffer + ctrl_alloc_len, ctrl->ctc_info_len);

    return ctrl;
}



int ompi_ctc_pack_buffer(ompi_ct_ctrl_t *ctrl, uint8_t *buffer,
                        uint32_t *len)
{
    int ret = 0;

    memcpy(buffer, &(ctrl->ctc_is_user_msg), ctrl_alloc_len);
    memcpy(buffer + ctrl_alloc_len, ctrl->ctc_info);
    *len = ctrl_alloc_len + ctrl->ctc_info_len;

    return ret;
}



int ompi_ctc_unpack_buffer(ompi_ct_ctrl_t *ctrl, uint8_t *buffer,
                          uint32_t *len)
{
    int ret = 0;

    return ret;
}



/*
 * Functions for accessing data in packed
 * control struct.
 */

uint16_t ompi_pk_ctc_is_user_msg(uint8_t *buffer)
{
    return *((uint16_t *) buffer);
}

uint16_t ompi_pk_ctc_get_routing_type(uint8_t *buffer)
{
    return *(uint16_t *) (buffer + sizeof(uint16_t));
}

uint32_t ompi_pk_ctc_get_sender(uint8_t *buffer)
{
    return *(uint32_t *) (buffer + 2 * sizeof(uint16_t));
}

uint32_t ompi_pkctc_get_dest(uint8_t *buffer)
{
    return *(uint32_t *) (buffer + 2 * sizeof(uint16_t) +
                          sizeof(uint32_t));
}

uint32_t ompi_pk_ctc_get_forwarding(uint8_t *buffer)
{
    return *(uint32_t *) (buffer + 2 * sizeof(uint16_t)
                          + 2 * sizeof(uint32_t));
}

void ompi_pk_ctc_set_forwarding(uint8_t *buffer, uint32_t node)
{
    memcpy(buffer + 2 * sizeof(uint16_t) + 2 * sizeof(uint32_t),
           &node, sizeof(node));
}

uint8_t *ompi_pk_ctc_get_info(uint8_t *buffer, uint32_t *len)
{
    memcpy(len, buffer + ctrl_alloc_len - sizeof(uint32_t),
           sizeof(uint32_t));
    return buffer + ctrl_alloc_len;
}

void ompi_pk_ctc_set_info(uint8_t *buffer, uint8_t *info)
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


void ompi_ctm_construct(ompi_ctmsg_t *msg)
{
    msg->ctm_ctrl = OBJ_NEW(ompi_ct_ctrl_t);
    msg->ctm_len = 0;
    msg->ctm_data = 0;
    msg->ctm_should_free = 1;
}

void ompi_ctm_destruct(ompi_ctmsg_t *msg)
{
    if (msg->ctm_should_free) {
        ompi_free(msg->ctm_data);
    }
    OBJECT_RELEASE(msg->ctm_ctrl);
}

ompi_ctmsg_t *ompi_ctm_create_with(int is_user_msg, int routing_type,
                                 uint32_t sender,
                                 uint32_t dest, uint8_t *data,
                                 uint32_t data_len, int should_free)
{
    ompi_ctmsg_t *msg;

    msg = OBJ_NEW(ompi_ctmsg_t);
    if (0 == msg) {
        return 0;
    }

    OBJ_CONSTRUCT(&msg->ctm_ctrl, ompi_ct_ctrl_t);
    ompi_ctc_construct_with(&(msg->ctm_ctrl), sender, dest);
    msg->ctm_should_free = should_free;
    msg->ctm_data = data;
    msg->ctm_len = data_len;

    return msg;
}

uint8_t *ompi_ctm_pack(ompi_ctmsg_t *msg)
{
    /* packed msg layout
       <msg len (uint32_t)><packed ctrl><data len (uint32_t)>
       <data>
     */
    uint32_t len;


}

ompi_ctmsg_t *ompi_ctm_unpack(uint8_t *buffer)
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

uint8_t *ompi_pk_ctm_get_control(uint8_t *buffer)
{

}

uint8_t *ompi_pk_ctm_get_data(uint8_t *buffer, uint32_t *len)
{

}
