/*
 * Copyright 2002-2003. The Regents of the University of California. This material
 * was produced under U.S. Government contract W-7405-ENG-36 for Los Alamos
 * National Laboratory, which is operated by the University of California for
 * the U.S. Department of Energy. The Government is granted for itself and
 * others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
 * license in this material to reproduce, prepare derivative works, and
 * perform publicly and display publicly. Beginning five (5) years after
 * October 10,2002 subject to additional five-year worldwide renewals, the
 * Government is granted for itself and others acting on its behalf a paid-up,
 * nonexclusive, irrevocable worldwide license in this material to reproduce,
 * prepare derivative works, distribute copies to the public, perform publicly
 * and display publicly, and to permit others to do so. NEITHER THE UNITED
 * STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR THE UNIVERSITY OF
 * CALIFORNIA, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR
 * IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
 * COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR
 * PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY
 * OWNED RIGHTS.
 
 * Additionally, this program is free software; you can distribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or any later version.  Accordingly, this program is distributed in the hope
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "runtime/ctnetwork/ctmessage.h"
#include "lam/util/malloc.h"


lam_class_info_t     lam_ctctrl_cls = {"lam_ct_ctrl_t", &lam_object_cls, 
    (class_init_t) lam_ctc_init, (class_destroy_t)lam_ctc_destroy};

lam_class_info_t     lam_ctmsg_cls = {"lam_ctmsg_t", &lam_object_cls, 
    (class_init_t) lam_ctm_init, (class_destroy_t)lam_ctm_destroy};


static const uint32_t ctrl_alloc_len = sizeof(lam_ct_ctrl_t) -
                                        sizeof(lam_object_t) -
                                        sizeof(ctrl->ctc_info);

void lam_ctc_init(lam_ct_ctrl_t *ctrl)
{
    SUPER_INIT(ctrl, lam_ctctrl_cls.cls_parent);
    
    ctrl->ctc_is_user_msg = 0;
    ctrl->ctc_routing_type = LAM_CT_PT2PT;
    ctrl->ctc_sender = 0;
    ctrl->ctc_dest = 0;
    ctrl->ctc_forwarding = 0;
    ctrl->ctc_client_tag = 0;
    ctrl->ctc_info_len = 0;
    ctrl->ctc_info = 0;
}



void lam_ctc_destroy(lam_ct_ctrl_t *ctrl)
{
    lam_free(ctrl->ctc_info);
    SUPER_DESTROY(ctrl, lam_ctctrl_cls.cls_parent);
}



void lam_ctc_init_with(lam_ct_ctrl_t *ctrl, int routing_type,
                       uint32_t sender,
                       uint32_t dest)
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
    uint8_t     *buffer;
    
    buffer = (uint8_t *)lam_malloc(ctrl_alloc_len 
                                   + ctrl->ctc_info_len);
    if ( 0 == buffer )
    {
        return 0;        
    }
    lam_ctc_pack_buffer(ctrl, buffer, len);
    
    return buffer;
}



lam_ct_ctrl_t *lam_ctc_unpack(uint8_t *buffer)
{
    lam_ct_ctrl_t       *ctrl;
 
    /* ASSERT: packed control struct looks like
        <ctc_is_user_msg (uint16_t)><ctc_routing_type (uint16_t)>
        <ctc_sender (uint32_t)><ctc_dest (uint32_t)>
        <ctc_forwarding (uint32_t)><ctc_client_tag (uint32_t)>
        <ctc_info_len (uint32_t)><ctc_info (uint8_t *)>
    */
    CREATE_OBJECT(ctrl, lam_ct_ctrl_t, &lam_ctctrl_cls);
    if ( 0 == ctrl )
    {
        return 0;        
    }
    
    memcpy(&(ctrl->ctc_is_user_msg), buffer, ctrl_alloc_len);
    ctrl->ctc_info = (uint8_t *)lam_malloc(ctrl->ctc_info_len);
    if ( 0 == ctrl->ctc_info )
    {
        OBJ_RELEASE(ctrl);
        return 0;
    }
    memcpy(ctrl->ctc_info, buffer + ctrl_alloc_len, ctrl->ctc_info_len);
    
    return ctrl;
}



int lam_ctc_pack_buffer(lam_ct_ctrl_t *ctrl, uint8_t *buffer, uint32_t *len)
{
    int         ret = 0;
        
    memcpy(buffer, &(ctrl->ctc_is_user_msg), ctrl_alloc_len);
    memcpy(buffer + ctrl_alloc_len, ctrl->ctc_info);
    *len = ctrl_alloc_len + ctrl->ctc_info_len;
    
    return ret;
}



int lam_ctc_unpack_buffer(lam_ct_ctrl_t *ctrl, uint8_t *buffer, uint32_t *len)
{    
    int         ret = 0;
    
    return ret;
}



/*
 * Functions for accessing data in packed
 * control struct.
 */

uint16_t lam_pk_ctc_is_user_msg(uint8_t *buffer)
{
    return *((uint16_t *)buffer);
}

uint16_t lam_pk_ctc_get_routing_type(uint8_t *buffer)
{
    return *(uint16_t *)(buffer + sizeof(uint16_t));    
}

uint32_t lam_pk_ctc_get_sender(uint8_t *buffer)
{
    return *(uint32_t *)(buffer + 2*sizeof(uint16_t));    
}

uint32_t lam_pkctc_get_dest(uint8_t *buffer)
{
    return *(uint32_t *)(buffer + 2*sizeof(uint16_t) + sizeof(uint32_t));    
}

uint32_t lam_pk_ctc_get_forwarding(uint8_t *buffer)
{
    return *(uint32_t *)(buffer + 2*sizeof(uint16_t)
                         + 2*sizeof(uint32_t));    
}

void lam_pk_ctc_set_forwarding(uint8_t *buffer, uint32_t node)
{
    memcpy(buffer + 2*sizeof(uint16_t) + 2*sizeof(uint32_t),
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
    uint32_t        len;
        
    memcpy(&len, buffer + ctrl_alloc_len - sizeof(uint32_t),
           sizeof(uint32_t));
    memcpy(buffer + ctrl_alloc_len, info, len);
}


/*
 *
 *  Message interface
 *
 */


void lam_ctm_init(lam_ctmsg_t *msg)
{
    SUPER_INIT(msg, lam_ctmsg_cls.cls_parent);
    CREATE_OBJECT(msg->ctm_ctrl, lam_ct_ctrl_t, &lam_ctctrl_cls);
    msg->ctm_len = 0;
    msg->ctm_data = 0;
    msg->ctm_should_free = 1;
}

void lam_ctm_destroy(lam_ctmsg_t *msg)
{
    if ( msg->ctm_should_free )
    {
        lam_free(msg->ctm_data);        
    }
    OBJECT_RELEASE(msg->ctm_ctrl);
    SUPER_DESTROY(msg, lam_ctmsg_cls.cls_parent);
}

lam_ctmsg_t *lam_ctm_create_with(int is_user_msg, int routing_type,
                                 uint32_t sender,
                                 uint32_t dest, uint8_t *data,
                                 uint32_t data_len,
                                 int should_free)
{
    lam_ctmsg_t     *msg;
    
    CREATE_OBJECT(msg, lam_ctmsg_t, &lam_ctmsg_cls);
    if ( 0 == msg )
    {
        return 0;        
    }
    
    STATIC_INIT(msg->ctm_ctrl, &lam_ctctrl_cls);
    lam_ctc_init_with(&(msg->ctm_ctrl), sender, dest);
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
    uint32_t        len;
    
    
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




