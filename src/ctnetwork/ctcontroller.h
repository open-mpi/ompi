/*
 * $HEADER$
 */

#ifndef OMPI_CT_CONTROLLER_H
#define OMPI_CT_CONTROLLER_H

#include "class/ompi_object.h"
#include "ctnetwork/ctnode.h"

typedef void    (*ompi_ctmsg_recvd_fn)(struct ompi_ctcontroller *,
                                            ompi_ctmsg_t *,
                                            void *);

typedef void    (*ompi_ctnode_failed_fn)(struct ompi_ctcontroller *,
                                            ompi_ctnode_t *,
                                            void *);

typedef struct ompi_ctcontroller
{
    ompi_object_t    super;
    ompi_ctnode_t    ctl_node;
    void            *ctl_user_info;
    ompi_ctmsg_recvd_fn      ctl_msg_recvd_callback;
    ompi_ctnode_failed_fn    ctl_node_failed_callback;
} ompi_ctctrl_t;

void ompi_ctl_construct(ompi_ctctrl_t *ctrl);
void ompi_ctl_destruct(ompi_ctctrl_t *ctrl);

inline void ompi_ctl_set_recvd_callback(ompi_ctctrl_t *ctrl, ompi_ctmsg_recvd_fn callback)
{
    ctrl->ctl_msg_recvd_callback = callback;
}


inline void ompi_ctl_set_failed_callback(ompi_ctctrl_t *ctrl, ompi_ctnode_failed_fn callback)
{
    ctrl->ctl_node_failed_callback = callback;
}


#endif  /* OMPI_CT_CONTROLLER_H */
