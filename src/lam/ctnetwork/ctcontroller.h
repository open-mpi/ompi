/*
 * $HEADER$
 */

#ifndef LAM_CT_CONTROLLER_H
#define LAM_CT_CONTROLLER_H

#include "lam/lfc/lam_object.h"
#include "lam/ctnetwork/ctnode.h"

typedef void    (*lam_ctmsg_recvd_fn)(struct lam_ctcontroller *,
                                            lam_ctmsg_t *,
                                            void *);

typedef void    (*lam_ctnode_failed_fn)(struct lam_ctcontroller *,
                                            lam_ctnode_t *,
                                            void *);

typedef struct lam_ctcontroller
{
    lam_object_t    super;
    lam_ctnode_t    ctl_node;
    void            *ctl_user_info;
    lam_ctmsg_recvd_fn      ctl_msg_recvd_callback;
    lam_ctnode_failed_fn    ctl_node_failed_callback;
} lam_ctctrl_t;

void lam_ctl_construct(lam_ctctrl_t *ctrl);
void lam_ctl_destruct(lam_ctctrl_t *ctrl);

inline void lam_ctl_set_recvd_callback(lam_ctctrl_t *ctrl, lam_ctmsg_recvd_fn callback)
{
    ctrl->ctl_msg_recvd_callback = callback;
}


inline void lam_ctl_set_failed_callback(lam_ctctrl_t *ctrl, lam_ctnode_failed_fn callback)
{
    ctrl->ctl_node_failed_callback = callback;
}


#endif  /* LAM_CT_CONTROLLER_H */
