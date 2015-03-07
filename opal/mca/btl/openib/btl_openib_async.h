/*
 * Copyright (c) 2007-2008 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

#ifndef MCA_BTL_OPENIB_ASYNC_H
#define MCA_BTL_OPENIB_ASYNC_H
#include "btl_openib_endpoint.h"

int        start_async_event_thread(void);
void       mca_btl_openib_load_apm(struct ibv_qp *qp, mca_btl_openib_endpoint_t *ep);
int        btl_openib_async_command_done(int exp);
#if HAVE_XRC && ! OPAL_HAVE_CONNECTX_XRC_DOMAINS
void       mca_btl_openib_load_apm_xrc_rcv(uint32_t qp_num, mca_btl_openib_endpoint_t *ep);
#endif

#define APM_ENABLED (0 != mca_btl_openib_component.apm_lmc || 0 != mca_btl_openib_component.apm_ports)

/*
 * Command types for communicating with the async thread
 */
typedef enum {
    OPENIB_ASYNC_CMD_FD_ADD,
    OPENIB_ASYNC_CMD_FD_REMOVE,
    OPENIB_ASYNC_IGNORE_QP_ERR,
    OPENIB_ASYNC_THREAD_EXIT
} btl_openib_async_cmd_type_t;

typedef struct {
    btl_openib_async_cmd_type_t a_cmd;
    int fd;
    struct ibv_qp *qp;
} mca_btl_openib_async_cmd_t;

#endif
