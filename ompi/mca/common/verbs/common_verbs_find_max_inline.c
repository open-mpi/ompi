/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <infiniband/verbs.h>
#include <stdint.h>

#include "opal_stdint.h"
#include "opal/types.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/class/opal_object.h"
#include "opal/util/show_help.h"

#include "ompi/mca/rte/rte.h"
#include "ompi/constants.h"

#include "common_verbs.h"

/* Horrible.  :-( Per the thread starting here:
   http://lists.openfabrics.org/pipermail/general/2008-June/051822.html,
   we can't rely on the value reported by the device to determine the
   maximum max_inline_data value.  So we have to search by looping
   over max_inline_data values and trying to make dummy QPs.  Yuck! */
int ompi_common_verbs_find_max_inline(struct ibv_device *device,
                                      struct ibv_context *context,
                                      struct ibv_pd *pd,
				      uint32_t *max_inline_arg)
{
    int ret;
    struct ibv_qp *qp;
    struct ibv_cq *cq;
    struct ibv_qp_init_attr init_attr;
    uint32_t max_inline_data;
    
    *max_inline_arg = 0;

    /* Make a dummy CQ */
#if OMPI_IBV_CREATE_CQ_ARGS == 3
    cq = ibv_create_cq(context, 1, NULL);
#else
    cq = ibv_create_cq(context, 1, NULL, NULL, 0);
#endif
    if (NULL == cq) {
        opal_show_help("help-mpi-btl-openib.txt", "init-fail-create-q",
                       true, ompi_process_info.nodename,
                       __FILE__, __LINE__, "ibv_create_cq",
                       strerror(errno), errno,
                       ibv_get_device_name(device));
        return OMPI_ERR_NOT_AVAILABLE;
    }
    
    /* Setup the QP attributes */
    memset(&init_attr, 0, sizeof(init_attr));
    init_attr.qp_type = IBV_QPT_RC;
    init_attr.send_cq = cq;
    init_attr.recv_cq = cq;
    init_attr.srq = 0;
    init_attr.cap.max_send_sge = 1;
    init_attr.cap.max_recv_sge = 1;
    init_attr.cap.max_recv_wr = 1;
    
    /* Loop over max_inline_data values; just check powers of 2 --
       that's good enough */
    init_attr.cap.max_inline_data = max_inline_data = 1 << 20;
    ret = OMPI_ERR_NOT_FOUND;
    while (max_inline_data > 0) {
        qp = ibv_create_qp(pd, &init_attr);
        if (NULL != qp) {
            *max_inline_arg = max_inline_data;
            ibv_destroy_qp(qp);
            ret = OMPI_SUCCESS;
            break;
        }
        max_inline_data >>= 1;
        init_attr.cap.max_inline_data = max_inline_data;
    }
    
    /* Destroy the temp CQ */
    ibv_destroy_cq(cq);

    return ret;
}
