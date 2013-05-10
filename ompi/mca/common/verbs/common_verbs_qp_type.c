/*
 * Copyright (c) 2012-2013 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <infiniband/verbs.h>

#include "ompi/constants.h"

#include "common_verbs.h"

/*
 * It seems you can't probe a device / port to see if it supports a
 * specific type of QP.  You just have to try to make it and see if it
 * works.  This is a short helper function to try to make a QP of a
 * specific type and return whether it worked.
 */
static bool make_qp(struct ibv_pd *pd, struct ibv_cq *cq, enum ibv_qp_type type)
{
    struct ibv_qp_init_attr qpia;
    struct ibv_qp *qp;

    memset(&qpia, 0, sizeof(qpia));
    qpia.qp_context = NULL;
    qpia.send_cq = cq;
    qpia.recv_cq = cq;
    qpia.srq = NULL;
    qpia.cap.max_send_wr = 1;
    qpia.cap.max_recv_wr = 1;
    qpia.cap.max_send_sge = 1;
    qpia.cap.max_recv_sge = 1;
    qpia.cap.max_inline_data = 0;
    qpia.qp_type = type;
    qpia.sq_sig_all = 0;
    
    qp = ibv_create_qp(pd, &qpia);
    if (NULL != qp) {
        ibv_destroy_qp(qp);
        return true;
    }

    return false;
}

int ompi_common_verbs_qp_test(struct ibv_context *device_context, int flags)
{
    int rc = OMPI_SUCCESS;
    struct ibv_pd *pd = NULL;
    struct ibv_cq *cq = NULL;

    /* Bozo check */
    if (NULL == device_context || 
        (0 == (flags & (OMPI_COMMON_VERBS_FLAGS_RC | OMPI_COMMON_VERBS_FLAGS_UD)))) {
        return OMPI_ERR_BAD_PARAM;       
    }

    /* Try to make both the PD and CQ */
    pd = ibv_alloc_pd(device_context);
    if (NULL == pd) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    cq = ibv_create_cq(device_context, 2, NULL, NULL, 0);
    if (NULL == cq) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out;
    }

    /* Now try to make the QP(s) of the desired type(s) */
    if (flags & OMPI_COMMON_VERBS_FLAGS_RC &&
        !make_qp(pd, cq, IBV_QPT_RC)) {
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto out;
    }
    if (flags & OMPI_COMMON_VERBS_FLAGS_NOT_RC &&
        make_qp(pd, cq, IBV_QPT_RC)) {
        rc = OMPI_ERR_TYPE_MISMATCH;
        goto out;
    }
    if (flags & OMPI_COMMON_VERBS_FLAGS_UD &&
        !make_qp(pd, cq, IBV_QPT_UD)) {
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto out;
    }

 out:
    /* Free the PD and/or CQ */
    if (NULL != pd) {
        ibv_dealloc_pd(pd);
    }
    if (NULL != cq) {
        ibv_destroy_cq(cq);
    }

    return rc;
}
