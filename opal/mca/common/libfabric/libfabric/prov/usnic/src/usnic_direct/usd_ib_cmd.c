/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
 *
 * LICENSE_BEGIN
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * LICENSE_END
 *
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <sys/stat.h>
#include <inttypes.h>

#include <infiniband/driver.h>

#include "kcompat.h"
#include "usnic_ib_abi.h"

#include "usnic_direct.h"
#include "usd.h"
#include "usd_ib_cmd.h"

int
usd_ib_cmd_get_context(
    struct usd_device *dev)
{
    struct usnic_get_context cmd;
    struct usnic_get_context_resp resp;
    struct ibv_get_context *icp;
    struct ibv_get_context_resp *irp;
    struct usnic_ib_get_context_cmd *ucp;
    struct usnic_ib_get_context_resp *urp;
    int n;

    /* clear cmd and response */
    memset(&cmd, 0, sizeof(cmd));
    memset(&resp, 0, sizeof(resp));

    /* fill in the command struct */
    icp = &cmd.ibv_cmd;
    icp->command = IB_USER_VERBS_CMD_GET_CONTEXT;
    icp->in_words = sizeof(cmd) / 4;
    icp->out_words = sizeof(resp) / 4;
    icp->response = (uintptr_t) & resp;

    ucp = &cmd.usnic_cmd;
    ucp->resp_version = USNIC_CTX_RESP_VERSION;
    ucp->num_caps = USNIC_CAP_CNT;

    n = write(dev->ud_ib_dev_fd, &cmd, sizeof(cmd));
    if (n != sizeof(cmd)) {
        return -errno;
    }

    irp = &resp.ibv_resp;
    dev->ud_attrs.uda_event_fd = irp->async_fd;
    //dev->ud_num_comp_vectors = irp->num_comp_vectors;

    urp = &resp.usnic_resp;

    if (urp->resp_version == USNIC_CTX_RESP_VERSION) {
        if (urp->num_caps > USNIC_CAP_CQ_SHARING &&
            urp->cap_info[USNIC_CAP_CQ_SHARING] > 0) {
            dev->ud_caps[USD_CAP_CQ_SHARING] = 1;
        }
        if (urp->num_caps > USNIC_CAP_MAP_PER_RES &&
            urp->cap_info[USNIC_CAP_MAP_PER_RES] > 0) {
            dev->ud_caps[USD_CAP_MAP_PER_RES] = 1;
        }
        if (urp->num_caps > USNIC_CAP_PIO &&
            urp->cap_info[USNIC_CAP_PIO] > 0) {
            dev->ud_caps[USD_CAP_PIO] = 1;
        }
    }

    return 0;
}

/*
 * Create a protection domain
 */
int
usd_ib_cmd_alloc_pd(
    struct usd_device *dev,
    uint32_t *handle_o)
{
    struct usnic_alloc_pd cmd;
    struct usnic_alloc_pd_resp resp;
    struct ibv_alloc_pd *icp;
    struct ibv_alloc_pd_resp *irp;
    int n;

    memset(&cmd, 0, sizeof(cmd));
    memset(&resp, 0, sizeof(resp));

    /* fill in command */
    icp = &cmd.ibv_cmd;
    icp->command = IB_USER_VERBS_CMD_ALLOC_PD;
    icp->in_words = sizeof(cmd) / 4;
    icp->out_words = sizeof(resp) / 4;
    icp->response = (uintptr_t) & resp;

    n = write(dev->ud_ib_dev_fd, &cmd, sizeof(cmd));
    if (n != sizeof(cmd)) {
        return -errno;
    }

    /* process response */
    irp = &resp.ibv_resp;
    *handle_o = irp->pd_handle;

    return 0;
}

int
usd_ib_cmd_reg_mr(
    struct usd_device *dev,
    void *vaddr,
    size_t length,
    struct usd_mr *mr)
{
    struct usnic_reg_mr cmd;
    struct usnic_reg_mr_resp resp;
    struct ibv_reg_mr *icp;
    struct ibv_reg_mr_resp *irp;
    int n;

    memset(&cmd, 0, sizeof(cmd));
    memset(&resp, 0, sizeof(resp));

    icp = &cmd.ibv_cmd;
    icp->command = IB_USER_VERBS_CMD_REG_MR;
    icp->in_words = sizeof(cmd) / 4;
    icp->out_words = sizeof(resp) / 4;
    icp->response = (uintptr_t) & resp;

    icp->start = (uintptr_t) vaddr;
    icp->length = length;
    icp->hca_va = (uintptr_t) vaddr;
    icp->pd_handle = dev->ud_pd_handle;
    icp->access_flags = IBV_ACCESS_LOCAL_WRITE;

    /* Issue command to IB driver */
    n = write(dev->ud_ib_dev_fd, &cmd, sizeof(cmd));
    if (n != sizeof(cmd)) {
        return errno;
    }

    /* process response */
    irp = &resp.ibv_resp;
    mr->umr_handle = irp->mr_handle;
    mr->umr_lkey = irp->lkey;
    mr->umr_rkey = irp->rkey;

    return 0;
}

int
usd_ib_cmd_dereg_mr(
    struct usd_device *dev,
    struct usd_mr *mr)
{
    struct ibv_dereg_mr cmd;
    struct ibv_dereg_mr *icp;
    int n;

    memset(&cmd, 0, sizeof(cmd));

    icp = &cmd;
    icp->command = IB_USER_VERBS_CMD_DEREG_MR;
    icp->in_words = sizeof(cmd) / 4;
    icp->out_words = 0;

    icp->mr_handle = mr->umr_handle;

    /* Issue command to IB driver */
    n = write(dev->ud_ib_dev_fd, &cmd, sizeof(cmd));
    if (n != sizeof(cmd)) {
        return -errno;
    }

    return 0;
}

/*
 * Make the verbs call to create a CQ
 */
int
usd_ib_cmd_create_cq(
    struct usd_device *dev,
    struct usd_cq_impl *cq)
{
    struct usnic_create_cq cmd;
    struct usnic_create_cq_resp resp;
    struct ibv_create_cq *icp;
    struct ibv_create_cq_resp *irp;
    int n;

    memset(&cmd, 0, sizeof(cmd));
    memset(&resp, 0, sizeof(resp));

    icp = &cmd.ibv_cmd;
    icp->command = IB_USER_VERBS_CMD_CREATE_CQ;
    icp->in_words = sizeof(cmd) / 4;
    icp->out_words = sizeof(resp) / 4;
    icp->response = (uintptr_t) & resp;

    icp->user_handle = (uintptr_t) cq;
    icp->cqe = cq->ucq_num_entries;
    icp->comp_channel = -1;
    icp->comp_vector = 0;

    /* Issue command to IB driver */
    n = write(dev->ud_ib_dev_fd, &cmd, sizeof(cmd));
    if (n != sizeof(cmd)) {
        return -errno;
    }

    /* process response */
    irp = &resp.ibv_resp;
    cq->ucq_handle = irp->cq_handle;

    return 0;
}

/*
 * Make the verbs call to destroy a CQ
 */
int
usd_ib_cmd_destroy_cq(
    struct usd_device *dev,
    struct usd_cq_impl *cq)
{
    struct ibv_destroy_cq cmd;
    struct ibv_destroy_cq *icp;
    int n;

    memset(&cmd, 0, sizeof(cmd));

    icp = &cmd;
    icp->command = IB_USER_VERBS_CMD_DESTROY_CQ;
    icp->in_words = sizeof(cmd) / 4;
    icp->out_words = 0;

    icp->cq_handle = cq->ucq_handle;

    /* Issue command to IB driver */
    n = write(dev->ud_ib_dev_fd, &cmd, sizeof(cmd));
    if (n != sizeof(cmd)) {
        return -errno;
    }

    return 0;
}

/*
 * Create a verbs QP without attaching any real resources to it yet
 */
int
usd_ib_cmd_create_qp(
    struct usd_device *dev,
    struct usd_qp_impl *qp,
    struct usd_vf_info *vfip)
{
    struct usnic_create_qp cmd;
    struct usnic_create_qp_resp *resp;
    struct ibv_create_qp *icp;
    struct ibv_create_qp_resp *irp = NULL;
    struct usnic_ib_create_qp_cmd *ucp;
    struct usnic_ib_create_qp_resp *urp;
    struct usd_qp_filter *qfilt;
    int ret;
    int n;
    uint32_t i;

    irp = NULL;
    memset(&cmd, 0, sizeof(cmd));

    resp = calloc(1, sizeof(*resp) +
            RES_TYPE_MAX*sizeof(struct usnic_vnic_barres_info));
    if (resp == NULL) {
        usd_err("Failed to allocate memory for create_qp_resp\n");
        return -ENOMEM;
    }

    icp = &cmd.ibv_cmd;
    icp->command = IB_USER_VERBS_CMD_CREATE_QP;
    icp->in_words = sizeof(cmd) / 4;
    icp->out_words = (sizeof(*resp) +
			RES_TYPE_MAX * sizeof(struct usnic_vnic_barres_info))
			/ 4;
    icp->response = (uintptr_t) resp;

    icp->user_handle = (uintptr_t) qp;
    icp->pd_handle = dev->ud_pd_handle;
    icp->send_cq_handle = qp->uq_wq.uwq_cq->ucq_handle;
    icp->recv_cq_handle = qp->uq_rq.urq_cq->ucq_handle;
    icp->srq_handle = 0;
    icp->max_send_wr = qp->uq_wq.uwq_num_entries;
    icp->max_recv_wr = qp->uq_rq.urq_num_entries;
    icp->max_send_sge = 1;
    icp->max_recv_sge = 1;
    icp->max_inline_data = 1024;
    icp->sq_sig_all = 0;
    icp->qp_type = IBV_QPT_UD;
    icp->is_srq = 0;
    icp->reserved = 0;

    ucp = &cmd.usnic_cmd;
    ucp->cmd_version = USNIC_IB_CREATE_QP_VERSION;
    qfilt = &qp->uq_filter;
    if (qfilt->qf_type == USD_FTY_UDP ||
            qfilt->qf_type == USD_FTY_UDP_SOCK) {
        ucp->spec.trans_type = USNIC_TRANSPORT_IPV4_UDP;
        ucp->spec.udp.sock_fd = qfilt->qf_filter.qf_udp.u_sockfd;
    } else {
        ret = -EINVAL;
        goto out;
    }

    /* Issue command to IB driver */
    n = write(dev->ud_ib_dev_fd, &cmd, sizeof(cmd));
    if (n != sizeof(cmd)) {
        ret = -errno;
        goto out;
    }

    /* process IB part of response */
    irp = &resp->ibv_resp;
    qp->uq_qp_handle = irp->qp_handle;
    qp->uq_qp_num = irp->qpn;

    /* process usnic part response */
    urp = &resp->usnic_resp;

    qp->uq_rq.urq_index = urp->rq_idx[0];
    qp->uq_wq.uwq_index = urp->wq_idx[0];

    qp->uq_rq.urq_cq->ucq_index = urp->cq_idx[0];
    if (qp->uq_rq.urq_cq != qp->uq_wq.uwq_cq) {
        qp->uq_wq.uwq_cq->ucq_index = urp->cq_idx[1];
    }

    /* Pull VF info */
    vfip->vi_vfid = urp->vfid;
    vfip->vi_bar_bus_addr = urp->bar_bus_addr;
    vfip->vi_bar_len = urp->bar_len;

    if (dev->ud_caps[USD_CAP_MAP_PER_RES] > 0) {
        for (i = 0; i < urp->num_barres; i++) {
            enum vnic_res_type type = urp->resources[i].type;
            if (type < RES_TYPE_MAX) {
                vfip->barres[type].type = type;
                vfip->barres[type].bus_addr = urp->resources[i].bus_addr;
                vfip->barres[type].len = urp->resources[i].len;
            }
        }
        if (vfip->barres[RES_TYPE_WQ].bus_addr == 0) {
                usd_err("Failed to retrieve WQ res info\n");
                ret = -ENXIO;
                goto out;
        }
        if (vfip->barres[RES_TYPE_RQ].bus_addr == 0) {
                usd_err("Failed to retrieve RQ res info\n");
                ret = -ENXIO;
                goto out;
        }
        if (vfip->barres[RES_TYPE_CQ].bus_addr == 0) {
                usd_err("Failed to retrieve CQ res info\n");
                ret = -ENXIO;
                goto out;
        }
        if (vfip->barres[RES_TYPE_INTR_CTRL].bus_addr == 0) {
                usd_err("Failed to retrieve INTR res info\n");
                ret = -ENXIO;
                goto out;
        }
        if (vfip->barres[RES_TYPE_DEVCMD].bus_addr == 0) {
                usd_err("Failed to retrieve DEVCMD res info\n");
                ret = -ENXIO;
                goto out;
        }
    }
    free(resp);
    return 0;

  out:
    if (irp != NULL)                   /* indicates successful IB create QP */
        usd_ib_cmd_destroy_qp(dev, qp);
    free(resp);
    return ret;
}

int
usd_ib_cmd_modify_qp(
    struct usd_device *dev,
    struct usd_qp_impl *qp,
    int state)
{
    struct ibv_modify_qp cmd;
    struct ibv_modify_qp *icp;
    int n;

    memset(&cmd, 0, sizeof(cmd));

    icp = &cmd;
    icp->command = IB_USER_VERBS_CMD_MODIFY_QP;
    icp->in_words = sizeof(cmd) / 4;
    icp->out_words = 0;

    icp->qp_handle = qp->uq_qp_handle;
    icp->attr_mask = IBV_QP_STATE;
    icp->qp_state = state;

    /* Issue command to IB driver */
    n = write(dev->ud_ib_dev_fd, &cmd, sizeof(cmd));
    if (n != sizeof(cmd)) {
        return -errno;
    }

    return 0;
}

int
usd_ib_cmd_destroy_qp(
    struct usd_device *dev,
    struct usd_qp_impl *qp)
{
    struct ibv_destroy_qp cmd;
    struct ibv_destroy_qp_resp resp;
    struct ibv_destroy_qp *icp;
    int n;

    memset(&cmd, 0, sizeof(cmd));

    icp = &cmd;
    icp->command = IB_USER_VERBS_CMD_DESTROY_QP;
    icp->in_words = sizeof(cmd) / 4;
    icp->out_words = sizeof(resp) / 4;
    icp->response = (uintptr_t) & resp;

    icp->qp_handle = qp->uq_qp_handle;

    /* Issue command to IB driver */
    n = write(dev->ud_ib_dev_fd, &cmd, sizeof(cmd));
    if (n != sizeof(cmd)) {
        return -errno;
    }

    return 0;
}

static int
usd_ib_cmd_query_device(
    struct usd_device *dev,
    struct ibv_query_device_resp *irp)
{
    struct ibv_query_device cmd;
    struct ibv_query_device *icp;
    int n;

    memset(&cmd, 0, sizeof(cmd));

    icp = &cmd;
    icp->command = IB_USER_VERBS_CMD_QUERY_DEVICE;
    icp->in_words = sizeof(cmd) / 4;
    icp->out_words = sizeof(*irp) / 4;
    icp->response = (uintptr_t) irp;

    /* keep Valgrind happy */
    memset(irp, 0x00, sizeof(*irp));

    /* Issue command to IB driver */
    n = write(dev->ud_ib_dev_fd, &cmd, sizeof(cmd));
    if (n != sizeof(cmd)) {
        return -errno;
    }

    return 0;
}

static int
usd_ib_cmd_query_port(
    struct usd_device *dev,
    struct ibv_query_port_resp *irp)
{
    struct ibv_query_port cmd;
    struct ibv_query_port *icp;
    int n;

    memset(&cmd, 0, sizeof(cmd));

    icp = &cmd;
    icp->command = IB_USER_VERBS_CMD_QUERY_PORT;
    icp->in_words = sizeof(cmd) / 4;
    icp->out_words = sizeof(*irp) / 4;
    icp->response = (uintptr_t) irp;

    icp->port_num = 1;

    /* keep Valgrind happy */
    memset(irp, 0x00, sizeof(*irp));

    /* Issue command to IB driver */
    n = write(dev->ud_ib_dev_fd, &cmd, sizeof(cmd));
    if (n != sizeof(cmd)) {
        return -errno;
    }

    return 0;
}

/*
 * Issue query commands for device and port and interpret the resaults
 */
int
usd_ib_query_dev(
    struct usd_device *dev)
{
    struct ibv_query_device_resp dresp;
    struct ibv_query_port_resp presp;
    struct usd_device_attrs *dap;
    unsigned speed;
    int ret;

    ret = usd_ib_cmd_query_device(dev, &dresp);
    if (ret != 0)
        return ret;

    ret = usd_ib_cmd_query_port(dev, &presp);
    if (ret != 0)
        return ret;

    /* copy out the attributes we care about */
    dap = &dev->ud_attrs;

    dap->uda_link_state =
        (presp.state == 4) ? USD_LINK_UP : USD_LINK_DOWN;

    /*
     * If link it up derive bandwidth from speed and width.
     * If link is down, driver reports bad speed, try to deduce from the
     * NIC device ID.
     */
    if (dap->uda_link_state == USD_LINK_UP) {
#define MKSW(S,W) (((S)<<8)|(W))
        speed = MKSW(presp.active_speed, presp.active_width);
        switch (speed) {
        case MKSW(2, 2):
        case MKSW(8, 1):
            dap->uda_bandwidth = 10000;
            break;
        case MKSW(8, 2):
        case MKSW(32, 2):
            dap->uda_bandwidth = 40000;
            break;
        default:
            printf("Warning: unrecognized speed/width %d/%d, assuming 10G\n",
                   presp.active_speed, presp.active_width);
            dap->uda_bandwidth = 10000;
            break;
        }
    } else {
        /* from pci_ids.h */
        switch (dap->uda_device_id) {
        case 0x4f: /* Vasona */
        case 0x84: /* Cotati */
        case 0x85: /* Lexington */
        case 0x12c: /* Calistoga */
        case 0x137: /* Mountain View */
        case 0x138: /* Walnut Creek */
            dap->uda_bandwidth = 10000;
            break;
        case 0xcd:  /* icehouse */
        case 0x14d: /* clearlake */
            dap->uda_bandwidth = 40000;
            break;
        default:
            dap->uda_bandwidth = 0;
        }
    }

    dap->uda_vendor_id = dresp.vendor_id;
    dap->uda_vendor_part_id = dresp.vendor_part_id;
    dap->uda_device_id = dresp.hw_ver;

    dap->uda_max_qp = dresp.max_qp;
    dap->uda_max_cq = dresp.max_cq;

    return 0;
}
