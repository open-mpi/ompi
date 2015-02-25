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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

#include <netinet/in.h>
#include <infiniband/verbs.h>

#include "kcompat.h"
#include "cq_enet_desc.h"
#include "wq_enet_desc.h"
#include "rq_enet_desc.h"

#include "usnic_abi.h"
#include "usnic_direct.h"
#include "usd.h"
#include "usd_ib_cmd.h"
#include "usd_util.h"
#include "usd_vnic.h"
#include "usd_device.h"

static int usd_create_qp_normal(struct usd_qp_impl *qp);

/*
 * Remove a usecount on a VF, free it if it goes to zero
 */
static void
usd_unmap_vf(
    struct usd_device *dev,
    struct usd_vf *vf)
{
    uint32_t i;
    --vf->vf_refcnt;

    if (vf->vf_refcnt == 0) {

        /* unlink from list (logic works for uninit struct also) */
        if (vf->vf_next != NULL)
            vf->vf_next->vf_prev = vf->vf_prev;
        if (vf->vf_prev != NULL)
            vf->vf_prev->vf_next = vf->vf_next;
        if (dev->ud_vf_list == vf)
            dev->ud_vf_list = vf->vf_next;

        if (vf->vf_vdev != NULL)
            vnic_dev_unregister(vf->vf_vdev);
        if (vf->vf_bar0.vaddr != MAP_FAILED) {
            munmap(vf->vf_bar0.vaddr, vf->vf_bar0.len);
        }
        for (i = 0; i < sizeof(vf->iomaps)/sizeof(vf->iomaps[0]); i++) {
            if (vf->iomaps[i].bus_addr != 0 &&
                    vf->iomaps[i].vaddr != MAP_FAILED) {
                munmap(vf->iomaps[i].vaddr, vf->iomaps[i].len);
            }
        }

        free(vf);
    }
}

static int
usd_map_one_res(struct usd_device *dev, struct usd_vf *vf,
                    struct usnic_vnic_barres_info *barres)
{
    struct vnic_dev_iomap_info* iomap;
    off64_t offset;
    uint64_t page_size = sysconf(_SC_PAGE_SIZE);

    iomap = &vf->iomaps[barres->type];
    iomap->bus_addr = barres->bus_addr;
    iomap->len = (barres->len + (page_size - 1)) & (~(page_size - 1));

    offset = USNIC_ENCODE_PGOFF(vf->vf_id, USNIC_MMAP_RES, barres->type);
    iomap->vaddr = mmap64(NULL, iomap->len, PROT_READ + PROT_WRITE,
                        MAP_SHARED, dev->ud_ib_dev_fd, offset);
    if (iomap->vaddr == MAP_FAILED) {
        usd_err("Failed to map res type %d, bus_addr 0x%lx, len 0x%lx\n",
                barres->type, iomap->bus_addr, iomap->len);
        return -errno;
    }
    vnic_dev_upd_res_vaddr(vf->vf_vdev, iomap);

    return 0;
}

static int
usd_map_vnic_res(struct usd_device *dev, struct usd_vf *vf,
                    struct usd_vf_info *vfip)
{
    int i, err;

    /* unmap bar0 */
    if (vf->vf_bar0.vaddr != MAP_FAILED) {
        munmap(vf->vf_bar0.vaddr, vf->vf_bar0.len);
        vf->vf_bar0.vaddr = MAP_FAILED;
    }

    for (i = RES_TYPE_EOL + 1; i < RES_TYPE_MAX; i++) {
        if (vfip->barres[i].bus_addr != 0) {
            err = usd_map_one_res(dev, vf, &vfip->barres[i]);
            if (err)
                return err;
        } else {
            /* Disable any other res not reported by kernel module */
            struct vnic_dev_iomap_info iomap;
            iomap.vaddr = 0;
            iomap.bus_addr = vnic_dev_get_res_bus_addr(
                                        vf->vf_vdev, i, 0);
            iomap.len = vnic_dev_get_res_type_len(
                                        vf->vf_vdev, i);
            vnic_dev_upd_res_vaddr(vf->vf_vdev, &iomap);
        }
    }

    return 0;
}

/*
 * Create a VF structure if we don't already have one in use,
 * update refcnt
 */
static int
usd_map_vf(
    struct usd_device *dev,
    struct usd_vf_info *vfip,
    struct usd_vf **vf_o)
{
    struct usd_vf *vf;
    off64_t offset;
    int ret;

    /* find matching VF */
    vf = dev->ud_vf_list;
    while (vf != NULL) {
        if (vf->vf_id == vfip->vi_vfid) break;
        vf = vf->vf_next;
    }

    /* Was VF actually found? If not, create and add */
    if (vf == NULL) {
        vf = calloc(sizeof(*vf), 1);
        if (vf == NULL) {
            ret = -errno;
            goto out;
        }

        /* Fill in function */
        vf->vf_id = vfip->vi_vfid;
        vf->vf_refcnt = 1;
        vf->vf_bar0.bus_addr = vfip->vi_bar_bus_addr;
        vf->vf_bar0.len = vfip->vi_bar_len;

        /* map BAR0 first to get res info */
        offset = USNIC_ENCODE_PGOFF(vf->vf_id, USNIC_MMAP_BAR, 0);
        vf->vf_bar0.vaddr = mmap64(NULL, vf->vf_bar0.len,
                                 PROT_READ + PROT_WRITE, MAP_SHARED,
                                 dev->ud_ib_dev_fd,
                                 offset);
        if (vf->vf_bar0.vaddr == MAP_FAILED) {
            usd_err("Failed to map bar0\n");
            ret = -errno;
            goto out;
        }

        /* Register it */
        vf->vf_vdev = vnic_dev_alloc_discover(NULL, NULL, (void *)dev,
                                        &vf->vf_bar0, 1);
        if (vf->vf_vdev == NULL) {
            ret = -ENOENT;
            goto out;
        }

        /* map individual vnic resource seperately */
        if (dev->ud_caps[USNIC_CAP_MAP_PER_RES] > 0) {
            ret = usd_map_vnic_res(dev, vf, vfip);
            if (ret)
                goto out;
        }

        ret = vnic_dev_cmd_init(vf->vf_vdev, 1);
        if (ret)
            goto out;

        /* link it in */
        vf->vf_next = dev->ud_vf_list;
        dev->ud_vf_list = vf;

        if (vf->vf_next != NULL)
            vf->vf_next->vf_prev = vf;
        vf->vf_prev = NULL;

    /* Found existing VF, bump reference count */
    } else {
        ++vf->vf_refcnt;
    }

    *vf_o = vf;

    return 0;

 out:
    if (vf != NULL)
        usd_unmap_vf(dev, vf);
    return ret;
}

static void
usd_get_vf(
    struct usd_vf *vf)
{
    ++vf->vf_refcnt;
}

/*
 * Function that does whatever is needed to make a CQ go away
 */
int
usd_destroy_cq(
    struct usd_cq *ucq)
{
    struct usd_cq_impl *cq;

    cq = to_cqi(ucq);

    if (cq->ucq_state & USD_QS_VERBS_CREATED)
        usd_ib_cmd_destroy_cq(cq->ucq_dev, cq);

    if (cq->ucq_state & USD_QS_VF_MAPPED)
        usd_unmap_vf(cq->ucq_dev, cq->ucq_vf);

    if (cq->ucq_desc_ring != NULL)
        usd_free_mr(cq->ucq_desc_ring);
    if (cq->ucq_rq_map != NULL)
        free(cq->ucq_rq_map);
    if (cq->ucq_wq_map != NULL)
        free(cq->ucq_wq_map);
    free(cq);

    return 0;
}

static int
usd_vnic_wq_init(
    struct usd_wq *wq,
    struct usd_vf *vf,
    uint64_t desc_ring)
{
    struct vnic_wq *vwq;
    int ret;

    vwq = &wq->uwq_vnic_wq;

    /* get address of control register */
    vwq->ctrl = vnic_dev_get_res(vf->vf_vdev, RES_TYPE_WQ, wq->uwq_index);
    if (vwq->ctrl == NULL)
        return -EINVAL;

    ret = vnic_wq_disable(vwq);
    if (ret != 0)
        return ret;

    writeq(desc_ring, &vwq->ctrl->ring_base);
    iowrite32(wq->uwq_num_entries, &vwq->ctrl->ring_size);
    iowrite32(0, &vwq->ctrl->fetch_index);
    iowrite32(0, &vwq->ctrl->posted_index);
    iowrite32(wq->uwq_cq->ucq_index, &vwq->ctrl->cq_index);
    iowrite32(0, &vwq->ctrl->error_interrupt_enable);
    iowrite32(0, &vwq->ctrl->error_interrupt_offset);
    iowrite32(0, &vwq->ctrl->error_status);

    wq->uwq_state |= USD_QS_VNIC_INITIALIZED;
    wq->uwq_next_desc = wq->uwq_desc_ring;
    wq->uwq_send_credits = wq->uwq_num_entries - 1;

    return 0;
}

/*
 * Allocate the resources for a previously created WQ for normal QP
 */
static int
usd_create_wq_normal(
    struct usd_qp_impl *qp)
{
    struct usd_wq *wq;
    uint32_t ring_size;
    int ret;

    wq = &qp->uq_wq;

    /* Allocate resources for WQ */
    ring_size = sizeof(struct wq_enet_desc) * wq->uwq_num_entries;
    ret = usd_alloc_mr(qp->uq_dev, ring_size, (void **)&wq->uwq_desc_ring);
    if (ret != 0)
        return ret;

    ret = usd_vnic_wq_init(wq, qp->uq_vf, (uint64_t)wq->uwq_desc_ring);
    if (ret != 0)
        goto out;

    return 0;

out:
    if (wq->uwq_desc_ring != NULL) {
        usd_free_mr(wq->uwq_desc_ring);
        wq->uwq_desc_ring = NULL;
    }
    return ret;
}

/*
 * Allocate the resources for a previously created WQ
 */
static int
usd_create_wq_pio(
    struct usd_qp_impl *qp)
{
    uint32_t pio_memsize;
    uint32_t used_size;
    uint32_t ring_size;
    void *pio_vaddr;
    uint64_t pio_paddr;
    uint64_t ivaddr;
    struct usd_wq *wq;
    struct usd_device *dev;
    int ret;

    dev = qp->uq_dev;
    if (dev->ud_caps[USNIC_CAP_PIO] == 0 ||
        vnic_dev_get_res_bus_addr(qp->uq_vf->vf_vdev, RES_TYPE_MEM, 0) == 0) {
        usd_err("dev does not support PIO\n");
        return -ENODEV;
    }

    pio_memsize = vnic_dev_get_res_count(qp->uq_vf->vf_vdev, RES_TYPE_MEM);
    pio_vaddr = vnic_dev_get_res(qp->uq_vf->vf_vdev, RES_TYPE_MEM, 0);

    ret = usd_get_devspec(qp);
    if (ret != 0)
        return ret;
    pio_paddr = qp->uq_attrs.uqa_pio_paddr;

    /* 512-byte alignment must match */
    if ((((uint64_t)pio_vaddr ^ pio_paddr) & 511) != 0) {
        fprintf(stderr, "Alignment mismatch, %p vs 0x%lx, cannot do PIO\n",
                pio_vaddr, pio_paddr);
        return -ENXIO;
    }

    /* skip past size */
    ivaddr = (uintptr_t)pio_vaddr;
    ivaddr += sizeof(uint64_t);

    /* round up to 512 bytes */
    ivaddr = (ivaddr + 511) & ~511;

    /* WQ ring goes first.  Allow space for 64-byte write of last desc */
    wq = &qp->uq_wq;
    ring_size = wq->uwq_num_entries * sizeof(struct wq_enet_desc);
    ring_size += 64 - sizeof(struct wq_enet_desc);
    wq->pio_v_wq_addr = (void *)ivaddr;
    wq->pio_p_wq_addr = pio_paddr + ivaddr - (uint64_t)pio_vaddr;
    ivaddr += ring_size;

    /* round up to 64 bytes */
    ivaddr = (ivaddr + 63) & ~63;

    /* we keep a copy of the ring, also */
    ret = usd_alloc_mr(qp->uq_dev, ring_size, (void **)&wq->uwq_desc_ring);
    if (ret != 0)
        return ret;

    /* packet buffer */
    wq->pio_v_pkt_buf = (void *)ivaddr;
    wq->pio_p_pkt_buf = pio_paddr + ivaddr - (uint64_t)pio_vaddr;
    ivaddr += wq->uwq_num_entries * 256;

    used_size = ivaddr - (uintptr_t)pio_vaddr;
    if (used_size > pio_memsize) {
        ret = -ENOMEM;
        goto out;
    }

    ret = usd_vnic_wq_init(wq, qp->uq_vf, wq->pio_p_wq_addr);
    if (ret != 0)
        goto out;

    return 0;

out:
    if (wq->uwq_desc_ring != NULL) {
        usd_free_mr(wq->uwq_desc_ring);
        wq->uwq_desc_ring = NULL;
    }
    return ret;
}

/*
 * Allocate the resources for a previously created WQ
 */
static int
usd_create_wq(
    struct usd_qp_impl *qp)
{
    struct usd_wq *wq;
    int ret;

    switch (qp->uq_attrs.uqa_qtype) {
    case USD_QTY_PIO:
        ret = usd_create_wq_pio(qp);
        break;
    case USD_QTY_NORMAL:
        ret = usd_create_wq_normal(qp);
        break;
    default:
        ret = -1;
        break;
    }

    if (ret == 0) {
        wq = &qp->uq_wq;
        wq->uwq_post_index_mask = (wq->uwq_num_entries-1);
        wq->uwq_post_index = 1;
        wq->uwq_last_comp = (wq->uwq_num_entries-1);
    }

    return ret;
}

static int
usd_vnic_rq_init(
    struct usd_rq *rq,
    struct usd_vf *vf,
    uint64_t desc_ring)
{
    struct vnic_rq *vrq;
    int ret;

    vrq = &rq->urq_vnic_rq;

    /* get address of control register */
    vrq->ctrl = vnic_dev_get_res(vf->vf_vdev, RES_TYPE_RQ, rq->urq_index);
    if (vrq->ctrl == NULL)
        return -EINVAL;

    ret = vnic_rq_disable(vrq);
    if (ret != 0)
        return ret;

    writeq(desc_ring, &vrq->ctrl->ring_base);
    iowrite32(rq->urq_num_entries, &vrq->ctrl->ring_size);
    iowrite32(0, &vrq->ctrl->fetch_index);
    iowrite32(0, &vrq->ctrl->posted_index);
    iowrite32(rq->urq_cq->ucq_index, &vrq->ctrl->cq_index);
    iowrite32(0, &vrq->ctrl->error_interrupt_enable);
    iowrite32(0, &vrq->ctrl->error_interrupt_offset);
    iowrite32(0, &vrq->ctrl->dropped_packet_count);
    iowrite32(0, &vrq->ctrl->error_status);

    rq->urq_state |= USD_QS_VNIC_INITIALIZED;
    rq->urq_next_desc = rq->urq_desc_ring;
    rq->urq_recv_credits = rq->urq_num_entries - 1;

    return 0;
}

/*
 * Allocate the resources for a previously created RQ
 */
static int
usd_create_rq(struct usd_qp_impl *qp)
{
    struct usd_rq *rq;
    uint32_t ring_size;
    int ret;

    rq = &qp->uq_rq;

    /* Allocate resources for RQ */
    ring_size = sizeof(struct rq_enet_desc) * rq->urq_num_entries;
    ret = usd_alloc_mr(qp->uq_dev, ring_size, (void **)&rq->urq_desc_ring);
    if (ret != 0)
        return ret;

    ret = usd_vnic_rq_init(rq, qp->uq_vf, (uint64_t)rq->urq_desc_ring);
    if (ret != 0)
        goto out;

    rq->urq_post_index_mask = (rq->urq_num_entries-1);
    rq->urq_post_index = 0;
    rq->urq_last_comp = (rq->urq_num_entries-1);

    return 0;
out:
    if (rq->urq_desc_ring != NULL) {
        usd_free_mr(rq->urq_desc_ring);
        rq->urq_desc_ring = NULL;
    }
    return ret;
}

static int
usd_vnic_disable_qp(
    struct usd_qp_impl *qp)
{
    struct usd_rq *rq;
    struct usd_wq *wq;
    int ret;

    wq = &qp->uq_wq;
    rq = &qp->uq_rq;

    /* disable both queues */
    ret = vnic_wq_disable(&wq->uwq_vnic_wq);
    if (ret != 0)
        return ret;
    ret = vnic_rq_disable(&rq->urq_vnic_rq);

    return ret;
}

static void
usd_vnic_enable_qp(
    struct usd_qp_impl *qp)
{
    struct usd_rq *rq;
    struct usd_wq *wq;

    wq = &qp->uq_wq;
    rq = &qp->uq_rq;

    vnic_rq_enable(&rq->urq_vnic_rq);
    vnic_wq_enable(&wq->uwq_vnic_wq);
}

/*
 * QP has been created and resources allocated.  Issue the IB commands to
 * change the state to INIT/RTR/RTS to trigger filter creation and enable the
 * QP to send and receive traffic.
 */
static int
usd_enable_verbs_qp(
    struct usd_qp_impl *qp)
{
    struct usd_rq *rq;
    struct usd_wq *wq;
    struct usd_device *dev;
    int ret;

    dev = qp->uq_dev;
    wq = &qp->uq_wq;
    rq = &qp->uq_rq;

    /* XXX is this really necessary? */
    ret = usd_vnic_disable_qp(qp);
    if (ret != 0) {
        goto out;
    }

    /* state to INIT */
    ret = usd_ib_cmd_modify_qp(dev, qp, IBV_QPS_INIT);
    if (ret != 0) {
        goto out;
    }

    /* state to "ready to receive," enable rq */
    ret = usd_ib_cmd_modify_qp(dev, qp, IBV_QPS_RTR);
    if (ret != 0) {
        goto out;
    }

    /* state to "ready to send," enable wq */
    ret = usd_ib_cmd_modify_qp(dev, qp, IBV_QPS_RTS);
    if (ret != 0) {
        goto out;
    }

    usd_vnic_enable_qp(qp);
    rq->urq_state |= USD_QS_READY;
    wq->uwq_state |= USD_QS_READY;

 out:
    return ret;
}

/*
 * Public interface to disable a QP
 */
int
usd_disable_qp(
    struct usd_qp *uqp)
{
    struct usd_qp_impl *qp;

    qp = to_qpi(uqp);
    usd_vnic_disable_qp(qp);
    return 0;
}

/*
 * Public interface to enable a QP
 */
int
usd_enable_qp(
    struct usd_qp *uqp)
{
    struct usd_qp_impl *qp;

    qp = to_qpi(uqp);
    usd_vnic_enable_qp(qp);
    return 0;
}

/*
 * Public interface to create a CQ
 * First, issue the verbs command to create a CW instance in the driver.
 * Second, allocate the data structures so that poll_cq can succeed, though
 * we will not actually have VIC resources allocated until the first create_qp
 * that uses this CQ.  We will finish configuring the CQ at that time.
 */
int
usd_create_cq(
    struct usd_device *dev,
    unsigned num_entries,
    int comp_fd,
    struct usd_cq **cq_o)
{
    unsigned qp_per_vf;
    struct usd_cq *ucq;
    struct usd_cq_impl *cq;
    unsigned ring_size;
    int ret;

    /* Make sure device ready */
    ret = usd_device_ready(dev);
    if (ret != 0) {
        return ret;
    }

    /* XXX need driver support for completion FDs */
    if (comp_fd != -1) {
        usd_err("create_cq does not support comp_fd yet\n");
        return -EINVAL;
    }

    if (num_entries > dev->ud_attrs.uda_max_cqe) {
        return -EINVAL;
    }

    cq = (struct usd_cq_impl *)calloc(sizeof(*cq), 1);
    if (cq == NULL) {
        ret = -errno;
        goto out;
    }

    qp_per_vf = dev->ud_attrs.uda_qp_per_vf;

    cq->ucq_wq_map = calloc(qp_per_vf, sizeof(struct usd_wq *));
    cq->ucq_rq_map = calloc(qp_per_vf, sizeof(struct usd_rq *));
    if (cq->ucq_wq_map == NULL || cq->ucq_rq_map == NULL) {
        ret = -ENOMEM;
        goto out;
    }

    cq->ucq_dev = dev;

    /* add 1 and round up to next POW2 and min() with 64 */
    num_entries = 1 << msbit(num_entries);
    if (num_entries < 64) {
        num_entries = 64;
    }

    cq->ucq_num_entries = num_entries;

    ring_size = sizeof(struct cq_desc) * num_entries;
    ret = usd_alloc_mr(dev, ring_size, &cq->ucq_desc_ring);
    if (ret != 0)
        goto out;
    memset(cq->ucq_desc_ring, 0, ring_size);

    ret = usd_ib_cmd_create_cq(dev, cq);
    if (ret != 0)
        goto out;

    cq->ucq_state |= USD_QS_VERBS_CREATED;

    /* initialize polling variables */
    cq->ucq_cqe_mask = num_entries - 1;
    cq->ucq_color_shift = msbit(num_entries) - 1;

    ucq = to_usdcq(cq);
    ucq->ucq_num_entries = num_entries - 1;
    *cq_o = to_usdcq(cq);
    return 0;

out:
    if (cq != NULL) {
        usd_destroy_cq(to_usdcq(cq));
    }
    return ret;
}

/*
 * Finish CQ creation after first QP has been created.  Associate a vf
 * and configure the CQ on the VIC.  It's OK if CQ is already configured, but
 * VFs must match.
 */
static int
usd_finish_create_cq(
    struct usd_cq_impl *cq,
    struct usd_vf *vf)
{
    struct vnic_cq *vcq;

    if (cq->ucq_state & USD_QS_VNIC_INITIALIZED) {
        if (cq->ucq_vf == vf) {
            return 0;
        } else {
            usd_err("Cannot share CQ across VFs\n");
            return -EINVAL;
        }
    }

    vcq = &cq->ucq_vnic_cq;
    vcq->index = cq->ucq_index;
    vcq->vdev = vf->vf_vdev;

    vcq->ctrl = vnic_dev_get_res(vcq->vdev, RES_TYPE_CQ, vcq->index);
    if (vcq->ctrl == NULL)
        return -EINVAL;

    cq->ucq_vf = vf;
    usd_get_vf(vf);     /* bump the reference count */
    cq->ucq_state |= USD_QS_VF_MAPPED;

    /*
     * Tell the VIC about this CQ
     */
    {
        unsigned int cq_flow_control_enable = 0;
        unsigned int cq_color_enable = 1;
        unsigned int cq_head = 0;
        unsigned int cq_tail = 0;
        unsigned int cq_tail_color = 1;
        unsigned int cq_intr_enable = 0;
        unsigned int cq_entry_enable = 1;
        unsigned int cq_msg_enable = 0;
        unsigned int cq_intr_offset = 0;
        uint64_t cq_msg_addr = 0;

        cq->ucq_vnic_cq.ring.base_addr = (uintptr_t)cq->ucq_desc_ring;
        cq->ucq_vnic_cq.ring.desc_count = cq->ucq_num_entries;

        vnic_cq_init(&cq->ucq_vnic_cq, cq_flow_control_enable,
                cq_color_enable, cq_head, cq_tail, cq_tail_color,
                cq_intr_enable, cq_entry_enable, cq_msg_enable,
                cq_intr_offset, cq_msg_addr);
    }
    cq->ucq_state |= USD_QS_VNIC_INITIALIZED;

    return 0;
}

/*
 * Fill in ops field for QP
 */
static int
usd_qp_get_ops(
    struct usd_qp_impl *qp)
{
    int tt;

#define USD_TT(TY,TR) ((TY)<<16|(TR))
    tt = USD_TT(qp->uq_attrs.uqa_transport, qp->uq_attrs.uqa_qtype);

    switch (tt) {
    case USD_TT(USD_QTR_UDP, USD_QTY_NORMAL):
        qp->uq_qp.uq_ops = usd_qp_ops_udp_normal;
        break;
    case USD_TT(USD_QTR_UDP, USD_QTY_PIO):
        qp->uq_qp.uq_ops = usd_qp_ops_udp_pio;
        break;
    case USD_TT(USD_QTR_RAW, USD_QTY_NORMAL):
        qp->uq_qp.uq_ops = usd_qp_ops_raw_normal;
        break;
    default:
        return -EINVAL;
    }

    return 0;
}

/*
 * Convert user's filter into internal representation
 */
static int
usd_filter_alloc(
    struct usd_device *dev,
    struct usd_filter *filt,
    struct usd_qp_filter *qfilt)
{
    struct sockaddr_in sin;
    int ret;
    int s;

    switch (filt->uf_type) {
    case USD_FTY_UDP_SOCK:
        qfilt->qf_type = USD_FTY_UDP_SOCK;
        qfilt->qf_filter.qf_udp.u_sockfd = filt->uf_filter.uf_udp_sock.u_sock;
        break;

    case USD_FTY_UDP:
        qfilt->qf_type = USD_FTY_UDP;
        qfilt->qf_filter.qf_udp.u_sockfd = -1;

        s = socket(AF_INET, SOCK_DGRAM, 0);
        if (s == -1)
            return -errno;
        memset(&sin, 0, sizeof(sin));
        sin.sin_family = AF_INET;
        sin.sin_addr.s_addr = dev->ud_attrs.uda_ipaddr_be;
        sin.sin_port = htons(filt->uf_filter.uf_udp.u_port);
        ret = bind(s, (struct sockaddr *)&sin, sizeof(sin));
        if (ret == -1) {
            ret = -errno;
            close(s);
            return ret;
        }

        /* save the socket */
        qfilt->qf_filter.qf_udp.u_sockfd = s;
        break;

    default:
        return -EINVAL;
    }

    return 0;
}

/*
 * Fill in local address given filter and return from verbs QP create
 */
static int
usd_get_qp_local_addr(
    struct usd_qp_impl *qp)
{
    socklen_t addrlen;
    int ret;

    switch (qp->uq_attrs.uqa_transport) {

    case USD_QTR_UDP:
        /* find out what address we got */
        addrlen = sizeof(qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr);
        ret = getsockname(qp->uq_filter.qf_filter.qf_udp.u_sockfd,
         (struct sockaddr *) &qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr,
         &addrlen);
        if (ret == -1)
            return -errno;
        break;

    default:
        break;
    }
    return 0;
}

static void
usd_filter_free(
    struct usd_qp_filter *qfilt)
{
    switch (qfilt->qf_type) {
    case USD_FTY_UDP:
        close(qfilt->qf_filter.qf_udp.u_sockfd);
        break;
    default:
        break;
    }
}

/*
 * Destroy a QP
 */
int
usd_destroy_qp(
    struct usd_qp *uqp)
{
    struct usd_wq *wq;
    struct usd_rq *rq;
    struct usd_qp_impl *qp;

    qp = to_qpi(uqp);

    wq = &qp->uq_wq;
    rq = &qp->uq_rq;

    if (wq->uwq_state & USD_QS_READY)
        usd_disable_qp(uqp);

    if (rq->urq_state & USD_QS_VNIC_ALLOCATED)
        vnic_rq_free(&rq->urq_vnic_rq);

    if (wq->uwq_state & USD_QS_VF_MAPPED)
        usd_unmap_vf(qp->uq_dev, qp->uq_vf);

    if (wq->uwq_state & USD_QS_VERBS_CREATED)
        usd_ib_cmd_destroy_qp(qp->uq_dev, qp);

    if (rq->urq_state & USD_QS_FILTER_ALLOC)
        usd_filter_free(&qp->uq_filter);

    if (rq->urq_context != NULL)
        free(rq->urq_context);
    if (wq->uwq_post_info != NULL)
        free(wq->uwq_post_info);
    if (wq->uwq_copybuf != NULL)
        usd_free_mr(wq->uwq_copybuf);
    if (wq->uwq_desc_ring != NULL)
        usd_free_mr(wq->uwq_desc_ring);
    if (rq->urq_desc_ring != NULL)
        usd_free_mr(rq->urq_desc_ring);

    free(qp);

    return 0;
}

/*
 * Create a normal or PIO QP
 */
static int
usd_create_qp_normal(
    struct usd_qp_impl *qp)
{
    struct usd_device *dev;
    unsigned num_wq_entries;
    unsigned num_rq_entries;
    struct usd_vf_info vf_info;
    struct usd_vf *vf;
    struct usd_rq *rq;
    struct usd_wq *wq;
    struct usd_cq_impl *wcq;
    struct usd_cq_impl *rcq;
    size_t copybuf_size;
    int ret;

    dev = qp->uq_dev;
    vf = NULL;

    wq = &qp->uq_wq;
    rq = &qp->uq_rq;
    wcq = wq->uwq_cq;
    rcq = rq->urq_cq;

    ret = usd_qp_get_ops(qp);
    if (ret != 0) {
        goto fail;
    }

    num_wq_entries = wq->uwq_num_entries;
    num_rq_entries = rq->urq_num_entries;

    rq->urq_context = calloc(sizeof(void *), num_rq_entries);
    wq->uwq_post_info = calloc(sizeof(struct usd_wq_post_info), num_wq_entries);
    if (rq->urq_context == NULL || wq->uwq_post_info == NULL) {
        ret = -ENOMEM;
        goto fail;
    }

    /*
     * Issue verbs command to create the QP.  This does not actually
     * instanstiate the filter in the VIC yet, need to bring the
     * verbs QP up to RTR state for that
     */
    memset(&vf_info, 0, sizeof(vf_info));
    ret = usd_ib_cmd_create_qp(dev, qp, &vf_info);
    if (ret != 0) {
        goto fail;
    }

    /* verbs create_qp command has been completed */
    rq->urq_state |= USD_QS_VERBS_CREATED;
    wq->uwq_state |= USD_QS_VERBS_CREATED;

    /*
     * Create/regmr for wq copybuf after verbs QP is created
     * because QP number information may be needed to register
     * mr under shared PD
     */
    copybuf_size = USD_SEND_MAX_COPY * num_wq_entries;
    ret = usd_alloc_mr(dev, copybuf_size, (void **)&wq->uwq_copybuf);
    if (ret != 0)
        goto fail;

    ret = usd_map_vf(dev, &vf_info, &vf);
    if (ret != 0) {
        goto fail;
    }

    qp->uq_vf = vf;
    rq->urq_state |= USD_QS_VF_MAPPED;
    wq->uwq_state |= USD_QS_VF_MAPPED;

    /*
     * Now that we have a VF, we can finish creating the CQs.
     * It's OK if rcq==wcq, finish_create_cq allows for CQ sharing
     */
    ret = usd_finish_create_cq(wcq, vf);
    if (ret != 0) {
        goto fail;
    }
    ret = usd_finish_create_cq(rcq, vf);
    if (ret != 0) {
        goto fail;
    }

    /* define the WQ and RQ to the VIC */
    ret = usd_create_wq(qp);
    if (ret != 0) {
        goto fail;
    }
    ret = usd_create_rq(qp);
    if (ret != 0) {
        goto fail;
    }

    /* Issue commands to driver to enable the QP */
    ret = usd_enable_verbs_qp(qp);
    if (ret != 0) {
        goto fail;
    }

    /* Attach WQ and RQ to CW */
    rcq->ucq_rq_map[rq->urq_index] = rq;
    wcq->ucq_wq_map[wq->uwq_index] = wq;

    qp->uq_attrs.uqa_max_send_credits = wq->uwq_num_entries - 1;
    qp->uq_attrs.uqa_max_recv_credits = rq->urq_num_entries - 1;
    qp->uq_attrs.uqa_max_inline = USD_SEND_MAX_COPY -
        qp->uq_attrs.uqa_hdr_len;

    /* build local address */
    ret = usd_get_qp_local_addr(qp);
    if (ret != 0) {
        goto fail;
    }

    return 0;

 fail:
    return ret;
}

/*
 * Public interface to create QP
 */
int
usd_create_qp(
    struct usd_device *dev,
    enum usd_qp_transport transport,
    enum usd_qp_type qtype,
    struct usd_cq *wucq,
    struct usd_cq *rucq,
    unsigned num_send_credits,
    unsigned num_recv_credits,
    struct usd_filter *filt,
    struct usd_qp **uqp_o)
{
    struct usd_qp_impl *qp;
    unsigned num_rq_entries;
    unsigned num_wq_entries;
    struct usd_cq_impl *wcq;
    struct usd_cq_impl *rcq;
    struct usd_rq *rq;
    struct usd_wq *wq;
    int ret;

    qp = NULL;

    /* Make sure device ready */
    ret = usd_device_ready(dev);
    if (ret != 0) {
        goto fail;
    }

    qp = calloc(sizeof(*qp), 1);
    if (qp == NULL) {
        ret = -ENOMEM;
        goto fail;
    }

    qp->uq_dev = dev;
    qp->uq_attrs.uqa_transport = transport;
    qp->uq_attrs.uqa_qtype = qtype;

    ret = usd_qp_get_ops(qp);
    if (ret != 0) {
        goto fail;
    }

    if (num_recv_credits > dev->ud_attrs.uda_max_recv_credits) {
        ret = -EINVAL;
        goto fail;
    }
    /* Add 1 and round num_entries up to POW2 and min to 32 */
    num_rq_entries = 1 << msbit(num_recv_credits);
    if (num_rq_entries < 32) num_rq_entries = 32;

    if (num_send_credits > dev->ud_attrs.uda_max_send_credits) {
        ret = -EINVAL;
        goto fail;
    }
    num_wq_entries = 1 << msbit(num_send_credits);
    if (num_wq_entries < 32) num_wq_entries = 32;

    rcq = to_cqi(rucq);
    wcq = to_cqi(wucq);

    rq = &qp->uq_rq;
    rq->urq_num_entries = num_rq_entries;
    rq->urq_cq = rcq;

    wq = &qp->uq_wq;
    wq->uwq_num_entries = num_wq_entries;
    wq->uwq_cq = wcq;

    /* do filter setup */
    ret = usd_filter_alloc(dev, filt, &qp->uq_filter);
    if (ret != 0) {
        goto fail;
    }
    rq->urq_state |= USD_QS_FILTER_ALLOC;

    /* Fill in some attrs */
    switch (transport) {
    case USD_QTR_UDP:
        qp->uq_attrs.uqa_hdr_len = sizeof(struct usd_udp_hdr);
        break;
    case USD_QTR_RAW:
        qp->uq_attrs.uqa_hdr_len = 0;
        break;
    }

    /*
     * Now, do the type-specific configuration
     */
    switch (qtype) {
    case USD_QTY_NORMAL:
    case USD_QTY_PIO:
        ret = usd_create_qp_normal(qp);
        if (ret != 0) {
            goto fail;
        }
        break;
    default:
        ret = -EINVAL;
        goto fail;
        break;
    }

    *uqp_o = to_usdqp(qp);
    return 0;

fail:
    if (qp != NULL) {
        usd_destroy_qp(to_usdqp(qp));
    }
    return ret;
}

/*
 * Return attributes of a QP
 */
int
usd_get_qp_attrs(
    struct usd_qp *uqp,
    struct usd_qp_attrs *qattrs)
{
    struct usd_qp_impl *qp;

    qp = to_qpi(uqp);
    *qattrs = qp->uq_attrs;
    return 0;
}
