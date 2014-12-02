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

#ifndef _USD_H_
#define _USD_H_

#include <sys/queue.h>

#include "kcompat.h"
#include "vnic_rq.h"
#include "vnic_wq.h"
#include "vnic_cq.h"
#include "wq_enet_desc.h"
#include "rq_enet_desc.h"

#include "usnic_abi.h"
#include "usnic_direct.h"
#include "usd_ib_sysfs.h"

#define USD_INVALID_HANDLE (~0)
#define USD_SF_ISSET(flags, flagname) \
    ((flags >> USD_SFS_##flagname) & 1)

#define USD_SEND_MAX_COPY 1024
#define USD_MAX_CQ_GROUP 1024
#define USD_MAX_PRESEND 4

#define USD_DEVF_CLOSE_CMD_FD (1<<0)

/* JMS Reset this back to 0 before production */
#ifndef USD_DEBUG
#define USD_DEBUG 1
#endif

/*
 * Instance of a device opened by user
 */
struct usd_device {
    struct usd_ib_dev *ud_ib_dev;       /* parent IB dev */
    int ud_ib_dev_fd;           /* file desc for IB dev */

    uint32_t ud_flags;
    struct usd_device_attrs ud_attrs;

    int ud_caps[USD_CAP_MAX];     /* device capablities */

    /* VFs we have associated with this device */
    struct usd_vf *ud_vf_list;

    /* CQ group management */
    struct usd_cq_group *ud_free_cq_grp;
    uint32_t ud_next_cq_grp_id;

    /* PD for this device */
    uint32_t ud_pd_handle;

    /* destination related */
    int ud_arp_sockfd;          /* for ARP */
    TAILQ_HEAD(, usd_dest_req) ud_pending_reqs;
    TAILQ_HEAD(, usd_dest_req) ud_completed_reqs;

    TAILQ_ENTRY(usd_device) ud_link;
};

/*
 * Registered memory region
 */
struct usd_mr {
    struct usd_device *umr_dev;
    void *umr_vaddr;
    uint32_t umr_handle;
    uint32_t umr_lkey;
    uint32_t umr_rkey;
    size_t umr_length;
};

/*
 * Information about the PCI virtual function
 */
struct usd_vf {
    uint32_t vf_id;
    int vf_refcnt;
    struct vnic_dev_bar vf_bar0;
    struct vnic_dev *vf_vdev;
    struct vnic_dev_iomap_info iomaps[RES_TYPE_MAX];

    /* Will also protect the devcmd region */
    pthread_mutex_t vf_lock;
    struct usd_vf *vf_next;
    struct usd_vf *vf_prev;
};

/*
 * Holding place for information about a VF
 */
struct usd_vf_info {
    uint32_t vi_vfid;
    dma_addr_t vi_bar_bus_addr;
    uint32_t vi_bar_len;
    struct usnic_vnic_barres_info barres[RES_TYPE_MAX];
};

/*
 * Internal representation of a filter
 */
struct usd_qp_filter {
    enum usd_filter_type qf_type;
    union {
        struct {
            int u_sockfd;
        } qf_udp;
    } qf_filter;
};

/*
 * Definitions and structures about queues
 */

/*
 * this is used to keep track of what has been allocated and/or
 * initialized to assist with teardown of partially completed queues
 */
enum usd_qstate {
    USD_QS_FILTER_ALLOC = (1 << 0),
    USD_QS_VERBS_CREATED = (1 << 1),
    USD_QS_VF_MAPPED = (1 << 2),
    USD_QS_VNIC_ALLOCATED = (1 << 3),
    USD_QS_VNIC_INITIALIZED = (1 << 4),
    USD_QS_READY = (1 << 5)
};

struct usd_cq_group {
    struct usd_device *cqg_dev;
    uint32_t cqg_id;
    uint16_t cqg_num_qp;
    uint16_t cqg_refcnt;
    struct usd_cq_group *cqg_next;
};

struct usd_cq_impl {
    struct usd_cq ucq_cq;
    struct usd_device *ucq_dev;
    struct usd_vf *ucq_vf;
    struct usd_cq_group *ucq_cq_group;

    uint32_t ucq_state;

    struct vnic_cq ucq_vnic_cq;

    void *ucq_desc_ring;
    uint32_t ucq_next_desc;
    uint32_t ucq_last_color;

    uint32_t ucq_index;
    uint32_t ucq_num_entries;
    uint32_t ucq_cqe_mask;
    uint32_t ucq_color_shift;
    uint32_t ucq_handle;

    struct usd_rq **ucq_rq_map;
    struct usd_wq **ucq_wq_map;
};
#define to_cqi(CQ) ((struct usd_cq_impl *)(CQ))
#define to_usdcq(CQ) (&(CQ)->ucq_cq)

struct usd_rq {
    struct usd_cq_impl *urq_cq;
    uint32_t urq_state;

    uint32_t urq_index;
    uint32_t urq_num_entries;
    struct vnic_rq urq_vnic_rq;

    void **urq_context;

    char *urq_rxbuf;
    char **urq_post_addr;
    uint32_t urq_post_index;    /* next rxbuf to post */
    uint32_t urq_post_index_mask;
    uint32_t urq_last_comp;
    uint32_t urq_accum_bytes;

    uint32_t urq_num_rxbuf;
    uint32_t urq_rxbuf_size;
};

struct usd_wq_post_info {
    void *wp_context;
    uint32_t wp_len;
};

struct usd_wq {
    struct usd_cq_impl *uwq_cq;
    uint32_t uwq_state;
    struct vnic_wq uwq_vnic_wq;

    uint32_t uwq_index;
    uint32_t uwq_num_entries;
    uint32_t uwq_send_credits;
    struct wq_enet_desc *uwq_desc_ring;
    struct wq_enet_desc *uwq_next_desc;
    uint32_t uwq_post_index;
    uint32_t uwq_post_index_mask;
    uint32_t uwq_last_comp;

    uint8_t *uwq_copybuf;
    struct usd_wq_post_info *uwq_post_info;

    /* used only for PIO QPs */
    void *pio_v_wq_addr;
    uint64_t pio_p_wq_addr;
    char *pio_v_pkt_buf;
    uint64_t pio_p_pkt_buf;
};

struct usd_qp_impl {
    struct usd_qp uq_qp;        /* user's view of QP */

    struct usd_device *uq_dev;
    struct usd_vf *uq_vf;

    struct usd_qp_attrs uq_attrs;

    uint32_t uq_qp_handle;      /* IB QP handle */
    uint32_t uq_qp_num;

    /* primary filter for this QP */
    struct usd_qp_filter uq_filter;

    struct usd_wq uq_wq;
    struct usd_rq uq_rq;
};
#define to_qpi(Q) ((struct usd_qp_impl *)(Q))
#define to_usdqp(Q) (&(Q)->uq_qp)

struct usd_dest {
    union {
        struct {
            struct usd_udp_hdr u_hdr;
        } ds_udp;
    } ds_dest;
};

extern struct usd_qp_ops usd_qp_ops_udp_normal;
extern struct usd_qp_ops usd_qp_ops_udp_pio;
extern struct usd_qp_ops usd_qp_ops_raw_normal;
#endif /* _USD_H_ */
