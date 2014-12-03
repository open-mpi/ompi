/*
 * Copyright (c) 2013, Cisco Systems, Inc. All rights reserved.
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


#ifndef USNIC_ABI_H
#define USNIC_ABI_H

/* ABI between userspace and kernel */
#define USNIC_UVERBS_ABI_VERSION	4

#define USNIC_QP_GRP_MAX_WQS		8
#define USNIC_QP_GRP_MAX_RQS		8
#define USNIC_QP_GRP_MAX_CQS		16

#define USNIC_DECODE_PGOFF_VFID(pgoff)	((pgoff) & ((1ULL << 32) - 1))
#define USNIC_DECODE_PGOFF_TYPE(pgoff)	((pgoff) >> 48)
#define USNIC_DECODE_PGOFF_RES(pgoff)	(((pgoff) >> 32) & ((1ULL << 16) - 1))
#define USNIC_DECODE_PGOFF_BAR(pgoff)	(((pgoff) >> 32) & ((1ULL << 16) - 1))

#define USNIC_ENCODE_PGOFF(vfid, map_type, res_type_bar_id) \
	(((((uint64_t)map_type & 0xffff) << 48) | \
	  (((uint64_t)res_type_bar_id & 0xffff) << 32) | \
	  ((uint64_t)vfid & ((1ULL << 32) - 1))) * sysconf(_SC_PAGE_SIZE))

enum usnic_mmap_type {
	USNIC_MMAP_BAR			= 0,
	USNIC_MMAP_RES			= 1,
};

enum usnic_transport_type {
	USNIC_TRANSPORT_UNKNOWN		= 0,
	USNIC_TRANSPORT_ROCE_CUSTOM	= 1,
	USNIC_TRANSPORT_IPV4_UDP	= 2,
	USNIC_TRANSPORT_MAX		= 3,
};

struct usnic_transport_spec {
	enum usnic_transport_type	trans_type;
	union {
		struct {
			uint16_t	port_num;
		} usnic_roce;
		struct {
			uint32_t	sock_fd;
		} udp;
	};
};

#define USNIC_IB_CREATE_QP_VERSION 1

struct usnic_ib_create_qp_cmd_v0 {
	struct usnic_transport_spec	spec;
};

struct usnic_ib_create_qp_cmd {
	struct usnic_transport_spec	spec;
	u32				cmd_version;
};

/*
 * infomation of vnic bar resource
 */
struct usnic_vnic_barres_info {
	int32_t	 	type;
	uint32_t	padding;
	uint64_t	bus_addr;
	uint64_t	len;
};

/*
 * All create_qp responses must start with this for backwards compatability
 */
#define USNIC_IB_CREATE_QP_RESP_V0_FIELDS                               \
	u32				vfid;                           \
	u32				qp_grp_id;                      \
	u64				bar_bus_addr;                   \
	u32				bar_len;                        \
	u32				wq_cnt;                         \
	u32				rq_cnt;                         \
	u32				cq_cnt;                         \
	u32				wq_idx[USNIC_QP_GRP_MAX_WQS];   \
	u32				rq_idx[USNIC_QP_GRP_MAX_RQS];   \
	u32				cq_idx[USNIC_QP_GRP_MAX_CQS];   \
	u32				transport;

struct usnic_ib_create_qp_resp_v0 {
	USNIC_IB_CREATE_QP_RESP_V0_FIELDS
};

struct usnic_ib_create_qp_resp {
	USNIC_IB_CREATE_QP_RESP_V0_FIELDS
	u32				cmd_version;
	u32				num_barres;
	u32				pad_to_8byte;
	struct usnic_vnic_barres_info	resources[0];
};

#define USNIC_CTX_RESP_VERSION 1

struct usnic_ib_get_context_cmd	{
	u32 resp_version;	/* response version requested */
	u32 num_caps;		/* number of capabilities requested */
};

/*
 * Note that this enum must never have members removed or re-ordered in order
 * to retain backwards compatability
 */
enum usnic_capability {
	USNIC_CAP_CQ_SHARING,	/* CQ sharing version */
	USNIC_CAP_MAP_PER_RES,	/* Map individual RES */
	USNIC_CAP_PIO,		/* PIO send */
	USNIC_CAP_CNT
};

/*
 * If and when there become multiple versions of this struct, it will
 * become a union for cross-version compatability to make sure there is always
 * space for older and larger versions of the contents.
 */
struct usnic_ib_get_context_resp {
	u32 resp_version;	/* response version returned */
	u32 num_caps;		/* number of capabilities returned */
	u32 cap_info[USNIC_CAP_CNT];
};

#endif /* USNIC_ABI_H */
