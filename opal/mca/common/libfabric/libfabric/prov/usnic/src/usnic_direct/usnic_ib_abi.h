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


#ifndef USNIC_IB_ABI_H
#define USNIC_IB_ABI_H

#include <infiniband/kern-abi.h>

/*
 * Pick up common file with driver
 */
#include "usnic_abi.h"

struct usnic_create_qp_resp {
	struct ibv_create_qp_resp   ibv_resp;
	struct usnic_ib_create_qp_resp usnic_resp;
};

struct usnic_get_context {
	struct ibv_get_context		ibv_cmd;
	struct usnic_ib_get_context_cmd usnic_cmd;
	__u64				reserved;
};

struct usnic_get_context_resp {
	struct ibv_get_context_resp	ibv_resp;
	struct usnic_ib_get_context_resp usnic_resp;
	__u64				reserved;
};

struct usnic_alloc_pd {
	struct ibv_alloc_pd		ibv_cmd;
	__u64				reserved;
};

struct usnic_alloc_pd_resp {
	struct ibv_alloc_pd_resp	ibv_resp;
	__u64				reserved;
};

struct usnic_reg_mr {
	struct ibv_reg_mr		ibv_cmd;
	__u64				reserved;
};

struct usnic_reg_mr_resp {
	struct ibv_reg_mr_resp		ibv_resp;
	__u64				reserved;
};

struct usnic_create_cq {
	struct ibv_create_cq		ibv_cmd;
	struct usnic_ib_create_cq_cmd   usnic_cmd;
	__u64				reserved;
};

struct usnic_create_cq_resp {
	struct ibv_create_cq_resp	ibv_resp;
	__u64				reserved;
};

struct usnic_create_qp {
	struct ibv_create_qp		ibv_cmd;
	struct usnic_ib_create_qp_cmd	usnic_cmd;
	__u64				reserved[8];
};

#endif /* USNIC_IB_ABI_H */
