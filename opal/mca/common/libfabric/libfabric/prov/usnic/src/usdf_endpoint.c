/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
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
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <asm/types.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/epoll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_errno.h>
#include "fi.h"
#include "fi_enosys.h"

#include "usnic_direct.h"
#include "usd.h"
#include "usdf.h"
#include "usdf_av.h"
#include "usdf_endpoint.h"
#include "usdf_progress.h"

static int
usdf_ep_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	struct usdf_ep *ep;

	ep = ep_fidtou(fid);

	switch (bfid->fclass) {

	case FI_CLASS_AV:
		if (ep->ep_av != NULL) {
			return -FI_EINVAL;
		}
		ep->ep_av = av_fidtou(bfid);
		break;

	case FI_CLASS_CQ:
		if (flags & FI_SEND) {
			if (ep->ep_wcq != NULL) {
				return -FI_EINVAL;
			}
			ep->ep_wcq = cq_fidtou(bfid);
		}

		if (flags & FI_RECV) {
			if (ep->ep_rcq != NULL) {
				return -FI_EINVAL;
			}
			ep->ep_rcq = cq_fidtou(bfid);
		}
		break;

	case FI_CLASS_EQ:
printf("bind EQ to ep!\n");
		if (ep->ep_eq != NULL) {
			return -FI_EINVAL;
		}
		ep->ep_eq = eq_fidtou(bfid);
		atomic_inc(&ep->ep_eq->eq_refcnt);
		break;
	default:
		return -FI_EINVAL;
	}

	return 0;
}

static int
usdf_ep_close(fid_t fid)
{
	struct usdf_ep *ep;

	ep = ep_fidtou(fid);

	if (atomic_get(&ep->ep_refcnt) > 0) {
		return -FI_EBUSY;
	}

	if (ep->ep_qp != NULL) {
		usd_destroy_qp(ep->ep_qp);
	}
	atomic_dec(&ep->ep_domain->dom_refcnt);
	if (ep->ep_eq != NULL) {
		atomic_dec(&ep->ep_eq->eq_refcnt);
	}
	
	free(ep);
	return 0;
}

struct fi_ops usdf_ep_ops = {
	.size = sizeof(struct fi_ops),
	.close = usdf_ep_close,
	.bind = usdf_ep_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open
};

int
usdf_ep_port_bind(struct usdf_ep *ep, struct fi_info *info)
{
	struct sockaddr_in *sin;
	socklen_t addrlen;
	int ret;

	sin = (struct sockaddr_in *)info->src_addr;
	ret = bind(ep->ep_sock, (struct sockaddr *)sin, sizeof(*sin));
	if (ret == -1) {
		return -errno;
	}

	addrlen = sizeof(*sin);
	ret = getsockname(ep->ep_sock, (struct sockaddr *)sin, &addrlen);
	if (ret == -1) {
		return -errno;
	}

	return 0;
}

int
usdf_endpoint_open(struct fid_domain *domain, struct fi_info *info,
	    struct fid_ep **ep_o, void *context)
{
	switch (info->ep_type) {
	case FI_EP_DGRAM:
		return usdf_ep_dgram_open(domain, info, ep_o, context);
	case FI_EP_MSG:
		return usdf_ep_msg_open(domain, info, ep_o, context);
	default:
		return -FI_ENODEV;
	}
}
