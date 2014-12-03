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
#include <unistd.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_errno.h>
#include "fi.h"

#include "usnic_direct.h"
#include "usdf.h"
#include "usdf_dgram.h"
#include "usdf_cm.h"
#include "usdf_msg.h"


static struct fi_ops_msg usdf_dgram_conn_ops = {
	.size = sizeof(struct fi_ops_msg),
	.recv = usdf_dgram_recv,
	.recvv = usdf_dgram_recvv,
	.recvmsg = usdf_dgram_recvmsg,
	.send = usdf_dgram_conn_send,
	.sendv = usdf_dgram_sendv,
	.sendmsg = usdf_dgram_sendmsg,
	.inject = usdf_dgram_inject,
	.senddata = usdf_dgram_senddata,
};

int
usdf_cm_dgram_connect(struct fid_ep *fep, const void *addr,
		const void *param, size_t paramlen)
{
	struct usdf_ep *ep;
	const struct sockaddr_in *sin;
	int ret;

	ep = ep_ftou(fep);
	sin = addr;

	ret = usd_create_dest(ep->ep_domain->dom_dev, sin->sin_addr.s_addr,
			sin->sin_port, &ep->ep_dest);
	if (!ret) {
		ep->ep_fid.msg = &usdf_dgram_conn_ops;
	}

	return ret;
}

int
usdf_cm_dgram_shutdown(struct fid_ep *ep, uint64_t flags)
{
	return 0;  // XXX
}

int
usdf_cm_msg_connect(struct fid_ep *fep, const void *addr,
		const void *param, size_t paramlen)
{
	struct usdf_ep *ep;
	const struct sockaddr_in *sin;
	int ret;

	ep = ep_ftou(fep);
	sin = addr;

	ep->ep_conn_sock = socket(AF_INET, SOCK_STREAM, 0);
	if (ep->ep_conn_sock == -1) {
		ret = -errno;
		goto fail;
	}

	ret = fcntl(ep->ep_conn_sock, F_GETFL, 0);
	if (ret == -1) {
		ret = -errno;
		goto fail;
	}
	ret = fcntl(ep->ep_conn_sock, F_SETFL, ret | O_NONBLOCK);
	if (ret == -1) {
		ret = -errno;
		goto fail;
	}

	ret = connect(ep->ep_conn_sock, (struct sockaddr *)sin, sizeof(*sin));
	if (ret != 0 && errno != EINPROGRESS) {
		ret = -errno;
		goto fail;
	}
printf("connect in progress\n");

	return 0;

fail:
	if (ep->ep_conn_sock != -1) {
		close(ep->ep_conn_sock);
	}
	return ret;
}

int
usdf_cm_msg_shutdown(struct fid_ep *ep, uint64_t flags)
{
	return -FI_ENOSYS;
}
