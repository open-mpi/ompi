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
#include <poll.h>
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

#include "usnic_direct.h"
#include "usdf.h"

static
int usdf_dereg_mr(fid_t fid)
{
	struct usdf_mr *mr;
	int ret;

	mr = container_of(fid, struct usdf_mr, mr_fid.fid);
	ret = usd_dereg_mr(mr->mr_mr);
	if (ret == 0) {
		free(mr);
	}
	return ret;
}

static struct fi_ops usdf_mr_ops = {
	.size = sizeof(struct fi_ops),
	.close = usdf_dereg_mr
};

int
usdf_reg_mr(struct fid *fid, const void *buf, size_t len,
	   uint64_t access, uint64_t offset, uint64_t requested_key,
	   uint64_t flags, struct fid_mr **mr_o, void *context)
{
	struct usdf_mr *mr;
	struct usdf_domain *udp;
	int ret;
	struct fid_domain *domain;

	if (flags != 0) {
		return -FI_EBADFLAGS;
	}

	if (fid->fclass != FI_CLASS_DOMAIN) {
		USDF_DEBUG("memory registration only supported "
				"for struct fid_domain\n");
		return -FI_EINVAL;
	}
	domain = container_of(fid, struct fid_domain, fid);

	mr = calloc(1, sizeof *mr);
	if (mr == NULL) {
		return -FI_ENOMEM;
	}

	mr->mr_fid.fid.fclass = FI_CLASS_MR;
	mr->mr_fid.fid.context = context;
	mr->mr_fid.fid.ops = &usdf_mr_ops;

	udp = container_of(domain, struct usdf_domain, dom_fid.fid);
	ret = usd_reg_mr(udp->dom_dev, (void *) buf, len, &mr->mr_mr);
	if (ret != 0) {
		goto fail;
	}

	*mr_o = &mr->mr_fid;
	return 0;

fail:
	free(mr);
	return ret;
}
