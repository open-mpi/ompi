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
#include <sys/queue.h>

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_errno.h>
#include "fi.h"

#include "usnic_direct.h"
#include "usd.h"
#include "usdf.h"
#include "usdf_av.h"

static ssize_t
usdf_cq_readerr(struct fid_cq *fcq, struct fi_cq_err_entry *entry,
	        uint64_t flags)
{
	struct usdf_cq *cq;

	cq = container_of(fcq, struct usdf_cq, cq_fid);

	// If top entry has no error, return 0
	if (cq->cq_comp.uc_status == 0) {
		return 0;
	}

	entry->op_context = cq->cq_comp.uc_context;
	entry->flags = 0;
	entry->err = FI_EIO;
	entry->prov_errno = cq->cq_comp.uc_status;

	cq->cq_comp.uc_status = 0;

	return sizeof(*entry);
}

static ssize_t
usdf_cq_sread(struct fid_cq *cq, void *buf, size_t count, const void *cond,
		int timeout)
{
	return -FI_ENOSYS;
}

static ssize_t
usdf_cq_read_context(struct fid_cq *fcq, void *buf, size_t count)
{
	struct usdf_cq *cq;
	struct fi_cq_entry *entry;
	struct fi_cq_entry *last;
	ssize_t ret;

	cq = cq_ftou(fcq);
	if (cq->cq_comp.uc_status != 0) {
		return -FI_EAVAIL;
	}

	ret = 0;
	entry = buf;
	last = entry + count;
	while (entry < last) {
		ret = usd_poll_cq(cq->cq_cq, &cq->cq_comp);
		if (ret == -EAGAIN) {
			ret = 0;
			break;
		}
		if (cq->cq_comp.uc_status != 0) {
			ret = -FI_EAVAIL;
			break;
		}

		entry->op_context = cq->cq_comp.uc_context;

		entry++;
	}

	if (entry > (struct fi_cq_entry *)buf) {
		return entry - (struct fi_cq_entry *)buf;
	} else {
		return ret;
	}
}

static ssize_t
usdf_cq_readfrom_context(struct fid_cq *fcq, void *buf, size_t count,
			fi_addr_t *src_addr)
{
	struct usdf_cq *cq;
	struct usd_cq_impl *ucq;
	struct fi_cq_entry *entry;
	struct fi_cq_entry *last;
	ssize_t ret;
	struct cq_desc *cq_desc;
	struct usdf_ep *ep;
	struct sockaddr_in sin;
	struct usd_udp_hdr *hdr;
	uint16_t index;

	cq = cq_ftou(fcq);
	if (cq->cq_comp.uc_status != 0) {
		return -FI_EAVAIL;
	}
	ucq = to_cqi(cq->cq_cq);

	ret = 0;
	entry = buf;
	last = entry + count;
	while (entry < last) {
		cq_desc = (struct cq_desc *)((uint8_t *)ucq->ucq_desc_ring +
				(ucq->ucq_next_desc << 4));

		ret = usd_poll_cq(cq->cq_cq, &cq->cq_comp);
		if (ret == -EAGAIN) {
			ret = 0;
			break;
		}
		if (cq->cq_comp.uc_status != 0) {
			ret = -FI_EAVAIL;
			break;
		}

		if (cq->cq_comp.uc_type == USD_COMPTYPE_RECV) {
			index = le16_to_cpu(cq_desc->completed_index) &
				CQ_DESC_COMP_NDX_MASK;
			ep = cq->cq_comp.uc_qp->uq_context;
			hdr = ep->ep_hdr_ptr[index];
			memset(&sin, 0, sizeof(sin));

			sin.sin_addr.s_addr = hdr->uh_ip.saddr;
			sin.sin_port = hdr->uh_udp.source;

			ret = fi_av_insert(av_utof(ep->ep_av), &sin, 1,
					src_addr, 0, NULL);
			if (ret != 1) {
				*src_addr = FI_ADDR_NOTAVAIL;
			}
			++src_addr;
		}
			

		entry->op_context = cq->cq_comp.uc_context;

		entry++;
	}

	if (entry > (struct fi_cq_entry *)buf) {
		return entry - (struct fi_cq_entry *)buf;
	} else {
		return ret;
	}
}

static ssize_t
usdf_cq_read_msg(struct fid_cq *fcq, void *buf, size_t count)
{
	struct usdf_cq *cq;
	struct fi_cq_msg_entry *entry;
	struct fi_cq_msg_entry *last;
	ssize_t ret;

	cq = cq_ftou(fcq);
	if (cq->cq_comp.uc_status != 0) {
		return -FI_EAVAIL;
	}

	ret = 0;
	entry = buf;
	last = entry + count;
	while (entry < last) {
		ret = usd_poll_cq(cq->cq_cq, &cq->cq_comp);
		if (ret == -EAGAIN) {
			ret = 0;
			break;
		}
		if (cq->cq_comp.uc_status != 0) {
			ret = -FI_EAVAIL;
			break;
		}

		entry->op_context = cq->cq_comp.uc_context;
		entry->flags = 0;
		entry->len = cq->cq_comp.uc_bytes;

		entry++;
	}

	if (entry > (struct fi_cq_msg_entry *)buf) {
		return entry - (struct fi_cq_msg_entry *)buf;
	} else {
		return ret;
	}
}

static ssize_t
usdf_cq_read_data(struct fid_cq *fcq, void *buf, size_t count)
{
	struct usdf_cq *cq;
	struct fi_cq_data_entry *entry;
	struct fi_cq_data_entry *last;
	ssize_t ret;

	cq = cq_ftou(fcq);
	if (cq->cq_comp.uc_status != 0) {
		return -FI_EAVAIL;
	}

	ret = 0;
	entry = buf;
	last = entry + count;
	while (entry < last) {
		ret = usd_poll_cq(cq->cq_cq, &cq->cq_comp);
		if (ret == -EAGAIN) {
			ret = 0;
			break;
		}
		if (cq->cq_comp.uc_status != 0) {
			ret = -FI_EAVAIL;
			break;
		}

		entry->op_context = cq->cq_comp.uc_context;
		entry->flags = 0;
		entry->len = cq->cq_comp.uc_bytes;
		entry->buf = 0;	/* XXX */
		entry->data = 0;

		entry++;
	}

	if (entry > (struct fi_cq_data_entry *)buf) {
		return entry - (struct fi_cq_data_entry *)buf;
	} else {
		return ret;
	}
}

static const char *
usdf_cq_strerror(struct fid_cq *eq, int prov_errno, const void *err_data,
		 char *buf, size_t len)
{
	strncpy(buf, "CQ Error", len-1);
	buf[len-1] = '\0';
	return buf;
}

static struct fi_ops_cq usdf_cq_context_ops = {
	.size = sizeof(struct fi_ops_cq),
	.read = usdf_cq_read_context,
	.sread = usdf_cq_sread,
	.readfrom = usdf_cq_readfrom_context,
	.readerr = usdf_cq_readerr,
	.strerror = usdf_cq_strerror
};

static struct fi_ops_cq usdf_cq_msg_ops = {
	.size = sizeof(struct fi_ops_cq),
	.read = usdf_cq_read_msg,
	.sread = usdf_cq_sread,
	.readerr = usdf_cq_readerr,
	.strerror = usdf_cq_strerror
};

static struct fi_ops_cq usdf_cq_data_ops = {
	.size = sizeof(struct fi_ops_cq),
	.read = usdf_cq_read_data,
	.sread = usdf_cq_sread,
	.readerr = usdf_cq_readerr,
	.strerror = usdf_cq_strerror
};

static int
usdf_cq_control(fid_t fid, int command, void *arg)
{
	return -FI_ENOSYS;
}

static int
usdf_cq_close(fid_t fid)
{
	struct usdf_cq *cq;
	int ret;

	cq = container_of(fid, struct usdf_cq, cq_fid.fid);
	if (cq->cq_cq) {
		ret = usd_destroy_cq(cq->cq_cq);
		if (ret != 0) {
			return ret;
		}
	}

	free(cq);
	return 0;
}

static struct fi_ops usdf_cq_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = usdf_cq_close,
	.control = usdf_cq_control,
};

int
usdf_cq_open(struct fid_domain *domain, struct fi_cq_attr *attr,
	    struct fid_cq **cq_o, void *context)
{
	struct usdf_cq *cq;
	int ret;

	if (attr->wait_obj != FI_WAIT_NONE) {
		return -FI_ENOSYS;
	}

	cq = calloc(1, sizeof(*cq));
	if (cq == NULL) {
		return -FI_ENOMEM;
	}

	cq->cq_domain = container_of(domain, struct usdf_domain, dom_fid);

	ret = usd_create_cq(cq->cq_domain->dom_dev, attr->size, -1, &cq->cq_cq);
	if (ret != 0) {
		goto fail;
	}

	cq->cq_fid.fid.fclass = FI_CLASS_CQ;
	cq->cq_fid.fid.context = context;
	cq->cq_fid.fid.ops = &usdf_cq_fi_ops;

	switch (attr->format) {
	case FI_CQ_FORMAT_CONTEXT:
		cq->cq_fid.ops = &usdf_cq_context_ops;
		break;
	case FI_CQ_FORMAT_MSG:
		cq->cq_fid.ops = &usdf_cq_msg_ops;
		break;
	case FI_CQ_FORMAT_DATA:
		cq->cq_fid.ops = &usdf_cq_data_ops;
		break;
	default:
		ret = -FI_ENOSYS;
		goto fail;
	}

	*cq_o = &cq->cq_fid;
	return 0;

fail:
	if (cq != NULL) {
		if (cq->cq_cq != NULL) {
			usd_destroy_cq(cq->cq_cq);
		}
		free(cq);
	}
	return ret;
}
