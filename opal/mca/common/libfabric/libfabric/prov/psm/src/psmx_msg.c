/*
 * Copyright (c) 2013 Intel Corporation. All rights reserved.
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "psmx.h"

ssize_t _psmx_recvfrom(struct fid_ep *ep, void *buf, size_t len,
		       void *desc, fi_addr_t src_addr, void *context,
		       uint64_t flags)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	struct psmx_epaddr_context *epaddr_context;
	psm_mq_req_t psm_req;
	uint64_t psm_tag, psm_tagsel;
	struct fi_context *fi_context;
	int user_fi_context = 0;
	int err;
	int recv_flag = 0;
	size_t idx;

	if (flags & FI_TRIGGER) {
		struct psmx_trigger *trigger;
		struct fi_triggered_context *ctxt = context;

		trigger = calloc(1, sizeof(*trigger));
		if (!trigger)
			return -ENOMEM;

		trigger->op = PSMX_TRIGGERED_RECV;
		trigger->cntr = container_of(ctxt->threshold.cntr,
					     struct psmx_fid_cntr, cntr);
		trigger->threshold = ctxt->threshold.threshold;
		trigger->recv.ep = ep;
		trigger->recv.buf = buf;
		trigger->recv.len = len;
		trigger->recv.desc = desc;
		trigger->recv.src_addr = src_addr;
		trigger->recv.context = context;
		trigger->recv.flags = flags & ~FI_TRIGGER;

		psmx_cntr_add_trigger(trigger->cntr, trigger);
		return 0;
	}

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	if (src_addr) {
		av = ep_priv->av;
		if (av && av->type == FI_AV_TABLE) {
			idx = (size_t)src_addr;
			if (idx >= av->last)
				return -EINVAL;

			src_addr = (fi_addr_t)av->psm_epaddrs[idx];
		}
		epaddr_context = psm_epaddr_getctxt((void *)src_addr);
		psm_tag = epaddr_context->epid | PSMX_MSG_BIT;
		psm_tagsel = -1ULL;
	}
	else {
		psm_tag = PSMX_MSG_BIT;
		psm_tagsel = PSMX_MSG_BIT;
	}

	if (ep_priv->recv_cq_event_flag && !(flags & FI_EVENT) && !context) {
		fi_context = &ep_priv->nocomp_recv_context;
	}
	else {
		if (!context)
			return -EINVAL;

		fi_context = context;
		user_fi_context = 1;
		if (flags & FI_MULTI_RECV) {
			struct psmx_multi_recv *req;

			req = calloc(1, sizeof(*req));
			if (!req)
				return -ENOMEM;

			req->tag = psm_tag;
			req->tagsel = psm_tagsel;
			req->flag = recv_flag;
			req->buf = buf;
			req->len = len;
			req->offset = 0;
			req->min_buf_size = ep_priv->min_multi_recv;
			req->context = fi_context; 
			PSMX_CTXT_TYPE(fi_context) = PSMX_MULTI_RECV_CONTEXT;
			PSMX_CTXT_USER(fi_context) = req;
		}
		else {
			PSMX_CTXT_TYPE(fi_context) = PSMX_RECV_CONTEXT;
			PSMX_CTXT_USER(fi_context) = buf;
		}
		PSMX_CTXT_EP(fi_context) = ep_priv;
	}

	err = psm_mq_irecv(ep_priv->domain->psm_mq,
			   psm_tag, psm_tagsel, recv_flag,
			   buf, len, (void *)fi_context, &psm_req);
	if (err != PSM_OK)
		return psmx_errno(err);

	if (user_fi_context)
		PSMX_CTXT_REQ(fi_context) = psm_req;

	return 0;
}

static ssize_t psmx_recvfrom(struct fid_ep *ep, void *buf, size_t len, void *desc,
			     fi_addr_t src_addr, void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	return _psmx_recvfrom(ep, buf, len, desc, src_addr, context, ep_priv->flags);
}

static ssize_t psmx_recvmsg(struct fid_ep *ep, const struct fi_msg *msg, uint64_t flags)
{
	void *buf;
	size_t len;

	if (!msg || msg->iov_count > 1)
		return -EINVAL;

	if (msg->iov_count) {
		buf = msg->msg_iov[0].iov_base;
		len = msg->msg_iov[0].iov_len;
	}
	else {
		buf = NULL;
		len = 0;
	}

	return _psmx_recvfrom(ep, buf, len,
			      msg->desc ? msg->desc[0] : NULL, msg->addr,
			      msg->context, flags);
}

static ssize_t psmx_recv(struct fid_ep *ep, void *buf, size_t len, void *desc,
			 void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	if (ep_priv->connected)
		return psmx_recvfrom(ep, buf, len, desc,
				     (fi_addr_t) ep_priv->peer_psm_epaddr, context);
	else
		return psmx_recvfrom(ep, buf, len, desc, 0, context);
}

static ssize_t psmx_recvv(struct fid_ep *ep, const struct iovec *iov, void **desc,
			  size_t count, void *context)
{
	void *buf;
	size_t len;

	if (!iov || count > 1)
		return -EINVAL;

	if (count) {
		buf = iov[0].iov_base;
		len = iov[0].iov_len;
	}
	else {
		buf = NULL;
		len = 0;
	}

	return psmx_recv(ep, buf, len, desc ? desc[0] : NULL, context);
}

ssize_t _psmx_sendto(struct fid_ep *ep, const void *buf, size_t len,
		     void *desc, fi_addr_t dest_addr, void *context,
		     uint64_t flags)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	int send_flag = 0;
	psm_epaddr_t psm_epaddr;
	psm_mq_req_t psm_req;
	uint64_t psm_tag;
	struct fi_context * fi_context;
	int user_fi_context = 0;
	int err;
	size_t idx;

	if (flags & FI_TRIGGER) {
		struct psmx_trigger *trigger;
		struct fi_triggered_context *ctxt = context;

		trigger = calloc(1, sizeof(*trigger));
		if (!trigger)
			return -ENOMEM;

		trigger->op = PSMX_TRIGGERED_SEND;
		trigger->cntr = container_of(ctxt->threshold.cntr,
					     struct psmx_fid_cntr, cntr);
		trigger->threshold = ctxt->threshold.threshold;
		trigger->send.ep = ep;
		trigger->send.buf = buf;
		trigger->send.len = len;
		trigger->send.desc = desc;
		trigger->send.dest_addr = dest_addr;
		trigger->send.context = context;
		trigger->send.flags = flags & ~FI_TRIGGER;

		psmx_cntr_add_trigger(trigger->cntr, trigger);
		return 0;
	}

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	assert(ep_priv->domain);

	av = ep_priv->av;
	if (av && av->type == FI_AV_TABLE) {
		idx = (size_t)dest_addr;
		if (idx >= av->last)
			return -EINVAL;

		psm_epaddr = av->psm_epaddrs[idx];
	}
	else  {
		psm_epaddr = (psm_epaddr_t) dest_addr;
	}

	psm_tag = ep_priv->domain->psm_epid | PSMX_MSG_BIT;

	if (flags & FI_INJECT) {
		fi_context = malloc(sizeof(*fi_context) + len);
		if (!fi_context)
			return -ENOMEM;

		memcpy((void *)fi_context + sizeof(*fi_context), buf, len);
		buf = (void *)fi_context + sizeof(*fi_context);

		PSMX_CTXT_TYPE(fi_context) = PSMX_INJECT_CONTEXT;
		PSMX_CTXT_EP(fi_context) = ep_priv;
	}
	else if (ep_priv->send_cq_event_flag && !(flags & FI_EVENT) && !context) {
		fi_context = &ep_priv->nocomp_send_context;
	}
	else {
		if (!context)
			return -EINVAL;

		fi_context = context;
		if (fi_context != &ep_priv->sendimm_context) {
			user_fi_context = 1;
			PSMX_CTXT_TYPE(fi_context) = PSMX_SEND_CONTEXT;
			PSMX_CTXT_USER(fi_context) = (void *)buf;
			PSMX_CTXT_EP(fi_context) = ep_priv;
		}
	}

	err = psm_mq_isend(ep_priv->domain->psm_mq, psm_epaddr, send_flag,
				psm_tag, buf, len, (void *)fi_context, &psm_req);

	if (err != PSM_OK)
		return psmx_errno(err);

	ep_priv->pending_sends++;

	if (user_fi_context)
		PSMX_CTXT_REQ(fi_context) = psm_req;

	return 0;
}

static ssize_t psmx_sendto(struct fid_ep *ep, const void *buf, size_t len,
			   void *desc, fi_addr_t dest_addr, void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	return _psmx_sendto(ep, buf, len, desc, dest_addr, context, ep_priv->flags);
}

static ssize_t psmx_sendmsg(struct fid_ep *ep, const struct fi_msg *msg, uint64_t flags)
{
	void *buf;
	size_t len;

	if (!msg || msg->iov_count > 1)
		return -EINVAL;

	if (msg->iov_count) {
		buf = msg->msg_iov[0].iov_base;
		len = msg->msg_iov[0].iov_len;
	}
	else {
		buf = NULL;
		len = 0;
	}

	return _psmx_sendto(ep, buf, len,
			    msg->desc ? msg->desc[0] : NULL, msg->addr,
			    msg->context, flags);
}

static ssize_t psmx_send(struct fid_ep *ep, const void *buf, size_t len, void *desc,
			 void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	if (!ep_priv->connected)
		return -ENOTCONN;

	return psmx_sendto(ep, buf, len, desc, (fi_addr_t) ep_priv->peer_psm_epaddr, context);
}

static ssize_t psmx_sendv(struct fid_ep *ep, const struct iovec *iov, void **desc,
			  size_t count, void *context)
{
	void *buf;
	size_t len;

	if (!iov || count > 1)
		return -EINVAL;

	if (count) {
		buf = iov[0].iov_base;
		len = iov[0].iov_len;
	}
	else {
		buf = NULL;
		len = 0;
	}

	return psmx_send(ep, buf, len, desc ? desc[0] : NULL, context);
}

static ssize_t psmx_injectto(struct fid_ep *ep, const void *buf, size_t len,
			     fi_addr_t dest_addr)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	return _psmx_sendto(ep, buf, len, NULL, dest_addr, NULL,
			    ep_priv->flags | FI_INJECT);
}

static ssize_t psmx_inject(struct fid_ep *ep, const void *buf, size_t len)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	if (!ep_priv->connected)
		return -ENOTCONN;

	return psmx_injectto(ep, buf, len, (fi_addr_t) ep_priv->peer_psm_epaddr);
}

struct fi_ops_msg psmx_msg_ops = {
	.size = sizeof(struct fi_ops_msg),
	.recv = psmx_recv,
	.recvv = psmx_recvv,
	.recvfrom = psmx_recvfrom,
	.recvmsg = psmx_recvmsg,
	.send = psmx_send,
	.sendv = psmx_sendv,
	.sendto = psmx_sendto,
	.sendmsg = psmx_sendmsg,
	.inject = psmx_inject,
	.injectto = psmx_injectto,
	.senddata = fi_no_msg_senddata,
	.senddatato = fi_no_msg_senddatato,
};

