/*
 * Copyright (c) 2013-2014 Intel Corporation. All rights reserved.
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

ssize_t _psmx_tagged_recv(struct fid_ep *ep, void *buf, size_t len,
			  void *desc, fi_addr_t src_addr,
			  uint64_t tag, uint64_t ignore,
			  void *context, uint64_t flags)
{
	struct psmx_fid_ep *ep_priv;
	psm_mq_req_t psm_req;
	uint64_t psm_tag, psm_tagsel;
	struct fi_context *fi_context;
	int user_fi_context = 0;
	int err;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	if (src_addr)
		psmx_debug("%s: warning: src_addr is currently ignored.", __func__);

	if (flags & FI_TRIGGER) {
		struct psmx_trigger *trigger;
		struct fi_triggered_context *ctxt = context;

		trigger = calloc(1, sizeof(*trigger));
		if (!trigger)
			return -ENOMEM;

		trigger->op = PSMX_TRIGGERED_TRECV;
		trigger->cntr = container_of(ctxt->threshold.cntr,
					     struct psmx_fid_cntr, cntr);
		trigger->threshold = ctxt->threshold.threshold;
		trigger->trecv.ep = ep;
		trigger->trecv.buf = buf;
		trigger->trecv.len = len;
		trigger->trecv.desc = desc;
		trigger->trecv.src_addr = src_addr;
		trigger->trecv.tag = tag;
		trigger->trecv.ignore = ignore;
		trigger->trecv.context = context;
		trigger->trecv.flags = flags & ~FI_TRIGGER;

		psmx_cntr_add_trigger(trigger->cntr, trigger);
		return 0;
	}

	if (tag & ep_priv->domain->reserved_tag_bits) {
		fprintf(stderr, "%s: warning: using reserved tag bits."
			"tag=%lx. reserved_bits=%lx.\n", __func__, tag,
			ep_priv->domain->reserved_tag_bits);
	}

	psm_tag = tag & (~ep_priv->domain->reserved_tag_bits);
	psm_tagsel = (~ignore) | ep_priv->domain->reserved_tag_bits;

	if (ep_priv->recv_cq_event_flag && !(flags & FI_EVENT) && !context) {
		fi_context = &ep_priv->nocomp_recv_context;
	}
	else {
		if (!context)
			return -EINVAL;

		fi_context = context;
		user_fi_context= 1;
		PSMX_CTXT_TYPE(fi_context) = PSMX_RECV_CONTEXT;
		PSMX_CTXT_USER(fi_context) = buf;
		PSMX_CTXT_EP(fi_context) = ep_priv;
	}

	err = psm_mq_irecv(ep_priv->domain->psm_mq,
			   psm_tag, psm_tagsel, 0, /* flags */
			   buf, len, (void *)fi_context, &psm_req);
	if (err != PSM_OK)
		return psmx_errno(err);

	if (user_fi_context)
		PSMX_CTXT_REQ(fi_context) = psm_req;

	return 0;
}

ssize_t psmx_tagged_recv_no_flag_av_map(struct fid_ep *ep, void *buf,
					size_t len, void *desc,
					fi_addr_t src_addr,
					uint64_t tag, uint64_t ignore,
					void *context)
{
	struct psmx_fid_ep *ep_priv;
	psm_mq_req_t psm_req;
	uint64_t psm_tag, psm_tagsel;
	struct fi_context *fi_context;
	int err;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	/* NOTE: src_addr currently unused */

	psm_tag = tag & (~ep_priv->domain->reserved_tag_bits);
	psm_tagsel = (~ignore) | ep_priv->domain->reserved_tag_bits;

	fi_context = context;
	PSMX_CTXT_TYPE(fi_context) = PSMX_RECV_CONTEXT;
	PSMX_CTXT_USER(fi_context) = buf;
	PSMX_CTXT_EP(fi_context) = ep_priv;

	err = psm_mq_irecv(ep_priv->domain->psm_mq,
			   psm_tag, psm_tagsel, 0, /* flags */
			   buf, len, (void *)fi_context, &psm_req);
	if (err != PSM_OK)
		return psmx_errno(err);

	PSMX_CTXT_REQ(fi_context) = psm_req;
	return 0;
}

ssize_t psmx_tagged_recv_no_flag_av_table(struct fid_ep *ep, void *buf,
					  size_t len, void *desc,
					  fi_addr_t src_addr,
					  uint64_t tag, uint64_t ignore,
					  void *context)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	psm_epaddr_t psm_epaddr;
	psm_mq_req_t psm_req;
	uint64_t psm_tag, psm_tagsel;
	struct fi_context *fi_context;
	int err;
	size_t idx;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	av = ep_priv->av;
	idx = (size_t)src_addr;
	if (idx >= av->last)
		return -EINVAL;

	psm_epaddr = av->psm_epaddrs[idx];

	/* NOTE: psm_epaddr currently unused */

	psm_tag = tag & (~ep_priv->domain->reserved_tag_bits);
	psm_tagsel = (~ignore) | ep_priv->domain->reserved_tag_bits;

	fi_context = context;
	PSMX_CTXT_TYPE(fi_context) = PSMX_RECV_CONTEXT;
	PSMX_CTXT_USER(fi_context) = buf;
	PSMX_CTXT_EP(fi_context) = ep_priv;

	err = psm_mq_irecv(ep_priv->domain->psm_mq,
			   psm_tag, psm_tagsel, 0, /* flags */
			   buf, len, (void *)fi_context, &psm_req);
	if (err != PSM_OK)
		return psmx_errno(err);

	PSMX_CTXT_REQ(fi_context) = psm_req;
	return 0;
}

ssize_t psmx_tagged_recv_no_event_av_map(struct fid_ep *ep, void *buf,
					 size_t len, void *desc,
					 fi_addr_t src_addr,
					 uint64_t tag, uint64_t ignore,
					 void *context)
{
	struct psmx_fid_ep *ep_priv;
	psm_mq_req_t psm_req;
	uint64_t psm_tag, psm_tagsel;
	struct fi_context *fi_context;
	int err;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	/* NOTE: src_addr currently unused */

	psm_tag = tag & (~ep_priv->domain->reserved_tag_bits);
	psm_tagsel = (~ignore) | ep_priv->domain->reserved_tag_bits;

	fi_context = &ep_priv->nocomp_recv_context;

	err = psm_mq_irecv(ep_priv->domain->psm_mq,
			   psm_tag, psm_tagsel, 0, /* flags */
			   buf, len, (void *)fi_context, &psm_req);

	return psmx_errno(err);
}

ssize_t psmx_tagged_recv_no_event_av_table(struct fid_ep *ep, void *buf,
					   size_t len, void *desc,
					   fi_addr_t src_addr,
					   uint64_t tag, uint64_t ignore,
					   void *context)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	psm_epaddr_t psm_epaddr;
	psm_mq_req_t psm_req;
	uint64_t psm_tag, psm_tagsel;
	struct fi_context *fi_context;
	int err;
	size_t idx;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	av = ep_priv->av;
	idx = (size_t)src_addr;
	if (idx >= av->last)
		return -EINVAL;

	psm_epaddr = av->psm_epaddrs[idx];

	/* NOTE: psm_epaddr currently unused */

	psm_tag = tag & (~ep_priv->domain->reserved_tag_bits);
	psm_tagsel = (~ignore) | ep_priv->domain->reserved_tag_bits;

	fi_context = &ep_priv->nocomp_recv_context;

	err = psm_mq_irecv(ep_priv->domain->psm_mq,
			   psm_tag, psm_tagsel, 0, /* flags */
			   buf, len, (void *)fi_context, &psm_req);

	return psmx_errno(err);
}

static ssize_t psmx_tagged_recv(struct fid_ep *ep, void *buf, size_t len, void *desc,
				fi_addr_t src_addr,
				uint64_t tag, uint64_t ignore, void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	return _psmx_tagged_recv(ep, buf, len, desc, src_addr, tag, ignore,
				 context, ep_priv->flags);
}

static ssize_t psmx_tagged_recvmsg(struct fid_ep *ep, const struct fi_msg_tagged *msg,
				   uint64_t flags)
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

	return _psmx_tagged_recv(ep, buf, len,
				 msg->desc ? msg->desc[0] : NULL,
				 msg->addr, msg->tag, msg->ignore,
				 msg->context, flags);
}

static ssize_t psmx_tagged_recv_no_flag(struct fid_ep *ep, void *buf, 
					size_t len, void *desc, fi_addr_t src_addr,
					uint64_t tag, uint64_t ignore,
					void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	return psmx_tagged_recv_no_flag_av_map(
					ep, buf, len, desc, src_addr,
					tag, ignore, context);
}

static ssize_t psmx_tagged_recv_no_event(struct fid_ep *ep, void *buf, 
					size_t len, void *desc, fi_addr_t src_addr,
					uint64_t tag, uint64_t ignore,
					void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	return psmx_tagged_recv_no_event_av_map(
					ep, buf, len, desc, src_addr,
					tag, ignore, context);
}

static ssize_t psmx_tagged_recvv(struct fid_ep *ep, const struct iovec *iov, void **desc,
				 size_t count, fi_addr_t src_addr,
				 uint64_t tag, uint64_t ignore, void *context)
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

	return psmx_tagged_recv(ep, buf, len,
				desc ? desc[0] : NULL, src_addr, tag, ignore, context);
}

static ssize_t psmx_tagged_recvv_no_flag(struct fid_ep *ep, const struct iovec *iov,
					 void **desc, size_t count, fi_addr_t src_addr,
					 uint64_t tag, uint64_t ignore, void *context)
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

	return psmx_tagged_recv_no_flag(ep, buf, len,
					desc ? desc[0] : NULL, src_addr,
					tag, ignore, context);
}

static ssize_t psmx_tagged_recvv_no_event(struct fid_ep *ep, const struct iovec *iov,
					 void **desc, size_t count, fi_addr_t src_addr,
					 uint64_t tag, uint64_t ignore, void *context)
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

	return psmx_tagged_recv_no_event(ep, buf, len,
					desc ? desc[0] : NULL, src_addr,
					tag, ignore, context);
}

ssize_t _psmx_tagged_send(struct fid_ep *ep, const void *buf, size_t len,
			  void *desc, fi_addr_t dest_addr, uint64_t tag,
			  void *context, uint64_t flags)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	psm_epaddr_t psm_epaddr;
	psm_mq_req_t psm_req;
	uint64_t psm_tag;
	struct fi_context *fi_context;
	int user_fi_context = 0;
	int err;
	size_t idx;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	if (flags & FI_TRIGGER) {
		struct psmx_trigger *trigger;
		struct fi_triggered_context *ctxt = context;

		trigger = calloc(1, sizeof(*trigger));
		if (!trigger)
			return -ENOMEM;

		trigger->op = PSMX_TRIGGERED_TSEND;
		trigger->cntr = container_of(ctxt->threshold.cntr,
					     struct psmx_fid_cntr, cntr);
		trigger->threshold = ctxt->threshold.threshold;
		trigger->tsend.ep = ep;
		trigger->tsend.buf = buf;
		trigger->tsend.len = len;
		trigger->tsend.desc = desc;
		trigger->tsend.dest_addr = dest_addr;
		trigger->tsend.tag = tag;
		trigger->tsend.context = context;
		trigger->tsend.flags = flags & ~FI_TRIGGER;

		psmx_cntr_add_trigger(trigger->cntr, trigger);
		return 0;
	}

	if (tag & ep_priv->domain->reserved_tag_bits) {
		fprintf(stderr, "%s: warning: using reserved tag bits."
			"tag=%lx. reserved_bits=%lx.\n", __func__, tag,
			ep_priv->domain->reserved_tag_bits);
	}

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

	psm_tag = tag & (~ep_priv->domain->reserved_tag_bits);

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

	err = psm_mq_isend(ep_priv->domain->psm_mq, psm_epaddr, 0,
				psm_tag, buf, len, (void*)fi_context, &psm_req);

	if (err != PSM_OK)
		return psmx_errno(err);

	if (user_fi_context)
		PSMX_CTXT_REQ(fi_context) = psm_req;

	return 0;
}

ssize_t psmx_tagged_send_no_flag_av_map(struct fid_ep *ep, const void *buf,
					size_t len, void *desc,
					fi_addr_t dest_addr, uint64_t tag,
				        void *context)
{
	struct psmx_fid_ep *ep_priv;
	psm_epaddr_t psm_epaddr;
	psm_mq_req_t psm_req;
	uint64_t psm_tag;
	struct fi_context *fi_context;
	int err;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	psm_epaddr = (psm_epaddr_t) dest_addr;
	psm_tag = tag & (~ep_priv->domain->reserved_tag_bits);

	fi_context = context;
	PSMX_CTXT_TYPE(fi_context) = PSMX_SEND_CONTEXT;
	PSMX_CTXT_USER(fi_context) = (void *)buf;
	PSMX_CTXT_EP(fi_context) = ep_priv;

	err = psm_mq_isend(ep_priv->domain->psm_mq, psm_epaddr, 0,
			   psm_tag, buf, len, (void*)fi_context, &psm_req);

	if (err != PSM_OK)
		return psmx_errno(err);

	PSMX_CTXT_REQ(fi_context) = psm_req;
	return 0;
}

ssize_t psmx_tagged_send_no_flag_av_table(struct fid_ep *ep, const void *buf,
					  size_t len, void *desc,
					  fi_addr_t dest_addr, uint64_t tag,
					  void *context)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	psm_epaddr_t psm_epaddr;
	psm_mq_req_t psm_req;
	uint64_t psm_tag;
	struct fi_context *fi_context;
	int err;
	size_t idx;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	av = ep_priv->av;
	idx = (size_t)dest_addr;
	if (idx >= av->last)
		return -EINVAL;

	psm_epaddr = av->psm_epaddrs[idx];
	psm_tag = tag & (~ep_priv->domain->reserved_tag_bits);

	fi_context = context;
	PSMX_CTXT_TYPE(fi_context) = PSMX_SEND_CONTEXT;
	PSMX_CTXT_USER(fi_context) = (void *)buf;
	PSMX_CTXT_EP(fi_context) = ep_priv;

	err = psm_mq_isend(ep_priv->domain->psm_mq, psm_epaddr, 0,
			   psm_tag, buf, len, (void*)fi_context, &psm_req);

	if (err != PSM_OK)
		return psmx_errno(err);

	PSMX_CTXT_REQ(fi_context) = psm_req;
	return 0;
}

ssize_t psmx_tagged_send_no_event_av_map(struct fid_ep *ep, const void *buf,
					 size_t len, void *desc,
					 fi_addr_t dest_addr, uint64_t tag,
				         void *context)
{
	struct psmx_fid_ep *ep_priv;
	psm_epaddr_t psm_epaddr;
	psm_mq_req_t psm_req;
	uint64_t psm_tag;
	struct fi_context *fi_context;
	int err;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	psm_epaddr = (psm_epaddr_t) dest_addr;
	psm_tag = tag & (~ep_priv->domain->reserved_tag_bits);

	fi_context = &ep_priv->nocomp_send_context;

	err = psm_mq_isend(ep_priv->domain->psm_mq, psm_epaddr, 0,
			   psm_tag, buf, len, (void*)fi_context, &psm_req);

	if (err != PSM_OK)
		return psmx_errno(err);

	return 0;
}

ssize_t psmx_tagged_send_no_event_av_table(struct fid_ep *ep, const void *buf,
					   size_t len, void *desc,
					   fi_addr_t dest_addr, uint64_t tag,
					   void *context)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	psm_epaddr_t psm_epaddr;
	psm_mq_req_t psm_req;
	uint64_t psm_tag;
	struct fi_context *fi_context;
	int err;
	size_t idx;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	av = ep_priv->av;
	idx = (size_t)dest_addr;
	if (idx >= av->last)
		return -EINVAL;

	psm_epaddr = av->psm_epaddrs[idx];
	psm_tag = tag & (~ep_priv->domain->reserved_tag_bits);

	fi_context = &ep_priv->nocomp_send_context;

	err = psm_mq_isend(ep_priv->domain->psm_mq, psm_epaddr, 0,
			   psm_tag, buf, len, (void*)fi_context, &psm_req);

	if (err != PSM_OK)
		return psmx_errno(err);

	return 0;
}

ssize_t psmx_tagged_inject_no_flag_av_map(struct fid_ep *ep, const void *buf, size_t len,
					  fi_addr_t dest_addr, uint64_t tag)
{
	struct psmx_fid_ep *ep_priv;
	psm_epaddr_t psm_epaddr;
	psm_mq_req_t psm_req;
	uint64_t psm_tag;
	struct fi_context *fi_context;
	int err;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	psm_epaddr = (psm_epaddr_t) dest_addr;
	psm_tag = tag & (~ep_priv->domain->reserved_tag_bits);

	fi_context = malloc(sizeof(*fi_context) + len);
	if (!fi_context)
		return -ENOMEM;

	memcpy((void *)fi_context + sizeof(*fi_context), buf, len);
	buf = (void *)fi_context + sizeof(*fi_context);

	PSMX_CTXT_TYPE(fi_context) = PSMX_INJECT_CONTEXT;
	PSMX_CTXT_EP(fi_context) = ep_priv;

	err = psm_mq_isend(ep_priv->domain->psm_mq, psm_epaddr, 0,
			   psm_tag, buf, len, (void*)fi_context, &psm_req);

	return psmx_errno(err);
}

ssize_t psmx_tagged_inject_no_flag_av_table(struct fid_ep *ep, const void *buf, size_t len,
					    fi_addr_t dest_addr, uint64_t tag)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	psm_epaddr_t psm_epaddr;
	psm_mq_req_t psm_req;
	uint64_t psm_tag;
	struct fi_context *fi_context;
	int err;
	size_t idx;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	av = ep_priv->av;
	idx = (size_t)dest_addr;
	if (idx >= av->last)
		return -EINVAL;

	psm_epaddr = av->psm_epaddrs[idx];
	psm_tag = tag & (~ep_priv->domain->reserved_tag_bits);

	fi_context = malloc(sizeof(*fi_context) + len);
	if (!fi_context)
		return -ENOMEM;

	memcpy((void *)fi_context + sizeof(*fi_context), buf, len);
	buf = (void *)fi_context + sizeof(*fi_context);

	PSMX_CTXT_TYPE(fi_context) = PSMX_INJECT_CONTEXT;
	PSMX_CTXT_EP(fi_context) = ep_priv;

	err = psm_mq_isend(ep_priv->domain->psm_mq, psm_epaddr, 0,
			   psm_tag, buf, len, (void*)fi_context, &psm_req);

	return psmx_errno(err);
}

static ssize_t psmx_tagged_send(struct fid_ep *ep, const void *buf, size_t len,
				void *desc, fi_addr_t dest_addr,
				uint64_t tag, void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	return _psmx_tagged_send(ep, buf, len, desc, dest_addr, tag, context,
				 ep_priv->flags);
}

static ssize_t psmx_tagged_sendmsg(struct fid_ep *ep, const struct fi_msg_tagged *msg,
				   uint64_t flags)
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

	return _psmx_tagged_send(ep, buf, len,
				   msg->desc ? msg->desc[0] : NULL, msg->addr,
				   msg->tag, msg->context, flags);
}

static ssize_t psmx_tagged_sendv(struct fid_ep *ep, const struct iovec *iov, void **desc,
				 size_t count, fi_addr_t dest_addr, uint64_t tag, void *context)
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

	return psmx_tagged_send(ep, buf, len,
				desc ? desc[0] : NULL, dest_addr, tag, context);
}

static ssize_t psmx_tagged_sendv_no_flag_av_map(struct fid_ep *ep, const struct iovec *iov,
						void **desc, size_t count,
						fi_addr_t dest_addr,
						uint64_t tag, void *context)
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

	return psmx_tagged_send_no_flag_av_map(ep, buf, len,
					       desc ? desc[0] : NULL, dest_addr,
					       tag, context);
}

static ssize_t psmx_tagged_sendv_no_flag_av_table(struct fid_ep *ep, const struct iovec *iov,
						  void **desc, size_t count,
						  fi_addr_t dest_addr,
						  uint64_t tag, void *context)
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

	return psmx_tagged_send_no_flag_av_table(ep, buf, len,
					         desc ? desc[0] : NULL, dest_addr,
					         tag, context);
}

static ssize_t psmx_tagged_sendv_no_event_av_map(struct fid_ep *ep, const struct iovec *iov,
						void **desc, size_t count,
						fi_addr_t dest_addr,
						uint64_t tag, void *context)
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

	return psmx_tagged_send_no_event_av_map(ep, buf, len,
					       desc ? desc[0] : NULL, dest_addr,
					       tag, context);
}

static ssize_t psmx_tagged_sendv_no_event_av_table(struct fid_ep *ep, const struct iovec *iov,
						  void **desc, size_t count,
						  fi_addr_t dest_addr,
						  uint64_t tag, void *context)
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

	return psmx_tagged_send_no_event_av_table(ep, buf, len,
					         desc ? desc[0] : NULL,
					         dest_addr, tag, context);
}

static ssize_t psmx_tagged_inject(struct fid_ep *ep, const void *buf, size_t len,
				  fi_addr_t dest_addr, uint64_t tag)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	return _psmx_tagged_send(ep, buf, len, NULL, dest_addr, tag, NULL,
				 ep_priv->flags | FI_INJECT);
}

static ssize_t psmx_tagged_search(struct fid_ep *ep, uint64_t *tag, uint64_t ignore,
				  uint64_t flags, fi_addr_t *src_addr, size_t *len,
				  void *context)
{
	struct psmx_fid_ep *ep_priv;
	psm_mq_status_t psm_status;
	uint64_t psm_tag, psm_tagsel;
	int err;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	if ((*tag) & ep_priv->domain->reserved_tag_bits) {
		fprintf(stderr, "%s: warning: using reserved tag bits."
			"tag=%lx. reserved_bits=%lx.\n", __func__, *tag,
			ep_priv->domain->reserved_tag_bits);
	}

	psm_tag = *tag & (~ep_priv->domain->reserved_tag_bits);
	psm_tagsel = (~ignore) | ep_priv->domain->reserved_tag_bits;

	if (flags & FI_CLAIM)
		return -FI_EOPNOTSUPP;

	err = psm_mq_iprobe(ep_priv->domain->psm_mq, psm_tag, psm_tagsel,
			    &psm_status);
	switch (err) {
	case PSM_OK:
		*tag = psm_status.msg_tag;
		*len = psm_status.msg_length;
		*src_addr = FI_ADDR_NOTAVAIL;
		return 1;

	case PSM_MQ_NO_COMPLETIONS:
		return -FI_ENOMSG;

	default:
		return psmx_errno(err);
	}
}

/* general case */
struct fi_ops_tagged psmx_tagged_ops = {
	.size = sizeof(struct fi_ops_tagged),
	.recv = psmx_tagged_recv,
	.recvv = psmx_tagged_recvv,
	.recvmsg = psmx_tagged_recvmsg,
	.send = psmx_tagged_send,
	.sendv = psmx_tagged_sendv,
	.sendmsg = psmx_tagged_sendmsg,
	.inject = psmx_tagged_inject,
	.senddata = fi_no_tagged_senddata,
	.injectdata = fi_no_tagged_injectdata,
	.search = psmx_tagged_search,
};

/* op_flags=0, no event suppression, FI_AV_MAP */
struct fi_ops_tagged psmx_tagged_ops_no_flag_av_map = {
	.size = sizeof(struct fi_ops_tagged),
	.recv = psmx_tagged_recv_no_flag,
	.recvv = psmx_tagged_recvv_no_flag,
	.recvmsg = psmx_tagged_recvmsg,
	.send = psmx_tagged_send_no_flag_av_map,
	.sendv = psmx_tagged_sendv_no_flag_av_map,
	.sendmsg = psmx_tagged_sendmsg,
	.inject = psmx_tagged_inject_no_flag_av_map,
	.senddata = fi_no_tagged_senddata,
	.injectdata = fi_no_tagged_injectdata,
	.search = psmx_tagged_search,
};

/* op_flags=0, no event suppression, FI_AV_TABLE */
struct fi_ops_tagged psmx_tagged_ops_no_flag_av_table = {
	.size = sizeof(struct fi_ops_tagged),
	.recv = psmx_tagged_recv_no_flag,
	.recvv = psmx_tagged_recvv_no_flag,
	.recvmsg = psmx_tagged_recvmsg,
	.send = psmx_tagged_send_no_flag_av_table,
	.sendv = psmx_tagged_sendv_no_flag_av_table,
	.sendmsg = psmx_tagged_sendmsg,
	.inject = psmx_tagged_inject_no_flag_av_table,
	.senddata = fi_no_tagged_senddata,
	.injectdata = fi_no_tagged_injectdata,
	.search = psmx_tagged_search,
};

/* op_flags=0, event suppression, FI_AV_MAP */
struct fi_ops_tagged psmx_tagged_ops_no_event_av_map = {
	.size = sizeof(struct fi_ops_tagged),
	.recv = psmx_tagged_recv_no_event,
	.recvv = psmx_tagged_recvv_no_event,
	.recvmsg = psmx_tagged_recvmsg,
	.send = psmx_tagged_send_no_event_av_map,
	.sendv = psmx_tagged_sendv_no_event_av_map,
	.sendmsg = psmx_tagged_sendmsg,
	.inject = psmx_tagged_inject_no_flag_av_map,
	.senddata = fi_no_tagged_senddata,
	.injectdata = fi_no_tagged_injectdata,
	.search = psmx_tagged_search,
};

/* op_flags=0, event suppression, FI_AV_TABLE */
struct fi_ops_tagged psmx_tagged_ops_no_event_av_table = {
	.size = sizeof(struct fi_ops_tagged),
	.recv = psmx_tagged_recv_no_event,
	.recvv = psmx_tagged_recvv_no_event,
	.recvmsg = psmx_tagged_recvmsg,
	.send = psmx_tagged_send_no_event_av_table,
	.sendv = psmx_tagged_sendv_no_event_av_table,
	.sendmsg = psmx_tagged_sendmsg,
	.inject = psmx_tagged_inject_no_flag_av_table,
	.senddata = fi_no_tagged_senddata,
	.injectdata = fi_no_tagged_injectdata,
	.search = psmx_tagged_search,
};

/* op_flags=0, send event suppression, FI_AV_MAP */
struct fi_ops_tagged psmx_tagged_ops_no_send_event_av_map = {
	.size = sizeof(struct fi_ops_tagged),
	.recv = psmx_tagged_recv_no_flag,
	.recvv = psmx_tagged_recvv_no_flag,
	.recvmsg = psmx_tagged_recvmsg,
	.send = psmx_tagged_send_no_event_av_map,
	.sendv = psmx_tagged_sendv_no_event_av_map,
	.sendmsg = psmx_tagged_sendmsg,
	.inject = psmx_tagged_inject_no_flag_av_map,
	.senddata = fi_no_tagged_senddata,
	.injectdata = fi_no_tagged_injectdata,
	.search = psmx_tagged_search,
};

/* op_flags=0, send event suppression, FI_AV_TABLE */
struct fi_ops_tagged psmx_tagged_ops_no_send_event_av_table = {
	.size = sizeof(struct fi_ops_tagged),
	.recv = psmx_tagged_recv_no_flag,
	.recvv = psmx_tagged_recvv_no_flag,
	.recvmsg = psmx_tagged_recvmsg,
	.send = psmx_tagged_send_no_event_av_table,
	.sendv = psmx_tagged_sendv_no_event_av_table,
	.sendmsg = psmx_tagged_sendmsg,
	.inject = psmx_tagged_inject_no_flag_av_table,
	.senddata = fi_no_tagged_senddata,
	.injectdata = fi_no_tagged_injectdata,
	.search = psmx_tagged_search,
};

/* op_flags=0, recv event suppression, FI_AV_MAP */
struct fi_ops_tagged psmx_tagged_ops_no_recv_event_av_map = {
	.size = sizeof(struct fi_ops_tagged),
	.recv = psmx_tagged_recv_no_event,
	.recvv = psmx_tagged_recvv_no_event,
	.recvmsg = psmx_tagged_recvmsg,
	.send = psmx_tagged_send_no_flag_av_map,
	.sendv = psmx_tagged_sendv_no_flag_av_map,
	.sendmsg = psmx_tagged_sendmsg,
	.inject = psmx_tagged_inject_no_flag_av_map,
	.senddata = fi_no_tagged_senddata,
	.injectdata = fi_no_tagged_injectdata,
	.search = psmx_tagged_search,
};

/* op_flags=0, recv event suppression, FI_AV_TABLE */
struct fi_ops_tagged psmx_tagged_ops_no_recv_event_av_table = {
	.size = sizeof(struct fi_ops_tagged),
	.recv = psmx_tagged_recv_no_event,
	.recvv = psmx_tagged_recvv_no_event,
	.recvmsg = psmx_tagged_recvmsg,
	.send = psmx_tagged_send_no_flag_av_table,
	.sendv = psmx_tagged_sendv_no_flag_av_table,
	.sendmsg = psmx_tagged_sendmsg,
	.inject = psmx_tagged_inject_no_flag_av_table,
	.senddata = fi_no_tagged_senddata,
	.injectdata = fi_no_tagged_injectdata,
	.search = psmx_tagged_search,
};

