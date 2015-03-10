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

static void psmx_ep_optimize_ops(struct psmx_fid_ep *ep)
{
	if (ep->ep.tagged) {
		if (ep->flags) {
			ep->ep.tagged = &psmx_tagged_ops;
			PSMX_DEBUG("generic tagged ops.\n");
		}
		else if (ep->send_cq_event_flag && ep->recv_cq_event_flag) {
			if (ep->av && ep->av->type == FI_AV_TABLE)
				ep->ep.tagged = &psmx_tagged_ops_no_event_av_table;
			else
				ep->ep.tagged = &psmx_tagged_ops_no_event_av_map;
			PSMX_DEBUG("tagged ops optimized for op_flags=0 and event suppression\n");
		}
		else if (ep->send_cq_event_flag) {
			if (ep->av && ep->av->type == FI_AV_TABLE)
				ep->ep.tagged = &psmx_tagged_ops_no_send_event_av_table;
			else
				ep->ep.tagged = &psmx_tagged_ops_no_send_event_av_map;
			PSMX_DEBUG("tagged ops optimized for op_flags=0 and send event suppression\n");
		}
		else if (ep->recv_cq_event_flag) {
			if (ep->av && ep->av->type == FI_AV_TABLE)
				ep->ep.tagged = &psmx_tagged_ops_no_recv_event_av_table;
			else
				ep->ep.tagged = &psmx_tagged_ops_no_recv_event_av_map;
			PSMX_DEBUG("tagged ops optimized for op_flags=0 and recv event suppression\n");
		}
		else {
			if (ep->av && ep->av->type == FI_AV_TABLE)
				ep->ep.tagged = &psmx_tagged_ops_no_flag_av_table;
			else
				ep->ep.tagged = &psmx_tagged_ops_no_flag_av_map;
			PSMX_DEBUG("tagged ops optimized for op_flags=0\n");
		}
	}
}

static ssize_t psmx_ep_cancel(fid_t fid, void *context)
{
	struct psmx_fid_ep *ep;
	psm_mq_status_t status;
	struct fi_context *fi_context = context;
	int err;

	ep = container_of(fid, struct psmx_fid_ep, ep.fid);
	if (!ep->domain)
		return -FI_EBADF;

	if (!fi_context)
		return -FI_EINVAL;

	err = psm_mq_cancel((psm_mq_req_t *)&PSMX_CTXT_REQ(fi_context));
	if (err == PSM_OK)
		err = psm_mq_test((psm_mq_req_t *)&PSMX_CTXT_REQ(fi_context), &status);

	return psmx_errno(err);
}

static int psmx_ep_getopt(fid_t fid, int level, int optname,
			void *optval, size_t *optlen)
{
	struct psmx_fid_ep *ep;

	ep = container_of(fid, struct psmx_fid_ep, ep.fid);

	if (level != FI_OPT_ENDPOINT)
		return -FI_ENOPROTOOPT;

	switch (optname) {
	case FI_OPT_MIN_MULTI_RECV:
		*(size_t *)optval = ep->min_multi_recv;
		*optlen = sizeof(size_t);
		break;

	default:
		return -FI_ENOPROTOOPT;
	}

	return 0;
}

static int psmx_ep_setopt(fid_t fid, int level, int optname,
			const void *optval, size_t optlen)
{
	struct psmx_fid_ep *ep;

	ep = container_of(fid, struct psmx_fid_ep, ep.fid);

	if (level != FI_OPT_ENDPOINT)
		return -FI_ENOPROTOOPT;

	switch (optname) {
	case FI_OPT_MIN_MULTI_RECV:
		ep->min_multi_recv = *(size_t *)optval;
		break;

	default:
		return -FI_ENOPROTOOPT;
	}

	return 0;
}

static int psmx_ep_close(fid_t fid)
{
	struct psmx_fid_ep *ep;

	ep = container_of(fid, struct psmx_fid_ep, ep.fid);

	psmx_domain_disable_ep(ep->domain, ep);

	free(ep);

	return 0;
}

static int psmx_ep_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	struct psmx_fid_ep *ep;
	struct psmx_fid_av *av;
	struct psmx_fid_cq *cq;
	struct psmx_fid_cntr *cntr;
	struct psmx_fid_stx *stx;
	int err;

	ep = container_of(fid, struct psmx_fid_ep, ep.fid);

	if (!bfid)
		return -FI_EINVAL;
	switch (bfid->fclass) {
	case FI_CLASS_EQ:
		return -FI_ENOSYS;

	case FI_CLASS_CQ:
		cq = container_of(bfid, struct psmx_fid_cq, cq.fid);
		if (ep->domain != cq->domain)
			return -FI_EINVAL;
		if (flags & FI_SEND) {
			ep->send_cq = cq;
			if (flags & FI_COMPLETION)
				ep->send_cq_event_flag = 1;
		}
		if (flags & FI_RECV) {
			ep->recv_cq = cq;
			if (flags & FI_COMPLETION)
				ep->recv_cq_event_flag = 1;
		}
		psmx_ep_optimize_ops(ep);
		break;

	case FI_CLASS_CNTR:
		cntr = container_of(bfid, struct psmx_fid_cntr, cntr.fid);
		if (ep->domain != cntr->domain)
			return -FI_EINVAL;
		if (flags & FI_SEND)
			ep->send_cntr = cntr;
		if (flags & FI_RECV)
			ep->recv_cntr = cntr;
		if (flags & FI_WRITE)
			ep->write_cntr = cntr;
		if (flags & FI_READ)
			ep->read_cntr = cntr;
		if (flags & FI_REMOTE_WRITE)
			ep->remote_write_cntr = cntr;
		if (flags & FI_REMOTE_READ)
			ep->remote_read_cntr = cntr;
		break;

	case FI_CLASS_AV:
		av = container_of(bfid,
				struct psmx_fid_av, av.fid);
		if (ep->domain != av->domain)
			return -FI_EINVAL;
		ep->av = av;
		psmx_ep_optimize_ops(ep);
		break;

	case FI_CLASS_MR:
		if (!bfid->ops || !bfid->ops->bind)
			return -FI_EINVAL;
		err = bfid->ops->bind(bfid, fid, flags);
		if (err)
			return err;
		break;

	case FI_CLASS_STX_CTX:
		stx = container_of(bfid,
				   struct psmx_fid_stx, stx.fid);
		if (ep->domain != stx->domain)
			return -FI_EINVAL;
		break;

	default:
		return -FI_ENOSYS;
	}

	return 0;
}

static int psmx_ep_control(fid_t fid, int command, void *arg)
{
	struct fi_alias *alias;
	struct psmx_fid_ep *ep, *new_ep;
	ep = container_of(fid, struct psmx_fid_ep, ep.fid);

	switch (command) {
	case FI_ALIAS:
		new_ep = (struct psmx_fid_ep *) calloc(1, sizeof *ep);
		if (!new_ep)
			return -FI_ENOMEM;
		alias = arg;
		*new_ep = *ep;
		new_ep->flags = alias->flags;
		psmx_ep_optimize_ops(new_ep);
		*alias->fid = &new_ep->ep.fid;
		break;

	case FI_SETFIDFLAG:
		ep->flags = *(uint64_t *)arg;
		psmx_ep_optimize_ops(ep);
		break;

	case FI_GETFIDFLAG:
		if (!arg)
			return -FI_EINVAL;
		*(uint64_t *)arg = ep->flags;
		break;

	case FI_ENABLE:
		return 0;

	default:
		return -FI_ENOSYS;
	}

	return 0;
}

static struct fi_ops psmx_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = psmx_ep_close,
	.bind = psmx_ep_bind,
	.control = psmx_ep_control,
};

static struct fi_ops_ep psmx_ep_ops = {
	.size = sizeof(struct fi_ops_ep),
	.cancel = psmx_ep_cancel,
	.getopt = psmx_ep_getopt,
	.setopt = psmx_ep_setopt,
	.tx_ctx = fi_no_tx_ctx,
	.rx_ctx = fi_no_rx_ctx,
	.rx_size_left = fi_no_rx_size_left,
	.tx_size_left = fi_no_tx_size_left,
};

int psmx_ep_open(struct fid_domain *domain, struct fi_info *info,
		struct fid_ep **ep, void *context)
{
	struct psmx_fid_domain *domain_priv;
	struct psmx_fid_ep *ep_priv;
	int err;
	uint64_t ep_cap;

	if (info)
		ep_cap = info->caps;
	else
		ep_cap = FI_TAGGED;

	domain_priv = container_of(domain, struct psmx_fid_domain, domain.fid);
	if (!domain_priv)
		return -FI_EINVAL;

	err = psmx_domain_check_features(domain_priv, ep_cap);
	if (err)
		return err; 

	ep_priv = (struct psmx_fid_ep *) calloc(1, sizeof *ep_priv);
	if (!ep_priv)
		return -FI_ENOMEM;

	ep_priv->ep.fid.fclass = FI_CLASS_EP;
	ep_priv->ep.fid.context = context;
	ep_priv->ep.fid.ops = &psmx_fi_ops;
	ep_priv->ep.ops = &psmx_ep_ops;
	ep_priv->ep.cm = &psmx_cm_ops;
	ep_priv->domain = domain_priv;

	PSMX_CTXT_TYPE(&ep_priv->nocomp_send_context) = PSMX_NOCOMP_SEND_CONTEXT;
	PSMX_CTXT_EP(&ep_priv->nocomp_send_context) = ep_priv;
	PSMX_CTXT_TYPE(&ep_priv->nocomp_recv_context) = PSMX_NOCOMP_RECV_CONTEXT;
	PSMX_CTXT_EP(&ep_priv->nocomp_recv_context) = ep_priv;
	PSMX_CTXT_TYPE(&ep_priv->sendimm_context) = PSMX_INJECT_CONTEXT;
	PSMX_CTXT_EP(&ep_priv->sendimm_context) = ep_priv;
	PSMX_CTXT_TYPE(&ep_priv->writeimm_context) = PSMX_INJECT_WRITE_CONTEXT;
	PSMX_CTXT_EP(&ep_priv->writeimm_context) = ep_priv;

	if (ep_cap & FI_TAGGED)
		ep_priv->ep.tagged = &psmx_tagged_ops;
	if (ep_cap & FI_MSG)
		ep_priv->ep.msg = &psmx_msg_ops;
	if ((ep_cap & FI_MSG) && psmx_env.am_msg)
		ep_priv->ep.msg = &psmx_msg2_ops;
	if (ep_cap & FI_RMA)
		ep_priv->ep.rma = &psmx_rma_ops;
	if (ep_cap & FI_ATOMICS)
		ep_priv->ep.atomic = &psmx_atomic_ops;

	ep_priv->caps = ep_cap;

	err = psmx_domain_enable_ep(domain_priv, ep_priv);
	if (err) {
		free(ep_priv);
		return err;
	}

	if (info) {
		if (info->tx_attr)
			ep_priv->flags = info->tx_attr->op_flags;
		if (info->rx_attr)
			ep_priv->flags |= info->rx_attr->op_flags;
	}

	psmx_ep_optimize_ops(ep_priv);

	*ep = &ep_priv->ep;

	return 0;
}

/* STX support is essentially no-op since PSM supports only one send/recv
 * context and thus always works in shared context mode.
 */

static int psmx_stx_close(fid_t fid)
{
	struct psmx_fid_stx *stx;

	stx = container_of(fid, struct psmx_fid_stx, stx.fid);
	free(stx);

	return 0;
}

static struct fi_ops psmx_fi_ops_stx = {
	.size = sizeof(struct fi_ops),
	.close = psmx_stx_close,
	.bind = fi_no_bind,
	.control = fi_no_control,
};

int psmx_stx_ctx(struct fid_domain *domain, struct fi_tx_attr *attr,
		 struct fid_stx **stx, void *context)
{
	struct psmx_fid_stx *stx_priv;

	PSMX_DEBUG("\n");

	stx_priv = (struct psmx_fid_stx *) calloc(1, sizeof *stx_priv);
	if (!stx_priv)
		return -FI_ENOMEM;

	stx_priv->stx.fid.fclass = FI_CLASS_STX_CTX;
	stx_priv->stx.fid.context = context;
	stx_priv->stx.fid.ops = &psmx_fi_ops_stx;
	stx_priv->domain = container_of(domain, struct psmx_fid_domain, domain.fid);

	*stx = &stx_priv->stx;
	return 0;
}

