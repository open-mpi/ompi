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

/* RMA protocol:
 *
 * Write REQ:
 *	args[0].u32w0	cmd, flag
 *	args[0].u32w1	len
 *	args[1].u64	req
 *	args[2].u64	addr
 *	args[3].u64	key
 *	args[4].u64	data (optional) / tag(long protocol)
 *	args[5].u64	<unused> / data (optional, long protocol)
 *
 * Write REP:
 *	args[0].u32w0	cmd, flag
 *	args[0].u32w1	error
 *	args[1].u64	req
 *
 * Read REQ:
 *	args[0].u32w0	cmd, flag
 *	args[0].u32w1	len
 *	args[1].u64	req
 *	args[2].u64	addr
 *	args[3].u64	key
 *	args[4].u64	offset / tag(long protocol)
 *
 * Read REP:
 *	args[0].u32w0	cmd, flag
 *	args[0].u32w1	error
 *	args[1].u64	req
 *	args[2].u64	offset
 */

int psmx_am_rma_handler(psm_am_token_t token, psm_epaddr_t epaddr,
			psm_amarg_t *args, int nargs, void *src, uint32_t len)
{
	psm_amarg_t rep_args[8];
	void *rma_addr;
	ssize_t rma_len;
	uint64_t key;
	int err = 0;
	int op_error = 0;
	int cmd, eom, has_data;
	struct psmx_am_request *req;
	struct psmx_cq_event *event;
	int chunk_size;
	uint64_t offset;
	struct psmx_fid_mr *mr;

	cmd = args[0].u32w0 & PSMX_AM_OP_MASK;
	eom = args[0].u32w0 & PSMX_AM_EOM;
	has_data = args[0].u32w0 & PSMX_AM_DATA;

	switch (cmd) {
	case PSMX_AM_REQ_WRITE:
		rma_len = args[0].u32w1;
		rma_addr = (void *)(uintptr_t)args[2].u64;
		key = args[3].u64;
		mr = psmx_mr_hash_get(key);
		op_error = mr ?
			psmx_mr_validate(mr, (uint64_t)rma_addr, len, FI_REMOTE_WRITE) :
			-EINVAL;
		if (!op_error) {
			rma_addr += mr->offset;
			memcpy(rma_addr, src, len);
			if (eom) {
				if (mr->cq) {
					/* TODO: report the addr/len of the whole write */
					event = psmx_cq_create_event(
							mr->cq,
							0, /* context */
							rma_addr,
							0, /* flags */
							rma_len,
							has_data ? args[4].u64 : 0,
							0, /* tag */
							0, /* olen */
							0);

					if (event)
						psmx_cq_enqueue_event(mr->cq, event);
					else
						err = -ENOMEM;
				}
				if (mr->cntr)
					psmx_cntr_inc(mr->cntr);
			}
		}
		if (eom || op_error) {
			rep_args[0].u32w0 = PSMX_AM_REP_WRITE | eom;
			rep_args[0].u32w1 = op_error;
			rep_args[1].u64 = args[1].u64;
			err = psm_am_reply_short(token, PSMX_AM_RMA_HANDLER,
					rep_args, 2, NULL, 0, 0,
					NULL, NULL );
		}
		break;

	case PSMX_AM_REQ_WRITE_LONG:
		rma_len = args[0].u32w1;
		rma_addr = (void *)(uintptr_t)args[2].u64;
		key = args[3].u64;
		mr = psmx_mr_hash_get(key);
		op_error = mr ?
			psmx_mr_validate(mr, (uint64_t)rma_addr, len, FI_REMOTE_WRITE) :
			-EINVAL;
		if (op_error) {
			rep_args[0].u32w0 = PSMX_AM_REP_WRITE | eom;
			rep_args[0].u32w1 = op_error;
			rep_args[1].u64 = args[1].u64;
			err = psm_am_reply_short(token, PSMX_AM_RMA_HANDLER,
					rep_args, 2, NULL, 0, 0,
					NULL, NULL );
			break;
		}

		rma_addr += mr->offset;

		req = calloc(1, sizeof(*req));
		if (!req) {
			err = -ENOMEM;
		}
		else {
			req->op = args[0].u32w0;
			req->write.addr = (uint64_t)rma_addr;
			req->write.len = rma_len;
			req->write.key = key;
			req->write.context = (void *)args[4].u64;
			req->write.data = has_data ? args[5].u64 : 0;
			PSMX_CTXT_TYPE(&req->fi_context) = PSMX_REMOTE_WRITE_CONTEXT;
			PSMX_CTXT_USER(&req->fi_context) = mr;
			psmx_am_enqueue_rma(mr->domain, req);
		}
		break;

	case PSMX_AM_REQ_READ:
		rma_len = args[0].u32w1;
		rma_addr = (void *)(uintptr_t)args[2].u64;
		key = args[3].u64;
		offset = args[4].u64;
		mr = psmx_mr_hash_get(key);
		op_error = mr ?
			psmx_mr_validate(mr, (uint64_t)rma_addr, rma_len, FI_REMOTE_READ) :
			-EINVAL;
		if (!op_error) {
			rma_addr += mr->offset;
		}
		else {
			rma_addr = NULL;
			rma_len = 0;
		}

		chunk_size = MIN(PSMX_AM_CHUNK_SIZE, psmx_am_param.max_reply_short);
		assert(rma_len <= chunk_size);
		rep_args[0].u32w0 = PSMX_AM_REP_READ | eom;
		rep_args[0].u32w1 = op_error;
		rep_args[1].u64 = args[1].u64;
		rep_args[2].u64 = offset;
		err = psm_am_reply_short(token, PSMX_AM_RMA_HANDLER,
				rep_args, 3, rma_addr, rma_len, 0,
				NULL, NULL );

		if (eom && !op_error) {
			if (mr->cq) {
				/* TODO: report the addr/len of the whole read */
				event = psmx_cq_create_event(
						mr->cq,
						0, /* context */
						rma_addr,
						0, /* flags */
						rma_len,
						0, /* data */
						0, /* tag */
						0, /* olen */
						0);

				if (event)
					psmx_cq_enqueue_event(mr->cq, event);
				else
					err = -ENOMEM;
			}
			if (mr->cntr)
				psmx_cntr_inc(mr->cntr);
		}
		break;

	case PSMX_AM_REQ_READ_LONG:
		rma_len = args[0].u32w1;
		rma_addr = (void *)(uintptr_t)args[2].u64;
		key = args[3].u64;
		mr = psmx_mr_hash_get(key);
		op_error = mr ?
			psmx_mr_validate(mr, (uint64_t)rma_addr, len, FI_REMOTE_WRITE) :
			-EINVAL;
		if (op_error) {
			rep_args[0].u32w0 = PSMX_AM_REP_READ | eom;
			rep_args[0].u32w1 = op_error;
			rep_args[1].u64 = args[1].u64;
			rep_args[2].u64 = 0;
			err = psm_am_reply_short(token, PSMX_AM_RMA_HANDLER,
					rep_args, 3, NULL, 0, 0,
					NULL, NULL );
			break;
		}

		rma_addr += mr->offset;

		req = calloc(1, sizeof(*req));
		if (!req) {
			err = -ENOMEM;
		}
		else {
			req->op = args[0].u32w0;
			req->read.addr = (uint64_t)rma_addr;
			req->read.len = rma_len;
			req->read.key = key;
			req->read.context = (void *)args[4].u64;
			req->read.peer_addr = (void *)epaddr;
			PSMX_CTXT_TYPE(&req->fi_context) = PSMX_REMOTE_READ_CONTEXT;
			PSMX_CTXT_USER(&req->fi_context) = mr;
			psmx_am_enqueue_rma(mr->domain, req);
		}
		break;

	case PSMX_AM_REP_WRITE:
		req = (struct psmx_am_request *)(uintptr_t)args[1].u64;
		assert(req->op == PSMX_AM_REQ_WRITE);
		op_error = (int)args[0].u32w1;
		if (!req->error)
			req->error = op_error;
		if (eom) {
			if (req->ep->send_cq && !req->no_event) {
				event = psmx_cq_create_event(
						req->ep->send_cq,
						req->write.context,
						req->write.buf,
						0, /* flags */
						req->write.len,
						0, /* data */
						0, /* tag */
						0, /* olen */
						req->error);
				if (event)
					psmx_cq_enqueue_event(req->ep->send_cq, event);
				else
					err = -ENOMEM;
			}

			if (req->ep->write_cntr &&
			    !(req->ep->write_cntr_event_flag && req->no_event))
				psmx_cntr_inc(req->ep->write_cntr);

			req->ep->pending_writes--;
			free(req);
		}
		break;

	case PSMX_AM_REP_READ:
		req = (struct psmx_am_request *)(uintptr_t)args[1].u64;
		assert(req->op == PSMX_AM_REQ_READ);
		op_error = (int)args[0].u32w1;
		offset = args[2].u64;
		if (!req->error)
			req->error = op_error;
		if (!op_error) {
			memcpy(req->read.buf + offset, src, len);
			req->read.len_read += len;
		}
		if (eom) {
			if (req->ep->send_cq && !req->no_event) {
				event = psmx_cq_create_event(
						req->ep->send_cq,
						req->read.context,
						req->read.buf,
						0, /* flags */
						req->read.len_read,
						0, /* data */
						0, /* tag */
						req->read.len - req->read.len_read,
						req->error);
				if (event)
					psmx_cq_enqueue_event(req->ep->send_cq, event);
				else
					err = -ENOMEM;
			}

			if (req->ep->read_cntr &&
			    !(req->ep->read_cntr_event_flag && req->no_event))
				psmx_cntr_inc(req->ep->read_cntr);

			req->ep->pending_reads--;
			free(req);
		}
		break;

	default:
		err = -EINVAL;
	}
	return err;
}

static ssize_t psmx_rma_self(int am_cmd,
			     struct psmx_fid_ep *ep,
			     void *buf, size_t len, void *desc,
			     uint64_t addr, uint64_t key,
			     void *context, uint64_t flags, uint64_t data)
{
	struct psmx_fid_mr *mr;
	struct psmx_cq_event *event;
	int no_event;
	int err = 0;
	int op_error = 0;
	int access;
	void *dst, *src;

	switch (am_cmd) {
	case PSMX_AM_REQ_WRITE:
		ep->pending_writes++;
		access = FI_REMOTE_WRITE;
		break;
	case PSMX_AM_REQ_READ:
		ep->pending_reads++;
		access = FI_REMOTE_READ;
		break;
	default:
		return -EINVAL;
	}

	mr = psmx_mr_hash_get(key);
	op_error = mr ? psmx_mr_validate(mr, addr, len, access) : -EINVAL;

	if (!op_error) {
		addr += mr->offset;
		if (am_cmd == PSMX_AM_REQ_WRITE) {
			dst = (void *)addr;
			src = buf;
		}
		else {
			dst = buf;
			src = (void *)addr;
		}

		memcpy(dst, src, len);

		if (mr->cq) {
			event = psmx_cq_create_event(
					mr->cq,
					0, /* context */
					(void *)addr,
					0, /* flags */
					len,
					flags & FI_REMOTE_CQ_DATA ? data : 0,
					0, /* tag */
					0, /* olen */
					0 /* err */);

			if (event)
				psmx_cq_enqueue_event(mr->cq, event);
			else
				err = -ENOMEM;
		}
		if (mr->cntr)
			psmx_cntr_inc(mr->cntr);
	}

	no_event = (flags & FI_INJECT) ||
		   (ep->send_cq_event_flag && !(flags & FI_EVENT));

	if (ep->send_cq && !no_event) {
		event = psmx_cq_create_event(
				ep->send_cq,
				context,
				(void *)buf,
				0, /* flags */
				len,
				0, /* data */
				0, /* tag */
				0, /* olen */
				op_error);
		if (event)
			psmx_cq_enqueue_event(ep->send_cq, event);
		else
			err = -ENOMEM;
	}

	switch (am_cmd) {
	case PSMX_AM_REQ_WRITE:
		if (ep->write_cntr &&
		    !(ep->write_cntr_event_flag && no_event))
			psmx_cntr_inc(ep->write_cntr);
		ep->pending_writes--;
		break;

	case PSMX_AM_REQ_READ:
		if (ep->read_cntr &&
		    !(ep->read_cntr_event_flag && no_event))
			psmx_cntr_inc(ep->read_cntr);
		ep->pending_reads--;
		break;
	}

	return err;
}

int psmx_am_process_rma(struct psmx_fid_domain *domain, struct psmx_am_request *req)
{
	int err;
	psm_mq_req_t psm_req;

	if ((req->op & PSMX_AM_OP_MASK) == PSMX_AM_REQ_WRITE_LONG) {
		err = psm_mq_irecv(domain->psm_mq, (uint64_t)req->write.context, -1ULL,
				0, (void *)req->write.addr, req->write.len,
				(void *)&req->fi_context, &psm_req);
	}
	else {
		err = psm_mq_isend(domain->psm_mq, (psm_epaddr_t)req->read.peer_addr,
				0, (uint64_t)req->read.context,
				(void *)req->read.addr, req->read.len,
				(void *)&req->fi_context, &psm_req);
	}

	return psmx_errno(err);
}

ssize_t _psmx_readfrom(struct fid_ep *ep, void *buf, size_t len,
		       void *desc, fi_addr_t src_addr,
		       uint64_t addr, uint64_t key, void *context,
		       uint64_t flags)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	struct psmx_epaddr_context *epaddr_context;
	struct psmx_am_request *req;
	psm_amarg_t args[8];
	int err;
	int chunk_size;
	size_t offset = 0;
	uint64_t psm_tag;
	psm_mq_req_t psm_req;
	size_t idx;

	if (flags & FI_TRIGGER) {
		struct psmx_trigger *trigger;
		struct fi_triggered_context *ctxt = context;

		trigger = calloc(1, sizeof(*trigger));
		if (!trigger)
			return -ENOMEM;

		trigger->op = PSMX_TRIGGERED_READ;
		trigger->cntr = container_of(ctxt->threshold.cntr,
					     struct psmx_fid_cntr, cntr);
		trigger->threshold = ctxt->threshold.threshold;
		trigger->read.ep = ep;
		trigger->read.buf = buf;
		trigger->read.len = len;
		trigger->read.desc = desc;
		trigger->read.src_addr = src_addr;
		trigger->read.addr = addr;
		trigger->read.key = key;
		trigger->read.context = context;
		trigger->read.flags = flags & ~FI_TRIGGER;

		psmx_cntr_add_trigger(trigger->cntr, trigger);
		return 0;
	}

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	assert(ep_priv->domain);

	if (!buf)
		return -EINVAL;

	av = ep_priv->av;
	if (av && av->type == FI_AV_TABLE) {
		idx = src_addr;
		if (idx >= av->last)
			return -EINVAL;

		src_addr = (fi_addr_t) av->psm_epaddrs[idx];
	}
	else if (!src_addr) {
		return -EINVAL;
	}

	epaddr_context = psm_epaddr_getctxt((void *)src_addr);
	if (epaddr_context->epid == ep_priv->domain->psm_epid)
		return psmx_rma_self(PSMX_AM_REQ_READ,
				     ep_priv, buf, len, desc,
				     addr, key, context, flags, 0);

	req = calloc(1, sizeof(*req));
	if (!req)
		return -ENOMEM;

	req->op = PSMX_AM_REQ_READ;
	req->read.buf = buf;
	req->read.len = len;
	req->read.addr = addr;	/* needed? */
	req->read.key = key; 	/* needed? */
	req->read.context = context;
	req->ep = ep_priv;
	PSMX_CTXT_TYPE(&req->fi_context) = PSMX_READ_CONTEXT;
	PSMX_CTXT_USER(&req->fi_context) = context;

	if (ep_priv->send_cq_event_flag && !(flags & FI_EVENT)) {
		PSMX_CTXT_TYPE(&req->fi_context) = PSMX_NOCOMP_READ_CONTEXT;
		req->no_event = 1;
	}

	chunk_size = MIN(PSMX_AM_CHUNK_SIZE, psmx_am_param.max_reply_short);

	if (psmx_env.tagged_rma && len > chunk_size) {
		psm_tag = PSMX_RMA_BIT | ep_priv->domain->psm_epid;
		err = psm_mq_irecv(ep_priv->domain->psm_mq, psm_tag, -1ULL,
			0, buf, len, (void *)&req->fi_context, &psm_req);

		args[0].u32w0 = PSMX_AM_REQ_READ_LONG;
		args[0].u32w1 = len;
		args[1].u64 = (uint64_t)req;
		args[2].u64 = addr;
		args[3].u64 = key;
		args[4].u64 = psm_tag;
		err = psm_am_request_short((psm_epaddr_t) src_addr,
					PSMX_AM_RMA_HANDLER, args, 5, NULL, 0,
					PSM_AM_FLAG_NOREPLY, NULL, NULL);

		ep_priv->pending_reads++;
		return 0;
	}

	args[0].u32w0 = PSMX_AM_REQ_READ;
	args[1].u64 = (uint64_t)(uintptr_t)req;
	args[3].u64 = key;
	while (len > chunk_size) {
		args[0].u32w1 = chunk_size;
		args[2].u64 = addr;
		args[4].u64 = offset;
		err = psm_am_request_short((psm_epaddr_t) src_addr,
					PSMX_AM_RMA_HANDLER, args, 5, NULL, 0,
					0, NULL, NULL);
		addr += chunk_size;
		len -= chunk_size;
		offset += chunk_size;
	}

	args[0].u32w0 = PSMX_AM_REQ_READ | PSMX_AM_EOM;
	args[0].u32w1 = len;
	args[2].u64 = addr;
	args[4].u64 = offset;
	err = psm_am_request_short((psm_epaddr_t) src_addr,
				PSMX_AM_RMA_HANDLER, args, 5, NULL, 0,
				0, NULL, NULL);

	ep_priv->pending_reads++;
	return 0;
}

static ssize_t psmx_readfrom(struct fid_ep *ep, void *buf, size_t len,
			 void *desc, fi_addr_t src_addr,
			 uint64_t addr, uint64_t key, void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	return _psmx_readfrom(ep, buf, len, desc, src_addr, addr,
			      key, context, ep_priv->flags);
}

static ssize_t psmx_readmsg(struct fid_ep *ep, const struct fi_msg_rma *msg,
			uint64_t flags)
{
	if (!msg || msg->iov_count != 1)
		return -EINVAL;

	return _psmx_readfrom(ep, msg->msg_iov[0].iov_base,
			      msg->msg_iov[0].iov_len,
			      msg->desc ? msg->desc[0] : NULL, msg->addr,
			      msg->rma_iov[0].addr, msg->rma_iov[0].key,
			      msg->context, flags);
}

static ssize_t psmx_read(struct fid_ep *ep, void *buf, size_t len,
		     void *desc, uint64_t addr, uint64_t key,
		     void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	assert(ep_priv->domain);

	if (!ep_priv->connected)
		return -ENOTCONN;

	return psmx_readfrom(ep, buf, len, desc, (fi_addr_t) ep_priv->peer_psm_epaddr,
			     addr, key, context);
}

static ssize_t psmx_readv(struct fid_ep *ep, const struct iovec *iov,
		      void **desc, size_t count, uint64_t addr,
		      uint64_t key, void *context)
{
	if (!iov || count != 1)
		return -EINVAL;

	return psmx_read(ep, iov->iov_base, iov->iov_len,
			 desc ? desc[0] : NULL, addr, key, context);
}

ssize_t _psmx_writeto(struct fid_ep *ep, const void *buf, size_t len,
		      void *desc, fi_addr_t dest_addr,
		      uint64_t addr, uint64_t key, void *context,
		      uint64_t flags, uint64_t data)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	struct psmx_epaddr_context *epaddr_context;
	struct psmx_am_request *req;
	psm_amarg_t args[8];
	int nargs;
	int am_flags = PSM_AM_FLAG_ASYNC;
	int err;
	int chunk_size;
	psm_mq_req_t psm_req;
	uint64_t psm_tag;
	size_t idx;

	if (flags & FI_TRIGGER) {
		struct psmx_trigger *trigger;
		struct fi_triggered_context *ctxt = context;

		trigger = calloc(1, sizeof(*trigger));
		if (!trigger)
			return -ENOMEM;

		trigger->op = PSMX_TRIGGERED_WRITE;
		trigger->cntr = container_of(ctxt->threshold.cntr,
					     struct psmx_fid_cntr, cntr);
		trigger->threshold = ctxt->threshold.threshold;
		trigger->write.ep = ep;
		trigger->write.buf = buf;
		trigger->write.len = len;
		trigger->write.desc = desc;
		trigger->write.dest_addr = dest_addr;
		trigger->write.addr = addr;
		trigger->write.key = key;
		trigger->write.context = context;
		trigger->write.flags = flags & ~FI_TRIGGER;
		trigger->write.data = data;

		psmx_cntr_add_trigger(trigger->cntr, trigger);
		return 0;
	}

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	assert(ep_priv->domain);

	if (!buf)
		return -EINVAL;

	av = ep_priv->av;
	if (av && av->type == FI_AV_TABLE) {
		idx = dest_addr;
		if (idx >= av->last)
			return -EINVAL;

		dest_addr = (fi_addr_t) av->psm_epaddrs[idx];
	}
	else if (!dest_addr) {
		return -EINVAL;
	}

	epaddr_context = psm_epaddr_getctxt((void *)dest_addr);
	if (epaddr_context->epid == ep_priv->domain->psm_epid)
		return psmx_rma_self(PSMX_AM_REQ_WRITE,
				     ep_priv, (void *)buf, len, desc,
				     addr, key, context, flags, data);

	if (flags & FI_INJECT) {
		req = malloc(sizeof(*req) + len);
		if (!req)
			return -ENOMEM;

		memset((void *)req, 0, sizeof(*req));
		memcpy((void *)req + sizeof(*req), (void *)buf, len);
		buf = (void *)req + sizeof(*req);

		PSMX_CTXT_TYPE(&req->fi_context) = PSMX_INJECT_WRITE_CONTEXT;
		req->no_event = 1;
	}
	else {
		req = calloc(1, sizeof(*req));
		if (!req)
			return -ENOMEM;

		if (ep_priv->send_cq_event_flag && !(flags & FI_EVENT)) {
			PSMX_CTXT_TYPE(&req->fi_context) = PSMX_NOCOMP_WRITE_CONTEXT;
			req->no_event = 1;
		}
		else {
			PSMX_CTXT_TYPE(&req->fi_context) = PSMX_WRITE_CONTEXT;
		}
	}

	req->op = PSMX_AM_REQ_WRITE;
	req->write.buf = (void *)buf;
	req->write.len = len;
	req->write.addr = addr;	/* needed? */
	req->write.key = key; 	/* needed? */
	req->write.context = context;
	req->ep = ep_priv;
	PSMX_CTXT_USER(&req->fi_context) = context;

	chunk_size = MIN(PSMX_AM_CHUNK_SIZE, psmx_am_param.max_request_short);

	if (psmx_env.tagged_rma && len > chunk_size) {
		psm_tag = PSMX_RMA_BIT | ep_priv->domain->psm_epid;
		args[0].u32w0 = PSMX_AM_REQ_WRITE_LONG;
		args[0].u32w1 = len;
		args[1].u64 = (uint64_t)req;
		args[2].u64 = addr;
		args[3].u64 = key;
		args[4].u64 = psm_tag;
		nargs = 5;
		if (flags & FI_REMOTE_CQ_DATA) {
			args[5].u64 = data;
			args[0].u32w0 |= PSMX_AM_DATA;
			nargs++;
		}
		err = psm_am_request_short((psm_epaddr_t) dest_addr,
					PSMX_AM_RMA_HANDLER, args, nargs,
					NULL, 0, am_flags | PSM_AM_FLAG_NOREPLY,
					NULL, NULL);

		psm_mq_isend(ep_priv->domain->psm_mq, (psm_epaddr_t) dest_addr,
				0, psm_tag, buf, len, (void *)&req->fi_context, &psm_req);

		ep_priv->pending_writes++;
		return 0;
	}

	nargs = 4;
	while (len > chunk_size) {
		args[0].u32w0 = PSMX_AM_REQ_WRITE;
		args[0].u32w1 = chunk_size;
		args[1].u64 = (uint64_t)(uintptr_t)req;
		args[2].u64 = addr;
		args[3].u64 = key;
		err = psm_am_request_short((psm_epaddr_t) dest_addr,
					PSMX_AM_RMA_HANDLER, args, nargs,
					(void *)buf, chunk_size,
					am_flags | PSM_AM_FLAG_NOREPLY, NULL, NULL);
		buf += chunk_size;
		addr += chunk_size;
		len -= chunk_size;
	}

	args[0].u32w0 = PSMX_AM_REQ_WRITE | PSMX_AM_EOM;
	args[0].u32w1 = len;
	args[1].u64 = (uint64_t)(uintptr_t)req;
	args[2].u64 = addr;
	args[3].u64 = key;
	if (flags & FI_REMOTE_CQ_DATA) {
		args[4].u64 = data;
		args[0].u32w0 |= PSMX_AM_DATA;
		nargs++;
	}
	err = psm_am_request_short((psm_epaddr_t) dest_addr,
				PSMX_AM_RMA_HANDLER, args, nargs,
				(void *)buf, len, am_flags, NULL, NULL);

	ep_priv->pending_writes++;
	return 0;
}

static ssize_t psmx_writeto(struct fid_ep *ep, const void *buf, size_t len,
			void *desc, fi_addr_t dest_addr, uint64_t addr,
			uint64_t key, void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	return _psmx_writeto(ep, buf, len, desc, dest_addr, addr, key, context,
			     ep_priv->flags, 0);
}

static ssize_t psmx_writemsg(struct fid_ep *ep, const struct fi_msg_rma *msg,
			 uint64_t flags)
{
	if (!msg || msg->iov_count != 1)
		return -EINVAL;

	return _psmx_writeto(ep, msg->msg_iov[0].iov_base,
			     msg->msg_iov[0].iov_len,
			     msg->desc ? msg->desc[0] : NULL, msg->addr,
			     msg->rma_iov[0].addr, msg->rma_iov[0].key,
			     msg->context, flags, msg->data);
}

static ssize_t psmx_write(struct fid_ep *ep, const void *buf, size_t len,
		      void *desc, uint64_t addr, uint64_t key,
		      void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	assert(ep_priv->domain);

	if (!ep_priv->connected)
		return -ENOTCONN;

	return psmx_writeto(ep, buf, len, desc, (fi_addr_t) ep_priv->peer_psm_epaddr,
			    addr, key, context);
}

static ssize_t psmx_writev(struct fid_ep *ep, const struct iovec *iov,
		       void **desc, size_t count, uint64_t addr,
		       uint64_t key, void *context)
{
	if (!iov || count != 1)
		return -EINVAL;

	return psmx_write(ep, iov->iov_base, iov->iov_len,
			  desc ? desc[0] : NULL, addr, key, context);
}

static ssize_t psmx_injectto(struct fid_ep *ep, const void *buf, size_t len,
			fi_addr_t dest_addr, uint64_t addr, uint64_t key)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	return _psmx_writeto(ep, buf, len, NULL, dest_addr, addr, key,
			     NULL, ep_priv->flags | FI_INJECT, 0);
}

static ssize_t psmx_inject(struct fid_ep *ep, const void *buf, size_t len,
			uint64_t addr, uint64_t key)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	assert(ep_priv->domain);

	if (!ep_priv->connected)
		return -ENOTCONN;

	return psmx_injectto(ep, buf, len, (fi_addr_t) ep_priv->peer_psm_epaddr, addr, key);
}

static ssize_t psmx_writedatato(struct fid_ep *ep, const void *buf, size_t len, void *desc,
				uint64_t data, fi_addr_t dest_addr, uint64_t addr,
				uint64_t key, void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	return _psmx_writeto(ep, buf, len, desc, dest_addr, addr, key, context,
			     ep_priv->flags | FI_REMOTE_CQ_DATA, data);
}

static ssize_t psmx_writedata(struct fid_ep *ep, const void *buf, size_t len, void *desc,
			      uint64_t data, uint64_t addr, uint64_t key, void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	assert(ep_priv->domain);

	if (!ep_priv->connected)
		return -ENOTCONN;

	return psmx_writedatato(ep, buf, len, desc, data, (fi_addr_t) ep_priv->peer_psm_epaddr,
				addr, key, context);
}

struct fi_ops_rma psmx_rma_ops = {
	.read = psmx_read,
	.readv = psmx_readv,
	.readfrom = psmx_readfrom,
	.readmsg = psmx_readmsg,
	.write = psmx_write,
	.writev = psmx_writev,
	.writeto = psmx_writeto,
	.writemsg = psmx_writemsg,
	.inject = psmx_inject,
	.injectto = psmx_injectto,
	.writedata = psmx_writedata,
	.writedatato = psmx_writedatato,
};

