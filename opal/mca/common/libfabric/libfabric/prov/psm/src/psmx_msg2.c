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

/* Message protocol:
 *
 * Send REQ:
 *	args[0].u32w0	cmd, flag
 *	args[0].u32w1	len
 *	args[1].u64	req
 *	args[2].u64	recv_req
 *	args[3].u64	offset
 *
 * Send REP:
 *	args[0].u32w0	cmd
 *	args[0].u32w1	error
 *	args[1].u64	req
 *	args[2].u64	recv_req
 */

struct psmx_unexp {
	psm_epaddr_t	sender_addr;
	uint64_t	sender_context;
	uint32_t	len_received;
	uint32_t	done;
	struct psmx_unexp *next;
	char		buf[0];
};

static struct psmx_unexp *psmx_unexp_head = NULL;
static struct psmx_unexp *psmx_unexp_tail = NULL;
static pthread_mutex_t psmx_unexp_lock = PTHREAD_MUTEX_INITIALIZER;

static void psmx_unexp_enqueue(struct psmx_unexp *unexp)
{
	pthread_mutex_lock(&psmx_unexp_lock);

	if (!psmx_unexp_head)
		psmx_unexp_head = psmx_unexp_tail = unexp;
	else
		psmx_unexp_tail->next = unexp;

	pthread_mutex_unlock(&psmx_unexp_lock);
}

static struct psmx_unexp *psmx_unexp_dequeue(fi_addr_t src_addr)
{
	struct psmx_unexp *unexp = NULL;
	struct psmx_unexp *prev;

	pthread_mutex_lock(&psmx_unexp_lock);

	if (psmx_unexp_head) {
		prev = NULL;
		unexp = psmx_unexp_head;
		while (unexp) {
			if (!src_addr || src_addr == (fi_addr_t)unexp->sender_addr) {
				if (prev)
					prev->next = unexp->next;
				else
					psmx_unexp_head = unexp->next;

				if (psmx_unexp_tail == unexp)
					psmx_unexp_tail = prev;

				unexp->next = NULL;
				break;
			}
			prev = unexp;
			unexp = unexp->next;
		}
	}

	pthread_mutex_unlock(&psmx_unexp_lock);

	return unexp;
}

int psmx_am_msg_handler(psm_am_token_t token, psm_epaddr_t epaddr,
			psm_amarg_t *args, int nargs, void *src, uint32_t len)
{
        psm_amarg_t rep_args[8];
        struct psmx_am_request *req;
        struct psmx_cq_event *event;
	struct psmx_epaddr_context *epaddr_context;
	struct psmx_fid_domain *domain;
	int msg_len;
	int copy_len;
	uint64_t offset;
	int cmd, eom;
	int err = 0;
	int op_error = 0;
	struct psmx_unexp *unexp;

	epaddr_context = psm_epaddr_getctxt(epaddr);
	if (!epaddr_context) {
		fprintf(stderr, "%s: NULL context for epaddr %p\n", __func__, epaddr);
		return -EIO;
	}

	domain = epaddr_context->domain;

	cmd = args[0].u32w0 & PSMX_AM_OP_MASK;
	eom = args[0].u32w0 & PSMX_AM_EOM;

	switch (cmd) {
	case PSMX_AM_REQ_SEND:
		msg_len = args[0].u32w1;
                offset = args[3].u64;
                assert(len == msg_len);
		if (offset == 0) {
			/* this is the first packet */
			req = psmx_am_search_and_dequeue_recv(domain, (const void *)epaddr);
			if (req) {
				copy_len = MIN(len, req->recv.len);
				memcpy(req->recv.buf, src, len);
				req->recv.len_received += copy_len;
			}
			else {
				unexp = malloc(sizeof(*unexp) + len);
				if (!unexp) {
					op_error = -ENOBUFS;
				}
				else  {
					memcpy(unexp->buf, src, len);
					unexp->sender_addr = epaddr;
					unexp->sender_context = args[1].u64;
					unexp->len_received = len;
					unexp->done = !!eom;
					unexp->next = NULL;
					psmx_unexp_enqueue(unexp);

					if (!eom) {
						/* stop here. will reply when recv is posted */
						break;
					}
				}
			}

			if (!op_error && !eom) {
				/* reply w/ recv req to be used for following packets */
				rep_args[0].u32w0 = PSMX_AM_REP_SEND;
				rep_args[0].u32w1 = 0;
				rep_args[1].u64 = args[1].u64;
				rep_args[2].u64 = (uint64_t)(uintptr_t)req;
				err = psm_am_reply_short(token, PSMX_AM_MSG_HANDLER,
						rep_args, 3, NULL, 0, 0,
						NULL, NULL );
			}
		}
		else {
			req = (struct psmx_am_request *)(uintptr_t)args[2].u64;
			if (req) {
				copy_len = MIN(req->recv.len + offset, len);
				memcpy(req->recv.buf + offset, src, copy_len);
				req->recv.len_received += copy_len;
			}
			else {
				fprintf(stderr, "%s: NULL recv_req in follow-up packets.\n", __func__);
				op_error = -EBADMSG;
			}
		}

		if (eom && req) {
			if (req->ep->recv_cq && !req->no_event) {
				event = psmx_cq_create_event(
						req->ep->recv_cq,
						req->recv.context,
						req->recv.buf,
						0, /* flags */
						req->recv.len_received,
						0, /* data */
						0, /* tag */
						req->recv.len - req->recv.len_received,
						0 /* err */);
				if (event)
					psmx_cq_enqueue_event(req->ep->recv_cq, event);
				else
					err = -ENOMEM;
			}

			if (req->ep->recv_cntr)
				psmx_cntr_inc(req->ep->recv_cntr);

			free(req);
		}

		if (eom || op_error) {
			rep_args[0].u32w0 = PSMX_AM_REP_SEND;
			rep_args[0].u32w1 = op_error;
			rep_args[1].u64 = args[1].u64;
			rep_args[2].u64 = 0; /* done */
			err = psm_am_reply_short(token, PSMX_AM_MSG_HANDLER,
					rep_args, 3, NULL, 0, 0,
					NULL, NULL );
		}
                break;

	case PSMX_AM_REP_SEND:
		req = (struct psmx_am_request *)(uintptr_t)args[1].u64;
		op_error = (int)args[0].u32w1;
		assert(req->op == PSMX_AM_REQ_SEND);

		if (args[2].u64) { /* more to send */
			req->send.peer_context = (void *)(uintptr_t)args[2].u64;

#if PSMX_AM_USE_SEND_QUEUE
			/* psm_am_request_short() can't be called inside the handler.
			 * put the request into a queue and process it later.
			 */
			psmx_am_enqueue_send(req->ep->domain, req);
			if (req->ep->domain->progress_thread)
				pthread_cond_signal(&req->ep->domain->progress_cond);
#else
			req->send.peer_ready = 1;
#endif
		}
		else { /* done */
			if (req->ep->send_cq && !req->no_event) {
				event = psmx_cq_create_event(
						req->ep->send_cq,
						req->send.context,
						req->send.buf,
						0, /* flags */
						req->send.len,
						0, /* data */
						0, /* tag */
						0, /* olen */
						op_error);
				if (event)
					psmx_cq_enqueue_event(req->ep->send_cq, event);
				else
					err = -ENOMEM;
			}

			if (req->ep->send_cntr)
				psmx_cntr_inc(req->ep->send_cntr);

			if (req->state == PSMX_AM_STATE_QUEUED)
				req->state = PSMX_AM_STATE_DONE;
			else
				free(req);
		}
		break;

	default:
		err = -EINVAL;
	}

	return err;
}

int psmx_am_process_send(struct psmx_fid_domain *domain, struct psmx_am_request *req)
{
	psm_amarg_t args[8];
	int am_flags = PSM_AM_FLAG_ASYNC;
	int chunk_size;
	size_t len;
	uint64_t offset;
	int err;

	req->state = PSMX_AM_STATE_PROCESSED;

	offset = req->send.len_sent;
	len = req->send.len - offset;

	chunk_size = MIN(PSMX_AM_CHUNK_SIZE, psmx_am_param.max_request_short);

	while (len > chunk_size) {
		args[0].u32w0 = PSMX_AM_REQ_SEND;
		args[0].u32w1 = chunk_size;
		args[1].u64 = (uint64_t)(uintptr_t)req;
		args[2].u64 = (uint64_t)(uintptr_t)req->send.peer_context;
		args[3].u64 = offset;

		err = psm_am_request_short((psm_epaddr_t) req->send.dest_addr,
					PSMX_AM_MSG_HANDLER, args, 4,
					req->send.buf+offset, chunk_size,
					am_flags | PSM_AM_FLAG_NOREPLY, NULL, NULL);

		len -= chunk_size;
		offset += chunk_size;
	}

	args[0].u32w0 = PSMX_AM_REQ_SEND | PSMX_AM_EOM;
	args[0].u32w1 = len;
	args[1].u64 = (uint64_t)(uintptr_t)req;
	args[2].u64 = (uint64_t)(uintptr_t)req->send.peer_context;
	args[3].u64 = offset;

	req->send.len_sent = offset + len;
	err = psm_am_request_short((psm_epaddr_t) req->send.dest_addr,
				PSMX_AM_MSG_HANDLER, args, 4,
				(void *)req->send.buf+offset, len,
				am_flags, NULL, NULL);

	free(req);

	return psmx_errno(err);
}

static ssize_t _psmx_recv2(struct fid_ep *ep, void *buf, size_t len,
			   void *desc, fi_addr_t src_addr,
			   void *context, uint64_t flags)
{
	psm_amarg_t args[8];
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	struct psmx_am_request *req;
	struct psmx_unexp *unexp;
	struct psmx_cq_event *event;
	int recv_done;
	int err = 0;
	size_t idx;

        ep_priv = container_of(ep, struct psmx_fid_ep, ep);

        if (src_addr) {
		av = ep_priv->av;
		if (av && av->type == FI_AV_TABLE) {
			idx = (size_t)src_addr;
			if (idx >= av->last)
				return -EINVAL;

			src_addr = (fi_addr_t)av->psm_epaddrs[idx];
		}
	}

	req = calloc(1, sizeof(*req));
	if (!req)
		return -ENOMEM;

	req->op = PSMX_AM_REQ_SEND;
	req->recv.buf = (void *)buf;
	req->recv.len = len;
	req->recv.context = context;
	req->recv.src_addr = (void *)src_addr;
	req->ep = ep_priv;

	if (ep_priv->recv_cq_event_flag && !(flags & FI_EVENT))
		req->no_event = 1;

	unexp = psmx_unexp_dequeue(src_addr);
	if (!unexp) {
		psmx_am_enqueue_recv(ep_priv->domain, req);
		return 0;
	}

	req->recv.len_received = MIN(req->recv.len, unexp->len_received);
	memcpy(req->recv.buf, unexp->buf, req->recv.len_received);

	recv_done = (req->recv.len_received >= req->recv.len);

	if (unexp->done) {
		recv_done = 1;
	}
	else {
		args[0].u32w0 = PSMX_AM_REP_SEND;
		args[0].u32w1 = 0;
		args[1].u64 = unexp->sender_context;
		args[2].u64 = recv_done ? 0 : (uint64_t)(uintptr_t)req;
		err = psm_am_request_short(unexp->sender_addr,
					   PSMX_AM_MSG_HANDLER,
					   args, 3, NULL, 0, 0,
					   NULL, NULL );
	}

	free(unexp);

	if (recv_done) {
		if (req->ep->recv_cq && !req->no_event) {
			event = psmx_cq_create_event(
					req->ep->recv_cq,
					req->recv.context,
					req->recv.buf,
					0, /* flags */
					req->recv.len_received,
					0, /* data */
					0, /* tag */
					req->recv.len - req->recv.len_received,
					0 /* err */);
			if (event)
				psmx_cq_enqueue_event(req->ep->recv_cq, event);
			else
				err = -ENOMEM;
		}

		if (req->ep->recv_cntr)
			psmx_cntr_inc(req->ep->recv_cntr);

		free(req);
	}

	return err;
}

static ssize_t psmx_recv2(struct fid_ep *ep, void *buf, size_t len,
			  void *desc, fi_addr_t src_addr, void *context)
{
	struct psmx_fid_ep *ep_priv;

        ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	return _psmx_recv2(ep, buf, len, desc, src_addr, context, ep_priv->flags);
}

static ssize_t psmx_recvmsg2(struct fid_ep *ep, const struct fi_msg *msg,
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

	return _psmx_recv2(ep, buf, len,
			   msg->desc, msg->addr, msg->context, flags);
}

static ssize_t psmx_recvv2(struct fid_ep *ep, const struct iovec *iov,
			   void **desc, size_t count, fi_addr_t src_addr,
			   void *context)
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

	return psmx_recv2(ep, buf, len, desc ? desc[0] : NULL, src_addr, context);
}

static ssize_t _psmx_send2(struct fid_ep *ep, const void *buf, size_t len,
			   void *desc, fi_addr_t dest_addr,
			   void *context, uint64_t flags)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	struct psmx_am_request *req;
	psm_amarg_t args[8];
	int am_flags = PSM_AM_FLAG_ASYNC;
	int err;
	int chunk_size, msg_size;
	size_t idx;

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

	chunk_size = MIN(PSMX_AM_CHUNK_SIZE, psmx_am_param.max_request_short);
	msg_size = MIN(len, chunk_size);

	req = calloc(1, sizeof(*req));
	if (!req)
		return -ENOMEM;

	req->op = PSMX_AM_REQ_SEND;
	req->send.buf = (void *)buf;
	req->send.len = len;
	req->send.context = context;
	req->send.len_sent = msg_size;
	req->send.dest_addr = (void *)dest_addr;
	req->ep = ep_priv;

	if ((ep_priv->send_cq_event_flag && !(flags & FI_EVENT)) ||
	     (context == &ep_priv->sendimm_context))
		req->no_event = 1;

	args[0].u32w0 = PSMX_AM_REQ_SEND | (msg_size == len ? PSMX_AM_EOM : 0);
	args[0].u32w1 = msg_size;
	args[1].u64 = (uint64_t)(uintptr_t)req;
	args[2].u64 = 0;
	args[3].u64 = 0;

	err = psm_am_request_short((psm_epaddr_t) dest_addr,
				PSMX_AM_MSG_HANDLER, args, 4,
				(void *)buf, msg_size, am_flags, NULL, NULL);

#if ! PSMX_AM_USE_SEND_QUEUE
	if (len > msg_size) {
		while (!req->send.peer_ready)
			psm_poll(ep_priv->domain->psm_ep);

		psmx_am_process_send(ep_priv->domain, req);
	}
#endif

	return psmx_errno(err);

}

static ssize_t psmx_send2(struct fid_ep *ep, const void *buf,
			  size_t len, void *desc,
			  fi_addr_t dest_addr, void *context)
{
	struct psmx_fid_ep *ep_priv;

        ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	return _psmx_send2(ep, buf, len, desc, dest_addr, context, ep_priv->flags);
}

static ssize_t psmx_sendmsg2(struct fid_ep *ep, const struct fi_msg *msg,
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

	return _psmx_send2(ep, buf, len,
			   msg->desc, msg->addr, msg->context, flags);
}

static ssize_t psmx_sendv2(struct fid_ep *ep, const struct iovec *iov,
			   void **desc, size_t count, fi_addr_t dest_addr,
			   void *context)
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

	return psmx_send2(ep, buf, len, desc ? desc[0] : NULL, dest_addr, context);
}

static ssize_t psmx_inject2(struct fid_ep *ep, const void *buf, size_t len,
			    fi_addr_t dest_addr)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	/* TODO: optimize it & guarantee buffered */
	return _psmx_send2(ep, buf, len, NULL, dest_addr, &ep_priv->sendimm_context,
			   ep_priv->flags | FI_INJECT);
}

struct fi_ops_msg psmx_msg2_ops = {
	.size = sizeof(struct fi_ops_msg),
	.recv = psmx_recv2,
	.recvv = psmx_recvv2,
	.recvmsg = psmx_recvmsg2,
	.send = psmx_send2,
	.sendv = psmx_sendv2,
	.sendmsg = psmx_sendmsg2,
	.inject = psmx_inject2,
	.senddata = fi_no_msg_senddata,
	.injectdata = fi_no_msg_injectdata,
};

