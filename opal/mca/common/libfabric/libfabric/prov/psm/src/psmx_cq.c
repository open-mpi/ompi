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

void psmx_cq_enqueue_event(struct psmx_fid_cq *cq, struct psmx_cq_event *event)
{
	slist_insert_tail(&event->list_entry, &cq->event_queue);
	cq->event_count++;
	if (cq->wait)
		psmx_wait_signal((struct fid_wait *)cq->wait);
}

static struct psmx_cq_event *psmx_cq_dequeue_event(struct psmx_fid_cq *cq)
{
	struct slist_entry *entry;

	if (slist_empty(&cq->event_queue))
		return NULL;

	entry = slist_remove_head(&cq->event_queue);
	cq->event_count--;

	return container_of(entry, struct psmx_cq_event, list_entry);
}

struct psmx_cq_event *psmx_cq_create_event(struct psmx_fid_cq *cq,
					   void *op_context, void *buf,
					   uint64_t flags, size_t len,
					   uint64_t data, uint64_t tag,
					   size_t olen, int err)
{
	struct psmx_cq_event *event;

	if (!slist_empty(&cq->free_list)) {
		event = container_of(slist_remove_head(&cq->free_list),
				     struct psmx_cq_event, list_entry);
	}
	else {
		event = calloc(1, sizeof(*event));
		if (!event) {
			PSMX_WARN("%s: out of memory.\n", __func__);
			exit(-1);
		}
	}

	if ((event->error = !!err)) {
		event->cqe.err.op_context = op_context;
		event->cqe.err.err = -err;
		event->cqe.err.data = data;
		event->cqe.err.tag = tag;
		event->cqe.err.olen = olen;
		event->cqe.err.prov_errno = PSM_INTERNAL_ERR;
		goto out;
	}

	switch (cq->format) {
	case FI_CQ_FORMAT_CONTEXT:
		event->cqe.context.op_context = op_context;
		break;

	case FI_CQ_FORMAT_MSG:
		event->cqe.msg.op_context = op_context;
		event->cqe.msg.flags = flags;
		event->cqe.msg.len = len;
		break;

	case FI_CQ_FORMAT_DATA:
		event->cqe.data.op_context = op_context;
		event->cqe.data.buf = buf;
		event->cqe.data.flags = flags;
		event->cqe.data.len = len;
		event->cqe.data.data = data;
		break;

	case FI_CQ_FORMAT_TAGGED:
		event->cqe.tagged.op_context = op_context;
		event->cqe.tagged.buf = buf;
		event->cqe.tagged.flags = flags;
		event->cqe.tagged.len = len;
		event->cqe.tagged.data = data;
		event->cqe.tagged.tag = tag;
		break;

	default:
		PSMX_WARN("%s: unsupported CQ format %d\n", __func__, cq->format);
		return NULL;
	}

out:
	return event;
}

static struct psmx_cq_event *psmx_cq_create_event_from_status(
				struct psmx_fid_cq *cq,
				psm_mq_status_t *psm_status,
				uint64_t data,
				struct psmx_cq_event *event_in,
				int count,
				fi_addr_t *src_addr)
{
	struct psmx_cq_event *event;
	struct psmx_multi_recv *req;
	struct fi_context *fi_context = psm_status->context;
	void *op_context, *buf;
	int is_recv = 0;
	uint64_t flags;

	switch((int)PSMX_CTXT_TYPE(fi_context)) {
	case PSMX_SEND_CONTEXT:
		op_context = fi_context;
		buf = PSMX_CTXT_USER(fi_context);
		flags = FI_SEND | FI_MSG;
		break;
	case PSMX_RECV_CONTEXT:
		op_context = fi_context;
		buf = PSMX_CTXT_USER(fi_context);
		flags = FI_RECV | FI_MSG;
		is_recv = 1;
		break;
	case PSMX_MULTI_RECV_CONTEXT:
		op_context = fi_context;
		req = PSMX_CTXT_USER(fi_context);
		buf = req->buf + req->offset;
		flags = FI_RECV | FI_MSG;
		is_recv = 1;
		break;
	case PSMX_TSEND_CONTEXT:
		op_context = fi_context;
		buf = PSMX_CTXT_USER(fi_context);
		flags = FI_SEND | FI_TAGGED;
		break;
	case PSMX_TRECV_CONTEXT:
		op_context = fi_context;
		buf = PSMX_CTXT_USER(fi_context);
		flags = FI_RECV | FI_TAGGED;
		is_recv = 1;
		break;
	case PSMX_READ_CONTEXT:
		op_context = PSMX_CTXT_USER(fi_context);
		buf = NULL;
		flags = FI_READ | FI_RMA;
		break;
	case PSMX_WRITE_CONTEXT:
		op_context = PSMX_CTXT_USER(fi_context);
		buf = NULL;
		flags = FI_WRITE | FI_RMA;
		break;
	case PSMX_REMOTE_READ_CONTEXT:
		op_context = PSMX_CTXT_USER(fi_context);
		buf = NULL;
		flags = FI_REMOTE_READ | FI_RMA;
		break;
	case PSMX_REMOTE_WRITE_CONTEXT:
		op_context = PSMX_CTXT_USER(fi_context);
		buf = NULL;
		flags = FI_REMOTE_WRITE | FI_RMA;
		break;
	default:
		op_context = PSMX_CTXT_USER(fi_context);
		buf = NULL;
		flags = 0;
		break;
	}

	/* NOTE: "event_in" only has space for the CQE of the current CQ format.
	 * Fields like "error_code" and "source" should not be filled in.
	 */
	if (event_in && count && !psm_status->error_code) {
		event = event_in;
	}
	else {
		if (!slist_empty(&cq->free_list)) {
			event = container_of(slist_remove_head(&cq->free_list),
					     struct psmx_cq_event, list_entry);
		}
		else {
			event = calloc(1, sizeof(*event));
			if (!event) {
				PSMX_WARN("%s: out of memory.\n", __func__);
				exit(-1);
			}
		}

		event->error = !!psm_status->error_code;
	}

	if (psm_status->error_code) {
		event->cqe.err.op_context = op_context;
		event->cqe.err.flags = flags;
		event->cqe.err.err = -psmx_errno(psm_status->error_code);
		event->cqe.err.prov_errno = psm_status->error_code;
		event->cqe.err.tag = psm_status->msg_tag;
		event->cqe.err.olen = psm_status->msg_length - psm_status->nbytes;
		if (data)
			event->cqe.err.data = data;
		goto out;
	}

	switch (cq->format) {
	case FI_CQ_FORMAT_CONTEXT:
		event->cqe.context.op_context = op_context;
		break;

	case FI_CQ_FORMAT_MSG:
		event->cqe.msg.op_context = op_context;
		event->cqe.msg.flags = flags;
		event->cqe.msg.len = psm_status->nbytes;
		break;

	case FI_CQ_FORMAT_DATA:
		event->cqe.data.op_context = op_context;
		event->cqe.data.buf = buf;
		event->cqe.data.flags = flags;
		event->cqe.data.len = psm_status->nbytes;
		if (data)
			event->cqe.data.data = data;
		break;

	case FI_CQ_FORMAT_TAGGED:
		event->cqe.tagged.op_context = op_context;
		event->cqe.tagged.buf = buf;
		event->cqe.tagged.flags = flags;
		event->cqe.tagged.len = psm_status->nbytes;
		event->cqe.tagged.tag = psm_status->msg_tag;
		if (data)
			event->cqe.tagged.data = data;
		break;

	default:
		PSMX_WARN("%s: unsupported EQ format %d\n", __func__, cq->format);
		return NULL;
	}

out:
	if (is_recv) {
		if (event == event_in) {
			if (src_addr) {
				int err = -FI_ENODATA;
				if (cq->domain->reserved_tag_bits & PSMX_MSG_BIT & psm_status->msg_tag) {
					err = psmx_epid_to_epaddr(cq->domain,
								  psm_status->msg_tag & ~PSMX_MSG_BIT,
								  (psm_epaddr_t *) src_addr);
				}
				if (err)
					*src_addr = FI_ADDR_NOTAVAIL;
			}
		}
		else {
			event->source = psm_status->msg_tag;
		}
	}

	return event;
}

static int psmx_cq_get_event_src_addr(struct psmx_fid_cq *cq,
				      struct psmx_cq_event *event,
				      fi_addr_t *src_addr)
{
	int err;

	if (!src_addr)
		return 0;

	if ((cq->domain->reserved_tag_bits & PSMX_MSG_BIT) &&
		(event->source & PSMX_MSG_BIT)) {
		err = psmx_epid_to_epaddr(cq->domain,
					  event->source & ~PSMX_MSG_BIT,
					  (psm_epaddr_t *) src_addr);
		if (err)
			return err;

		return 0;
	}

	return -FI_ENODATA;
}

int psmx_cq_poll_mq(struct psmx_fid_cq *cq, struct psmx_fid_domain *domain,
		    struct psmx_cq_event *event_in, int count, fi_addr_t *src_addr)
{
	psm_mq_req_t psm_req;
	psm_mq_status_t psm_status;
	struct fi_context *fi_context;
	struct psmx_fid_ep *tmp_ep;
	struct psmx_fid_cq *tmp_cq;
	struct psmx_fid_cntr *tmp_cntr;
	struct psmx_cq_event *event;
	int multi_recv;
	int err;

	while (1) {
		err = psm_mq_ipeek(domain->psm_mq, &psm_req, NULL);

		if (err == PSM_OK) {
			err = psm_mq_test(&psm_req, &psm_status);

			fi_context = psm_status.context;

			tmp_ep = PSMX_CTXT_EP(fi_context);
			tmp_cq = NULL;
			tmp_cntr = NULL;
			multi_recv = 0;

			switch ((int)PSMX_CTXT_TYPE(fi_context)) {
			case PSMX_NOCOMP_SEND_CONTEXT:
				tmp_cntr = tmp_ep->send_cntr;
				break;

			case PSMX_NOCOMP_RECV_CONTEXT:
				tmp_cntr = tmp_ep->recv_cntr;
				break;

			case PSMX_NOCOMP_WRITE_CONTEXT:
				tmp_cntr = tmp_ep->write_cntr;
				break;

			case PSMX_NOCOMP_READ_CONTEXT:
				tmp_cntr = tmp_ep->read_cntr;
				break;

			case PSMX_INJECT_CONTEXT:
				tmp_cntr = tmp_ep->send_cntr;
				free(fi_context);
				break;

			case PSMX_INJECT_WRITE_CONTEXT:
				tmp_cntr = tmp_ep->write_cntr;
				free(fi_context);
				break;

			case PSMX_SEND_CONTEXT:
			case PSMX_TSEND_CONTEXT:
				tmp_cq = tmp_ep->send_cq;
				tmp_cntr = tmp_ep->send_cntr;
				break;

			case PSMX_RECV_CONTEXT:
			case PSMX_TRECV_CONTEXT:
				tmp_cq = tmp_ep->recv_cq;
				tmp_cntr = tmp_ep->recv_cntr;
				break;

			case PSMX_MULTI_RECV_CONTEXT:
				multi_recv = 1;
				tmp_cq = tmp_ep->recv_cq;
				tmp_cntr = tmp_ep->recv_cntr;
				break;

			case PSMX_READ_CONTEXT:
				tmp_cq = tmp_ep->send_cq;
				tmp_cntr = tmp_ep->read_cntr;
				break;

			case PSMX_WRITE_CONTEXT:
				tmp_cq = tmp_ep->send_cq;
				tmp_cntr = tmp_ep->write_cntr;
				break;

			case PSMX_REMOTE_WRITE_CONTEXT:
				{
				  struct fi_context *fi_context = psm_status.context;
				  struct psmx_fid_mr *mr;
				  struct psmx_am_request *req;
				  req = container_of(fi_context, struct psmx_am_request, fi_context);
				  mr = PSMX_CTXT_USER(fi_context);
				  if (mr->cq) {
					event = psmx_cq_create_event_from_status(
							mr->cq, &psm_status, req->write.data,
							(mr->cq == cq) ? event_in : NULL,
							count, src_addr);
					if (!event)
						return -FI_ENOMEM;

					if (event != event_in)
						psmx_cq_enqueue_event(mr->cq, event);
				  }
				  if (mr->cntr)
					psmx_cntr_inc(mr->cntr);
				  if (mr->domain->rma_ep->remote_write_cntr)
					psmx_cntr_inc(mr->domain->rma_ep->remote_write_cntr);
				  if (!cq || mr->cq == cq)
					return psm_status.error_code ? -FI_EAVAIL : 1;
				  continue;
				}

			case PSMX_REMOTE_READ_CONTEXT:
				{
				  struct fi_context *fi_context = psm_status.context;
				  struct psmx_fid_mr *mr;
				  mr = PSMX_CTXT_USER(fi_context);
				  if (mr->domain->rma_ep->remote_read_cntr)
					psmx_cntr_inc(mr->domain->rma_ep->remote_read_cntr);
				  if (!cq)
					return psm_status.error_code ? -FI_EAVAIL : 1;
				  continue;
				}
			}

			if (tmp_cq) {
				event = psmx_cq_create_event_from_status(tmp_cq, &psm_status, 0,
							(tmp_cq == cq) ? event_in : NULL, count,
							src_addr);
				if (!event)
					return -FI_ENOMEM;

				if (event != event_in)
					psmx_cq_enqueue_event(tmp_cq, event);
			}

			if (tmp_cntr)
				psmx_cntr_inc(tmp_cntr);

			if (multi_recv) {
				struct psmx_multi_recv *req;
				psm_mq_req_t psm_req;

				req = PSMX_CTXT_USER(fi_context);
				req->offset += psm_status.nbytes;
				if (req->offset + req->min_buf_size <= req->len) {
					err = psm_mq_irecv(tmp_ep->domain->psm_mq,
							   req->tag, req->tagsel, req->flag,
							   req->buf + req->offset, 
							   req->len - req->offset,
							   (void *)fi_context, &psm_req);
					if (err != PSM_OK)
						return psmx_errno(err);

					PSMX_CTXT_REQ(fi_context) = psm_req;
				}
				else {
					if (tmp_cq) {
						event = psmx_cq_create_event(
								tmp_cq,
								req->context,
								req->buf,
								FI_MULTI_RECV,
								req->len,
								req->len - req->offset, /* data */
								0,	/* tag */
								0,	/* olen */
								0);	/* err */
						if (!event)
							return -FI_ENOMEM;

						psmx_cq_enqueue_event(tmp_cq, event);
					}

					free(req);
				}
			}

			if (!cq || tmp_cq == cq)
				return psm_status.error_code ? -FI_EAVAIL : 1;
		}
		else if (err == PSM_MQ_NO_COMPLETIONS) {
			return 0;
		}
		else {
			return psmx_errno(err);
		}
	}
}

static ssize_t psmx_cq_readfrom(struct fid_cq *cq, void *buf, size_t count,
				fi_addr_t *src_addr)
{
	struct psmx_fid_cq *cq_priv;
	struct psmx_cq_event *event;
	int ret;
	ssize_t read_count;

	cq_priv = container_of(cq, struct psmx_fid_cq, cq);

	if (slist_empty(&cq_priv->event_queue) || !buf) {
		ret = psmx_cq_poll_mq(cq_priv, cq_priv->domain,
				      (struct psmx_cq_event *)buf, count, src_addr);
		if (ret > 0)
			return ret;

		psmx_am_progress(cq_priv->domain);
	}

	if (cq_priv->pending_error)
		return -FI_EAVAIL;

	if (!buf && count)
		return -FI_EINVAL;

	read_count = 0;
	while (count--) {
		event = psmx_cq_dequeue_event(cq_priv);
		if (event) {
			if (!event->error) {
				memcpy(buf, (void *)&event->cqe, cq_priv->entry_size);
				if (psmx_cq_get_event_src_addr(cq_priv, event, src_addr))
					*src_addr = FI_ADDR_NOTAVAIL;

				memset(event, 0, sizeof(*event));
				slist_insert_tail(&event->list_entry, &cq_priv->free_list);

				read_count++;
				buf += cq_priv->entry_size;
				src_addr++;
				continue;
			}
			else {
				cq_priv->pending_error = event;
				if (!read_count)
					read_count = -FI_EAVAIL;
				break;
			}
		}
		else {
			break;
		}
	}

	return read_count;
}

static ssize_t psmx_cq_read(struct fid_cq *cq, void *buf, size_t count)
{
	return psmx_cq_readfrom(cq, buf, count, NULL);
}

static ssize_t psmx_cq_readerr(struct fid_cq *cq, struct fi_cq_err_entry *buf,
			       uint64_t flags)
{
	struct psmx_fid_cq *cq_priv;

	cq_priv = container_of(cq, struct psmx_fid_cq, cq);

	if (cq_priv->pending_error) {
		memcpy(buf, &cq_priv->pending_error->cqe, sizeof *buf);
		free(cq_priv->pending_error);
		cq_priv->pending_error = NULL;
		return sizeof *buf;
	}

	return 0;
}

static ssize_t psmx_cq_write(struct fid_cq *cq, const void *buf, size_t len)
{
	struct psmx_fid_cq *cq_priv;
	struct psmx_cq_event *event;
	ssize_t written_len = 0;

	cq_priv = container_of(cq, struct psmx_fid_cq, cq);

	while (len >= cq_priv->entry_size) {
		event = calloc(1, sizeof(*event));
		if (!event) {
			PSMX_WARN("%s: out of memory\n", __func__);
			return -FI_ENOMEM;
		}

		memcpy((void *)&event->cqe, buf + written_len, cq_priv->entry_size);
		psmx_cq_enqueue_event(cq_priv, event);
		written_len += cq_priv->entry_size;
		len -= cq_priv->entry_size;
	}

	return written_len;
}

static ssize_t psmx_cq_writeerr(struct fid_cq *cq, struct fi_cq_err_entry *buf,
				size_t len, uint64_t flags)
{
	struct psmx_fid_cq *cq_priv;
	struct psmx_cq_event *event;
	ssize_t written_len = 0;

	cq_priv = container_of(cq, struct psmx_fid_cq, cq);

	while (len >= sizeof(*buf)) {
		event = calloc(1, sizeof(*event));
		if (!event) {
			PSMX_WARN("%s: out of memory\n", __func__);
			return -FI_ENOMEM;
		}

		memcpy((void *)&event->cqe, buf + written_len, sizeof(*buf));
		event->error = !!event->cqe.err.err;
		psmx_cq_enqueue_event(cq_priv, event);
		written_len += sizeof(*buf);
		len -= sizeof(*buf);
	}

	return written_len;
}

static ssize_t psmx_cq_sreadfrom(struct fid_cq *cq, void *buf, size_t count,
				 fi_addr_t *src_addr, const void *cond,
				 int timeout)
{
	struct psmx_fid_cq *cq_priv;
	struct timespec ts0, ts;
	size_t threshold, event_count;
	int msec_passed = 0;

	cq_priv = container_of(cq, struct psmx_fid_cq, cq);
	if (cq_priv->wait_cond == FI_CQ_COND_THRESHOLD)
		threshold = (size_t) cond;
	else
		threshold = 1;

	/* NOTE: "cond" is only a hint, not a mandatory condition. */
	event_count = cq_priv->event_count;
	if (event_count < threshold) {
		if (cq_priv->wait) {
			psmx_wait_wait((struct fid_wait *)cq_priv->wait, timeout);
		}
		else {
			clock_gettime(CLOCK_REALTIME, &ts0);
			while (1) {
				if (psmx_cq_poll_mq(cq_priv, cq_priv->domain, NULL, 0, NULL))
					break;

				/* CQ may be updated asynchronously by the AM handlers */
				if (cq_priv->event_count > event_count)
					break;

				if (timeout < 0)
					continue;

				clock_gettime(CLOCK_REALTIME, &ts);
				msec_passed = (ts.tv_sec - ts0.tv_sec) * 1000 +
					       (ts.tv_nsec - ts0.tv_nsec) / 1000000;

				if (msec_passed >= timeout)
					break;
			}
		}
	}

	return psmx_cq_readfrom(cq, buf, count, src_addr);
}

static ssize_t psmx_cq_sread(struct fid_cq *cq, void *buf, size_t count,
			     const void *cond, int timeout)
{
	return psmx_cq_sreadfrom(cq, buf, count, NULL, cond, timeout);
}

static const char *psmx_cq_strerror(struct fid_cq *cq, int prov_errno, const void *prov_data,
				    char *buf, size_t len)
{
	return psm_error_get_string(prov_errno);
}

static int psmx_cq_close(fid_t fid)
{
	struct psmx_fid_cq *cq;
	struct slist_entry *entry;
	struct psmx_cq_event *item;

	cq = container_of(fid, struct psmx_fid_cq, cq.fid);

	while (!slist_empty(&cq->free_list)) {
		entry = slist_remove_head(&cq->free_list);
		item = container_of(entry, struct psmx_cq_event, list_entry);
		free(item);
	}

	if (cq->wait && cq->wait_is_local)
		fi_close((fid_t)cq->wait);

	free(cq);

	return 0;
}

static int psmx_cq_control(struct fid *fid, int command, void *arg)
{
	struct psmx_fid_cq *cq;
	int ret = 0;

	cq = container_of(fid, struct psmx_fid_cq, cq.fid);

	switch (command) {
	case FI_GETWAIT:
		ret = psmx_wait_get_obj(cq->wait, arg);
		break;

	default:
		return -FI_ENOSYS;
	}

	return ret;
}

static struct fi_ops psmx_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = psmx_cq_close,
	.bind = fi_no_bind,
	.control = psmx_cq_control,
};

static struct fi_ops_cq psmx_cq_ops = {
	.size = sizeof(struct fi_ops_cq),
	.read = psmx_cq_read,
	.readfrom = psmx_cq_readfrom,
	.readerr = psmx_cq_readerr,
	.write = psmx_cq_write,
	.writeerr = psmx_cq_writeerr,
	.sread = psmx_cq_sread,
	.sreadfrom = psmx_cq_sreadfrom,
	.strerror = psmx_cq_strerror,
};

int psmx_cq_open(struct fid_domain *domain, struct fi_cq_attr *attr,
		 struct fid_cq **cq, void *context)
{
	struct psmx_fid_domain *domain_priv;
	struct psmx_fid_cq *cq_priv;
	struct psmx_fid_wait *wait = NULL;
	struct psmx_cq_event *event;
	struct fi_wait_attr wait_attr;
	int wait_is_local = 0;
	int entry_size;
	int err;
	int i;

	domain_priv = container_of(domain, struct psmx_fid_domain, domain);
	switch (attr->format) {
	case FI_CQ_FORMAT_UNSPEC:
		attr->format = FI_CQ_FORMAT_TAGGED;
		entry_size = sizeof(struct fi_cq_tagged_entry);
		break;

	case FI_CQ_FORMAT_CONTEXT:
		entry_size = sizeof(struct fi_cq_entry);
		break;

	case FI_CQ_FORMAT_MSG:
		entry_size = sizeof(struct fi_cq_msg_entry);
		break;

	case FI_CQ_FORMAT_DATA:
		entry_size = sizeof(struct fi_cq_data_entry);
		break;

	case FI_CQ_FORMAT_TAGGED:
		entry_size = sizeof(struct fi_cq_tagged_entry);
		break;

	default:
		PSMX_DEBUG("attr->format=%d, supported=%d...%d\n", attr->format,
				FI_CQ_FORMAT_UNSPEC, FI_CQ_FORMAT_TAGGED);
		return -FI_EINVAL;
	}

	switch (attr->wait_obj) {
	case FI_WAIT_NONE:
	case FI_WAIT_UNSPEC:
		break;

	case FI_WAIT_SET:
		if (!attr->wait_set) {
			PSMX_DEBUG("FI_WAIT_SET is specified but attr->wait_set is NULL\n");
			return -FI_EINVAL;
		}
		wait = (struct psmx_fid_wait *)attr->wait_set;
		break;

	case FI_WAIT_FD:
	case FI_WAIT_MUTEX_COND:
		wait_attr.wait_obj = attr->wait_obj;
		wait_attr.flags = 0;
		err = psmx_wait_open(&domain_priv->fabric->fabric,
				     &wait_attr, (struct fid_wait **)&wait);
		if (err)
			return err;
		wait_is_local = 1;
		break;

	default:
		PSMX_DEBUG("attr->wait_obj=%d, supported=%d...%d\n", attr->wait_obj,
				FI_WAIT_NONE, FI_WAIT_MUTEX_COND);
		return -FI_EINVAL;
	}

	if (wait) {
		switch (attr->wait_cond) {
		case FI_CQ_COND_NONE:
		case FI_CQ_COND_THRESHOLD:
			break;

		default:
			PSMX_DEBUG("attr->wait_cond=%d, supported=%d...%d\n",
					attr->wait_cond, FI_CQ_COND_NONE, FI_CQ_COND_THRESHOLD);
			return -FI_EINVAL;
		}
	}

	cq_priv = (struct psmx_fid_cq *) calloc(1, sizeof *cq_priv);
	if (!cq_priv) {
		if (wait)
			free(wait);
		return -FI_ENOMEM;
	}

	cq_priv->domain = domain_priv;
	cq_priv->format = attr->format;
	cq_priv->entry_size = entry_size;
	cq_priv->wait = wait;
	if (wait)
		cq_priv->wait_cond = attr->wait_cond;
	cq_priv->wait_is_local = wait_is_local;

	cq_priv->cq.fid.fclass = FI_CLASS_CQ;
	cq_priv->cq.fid.context = context;
	cq_priv->cq.fid.ops = &psmx_fi_ops;
	cq_priv->cq.ops = &psmx_cq_ops;

	slist_init(&cq_priv->event_queue);
	slist_init(&cq_priv->free_list);

#define PSMX_FREE_LIST_SIZE	64
	for (i=0; i<PSMX_FREE_LIST_SIZE; i++) {
		event = calloc(1, sizeof(*event));
		if (!event) {
			PSMX_WARN("%s: out of memory.\n", __func__);
			exit(-1);
		}
		slist_insert_tail(&event->list_entry, &cq_priv->free_list);
	}

	*cq = &cq_priv->cq;
	return 0;
}

