/*
 * Copyright (c) 2013-2014 Intel Corporation. All rights reserved.
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * OpenFabrics.org BSD license below:
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdlib.h>
#include <string.h>

#include "sock.h"
#include "sock_util.h"

extern struct fi_ops_rma sock_ep_rma;
extern struct fi_ops_msg sock_ep_msg_ops;
extern struct fi_ops_tagged sock_ep_tagged;
extern struct fi_ops_atomic sock_ep_atomic;

extern struct fi_ops_cm sock_ep_cm_ops;
extern struct fi_ops_ep sock_ep_ops;
extern struct fi_ops sock_ep_fi_ops;
extern struct fi_ops_ep sock_ctx_ep_ops;
extern struct fi_ops sock_ctx_ops;

extern const struct fi_domain_attr sock_domain_attr;
extern const struct fi_fabric_attr sock_fabric_attr;

extern const char const sock_fab_name[];
extern const char const sock_dom_name[];

static int sock_ctx_close(struct fid *fid)
{
	struct sock_ep *ep;
	struct dlist_entry *entry;
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;

	switch (fid->fclass) {
	case FI_CLASS_TX_CTX:
		tx_ctx = container_of(fid, struct sock_tx_ctx, ctx.fid);
		
		for (entry = tx_ctx->ep_list.next; entry != &tx_ctx->ep_list;
		    entry = entry->next) {
			ep = container_of(entry, struct sock_ep, tx_ctx_entry);
			atomic_dec(&ep->num_tx_ctx);
		}
		sock_tx_ctx_free(tx_ctx);
		break;

	case FI_CLASS_RX_CTX:
		rx_ctx = container_of(fid, struct sock_rx_ctx, ctx.fid);
		
		for (entry = rx_ctx->ep_list.next; entry != &rx_ctx->ep_list;
		    entry = entry->next) {
			ep = container_of(entry, struct sock_ep, rx_ctx_entry);
			atomic_dec(&ep->num_rx_ctx);
		}
		sock_rx_ctx_free(rx_ctx);
		break;

	case FI_CLASS_STX_CTX:
		tx_ctx = container_of(fid, struct sock_tx_ctx, stx.fid);
		atomic_dec(&tx_ctx->domain->ref);
		sock_tx_ctx_free(tx_ctx);
		break;

	case FI_CLASS_SRX_CTX:
		rx_ctx = container_of(fid, struct sock_rx_ctx, ctx.fid);
		atomic_dec(&rx_ctx->domain->ref);
		sock_rx_ctx_free(rx_ctx);
		break;

	default:
		SOCK_LOG_ERROR("Invalid fid\n");
		return -FI_EINVAL;
	}
	return 0;
}

static int sock_ctx_bind_cq(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	struct sock_cq *sock_cq;
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;

	sock_cq = container_of(bfid, struct sock_cq, cq_fid.fid);
	switch (fid->fclass) {
	case FI_CLASS_TX_CTX:
		tx_ctx = container_of(fid, struct sock_tx_ctx, ctx);
		if (flags & FI_SEND) {
			tx_ctx->comp.send_cq = sock_cq;
			if (flags & FI_COMPLETION)
				tx_ctx->comp.send_cq_event = 1;
		}

		if (flags & FI_READ) {
			tx_ctx->comp.read_cq = sock_cq;
			if (flags & FI_COMPLETION)
				tx_ctx->comp.read_cq_event = 1;
		}

		if (flags & FI_WRITE) {
			tx_ctx->comp.write_cq = sock_cq;
			if (flags & FI_COMPLETION)
				tx_ctx->comp.write_cq_event = 1;
		}

		if (!tx_ctx->progress) {
			tx_ctx->progress = 1;
			sock_pe_add_tx_ctx(tx_ctx->domain->pe, tx_ctx);
		}
		dlist_insert_tail(&tx_ctx->cq_entry, &sock_cq->tx_list);
		break;
		
	case FI_CLASS_RX_CTX:
		rx_ctx = container_of(fid, struct sock_rx_ctx, ctx.fid);
		if (flags & FI_RECV) {
			rx_ctx->comp.recv_cq = sock_cq;
			if (flags & FI_COMPLETION)
				rx_ctx->comp.recv_cq_event = 1;
		}

		if (flags & FI_REMOTE_READ) {
			rx_ctx->comp.rem_read_cq = sock_cq;
			if (flags & FI_COMPLETION)
				rx_ctx->comp.rem_read_cq_event = 1;
		}

		if (flags & FI_REMOTE_WRITE) {
			rx_ctx->comp.rem_write_cq = sock_cq;
			if (flags & FI_COMPLETION)
				rx_ctx->comp.rem_write_cq_event = 1;
		}

		if (!rx_ctx->progress) {
			rx_ctx->progress = 1;
			sock_pe_add_rx_ctx(rx_ctx->domain->pe, rx_ctx);
		}
		dlist_insert_tail(&rx_ctx->cq_entry, &sock_cq->rx_list);
		break;

	case FI_CLASS_STX_CTX:
		tx_ctx = container_of(fid, struct sock_tx_ctx, stx.fid);
		if (flags & FI_SEND) {
			tx_ctx->comp.send_cq = sock_cq;
			if (flags & FI_COMPLETION)
				tx_ctx->comp.send_cq_event = 1;
		}

		if (flags & FI_READ) {
			tx_ctx->comp.read_cq = sock_cq;
			if (flags & FI_COMPLETION)
				tx_ctx->comp.read_cq_event = 1;
		}

		if (flags & FI_WRITE) {
			tx_ctx->comp.write_cq = sock_cq;
			if (flags & FI_COMPLETION)
				tx_ctx->comp.write_cq_event = 1;
		}

		if (!tx_ctx->progress) {
			tx_ctx->progress = 1;
			sock_pe_add_tx_ctx(tx_ctx->domain->pe, tx_ctx);
		}
		dlist_insert_tail(&tx_ctx->cq_entry, &sock_cq->tx_list);
		break;
			
	default:
		SOCK_LOG_ERROR("Invalid fid\n");
		return -FI_EINVAL;
	}
	return 0;
}

static int sock_ctx_bind_cntr(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	struct sock_cntr *cntr;
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;

	cntr = container_of(bfid, struct sock_cntr, cntr_fid.fid);
	switch (fid->fclass) {
	case FI_CLASS_TX_CTX:
		tx_ctx = container_of(fid, struct sock_tx_ctx, ctx.fid);
		if (flags & FI_SEND)
			tx_ctx->comp.send_cntr = cntr;
		
		if (flags & FI_READ)
			tx_ctx->comp.read_cntr = cntr;

		if (flags & FI_WRITE)
			tx_ctx->comp.write_cntr = cntr;

		if (!tx_ctx->progress) {
			tx_ctx->progress = 1;
			sock_pe_add_tx_ctx(tx_ctx->domain->pe, tx_ctx);
		}
		dlist_insert_tail(&tx_ctx->cntr_entry, &cntr->tx_list);
			
		break;
		
	case FI_CLASS_RX_CTX:
		rx_ctx = container_of(fid, struct sock_rx_ctx, ctx.fid);
		if (flags & FI_RECV) 
			rx_ctx->comp.recv_cntr = cntr;

		if (flags & FI_REMOTE_READ) 
			rx_ctx->comp.rem_read_cntr = cntr;

		if (flags & FI_REMOTE_WRITE) 
			rx_ctx->comp.rem_write_cntr = cntr;
		
		if (!rx_ctx->progress) {
			rx_ctx->progress = 1;
			sock_pe_add_rx_ctx(rx_ctx->domain->pe, rx_ctx);
		}
		dlist_insert_tail(&rx_ctx->cntr_entry, &cntr->rx_list);
		break;

	case FI_CLASS_STX_CTX:
		tx_ctx = container_of(fid, struct sock_tx_ctx, ctx.fid);
		if (flags & FI_SEND)
			tx_ctx->comp.send_cntr = cntr;
		
		if (flags & FI_READ)
			tx_ctx->comp.read_cntr = cntr;

		if (flags & FI_WRITE)
			tx_ctx->comp.write_cntr = cntr;

		if (!tx_ctx->progress) {
			tx_ctx->progress = 1;
			sock_pe_add_tx_ctx(tx_ctx->domain->pe, tx_ctx);
		}
		dlist_insert_tail(&tx_ctx->cntr_entry, &cntr->tx_list);
			
		break;
			
	default:
		SOCK_LOG_ERROR("Invalid fid\n");
		return -FI_EINVAL;
	}
	return 0;
}

static int sock_ctx_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	switch (bfid->fclass) {
	case FI_CLASS_CQ:
		return sock_ctx_bind_cq(fid, bfid, flags);

	case FI_CLASS_CNTR:
		return sock_ctx_bind_cntr(fid, bfid, flags);

	default:
		SOCK_LOG_ERROR("Invalid bind()\n");
		return -FI_EINVAL;
	}

}

static int sock_ctx_control(struct fid *fid, int command, void *arg)
{
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;

	switch (fid->fclass) {
	case FI_CLASS_TX_CTX:
		tx_ctx = container_of(fid, struct sock_tx_ctx, ctx.fid);
		switch (command) {
		case FI_GETOPSFLAG:
			*(uint64_t*)arg = tx_ctx->attr.op_flags;
			break;
		case FI_SETOPSFLAG:
			tx_ctx->attr.op_flags = (uint64_t)arg;
			break;
		default:
			return -FI_EINVAL;
		}
		break;

	case FI_CLASS_RX_CTX:
		rx_ctx = container_of(fid, struct sock_rx_ctx, ctx.fid);
		switch (command) {
		case FI_GETOPSFLAG:
			*(uint64_t*)arg = rx_ctx->attr.op_flags;
			break;
		case FI_SETOPSFLAG:
			rx_ctx->attr.op_flags = (uint64_t)arg;
			break;
		default:
			return -FI_EINVAL;
		}
		break;

	case FI_CLASS_STX_CTX:
		tx_ctx = container_of(fid, struct sock_tx_ctx, stx.fid);
		switch (command) {
		case FI_GETOPSFLAG:
			*(uint64_t*)arg = tx_ctx->attr.op_flags;
			break;
		case FI_SETOPSFLAG:
			tx_ctx->attr.op_flags = (uint64_t)arg;
			break;
		default:
			return -FI_EINVAL;
		}
		break;

	default:
		return -FI_EINVAL;
	}
	
	return 0;
}

struct fi_ops sock_ctx_ops = {
	.size = sizeof(struct fi_ops),
	.close = sock_ctx_close,
	.bind = sock_ctx_bind,
	.control = sock_ctx_control,
};

static int sock_ctx_enable(struct fid_ep *ep)
{
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;

	switch (ep->fid.fclass) {
	case FI_CLASS_RX_CTX:
		rx_ctx = container_of(ep, struct sock_rx_ctx, ctx.fid);
		rx_ctx->enabled = 1;
		return 0;

	case FI_CLASS_TX_CTX:
		tx_ctx = container_of(ep, struct sock_tx_ctx, ctx.fid);
		tx_ctx->enabled = 1;
		return 0;

	default:
		SOCK_LOG_ERROR("Invalid CTX\n");
		break;
	}
	return -FI_EINVAL;
}

static int sock_ctx_getopt(fid_t fid, int level, int optname,
		       void *optval, size_t *optlen)
{
	struct sock_rx_ctx *rx_ctx;
	rx_ctx = container_of(fid, struct sock_rx_ctx, ctx.fid);

	if (level != FI_OPT_ENDPOINT)
		return -ENOPROTOOPT;

	switch (optname) {
	case FI_OPT_MIN_MULTI_RECV:
		*(size_t *)optval = rx_ctx->min_multi_recv;
		*optlen = sizeof(size_t);
		break;

	default:
		return -FI_ENOPROTOOPT;
	}
	return 0;
}

static int sock_ctx_setopt(fid_t fid, int level, int optname,
		       const void *optval, size_t optlen)
{
	struct sock_rx_ctx *rx_ctx;
	rx_ctx = container_of(fid, struct sock_rx_ctx, ctx.fid);

	if (level != FI_OPT_ENDPOINT)
		return -ENOPROTOOPT;

	switch (optname) {
	case FI_OPT_MIN_MULTI_RECV:
		rx_ctx->min_multi_recv = *(size_t *)optval;
		break;
		
	default:
		return -ENOPROTOOPT;
	}
	return 0;
}

static ssize_t sock_ep_cancel(fid_t fid, void *context)
{
	int ret;
	struct sock_rx_ctx *rx_ctx;
	struct sock_rx_entry *rx_entry;
	struct sock_ep *sock_ep;
	struct dlist_entry *entry;

	switch (fid->fclass) {
	case FI_CLASS_EP:
		sock_ep = container_of(fid, struct sock_ep, ep.fid);
		rx_ctx = sock_ep->rx_ctx;
		break;

	case FI_CLASS_RX_CTX:
	case FI_CLASS_SRX_CTX:
		rx_ctx = container_of(fid, struct sock_rx_ctx, ctx.fid);
		break;

	default:
		SOCK_LOG_ERROR("Invalid ep type\n");
		return -FI_EINVAL;
	}

	ret = -FI_ENOENT;
	fastlock_acquire(&rx_ctx->lock);
	for (entry = rx_ctx->rx_entry_list.next;
	     entry != &rx_ctx->rx_entry_list; entry = entry->next) {
		
		rx_entry = container_of(entry, struct sock_rx_entry, entry);
		if (rx_entry->is_busy || rx_entry->used)
			continue;

		if ((uint64_t)context == rx_entry->context) {
			dlist_remove(&rx_entry->entry);
			sock_rx_release_entry(rx_entry);
			ret = 0;
			break;
		}
	}		
	fastlock_release(&rx_ctx->lock);
	return ret;
}

struct fi_ops_ep sock_ctx_ep_ops = {
	.size = sizeof(struct fi_ops_ep),
	.enable = sock_ctx_enable,
	.cancel = sock_ep_cancel,
	.getopt = sock_ctx_getopt,
	.setopt = sock_ctx_setopt,
	.tx_ctx = fi_no_tx_ctx,
	.rx_ctx = fi_no_rx_ctx,
};

static int sock_ep_close(struct fid *fid)
{
	struct sock_ep *sock_ep;
	sock_ep = container_of(fid, struct sock_ep, ep.fid);

	if (atomic_get(&sock_ep->ref) || atomic_get(&sock_ep->num_rx_ctx) ||
	    atomic_get(&sock_ep->num_tx_ctx))
		return -FI_EBUSY;
	
	if (sock_ep->tx_array[sock_ep->ep_attr.tx_ctx_cnt]) 
		sock_tx_ctx_free(sock_ep->tx_array[sock_ep->ep_attr.tx_ctx_cnt]);
	if (sock_ep->rx_array[sock_ep->ep_attr.rx_ctx_cnt]) 
		sock_rx_ctx_free(sock_ep->rx_array[sock_ep->ep_attr.rx_ctx_cnt]);

	free(sock_ep->tx_array);
	free(sock_ep->rx_array);
	
	if (sock_ep->src_addr)
		free(sock_ep->src_addr);
	if (sock_ep->dest_addr)
		free(sock_ep->dest_addr);
	
	free(sock_ep);
	return 0;
}

static int sock_ep_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	int ret, i;
	struct sock_ep *ep;
	struct sock_cq *cq;
	struct sock_av *av;
	struct sock_cntr *cntr;
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;

	ep = container_of(fid, struct sock_ep, ep.fid);
	
	switch (bfid->fclass) {
	case FI_CLASS_EQ:
		return -FI_EINVAL;

	case FI_CLASS_MR:
		return -FI_EINVAL;

	case FI_CLASS_CQ:
		cq = container_of(bfid, struct sock_cq, cq_fid.fid);
		assert(ep->domain == cq->domain);

		if (flags & FI_SEND) {
			ep->comp.send_cq = cq;
			if (flags & FI_COMPLETION)
				ep->comp.send_cq_event = 1;
		}

		if (flags & FI_READ) {
			ep->comp.read_cq = cq;
			if (flags & FI_COMPLETION)
				ep->comp.read_cq_event = 1;
		}

		if (flags & FI_WRITE) {
			ep->comp.write_cq = cq;
			if (flags & FI_COMPLETION)
				ep->comp.write_cq_event = 1;
		}

		if (flags & FI_RECV) {
			ep->comp.recv_cq = cq;
			if (flags & FI_COMPLETION)
				ep->comp.recv_cq_event = 1;
		}

		if (flags & FI_REMOTE_READ) {
			ep->comp.rem_read_cq = cq;
			if (flags & FI_COMPLETION)
				ep->comp.rem_read_cq_event = 1;
		}

		if (flags & FI_REMOTE_WRITE) {
			ep->comp.rem_write_cq = cq;
			if (flags & FI_COMPLETION)
				ep->comp.rem_write_cq_event = 1;
		}

		if (flags & FI_SEND || flags & FI_WRITE || flags & FI_READ) {
			for (i=0; i<=ep->ep_attr.tx_ctx_cnt; i++) {
				tx_ctx = ep->tx_array[i];
				
				if (!tx_ctx)
					continue;
				
				if ((ret = sock_ctx_bind_cq(&tx_ctx->ctx.fid, 
							    bfid, flags)))
					return ret;
			}
		}

		if (flags & FI_RECV || flags & FI_REMOTE_READ || 
		    flags & FI_REMOTE_WRITE) {
			for (i=0; i<=ep->ep_attr.rx_ctx_cnt; i++) {
				rx_ctx = ep->rx_array[i];
				
				if (!rx_ctx)
					continue;
				
				if ((ret = sock_ctx_bind_cq(&rx_ctx->ctx.fid, 
							    bfid, flags)))
					return ret;
			}
		}
		break;

	case FI_CLASS_CNTR:
		cntr = container_of(bfid, struct sock_cntr, cntr_fid.fid);
		assert(ep->domain == cntr->domain);

		if (flags & FI_SEND)
			ep->comp.send_cntr = cntr;

		if (flags & FI_RECV)
			ep->comp.recv_cntr = cntr;

		if (flags & FI_READ)
			ep->comp.read_cntr = cntr;

		if (flags & FI_WRITE)
			ep->comp.write_cntr = cntr;

		if (flags & FI_REMOTE_READ)
			ep->comp.rem_read_cntr = cntr;
		
		if (flags & FI_REMOTE_WRITE)
			ep->comp.rem_write_cntr = cntr;
		
		if (flags & FI_SEND || flags & FI_WRITE || flags & FI_READ) {
			for (i=0; i<=ep->ep_attr.tx_ctx_cnt; i++) {
				tx_ctx = ep->tx_array[i];
				
				if (!tx_ctx)
					continue;
				
				if ((ret = sock_ctx_bind_cntr(&tx_ctx->ctx.fid, 
							      bfid, flags)))
					return ret;
			}
		}

		if (flags & FI_RECV || flags & FI_REMOTE_READ || 
		    flags & FI_REMOTE_WRITE) {
			for (i=0; i<=ep->ep_attr.rx_ctx_cnt; i++) {
				rx_ctx = ep->rx_array[i];
				
				if (!rx_ctx)
					continue;
				
				if ((ret = sock_ctx_bind_cntr(&rx_ctx->ctx.fid, 
							      bfid, flags)))
					return ret;
			}
		}
		break;

	case FI_CLASS_AV:
		av = container_of(bfid, struct sock_av, av_fid.fid);
		assert(ep->domain == av->domain);

		ep->av = av;
		av->cmap = &av->domain->r_cmap;

		if (ep->tx_ctx && 
		    ep->tx_ctx->ctx.fid.fclass == FI_CLASS_TX_CTX) {
			ep->tx_ctx->av = av;
		}
		
		if (ep->rx_ctx && 
		    ep->rx_ctx->ctx.fid.fclass == FI_CLASS_RX_CTX)
			ep->rx_ctx->av = av;
		
		for (i=0; i<ep->ep_attr.tx_ctx_cnt; i++) {
			if (ep->tx_array[i])
				ep->tx_array[i]->av = av;
		}

		for (i=0; i<ep->ep_attr.rx_ctx_cnt; i++) {
			if (ep->rx_array[i])
				ep->rx_array[i]->av = av;
		}
		
		break;

	case FI_CLASS_STX_CTX:
		tx_ctx = container_of(bfid, struct sock_tx_ctx, stx.fid);
		dlist_insert_tail(&ep->tx_ctx_entry, &tx_ctx->ep_list);
		ep->tx_ctx = tx_ctx;
		ep->tx_array[ep->ep_attr.tx_ctx_cnt] = tx_ctx;
		break;

	case FI_CLASS_SRX_CTX:
		rx_ctx = container_of(bfid, struct sock_rx_ctx, ctx);
		dlist_insert_tail(&ep->rx_ctx_entry, &rx_ctx->ep_list);
		ep->rx_ctx = rx_ctx;
		ep->rx_array[ep->ep_attr.rx_ctx_cnt] = rx_ctx;
		break;

	default:
		return -ENOSYS;
	}

	return 0;
}

static int sock_ep_control(struct fid *fid, int command, void *arg)
{
	struct fi_alias *alias;
	struct sock_ep *ep, *new_ep;
	ep = container_of(fid, struct sock_ep, ep.fid);

	switch (command) {
	case FI_ALIAS:
		alias = (struct fi_alias*)arg;
		new_ep = calloc(1, sizeof(*new_ep));
		if (!new_ep)
			return -FI_ENOMEM;
		*new_ep = *ep;
		new_ep->op_flags = alias->flags;
		*alias->fid = &new_ep->ep.fid;
		break;

	case FI_GETOPSFLAG:
		*(uint64_t*)arg = ep->op_flags;
		break;

	case FI_SETOPSFLAG:
		ep->op_flags = (uint64_t)arg;
		break;

	default:
		return -FI_EINVAL;
	}
	return 0;
}


struct fi_ops sock_ep_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = sock_ep_close,
	.bind = sock_ep_bind,
	.control = sock_ep_control,
	.ops_open = fi_no_ops_open,
};

static int sock_ep_enable(struct fid_ep *ep)
{
	int i;
	struct sock_ep *sock_ep;

	sock_ep = container_of(ep, struct sock_ep, ep);

	if (sock_ep->tx_ctx && 
	    sock_ep->tx_ctx->ctx.fid.fclass == FI_CLASS_TX_CTX)
		sock_ep->tx_ctx->enabled = 1;

	if (sock_ep->rx_ctx && 
	    sock_ep->rx_ctx->ctx.fid.fclass == FI_CLASS_RX_CTX)
		sock_ep->rx_ctx->enabled = 1;

	for (i=0; i<sock_ep->ep_attr.tx_ctx_cnt; i++) {
		if (sock_ep->tx_array[i])
			sock_ep->tx_array[i]->enabled = 1;
	}

	for (i=0; i<sock_ep->ep_attr.rx_ctx_cnt; i++) {
		if (sock_ep->rx_array[i])
			sock_ep->rx_array[i]->enabled = 1;
	}
	return 0;
}

static int sock_ep_getopt(fid_t fid, int level, int optname,
		       void *optval, size_t *optlen)
{
	struct sock_ep *sock_ep;
	sock_ep = container_of(fid, struct sock_ep, ep.fid);

	if (level != FI_OPT_ENDPOINT)
		return -ENOPROTOOPT;

	switch (optname) {
	case FI_OPT_MIN_MULTI_RECV:
		*(size_t *)optval = sock_ep->min_multi_recv;
		*optlen = sizeof(size_t);
		break;

	default:
		return -FI_ENOPROTOOPT;
	}
	return 0;
}

static int sock_ep_setopt(fid_t fid, int level, int optname,
		       const void *optval, size_t optlen)
{
	int i;
	struct sock_ep *sock_ep;
	sock_ep = container_of(fid, struct sock_ep, ep.fid);

	if (level != FI_OPT_ENDPOINT)
		return -ENOPROTOOPT;

	switch (optname) {
	case FI_OPT_MIN_MULTI_RECV:

		sock_ep->min_multi_recv = *(size_t *)optval;
		for (i = 0; i < sock_ep->ep_attr.rx_ctx_cnt + 1; i ++) {
			if (sock_ep->rx_array[i] != NULL) {
				sock_ep->rx_array[i]->min_multi_recv = 
					sock_ep->min_multi_recv;
			}
		}
		break;
		
	default:
		return -ENOPROTOOPT;
	}
	return 0;
}

static int sock_ep_tx_ctx(struct fid_sep *ep, int index, struct fi_tx_attr *attr,
			  struct fid_ep **tx_ep, void *context)
{
	struct sock_ep *sock_ep;
	struct sock_tx_ctx *tx_ctx;

	sock_ep = container_of(ep, struct sock_ep, sep);
	if (index >= sock_ep->ep_attr.tx_ctx_cnt)
		return -FI_EINVAL;

	tx_ctx = sock_tx_ctx_alloc(&sock_ep->tx_attr, context);
	if (!tx_ctx)
		return -FI_ENOMEM;

	tx_ctx->tx_id = index;
	tx_ctx->ep = sock_ep;
	tx_ctx->domain = sock_ep->domain;
	dlist_insert_tail(&sock_ep->tx_ctx_entry, &tx_ctx->ep_list);

	tx_ctx->ctx.ops = &sock_ctx_ep_ops;
	tx_ctx->ctx.msg = &sock_ep_msg_ops;
	tx_ctx->ctx.tagged = &sock_ep_tagged;
	tx_ctx->ctx.rma = &sock_ep_rma;
	tx_ctx->ctx.atomic = &sock_ep_atomic;

	*tx_ep = &tx_ctx->ctx;
	sock_ep->tx_array[index] = tx_ctx;
	atomic_inc(&sock_ep->num_tx_ctx);
	return 0;
}

static int sock_ep_rx_ctx(struct fid_sep *ep, int index, struct fi_rx_attr *attr,
		    struct fid_ep **rx_ep, void *context)
{
	struct sock_ep *sock_ep;
	struct sock_rx_ctx *rx_ctx;

	sock_ep = container_of(ep, struct sock_ep, sep);
	if (index >= sock_ep->ep_attr.rx_ctx_cnt)
		return -FI_EINVAL;

	rx_ctx = sock_rx_ctx_alloc(attr, context);
	if (!rx_ctx)
		return -FI_ENOMEM;

	rx_ctx->rx_id = index;
	rx_ctx->ep = sock_ep;
	rx_ctx->domain = sock_ep->domain;
	dlist_insert_tail(&sock_ep->rx_ctx_entry, &rx_ctx->ep_list);

	rx_ctx->ctx.ops = &sock_ctx_ep_ops;
	rx_ctx->ctx.msg = &sock_ep_msg_ops;
	rx_ctx->ctx.tagged = &sock_ep_tagged;

	rx_ctx->min_multi_recv = sock_ep->min_multi_recv;
	*rx_ep = &rx_ctx->ctx;
	sock_ep->rx_array[index] = rx_ctx;
	atomic_inc(&sock_ep->num_rx_ctx);
	return 0;
}

struct fi_ops_ep sock_ep_ops ={
	.size = sizeof(struct fi_ops_ep),
	.enable = sock_ep_enable,
	.cancel = sock_ep_cancel,
	.getopt = sock_ep_getopt,
	.setopt = sock_ep_setopt,
	.tx_ctx = sock_ep_tx_ctx,
	.rx_ctx = sock_ep_rx_ctx,
};

static int sock_ep_cm_getname(fid_t fid, void *addr, size_t *addrlen)
{
	struct sock_ep *sock_ep;
	if (*addrlen == 0) {
		*addrlen = sizeof(struct sockaddr_in);
		return -FI_ETOOSMALL;
	}

	sock_ep = container_of(fid, struct sock_ep, ep.fid);
	*addrlen = MIN(*addrlen, sizeof(struct sockaddr_in));
	memcpy(addr, sock_ep->src_addr, *addrlen);
	return 0;
}

struct fi_ops_cm sock_ep_cm_ops = {
	.size = sizeof(struct fi_ops_cm),
	.getname = sock_ep_cm_getname,
	.getpeer = fi_no_getpeer,
	.connect = fi_no_connect,
	.listen = fi_no_listen,
	.accept = fi_no_accept,
	.reject = fi_no_reject,
	.shutdown = fi_no_shutdown,
	.join = fi_no_join,
	.leave = fi_no_leave,
};

int sock_stx_ctx(struct fid_domain *domain,
		 struct fi_tx_attr *attr, struct fid_stx **stx, void *context)
{
	struct sock_domain *dom;
	struct sock_tx_ctx *tx_ctx;

	dom = container_of(domain, struct sock_domain, dom_fid);
	
	tx_ctx = sock_tx_ctx_alloc(attr, context);
	if (!tx_ctx)
		return -FI_ENOMEM;

	tx_ctx->domain = dom;
	tx_ctx->stx.ops = sock_ep_ops;
	atomic_inc(&dom->ref);

	*stx = &tx_ctx->stx;
	return 0;
}

int sock_srx_ctx(struct fid_domain *domain,
		 struct fi_rx_attr *attr, struct fid_ep **srx, void *context)
{
	struct sock_domain *dom;
	struct sock_rx_ctx *rx_ctx;

	dom = container_of(domain, struct sock_domain, dom_fid);
	rx_ctx = sock_rx_ctx_alloc(attr, context);
	if (!rx_ctx)
		return -FI_ENOMEM;

	rx_ctx->domain = dom;
	rx_ctx->ctx.fid.fclass = FI_CLASS_SRX_CTX;
	
	rx_ctx->ctx.ops = &sock_ctx_ep_ops;
	rx_ctx->ctx.msg = &sock_ep_msg_ops;
	rx_ctx->ctx.tagged = &sock_ep_tagged;
	
	/* default config */
	rx_ctx->min_multi_recv = SOCK_EP_MIN_MULTI_RECV;
	
	*srx = &rx_ctx->ctx;
	atomic_inc(&dom->ref);
	return 0;
}

struct fi_info *sock_fi_info(enum fi_ep_type ep_type, 
			     struct fi_info *hints, void *src_addr, void *dest_addr)
{
	struct fi_info *_info = fi_allocinfo_internal();
	if (!_info)
		return NULL;
	
	_info->src_addr = calloc(1, sizeof(struct sockaddr_in));
	_info->dest_addr = calloc(1, sizeof(struct sockaddr_in));
	
	_info->ep_type = ep_type;
	_info->mode = SOCK_MODE;
	_info->addr_format = FI_SOCKADDR_IN;
	_info->dest_addrlen =_info->src_addrlen = sizeof(struct sockaddr_in);

	if (src_addr) {
		memcpy(_info->src_addr, src_addr, sizeof(struct sockaddr_in));
	}
	
	if (dest_addr) {
		memcpy(_info->dest_addr, dest_addr, sizeof(struct sockaddr_in));
	}

	if (hints->caps) 
		_info->caps = hints->caps;
		
	if (hints->ep_attr)
		*(_info->ep_attr) = *hints->ep_attr;
	
	if (hints->tx_attr)
		*(_info->tx_attr) = *hints->tx_attr;

	if (hints->rx_attr)
		*(_info->rx_attr) = *hints->rx_attr;

	*(_info->domain_attr) = hints->domain_attr ? *hints->domain_attr : 
		sock_domain_attr;
	*(_info->fabric_attr) = hints->fabric_attr ? *hints->fabric_attr : 
		sock_fabric_attr;

	_info->domain_attr->name = strdup(sock_dom_name);
	_info->fabric_attr->name = strdup(sock_fab_name);
	_info->fabric_attr->prov_name = strdup(sock_fab_name);

	return _info;
}

int sock_alloc_endpoint(struct fid_domain *domain, struct fi_info *info,
		  struct sock_ep **ep, void *context, size_t fclass)
{
	int ret;
	struct sock_ep *sock_ep;
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;
	struct sock_domain *sock_dom;
	
	if (info) {
		ret = sock_verify_info(info);
		if (ret) {
			SOCK_LOG_INFO("Cannot support requested options!\n");
			return -FI_EINVAL;
		}
	}

	if (domain)
		sock_dom = container_of(domain, struct sock_domain, dom_fid);
	else
		sock_dom = NULL;
	
	sock_ep = (struct sock_ep*)calloc(1, sizeof(*sock_ep));
	if (!sock_ep)
		return -FI_ENOMEM;

	atomic_init(&sock_ep->ref, 0);

	switch (fclass) {
	case FI_CLASS_EP:
		sock_ep->ep.fid.fclass = FI_CLASS_EP;
		sock_ep->ep.fid.context = context;	
		sock_ep->ep.fid.ops = &sock_ep_fi_ops;
		
		sock_ep->ep.ops = &sock_ep_ops;
		sock_ep->ep.cm = &sock_ep_cm_ops;
		sock_ep->ep.msg = &sock_ep_msg_ops;
		sock_ep->ep.rma = &sock_ep_rma;
		sock_ep->ep.tagged = &sock_ep_tagged;
		sock_ep->ep.atomic = &sock_ep_atomic;
		break;

	case FI_CLASS_SEP:
		sock_ep->sep.fid.fclass = FI_CLASS_SEP;
		sock_ep->sep.fid.context = context;	
		sock_ep->sep.fid.ops = &sock_ep_fi_ops;
		
		sock_ep->sep.ops = &sock_ep_ops;
		sock_ep->sep.cm = &sock_ep_cm_ops;
		break;

	case FI_CLASS_PEP:
		sock_ep->pep.fid.fclass = FI_CLASS_SEP;
		sock_ep->pep.fid.context = context;	
		sock_ep->pep.fid.ops = &sock_ep_fi_ops;
		
		sock_ep->pep.ops = &sock_ep_ops;
		sock_ep->pep.cm = &sock_ep_cm_ops;
		break;
		
	default:
		goto err;
	}

	sock_ep->fclass = fclass;
	*ep = sock_ep;	

	if (info) {
		sock_ep->info.caps = info->caps;
		sock_ep->info.addr_format = FI_SOCKADDR_IN;
		
		if (info->src_addr) {
			sock_ep->src_addr = calloc(1, sizeof(struct sockaddr_in));
			memcpy(sock_ep->src_addr, info->src_addr, 
			       sizeof(struct sockaddr_in));
		}
		
		if (info->dest_addr) {
			sock_ep->dest_addr = calloc(1, sizeof(struct sockaddr_in));
			memcpy(sock_ep->dest_addr, info->dest_addr, 
			       sizeof(struct sockaddr_in));
		}
		
		if (info->ep_attr) 
			sock_ep->ep_attr = *info->ep_attr;
		
		if (info->tx_attr) {
			sock_ep->tx_attr = *info->tx_attr;
			sock_ep->op_flags = info->tx_attr->op_flags;
			sock_ep->tx_attr.size = sock_ep->tx_attr.size ?
			  sock_ep->tx_attr.size : SOCK_EP_MAX_TX_CTX_SZ;
		}
		
		if (info->rx_attr) {
			sock_ep->rx_attr = *info->rx_attr;
			sock_ep->op_flags |= info->rx_attr->op_flags;
		}
	}
	
	atomic_init(&sock_ep->ref, 0);
	atomic_init(&sock_ep->num_tx_ctx, 0);
	atomic_init(&sock_ep->num_rx_ctx, 0);

	if (sock_ep->fclass != FI_CLASS_SEP) {
		sock_ep->ep_attr.tx_ctx_cnt = 0;
		sock_ep->ep_attr.rx_ctx_cnt = 0;
	}

	if (sock_ep->ep_attr.tx_ctx_cnt != FI_SHARED_CONTEXT) {
		sock_ep->tx_array = calloc(sock_ep->ep_attr.tx_ctx_cnt + 1, 
					   sizeof(struct sock_tx_ctx *));

		/* default tx ctx */
		tx_ctx = sock_tx_ctx_alloc(&sock_ep->tx_attr, context);
		tx_ctx->ep = sock_ep;
		tx_ctx->domain = sock_dom;
		tx_ctx->tx_id = sock_ep->ep_attr.tx_ctx_cnt;
		dlist_insert_tail(&sock_ep->tx_ctx_entry, &tx_ctx->ep_list);
		sock_ep->tx_array[sock_ep->ep_attr.tx_ctx_cnt] = tx_ctx;
		sock_ep->tx_ctx = tx_ctx;
	}
	
	if (sock_ep->ep_attr.rx_ctx_cnt != FI_SHARED_CONTEXT) {
		sock_ep->rx_array = calloc(sock_ep->ep_attr.rx_ctx_cnt + 1,
					   sizeof(struct sock_rx_ctx *));
		
		/* default rx_ctx */
		rx_ctx = sock_rx_ctx_alloc(&sock_ep->rx_attr, context);
		rx_ctx->ep = sock_ep;
		rx_ctx->domain = sock_dom;
		rx_ctx->rx_id = sock_ep->ep_attr.rx_ctx_cnt;
		dlist_insert_tail(&sock_ep->rx_ctx_entry, &rx_ctx->ep_list);
		sock_ep->rx_array[sock_ep->ep_attr.rx_ctx_cnt] = rx_ctx;
		sock_ep->rx_ctx = rx_ctx;
	}
	
	/* default config */
	sock_ep->min_multi_recv = SOCK_EP_MIN_MULTI_RECV;
	
  	sock_ep->domain = sock_dom;
	atomic_inc(&sock_dom->ref);
	return 0;
	
err:
	free(sock_ep);
	return -FI_EAVAIL;
}
