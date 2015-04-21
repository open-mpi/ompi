/*
 * Copyright (c) 2014 Intel Corporation. All rights reserved.
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

#define SOCK_LOG_INFO(...) _SOCK_LOG_INFO(FI_LOG_EP_CTRL, __VA_ARGS__)
#define SOCK_LOG_ERROR(...) _SOCK_LOG_ERROR(FI_LOG_EP_CTRL, __VA_ARGS__)

struct sock_rx_ctx *sock_rx_ctx_alloc(const struct fi_rx_attr *attr, void *context)
{
	struct sock_rx_ctx *rx_ctx;
	rx_ctx = calloc(1, sizeof(*rx_ctx));
	if (!rx_ctx)
		return NULL;

	dlist_init(&rx_ctx->cq_entry);
	dlist_init(&rx_ctx->cntr_entry);
	dlist_init(&rx_ctx->pe_entry);

	dlist_init(&rx_ctx->pe_entry_list);
	dlist_init(&rx_ctx->rx_entry_list);
	dlist_init(&rx_ctx->rx_buffered_list);
	dlist_init(&rx_ctx->ep_list);

	fastlock_init(&rx_ctx->lock);

	rx_ctx->ctx.fid.fclass = FI_CLASS_RX_CTX;
	rx_ctx->ctx.fid.context = context;
	rx_ctx->num_left = attr->size;
	rx_ctx->attr = *attr;
	return rx_ctx;
}

void sock_rx_ctx_free(struct sock_rx_ctx *rx_ctx)
{
	fastlock_destroy(&rx_ctx->lock);
	free(rx_ctx);
}

static struct sock_tx_ctx *sock_tx_context_alloc(const struct fi_tx_attr *attr, 
					     void *context, size_t fclass)
{
	struct sock_tx_ctx *tx_ctx;

	tx_ctx = calloc(sizeof(*tx_ctx), 1);
	if (!tx_ctx)
		return NULL;

	if (rbfdinit(&tx_ctx->rbfd, 
		     (attr->size) ? attr->size * SOCK_EP_TX_ENTRY_SZ: 
		     SOCK_EP_TX_SZ * SOCK_EP_TX_ENTRY_SZ))
		goto err;

	dlist_init(&tx_ctx->cq_entry);
	dlist_init(&tx_ctx->cntr_entry);
	dlist_init(&tx_ctx->pe_entry);
	
	dlist_init(&tx_ctx->pe_entry_list);
	dlist_init(&tx_ctx->ep_list);
	
	fastlock_init(&tx_ctx->rlock);
	fastlock_init(&tx_ctx->wlock);

	switch (fclass) {
	case FI_CLASS_TX_CTX:
		tx_ctx->fid.ctx.fid.fclass = FI_CLASS_TX_CTX;
		tx_ctx->fid.ctx.fid.context = context;
		break;
	case FI_CLASS_STX_CTX:
		tx_ctx->fid.stx.fid.fclass = FI_CLASS_STX_CTX;
		tx_ctx->fid.stx.fid.context = context;
		break;
	default:
		goto err;
	}
	tx_ctx->attr = *attr;		
	tx_ctx->attr.op_flags |= FI_TRANSMIT_COMPLETE;
	return tx_ctx;

err:
	free(tx_ctx);
	return NULL;
}


struct sock_tx_ctx *sock_tx_ctx_alloc(const struct fi_tx_attr *attr, void *context)
{
	return sock_tx_context_alloc(attr, context, FI_CLASS_TX_CTX);
}

void sock_tx_ctx_free(struct sock_tx_ctx *tx_ctx)
{
	fastlock_destroy(&tx_ctx->rlock);
	fastlock_destroy(&tx_ctx->wlock);
	rbfdfree(&tx_ctx->rbfd);
	free(tx_ctx);
}

void sock_tx_ctx_start(struct sock_tx_ctx *tx_ctx)
{
	fastlock_acquire(&tx_ctx->wlock);
}

void sock_tx_ctx_write(struct sock_tx_ctx *tx_ctx, const void *buf, size_t len)
{
	rbfdwrite(&tx_ctx->rbfd, buf, len);
}

void sock_tx_ctx_commit(struct sock_tx_ctx *tx_ctx)
{
	rbfdcommit(&tx_ctx->rbfd);
	fastlock_release(&tx_ctx->wlock);
}

void sock_tx_ctx_abort(struct sock_tx_ctx *tx_ctx)
{
	rbfdabort(&tx_ctx->rbfd);
	fastlock_release(&tx_ctx->wlock);
}

void sock_tx_ctx_write_op_send(struct sock_tx_ctx *tx_ctx,
		struct sock_op *op, uint64_t flags, uint64_t context,
		uint64_t dest_addr, uint64_t buf, struct sock_ep *ep,
		struct sock_conn *conn)
{
	sock_tx_ctx_write(tx_ctx, op, sizeof *op);
	sock_tx_ctx_write(tx_ctx, &flags, sizeof flags);
	sock_tx_ctx_write(tx_ctx, &context, sizeof context);
	sock_tx_ctx_write(tx_ctx, &dest_addr, sizeof dest_addr);
	sock_tx_ctx_write(tx_ctx, &buf, sizeof buf);
	sock_tx_ctx_write(tx_ctx, &ep, sizeof ep);
	sock_tx_ctx_write(tx_ctx, &conn, sizeof conn);
}

void sock_tx_ctx_write_op_tsend(struct sock_tx_ctx *tx_ctx,
		struct sock_op *op, uint64_t flags, uint64_t context,
		uint64_t dest_addr, uint64_t buf, struct sock_ep *ep,
		struct sock_conn *conn, uint64_t tag)
{
	sock_tx_ctx_write_op_send(tx_ctx, op, flags, context, dest_addr,
			buf, ep, conn);
	sock_tx_ctx_write(tx_ctx, &tag, sizeof tag);
}

void sock_tx_ctx_read_op_send(struct sock_tx_ctx *tx_ctx,
		struct sock_op *op, uint64_t *flags, uint64_t *context,
		uint64_t *dest_addr, uint64_t *buf, struct sock_ep **ep,
		struct sock_conn **conn)
{
	rbfdread(&tx_ctx->rbfd, op, sizeof *op);
	rbfdread(&tx_ctx->rbfd, flags, sizeof *flags);
	rbfdread(&tx_ctx->rbfd, context, sizeof *context);
	rbfdread(&tx_ctx->rbfd, dest_addr, sizeof *dest_addr);
	rbfdread(&tx_ctx->rbfd, buf, sizeof *buf);
	rbfdread(&tx_ctx->rbfd, ep, sizeof *ep);
	rbfdread(&tx_ctx->rbfd, conn, sizeof *conn);
}
