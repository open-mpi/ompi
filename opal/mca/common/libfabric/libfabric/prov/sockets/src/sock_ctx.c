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


struct sock_rx_ctx *sock_rx_ctx_alloc(struct fi_rx_attr *attr, void *context)
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
	rx_ctx->attr = *attr;
	return rx_ctx;
}

void sock_rx_ctx_free(struct sock_rx_ctx *rx_ctx)
{
	fastlock_destroy(&rx_ctx->lock);
	free(rx_ctx);
}

static struct sock_tx_ctx *sock_tx_context_alloc(struct fi_tx_attr *attr, 
					     void *context, size_t fclass)
{
	struct sock_tx_ctx *tx_ctx;

	tx_ctx = calloc(sizeof(*tx_ctx), 1);
	if (!tx_ctx)
		return NULL;

	if (rbfdinit(&tx_ctx->rbfd, attr->size))
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
		tx_ctx->ctx.fid.fclass = FI_CLASS_TX_CTX;
		tx_ctx->ctx.fid.context = context;
		break;
	case FI_CLASS_STX_CTX:
		tx_ctx->stx.fid.fclass = FI_CLASS_TX_CTX;
		tx_ctx->stx.fid.context = context;
		break;
	default:
		goto err;
	}
	tx_ctx->attr = *attr;		
	return tx_ctx;

err:
	free(tx_ctx);
	return NULL;
}


struct sock_tx_ctx *sock_tx_ctx_alloc(struct fi_tx_attr *attr, void *context)
{
	return sock_tx_context_alloc(attr, context, FI_CLASS_TX_CTX);
}

struct sock_tx_ctx *sock_stx_ctx_alloc(struct fi_tx_attr *attr, void *context)
{
	return sock_tx_context_alloc(attr, context, FI_CLASS_STX_CTX);
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
	fastlock_release(&tx_ctx->rlock);
}

