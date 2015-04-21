/*
 * Copyright (c) 2014 Intel Corporation, Inc.  All rights reserved.
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <errno.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "sock.h"
#include "sock_util.h"

#define SOCK_LOG_INFO(...) _SOCK_LOG_INFO(FI_LOG_EP_DATA, __VA_ARGS__)
#define SOCK_LOG_ERROR(...) _SOCK_LOG_ERROR(FI_LOG_EP_DATA, __VA_ARGS__)

/* FIXME: pool of rx_entry */
struct sock_rx_entry *sock_rx_new_entry(struct sock_rx_ctx *rx_ctx)
{
	struct sock_rx_entry *rx_entry;

	rx_entry = calloc(1, sizeof(*rx_entry));
	if (!rx_entry)
		return NULL;
	
	rx_entry->is_tagged = 0;
	SOCK_LOG_INFO("New rx_entry: %p, ctx: %p\n", rx_entry, rx_ctx);
	dlist_init(&rx_entry->entry);

	fastlock_acquire(&rx_ctx->lock);
	rx_ctx->num_left--;
	fastlock_release(&rx_ctx->lock);
	
	return rx_entry;
}

void sock_rx_release_entry(struct sock_rx_entry *rx_entry)
{
	SOCK_LOG_INFO("Releasing rx_entry: %p\n", rx_entry);
	free(rx_entry);
}

struct sock_rx_entry *sock_rx_new_buffered_entry(struct sock_rx_ctx *rx_ctx,
						 size_t len)
{
	struct sock_rx_entry *rx_entry;

	if (rx_ctx->buffered_len + len >= rx_ctx->attr.total_buffered_recv) {
		SOCK_LOG_ERROR("Reached max buffered recv limit\n");
		return NULL;
	}

	rx_entry = calloc(1, sizeof(*rx_entry) + len);
	if (!rx_entry)
		return NULL;

	SOCK_LOG_INFO("New buffered entry:%p len: %lu, ctx: %p\n", 
		       rx_entry, len, rx_ctx);

	rx_entry->is_busy = 1;
	rx_entry->is_buffered = 1;
	rx_entry->rx_op.dest_iov_len = 1;
	rx_entry->iov[0].iov.len = len;
	rx_entry->iov[0].iov.addr = (uintptr_t) (rx_entry + 1);
	rx_entry->total_len = len;
	
	rx_ctx->buffered_len += len;
	dlist_insert_tail(&rx_entry->entry, &rx_ctx->rx_buffered_list);
	rx_entry->is_busy = 1;
	rx_entry->is_tagged = 0;

	return rx_entry;
}

inline size_t sock_rx_avail_len(struct sock_rx_entry *rx_entry)
{
	return rx_entry->total_len - rx_entry->used;
}

struct sock_rx_entry *sock_rx_get_entry(struct sock_rx_ctx *rx_ctx, 
					uint64_t addr, uint64_t tag, 
					uint8_t op_type)
{
	struct dlist_entry *entry;
	struct sock_rx_entry *rx_entry;

	for (entry = rx_ctx->rx_entry_list.next;
	     entry != &rx_ctx->rx_entry_list; entry = entry->next) {

		rx_entry = container_of(entry, struct sock_rx_entry, entry);
		if (rx_entry->is_busy || (op_type != rx_entry->is_tagged))
			continue;

		if (((rx_entry->tag & ~rx_entry->ignore) == (tag & ~rx_entry->ignore)) &&
		    (rx_entry->addr == FI_ADDR_UNSPEC || addr == FI_ADDR_UNSPEC || 
		     rx_entry->addr == addr ||
		     (rx_ctx->av && 
		      !sock_av_compare_addr(rx_ctx->av, addr, rx_entry->addr)))) {
			rx_entry->is_busy = 1;
			return rx_entry;
		}
	}
	return NULL;
}
