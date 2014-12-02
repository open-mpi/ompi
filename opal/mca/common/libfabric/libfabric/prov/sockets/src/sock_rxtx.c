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


struct sock_rxtx *sock_rxtx_alloc(size_t size)
{
	struct sock_rxtx *rxtx;

	rxtx = calloc(sizeof(*rxtx), 1);
	if (!rxtx)
		return NULL;

	if (rbfdinit(&rxtx->rbfd, size))
		goto err;

	fastlock_init(&rxtx->rlock);
	fastlock_init(&rxtx->wlock);
	return rxtx;
err:
	free(rxtx);
	return NULL;
}

void sock_rxtx_free(struct sock_rxtx *rxtx)
{
	fastlock_destroy(&rxtx->rlock);
	fastlock_destroy(&rxtx->wlock);
	rbfdfree(&rxtx->rbfd);
	free(rxtx);
}

void sock_rxtx_start(struct sock_rxtx *rxtx)
{
	fastlock_acquire(&rxtx->wlock);
}

int sock_rxtx_write(struct sock_rxtx *rxtx, const void *buf, size_t len)
{
	if (rbfdavail(&rxtx->rbfd) < len)
		return -FI_EAGAIN;

	rbfdwrite(&rxtx->rbfd, buf, len);
	return 0;
}

void sock_rxtx_commit(struct sock_rxtx *rxtx)
{
	rbfdcommit(&rxtx->rbfd);
	fastlock_release(&rxtx->rlock);
}

void sock_rxtx_abort(struct sock_rxtx *rxtx)
{
	rbfdabort(&rxtx->rbfd);
	fastlock_release(&rxtx->rlock);
}

int sock_rxtx_read(struct sock_rxtx *rxtx, void *buf, size_t len)
{
	int ret;

	fastlock_acquire(&rxtx->rlock);
	if (rbfdused(&rxtx->rbfd) >= len) {
		rbfdread(&rxtx->rbfd, buf, len);
		ret = 0;
	} else {
		ret = -FI_EAGAIN;
	}
	fastlock_release(&rxtx->rlock);

	return ret;
}

