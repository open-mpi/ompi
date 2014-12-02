/*
 * Copyright (c) 2014 Intel Corporation.  All rights reserved.
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
 *
 */

#if !defined(RBUF_H)
#define RBUF_H

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <fcntl.h>
#include <fi.h>


/*
 * Simple ring buffer
 */
struct ringbuf {
	size_t		size;
	size_t		size_mask;
	size_t		rcnt;
	size_t		wcnt;
	size_t		wpos;
	void		*buf;
};

static inline int rbinit(struct ringbuf *rb, size_t size)
{
	rb->size = roundup_power_of_two(size);
	rb->size_mask = rb->size - 1;
	rb->rcnt = 0;
	rb->wcnt = 0;
	rb->wpos = 0;
	rb->buf = calloc(1, rb->size);
	if (!rb->buf)
		return -ENOMEM;
	return 0;
}

static inline void rbfree(struct ringbuf *rb)
{
	free(rb->buf);
}

static inline int rbfull(struct ringbuf *rb)
{
	return rb->wcnt - rb->rcnt >= rb->size;
}

static inline int rbempty(struct ringbuf *rb)
{
	return rb->wcnt == rb->rcnt;
}

static inline size_t rbused(struct ringbuf *rb)
{
	return rb->wcnt - rb->rcnt;
}

static inline size_t rbavail(struct ringbuf *rb)
{
	return rb->size - rbused(rb);
}

static inline void rbwrite(struct ringbuf *rb, const void *buf, size_t len)
{
	size_t endlen;

	endlen = rb->size - (rb->wpos & rb->size_mask);
	if (len <= endlen) {
		memcpy(rb->buf + (rb->wpos & rb->size_mask), buf, len);
	} else {
		memcpy(rb->buf + (rb->wpos & rb->size_mask), buf, endlen);
		memcpy(rb->buf, buf, len - endlen);
	}
	rb->wpos += len;
}

static inline void rbcommit(struct ringbuf *rb)
{
	rb->wcnt = rb->wpos;
}

static inline void rbabort(struct ringbuf *rb)
{
	rb->wpos = rb->wcnt;
}

static inline void rbpeek(struct ringbuf *rb, void *buf, size_t len)
{
	size_t endlen;

	endlen = rb->size - (rb->rcnt & rb->size_mask);
	if (len <= endlen) {
		memcpy(buf, rb->buf + (rb->rcnt & rb->size_mask), len);
	} else {
		memcpy(buf, rb->buf + (rb->rcnt & rb->size_mask), endlen);
		memcpy(buf, rb->buf, len - endlen);
	}
}

static inline void rbread(struct ringbuf *rb, void *buf, size_t len)
{
	rbpeek(rb, buf, len);
	rb->rcnt += len;
}


/*
 * Ring buffer with blocking read support using an fd
 */
enum {
	RB_READ_FD,
	RB_WRITE_FD
};

struct ringbuffd {
	struct ringbuf	rb;
	int		fdrcnt;
	int		fdwcnt;
	int		fd[2];
};

static inline int rbfdinit(struct ringbuffd *rbfd, size_t size)
{
	int ret;

	rbfd->fdrcnt = 0;
	rbfd->fdwcnt = 0;
	ret = rbinit(&rbfd->rb, size);
	if (!ret)
		return ret;

	ret = socketpair(AF_UNIX, SOCK_STREAM, 0, rbfd->fd);
	if (ret < 0)
		goto err1;

	ret = fcntl(rbfd->fd[RB_READ_FD], F_SETFL, O_NONBLOCK);
	if (ret < 0)
		goto err2;

	return 0;

err2:
	close(rbfd->fd[0]);
	close(rbfd->fd[1]);
err1:
	rbfree(&rbfd->rb);
	return -errno;
}

static inline void rbfdfree(struct ringbuffd *rbfd)
{
	rbfree(&rbfd->rb);
	close(rbfd->fd[0]);
	close(rbfd->fd[1]);
}

static inline int rbfdfull(struct ringbuffd *rbfd)
{
	return rbfull(&rbfd->rb);
}

static inline int rbfdempty(struct ringbuffd *rbfd)
{
	return rbempty(&rbfd->rb);
}

static inline size_t rbfdused(struct ringbuffd *rbfd)
{
	return rbused(&rbfd->rb);
}

static inline size_t rbfdavail(struct ringbuffd *rbfd)
{
	return rbavail(&rbfd->rb);
}

static inline void rbfdsignal(struct ringbuffd *rbfd)
{
	if (rbfd->fdwcnt == rbfd->fdrcnt) {
		write(rbfd->fd[RB_WRITE_FD], rbfd, sizeof rbfd);
		rbfd->fdwcnt++;
	}
}

static inline void rbfdreset(struct ringbuffd *rbfd)
{
	void *buf;

	if (rbfdempty(rbfd) && (rbfd->fdrcnt < rbfd->fdwcnt)) {
		read(rbfd->fd[RB_READ_FD], &buf, sizeof buf);
		rbfd->fdrcnt++;
	}
}

static inline void rbfdwrite(struct ringbuffd *rbfd, const void *buf, size_t len)
{
	rbwrite(&rbfd->rb, buf, len);
}

static inline void rbfdcommit(struct ringbuffd *rbfd)
{
	rbcommit(&rbfd->rb);
	rbfdsignal(rbfd);
}

static inline void rbfdabort(struct ringbuffd *rbfd)
{
	rbabort(&rbfd->rb);
}

static inline void rbfdpeek(struct ringbuffd *rbfd, void *buf, size_t len)
{
	rbpeek(&rbfd->rb, buf, len);
}

static inline void rbfdread(struct ringbuffd *rbfd, void *buf, size_t len)
{
	rbread(&rbfd->rb, buf, len);
	rbfdreset(rbfd);
}

static inline size_t rbfdsread(struct ringbuffd *rbfd, void *buf, size_t len,
				int timeout)
{
	size_t avail;
	int ret;

	do {
		avail = rbfdused(rbfd);
		if (avail) {
			len = MIN(len, avail);
			rbfdread(rbfd, buf, len);
			return len;
		}

		ret = fi_poll_fd(rbfd->fd[RB_READ_FD], timeout);
	} while (!ret);

	return ret;
}

#endif /* RBUF_H */
