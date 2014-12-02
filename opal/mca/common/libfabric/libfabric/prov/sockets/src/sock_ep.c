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

int _sock_verify_ep_attr(struct fi_ep_attr *attr)
{
	switch (attr->protocol) {
	case FI_PROTO_UNSPEC:
	case FI_PROTO_SOCK_RDS:
		break;
	default:
		return -FI_ENODATA;
	}

	if(attr->max_msg_size > SOCK_EP_MAX_MSG_SZ)
		return -FI_ENODATA;

	if(attr->inject_size > SOCK_EP_MAX_INJECT_SZ)
		return -FI_ENODATA;

	if(attr->total_buffered_recv > SOCK_EP_MAX_BUFF_RECV)
		return -FI_ENODATA;

	return 0;
}

static ssize_t sock_ep_cancel(fid_t fid, void *context)
{
	return -FI_ENOSYS;
}

static int sock_ep_getopt(fid_t fid, int level, int optname,
			void *optval, size_t *optlen)
{
	return -FI_ENOSYS;
}

static int sock_ep_setopt(fid_t fid, int level, int optname,
			const void *optval, size_t optlen)
{
	return -FI_ENOSYS;
}

static int sock_ep_enable(struct fid_ep *ep)
{
	return -FI_ENOSYS;
}

struct fi_ops_ep sock_ep_ops = {
	.size = sizeof(struct fi_ops_ep),
	.cancel = sock_ep_cancel,
	.getopt = sock_ep_getopt,
	.setopt = sock_ep_setopt,
	.enable = sock_ep_enable,
};

int sock_ep_connect(struct fid_ep *ep, const void *addr,
			const void *param, size_t paramlen)
{
	int ret;
	struct sock_ep *sock_ep;

	sock_ep = container_of(ep, struct sock_ep, ep);
	if(!sock_ep)
		return -FI_EINVAL;
	
	if(sock_ep->connected)
		return 0;

	ret = connect(sock_ep->sock_fd, (struct sockaddr *)addr, 
		      sizeof(struct sockaddr));
	if(ret)
		return -errno;
	sock_ep->connected = 1;

	/* TODO: event */
	return 0;
}

int sock_ep_listen(struct fid_pep *pep)
{
	int ret;
	struct sock_pep *sock_pep;

	sock_pep = container_of(pep, struct sock_pep, pep);
	ret = listen(sock_pep->sock_fd, SOCK_EP_BACKLOG);
	if(ret)
		return -errno;
	return 0;
}

int sock_ep_accept(struct fid_ep *ep, fi_connreq_t connreq,
			const void *param, size_t paramlen)
{
	return -FI_ENOSYS;
}
	
int sock_ep_reject(struct fid_pep *pep, fi_connreq_t connreq,
			const void *param, size_t paramlen)
{
	return -FI_ENOSYS;
}

int sock_ep_shutdown(struct fid_ep *ep, uint64_t flags)
{
	return -FI_ENOSYS;
}
	
int sock_ep_join(struct fid_ep *ep, void *addr, fi_addr_t *fi_addr,
			uint64_t flags, void *context)
{
	return -FI_ENOSYS;
}
       
int sock_ep_leave(struct fid_ep *ep, void *addr, fi_addr_t fi_addr,
			uint64_t flags)
{
	return -FI_ENOSYS;
}

struct fi_ops_cm sock_cm_ops = {
	.size = sizeof(struct fi_ops_cm),
	.getname = NULL,
	.getpeer = NULL,
	.connect = NULL,
	.listen = NULL,
	.accept = NULL,
	.reject = NULL,
	.shutdown = NULL,
	.join = NULL,
	.leave= NULL,
};

ssize_t sock_ep_recv(struct fid_ep *ep, void *buf, size_t len, void *desc,
			void *context)
{
/*
	struct sock_ep *sock_ep;
	recv_buf_t *list_entry;
	sock_ep = container_of(ep, struct _struct sock_ep, ep);
	
	if(NULL == (list_entry = get_from_free_recv_list(sock_ep)))
		return -FI_ENOMEM;
	
	list_entry->buf = buf;
	list_entry->buf_len = len;

	enqueue_post_recv_list(sock_ep,  list_entry);
*/
	return 0;
}

ssize_t sock_ep_recvv(struct fid_ep *ep, const struct iovec *iov, void **desc,
			size_t count, void *context)
{
	return 0;
}

ssize_t sock_ep_recvfrom(struct fid_ep *ep, void *buf, size_t len, void *desc,
			fi_addr_t src_addr, void *context)
{
	return 0;
}

ssize_t sock_ep_recvmsg(struct fid_ep *ep, const struct fi_msg *msg,
			uint64_t flags)
{
	return 0;
}

ssize_t sock_ep_send(struct fid_ep *ep, const void *buf, size_t len, void *desc,
			void *context)
{
	return 0;
}

ssize_t sock_ep_sendv(struct fid_ep *ep, const struct iovec *iov, void **desc,
			size_t count, void *context)
{
	return 0;
}

ssize_t sock_ep_sendto(struct fid_ep *ep, const void *buf, size_t len, void *desc,
			fi_addr_t dest_addr, void *context)
{
	return 0;
}

ssize_t sock_ep_sendmsg(struct fid_ep *ep, const struct fi_msg *msg,
			uint64_t flags)
{
	return 0;
}

ssize_t sock_ep_inject(struct fid_ep *ep, const void *buf, size_t len)
{
	return 0;
}

ssize_t sock_ep_injectto(struct fid_ep *ep, const void *buf, size_t len,
			  fi_addr_t dest_addr)
{
	return 0;
}

ssize_t sock_ep_senddata(struct fid_ep *ep, const void *buf, size_t len, void *desc,
			uint64_t data, void *context)
{
	return 0;
}

ssize_t sock_ep_senddatato(struct fid_ep *ep, const void *buf, size_t len, 
				  void *desc, uint64_t data, fi_addr_t dest_addr, void *context)
{
	return 0;
}

int sock_pendpoint(struct fid_fabric *fabric, struct fi_info *info,
			struct fid_pep **pep, void *context)
{
	return -FI_ENOSYS;
}

struct fi_ops_msg sock_msg_ops = {
		.size = sizeof(struct fi_ops_msg),
		.recv = NULL,
		.recvv = NULL,
		.recvfrom = NULL,
		.recvmsg = NULL,
		.send = NULL,
		.sendv = NULL,
		.sendto = NULL,
		.sendmsg = NULL,
		.inject = NULL,
		.injectto = NULL,
		.senddata = NULL,
		.senddatato = NULL,
};

