/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef _USDF_DGRAM_H_
#define _USDF_DGRAM_H_

/* fi_ops_cm for DGRAM */
int usdf_cm_dgram_connect(struct fid_ep *ep, const void *addr,
	const void *param, size_t paramlen);
int usdf_cm_dgram_shutdown(struct fid_ep *ep, uint64_t flags);

/* fi_ops_msg for DGRAM */
ssize_t usdf_dgram_recv(struct fid_ep *ep, void *buf, size_t len, void *desc,
	fi_addr_t src_addr, void *context);
ssize_t usdf_dgram_recvv(struct fid_ep *ep, const struct iovec *iov,
	void **desc, size_t count, fi_addr_t src_addr, void *context);
ssize_t usdf_dgram_recvmsg(struct fid_ep *ep, const struct fi_msg *msg,
	uint64_t flags);
ssize_t usdf_dgram_send(struct fid_ep *ep, const void *buf, size_t len,
	void *desc, fi_addr_t dest_addr, void *context);
ssize_t usdf_dgram_conn_send(struct fid_ep *ep, const void *buf, size_t len,
	void *desc, fi_addr_t dest_addr, void *context);
ssize_t usdf_dgram_sendv(struct fid_ep *ep, const struct iovec *iov,
	void **desc, size_t count, fi_addr_t dest_addr, void *context);
ssize_t usdf_dgram_sendmsg(struct fid_ep *ep, const struct fi_msg *msg,
	uint64_t flags);
ssize_t usdf_dgram_inject(struct fid_ep *ep, const void *buf, size_t len,
	fi_addr_t dest_addr);
ssize_t usdf_dgram_senddata(struct fid_ep *ep, const void *buf, size_t len,
	void *desc, uint64_t data, fi_addr_t dest_addr, void *context);

ssize_t usdf_dgram_prefix_recv(struct fid_ep *ep, void *buf, size_t len,
	void *desc, fi_addr_t src_addr, void *context);
ssize_t usdf_dgram_prefix_recvv(struct fid_ep *ep, const struct iovec *iov,
	void **desc, size_t count, fi_addr_t src_addr, void *context);
ssize_t usdf_dgram_prefix_send(struct fid_ep *ep, const void *buf, size_t len,
	void *desc, fi_addr_t dest_addr, void *context);

#endif /* _USDF_DGRAM_H_ */
