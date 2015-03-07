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
#ifndef _USDF_CM_H_
#define _USDF_CM_H_

#include <sys/queue.h>

#define USDF_MAX_CONN_DATA 256

struct usdf_connreq_msg {
	uint32_t creq_peer_id;
	uint32_t creq_ipaddr;
	uint32_t creq_port;
	uint8_t creq_mac[ETH_ALEN];
	uint8_t pad[8 - ETH_ALEN];
	uint32_t creq_result;
	uint32_t creq_reason;
	uint32_t creq_datalen;
	uint8_t creq_data[0];
} __attribute__((packed));

struct usdf_connreq {
	int cr_sockfd;
	struct usdf_pep *cr_pep;
	struct usdf_ep *cr_ep;
	TAILQ_ENTRY(usdf_connreq) cr_link;

	struct usdf_poll_item cr_pollitem;

	uint8_t *cr_ptr;
	size_t cr_resid;

	size_t cr_datalen;
	uint8_t cr_data[0];
};

void usdf_cm_msg_connreq_failed(struct usdf_connreq *crp, int error);

int usdf_cm_rdm_getname(fid_t fid, void *addr, size_t *addrlen);

#endif /* _USDF_CM_H_ */
