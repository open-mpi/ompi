/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
 *
 * LICENSE_BEGIN
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
 *
 * LICENSE_END
 *
 *
 */

#ifndef LIBNL1_UTILS_H
#define LIBNL1_UTILS_H

#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#include <netlink/netlink.h>
#include <netlink/route/rtnl.h>
#include <netlink/route/route.h>

typedef struct nl_handle NL_HANDLE;

#define NLMSG_SIZE(size) nlmsg_msg_size(size)
#define NL_GETERROR(err) nl_geterror()
#define NL_HANDLE_ALLOC nl_handle_alloc
#define NL_HANDLE_FREE nl_handle_destroy
#define NL_DISABLE_SEQ_CHECK nl_disable_sequence_check
#define INC_CB_MSGCNT(arg) \
	do { \
		arg->msg_cnt++; \
	} while (0)

/*
 * the return value of nl_recvmsgs_default does not tell
 * whether it returns because of successful read or socket
 * timeout. This is a limitation in libnl1. So we compare
 * message count before and after the call to decide if there
 * is no new message arriving. In this case, this function
 * needs to terminate to prevent the caller from
 * blocking forever.
 * NL_CB_MSG_IN traps every received message, so
 * there should be no premature exit
 */
#define NL_RECVMSGS(nlh, cb_arg, rc, err, out) \
	do { \
		int msg_cnt = cb_arg.msg_cnt; \
		err = nl_recvmsgs_default(nlh); \
		if (err < 0) { \
			usnic_err("Failed to receive netlink reply message, error %s\n", \
				NL_GETERROR(err)); \
			goto out; \
		} \
		if (msg_cnt == cb_arg.msg_cnt) {\
			err = rc; \
			goto out; \
		} \
	} while (0)

struct usnic_rt_cb_arg {
	uint32_t		nh_addr;
	int			oif;
	int			found;
	int			msg_cnt;
	struct usnic_nl_sk	*unlsk;
};

#endif /* LIBNL1_UTILS_H */
