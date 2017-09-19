/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2017      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 *
 * Portions of this software copied from libfabric
 * (https://github.com/ofiwg/libfabric)
 *
 * LICENSE_BEGIN
 *
 * BSD license:
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
 */

#ifndef LIBNL3_UTILS_H
#define LIBNL3_UTILS_H

#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#include <netlink/netlink.h>
#include <netlink/route/rtnl.h>
#include <netlink/route/route.h>

typedef struct nl_sock NL_HANDLE;

#define NLMSG_SIZE(size) nlmsg_size(size)
#define NL_GETERROR(err) nl_geterror(err)
#define NL_HANDLE_ALLOC nl_socket_alloc
#define NL_HANDLE_FREE nl_socket_free
#define NL_DISABLE_SEQ_CHECK nl_socket_disable_seq_check
#define INC_CB_MSGCNT(arg)

/* err will be returned as -NLE_AGAIN */
/* if the socket times out */
#define NL_RECVMSGS(nlh, cb_arg, rc, err, out) \
	do { \
		err = nl_recvmsgs_default(nlh); \
		if (err < 0) { \
                    opal_output(0, "Failed to receive netlink reply message, error %s\n", \
				NL_GETERROR(err)); \
			if (err == -NLE_AGAIN) \
				err = rc; \
			goto out; \
		} \
	} while (0)

struct opal_reachable_netlink_rt_cb_arg {
	int			oif;
	int			found;
	int			has_gateway;
	int			replied;
	struct opal_reachable_netlink_sk	*unlsk;
};

#endif /* LIBNL3_UTILS_H */
