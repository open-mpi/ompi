/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* This code is derived from similar code in libusnic_verbs.  Consider porting
 * any future bug fixes from that code to this code.  The original version of
 * this code was written by Xuyang Wang @ Cisco.
 */

#include "ompi_config.h"

#include <errno.h>
#include <arpa/inet.h>
#include <time.h>
#include <net/if.h>
#include <netlink/netlink.h>
#include <netlink/addr.h>
#include <netlink/route/rtnl.h>
#include <netlink/route/link.h>
#include <netlink/route/addr.h>
#include <netlink/route/neighbour.h>
#include <netlink/route/neightbl.h>
#include <netlink/route/route.h>

#include "btl_usnic.h"
#include "btl_usnic_compat.h"

#include "btl_usnic_libnl_utils.h"


/* minimize divergence from the libusnic_verbs version of this code */
#define WANT_DEBUG_MSGS 0
#define usnic_err(...) opal_output_verbose(20, USNIC_OUT, __VA_ARGS__)
#define usnic_rtnl_sk_alloc ompi_btl_usnic_rtnl_sk_alloc
#define usnic_rtnl_sk_free ompi_btl_usnic_rtnl_sk_free

struct usnic_rtnl_sk {
	struct nl_handle *nlh;
	uint32_t	seq;
};

struct nl_lookup_arg {
	uint32_t	nh_addr;
	int		oif;
	int		found;
	int		replied;
	int		msg_count;
	int		metric;
	struct usnic_rtnl_sk	*unlsk;
};

static struct nla_policy route_policy[RTA_MAX+1];

/* *sigh* use a helper routine to initialize this structure array b/c OMPI v1.6
 * demands C89, which does not support the designated initializers used in the
 * original code */
static void init_route_policy(struct nla_policy *policy)
{
    policy[RTA_IIF].type = NLA_STRING;
    policy[RTA_IIF].maxlen = IFNAMSIZ;
    policy[RTA_OIF].type = NLA_U32;
    policy[RTA_PRIORITY].type = NLA_U32;
    policy[RTA_FLOW].type = NLA_U32;
    policy[RTA_MP_ALGO].type = NLA_U32;
    policy[RTA_CACHEINFO].minlen = sizeof(struct rta_cacheinfo);
    policy[RTA_METRICS].type = NLA_NESTED;
    policy[RTA_MULTIPATH].type = NLA_NESTED;
}

static int rtnl_raw_parse_cb(struct nl_msg *msg, void *arg)
{
	struct nl_lookup_arg *lookup_arg = (struct nl_lookup_arg *)arg;
	struct usnic_rtnl_sk *unlsk = lookup_arg->unlsk;
	struct nlmsghdr	*nlm_hdr = nlmsg_hdr(msg);
	struct rtmsg *rtm;
	struct nlattr *tb[RTA_MAX + 1];
	int found = 0;
	int err;

#if WANT_DEBUG_MSGS
	nl_msg_dump(msg, stderr);
#endif /* WANT_DEBUG_MSGS */

	lookup_arg->nh_addr	= 0;
	lookup_arg->found 	= 0;
	lookup_arg->replied	= 0;
	lookup_arg->msg_count++;

	if (nlm_hdr->nlmsg_pid != nl_socket_get_local_port(unlsk->nlh)
		|| nlm_hdr->nlmsg_seq != unlsk->seq) {
		usnic_err("Not an expected reply msg pid: %u local pid: %u "
				"msg seq: %u expected seq: %u\n",
				nlm_hdr->nlmsg_pid, nl_socket_get_local_port(unlsk->nlh),
				nlm_hdr->nlmsg_seq, unlsk->seq);
		return NL_SKIP;
	}
	lookup_arg->replied = 1;

	if (nlm_hdr->nlmsg_type == NLMSG_ERROR) {
		struct nlmsgerr *e = (struct nlmsgerr *)nlmsg_data(nlm_hdr);
		if (nlm_hdr->nlmsg_len >= (__u32)nlmsg_msg_size(sizeof(*e))) {
			usnic_err("Received a netlink error message %d\n",
					e->error);
		}
		else {
			usnic_err("Received a truncated netlink error message\n");
		}
		return NL_STOP;
	}

	if (nlm_hdr->nlmsg_type != RTM_NEWROUTE) {
		usnic_err("Received an invalid route request reply message\n");
		return NL_STOP;
	}

	rtm = nlmsg_data(nlm_hdr);
	if (rtm->rtm_family != AF_INET) {
		usnic_err("RTM message contains invalid AF family\n");
		return NL_STOP;
	}

        init_route_policy(route_policy);
	err = nlmsg_parse(nlm_hdr, sizeof(struct rtmsg), tb, RTA_MAX,
			  route_policy);
	if (err < 0) {
		usnic_err("nlmsg parse error %d\n", err);
		return NL_STOP;
	}

	if (tb[RTA_OIF]) {
		if (nla_get_u32(tb[RTA_OIF]) == (uint32_t)lookup_arg->oif)
			found = 1;
		else
			usnic_err("Retrieved route has a different outgoing interface %d (expected %d)\n",
					nla_get_u32(tb[RTA_OIF]),
					lookup_arg->oif);
	}

        if (found && tb[RTA_METRICS]) {
            lookup_arg->metric = (int)nla_get_u32(tb[RTA_METRICS]);
        }

	if (found && tb[RTA_GATEWAY])
		lookup_arg->nh_addr = nla_get_u32(tb[RTA_GATEWAY]);

	lookup_arg->found = found;
	return NL_STOP;
}

static int rtnl_send_ack_disable(struct usnic_rtnl_sk *unlsk, struct nl_msg *msg)
{
	struct nlmsghdr *nlhdr;

	nlhdr = nlmsg_hdr(msg);
	nlhdr->nlmsg_pid = nl_socket_get_local_port(unlsk->nlh);
	nlhdr->nlmsg_seq = ++unlsk->seq;
	nlmsg_set_proto(msg, NETLINK_ROUTE);

	nlhdr->nlmsg_flags |= NLM_F_REQUEST;

	return nl_send(unlsk->nlh, msg);
}

static int nl_set_recv_timeout(struct nl_handle *handle)
{
	int err = 0;
	struct timeval timeout;

	timeout.tv_sec = 1;
	timeout.tv_usec = 0;

	err = setsockopt(nl_socket_get_fd(handle), SOL_SOCKET, SO_RCVTIMEO,
				(char *)&timeout, sizeof(timeout));
	if (err < 0)
		usnic_err("Failed to set SO_RCVTIMEO socket option for nl socket, err %d\n",
			err);

	return err;
}

int ompi_btl_usnic_nl_ip_rt_lookup(struct usnic_rtnl_sk *unlsk,
                                   const char *src_ifname,
                                   uint32_t src_addr,
                                   uint32_t dst_addr, int *metric)
{
	struct nl_msg *nlm;
	struct rtmsg rmsg;
	struct nl_lookup_arg arg;
	int	msg_cnt;
	int err;
	int oif;

	oif = if_nametoindex(src_ifname);
	if (0 == oif) {
	    return errno;
	}

	arg.nh_addr 	= 0;
	arg.oif		= oif;
	arg.found	= 0;
	arg.replied	= 0;
	arg.unlsk	= unlsk;
	arg.msg_count = msg_cnt = 0;

	memset(&rmsg, 0, sizeof(rmsg));
	rmsg.rtm_family = AF_INET;
	rmsg.rtm_dst_len = sizeof(dst_addr)*8;
	rmsg.rtm_src_len = sizeof(src_addr)*8;

	nlm = nlmsg_alloc_simple(RTM_GETROUTE, 0);
	nlmsg_append(nlm, &rmsg, sizeof(rmsg), NLMSG_ALIGNTO);
	nla_put_u32(nlm, RTA_DST, dst_addr);
	nla_put_u32(nlm, RTA_SRC, src_addr);

	err = rtnl_send_ack_disable(unlsk, nlm);
	nlmsg_free(nlm);
	if (err < 0) {
		usnic_err("Failed to send nl route message to kernel, "
			"error %s\n", nl_geterror());
		return err;
	}

	err = nl_socket_modify_cb(unlsk->nlh, NL_CB_MSG_IN, NL_CB_CUSTOM,
					rtnl_raw_parse_cb, &arg);
	if (err != 0) {
		usnic_err("Failed to setup callback function, error %s\n", nl_geterror());
		return err;
	}

	while (!arg.replied) {
		err = nl_recvmsgs_default(unlsk->nlh);
		if (err < 0) {
			usnic_err("Failed to receive nl route message from "
				"kernel, error %s\n", nl_geterror());
			return err;
		}

		/*
		 * the return value of nl_recvmsgs_default does not tell
		 * whether it returns because of successful read or socket
		 * timeout. So we compare msg count before and after the call
		 * to decide if no new message arrives. In such case,
		 * this function needs to terminate to prevent the caller from
		 * blocking forever
		 * NL_CB_MSG_IN traps every received message, so
		 * there should be no premature exit
		 */
		if (msg_cnt != arg.msg_count)
			msg_cnt = arg.msg_count;
		else
			break;
	}

	if (arg.found) {
                if (metric != NULL) {
                    *metric = arg.metric;
                }
		return 0;
	}
	else {
		return -1;
	}
}

int usnic_rtnl_sk_alloc(struct usnic_rtnl_sk **p_sk)
{
	struct usnic_rtnl_sk *unlsk;
	struct nl_handle *nlh;
	int err;

	unlsk = calloc(1, sizeof(*unlsk));
	if (!unlsk) {
		usnic_err("Failed to allocate usnic_rtnl_sk struct\n");
		return -ENOMEM;
	}

	nlh =  nl_handle_alloc();
	if (!nlh) {
		usnic_err("Failed to allocate nl handle\n");
		err = -ENOMEM;
		goto err_free_unlsk;
	}

	err = nl_connect(nlh, NETLINK_ROUTE);
	if (err < 0) {
		usnic_err("Failed to connnect netlink route socket\n");
		goto err_free_nlh;
	}

	nl_disable_sequence_check(nlh);
	err = nl_set_recv_timeout(nlh);
	if (err < 0)
		goto err_close_nlh;

	unlsk->nlh = nlh;
	unlsk->seq = time(NULL);
	*p_sk = unlsk;
	return 0;

err_close_nlh:
	nl_close(nlh);
err_free_nlh:
	nl_handle_destroy(nlh);
err_free_unlsk:
	free(unlsk);
	return err;
}

void usnic_rtnl_sk_free(struct usnic_rtnl_sk* u_nlsk)
{
    if (u_nlsk != NULL) {
	nl_close(u_nlsk->nlh);
	nl_handle_destroy(u_nlsk->nlh);
	free(u_nlsk);
    }
}
