/*
 * Copyright (c) 2014 Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2017      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
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
 *
 *
 */

#include "opal_config.h"

#include <errno.h>
#include <arpa/inet.h>
#include <time.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "libnl_utils.h"

/* Adapt this copied code for Open MPI */
#include "opal/util/output.h"


static struct nla_policy route_policy[RTA_MAX+1] = {
	[RTA_IIF]	= { .type = NLA_STRING,
			    .maxlen = IFNAMSIZ, },
	[RTA_OIF]	= { .type = NLA_U32 },
	[RTA_PRIORITY]	= { .type = NLA_U32 },
	[RTA_FLOW]	= { .type = NLA_U32 },
	[RTA_MP_ALGO]	= { .type = NLA_U32 },
	[RTA_CACHEINFO]	= { .minlen = sizeof(struct rta_cacheinfo) },
	[RTA_METRICS]	= { .type = NLA_NESTED },
	[RTA_MULTIPATH]	= { .type = NLA_NESTED },
};

static int opal_reachable_netlink_is_nlreply_expected(struct opal_reachable_netlink_sk *unlsk,
						      struct nlmsghdr *nlm_hdr)
{
#if OPAL_ENABLE_DEBUG
    if (nlm_hdr->nlmsg_pid != nl_socket_get_local_port(unlsk->nlh)
        || nlm_hdr->nlmsg_seq != unlsk->seq) {
        opal_output(0, "Not an expected reply msg pid: %u local pid: %u msg seq: %u expected seq: %u\n",
                    nlm_hdr->nlmsg_pid,
                    nl_socket_get_local_port(unlsk->nlh),
                    nlm_hdr->nlmsg_seq, unlsk->seq);
        return 0;
    }
#endif

    return 1;
}

static int opal_reachable_netlink_is_nlreply_err(struct nlmsghdr *nlm_hdr)
{
    if (nlm_hdr->nlmsg_type == NLMSG_ERROR) {
        struct nlmsgerr *e = (struct nlmsgerr *)nlmsg_data(nlm_hdr);
        if (nlm_hdr->nlmsg_len >= (__u32)NLMSG_SIZE(sizeof(*e)))
            opal_output_verbose(20, 0,
                                "Received a netlink error message");
        else
            opal_output_verbose(20, 0,
                                "Received a truncated netlink error message\n");
        return 1;
    }

    return 0;
}

static int opal_reachable_netlink_send_query(struct opal_reachable_netlink_sk *unlsk,
					     struct nl_msg *msg,
					     int protocol, int flag)
{
    struct nlmsghdr *nlhdr;

    nlhdr = nlmsg_hdr(msg);
    nlhdr->nlmsg_pid = nl_socket_get_local_port(unlsk->nlh);
    nlhdr->nlmsg_seq = ++unlsk->seq;
    nlmsg_set_proto(msg, protocol);
    nlhdr->nlmsg_flags = flag;

    return nl_send(unlsk->nlh, msg);
}

static int opal_reachable_netlink_set_rcvsk_timer(NL_HANDLE *nlh)
{
    int err = 0;
    struct timeval timeout;

    timeout.tv_sec = 1;
    timeout.tv_usec = 0;

    err = setsockopt(nl_socket_get_fd(nlh), SOL_SOCKET, SO_RCVTIMEO,
                     (char *)&timeout, sizeof(timeout));
#if OPAL_ENABLE_DEBUG
    if (err < 0)
        opal_output(0, "Failed to set SO_RCVTIMEO for nl socket");
#endif

    return err;
}

static int opal_reachable_netlink_sk_alloc(struct opal_reachable_netlink_sk **p_sk, int protocol)
{
    struct opal_reachable_netlink_sk *unlsk;
    NL_HANDLE *nlh;
    int err;

    unlsk = calloc(1, sizeof(*unlsk));
    if (!unlsk) {
        opal_output(0, "Failed to allocate opal_reachable_netlink_sk struct\n");
        return ENOMEM;
    }

    nlh = NL_HANDLE_ALLOC();
    if (!nlh) {
        opal_output(0, "Failed to allocate nl handle\n");
        err = ENOMEM;
        goto err_free_unlsk;
    }

    err = nl_connect(nlh, protocol);
    if (err < 0) {
        opal_output(0, "Failed to connnect netlink route socket error: %s\n",
                    NL_GETERROR(err));
        err = EINVAL;
        goto err_free_nlh;
    }

    NL_DISABLE_SEQ_CHECK(nlh);
    err = opal_reachable_netlink_set_rcvsk_timer(nlh);
    if (err < 0)
        goto err_close_nlh;

    unlsk->nlh = nlh;
    unlsk->seq = time(NULL);
    *p_sk = unlsk;
    return 0;

 err_close_nlh:
    nl_close(nlh);
 err_free_nlh:
    NL_HANDLE_FREE(nlh);
 err_free_unlsk:
    free(unlsk);
    return err;
}

static void opal_reachable_netlink_sk_free(struct opal_reachable_netlink_sk *unlsk)
{
    nl_close(unlsk->nlh);
    NL_HANDLE_FREE(unlsk->nlh);
    free(unlsk);
}

static int opal_reachable_netlink_rt_raw_parse_cb(struct nl_msg *msg, void *arg)
{
    struct opal_reachable_netlink_rt_cb_arg *lookup_arg = (struct opal_reachable_netlink_rt_cb_arg *)arg;
    struct opal_reachable_netlink_sk *unlsk = lookup_arg->unlsk;
    struct nlmsghdr *nlm_hdr = nlmsg_hdr(msg);
    struct rtmsg *rtm;
    struct nlattr *tb[RTA_MAX + 1];
    int found = 0;
    int err;

    INC_CB_MSGCNT(lookup_arg);

    if (!opal_reachable_netlink_is_nlreply_expected(unlsk, nlm_hdr)) {
#if OPAL_ENABLE_DEBUG
        nl_msg_dump(msg, stderr);
#endif
        return NL_SKIP;
    }

    if (opal_reachable_netlink_is_nlreply_err(nlm_hdr)) {
#if OPAL_ENABLE_DEBUG
        nl_msg_dump(msg, stderr);
#endif
        return NL_SKIP;
    }

    if (nlm_hdr->nlmsg_type != RTM_NEWROUTE) {
#if OPAL_ENABLE_DEBUG
        char buf[128];
        nl_nlmsgtype2str(nlm_hdr->nlmsg_type, buf, sizeof(buf));
        opal_output(0, "Received an invalid route request reply message type: %s\n",
                    buf);
        nl_msg_dump(msg, stderr);
#endif
        return NL_SKIP;
    }

    rtm = nlmsg_data(nlm_hdr);
    if (rtm->rtm_family != AF_INET
#if OPAL_ENABLE_IPV6
	&& rtm->rtm_family != AF_INET6
#endif
	) {
#if OPAL_ENABLE_DEBUG
        opal_output(0, "RTM message contains invalid AF family: %u\n",
                    rtm->rtm_family);
        nl_msg_dump(msg, stderr);
#endif
        return NL_SKIP;
    }

    err = nlmsg_parse(nlm_hdr, sizeof(struct rtmsg), tb, RTA_MAX,
                      route_policy);
    if (err < 0) {
#if OPAL_ENABLE_DEBUG
        opal_output(0, "nlmsg parse error %s\n", NL_GETERROR(err));
        nl_msg_dump(msg, stderr);
#endif
        return NL_SKIP;
    }

    if (tb[RTA_OIF]) {
        if (nla_get_u32(tb[RTA_OIF]) == (uint32_t)lookup_arg->oif)
            found = 1;
        else
            /* usually, this means that there is a route to the remote
               host, but that it's not through the given interface.  For
               our purposes, that means it's not reachable. */
            opal_output_verbose(20, 0, "Retrieved route has a different outgoing interface %d (expected %d)\n",
				nla_get_u32(tb[RTA_OIF]),
				lookup_arg->oif);
    }

    if (found && tb[RTA_GATEWAY]) {
        lookup_arg->has_gateway = 1;
    }
    lookup_arg->found = found;
    return NL_STOP;
}

int opal_reachable_netlink_rt_lookup(uint32_t src_addr,
				     uint32_t dst_addr,
				     int outgoing_interface,
                                     int *has_gateway)
{
    struct opal_reachable_netlink_sk *unlsk; /* netlink socket */
    struct nl_msg *nlm; /* netlink message */
    struct rtmsg rmsg; /* route message */
    struct opal_reachable_netlink_rt_cb_arg arg; /* callback argument */
    int err;

    /* allocate netlink socket */
    unlsk = NULL;
    err = opal_reachable_netlink_sk_alloc(&unlsk, NETLINK_ROUTE);
    if (err)
        return err;

    /* allocate route message */
    memset(&rmsg, 0, sizeof(rmsg));
    rmsg.rtm_family = AF_INET;
    rmsg.rtm_dst_len = sizeof(dst_addr) * CHAR_BIT;
    rmsg.rtm_src_len = sizeof(src_addr) * CHAR_BIT;

    /* allocate netlink message of type RTM_GETROUTE */
    nlm = nlmsg_alloc_simple(RTM_GETROUTE, 0);
    if (!nlm) {
        opal_output(0, "Failed to alloc nl message, %s\n",
                    NL_GETERROR(err));
        err = ENOMEM;
        goto out;
    }

    /* append route message and addresses to netlink message.   */
    nlmsg_append(nlm, &rmsg, sizeof(rmsg), NLMSG_ALIGNTO);
    nla_put_u32(nlm, RTA_DST, dst_addr);
    nla_put_u32(nlm, RTA_SRC, src_addr);

    /* query kernel */
    err = opal_reachable_netlink_send_query(unlsk, nlm, NETLINK_ROUTE, NLM_F_REQUEST);
    nlmsg_free(nlm);
    if (err < 0) {
        opal_output(0, "Failed to send RTM_GETROUTE query message, error %s\n",
                    NL_GETERROR(err));
        err = EINVAL;
        goto out;
    }

    /* Setup callback function */
    memset(&arg, 0, sizeof(arg));
    arg.oif = outgoing_interface;
    arg.unlsk = unlsk;
    err = nl_socket_modify_cb(unlsk->nlh, NL_CB_MSG_IN, NL_CB_CUSTOM,
                              opal_reachable_netlink_rt_raw_parse_cb, &arg);
    if (err != 0) {
        opal_output(0, "Failed to setup callback function, error %s\n",
                    NL_GETERROR(err));
        err = EINVAL;
        goto out;
    }

    /* recieve results */
    NL_RECVMSGS(unlsk->nlh, arg, EHOSTUNREACH, err, out);

    /* check whether a route was found */
    if (arg.found) {
        *has_gateway = arg.has_gateway;
        err = 0;
    } else {
        *has_gateway = 0;
        err = EHOSTUNREACH;
    }

 out:
    opal_reachable_netlink_sk_free(unlsk);
    return err;
}


#if OPAL_ENABLE_IPV6
int opal_reachable_netlink_rt_lookup6(struct in6_addr *src_addr,
				      struct in6_addr *dst_addr,
				      int outgoing_interface,
				      int *has_gateway)
{

    struct opal_reachable_netlink_sk *unlsk; /* netlink socket */
    struct nl_msg *nlm; /* netlink message */
    struct rtmsg rmsg; /* route message */
    struct opal_reachable_netlink_rt_cb_arg arg; /* callback argument */
    int err;

    /* allocate netlink socket */
    unlsk = NULL;
    err = opal_reachable_netlink_sk_alloc(&unlsk, NETLINK_ROUTE);
    if (err)
	return err;

    /* allocate route message */
    memset(&rmsg, 0, sizeof(rmsg));
    rmsg.rtm_family = AF_INET6;
    rmsg.rtm_dst_len = sizeof(*dst_addr) * CHAR_BIT;
    rmsg.rtm_src_len = sizeof(*src_addr) * CHAR_BIT;

    /* allocate netlink message of type RTM_GETROUTE */
    nlm = nlmsg_alloc_simple(RTM_GETROUTE, 0);
    if (!nlm) {
	opal_output(0, "Failed to alloc nl message, %s\n",
		    NL_GETERROR(err));
	err = ENOMEM;
	goto out;
    }

    /* append route message and addresses to netlink message.   */
    nlmsg_append(nlm, &rmsg, sizeof(rmsg), NLMSG_ALIGNTO);
    nla_put(nlm, RTA_DST, sizeof(dst_addr->s6_addr), &(dst_addr->s6_addr));
    nla_put(nlm, RTA_SRC, sizeof(src_addr->s6_addr), &(src_addr->s6_addr));

    /* query kernel */
    err = opal_reachable_netlink_send_query(unlsk, nlm, NETLINK_ROUTE, NLM_F_REQUEST);
    nlmsg_free(nlm);
    if (err < 0) {
	opal_output(0, "Failed to send RTM_GETROUTE query message, error %s\n",
		    NL_GETERROR(err));
	err = EINVAL;
	goto out;
    }

    /* Setup callback function */
    memset(&arg, 0, sizeof(arg));
    arg.oif = outgoing_interface;
    arg.unlsk = unlsk;
    err = nl_socket_modify_cb(unlsk->nlh, NL_CB_MSG_IN, NL_CB_CUSTOM,
			      opal_reachable_netlink_rt_raw_parse_cb, &arg);
    if (err != 0) {
	opal_output(0, "Failed to setup callback function, error %s\n",
		    NL_GETERROR(err));
	err = EINVAL;
	goto out;
    }

    /* receive results */
    NL_RECVMSGS(unlsk->nlh, arg, EHOSTUNREACH, err, out);

    /* check whether a route was found */
    if (arg.found) {
        *has_gateway = arg.has_gateway;
	err = 0;
    } else {
        *has_gateway = 0;
	err = EHOSTUNREACH;
    }

 out:
    opal_reachable_netlink_sk_free(unlsk);
    return err;
}
#endif /* #if OPAL_ENABLE_IPV6 */
